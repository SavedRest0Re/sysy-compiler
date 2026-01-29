use koopa::ir::{
    BasicBlock, BinaryOp, Function, FunctionData, Program, Type, TypeKind, Value, ValueKind,
};

use std::{
    collections::HashMap,
    fs::File,
    io::{Result, Write},
};

use crate::asm::{
    ctx::{Ctx, RegAlloc},
    datasection::generate_data_section,
    instruction::RVInst,
};

const ARG_REGS: [&str; 8] = ["a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"];

fn fits_i12(x: i32) -> bool {
    (-2048..=2047).contains(&x)
}

fn emit_add_sp_imm_or_tmp(ctx: &mut Ctx, buf: &mut File, rd: &'static str, off: i32) -> Result<()> {
    if fits_i12(off) {
        RVInst::Addi {
            rd,
            rs1: "sp",
            imm12: off,
        }
        .emit_indent2(buf)
    } else {
        let tmp = ctx.alloc_reg(None);
        RVInst::Li {
            rd: RegAlloc::literal(tmp),
            imm12: off,
        }
        .emit_indent2(buf)?;
        RVInst::Add {
            rd,
            rs1: "sp",
            rs2: RegAlloc::literal(tmp),
        }
        .emit_indent2(buf)?;
        ctx.free_reg(tmp);
        Ok(())
    }
}

fn emit_lw_sp_offset(ctx: &mut Ctx, buf: &mut File, rd: &'static str, off: i32) -> Result<()> {
    if fits_i12(off) {
        RVInst::Lw {
            rd,
            rs1: "sp",
            offset: off,
        }
        .emit_indent2(buf)
    } else {
        let addr = ctx.alloc_reg(None);
        emit_add_sp_imm_or_tmp(ctx, buf, RegAlloc::literal(addr), off)?;
        RVInst::Lw {
            rd,
            rs1: RegAlloc::literal(addr),
            offset: 0,
        }
        .emit_indent2(buf)?;
        ctx.free_reg(addr);
        Ok(())
    }
}

fn emit_sw_sp_offset(ctx: &mut Ctx, buf: &mut File, rs2: &'static str, off: i32) -> Result<()> {
    if fits_i12(off) {
        RVInst::Sw {
            rs2,
            rs1: "sp",
            offset: off,
        }
        .emit_indent2(buf)
    } else {
        let addr = ctx.alloc_reg(None);
        emit_add_sp_imm_or_tmp(ctx, buf, RegAlloc::literal(addr), off)?;
        RVInst::Sw {
            rs2,
            rs1: RegAlloc::literal(addr),
            offset: 0,
        }
        .emit_indent2(buf)?;
        ctx.free_reg(addr);
        Ok(())
    }
}

fn save_caller_regs(ctx: &mut Ctx, buf: &mut File) {
    for (i, reg) in ARG_REGS.iter().enumerate() {
        emit_sw_sp_offset(ctx, buf, reg, ctx.saved_arg_offset(i)).unwrap();
    }
}

// 如果函数有返回值, 返回值放在 a0, 恢复 caller-saved 时跳过 a0.
fn restore_caller_regs_mut(ctx: &mut Ctx, buf: &mut File, keep_a0: bool) {
    let restore_from = if keep_a0 { 1 } else { 0 };
    for (i, reg) in ARG_REGS.iter().enumerate().skip(restore_from) {
        emit_lw_sp_offset(ctx, buf, reg, ctx.saved_arg_offset(i)).unwrap();
    }
}

fn emit_prologue(ctx: &mut Ctx, buf: &mut File) -> Result<()> {
    let sp_delta = -(ctx.stack_size as i32);
    if fits_i12(sp_delta) {
        RVInst::Addi {
            rd: "sp",
            rs1: "sp",
            imm12: sp_delta,
        }
        .emit_indent2(buf)?;
    } else {
        let tmp = ctx.alloc_reg(None);
        RVInst::Li {
            rd: RegAlloc::literal(tmp),
            imm12: sp_delta,
        }
        .emit_indent2(buf)?;
        RVInst::Add {
            rd: "sp",
            rs1: "sp",
            rs2: RegAlloc::literal(tmp),
        }
        .emit_indent2(buf)?;
        ctx.free_reg(tmp);
    }

    emit_sw_sp_offset(ctx, buf, "ra", ctx.ra_offset())?;

    // FIXME: 总是保存 a0-a7
    save_caller_regs(ctx, buf);

    Ok(())
}

fn emit_epilogue(ctx: &mut Ctx, buf: &mut File, keep_a0: bool) -> Result<()> {
    restore_caller_regs_mut(ctx, buf, keep_a0);

    emit_lw_sp_offset(ctx, buf, "ra", ctx.ra_offset())?;

    let sp_delta = ctx.stack_size as i32;
    if fits_i12(sp_delta) {
        RVInst::Addi {
            rd: "sp",
            rs1: "sp",
            imm12: sp_delta,
        }
        .emit_indent2(buf)?;
    } else {
        let tmp = ctx.alloc_reg(None);
        RVInst::Li {
            rd: RegAlloc::literal(tmp),
            imm12: sp_delta,
        }
        .emit_indent2(buf)?;
        RVInst::Add {
            rd: "sp",
            rs1: "sp",
            rs2: RegAlloc::literal(tmp),
        }
        .emit_indent2(buf)?;
        ctx.free_reg(tmp);
    }

    Ok(())
}

#[derive(Clone, Copy)]
enum LoadedReg {
    Fixed(&'static str),
    Temp(usize),
}

impl LoadedReg {
    fn as_str(&self) -> &'static str {
        match self {
            LoadedReg::Fixed(r) => r,
            LoadedReg::Temp(r) => RegAlloc::literal(*r),
        }
    }

    fn free(self, ctx: &mut Ctx) {
        if let LoadedReg::Temp(r) = self {
            ctx.free_reg(r);
        }
    }
}

fn is_zero_integer(kind: &ValueKind) -> bool {
    matches!(kind, ValueKind::Integer(i) if i.value() == 0)
}

fn value_type<'a>(ctx: &Ctx<'a>, v: Value) -> Type {
    if v.is_global() {
        ctx.program().borrow_value(v).ty().clone()
    } else {
        ctx.func_data().dfg().value(v).ty().clone()
    }
}

fn ptr_elem_size_for_getptr<'a>(ctx: &Ctx<'a>, src: Value) -> usize {
    match value_type(ctx, src).kind() {
        TypeKind::Pointer(base) => base.size(),
        _ => unreachable!(),
    }
}

fn ptr_elem_size_for_getelemptr<'a>(ctx: &Ctx<'a>, src: Value) -> usize {
    match value_type(ctx, src).kind() {
        TypeKind::Pointer(pointee) => match pointee.kind() {
            TypeKind::Array(base, _) => base.size(),
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

fn load_from_stack(ctx: &mut Ctx, buf: &mut File, v: Value, offset: i32) -> Result<LoadedReg> {
    let reg = ctx.alloc_reg(Some(v));
    emit_lw_sp_offset(ctx, buf, RegAlloc::literal(reg), offset)?;
    Ok(LoadedReg::Temp(reg))
}

// globals / locals / other `Value`.
fn load_operand_to_reg(ctx: &mut Ctx, buf: &mut File, v: Value) -> Result<LoadedReg> {
    // operand 可能是 globals
    if v.is_global() {
        let v_data = ctx.program().borrow_value(v);
        if is_zero_integer(v_data.kind()) {
            return Ok(LoadedReg::Fixed("x0"));
        }
        if let ValueKind::GlobalAlloc(_) = v_data.kind() {
            let reg = ctx.alloc_reg(Some(v));
            let name = v_data
                .name()
                .clone()
                .expect("global alloc must have a name")
                .strip_prefix('@')
                .unwrap()
                .to_string();
            RVInst::La {
                rd: RegAlloc::literal(reg),
                label: name,
            }
            .emit_indent2(buf)?;
            return Ok(LoadedReg::Temp(reg));
        }
    } else {
        let v_data = ctx.func_data().dfg().value(v);
        if is_zero_integer(v_data.kind()) {
            return Ok(LoadedReg::Fixed("x0"));
        }

        if let ValueKind::FuncArgRef(arg_ref) = v_data.kind() {
            let index = arg_ref.index();
            if index < 8 {
                let offset = ctx.saved_arg_offset(index);
                return load_from_stack(ctx, buf, v, offset);
            }

            let offset = ctx.incoming_stack_arg_offset(index);
            return load_from_stack(ctx, buf, v, offset);
        }

        // `%var1 = load %ptr`, `var1` 是个栈上对象, 存放的是某个地址.
        // `addi reg, sp, offset` 将 var1 的地址加载到寄存器
        if let ValueKind::Alloc(_) = v_data.kind() {
            let offset = ctx
                .stack_offset(&v)
                .expect("Alloc value must have stack slot");
            let reg = ctx.alloc_reg(Some(v));
            emit_add_sp_imm_or_tmp(ctx, buf, RegAlloc::literal(reg), offset)?;
            return Ok(LoadedReg::Temp(reg));
        }
    }

    if let Some(offset) = ctx.stack_offset(&v) {
        load_from_stack(ctx, buf, v, offset)
    } else {
        let saved_value = ctx.cur_value().unwrap();
        v.generate(ctx, buf)?;
        ctx.set_cur_value(saved_value);

        Ok(match ctx.query_reg(v) {
            "x0" => LoadedReg::Fixed("x0"),
            reg_lit => {
                let reg = ctx
                    .reg_allocator
                    .as_ref()
                    .unwrap()
                    .query_reg_by_value(v)
                    .expect("Value not in register");
                debug_assert_eq!(RegAlloc::literal(reg), reg_lit);
                LoadedReg::Temp(reg)
            }
        })
    }
}

fn emit_binary_inst(
    op: BinaryOp,
    rd: &'static str,
    rs1: &'static str,
    rs2: &'static str,
    buf: &mut File,
) -> Result<()> {
    use BinaryOp::*;
    match op {
        // Simple R-type instructions
        Add => RVInst::Add { rd, rs1, rs2 }.emit_indent2(buf),
        Sub => RVInst::Sub { rd, rs1, rs2 }.emit_indent2(buf),
        Mul => RVInst::Mul { rd, rs1, rs2 }.emit_indent2(buf),
        Div => RVInst::Div { rd, rs1, rs2 }.emit_indent2(buf),
        Mod => RVInst::Rem { rd, rs1, rs2 }.emit_indent2(buf),
        And => RVInst::And { rd, rs1, rs2 }.emit_indent2(buf),
        Or => RVInst::Or { rd, rs1, rs2 }.emit_indent2(buf),
        Lt => RVInst::Slt { rd, rs1, rs2 }.emit_indent2(buf),
        Gt => RVInst::Slt {
            rd,
            rs1: rs2,
            rs2: rs1,
        }
        .emit_indent2(buf),

        // Comparison operations that need two instructions
        Eq => {
            RVInst::Xor { rd, rs1, rs2 }.emit_indent2(buf)?;
            RVInst::Seqz { rd, rs: rd }.emit_indent2(buf)
        }
        NotEq => {
            RVInst::Xor { rd, rs1, rs2 }.emit_indent2(buf)?;
            RVInst::Snez { rd, rs: rd }.emit_indent2(buf)
        }
        Le => {
            // a <= b  <==>  !(a > b)  <==>  !(b < a) == 0
            RVInst::Slt {
                rd,
                rs1: rs2,
                rs2: rs1,
            }
            .emit_indent2(buf)?;
            RVInst::Seqz { rd, rs: rd }.emit_indent2(buf)
        }
        Ge => {
            // a >= b  <==>  !(a < b)
            RVInst::Slt { rd, rs1, rs2 }.emit_indent2(buf)?;
            RVInst::Seqz { rd, rs: rd }.emit_indent2(buf)
        }

        _ => unimplemented!("Unsupported binary op: {:?}", op),
    }
}

fn spill_to_stack_if_needed(
    ctx: &mut Ctx,
    buf: &mut File,
    v: &Value,
    src: &'static str,
) -> Result<()> {
    if let Some(offset) = ctx.stack_offset(v) {
        emit_sw_sp_offset(ctx, buf, src, offset)?;
    }
    Ok(())
}

pub trait AsmGen {
    type Output;
    fn generate(&self, ctx: &mut Ctx, buf: &mut File) -> Result<Self::Output>;
}

impl AsmGen for Program {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx, buf: &mut File) -> Result<Self::Output> {
        // data section
        generate_data_section(self, buf);

        // text section
        for &func in self.func_layout() {
            // skip codegen for function declarations
            if self.func(func).layout().entry_bb().is_none() {
                continue;
            }
            func.generate(ctx, buf)?;
        }

        Ok(())
    }
}

impl AsmGen for Function {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx, buf: &mut File) -> Result<Self::Output> {
        ctx.set_cur_func(*self);
        ctx.set_reg_alloc(RegAlloc::new());
        ctx.stack_size = 0;
        ctx.stack_alloc = HashMap::new();

        let func_data = ctx.func_data();
        let name = func_data.name().strip_prefix('@').unwrap();
        writeln!(buf, "  .text")?;
        writeln!(buf, "  .global {name}")?;
        writeln!(buf, "{name}:")?;

        let layout = FrameLayout::build(func_data);
        layout.apply(ctx);

        // 生成 prologue
        emit_prologue(ctx, buf)?;

        let bbs: Vec<_> = func_data.layout().bbs().keys().copied().collect();
        for bb in bbs {
            bb.generate(ctx, buf)?;
        }

        ctx.stack_alloc.clear();
        ctx.stack_size = 0;
        ctx.outgoing_args_size = 0;
        ctx.arg_save_base = 0;
        ctx.unset_reg_alloc();
        ctx.unset_cur_func();
        Ok(())
    }
}

// stack top to bottom: saved ra, saved a0-a7, locals/spills, outgoing args.
struct FrameLayout {
    stack_alloc: HashMap<Value, usize>,
    stack_size: usize,

    outgoing_args_size: usize,
    arg_save_base: usize,
}

impl FrameLayout {
    fn build(func_data: &FunctionData) -> Self {
        // 在生成 koopa ir 时, 已经将本函数的 args 转译为 `alloc` 指令, 这里不再需要单独计算. 其所占用空间, 可以视为 local vars 来计算.
        // `call` 的返回值也在下面计算了, 视为 local vars.

        let insts = func_data
            .layout()
            .bbs()
            .nodes()
            .flat_map(|bb_node| bb_node.insts().keys().map(|&value| value))
            .collect::<Vec<_>>();

        let max_args = insts
            .iter()
            .map(|&inst| match func_data.dfg().value(inst).kind() {
                ValueKind::Call(call) => call.args().len(),
                _ => 0,
            })
            .max()
            .unwrap_or(0);

        let outgoing_args_size = if max_args > 8 { (max_args - 8) * 4 } else { 0 };
        let locals_base = outgoing_args_size;
        let mut locals_size = 0;
        let mut stack_alloc = HashMap::new();

        // Calculate local vars space & map the virtual register to offset relative to sp
        for &inst in insts.iter() {
            let inst_data = func_data.dfg().value(inst);

            // stack allocation for ALL virtual registers
            // `Alloc` do not generate any asm, so we don't need to allocate stack space for it
            if let ValueKind::Alloc(_) = inst_data.kind() {
                stack_alloc.insert(inst, locals_base + locals_size);
                if let TypeKind::Pointer(base_ty) = inst_data.ty().kind() {
                    locals_size += base_ty.size();
                } else {
                    unreachable!()
                }
            } else {
                // If the inst has return value, we need to allocate stack space for it
                if !inst_data.ty().is_unit() {
                    stack_alloc.insert(inst, locals_base + locals_size);
                    locals_size += 4;
                }
            }
        }

        let arg_save_base = locals_base + locals_size;
        let arg_save_size = ARG_REGS.len() * 4;

        let mut stack_size = arg_save_base + arg_save_size + 4;
        stack_size = (stack_size + 15) & !15; // align to 16

        Self {
            outgoing_args_size,
            arg_save_base,
            stack_alloc,
            stack_size,
        }
    }

    fn apply(self, ctx: &mut Ctx) {
        ctx.outgoing_args_size = self.outgoing_args_size;
        ctx.arg_save_base = self.arg_save_base;
        ctx.stack_alloc = self.stack_alloc;
        ctx.stack_size = self.stack_size;
    }
}

impl AsmGen for BasicBlock {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx, buf: &mut File) -> Result<Self::Output> {
        ctx.set_cur_bb(*self);

        // entry_bb 不需要另外生成 label, 因为函数名就是 entry_bb 的 label.
        if let Some(entry_bb) = ctx.func_data().layout().entry_bb() {
            if entry_bb != *self {
                let label = ctx.bb_label(*self);
                writeln!(buf, "{}:", label)?;
            }
        }

        let insts: Vec<_> = ctx
            .func_data()
            .layout()
            .bbs()
            .node(self)
            .unwrap()
            .insts()
            .keys()
            .copied()
            .collect();

        for inst in insts {
            inst.generate(ctx, buf)?;
        }

        ctx.unset_cur_bb();
        Ok(())
    }
}

// koopa IR 中的 "虚拟寄存器" 使用 `Value` 来表示, Value 也是一条指令的 handle.
impl AsmGen for Value {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx, buf: &mut File) -> Result<Self::Output> {
        ctx.set_cur_value(*self);

        let value_data = if self.is_global() {
            ctx.program().borrow_value(*self).clone()
        } else {
            ctx.func_data().dfg().value(*self).clone()
        };
        match value_data.kind() {
            ValueKind::Integer(i) => {
                if i.value() != 0 {
                    let reg = ctx.alloc_reg(Some(*self));
                    RVInst::Li {
                        rd: RegAlloc::literal(reg),
                        imm12: i.value(),
                    }
                    .emit_indent2(buf)?;
                }
                // NOTE: Do not free allocated register here
            }
            ValueKind::Binary(binary) => {
                let lhs_v = binary.lhs();
                let rhs_v = binary.rhs();

                let lhs_reg = load_operand_to_reg(ctx, buf, lhs_v)?;
                let rhs_reg = load_operand_to_reg(ctx, buf, rhs_v)?;

                let result_reg = ctx.alloc_reg(Some(*self));

                emit_binary_inst(
                    binary.op(),
                    RegAlloc::literal(result_reg),
                    lhs_reg.as_str(),
                    rhs_reg.as_str(),
                    buf,
                )?;

                spill_to_stack_if_needed(ctx, buf, self, RegAlloc::literal(result_reg))?;

                // 释放 lhs, rhs 和 result 寄存器
                lhs_reg.free(ctx);
                rhs_reg.free(ctx);
                ctx.free_reg(result_reg);
            }
            ValueKind::Return(ret) => {
                if let Some(retval) = ret.value() {
                    let ret_reg = load_operand_to_reg(ctx, buf, retval)?;
                    if ret_reg.as_str() != "a0" {
                        RVInst::Mv {
                            rd: "a0",
                            rs: ret_reg.as_str(),
                        }
                        .emit_indent2(buf)?;
                    }
                    ret_reg.free(ctx);
                }

                // FIXME: if stack size is not in range [-2048, 2047], we need to use a different way to allocate stack space
                // insert epilogue before return instruction
                emit_epilogue(ctx, buf, ret.value().is_some())?;
                RVInst::Ret.emit_indent2(buf)?;
            }
            ValueKind::Alloc(_) => {
                // Do not generate asm for Alloc IR in text section.
                // Because it's side effect is stack allocation, which is already handled in prologue
            }
            ValueKind::GlobalAlloc(_) => {
                // Do not generate asm for Gloabl Alloc IR in text section.
                // Because it has been handled in data section.
            }
            ValueKind::Load(load) => {
                // Koopa: %v = load %ptr
                // asm:
                //   lw tmp, 0(ptr)
                //   sw tmp, off(sp)     (if this value has stack slot)
                let ptr = load.src(); // ptr 是个 `ValueKind::Alloc(_)` (局部)
                let ptr_reg = load_operand_to_reg(ctx, buf, ptr)?;

                let tmp = ctx.alloc_reg(None);

                RVInst::Lw {
                    rd: RegAlloc::literal(tmp),
                    rs1: ptr_reg.as_str(),
                    offset: 0,
                }
                .emit_indent2(buf)?;

                spill_to_stack_if_needed(ctx, buf, self, RegAlloc::literal(tmp))?;

                ctx.free_reg(tmp);

                ptr_reg.free(ctx);
            }
            ValueKind::Store(store) => {
                // Koopa: store %value, %ptr
                // asm:
                //   sw value, 0(ptr)
                let value = store.value();
                let ptr = store.dest();
                let value_reg = load_operand_to_reg(ctx, buf, value)?;
                let ptr_reg = load_operand_to_reg(ctx, buf, ptr)?;
                RVInst::Sw {
                    rs2: value_reg.as_str(),
                    rs1: ptr_reg.as_str(),
                    offset: 0,
                }
                .emit_indent2(buf)?;

                value_reg.free(ctx);
                ptr_reg.free(ctx);
            }
            ValueKind::Branch(br) => {
                let cond = br.cond();
                let true_label = ctx.bb_label(br.true_bb());
                let false_label = ctx.bb_label(br.false_bb());

                let cond_reg = load_operand_to_reg(ctx, buf, cond)?;
                RVInst::Bnez {
                    rs: cond_reg.as_str(),
                    label: true_label,
                }
                .emit_indent2(buf)?;
                RVInst::J { label: false_label }.emit_indent2(buf)?;

                cond_reg.free(ctx);
            }
            ValueKind::Jump(jump) => {
                let target_label = ctx.bb_label(jump.target());
                RVInst::J {
                    label: target_label,
                }
                .emit_indent2(buf)?;
            }
            ValueKind::Call(call) => {
                let callee = call.callee();
                let args = call.args();

                let callee_data = ctx.program().func(callee);

                let func_name = callee_data
                    .name()
                    .strip_prefix('@')
                    .unwrap_or(callee_data.name())
                    .to_string();

                let reg_arg_count = args.len().min(8);
                let stack_arg_count = args.len().saturating_sub(8);

                if stack_arg_count > 0 {
                    let stack_args = &args[8..];
                    for (i, &arg) in stack_args.iter().enumerate() {
                        let arg_reg = load_operand_to_reg(ctx, buf, arg)?;
                        RVInst::Sw {
                            rs2: arg_reg.as_str(),
                            rs1: "sp",
                            offset: ctx.outgoing_arg_offset(i),
                        }
                        .emit_indent2(buf)?;
                        arg_reg.free(ctx);
                    }
                }

                for (i, &arg) in args.iter().enumerate().take(reg_arg_count) {
                    let arg_reg = load_operand_to_reg(ctx, buf, arg)?;
                    let dst = ARG_REGS[i];
                    if arg_reg.as_str() != dst {
                        RVInst::Mv {
                            rd: dst,
                            rs: arg_reg.as_str(),
                        }
                        .emit_indent2(buf)?;
                    }
                    arg_reg.free(ctx);
                }

                RVInst::Call { label: func_name }.emit_indent2(buf)?;

                if !value_data.ty().is_unit() {
                    spill_to_stack_if_needed(ctx, buf, self, "a0")?;
                }
            }
            ValueKind::GetPtr(getptr) => {
                let src = getptr.src();
                let index = getptr.index();

                let src_reg = load_operand_to_reg(ctx, buf, src)?;
                let index_reg = load_operand_to_reg(ctx, buf, index)?;

                let elem_size = ptr_elem_size_for_getptr(ctx, src) as i32;

                let tmp_scale = ctx.alloc_reg(None);
                RVInst::Li {
                    rd: RegAlloc::literal(tmp_scale),
                    imm12: elem_size,
                }
                .emit_indent2(buf)?;

                let tmp_off = ctx.alloc_reg(None);
                RVInst::Mul {
                    rd: RegAlloc::literal(tmp_off),
                    rs1: index_reg.as_str(),
                    rs2: RegAlloc::literal(tmp_scale),
                }
                .emit_indent2(buf)?;

                let result_reg = ctx.alloc_reg(Some(*self));
                RVInst::Add {
                    rd: RegAlloc::literal(result_reg),
                    rs1: src_reg.as_str(),
                    rs2: RegAlloc::literal(tmp_off),
                }
                .emit_indent2(buf)?;

                spill_to_stack_if_needed(ctx, buf, self, RegAlloc::literal(result_reg))?;

                src_reg.free(ctx);
                index_reg.free(ctx);
                ctx.free_reg(tmp_scale);
                ctx.free_reg(tmp_off);
                ctx.free_reg(result_reg);
            }
            ValueKind::GetElemPtr(gep) => {
                let src = gep.src();
                let index = gep.index();

                let src_reg = load_operand_to_reg(ctx, buf, src)?;
                let index_reg = load_operand_to_reg(ctx, buf, index)?;

                let elem_size = ptr_elem_size_for_getelemptr(ctx, src) as i32;

                let tmp_scale = ctx.alloc_reg(None);
                RVInst::Li {
                    rd: RegAlloc::literal(tmp_scale),
                    imm12: elem_size,
                }
                .emit_indent2(buf)?;

                let tmp_off = ctx.alloc_reg(None);
                RVInst::Mul {
                    rd: RegAlloc::literal(tmp_off),
                    rs1: index_reg.as_str(),
                    rs2: RegAlloc::literal(tmp_scale),
                }
                .emit_indent2(buf)?;

                let result_reg = ctx.alloc_reg(Some(*self));
                RVInst::Add {
                    rd: RegAlloc::literal(result_reg),
                    rs1: src_reg.as_str(),
                    rs2: RegAlloc::literal(tmp_off),
                }
                .emit_indent2(buf)?;

                spill_to_stack_if_needed(ctx, buf, self, RegAlloc::literal(result_reg))?;

                src_reg.free(ctx);
                index_reg.free(ctx);
                ctx.free_reg(tmp_scale);
                ctx.free_reg(tmp_off);
                ctx.free_reg(result_reg);
            }
            _ => unimplemented!(),
        }

        ctx.unset_cur_value();
        Ok(())
    }
}
