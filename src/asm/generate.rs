use koopa::ir::{BasicBlock, BinaryOp, Function, Program, Value, ValueKind};

use std::{
    collections::HashMap,
    fs::File,
    io::{Result, Write},
};

use crate::asm::{
    ctx::{Ctx, RegAlloc},
    instruction::RVInst,
};

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

/// Load a Value operand into a register, returns the register literal
/// - Integer 0 -> returns "x0"
/// - Value on stack -> generates lw and returns allocated register
/// - Otherwise -> recursively generates value and returns its register
fn load_operand_to_reg(ctx: &mut Ctx, buf: &mut File, v: Value) -> Result<&'static str> {
    // Check if value is integer 0 -> use x0
    let v_data = ctx.func_data().dfg().value(v);
    if let ValueKind::Integer(i) = v_data.kind() {
        if i.value() == 0 {
            return Ok("x0");
        }
    }

    // Check if value is on stack (virtual register)
    if let Some(offset) = ctx.stack_offset(&v) {
        let reg = ctx.alloc_reg(Some(v));
        RVInst::Lw {
            rd: RegAlloc::literal(reg),
            rs1: "sp",
            offset,
        }
        .emit_indent2(buf)?;
        Ok(RegAlloc::literal(reg))
    } else {
        // Recursively generate the value (e.g., immediate)
        let saved_value = ctx.cur_value().unwrap();
        v.generate(ctx, buf)?;
        ctx.set_cur_value(saved_value);
        Ok(ctx.query_reg(v))
    }
}

pub trait AsmGen {
    type Output;
    fn generate(&self, ctx: &mut Ctx, buf: &mut File) -> Result<Self::Output>;
}

impl AsmGen for Program {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx, buf: &mut File) -> Result<Self::Output> {
        writeln!(buf, "  .text")?;

        for &func in self.func_layout() {
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
        writeln!(buf, "  .global {name}")?;
        writeln!(buf, "{name}:")?;

        // Calculate stack size & map the virtual register to offset relative to sp
        for (_, bb_node) in func_data.layout().bbs() {
            for &inst in bb_node.insts().keys() {
                let inst_data = func_data.dfg().value(inst);

                // stack allocation for ALL virtual registers
                // `Alloc` do not generate any asm, so we don't need to allocate stack space for it
                if let ValueKind::Alloc(_) = inst_data.kind() {
                    ctx.stack_alloc.insert(inst, ctx.stack_size);
                    // ctx.stack_size += inst_data.ty().size(); // error
                    ctx.stack_size += 4;
                } else {
                    // If the inst has return value, we need to allocate stack space for it
                    if !inst_data.ty().is_unit() {
                        ctx.stack_alloc.insert(inst, ctx.stack_size);
                        ctx.stack_size += 4;
                    }
                }
            }
        }
        // sp alignment to 16
        ctx.stack_size = (ctx.stack_size + 15) & !15;

        let bbs: Vec<_> = func_data.layout().bbs().keys().copied().collect();
        for bb in bbs {
            bb.generate(ctx, buf)?;
        }

        ctx.stack_alloc.clear();
        ctx.stack_size = 0;
        ctx.unset_reg_alloc();
        ctx.unset_cur_func();
        Ok(())
    }
}

impl AsmGen for BasicBlock {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx, buf: &mut File) -> Result<Self::Output> {
        ctx.set_cur_bb(*self);

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

        let value_data = ctx.func_data().dfg().value(*self).clone();
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
                    lhs_reg,
                    rhs_reg,
                    buf,
                )?;

                // Store result to stack if this value has stack allocation
                if let Some(offset) = ctx.stack_offset(self) {
                    RVInst::Sw {
                        rs2: RegAlloc::literal(result_reg),
                        rs1: "sp",
                        offset,
                    }
                    .emit_indent2(buf)?;
                }

                // 释放 lhs, rhs 和 result 寄存器
                ctx.free_reg_lit(lhs_reg);
                ctx.free_reg_lit(rhs_reg);
                ctx.free_reg(result_reg);
            }
            ValueKind::Return(ret) => {
                if let Some(retval) = ret.value() {
                    let ret_reg = load_operand_to_reg(ctx, buf, retval)?;
                    if ret_reg != "a0" {
                        RVInst::Mv {
                            rd: "a0",
                            rs: ret_reg,
                        }
                        .emit_indent2(buf)?;
                    }
                    ctx.free_reg_lit(ret_reg);
                }

                // FIXME: if stack size is not in range [-2048, 2047], we need to use a different way to allocate stack space
                // insert epilogue before return instruction
                RVInst::Addi {
                    rd: "sp",
                    rs1: "sp",
                    imm12: ctx.stack_size as i32,
                }
                .emit_indent2(buf)?;
                RVInst::Ret.emit_indent2(buf)?;
            }
            ValueKind::Alloc(_) => {
                // Do not generate asm for Alloc IR
                // Because it's side effect is stack allocation, which is already handled in prologue
            }
            ValueKind::Load(load) => {
                // 1. load src(virtual reg) to temporary register
                // 2. store temporary register to dest (stack address)
                let src = load.src();
                let src_reg = load_operand_to_reg(ctx, buf, src)?;

                let dest_offset = ctx.stack_offset(self).expect("Load dest not on stack");
                RVInst::Sw {
                    rs2: src_reg,
                    rs1: "sp",
                    offset: dest_offset,
                }
                .emit_indent2(buf)?;

                ctx.free_reg_lit(src_reg);
            }
            ValueKind::Store(store) => {
                // 1. load src(virtual reg/immediate) to register
                // 2. store register to dest (stack address)
                let src = store.value();
                let dest = store.dest();

                let src_reg = load_operand_to_reg(ctx, buf, src)?;

                let dest_offset = ctx.stack_offset(&dest).expect("Store dest not on stack");
                RVInst::Sw {
                    rs2: src_reg,
                    rs1: "sp",
                    offset: dest_offset,
                }
                .emit_indent2(buf)?;

                ctx.free_reg_lit(src_reg);
            }
            ValueKind::Branch(br) => {
                let cond = br.cond();
                let true_label = ctx.bb_label(br.true_bb());
                let false_label = ctx.bb_label(br.false_bb());

                let cond_reg = load_operand_to_reg(ctx, buf, cond)?;
                RVInst::Bnez {
                    rs: cond_reg,
                    label: true_label,
                }
                .emit_indent2(buf)?;
                RVInst::J { label: false_label }.emit_indent2(buf)?;

                ctx.free_reg_lit(cond_reg);
            }
            ValueKind::Jump(jump) => {
                let target_label = ctx.bb_label(jump.target());
                RVInst::J {
                    label: target_label,
                }
                .emit_indent2(buf)?;
            }
            _ => unimplemented!(),
        }

        ctx.unset_cur_value();
        Ok(())
    }
}
