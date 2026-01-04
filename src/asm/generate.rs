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

macro_rules! query_reg_by_value {
    ($ctx:expr, $value:expr) => {
        match $ctx.func_data().dfg().value($value).kind() {
            ValueKind::Integer(i) if i.value() == 0 => "x0",
            _ => {
                let reg = $ctx
                    .reg_allocator
                    .as_ref()
                    .unwrap()
                    .query_reg_by_value($value)
                    .unwrap();
                RegAlloc::literal(reg)
            }
        }
    };
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
        ctx.set_func_alloc(RegAlloc::new());
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
                    // dbg!(inst_data.kind());
                    ctx.stack_alloc.insert(inst, ctx.stack_size);
                    // ctx.stack_size += inst_data.ty().size(); // error
                    ctx.stack_size += 4;
                } else {
                    // If the inst has return value, we need to allocate stack space for it
                    if !inst_data.ty().is_unit() {
                        // dbg!(inst_data.kind());
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
        ctx.unset_func_alloc();
        ctx.unset_cur_func();
        Ok(())
    }
}

impl AsmGen for BasicBlock {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx, buf: &mut File) -> Result<Self::Output> {
        ctx.set_cur_bb(*self);

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
                // use x0 for constant 0, so we don't need to allocate a register
                if i.value() != 0 {
                    let reg = ctx
                        .reg_allocator
                        .as_mut()
                        .unwrap()
                        .alloc_reg(Some(*self))
                        .unwrap();
                    RVInst::Li {
                        rd: RegAlloc::literal(reg),
                        imm12: i.value(),
                    }
                    .emit_ident2(buf)?;
                }

                // NOTE: Do not free allocated register here
            }
            ValueKind::Binary(binary) => {
                let lhs_v = binary.lhs();
                let rhs_v = binary.rhs();

                // Helper closure to load a value into a register
                // Returns the register literal string
                let load_operand =
                    |ctx: &mut Ctx, buf: &mut File, v: Value| -> Result<&'static str> {
                        // Check if value is integer 0 -> use x0
                        let v_data = ctx.func_data().dfg().value(v);
                        if let ValueKind::Integer(i) = v_data.kind() {
                            if i.value() == 0 {
                                return Ok("x0");
                            }
                        }

                        // Check if value is in stack_alloc (virtual register)
                        if let Some(&offset) = ctx.stack_alloc.get(&v) {
                            let reg = ctx
                                .reg_allocator
                                .as_mut()
                                .unwrap()
                                .alloc_reg(Some(v))
                                .unwrap();
                            RVInst::Lw {
                                rd: RegAlloc::literal(reg),
                                rs1: "sp",
                                offset: offset as i32,
                            }
                            .emit_ident2(buf)?;
                            Ok(RegAlloc::literal(reg))
                        } else {
                            // Generate the value (e.g., immediate)
                            v.generate(ctx, buf)?;
                            // Query the register
                            let reg = ctx
                                .reg_allocator
                                .as_ref()
                                .unwrap()
                                .query_reg_by_value(v)
                                .unwrap();
                            Ok(RegAlloc::literal(reg))
                        }
                    };

                let lhs_reg = load_operand(ctx, buf, lhs_v)?;
                let rhs_reg = load_operand(ctx, buf, rhs_v)?;

                // FIXME: Restore cur_value since recursive calls changed it
                ctx.set_cur_value(*self);

                let result_reg = ctx
                    .reg_allocator
                    .as_mut()
                    .unwrap()
                    .alloc_reg(Some(*self))
                    .unwrap();

                match binary.op() {
                    // FIXME: `add lhs_reg lhs_reg rhs_reg` 节省寄存器
                    BinaryOp::Add => {
                        RVInst::Add {
                            rd: RegAlloc::literal(result_reg),
                            rs1: lhs_reg,
                            rs2: rhs_reg,
                        }
                        .emit_ident2(buf)?;
                    }
                    BinaryOp::Sub => {
                        RVInst::Sub {
                            rd: RegAlloc::literal(result_reg),
                            rs1: lhs_reg,
                            rs2: rhs_reg,
                        }
                        .emit_ident2(buf)?;
                    }
                    BinaryOp::Mul => {
                        RVInst::Mul {
                            rd: RegAlloc::literal(result_reg),
                            rs1: lhs_reg,
                            rs2: rhs_reg,
                        }
                        .emit_ident2(buf)?;
                    }
                    BinaryOp::Div => {
                        RVInst::Div {
                            rd: RegAlloc::literal(result_reg),
                            rs1: lhs_reg,
                            rs2: rhs_reg,
                        }
                        .emit_ident2(buf)?;
                    }

                    BinaryOp::Mod => {
                        RVInst::Rem {
                            rd: RegAlloc::literal(result_reg),
                            rs1: lhs_reg,
                            rs2: rhs_reg,
                        }
                        .emit_ident2(buf)?;
                    }

                    BinaryOp::Eq => {
                        RVInst::Xor {
                            rd: RegAlloc::literal(result_reg),
                            rs1: lhs_reg,
                            rs2: rhs_reg,
                        }
                        .emit_ident2(buf)?;
                        RVInst::Seqz {
                            rd: RegAlloc::literal(result_reg),
                            rs: RegAlloc::literal(result_reg),
                        }
                        .emit_ident2(buf)?;
                    }
                    BinaryOp::NotEq => {
                        RVInst::Xor {
                            rd: RegAlloc::literal(result_reg),
                            rs1: lhs_reg,
                            rs2: rhs_reg,
                        }
                        .emit_ident2(buf)?;
                        RVInst::Snez {
                            rd: RegAlloc::literal(result_reg),
                            rs: RegAlloc::literal(result_reg),
                        }
                        .emit_ident2(buf)?;
                    }

                    BinaryOp::Lt => {
                        RVInst::Slt {
                            rd: RegAlloc::literal(result_reg),
                            rs1: lhs_reg,
                            rs2: rhs_reg,
                        }
                        .emit_ident2(buf)?;
                    }

                    BinaryOp::Le => {
                        RVInst::Slt {
                            rd: RegAlloc::literal(result_reg),
                            rs1: rhs_reg,
                            rs2: lhs_reg,
                        }
                        .emit_ident2(buf)?;
                        RVInst::Seqz {
                            rd: RegAlloc::literal(result_reg),
                            rs: RegAlloc::literal(result_reg),
                        }
                        .emit_ident2(buf)?;
                    }

                    BinaryOp::Gt => {
                        RVInst::Slt {
                            rd: RegAlloc::literal(result_reg),
                            rs1: rhs_reg,
                            rs2: lhs_reg,
                        }
                        .emit_ident2(buf)?;
                    }

                    BinaryOp::Ge => {
                        RVInst::Slt {
                            rd: RegAlloc::literal(result_reg),
                            rs1: lhs_reg,
                            rs2: rhs_reg,
                        }
                        .emit_ident2(buf)?;
                        RVInst::Seqz {
                            rd: RegAlloc::literal(result_reg),
                            rs: RegAlloc::literal(result_reg),
                        }
                        .emit_ident2(buf)?;
                    }

                    BinaryOp::And => {
                        RVInst::And {
                            rd: RegAlloc::literal(result_reg),
                            rs1: lhs_reg,
                            rs2: rhs_reg,
                        }
                        .emit_ident2(buf)?;
                    }

                    BinaryOp::Or => {
                        RVInst::Or {
                            rd: RegAlloc::literal(result_reg),
                            rs1: lhs_reg,
                            rs2: rhs_reg,
                        }
                        .emit_ident2(buf)?;
                    }

                    _ => unimplemented!(),
                };

                // Store result to stack if this value has stack allocation
                if let Some(&offset) = ctx.stack_alloc.get(self) {
                    RVInst::Sw {
                        rs2: RegAlloc::literal(result_reg),
                        rs1: "sp",
                        offset: offset as i32,
                    }
                    .emit_ident2(buf)?;
                }

                // 释放 lhs, rhs 和 result 寄存器
                ctx.reg_allocator.as_mut().unwrap().free_reg_lit(lhs_reg);
                ctx.reg_allocator.as_mut().unwrap().free_reg_lit(rhs_reg);
                ctx.reg_allocator.as_mut().unwrap().free_reg(result_reg);
            }
            ValueKind::Return(ret) => {
                if let Some(retval) = ret.value() {
                    // ret `virtual reg`
                    if let Some(&retval_offset) = ctx.stack_alloc.get(&retval) {
                        RVInst::Lw {
                            rd: "a0",
                            rs1: "sp",
                            offset: retval_offset as i32,
                        }
                        .emit_ident2(buf)?;
                    } else {
                        retval.generate(ctx, buf)?;
                        let ret_reg = query_reg_by_value!(ctx, retval);

                        RVInst::Mv {
                            rd: "a0",
                            rs: ret_reg,
                        }
                        .emit_ident2(buf)?;

                        // 释放 ret_reg 临时寄存器
                        ctx.reg_allocator.as_mut().unwrap().free_reg_lit(ret_reg);
                    };
                }

                // FIXME: if stack size is not in range [-2048, 2047], we need to use a different way to allocate stack space
                // insert epilogue before return instruction
                RVInst::Addi {
                    rd: "sp",
                    rs1: "sp",
                    imm12: ctx.stack_size as i32,
                }
                .emit_ident2(buf)?;
                RVInst::Ret.emit_ident2(buf)?;
            }
            ValueKind::Alloc(_) => {
                // Do not generate asm for Alloc IR
                // Because it's side effect is stack allocation, which is already handled in prologue
            }
            ValueKind::Load(load) => {
                // 1. load src(virtual reg) to temporary register
                // 2. store temporary register to dest (stack address)
                let src = load.src();
                let Some(&src_offset) = ctx.stack_alloc.get(&src) else {
                    panic!("Unknown variable {:?}", src);
                };
                // let src_reg = query_reg_by_value!(ctx, src);
                let src_reg = ctx
                    .reg_allocator
                    .as_mut()
                    .unwrap()
                    .alloc_reg(Some(src))
                    .unwrap();
                RVInst::Lw {
                    rd: RegAlloc::literal(src_reg),
                    rs1: "sp",
                    offset: src_offset as i32,
                }
                .emit_ident2(buf)?;

                let dest_offset = ctx.stack_alloc.get(self).unwrap();
                RVInst::Sw {
                    rs2: RegAlloc::literal(src_reg),
                    rs1: "sp",
                    offset: *dest_offset as i32,
                }
                .emit_ident2(buf)?;

                // free the temporary register
                ctx.reg_allocator.as_mut().unwrap().free_reg(src_reg);
            }
            ValueKind::Store(store) => {
                // `Store` IR have not return value.
                // 1. load src(virtual reg/immediate) to temporary register
                // 2. store temporary register to dest (stack address)
                let src = store.value();
                let dest = store.dest();

                let src_reg;

                match ctx.stack_alloc.get(&src) {
                    // 如果 src 在栈上, 生成 lw
                    // virtual reg `src` Do not recursively dive into
                    Some(&src_offset) => {
                        src_reg = RegAlloc::literal(
                            ctx.reg_allocator
                                .as_mut()
                                .unwrap()
                                .alloc_reg(Some(*self))
                                .unwrap(),
                        );
                        RVInst::Lw {
                            rd: src_reg,
                            rs1: "sp",
                            offset: src_offset as i32,
                        }
                        .emit_ident2(buf)?;
                    }
                    None => {
                        // Recursive here.
                        let src_data = ctx.func_data().dfg().value(src);
                        let ValueKind::Integer(_) = src_data.kind() else {
                            panic!("Expect integer, got {:?}", src_data.kind())
                        };
                        src.generate(ctx, buf)?;

                        src_reg = query_reg_by_value!(ctx, src);
                    }
                }

                let Some(&dest_offset) = ctx.stack_alloc.get(&dest) else {
                    panic!("Unknown variable {:?}", dest);
                };
                RVInst::Sw {
                    rs2: src_reg,
                    rs1: "sp",
                    offset: dest_offset as i32,
                }
                .emit_ident2(buf)?;

                // free the temporary register
                ctx.reg_allocator.as_mut().unwrap().free_reg_lit(src_reg);
            }
            _ => unimplemented!(),
        }

        ctx.unset_cur_value();
        Ok(())
    }
}
