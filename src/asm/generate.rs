use koopa::ir::{
    BinaryOp, FunctionData, Program, ValueKind, entities::ValueData, layout::BasicBlockNode,
};

use std::{
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

        // Generate functions
        for &func in self.func_layout() {
            ctx.set_cur_func(func);
            ctx.set_func_alloc(RegAlloc::new());
            let func_data = self.func(func);
            func_data.generate(ctx, buf)?;
            ctx.unset_func_alloc();
            ctx.unset_cur_func();
        }

        Ok(())
    }
}

impl AsmGen for FunctionData {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx, buf: &mut File) -> Result<Self::Output> {
        let name = self.name().strip_prefix('@').unwrap();
        writeln!(buf, "  .global {name}")?;
        writeln!(buf, "{name}:")?;

        for (&bb, bbnode) in self.layout().bbs() {
            ctx.set_cur_bb(bb);
            bbnode.generate(ctx, buf)?;
            ctx.unset_cur_bb();
        }

        Ok(())
    }
}

impl AsmGen for BasicBlockNode {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx, buf: &mut File) -> Result<Self::Output> {
        for &inst in self.insts().keys() {
            ctx.set_cur_value(inst);
            let inst_data = ctx.func_data().dfg().value(inst).clone();
            inst_data.generate(ctx, buf)?;
            ctx.unset_cur_value();
        }

        Ok(())
    }
}

// koopa IR 中的 "虚拟寄存器" 使用 `Value` 来表示, Value 也是一条指令的 handle.

// TODO: 基于 Value 来 DFS 而不是 ValueData. 不用那么多 Saved-Restore
impl AsmGen for ValueData {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx, buf: &mut File) -> Result<Self::Output> {
        match self.kind() {
            ValueKind::Integer(i) => {
                // use x0 for constant 0, so we don't need to allocate a register
                if i.value() != 0 {
                    let cur_value = ctx.cur_value().unwrap();
                    let reg = ctx
                        .reg_allocator
                        .as_mut()
                        .unwrap()
                        .alloc_reg(cur_value)
                        .unwrap();
                    writeln!(
                        buf,
                        "{}",
                        RVInst::Li {
                            rd: RegAlloc::literal(reg),
                            imm12: i.value()
                        }
                        .to_asm_indent2()
                    )?;
                }
                return Ok(());
            }
            ValueKind::Binary(binary) => {
                // Generate asm for lhs
                let lhs_v = binary.lhs();
                let lhs_data = ctx.func_data().dfg().value(lhs_v).clone();
                // save the current value before DFS.
                let saved_value = ctx.cur_value().unwrap();
                ctx.set_cur_value(lhs_v);
                lhs_data.generate(ctx, buf)?;
                ctx.set_cur_value(saved_value);

                // Generate asm for rhs
                let rhs_v = binary.rhs();
                let rhs_data = ctx.func_data().dfg().value(rhs_v).clone();
                // save the current value before DFS.
                let saved_value = ctx.cur_value().unwrap();
                ctx.set_cur_value(rhs_v);
                rhs_data.generate(ctx, buf)?;
                ctx.set_cur_value(saved_value);

                let lhs_reg = query_reg_by_value!(ctx, lhs_v);
                let rhs_reg = query_reg_by_value!(ctx, rhs_v);
                let cur_value = ctx.cur_value().unwrap();
                let result_reg = ctx
                    .reg_allocator
                    .as_mut()
                    .unwrap()
                    .alloc_reg(cur_value)
                    .unwrap();

                match binary.op() {
                    BinaryOp::Add => {
                        writeln!(
                            buf,
                            "{}",
                            RVInst::Add {
                                rd: RegAlloc::literal(result_reg),
                                rs1: lhs_reg,
                                rs2: rhs_reg,
                            }
                            .to_asm_indent2()
                        )?;
                    }
                    BinaryOp::Sub => {
                        writeln!(
                            buf,
                            "{}",
                            RVInst::Sub {
                                rd: RegAlloc::literal(result_reg),
                                rs1: lhs_reg,
                                rs2: rhs_reg,
                            }
                            .to_asm_indent2()
                        )?;
                    }
                    BinaryOp::Mul => {
                        writeln!(
                            buf,
                            "{}",
                            RVInst::Mul {
                                rd: RegAlloc::literal(result_reg),
                                rs1: lhs_reg,
                                rs2: rhs_reg,
                            }
                            .to_asm_indent2()
                        )?;
                    }
                    BinaryOp::Div => {
                        writeln!(
                            buf,
                            "{}",
                            RVInst::Div {
                                rd: RegAlloc::literal(result_reg),
                                rs1: lhs_reg,
                                rs2: rhs_reg,
                            }
                            .to_asm_indent2()
                        )?;
                    }

                    BinaryOp::Mod => {
                        writeln!(
                            buf,
                            "{}",
                            RVInst::Rem {
                                rd: RegAlloc::literal(result_reg),
                                rs1: lhs_reg,
                                rs2: rhs_reg,
                            }
                            .to_asm_indent2()
                        )?;
                    }

                    BinaryOp::Eq => {
                        writeln!(
                            buf,
                            "{}",
                            RVInst::Xor {
                                rd: RegAlloc::literal(result_reg),
                                rs1: lhs_reg,
                                rs2: rhs_reg,
                            }
                            .to_asm_indent2()
                        )?;
                        writeln!(
                            buf,
                            "{}",
                            RVInst::Seqz {
                                rd: RegAlloc::literal(result_reg),
                                rs: RegAlloc::literal(result_reg),
                            }
                            .to_asm_indent2()
                        )?;
                    }
                    BinaryOp::NotEq => {
                        writeln!(
                            buf,
                            "{}",
                            RVInst::Xor {
                                rd: RegAlloc::literal(result_reg),
                                rs1: lhs_reg,
                                rs2: rhs_reg,
                            }
                            .to_asm_indent2()
                        )?;
                        writeln!(
                            buf,
                            "{}",
                            RVInst::Snez {
                                rd: RegAlloc::literal(result_reg),
                                rs: RegAlloc::literal(result_reg),
                            }
                            .to_asm_indent2()
                        )?;
                    }

                    BinaryOp::Lt => {
                        writeln!(
                            buf,
                            "{}",
                            RVInst::Slt {
                                rd: RegAlloc::literal(result_reg),
                                rs1: lhs_reg,
                                rs2: rhs_reg,
                            }
                            .to_asm_indent2()
                        )?;
                    }

                    BinaryOp::Le => {
                        writeln!(
                            buf,
                            "{}",
                            RVInst::Slt {
                                rd: RegAlloc::literal(result_reg),
                                rs1: rhs_reg,
                                rs2: lhs_reg,
                            }
                            .to_asm_indent2()
                        )?;
                        writeln!(
                            buf,
                            "{}",
                            RVInst::Seqz {
                                rd: RegAlloc::literal(result_reg),
                                rs: RegAlloc::literal(result_reg),
                            }
                            .to_asm_indent2()
                        )?;
                    }

                    BinaryOp::Gt => {
                        writeln!(
                            buf,
                            "{}",
                            RVInst::Slt {
                                rd: RegAlloc::literal(result_reg),
                                rs1: rhs_reg,
                                rs2: lhs_reg,
                            }
                            .to_asm_indent2()
                        )?;
                    }

                    BinaryOp::Ge => {
                        writeln!(
                            buf,
                            "{}",
                            RVInst::Slt {
                                rd: RegAlloc::literal(result_reg),
                                rs1: lhs_reg,
                                rs2: rhs_reg,
                            }
                            .to_asm_indent2()
                        )?;
                        writeln!(
                            buf,
                            "{}",
                            RVInst::Seqz {
                                rd: RegAlloc::literal(result_reg),
                                rs: RegAlloc::literal(result_reg),
                            }
                            .to_asm_indent2()
                        )?;
                    }

                    BinaryOp::And => {
                        writeln!(
                            buf,
                            "{}",
                            RVInst::And {
                                rd: RegAlloc::literal(result_reg),
                                rs1: lhs_reg,
                                rs2: rhs_reg,
                            }
                            .to_asm_indent2()
                        )?;
                    }

                    BinaryOp::Or => {
                        writeln!(
                            buf,
                            "{}",
                            RVInst::Or {
                                rd: RegAlloc::literal(result_reg),
                                rs1: lhs_reg,
                                rs2: rhs_reg,
                            }
                            .to_asm_indent2()
                        )?;
                    }

                    _ => unimplemented!(),
                };

                // FIXME: 释放 lhs 和 rhs 寄存器
                ctx.reg_allocator.as_mut().unwrap().free_reg_lit(lhs_reg);
                ctx.reg_allocator.as_mut().unwrap().free_reg_lit(rhs_reg);

                return Ok(());
            }
            ValueKind::Return(ret) => {
                if let Some(retval) = ret.value() {
                    let retval_data = ctx.func_data().dfg().value(retval).clone();

                    // save the current value before DFS.
                    let saved_value = ctx.cur_value().unwrap();
                    ctx.set_cur_value(retval);
                    retval_data.generate(ctx, buf)?;
                    ctx.set_cur_value(saved_value);

                    let ret_reg = query_reg_by_value!(ctx, retval);

                    writeln!(
                        buf,
                        "{}",
                        RVInst::Mv {
                            rd: "a0",
                            rs: ret_reg,
                        }
                        .to_asm_indent2()
                    )?;
                    writeln!(buf, "{}", RVInst::Ret.to_asm_indent2())?;
                } else {
                    writeln!(buf, "{}", RVInst::Ret.to_asm_indent2())?;
                }

                return Ok(());
            }
            _ => unimplemented!(),
        }
        Ok(())
    }
}
