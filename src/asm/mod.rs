mod ctx;
pub mod instruction;

use ctx::CodegenCtx;
use koopa::ir::{
    BinaryOp, FunctionData, Program, ValueKind, entities::ValueData, layout::BasicBlockNode,
};
use std::fs::File;
use std::io::{Result, Write};

pub fn generate_asm(program: &Program, buf: &mut File) {
    let mut ctx = CodegenCtx::new(program);
    program.generate(&mut ctx, buf).unwrap();
}

trait GenerateAsm {
    type Output;
    fn generate(&self, ctx: &mut CodegenCtx, buf: &mut File) -> Result<Self::Output>;
}

impl GenerateAsm for Program {
    type Output = ();

    fn generate(&self, ctx: &mut CodegenCtx, buf: &mut File) -> Result<Self::Output> {
        buf.write_all(b"  .text\n")?;

        // Generate global allocations

        // Generate functions
        for &func in self.func_layout() {
            ctx.set_cur_func(func);
            self.func(func).generate(ctx, buf)?;
        }

        Ok(())
    }
}

impl GenerateAsm for FunctionData {
    type Output = ();

    fn generate(&self, ctx: &mut CodegenCtx, buf: &mut File) -> Result<Self::Output> {
        buf.write_all(
            format!("  .global {}\n", self.name().strip_prefix('@').unwrap()).as_bytes(),
        )?;
        buf.write_all(format!("{}:\n", self.name().strip_prefix('@').unwrap()).as_bytes())?;

        for (&bb, node) in self.layout().bbs() {
            node.generate(ctx, buf)?;
        }
        Ok(())
    }
}

impl GenerateAsm for BasicBlockNode {
    type Output = ();

    fn generate(&self, ctx: &mut CodegenCtx, buf: &mut File) -> Result<Self::Output> {
        if let Some(&inst) = self.insts().keys().last() {
            let value_data = ctx
                .program()
                .func(*ctx.cur_func().unwrap())
                .dfg()
                .value(inst);
            value_data.generate(ctx, buf)?;
        }
        Ok(())
    }
}

impl GenerateAsm for ValueData {
    type Output = ();

    fn generate(&self, ctx: &mut CodegenCtx, buf: &mut File) -> Result<Self::Output> {
        dbg!(&self);
        match self.kind() {
            ValueKind::Integer(i) => {
                buf.write_all(format!("  li a0, {}\n", i.value()).as_bytes())?
            }
            ValueKind::Binary(bin) => {
                let func = ctx.program().func(*ctx.cur_func().unwrap());
                let dfg = func.dfg();

                // Evaluate lhs
                dfg.value(bin.lhs()).generate(ctx, buf)?;
                // Push lhs (a0) onto stack
                buf.write_all(b"  addi sp, sp, -4\n")?;
                buf.write_all(b"  sw a0, 0(sp)\n")?;

                // Evaluate rhs
                dfg.value(bin.rhs()).generate(ctx, buf)?;

                // Pop lhs into t0
                buf.write_all(b"  lw t0, 0(sp)\n")?;
                buf.write_all(b"  addi sp, sp, 4\n")?;

                // Now: t0 = lhs, a0 = rhs
                match bin.op() {
                    BinaryOp::Add => {
                        buf.write_all(b"  add a0, t0, a0\n")?;
                    }
                    BinaryOp::Sub => {
                        buf.write_all(b"  sub a0, t0, a0\n")?;
                    }
                    BinaryOp::Mul => {
                        buf.write_all(b"  mul a0, t0, a0\n")?;
                    }
                    BinaryOp::Div => {
                        buf.write_all(b"  div a0, t0, a0\n")?;
                    }
                    BinaryOp::Mod => {
                        buf.write_all(b"  rem a0, t0, a0\n")?;
                    }
                    BinaryOp::Lt => {
                        buf.write_all(b"  slt a0, t0, a0\n")?;
                    }
                    BinaryOp::Gt => {
                        buf.write_all(b"  slt a0, a0, t0\n")?;
                    }
                    BinaryOp::Le => {
                        buf.write_all(b"  slt a0, a0, t0\n")?;
                        buf.write_all(b"  seqz a0, a0\n")?;
                    }
                    BinaryOp::Ge => {
                        buf.write_all(b"  slt a0, t0, a0\n")?;
                        buf.write_all(b"  seqz a0, a0\n")?;
                    }
                    BinaryOp::Eq => {
                        buf.write_all(b"  sub a0, t0, a0\n")?;
                        buf.write_all(b"  seqz a0, a0\n")?;
                    }
                    BinaryOp::NotEq => {
                        buf.write_all(b"  sub a0, t0, a0\n")?;
                        buf.write_all(b"  snez a0, a0\n")?;
                    }
                    // Logical && and || without short-circuit semantics
                    BinaryOp::And => {
                        buf.write_all(b"  snez t0, t0\n")?;
                        buf.write_all(b"  snez a0, a0\n")?;
                        buf.write_all(b"  and a0, t0, a0\n")?;
                    }
                    BinaryOp::Or => {
                        buf.write_all(b"  snez t0, t0\n")?;
                        buf.write_all(b"  snez a0, a0\n")?;
                        buf.write_all(b"  or a0, t0, a0\n")?;
                    }
                    _ => unimplemented!(),
                }
            }
            ValueKind::Return(ret) => {
                if let Some(ret) = ret.value() {
                    ctx.program()
                        .func(*ctx.cur_func().unwrap())
                        .dfg()
                        .value(ret)
                        .generate(ctx, buf)?;
                }
                buf.write_all(b"  ret\n")?;
            }
            _ => todo!(),
        }
        Ok(())
    }
}
