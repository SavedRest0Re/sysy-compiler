mod codegenctx;

use codegenctx::CodegenCtx;
use koopa::ir::{FunctionData, Program, ValueKind, entities::ValueData, layout::BasicBlockNode};
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
        for &inst in self.insts().keys() {
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
