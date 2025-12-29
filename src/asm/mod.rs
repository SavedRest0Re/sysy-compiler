mod ctx;
pub mod generate;
pub mod instruction;

use ctx::Ctx;

use std::fs::File;

use koopa::ir::Program;

use crate::asm::generate::AsmGen;

pub fn generate_asm(program: &Program, buf: &mut File) {
    let mut ctx = Ctx::new(program);
    program.generate(&mut ctx, buf).unwrap();
}
