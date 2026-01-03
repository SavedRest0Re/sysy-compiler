mod const_eval;
mod ctx;
mod generate;
pub mod macros;

use std::fmt::{self};

use koopa::ir::Program;

use crate::{
    ast::*,
    ir::{ctx::Ctx, generate::IRGen},
};

pub type IRResult<T> = std::result::Result<T, Error>;

pub enum Error {
    DuplicatedDef,
    SymbolNotFound,
    FailedToEval,
    InvalidArrayLen,
    InvalidInit,
    ArrayAssign,
    NotInLoop,
    RetValInVoidFunc,
    DerefInt,
    UseVoidValue,
    ArgMismatch,
    NonIntCalc,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::DuplicatedDef => write!(f, "duplicated symbol definition"),
            Self::SymbolNotFound => write!(f, "symbol not found"),
            Self::FailedToEval => write!(f, "failed to evaluate constant"),
            Self::InvalidArrayLen => write!(f, "invalid array length"),
            Self::InvalidInit => write!(f, "invalid initializer"),
            Self::ArrayAssign => write!(f, "assigning to array"),
            Self::NotInLoop => write!(f, "using break/continue outside of loop"),
            Self::RetValInVoidFunc => write!(f, "returning value in void fucntion"),
            Self::DerefInt => write!(f, "dereferencing an integer"),
            Self::UseVoidValue => write!(f, "using a void value"),
            Self::ArgMismatch => write!(f, "argument mismatch"),
            Self::NonIntCalc => write!(f, "non-integer calculation"),
        }
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

pub fn generate_program(comp_unit: CompUnit) -> IRResult<Program> {
    let mut ctx = Ctx::new();
    comp_unit.generate(&mut ctx)?;
    Ok(ctx.program)
}
