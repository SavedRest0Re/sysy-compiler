use std::fmt::{self};

use koopa::ir::{
    FunctionData, Program, Type,
    builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
};

use crate::{add_bb, add_inst, ast::*, new_bb, new_value};

pub type Result<T> = std::result::Result<T, Error>;

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

pub fn generate_program(comp_unit: &CompUnit) -> Result<Program> {
    let mut program = Program::new();
    comp_unit.generate(&mut program)?;
    Ok(program)
}

pub trait IRGen<'ast> {
    type Output;

    fn generate(&'ast self, program: &mut Program) -> Result<Self::Output>;
}

impl<'ast> IRGen<'ast> for CompUnit {
    type Output = ();

    fn generate(&'ast self, program: &mut Program) -> Result<Self::Output> {
        self.func_def.generate(program)
    }
}

impl<'ast> IRGen<'ast> for FuncDef {
    type Output = ();

    fn generate(&'ast self, program: &mut Program) -> Result<Self::Output> {
        // let ret_ty = self.ret_ty.generate(program)?;

        let func_def = FunctionData::new(format!("@{}", self.ident), vec![], Type::get_i32());
        let func = program.new_func(func_def);

        let func_data = program.func_mut(func);
        // Create basic block
        let entry_bb = new_bb!(func_data).basic_block(Some("%entry".into()));

        // Add the basic block to the function's layout first
        add_bb!(func_data, entry_bb);

        // Create values after the function is added to the program
        let ret_value = new_value!(func_data).integer(self.block.stmt.num);
        let ret_inst = new_value!(func_data).ret(Some(ret_value));

        add_inst!(func_data, entry_bb, ret_inst);

        Ok(())
    }
}

impl<'ast> IRGen<'ast> for FuncType {
    type Output = Type;

    fn generate(&'ast self, _: &mut Program) -> Result<Self::Output> {
        Ok(match self {
            FuncType::Int => Type::get_i32(),
        })
    }
}

impl<'ast> IRGen<'ast> for Block {
    type Output = ();

    fn generate(&'ast self, program: &mut Program) -> Result<Self::Output> {
        todo!()
        // self.stmt.generate(program)
    }
}

impl<'ast> IRGen<'ast> for Stmt {
    type Output = i32;

    fn generate(&'ast self, _: &mut Program) -> Result<Self::Output> {
        Ok(self.num)
    }
}
