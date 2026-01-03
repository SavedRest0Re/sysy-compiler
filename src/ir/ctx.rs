use std::collections::HashMap;

use koopa::ir::{Function, FunctionData, Program, Value};

use crate::ir::{Error, IRResult};

#[derive(Clone, Copy)]
pub enum Symbol {
    Const(i32),
    Var(Value),
}

pub struct SymbolTable {
    table: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, symbol: Symbol) -> IRResult<()> {
        if self.table.contains_key(&name) {
            return Err(Error::DuplicatedDef);
        }
        self.table.insert(name, symbol);
        Ok(())
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.table.get(name)
    }
}

pub struct Ctx {
    pub program: Program,
    pub cur_func: Option<Function>,
    pub symbol_table: SymbolTable,
}

impl Ctx {
    pub fn new() -> Self {
        Self {
            program: Program::new(),
            cur_func: None,
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn set_cur_func(&mut self, func: Function) {
        self.cur_func = Some(func);
    }

    pub fn unset_cur_func(&mut self) {
        self.cur_func = None;
    }

    pub fn func_data(&self) -> &FunctionData {
        self.program.func(self.cur_func.unwrap())
    }

    pub fn func_data_mut(&mut self) -> &mut FunctionData {
        self.program.func_mut(self.cur_func.unwrap())
    }
}
