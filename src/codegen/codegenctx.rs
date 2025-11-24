use std::collections::HashMap;

use koopa::ir::{Function, Program, Value};

pub struct CodegenCtx<'p> {
    program: &'p Program,
    cur_func: Option<Function>,
    global_values: HashMap<Value, String>,
}

impl<'p> CodegenCtx<'p> {
    pub fn new(program: &'p Program) -> Self {
        Self {
            program,
            cur_func: None,
            global_values: HashMap::new(),
        }
    }

    pub fn program(&self) -> &'p Program {
        self.program
    }

    pub fn cur_func(&self) -> Option<&Function> {
        self.cur_func.as_ref()
    }

    pub fn cur_func_mut(&mut self) -> Option<&mut Function> {
        self.cur_func.as_mut()
    }

    pub fn set_cur_func(&mut self, func: Function) {
        self.cur_func = Some(func);
    }

    pub fn global_value(&self, value: Value) -> &str {
        self.global_values.get(&value).unwrap()
    }

    pub fn insert_global_value(&mut self, value: Value, name: String) {
        self.global_values.insert(value, name);
    }
}
