use koopa::ir::{Function, FunctionData, Program};

pub struct Ctx {
    pub program: Program,
    pub cur_func: Option<Function>,
}

impl Ctx {
    pub fn new() -> Self {
        Self {
            program: Program::new(),
            cur_func: None,
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
