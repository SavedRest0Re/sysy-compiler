use std::collections::HashMap;

use koopa::ir::{BasicBlock, Function, FunctionData, Program, Value};

// t0-t6 for temporary registers
pub struct RegAlloc {
    regs: Vec<bool>,
    used_by_value: HashMap<Value, usize>,
}

impl RegAlloc {
    const REGS: [&str; 7] = ["t0", "t1", "t2", "t3", "t4", "t5", "t6"];

    pub fn new() -> Self {
        Self {
            regs: vec![false; 7],
            used_by_value: HashMap::new(),
        }
    }

    pub fn literal(id: usize) -> &'static str {
        RegAlloc::REGS[id]
    }

    pub fn alloc_reg(&mut self, value: Option<Value>) -> Option<usize> {
        // check if already allocated
        if let Some(v) = value {
            if let Some(&r) = self.used_by_value.get(&v) {
                return Some(r);
            }
        }

        for r in 0..self.regs.len() {
            if self.regs[r] == false {
                if let Some(v) = value {
                    self.used_by_value.insert(v, r);
                }
                self.regs[r] = true;

                return Some(r);
            }
        }
        None
    }

    pub fn free_reg(&mut self, reg: usize) {
        self.regs[reg] = false;
        self.used_by_value.retain(|&_, reg_idx| *reg_idx != reg);
    }

    pub fn free_reg_lit(&mut self, reg: &str) {
        for r in 0..RegAlloc::REGS.len() {
            if RegAlloc::REGS[r] == reg {
                self.regs[r] = false;
                self.used_by_value.retain(|&_, reg_idx| *reg_idx != r);
                break;
            }
        }
    }

    pub fn query_reg_by_value(&self, value: Value) -> Option<usize> {
        self.used_by_value.get(&value).copied()
    }
}

pub struct Ctx<'a> {
    program: &'a Program,
    cur_func: Option<Function>,
    cur_bb: Option<BasicBlock>,
    cur_value: Option<Value>,

    pub reg_allocator: Option<RegAlloc>,

    // Function stack frame size
    pub stack_size: usize,
    // virtual register -> offset relative to sp
    pub stack_alloc: HashMap<Value, usize>,

    // static call frame layout
    pub outgoing_args_size: usize,
    pub arg_save_base: usize, // a0-a7 保存位置, 相对 sp.
}

impl<'a> Ctx<'a> {
    pub fn new(program: &'a Program) -> Self {
        Self {
            program,
            cur_func: None,
            reg_allocator: None,
            cur_bb: None,
            cur_value: None,

            stack_size: 0,
            stack_alloc: HashMap::new(),

            outgoing_args_size: 0,
            arg_save_base: 0,
        }
    }

    pub fn program(&self) -> &'a Program {
        self.program
    }

    pub fn set_reg_alloc(&mut self, reg_alloc: RegAlloc) {
        self.reg_allocator = Some(reg_alloc);
    }

    pub fn unset_reg_alloc(&mut self) {
        self.reg_allocator = None;
    }

    pub fn set_cur_func(&mut self, func: Function) {
        self.cur_func = Some(func);
    }

    pub fn unset_cur_func(&mut self) {
        self.cur_func = None;
    }

    pub fn cur_func(&self) -> Option<Function> {
        self.cur_func
    }

    pub fn func_data(&self) -> &'a FunctionData {
        self.program.func(self.cur_func.unwrap())
    }

    pub fn set_cur_bb(&mut self, bb: BasicBlock) {
        self.cur_bb = Some(bb);
    }

    pub fn unset_cur_bb(&mut self) {
        self.cur_bb = None;
    }

    pub fn set_cur_value(&mut self, value: Value) {
        self.cur_value = Some(value);
    }

    pub fn unset_cur_value(&mut self) {
        self.cur_value = None;
    }

    pub fn cur_value(&self) -> Option<Value> {
        self.cur_value
    }

    pub fn with_value<F, R>(&mut self, value: Value, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let saved = self.cur_value;
        self.set_cur_value(value);
        let result = f(self);
        self.cur_value = saved;
        result
    }

    pub fn alloc_reg(&mut self, value: Option<Value>) -> usize {
        self.reg_allocator
            .as_mut()
            .expect("RegAlloc not initialized")
            .alloc_reg(value)
            .expect("No available registers")
    }

    pub fn free_reg(&mut self, reg: usize) {
        self.reg_allocator
            .as_mut()
            .expect("RegAlloc not initialized")
            .free_reg(reg);
    }

    pub fn free_reg_lit(&mut self, reg: &str) {
        self.reg_allocator
            .as_mut()
            .expect("RegAlloc not initialized")
            .free_reg_lit(reg);
    }

    /// Query register by Value, handles x0 special case for integer 0
    pub fn query_reg(&self, value: Value) -> &'static str {
        use koopa::ir::ValueKind;
        if let ValueKind::Integer(i) = self.func_data().dfg().value(value).kind() {
            if i.value() == 0 {
                return "x0";
            }
        }
        let reg = self
            .reg_allocator
            .as_ref()
            .expect("RegAlloc not initialized")
            .query_reg_by_value(value)
            .expect("Value not in register");
        RegAlloc::literal(reg)
    }

    pub fn stack_offset(&self, value: &Value) -> Option<i32> {
        self.stack_alloc.get(value).map(|&v| v as i32)
    }

    pub fn outgoing_arg_offset(&self, index: usize) -> i32 {
        (index * 4) as i32
    }

    pub fn saved_arg_offset(&self, index: usize) -> i32 {
        self.arg_save_base as i32 + (index * 4) as i32
    }

    pub fn incoming_stack_arg_offset(&self, index: usize) -> i32 {
        debug_assert!(index >= 8);
        self.stack_size as i32 + ((index - 8) * 4) as i32
    }

    pub fn ra_offset(&self) -> i32 {
        (self.stack_size - 4) as i32
    }

    pub fn bb_label(&self, bb: BasicBlock) -> String {
        let label = self.func_data().dfg().bb(bb).name().clone().unwrap();
        label.replace('%', "")
    }
}
