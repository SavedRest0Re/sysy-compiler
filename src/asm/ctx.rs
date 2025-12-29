use std::collections::HashMap;

use koopa::ir::{BasicBlock, Function, FunctionData, Program, Value};

// t0-t6 + a1-a7 for temporary registers
pub struct RegAlloc {
    regs: Vec<bool>,
    used_by_value: HashMap<Value, usize>,
}

impl RegAlloc {
    const REGS: [&str; 14] = [
        "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a1", "a2", "a3", "a4", "a5", "a6", "a7",
    ];

    pub fn new() -> Self {
        Self {
            regs: vec![false; 7 + 7],
            used_by_value: HashMap::new(),
        }
    }

    pub fn literal(id: usize) -> &'static str {
        RegAlloc::REGS[id]
    }

    pub fn alloc_reg(&mut self, value: Value) -> Option<usize> {
        // check if already allocated
        if let Some(&r) = self.used_by_value.get(&value) {
            return Some(r);
        }

        for r in 0..self.regs.len() {
            if self.regs[r] == false {
                self.used_by_value.insert(value, r);
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
    pub reg_allocator: Option<RegAlloc>,
    cur_bb: Option<BasicBlock>,
    cur_value: Option<Value>,
}

impl<'a> Ctx<'a> {
    pub fn new(program: &'a Program) -> Self {
        Self {
            program,
            cur_func: None,
            reg_allocator: None,
            cur_bb: None,
            cur_value: None,
        }
    }

    pub fn set_func_alloc(&mut self, func_alloc: RegAlloc) {
        self.reg_allocator = Some(func_alloc);
    }

    pub fn unset_func_alloc(&mut self) {
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

    pub fn func_data(&self) -> &FunctionData {
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
}
