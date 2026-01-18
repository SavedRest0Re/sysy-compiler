use std::collections::HashMap;

use koopa::ir::{
    BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value, ValueKind,
    builder::{BasicBlockBuilder, GlobalInstBuilder, LocalInstBuilder, ValueBuilder},
};

use crate::ir::{Error, IRResult};

#[derive(Clone, Copy)]
pub enum Symbol {
    Const(i32),
    Var(Value),
}

pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>, // no `@` prefix
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()], // init with global scope
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn define(&mut self, name: String, symbol: Symbol) -> IRResult<()> {
        if self.scopes.last().unwrap().contains_key(&name) {
            return Err(Error::DuplicatedDef);
        }
        self.scopes.last_mut().unwrap().insert(name, symbol);
        Ok(())
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }
}

pub struct Ctx {
    pub funcs: HashMap<String, Function>, // no `@` prefix

    pub program: Program,
    pub cur_func: Option<Function>,
    pub cur_bb: Option<BasicBlock>,
    pub loop_stack: Vec<(BasicBlock, BasicBlock)>, // (cond_bb, end_bb)

    pub symbol_table: SymbolTable,
    counter: u32,
}

impl Ctx {
    pub fn new() -> Self {
        Self {
            funcs: HashMap::new(),

            program: Program::new(),
            cur_func: None,
            cur_bb: None,
            loop_stack: Vec::new(),

            symbol_table: SymbolTable::new(),
            counter: 0,
        }
    }

    pub fn is_global(&self) -> bool {
        self.symbol_table.scopes.len() == 1
    }

    pub fn set_cur_func(&mut self, func: Function) {
        self.cur_func = Some(func);

        self.cur_bb = None;
        self.loop_stack = vec![];
    }

    pub fn unset_cur_func(&mut self) {
        self.cur_func = None;

        self.cur_bb = None;
        self.loop_stack = vec![];
    }

    pub fn func_data(&self) -> &FunctionData {
        self.program.func(self.cur_func.unwrap())
    }

    pub fn func_data_mut(&mut self) -> &mut FunctionData {
        self.program.func_mut(self.cur_func.unwrap())
    }

    pub fn func_params(&self) -> &[Value] {
        self.func_data().params()
    }

    pub fn fetch_counter(&mut self) -> u32 {
        let counter = self.counter;
        self.counter += 1;
        counter
    }

    pub fn cur_bb(&self) -> BasicBlock {
        self.cur_bb.expect("no current basic block")
    }

    pub fn set_cur_bb(&mut self, bb: BasicBlock) {
        self.cur_bb = Some(bb);
    }

    pub fn unset_cur_bb(&mut self) {
        self.cur_bb = None;
    }

    pub fn create_bb(&mut self, name: Option<&str>) -> BasicBlock {
        let bb = self
            .func_data_mut()
            .dfg_mut()
            .new_bb()
            .basic_block(name.map(|s| s.to_string()));
        self.func_data_mut()
            .layout_mut()
            .bbs_mut()
            .push_key_back(bb)
            .unwrap();
        bb
    }

    // ========== Instruction Emission ==========
    pub fn emit_integer(&mut self, value: i32) -> Value {
        self.func_data_mut().dfg_mut().new_value().integer(value)
    }

    pub fn emit_binary(&mut self, op: BinaryOp, lhs: Value, rhs: Value) -> Value {
        let inst = self
            .func_data_mut()
            .dfg_mut()
            .new_value()
            .binary(op, lhs, rhs);
        self.add_inst(inst);
        inst
    }

    pub fn emit_load(&mut self, ptr: Value) -> Value {
        let inst = self.func_data_mut().dfg_mut().new_value().load(ptr);
        self.add_inst(inst);
        inst
    }

    pub fn emit_store(&mut self, value: Value, ptr: Value) {
        let inst = self.func_data_mut().dfg_mut().new_value().store(value, ptr);
        self.add_inst(inst);
    }

    pub fn emit_ret(&mut self, value: Option<Value>) {
        let inst = self.func_data_mut().dfg_mut().new_value().ret(value);
        self.add_inst(inst);
    }

    pub fn emit_jump(&mut self, target: BasicBlock) {
        let inst = self.func_data_mut().dfg_mut().new_value().jump(target);
        self.add_inst(inst);
    }

    pub fn emit_br_if_needed(&mut self, target: BasicBlock) {
        if !self.is_cur_bb_terminated() {
            self.emit_jump(target);
        }
    }

    pub fn emit_cond_br(&mut self, cond: Value, then_bb: BasicBlock, else_bb: BasicBlock) {
        let inst = self
            .func_data_mut()
            .dfg_mut()
            .new_value()
            .branch(cond, then_bb, else_bb);
        self.add_inst(inst);
    }

    pub fn emit_call(&mut self, func: Function, args: Vec<Value>) -> Value {
        let inst = self.func_data_mut().dfg_mut().new_value().call(func, args);
        self.add_inst(inst);
        inst
    }

    pub fn emit_alloc(&mut self, ty: Type) -> Value {
        let inst = self.func_data_mut().dfg_mut().new_value().alloc(ty);
        self.add_inst(inst);
        inst
    }

    pub fn emit_global_alloc(&mut self, init: Value) -> Value {
        let inst = self.program.new_value().global_alloc(init);
        inst
    }

    pub fn set_value_name(&mut self, value: Value, name: String) {
        self.func_data_mut()
            .dfg_mut()
            .set_value_name(value, Some(name));
    }

    fn add_inst(&mut self, inst: Value) {
        let bb = self.cur_bb();
        self.func_data_mut()
            .layout_mut()
            .bb_mut(bb)
            .insts_mut()
            .push_key_back(inst)
            .unwrap();
    }

    pub fn is_bb_terminated(&self, bb: BasicBlock) -> bool {
        let bb_node = self
            .func_data()
            .layout()
            .bbs()
            .node(&bb)
            .expect("basic block not in layout");

        let Some(&last_inst) = bb_node.insts().keys().last() else {
            return false;
        };

        match self.func_data().dfg().value(last_inst).kind() {
            ValueKind::Jump(_) | ValueKind::Branch(_) | ValueKind::Return(_) => true,
            _ => false,
        }
    }

    pub fn is_cur_bb_terminated(&self) -> bool {
        let Some(bb) = self.cur_bb else {
            return true;
        };
        self.is_bb_terminated(bb)
    }
}
