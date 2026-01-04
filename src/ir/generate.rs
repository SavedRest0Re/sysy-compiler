use koopa::ir::{
    BinaryOp, FunctionData, Type, Value,
    builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
};

use crate::{
    add_bb, add_inst,
    ast::{
        AddExp, AddOp, Block, BlockItem, CompUnit, ConstDef, ConstInitVal, Decl, EqExp, EqOp, Exp,
        FuncDef, FuncType, InitVal, LAndExp, LOrExp, LVal, MulExp, MulOp, PrimaryExp, RelExp,
        RelOp, Stmt, UnaryExp, UnaryOp, VarDef,
    },
    entry_bb,
    ir::{
        Error, IRResult,
        const_eval::consteval,
        ctx::{Ctx, Symbol},
    },
    new_bb, new_value,
};

pub trait IRGen {
    type Output;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output>;
}

impl IRGen for CompUnit {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        self.func_def.generate(ctx)
    }
}

impl IRGen for FuncDef {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        let func = ctx.program.new_func(FunctionData::new(
            format!("@{}", self.ident),
            vec![],
            Type::get_i32(),
        ));

        ctx.set_cur_func(func);

        let entry_bb = new_bb!(ctx.func_data_mut()).basic_block(Some("%entry".into()));
        add_bb!(ctx.func_data_mut(), entry_bb);

        self.block.generate(ctx)?;

        ctx.unset_cur_func();

        Ok(())
    }
}

impl IRGen for FuncType {
    type Output = Type;

    fn generate(&self, _ctx: &mut Ctx) -> IRResult<Self::Output> {
        Ok(match self {
            FuncType::Int => Type::get_i32(),
        })
    }
}

impl IRGen for Block {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        ctx.symbol_table.enter_scope();
        for item in self.items.iter() {
            match item {
                BlockItem::Decl(decl) => decl.generate(ctx)?,
                BlockItem::Stmt(stmt) => stmt.generate(ctx)?,
            };
        }
        ctx.symbol_table.exit_scope();
        Ok(())
    }
}

impl IRGen for Stmt {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            Stmt::Assign(lval, exp) => {
                let entry_bb = entry_bb!(ctx);

                let rhs_value = exp.generate(ctx)?;

                let LVal::Ident(ident) = lval;

                let var = match ctx.symbol_table.resolve(ident) {
                    Some(sym) => match sym {
                        Symbol::Const(_) => panic!("\"{}\" is a constant, not a variable", ident),
                        Symbol::Var(var) => *var,
                    },
                    None => panic!("undefined: \"{}\"", ident),
                };

                let store_inst = new_value!(ctx.func_data_mut()).store(rhs_value, var);
                add_inst!(ctx.func_data_mut(), entry_bb, store_inst);
            }
            Stmt::Return(exp) => {
                let entry_bb = entry_bb!(ctx);

                let result = exp.generate(ctx)?;
                let ret_inst = new_value!(ctx.func_data_mut()).ret(Some(result));
                add_inst!(ctx.func_data_mut(), entry_bb, ret_inst);
            }
            Stmt::Exp(exp) => {
                if let Some(exp) = exp {
                    exp.generate(ctx)?;
                }
            }
            Stmt::Block(block) => block.generate(ctx)?,
        }
        Ok(())
    }
}

impl IRGen for Decl {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            Decl::Const(const_decl) => {
                for def in const_decl.const_defs.iter() {
                    def.generate(ctx)?;
                }
                Ok(())
            }
            Decl::Var(var_decl) => {
                for def in var_decl.defs.iter() {
                    def.generate(ctx)?;
                }
                Ok(())
            }
        }
    }
}

impl IRGen for ConstDef {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        let result = self.init_val.generate(ctx)?;
        ctx.symbol_table
            .define(self.ident.clone(), Symbol::Const(result))?;
        Ok(())
    }
}

impl IRGen for ConstInitVal {
    type Output = i32;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        let ConstInitVal::ConstExp(const_exp) = self;
        Ok(consteval(const_exp, ctx))
    }
}

impl IRGen for VarDef {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        let entry_bb = entry_bb!(ctx);

        let var_alloc_inst = new_value!(ctx.func_data_mut()).alloc(Type::get_i32()); // BType must be i32
        let unique_name = ctx.unique_name(&self.ident);
        ctx.func_data_mut()
            .dfg_mut()
            .set_value_name(var_alloc_inst, Some(unique_name));
        add_inst!(ctx.func_data_mut(), entry_bb, var_alloc_inst);

        if let Some(ref init_val) = self.init_val {
            let InitVal::Exp(exp) = init_val;

            let init_value = exp.generate(ctx)?;
            let store_inst = new_value!(ctx.func_data_mut()).store(init_value, var_alloc_inst);
            add_inst!(ctx.func_data_mut(), entry_bb, store_inst);
        }

        ctx.symbol_table
            .define(self.ident.clone(), Symbol::Var(var_alloc_inst))?;

        Ok(())
    }
}

impl IRGen for Exp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            Exp::LOr(lor_exp) => lor_exp.generate(ctx),
        }
    }
}

impl IRGen for LOrExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            LOrExp::LAnd(l_and_exp) => l_and_exp.generate(ctx),
            LOrExp::LOrLAnd(l, r) => {
                let lval = l.generate(ctx)?;
                let rval = r.generate(ctx)?;

                // l || r => (l != 0) || (r != 0)
                let entry_bb = entry_bb!(ctx);
                let zero = new_value!(ctx.func_data_mut()).integer(0);

                let lnez = new_value!(ctx.func_data_mut()).binary(BinaryOp::NotEq, lval, zero);
                add_inst!(ctx.func_data_mut(), entry_bb, lnez);

                let rnez = new_value!(ctx.func_data_mut()).binary(BinaryOp::NotEq, rval, zero);
                add_inst!(ctx.func_data_mut(), entry_bb, rnez);

                let or_inst = new_value!(ctx.func_data_mut()).binary(BinaryOp::Or, lnez, rnez);
                add_inst!(ctx.func_data_mut(), entry_bb, or_inst);

                Ok(or_inst)
            }
        }
    }
}

impl IRGen for LAndExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            LAndExp::Eq(eq_exp) => eq_exp.generate(ctx),
            LAndExp::LAndEq(l, r) => {
                let l_val = l.generate(ctx)?;
                let r_val = r.generate(ctx)?;

                // l && r => (l != 0) && (r != 0)
                let entry_bb = entry_bb!(ctx);
                let zero = new_value!(ctx.func_data_mut()).integer(0);

                let lnez = new_value!(ctx.func_data_mut()).binary(BinaryOp::NotEq, l_val, zero);
                add_inst!(ctx.func_data_mut(), entry_bb, lnez);

                let rnez = new_value!(ctx.func_data_mut()).binary(BinaryOp::NotEq, r_val, zero);
                add_inst!(ctx.func_data_mut(), entry_bb, rnez);

                let and_inst = new_value!(ctx.func_data_mut()).binary(BinaryOp::And, lnez, rnez);
                add_inst!(ctx.func_data_mut(), entry_bb, and_inst);

                Ok(and_inst)
            }
        }
    }
}

impl IRGen for EqExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            EqExp::Rel(rel_exp) => rel_exp.generate(ctx),
            EqExp::EqRel(l, eq_op, r) => {
                let l_val = l.generate(ctx)?;
                let r_val = r.generate(ctx)?;

                let op = match eq_op {
                    EqOp::Eq => BinaryOp::Eq,
                    EqOp::Ne => BinaryOp::NotEq,
                };

                let entry_bb = entry_bb!(ctx);
                let eq_inst = new_value!(ctx.func_data_mut()).binary(op, l_val, r_val);
                add_inst!(ctx.func_data_mut(), entry_bb, eq_inst);

                Ok(eq_inst)
            }
        }
    }
}

impl IRGen for RelExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            RelExp::Add(add_exp) => add_exp.generate(ctx),
            RelExp::RelAdd(l, rel_op, r) => {
                let l_val = l.generate(ctx)?;
                let r_val = r.generate(ctx)?;

                let op = match rel_op {
                    RelOp::Lt => BinaryOp::Lt,
                    RelOp::Gt => BinaryOp::Gt,
                    RelOp::Le => BinaryOp::Le,
                    RelOp::Ge => BinaryOp::Ge,
                };

                let entry_bb = entry_bb!(ctx);
                let rel_inst = new_value!(ctx.func_data_mut()).binary(op, l_val, r_val);
                add_inst!(ctx.func_data_mut(), entry_bb, rel_inst);

                Ok(rel_inst)
            }
        }
    }
}

impl IRGen for AddExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            AddExp::Mul(mul_exp) => mul_exp.generate(ctx),
            AddExp::AddMul(l, add_op, r) => {
                let l_val = l.generate(ctx)?;
                let r_val = r.generate(ctx)?;

                let op = match add_op {
                    AddOp::Add => BinaryOp::Add,
                    AddOp::Sub => BinaryOp::Sub,
                };

                let entry_bb = entry_bb!(ctx);
                let add_inst = new_value!(ctx.func_data_mut()).binary(op, l_val, r_val);
                add_inst!(ctx.func_data_mut(), entry_bb, add_inst);

                Ok(add_inst)
            }
        }
    }
}

impl IRGen for MulExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            MulExp::Unary(unary_exp) => unary_exp.generate(ctx),
            MulExp::MulUnary(l, mul_op, r) => {
                let l_val = l.generate(ctx)?;
                let r_val = r.generate(ctx)?;

                let op = match mul_op {
                    MulOp::Mul => BinaryOp::Mul,
                    MulOp::Div => BinaryOp::Div,
                    MulOp::Mod => BinaryOp::Mod,
                };

                let entry_bb = entry_bb!(ctx);
                let mul_inst = new_value!(ctx.func_data_mut()).binary(op, l_val, r_val);
                add_inst!(ctx.func_data_mut(), entry_bb, mul_inst);

                Ok(mul_inst)
            }
        }
    }
}

impl IRGen for UnaryExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.generate(ctx),
            UnaryExp::Unary(op, exp) => {
                let exp_val = exp.generate(ctx)?;

                let entry_bb = entry_bb!(ctx);
                let zero = new_value!(ctx.func_data_mut()).integer(0);

                let result = match op {
                    UnaryOp::Plus => exp_val,
                    UnaryOp::Minus => {
                        let sub_inst =
                            new_value!(ctx.func_data_mut()).binary(BinaryOp::Sub, zero, exp_val);
                        add_inst!(ctx.func_data_mut(), entry_bb, sub_inst);
                        sub_inst
                    }
                    UnaryOp::Not => {
                        let eq_inst =
                            new_value!(ctx.func_data_mut()).binary(BinaryOp::Eq, exp_val, zero);
                        add_inst!(ctx.func_data_mut(), entry_bb, eq_inst);
                        eq_inst
                    }
                };

                Ok(result)
            }
        }
    }
}

impl IRGen for PrimaryExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            PrimaryExp::Number(num) => Ok(new_value!(ctx.func_data_mut()).integer(*num)),
            PrimaryExp::Paren(exp) => exp.generate(ctx),
            PrimaryExp::LVal(lval) => {
                let LVal::Ident(ident) = lval;
                let sym = *ctx
                    .symbol_table
                    .resolve(ident)
                    .ok_or(Error::SymbolNotFound)?;

                match sym {
                    Symbol::Const(c) => Ok(new_value!(ctx.func_data_mut()).integer(c)),
                    Symbol::Var(var) => {
                        let entry_bb = entry_bb!(ctx);
                        let load_inst = new_value!(ctx.func_data_mut()).load(var);
                        add_inst!(ctx.func_data_mut(), entry_bb, load_inst);
                        Ok(load_inst)
                    }
                }
            }
        }
    }
}
