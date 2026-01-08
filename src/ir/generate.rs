use koopa::ir::{BinaryOp, FunctionData, Type, Value};

use crate::{
    ast::{
        AddExp, Block, BlockItem, CompUnit, ConstDef, ConstInitVal, Decl, EqExp, Exp, FuncDef,
        FuncType, InitVal, LAndExp, LOrExp, LVal, MulExp, PrimaryExp, RelExp, Stmt, UnaryExp,
        UnaryOp, VarDef,
    },
    ir::{
        Error, IRResult,
        const_eval::consteval,
        ctx::{Ctx, Symbol},
    },
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

        // Create entry basic block and set it as current
        let entry_bb = ctx.create_bb(Some("%entry"));
        ctx.set_cur_bb(entry_bb);

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
            if ctx.is_cur_bb_terminated() {
                break;
            }
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
                let rhs_value = exp.generate(ctx)?;

                let LVal::Ident(ident) = lval;

                let var = match ctx.symbol_table.resolve(ident) {
                    Some(sym) => match sym {
                        Symbol::Const(_) => panic!("\"{ident}\" is a constant, not a variable"),
                        Symbol::Var(var) => *var,
                    },
                    None => panic!("undefined: \"{ident}\""),
                };

                ctx.emit_store(rhs_value, var);
            }
            Stmt::Return(exp) => {
                let result = exp.generate(ctx)?;
                ctx.emit_ret(Some(result));
                // return must terminate the basic block
                // ctx.unset_cur_bb();
            }
            Stmt::Exp(exp) => {
                if let Some(exp) = exp {
                    exp.generate(ctx)?;
                }
            }
            Stmt::Block(block) => block.generate(ctx)?,
            Stmt::If(cond, then_stmt, else_stmt) => {
                let counter = ctx.fetch_counter();

                let cond_val = cond.generate(ctx)?;
                let then_bb = ctx.create_bb(Some(&format!("%then_{counter}")));
                let end_bb = ctx.create_bb(Some(&format!("%endif_{counter}")));

                if let Some(else_stmt) = else_stmt {
                    let else_bb = ctx.create_bb(Some(&format!("%else_{counter}")));
                    ctx.emit_cond_br(cond_val, then_bb, else_bb);

                    ctx.set_cur_bb(then_bb);
                    then_stmt.generate(ctx)?;
                    ctx.emit_br_if_needed(end_bb); // maybe `ret`

                    ctx.set_cur_bb(else_bb);
                    else_stmt.generate(ctx)?;
                    ctx.emit_br_if_needed(end_bb);
                } else {
                    ctx.emit_cond_br(cond_val, then_bb, end_bb);

                    ctx.set_cur_bb(then_bb);
                    then_stmt.generate(ctx)?;
                    ctx.emit_br_if_needed(end_bb);
                }

                ctx.set_cur_bb(end_bb);
            }
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
        Ok(consteval(const_exp, ctx)?)
    }
}

impl IRGen for VarDef {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        let var_alloc = ctx.emit_alloc(Type::get_i32()); // BType must be i32
        let vir_reg_name = ctx.unique_name(format!("@{}", self.ident));
        ctx.set_value_name(var_alloc, vir_reg_name);

        if let Some(ref init_val) = self.init_val {
            let InitVal::Exp(exp) = init_val;
            let init_value = exp.generate(ctx)?;
            ctx.emit_store(init_value, var_alloc);
        }

        ctx.symbol_table
            .define(self.ident.clone(), Symbol::Var(var_alloc))?;

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
                let counter = ctx.fetch_counter();

                let lor_rhs = ctx.create_bb(Some(&format!("%lor_rhs_{}", counter)));
                let lor_end = ctx.create_bb(Some(&format!("%lor_end_{}", counter)));

                let result = ctx.emit_alloc(Type::get_i32());

                let zero = ctx.emit_integer(0);

                // l || r => (l != 0) | (r != 0)
                // gen lhs
                let lhs = l.generate(ctx)?;
                let lnez = ctx.emit_binary(BinaryOp::NotEq, lhs, zero);
                ctx.emit_store(lnez, result);
                ctx.emit_cond_br(lnez, lor_end, lor_rhs);

                // gen rhs
                ctx.set_cur_bb(lor_rhs);
                let rhs = r.generate(ctx)?;
                let rnez = ctx.emit_binary(BinaryOp::NotEq, rhs, zero);
                ctx.emit_store(rnez, result);
                ctx.emit_jump(lor_end);

                // gen end
                ctx.set_cur_bb(lor_end);
                let result = ctx.emit_load(result);

                Ok(result)
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
                let counter = ctx.fetch_counter();

                let land_rhs = ctx.create_bb(Some(&format!("%land_rhs_{}", counter)));
                let land_end = ctx.create_bb(Some(&format!("%land_end_{}", counter)));

                let result = ctx.emit_alloc(Type::get_i32());

                let zero = ctx.emit_integer(0);

                // l && r => (l != 0) & (r != 0)
                // gen lhs
                let lhs = l.generate(ctx)?;
                let lnez = ctx.emit_binary(BinaryOp::NotEq, lhs, zero);
                ctx.emit_store(lnez, result);
                ctx.emit_cond_br(lnez, land_rhs, land_end);

                // gen rhs
                ctx.set_cur_bb(land_rhs);
                let rhs = r.generate(ctx)?;
                let rnez = ctx.emit_binary(BinaryOp::NotEq, rhs, zero);
                ctx.emit_store(rnez, result);
                ctx.emit_jump(land_end);

                // gen end
                ctx.set_cur_bb(land_end);
                let result = ctx.emit_load(result);

                Ok(result)
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
                Ok(ctx.emit_binary((*eq_op).into(), l_val, r_val))
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
                Ok(ctx.emit_binary((*rel_op).into(), l_val, r_val))
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
                Ok(ctx.emit_binary((*add_op).into(), l_val, r_val))
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
                Ok(ctx.emit_binary((*mul_op).into(), l_val, r_val))
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
                let zero = ctx.emit_integer(0);

                let result = match op {
                    UnaryOp::Plus => exp_val,
                    UnaryOp::Minus => ctx.emit_binary(BinaryOp::Sub, zero, exp_val),
                    UnaryOp::Not => ctx.emit_binary(BinaryOp::Eq, exp_val, zero),
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
            PrimaryExp::Number(num) => Ok(ctx.emit_integer(*num)),
            PrimaryExp::Paren(exp) => exp.generate(ctx),
            PrimaryExp::LVal(lval) => {
                let LVal::Ident(ident) = lval;
                let sym = *ctx
                    .symbol_table
                    .resolve(ident)
                    .ok_or(Error::SymbolNotFound)?;

                match sym {
                    Symbol::Const(c) => Ok(ctx.emit_integer(c)),
                    Symbol::Var(var) => Ok(ctx.emit_load(var)),
                }
            }
        }
    }
}
