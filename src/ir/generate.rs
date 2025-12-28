use koopa::ir::{
    BasicBlock, BinaryOp, FunctionData, Program, Type, Value,
    builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
};

use crate::{
    add_bb, add_inst,
    ast::{
        AddExp, AddOp, Block, CompUnit, EqExp, EqOp, Exp, FuncDef, FuncType, LAndExp, LOrExp,
        MulExp, MulOp, PrimaryExp, RelExp, RelOp, Stmt, UnaryExp, UnaryOp,
    },
    ir::{IRResult, ctx::Ctx},
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

        let ret_value = self.block.stmt.exp.generate(ctx)?;

        let ret_inst = new_value!(ctx.func_data_mut()).ret(Some(ret_value));
        add_inst!(ctx.func_data_mut(), entry_bb, ret_inst);

        ctx.unset_cur_func();

        Ok(())
    }
}

impl IRGen for FuncType {
    type Output = Type;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        Ok(match self {
            FuncType::Int => Type::get_i32(),
        })
    }
}

impl IRGen for Block {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        todo!()
    }
}

impl IRGen for Stmt {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        todo!()
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
                Ok(gen_lor(lval, rval, ctx))
            }
        }
    }
}

// l || r
// (l != 0) || (r != 0)
fn gen_lor(l: Value, r: Value, ctx: &mut Ctx) -> Value {
    let entry_bb = ctx.func_data().layout().entry_bb().unwrap();

    let zero = new_value!(ctx.func_data_mut()).integer(0);

    let lnez = new_value!(ctx.func_data_mut()).binary(BinaryOp::NotEq, l, zero);
    add_inst!(ctx.func_data_mut(), entry_bb, lnez);

    let rnez = new_value!(ctx.func_data_mut()).binary(BinaryOp::NotEq, r, zero);
    add_inst!(ctx.func_data_mut(), entry_bb, rnez);

    let or_inst = new_value!(ctx.func_data_mut()).binary(BinaryOp::Or, lnez, rnez);
    add_inst!(ctx.func_data_mut(), entry_bb, or_inst);

    or_inst
}

impl IRGen for LAndExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            LAndExp::Eq(eq_exp) => eq_exp.generate(ctx),
            LAndExp::LAndEq(l, r) => {
                let l_val = l.generate(ctx)?;
                let r_val = r.generate(ctx)?;
                Ok(gen_land(l_val, r_val, ctx))
            }
        }
    }
}

fn gen_land(l: Value, r: Value, ctx: &mut Ctx) -> Value {
    let entry_bb = ctx.func_data().layout().entry_bb().unwrap();

    let zero = new_value!(ctx.func_data_mut()).integer(0);

    let lnez = new_value!(ctx.func_data_mut()).binary(BinaryOp::NotEq, l, zero);
    add_inst!(ctx.func_data_mut(), entry_bb, lnez);

    let rnez = new_value!(ctx.func_data_mut()).binary(BinaryOp::NotEq, r, zero);
    add_inst!(ctx.func_data_mut(), entry_bb, rnez);

    let and_inst = new_value!(ctx.func_data_mut()).binary(BinaryOp::And, lnez, rnez);
    add_inst!(ctx.func_data_mut(), entry_bb, and_inst);

    and_inst
}

impl IRGen for EqExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            EqExp::Rel(rel_exp) => rel_exp.generate(ctx),
            EqExp::EqRel(l, eq_op, r) => {
                let l_val = l.generate(ctx)?;
                let r_val = r.generate(ctx)?;
                Ok(gen_eq(l_val, r_val, *eq_op, ctx))
            }
        }
    }
}

fn gen_eq(l: Value, r: Value, op: EqOp, ctx: &mut Ctx) -> Value {
    let op = match op {
        EqOp::Eq => BinaryOp::Eq,
        EqOp::Ne => BinaryOp::NotEq,
    };

    let entry_bb = ctx.func_data().layout().entry_bb().unwrap();

    let eq_inst = new_value!(ctx.func_data_mut()).binary(op, l, r);
    add_inst!(ctx.func_data_mut(), entry_bb, eq_inst);

    eq_inst
}

impl IRGen for RelExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            RelExp::Add(add_exp) => add_exp.generate(ctx),
            RelExp::RelAdd(l, op, r) => {
                let l_val = l.generate(ctx)?;
                let r_val = r.generate(ctx)?;
                Ok(gen_rel(l_val, r_val, *op, ctx))
            }
        }
    }
}

fn gen_rel(l: Value, r: Value, op: RelOp, ctx: &mut Ctx) -> Value {
    let op = match op {
        RelOp::Lt => BinaryOp::Lt,
        RelOp::Gt => BinaryOp::Gt,
        RelOp::Le => BinaryOp::Le,
        RelOp::Ge => BinaryOp::Ge,
    };

    let entry_bb = ctx.func_data().layout().entry_bb().unwrap();

    let rel_inst = new_value!(ctx.func_data_mut()).binary(op, l, r);
    add_inst!(ctx.func_data_mut(), entry_bb, rel_inst);

    rel_inst
}

impl IRGen for AddExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            AddExp::Mul(mul_exp) => mul_exp.generate(ctx),
            AddExp::AddMul(l, op, r) => {
                let l_val = l.generate(ctx)?;
                let r_val = r.generate(ctx)?;
                Ok(gen_add(l_val, r_val, *op, ctx))
            }
        }
    }
}

fn gen_add(l: Value, r: Value, op: AddOp, ctx: &mut Ctx) -> Value {
    let op = match op {
        AddOp::Add => BinaryOp::Add,
        AddOp::Sub => BinaryOp::Sub,
    };

    let entry_bb = ctx.func_data().layout().entry_bb().unwrap();

    let add_inst = new_value!(ctx.func_data_mut()).binary(op, l, r);
    add_inst!(ctx.func_data_mut(), entry_bb, add_inst);

    add_inst
}

impl IRGen for MulExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            MulExp::Unary(unary_exp) => unary_exp.generate(ctx),
            MulExp::MulUnary(l, op, r) => {
                let l_val = l.generate(ctx)?;
                let r_val = r.generate(ctx)?;
                Ok(gen_mul(l_val, r_val, *op, ctx))
            }
        }
    }
}

fn gen_mul(l: Value, r: Value, op: MulOp, ctx: &mut Ctx) -> Value {
    let op = match op {
        MulOp::Mul => BinaryOp::Mul,
        MulOp::Div => BinaryOp::Div,
        MulOp::Mod => BinaryOp::Mod,
    };

    let entry_bb = ctx.func_data().layout().entry_bb().unwrap();

    let mul_exp = new_value!(ctx.func_data_mut()).binary(op, l, r);
    add_inst!(ctx.func_data_mut(), entry_bb, mul_exp);

    mul_exp
}

impl IRGen for UnaryExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.generate(ctx),
            UnaryExp::Unary(op, exp) => {
                let exp_val = exp.generate(ctx)?;
                Ok(gen_unary(*op, exp_val, ctx))
            }
        }
    }
}

fn gen_unary(op: UnaryOp, operand: Value, ctx: &mut Ctx) -> Value {
    let entry_bb = ctx.func_data().layout().entry_bb().unwrap();

    let zero = new_value!(ctx.func_data_mut()).integer(0);

    match op {
        UnaryOp::Plus => {
            // Do nothing
            operand
        }
        UnaryOp::Minus => {
            let sub_inst = new_value!(ctx.func_data_mut()).binary(BinaryOp::Sub, zero, operand);
            add_inst!(ctx.func_data_mut(), entry_bb, sub_inst);
            sub_inst
        }
        UnaryOp::Not => {
            // operand == 0
            let eq_inst = new_value!(ctx.func_data_mut()).binary(BinaryOp::Eq, operand, zero);
            add_inst!(ctx.func_data_mut(), entry_bb, eq_inst);
            eq_inst
        }
    }
}

impl IRGen for PrimaryExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        let entry_bb = ctx.func_data().layout().entry_bb().unwrap();

        match self {
            PrimaryExp::Number(num) => Ok(new_value!(ctx.func_data_mut()).integer(*num)),
            PrimaryExp::Paren(exp) => exp.generate(ctx),
        }
    }
}
