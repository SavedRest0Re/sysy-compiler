use crate::{
    ast::{
        AddExp, ConstExp, EqExp, Exp, LAndExp, LOrExp, LVal, MulExp, PrimaryExp, RelExp, UnaryExp,
        UnaryOp,
    },
    ir::{
        Error, IRResult,
        ctx::{Ctx, Symbol},
    },
};

pub fn consteval(cexp: &ConstExp, ctx: &Ctx) -> IRResult<i32> {
    consteval_exp(&cexp.exp, ctx)
}

fn consteval_exp(exp: &Exp, ctx: &Ctx) -> IRResult<i32> {
    match exp {
        Exp::LOr(lor) => consteval_lor(lor, ctx),
    }
}

fn consteval_lor(exp: &LOrExp, ctx: &Ctx) -> IRResult<i32> {
    match exp {
        LOrExp::LAnd(land) => consteval_land(land, ctx),
        LOrExp::LOrLAnd(l, r) => {
            let l_result = consteval_lor(l, ctx)?;
            if l_result != 0 {
                return Ok(1);
            }
            if consteval_land(r, ctx)? != 0 {
                return Ok(1);
            }
            Ok(0)
        }
    }
}

fn consteval_land(exp: &LAndExp, ctx: &Ctx) -> IRResult<i32> {
    match exp {
        LAndExp::Eq(eq) => consteval_eq(eq, ctx),
        LAndExp::LAndEq(l, r) => {
            let l_result = consteval_land(l, ctx)?;
            if l_result == 0 {
                return Ok(0);
            }
            if consteval_eq(r, ctx)? != 0 {
                return Ok(1);
            }
            Ok(0)
        }
    }
}

fn consteval_eq(exp: &EqExp, ctx: &Ctx) -> IRResult<i32> {
    match exp {
        EqExp::Rel(rel) => consteval_rel(rel, ctx),
        EqExp::EqRel(l, op, r) => {
            let l_result = consteval_eq(l, ctx)?;
            let r_result = consteval_rel(r, ctx)?;
            let cmp = match op {
                crate::ast::EqOp::Eq => l_result == r_result,
                crate::ast::EqOp::Ne => l_result != r_result,
            };
            Ok(cmp as i32)
        }
    }
}

fn consteval_rel(exp: &RelExp, ctx: &Ctx) -> IRResult<i32> {
    match exp {
        RelExp::Add(add) => consteval_add(add, ctx),
        RelExp::RelAdd(l, op, r) => {
            let l_result = consteval_rel(l, ctx)?;
            let r_result = consteval_add(r, ctx)?;
            let cmp = match op {
                crate::ast::RelOp::Lt => l_result < r_result,
                crate::ast::RelOp::Gt => l_result > r_result,
                crate::ast::RelOp::Le => l_result <= r_result,
                crate::ast::RelOp::Ge => l_result >= r_result,
            };
            Ok(cmp as i32)
        }
    }
}

fn consteval_add(exp: &AddExp, ctx: &Ctx) -> IRResult<i32> {
    match exp {
        AddExp::Mul(mul) => consteval_mul(mul, ctx),
        AddExp::AddMul(l, op, r) => {
            let l_result = consteval_add(l, ctx)?;
            let r_result = consteval_mul(r, ctx)?;
            Ok(match op {
                crate::ast::AddOp::Add => l_result + r_result,
                crate::ast::AddOp::Sub => l_result - r_result,
            })
        }
    }
}

fn consteval_mul(exp: &MulExp, ctx: &Ctx) -> IRResult<i32> {
    match exp {
        MulExp::Unary(unary_exp) => consteval_unary(unary_exp, ctx),
        MulExp::MulUnary(l, mul_op, r) => {
            let l_result = consteval_mul(l, ctx)?;
            let r_result = consteval_unary(r, ctx)?;
            Ok(match mul_op {
                crate::ast::MulOp::Mul => l_result * r_result,
                crate::ast::MulOp::Div => l_result / r_result,
                crate::ast::MulOp::Mod => l_result % r_result,
            })
        }
    }
}

fn consteval_unary(exp: &UnaryExp, ctx: &Ctx) -> IRResult<i32> {
    match exp {
        UnaryExp::PrimaryExp(primary) => consteval_primary(primary, ctx),
        UnaryExp::Unary(op, unary) => {
            let result = consteval_unary(unary, ctx)?;
            Ok(match op {
                UnaryOp::Plus => result,
                UnaryOp::Minus => -result,
                UnaryOp::Not => {
                    if result == 0 {
                        1
                    } else {
                        0
                    }
                }
            })
        }
    }
}

fn consteval_primary(exp: &PrimaryExp, ctx: &Ctx) -> IRResult<i32> {
    match exp {
        PrimaryExp::Paren(exp) => consteval_exp(exp, ctx),
        PrimaryExp::LVal(lval) => consteval_lval(lval, ctx),
        PrimaryExp::Number(i) => Ok(*i),
    }
}

fn consteval_lval(exp: &LVal, ctx: &Ctx) -> IRResult<i32> {
    match exp {
        LVal::Ident(ident) => match ctx.symbol_table.resolve(ident) {
            Some(sym) => match sym {
                Symbol::Const(c) => Ok(*c),
                Symbol::Var(_) => Err(Error::FailedToEval), // variable is not a compile-time constant
            },
            None => Err(Error::SymbolNotFound),
        },
    }
}
