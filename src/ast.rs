#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

#[derive(Debug)]
pub struct FuncDef {
    pub ret_ty: FuncType,
    pub ident: String,
    pub block: Block,
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

#[derive(Debug)]
pub struct Block {
    pub stmt: Stmt,
}

#[derive(Debug)]
pub struct Stmt {
    pub exp: Exp,
}

#[derive(Debug)]
pub enum Exp {
    LOr(LOrExp),
}

#[derive(Debug)]
pub enum LOrExp {
    LAnd(LAndExp),
    LOrLAnd(Box<LOrExp>, LAndExp),
}

#[derive(Debug)]
pub enum LAndExp {
    Eq(EqExp),
    LAndEq(Box<LAndExp>, EqExp),
}

#[derive(Debug)]
pub enum EqExp {
    Rel(RelExp),
    EqRel(Box<EqExp>, EqOp, RelExp),
}

#[derive(Debug)]
pub enum RelExp {
    Add(AddExp),
    RelAdd(Box<RelExp>, RelOp, AddExp),
}

#[derive(Debug)]
pub enum AddExp {
    Mul(MulExp),
    AddMul(Box<AddExp>, AddOp, MulExp),
}

#[derive(Debug)]
pub enum MulExp {
    Unary(UnaryExp),
    MulUnary(Box<MulExp>, MulOp, UnaryExp),
}

#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    Unary(UnaryOp, Box<UnaryExp>),
}

#[derive(Debug)]
pub enum PrimaryExp {
    Number(i32),
    Paren(Box<Exp>),
}

#[derive(Debug, Clone, Copy)]
pub enum EqOp {
    Eq,
    Ne,
}

#[derive(Debug, Clone, Copy)]
pub enum RelOp {
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, Clone, Copy)]
pub enum AddOp {
    Add,
    Sub,
}

#[derive(Debug, Clone, Copy)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}
