mod sexpr;

use std::ops::Deref;

use sexpr::stmt_to_string;

pub type Span = std::ops::Range<usize>;
pub type Spanned<T> = (Span, T);

#[derive(Debug, PartialEq, Clone)]
pub struct Program<'a>(Vec<Spanned<Stmt<'a>>>);

impl<'a> Program<'a> {
    pub fn new(stmts: Vec<Spanned<Stmt<'a>>>) -> Self {
        Self(stmts)
    }

    /// Creates an S-expression representation of the program.
    pub fn to_sexpr(&self) -> String {
        let mut s = String::new();
        for (_, d) in self.0.iter() {
            stmt_to_string(d, &mut s);
        }
        s
    }
}

impl<'a> Deref for Program<'a> {
    type Target = Vec<Spanned<Stmt<'a>>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt<'a> {
    Block(Vec<Spanned<Stmt<'a>>>),
    Class(Spanned<ClassStmt<'a>>),
    Fun(Spanned<FunStmt<'a>>),
    Var(Spanned<VarStmt<'a>>),
    Expr(Spanned<Expr<'a>>),
    If(Spanned<If<'a>>),
    Print(Spanned<Expr<'a>>),
    Return(Option<Spanned<Expr<'a>>>),
    While(Spanned<While<'a>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ClassStmt<'a> {
    pub name: Spanned<&'a str>,
    pub superclass: Option<Spanned<&'a str>>,
    pub functions: Vec<Spanned<FunStmt<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunStmt<'a> {
    pub name: Spanned<&'a str>,
    pub params: Vec<Spanned<&'a str>>,
    pub body: Spanned<Vec<Spanned<Stmt<'a>>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarStmt<'a> {
    pub lhs: Spanned<&'a str>,
    pub rhs: Option<Spanned<Expr<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct For<'a> {
    pub cond: Option<Spanned<Expr<'a>>>,
    pub incr: Option<Spanned<Expr<'a>>>,
    pub stmt: Box<Spanned<Stmt<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct If<'a> {
    pub cond: Spanned<Expr<'a>>,
    pub body: Box<Spanned<Stmt<'a>>>,
    pub else_body: Option<Box<Spanned<Stmt<'a>>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct While<'a> {
    pub cond: Spanned<Expr<'a>>,
    pub body: Box<Spanned<Stmt<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    Assign(Spanned<Assign<'a>>),
    Binary(Spanned<Binary<'a>>),
    Call(Spanned<Call<'a>>),
    Unary(Spanned<Unary<'a>>),
    Grouping(Box<Spanned<Expr<'a>>>),
    Primary(Spanned<Primary<'a>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assign<'a> {
    pub lhs: Box<Spanned<&'a str>>,
    pub rhs: Box<Spanned<Expr<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binary<'a> {
    pub op: Spanned<BinaryOp>,
    pub lhs: Box<Spanned<Expr<'a>>>,
    pub rhs: Box<Spanned<Expr<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Call<'a> {
    pub callee: Box<Spanned<Expr<'a>>>,
    pub args: Vec<Spanned<Expr<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Unary<'a> {
    pub op: Spanned<UnaryOp>,
    pub rhs: Box<Spanned<Expr<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Primary<'a> {
    Nil,
    Bool(bool),
    Number(f64),
    String(&'a str),
    Identifier(&'a str),
    Super(&'a str),
}
