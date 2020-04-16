#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Ident(pub String);

#[derive(Debug, Eq, PartialEq)]
pub enum Tycon {
    // Type
    Arrow(Box<Tycon>, Box<Tycon>),
    Ident(Ident),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Decl {
    Vbind(Ident, Expr),
    Fbind(Ident, Vec<Ident>, Expr),
    Infix(u8, u8, Ident),
    Type(Ident, Tycon)
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Extern,
    Var(Ident),
    Dot(Box<Expr>, Ident),
    Number(i32),

    Lam(Ident, Tycon, Box<Expr>),
    Ap(Box<Expr>, Box<Expr>),

    Let(Vec<Decl>, Box<Expr>),
    BinOp(Box<Expr>, Ident, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Prog(pub Vec<Decl>);
