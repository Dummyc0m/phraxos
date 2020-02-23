#[derive(Debug, Eq, PartialEq)]
pub enum BinOp {
    Plus,
    Minus,
    Times,
    Div,
    Mod,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Tycon {
    // Type
    Arrow(Box<Tycon>, Box<Tycon>),
    Int
}

#[derive(Debug, Eq, PartialEq)]
pub enum AbtNode {
    Var(String),
    Int(i32),

    Lam(String, Tycon, Box<AbtNode>),
    Ap(Box<AbtNode>, Box<AbtNode>),

    Let(String, Box<AbtNode>, Box<AbtNode>),
    BinOp(Box<AbtNode>, BinOp, Box<AbtNode>),
}
