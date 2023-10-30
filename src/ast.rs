#[derive(Debug, Clone)]
pub enum Type {
    I32
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(i32)
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Box<Expr>),
    Return(Box<Expr>)
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<(String, Type)>,
    pub block: Block
}

#[derive(Debug, Clone)]
pub enum TopLevel {
    Function(Function)
}

#[derive(Debug, Clone)]
pub struct File {
    pub top_levels: Vec<TopLevel>
}
