#[macro_export]
macro_rules! sym {
    ($name:ident) => {
        Expr::Sym(stringify!($name).to_string())
    };
}

#[macro_export]
macro_rules! var {
    ($name:ident) => {
        Expr::Var(stringify!($name).to_string())
    };
}

#[macro_export]
macro_rules! fun {
    ($head:ident) => {Expr::Fun(Box::new(Expr::Sym(stringify!($head).to_string())), vec![])};
    ($head:ident,$($args:expr),*) => {
        Expr::Fun(Box::new(Expr::Sym(stringify!($head).to_string())), vec![$($args),*])
    };
    ($head:expr) => {Expr::Fun(Box::new($head), vec![])};
    ($head:expr,$($args:expr),*) => {
        Expr::Fun(Box::new($head), vec![$($args),*])
    };
}

#[macro_export]
macro_rules! xform {
    ($from:expr,$to:expr) => {
        XForm::new($from, $to)
    };
}
