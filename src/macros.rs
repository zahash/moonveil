#[macro_export]
macro_rules! var {
    ($name:ident) => {
        Expr::Var(stringify!($name).to_string())
    };
}

#[macro_export]
macro_rules! fun {
    ($name:ident) => {Expr::Fun(stringify!($name).to_string(), vec![])};
    ($name:ident,$($args:expr),*) => {
        Expr::Fun(stringify!($name).to_string(), vec![$($args),*])
    };
}

#[macro_export]
macro_rules! xform {
    ($from:expr,$to:expr) => {
        XForm::new($from, $to)
    };
}
