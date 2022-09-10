#[macro_export]
macro_rules! sym {
    ($name:ident) => {
        Expr::Sym(stringify!($name).to_string())
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