use std::fmt::Display;

use crate::expr::Expr;

#[derive(Debug, PartialEq, Clone)]
pub struct XForm(Expr, Expr);

impl XForm {
    pub fn new(from: Expr, to: Expr) -> Self {
        Self(from, to)
    }

    pub fn apply(&self, expr: &Expr) -> Expr {
        match self.0.match_with(expr) {
            Some(matches) => self.1.substitute(&matches),
            None => {
                use Expr::*;
                match expr {
                    Sym(_) => todo!(),
                    Var(_) => expr.clone(),
                    Fun(name, args) => Fun(
                        name.clone(),
                        args.iter()
                            .map(|arg| self.apply(arg))
                            .collect::<Vec<Expr>>(),
                    ),
                }
            }
        }
    }
}

impl Display for XForm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.0, self.1)
    }
}

#[cfg(test)]
mod transform_tests {
    use super::XForm;
    use crate::{expr::Expr, fun, var, xform};
    // use pretty_assertions::assert_eq;

    #[test]
    fn identity_transformation() {
        assert_transformation(xform!(fun!(F), fun!(F)), fun!(F), fun!(F));
        assert_transformation(
            xform!(fun!(F, var!(x)), fun!(F, var!(x))),
            fun!(F, var!(y)),
            fun!(F, var!(y)),
        );
        assert_transformation(
            xform!(
                fun!(F, fun!(G, var!(x0)), var!(x1)),
                fun!(F, fun!(G, var!(x0)), var!(x1))
            ),
            fun!(F, fun!(G, var!(y0)), var!(y1)),
            fun!(F, fun!(G, var!(y0)), var!(y1)),
        );
    }

    #[test]
    fn mismatching_identity_transformation() {
        assert_transformation(xform!(fun!(F), fun!(F)), fun!(G), fun!(G));
        assert_transformation(
            xform!(fun!(F), fun!(F)),
            fun!(G, fun!(H, var!(x), var!(y)), var!(z)),
            fun!(G, fun!(H, var!(x), var!(y)), var!(z)),
        );
    }

    #[test]
    fn mismatching() {
        assert_transformation(xform!(fun!(F), fun!(F)), fun!(G), fun!(G));
        assert_transformation(xform!(fun!(F), fun!(G)), fun!(H), fun!(H));
        assert_transformation(xform!(fun!(F), fun!(G)), fun!(F, var!(x)), fun!(F, var!(x)));
        assert_transformation(
            xform!(
                fun!(F, fun!(G, var!(x), var!(y)), var!(z)),
                fun!(H, var!(z), var!(y), fun!(G, var!(x)))
            ),
            fun!(K, var!(a)),
            fun!(K, var!(a)),
        );
    }

    #[test]
    fn matching() {
        assert_transformation(xform!(fun!(F), fun!(G)), fun!(F), fun!(G));
        assert_transformation(
            xform!(fun!(F, var!(x)), fun!(G, var!(x))),
            fun!(F, var!(y)),
            fun!(G, var!(y)),
        );
        assert_transformation(
            xform!(fun!(F, var!(x0), var!(x1)), fun!(G, var!(x0), var!(x1))),
            fun!(F, var!(y0), var!(y1)),
            fun!(G, var!(y0), var!(y1)),
        );
        assert_transformation(
            xform!(
                fun!(F, fun!(G, var!(x0)), var!(x1)),
                fun!(G, fun!(F, var!(x1)), var!(x0))
            ),
            fun!(F, fun!(G, var!(y0)), var!(y1)),
            fun!(G, fun!(F, var!(y1)), var!(y0)),
        );
    }

    #[test]
    fn inner_matching() {
        assert_transformation(
            xform!(fun!(F, var!(x)), fun!(G, var!(x))),
            fun!(H, fun!(F, var!(y))),
            fun!(H, fun!(G, var!(y))),
        );
        assert_transformation(
            xform!(fun!(F, var!(x0), var!(x1)), fun!(G, var!(x0), var!(x1))),
            fun!(H, fun!(F, var!(y0), var!(y1))),
            fun!(H, fun!(G, var!(y0), var!(y1))),
        );
        assert_transformation(
            xform!(fun!(F, var!(x)), fun!(G, var!(x))),
            fun!(H, fun!(F, var!(y)), fun!(F, var!(y))),
            fun!(H, fun!(G, var!(y)), fun!(G, var!(y))),
        );
        assert_transformation(
            xform!(fun!(F, var!(x)), fun!(G, var!(x))),
            fun!(
                H,
                var!(b),
                fun!(
                    H,
                    fun!(F, var!(y)),
                    var!(a),
                    fun!(H, fun!(F, var!(y)), fun!(F, var!(y))),
                    fun!(F, var!(y))
                )
            ),
            fun!(
                H,
                var!(b),
                fun!(
                    H,
                    fun!(G, var!(y)),
                    var!(a),
                    fun!(H, fun!(G, var!(y)), fun!(G, var!(y))),
                    fun!(G, var!(y))
                )
            ),
        );
    }

    #[test]
    fn integration_tests() {
        assert_transformation(
            xform!(
                fun!(SWAP, fun!(PAIR, var!(a), var!(b))),
                fun!(PAIR, var!(b), var!(a))
            ),
            fun!(
                W,
                fun!(SWAP, fun!(PAIR, fun!(F, var!(a)), fun!(G, var!(b)))),
                fun!(SWAP, fun!(PAIR, fun!(Q, var!(c)), fun!(Z, var!(d))))
            ),
            fun!(
                W,
                fun!(PAIR, fun!(G, var!(b)), fun!(F, var!(a))),
                fun!(PAIR, fun!(Z, var!(d)), fun!(Q, var!(c)))
            ),
        );
    }

    fn assert_transformation(
        transformation: XForm,
        expr_to_be_transformed: Expr,
        expected_expr_after_transformation: Expr,
    ) {
        let transformed = transformation.apply(&expr_to_be_transformed);
        assert_eq!(expected_expr_after_transformation, transformed);
    }
}
