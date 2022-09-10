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
                    Sym(_) => expr.clone(),
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
    use crate::{expr::Expr, fun, sym, xform};
    // use pretty_assertions::assert_eq;

    #[test]
    fn identity_transformation() {
        assert_transformation(xform!(fun!(F), fun!(F)), fun!(F), fun!(F));
        assert_transformation(
            xform!(fun!(F, sym!(x)), fun!(F, sym!(x))),
            fun!(F, sym!(y)),
            fun!(F, sym!(y)),
        );
        assert_transformation(
            xform!(
                fun!(F, fun!(G, sym!(x0)), sym!(x1)),
                fun!(F, fun!(G, sym!(x0)), sym!(x1))
            ),
            fun!(F, fun!(G, sym!(y0)), sym!(y1)),
            fun!(F, fun!(G, sym!(y0)), sym!(y1)),
        );
    }

    #[test]
    fn mismatching_identity_transformation() {
        assert_transformation(xform!(fun!(F), fun!(F)), fun!(G), fun!(G));
        assert_transformation(
            xform!(fun!(F), fun!(F)),
            fun!(G, fun!(H, sym!(x), sym!(y)), sym!(z)),
            fun!(G, fun!(H, sym!(x), sym!(y)), sym!(z)),
        );
    }

    #[test]
    fn mismatching() {
        assert_transformation(xform!(fun!(F), fun!(F)), fun!(G), fun!(G));
        assert_transformation(xform!(fun!(F), fun!(G)), fun!(H), fun!(H));
        assert_transformation(xform!(fun!(F), fun!(G)), fun!(F, sym!(x)), fun!(F, sym!(x)));
        assert_transformation(
            xform!(
                fun!(F, fun!(G, sym!(x), sym!(y)), sym!(z)),
                fun!(H, sym!(z), sym!(y), fun!(G, sym!(x)))
            ),
            fun!(K, sym!(a)),
            fun!(K, sym!(a)),
        );
    }

    #[test]
    fn matching() {
        assert_transformation(xform!(fun!(F), fun!(G)), fun!(F), fun!(G));
        assert_transformation(
            xform!(fun!(F, sym!(x)), fun!(G, sym!(x))),
            fun!(F, sym!(y)),
            fun!(G, sym!(y)),
        );
        assert_transformation(
            xform!(fun!(F, sym!(x0), sym!(x1)), fun!(G, sym!(x0), sym!(x1))),
            fun!(F, sym!(y0), sym!(y1)),
            fun!(G, sym!(y0), sym!(y1)),
        );
        assert_transformation(
            xform!(
                fun!(F, fun!(G, sym!(x0)), sym!(x1)),
                fun!(G, fun!(F, sym!(x1)), sym!(x0))
            ),
            fun!(F, fun!(G, sym!(y0)), sym!(y1)),
            fun!(G, fun!(F, sym!(y1)), sym!(y0)),
        );
    }

    #[test]
    fn inner_matching() {
        assert_transformation(
            xform!(fun!(F, sym!(x)), fun!(G, sym!(x))),
            fun!(H, fun!(F, sym!(y))),
            fun!(H, fun!(G, sym!(y))),
        );
        assert_transformation(
            xform!(fun!(F, sym!(x0), sym!(x1)), fun!(G, sym!(x0), sym!(x1))),
            fun!(H, fun!(F, sym!(y0), sym!(y1))),
            fun!(H, fun!(G, sym!(y0), sym!(y1))),
        );
        assert_transformation(
            xform!(fun!(F, sym!(x)), fun!(G, sym!(x))),
            fun!(H, fun!(F, sym!(y)), fun!(F, sym!(y))),
            fun!(H, fun!(G, sym!(y)), fun!(G, sym!(y))),
        );
        assert_transformation(
            xform!(fun!(F, sym!(x)), fun!(G, sym!(x))),
            fun!(
                H,
                sym!(b),
                fun!(
                    H,
                    fun!(F, sym!(y)),
                    sym!(a),
                    fun!(H, fun!(F, sym!(y)), fun!(F, sym!(y))),
                    fun!(F, sym!(y))
                )
            ),
            fun!(
                H,
                sym!(b),
                fun!(
                    H,
                    fun!(G, sym!(y)),
                    sym!(a),
                    fun!(H, fun!(G, sym!(y)), fun!(G, sym!(y))),
                    fun!(G, sym!(y))
                )
            ),
        );
    }

    #[test]
    fn integration_tests() {
        assert_transformation(
            xform!(
                fun!(SWAP, fun!(PAIR, sym!(a), sym!(b))),
                fun!(PAIR, sym!(b), sym!(a))
            ),
            fun!(
                W,
                fun!(SWAP, fun!(PAIR, fun!(F, sym!(a)), fun!(G, sym!(b)))),
                fun!(SWAP, fun!(PAIR, fun!(Q, sym!(c)), fun!(Z, sym!(d))))
            ),
            fun!(
                W,
                fun!(PAIR, fun!(G, sym!(b)), fun!(F, sym!(a))),
                fun!(PAIR, fun!(Z, sym!(d)), fun!(Q, sym!(c)))
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
