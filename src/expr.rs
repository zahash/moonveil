use std::{collections::HashMap, fmt::Display};

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Sym(String),
    Fun(String, Vec<Expr>),
}

type Matches = HashMap<String, Expr>;

impl Expr {
    pub fn match_with(&self, expr: &Expr) -> Option<Matches> {
        fn _match_with(pattern: &Expr, expr: &Expr, matches: &mut Matches) -> bool {
            use Expr::*;
            match (pattern, expr) {
                (Sym(name), expr) => {
                    if let Some(already_bound_expr) = matches.get(name) {
                        if already_bound_expr != expr {
                            return false;
                        }
                    }
                    matches.insert(name.clone(), expr.clone());
                    true
                }
                (Fun(_, _), Sym(_)) => false,
                (Fun(pttrn_name, _), Fun(expr_name, _)) if pttrn_name != expr_name => false,
                (Fun(_, pttrn_args), Fun(_, expr_args)) if pttrn_args.len() != expr_args.len() => {
                    false
                }
                (Fun(_, pttrn_args), Fun(_, expr_args)) => pttrn_args
                    .iter()
                    .zip(expr_args)
                    .all(|(pttrn_arg, expr_arg)| _match_with(pttrn_arg, expr_arg, matches)),
            }
        }

        let mut matches = HashMap::new();

        if _match_with(self, expr, &mut matches) {
            Option::Some(matches)
        } else {
            Option::None
        }
    }

    pub fn substitute(&self, matches: &Matches) -> Expr {
        use Expr::*;
        match self {
            Sym(name) => matches.get(name).unwrap_or(self).clone(),
            Fun(name, args) => Fun(
                name.clone(),
                args.iter()
                    .map(|arg| arg.substitute(matches))
                    .collect::<Vec<Expr>>(),
            ),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn p_expr(expr: &Expr) -> String {
            match expr {
                Expr::Sym(name) => name.to_string(),
                Expr::Fun(name, args) => format!(
                    "{}({})",
                    name,
                    args.iter().map(p_expr).collect::<Vec<String>>().join(", ")
                ),
            }
        }

        write!(f, "{}", p_expr(self))
    }
}

#[cfg(test)]
mod pattern_match_tests {
    use super::{Expr, Matches};
    use crate::{fun, sym};
    use pretty_assertions::assert_eq;

    #[test]
    fn anything_matches_with_a_symbol() {
        assert_matches(sym!(a), sym!(a), vec![("a", sym!(a))]);
        assert_matches(sym!(a), sym!(b), vec![("a", sym!(b))]);
        assert_matches(sym!(a), fun!(F), vec![("a", fun!(F))]);
        assert_matches(sym!(a), fun!(F, sym!(x)), vec![("a", fun!(F, sym!(x)))]);
        assert_matches(
            sym!(a),
            fun!(F, sym!(x), fun!(G, sym!(y)), sym!(z)),
            vec![("a", fun!(F, sym!(x), fun!(G, sym!(y)), sym!(z)))],
        );
    }

    #[test]
    fn function_pattern_only_matches_with_other_functions_with_same_name_and_number_of_args() {
        assert_no_matches(fun!(F), sym!(a));
        assert_no_matches(fun!(F), fun!(G));
        assert_no_matches(fun!(F, sym!(x0)), fun!(F, sym!(y0), sym!(y1)));

        assert_matches(fun!(F), fun!(F), vec![]);
        assert_matches(fun!(F, sym!(x0)), fun!(F, sym!(y0)), vec![("x0", sym!(y0))]);
    }

    #[test]
    fn test_with_same_repeated_symbols() {
        assert_matches(
            fun!(F, sym!(x0), sym!(x0)),
            fun!(F, sym!(y0), sym!(y0)),
            vec![("x0", sym!(y0))],
        );
        assert_matches(
            fun!(F, fun!(G, sym!(x0), sym!(x1)), sym!(x0)),
            fun!(F, fun!(G, sym!(y0), sym!(y1)), sym!(y0)),
            vec![("x0", sym!(y0)), ("x1", sym!(y1))],
        );

        assert_no_matches(fun!(F, sym!(x0), sym!(x0)), fun!(F, sym!(y0), sym!(y1)));
        assert_no_matches(
            fun!(F, fun!(G, sym!(x0), sym!(x0)), sym!(x1)),
            fun!(F, fun!(G, sym!(y0), sym!(y1)), sym!(y1)),
        );
    }

    #[test]
    fn test_recursive_pattern_matching() {
        assert_matches(
            fun!(F, sym!(x0), sym!(x1)),
            fun!(F, sym!(y0), sym!(y1)),
            vec![("x0", sym!(y0)), ("x1", sym!(y1))],
        );
        assert_matches(
            fun!(F, sym!(x0), fun!(G, sym!(x1)), sym!(x2)),
            fun!(F, sym!(y0), fun!(G, sym!(y1)), sym!(y2)),
            vec![("x0", sym!(y0)), ("x1", sym!(y1)), ("x2", sym!(y2))],
        );
        assert_matches(
            fun!(F, sym!(x0)),
            fun!(F, fun!(G, sym!(y0), sym!(y1))),
            vec![("x0", fun!(G, sym!(y0), sym!(y1)))],
        );

        assert_no_matches(
            fun!(F, fun!(G, sym!(x0))),
            fun!(F, fun!(G, sym!(y0), sym!(y1))),
        );
    }

    fn assert_matches(pattern: Expr, with: Expr, expected_matches: Vec<(&str, Expr)>) {
        let expected_matches = expected_matches
            .into_iter()
            .map(|(name, ex)| (name.to_string(), ex))
            .collect::<Matches>();

        let actual_matches = pattern.match_with(&with).unwrap();
        assert_eq!(expected_matches, actual_matches)
    }

    fn assert_no_matches(pattern: Expr, with: Expr) {
        assert_eq!(Option::None, pattern.match_with(&with));
    }
}

#[cfg(test)]
mod substitute_tests {
    use super::{Expr, Matches};
    use crate::{fun, sym};
    use std::collections::HashMap;

    #[test]
    fn empty_matches() {
        assert_substitute(fun!(F), HashMap::new(), fun!(F));
        assert_substitute(sym!(a), HashMap::new(), sym!(a));
        assert_substitute(
            fun!(F, fun!(G, sym!(x)), sym!(y)),
            HashMap::new(),
            fun!(F, fun!(G, sym!(x)), sym!(y)),
        );
    }

    #[test]
    fn partial_matches() {
        assert_substitute(
            fun!(F, sym!(x), sym!(y)),
            HashMap::from([("x".into(), sym!(w))]),
            fun!(F, sym!(w), sym!(y)),
        );
        assert_substitute(
            fun!(F, fun!(G, sym!(x)), sym!(y)),
            HashMap::from([("x".into(), sym!(w))]),
            fun!(F, fun!(G, sym!(w)), sym!(y)),
        );
        assert_substitute(
            fun!(F, fun!(G, sym!(x)), sym!(y), sym!(x)),
            HashMap::from([("x".into(), sym!(w))]),
            fun!(F, fun!(G, sym!(w)), sym!(y), sym!(w)),
        );
    }

    #[test]
    fn full_matches() {
        assert_substitute(sym!(a), HashMap::from([("a".into(), sym!(b))]), sym!(b));
        assert_substitute(
            fun!(F, sym!(x)),
            HashMap::from([("x".into(), sym!(y))]),
            fun!(F, sym!(y)),
        );
        assert_substitute(
            fun!(F, sym!(x), sym!(y)),
            HashMap::from([("x".into(), sym!(s)), ("y".into(), sym!(t))]),
            fun!(F, sym!(s), sym!(t)),
        );
        assert_substitute(
            fun!(F, fun!(G, sym!(x)), sym!(y)),
            HashMap::from([("x".into(), sym!(s)), ("y".into(), sym!(t))]),
            fun!(F, fun!(G, sym!(s)), sym!(t)),
        );
        assert_substitute(
            fun!(F, fun!(G, sym!(x)), sym!(y), sym!(x)),
            HashMap::from([("x".into(), sym!(s)), ("y".into(), sym!(t))]),
            fun!(F, fun!(G, sym!(s)), sym!(t), sym!(s)),
        );
    }

    fn assert_substitute(
        expr_to_be_substituted: Expr,
        matches: Matches,
        expected_expr_after_substitution: Expr,
    ) {
        let expr_after_substitution = expr_to_be_substituted.substitute(&matches);
        assert_eq!(expected_expr_after_substitution, expr_after_substitution);
    }
}
