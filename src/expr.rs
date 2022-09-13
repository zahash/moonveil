use std::{collections::HashMap, fmt::Display};

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Sym(String),
    Var(String),
    Fun(String, Vec<Expr>),
}

type Matches = HashMap<String, Expr>;

impl Expr {
    pub fn match_with(&self, expr: &Expr) -> Option<Matches> {
        fn _match_with(pattern: &Expr, expr: &Expr, matches: &mut Matches) -> bool {
            use Expr::*;
            match (pattern, expr) {
                (Sym(_), Sym(_)) => todo!(),

                (Var(name), expr) => {
                    if let Some(already_bound_expr) = matches.get(name) {
                        if already_bound_expr != expr {
                            return false;
                        }
                    }
                    matches.insert(name.clone(), expr.clone());
                    true
                }

                (Fun(_, _), Var(_)) => false,
                (Fun(pttrn_name, _), Fun(expr_name, _)) if pttrn_name != expr_name => false,
                (Fun(_, pttrn_args), Fun(_, expr_args)) if pttrn_args.len() != expr_args.len() => {
                    false
                }

                (Fun(_, pttrn_args), Fun(_, expr_args)) => pttrn_args
                    .iter()
                    .zip(expr_args)
                    .all(|(pttrn_arg, expr_arg)| _match_with(pttrn_arg, expr_arg, matches)),

                _ => todo!(),
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
            Sym(_) => self.clone(),
            Var(name) => matches.get(name).unwrap_or(self).clone(),
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
            use Expr::*;
            match expr {
                Sym(name) | Var(name) => name.to_string(),
                Fun(name, args) => format!(
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
    use crate::{fun, var};
    use pretty_assertions::assert_eq;

    #[test]
    fn anything_matches_with_a_variable() {
        assert_matches(var!(a), var!(a), vec![("a", var!(a))]);
        assert_matches(var!(a), var!(b), vec![("a", var!(b))]);
        assert_matches(var!(a), fun!(F), vec![("a", fun!(F))]);
        assert_matches(var!(a), fun!(F, var!(x)), vec![("a", fun!(F, var!(x)))]);
        assert_matches(
            var!(a),
            fun!(F, var!(x), fun!(G, var!(y)), var!(z)),
            vec![("a", fun!(F, var!(x), fun!(G, var!(y)), var!(z)))],
        );
    }

    #[test]
    fn function_pattern_only_matches_with_other_functions_with_same_name_and_number_of_args() {
        assert_no_matches(fun!(F), var!(a));
        assert_no_matches(fun!(F), fun!(G));
        assert_no_matches(fun!(F, var!(x0)), fun!(F, var!(y0), var!(y1)));

        assert_matches(fun!(F), fun!(F), vec![]);
        assert_matches(fun!(F, var!(x0)), fun!(F, var!(y0)), vec![("x0", var!(y0))]);
    }

    #[test]
    fn test_with_same_repeated_variables() {
        assert_matches(
            fun!(F, var!(x0), var!(x0)),
            fun!(F, var!(y0), var!(y0)),
            vec![("x0", var!(y0))],
        );
        assert_matches(
            fun!(F, fun!(G, var!(x0), var!(x1)), var!(x0)),
            fun!(F, fun!(G, var!(y0), var!(y1)), var!(y0)),
            vec![("x0", var!(y0)), ("x1", var!(y1))],
        );

        assert_no_matches(fun!(F, var!(x0), var!(x0)), fun!(F, var!(y0), var!(y1)));
        assert_no_matches(
            fun!(F, fun!(G, var!(x0), var!(x0)), var!(x1)),
            fun!(F, fun!(G, var!(y0), var!(y1)), var!(y1)),
        );
    }

    #[test]
    fn test_recursive_pattern_matching() {
        assert_matches(
            fun!(F, var!(x0), var!(x1)),
            fun!(F, var!(y0), var!(y1)),
            vec![("x0", var!(y0)), ("x1", var!(y1))],
        );
        assert_matches(
            fun!(F, var!(x0), fun!(G, var!(x1)), var!(x2)),
            fun!(F, var!(y0), fun!(G, var!(y1)), var!(y2)),
            vec![("x0", var!(y0)), ("x1", var!(y1)), ("x2", var!(y2))],
        );
        assert_matches(
            fun!(F, var!(x0)),
            fun!(F, fun!(G, var!(y0), var!(y1))),
            vec![("x0", fun!(G, var!(y0), var!(y1)))],
        );

        assert_no_matches(
            fun!(F, fun!(G, var!(x0))),
            fun!(F, fun!(G, var!(y0), var!(y1))),
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
    use crate::{fun, sym, var};
    use std::collections::HashMap;

    #[test]
    fn symbols_never_get_substituted() {
        assert_substitute(sym!(a), HashMap::new(), sym!(a));
        assert_substitute(sym!(a), HashMap::from([("a".into(), sym!(b))]), sym!(a));
        assert_substitute(sym!(a), HashMap::from([("a".into(), var!(b))]), sym!(a));
        assert_substitute(sym!(a), HashMap::from([("a".into(), fun!(F))]), sym!(a));
    }

    #[test]
    fn empty_matches() {
        assert_substitute(fun!(F), HashMap::new(), fun!(F));
        assert_substitute(var!(a), HashMap::new(), var!(a));
        assert_substitute(
            fun!(F, fun!(G, var!(x)), var!(y)),
            HashMap::new(),
            fun!(F, fun!(G, var!(x)), var!(y)),
        );
    }

    #[test]
    fn partial_matches() {
        assert_substitute(
            fun!(F, var!(x), var!(y)),
            HashMap::from([("x".into(), var!(w))]),
            fun!(F, var!(w), var!(y)),
        );
        assert_substitute(
            fun!(F, fun!(G, var!(x)), var!(y)),
            HashMap::from([("x".into(), var!(w))]),
            fun!(F, fun!(G, var!(w)), var!(y)),
        );
        assert_substitute(
            fun!(F, fun!(G, var!(x)), var!(y), var!(x)),
            HashMap::from([("x".into(), var!(w))]),
            fun!(F, fun!(G, var!(w)), var!(y), var!(w)),
        );
    }

    #[test]
    fn full_matches() {
        assert_substitute(var!(a), HashMap::from([("a".into(), var!(b))]), var!(b));
        assert_substitute(
            fun!(F, var!(x)),
            HashMap::from([("x".into(), var!(y))]),
            fun!(F, var!(y)),
        );
        assert_substitute(
            fun!(F, var!(x), var!(y)),
            HashMap::from([("x".into(), var!(s)), ("y".into(), var!(t))]),
            fun!(F, var!(s), var!(t)),
        );
        assert_substitute(
            fun!(F, fun!(G, var!(x)), var!(y)),
            HashMap::from([("x".into(), var!(s)), ("y".into(), var!(t))]),
            fun!(F, fun!(G, var!(s)), var!(t)),
        );
        assert_substitute(
            fun!(F, fun!(G, var!(x)), var!(y), var!(x)),
            HashMap::from([("x".into(), var!(s)), ("y".into(), var!(t))]),
            fun!(F, fun!(G, var!(s)), var!(t), var!(s)),
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
