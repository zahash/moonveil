use std::{collections::HashMap, fmt::Display};

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Sym(String),
    Var(String),
    Fun(Box<Expr>, Vec<Expr>),
}

type Matches = HashMap<String, Expr>;

impl Expr {
    pub fn match_with(&self, expr: &Expr) -> Option<Matches> {
        fn _match_with(pattern: &Expr, expr: &Expr, matches: &mut Matches) -> bool {
            use Expr::*;
            match (pattern, expr) {
                (Sym(p_name), Sym(e_name)) => p_name == e_name,
                (Sym(_), _) => false,

                (Var(p_name), expr) => {
                    if let Some(already_bound_expr) = matches.get(p_name) {
                        if already_bound_expr != expr {
                            return false;
                        }
                    }
                    matches.insert(p_name.clone(), expr.clone());
                    true
                }

                (Fun(_, _), Sym(_)) | (Fun(_, _), Var(_)) => false,
                (Fun(p_head, _), Fun(e_head, _)) if !_match_with(p_head, e_head, matches) => false,
                (Fun(_, p_args), Fun(_, e_args)) if p_args.len() != e_args.len() => false,

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
            Sym(_) => self.clone(),
            Var(name) => matches.get(name).unwrap_or(self).clone(),
            Fun(name, args) => Fun(
                Box::new(name.substitute(matches)),
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
    use crate::{fun, sym, var};
    use pretty_assertions::assert_eq;

    #[test]
    fn nothing_matches_with_a_symbol_except_itself() {
        assert_matches(sym!(a), sym!(a), vec![]);

        assert_no_matches(sym!(a), sym!(b));
        assert_no_matches(sym!(a), var!(a));
        assert_no_matches(sym!(a), fun!(F));
    }

    #[test]
    fn anything_matches_with_a_variable() {
        assert_matches(var!(a), sym!(a), vec![("a", sym!(a))]);
        assert_matches(var!(a), var!(a), vec![("a", var!(a))]);
        assert_matches(var!(a), var!(b), vec![("a", var!(b))]);
        assert_matches(var!(a), fun!(F), vec![("a", fun!(F))]);
    }

    #[test]
    fn function_doesnot_match_with_non_functions() {
        assert_no_matches(fun!(F), sym!(a));
        assert_no_matches(fun!(F), var!(a));
    }

    #[test]
    fn function_matches_with_other_functions_with_matching_head() {
        assert_no_matches(fun!(sym!(F)), fun!(sym!(G)));
        assert_no_matches(fun!(sym!(F)), fun!(var!(F)));
        assert_no_matches(fun!(sym!(F)), fun!(fun!(F)));
        assert_no_matches(fun!(fun!(F)), fun!(fun!(G)));

        assert_matches(fun!(var!(a)), fun!(sym!(a)), vec![("a", sym!(a))]);
        assert_matches(fun!(var!(a)), fun!(var!(a)), vec![("a", var!(a))]);
        assert_matches(fun!(var!(a)), fun!(var!(b)), vec![("a", var!(b))]);
        assert_matches(fun!(var!(a)), fun!(fun!(F)), vec![("a", fun!(F))]);

        assert_matches(
            fun!(fun!(F, var!(x0), fun!(G, var!(x1)), var!(x2))),
            fun!(fun!(F, var!(y0), fun!(G, var!(y1)), var!(y2))),
            vec![("x0", var!(y0)), ("x1", var!(y1)), ("x2", var!(y2))],
        );
    }

    #[test]
    fn function_pattern_only_matches_with_other_functions_with_same_number_of_args() {
        assert_no_matches(fun!(F, var!(x0)), fun!(F, var!(y0), var!(y1)));
        assert_no_matches(fun!(F, sym!(x0)), fun!(F, sym!(y0)));
        assert_no_matches(fun!(F, sym!(x0)), fun!(F, var!(x0)));

        assert_matches(fun!(F), fun!(F), vec![]);
        assert_matches(fun!(F, sym!(x0)), fun!(F, sym!(x0)), vec![]);
        assert_matches(fun!(F, var!(x0)), fun!(F, var!(y0)), vec![("x0", var!(y0))]);
        assert_matches(fun!(F, var!(x0)), fun!(F, sym!(x0)), vec![("x0", sym!(x0))]);
    }

    #[test]
    fn test_with_same_repeated_variables() {
        assert_no_matches(fun!(F, sym!(x0), sym!(x0)), fun!(F, sym!(y0), sym!(y0)));
        assert_no_matches(fun!(F, sym!(x0), sym!(x0)), fun!(F, sym!(y0), sym!(y1)));
        assert_no_matches(
            fun!(F, sym!(x0), fun!(G, sym!(x1)), sym!(x0)),
            fun!(F, sym!(y0), fun!(G, sym!(x1)), sym!(y0)),
        );

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
    fn substitute_variable_with_anything() {
        assert_substitute(var!(a), HashMap::new(), var!(a));
        assert_substitute(var!(a), HashMap::from([("a".into(), sym!(b))]), sym!(b));
        assert_substitute(var!(a), HashMap::from([("a".into(), var!(b))]), var!(b));
        assert_substitute(var!(a), HashMap::from([("a".into(), fun!(F))]), fun!(F));
    }

    #[test]
    fn empty_function_matches() {
        assert_substitute(fun!(F), HashMap::new(), fun!(F));
        assert_substitute(
            fun!(F, sym!(z), fun!(G, sym!(x)), var!(y), var!(x)),
            HashMap::new(),
            fun!(F, sym!(z), fun!(G, sym!(x)), var!(y), var!(x)),
        );
    }

    #[test]
    fn substitute_function_head() {
        assert_substitute(
            fun!(sym!(F)),
            HashMap::from([("F".into(), var!(G))]),
            fun!(sym!(F)),
        );
        assert_substitute(
            fun!(var!(F)),
            HashMap::from([("F".into(), var!(G))]),
            fun!(var!(G)),
        );
        assert_substitute(
            fun!(fun!(var!(F))),
            HashMap::from([("F".into(), sym!(G))]),
            fun!(fun!(sym!(G))),
        );
    }

    #[test]
    fn partial_function_matches() {
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
    fn full_function_matches() {
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
