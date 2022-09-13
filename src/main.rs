mod expr;
mod macros;
mod xform;

use expr::Expr;
use xform::XForm;

fn main() {
    let differentiation = xform!(
        fun!(DIFF, fun!(var!(F), var!(x))),
        fun!(
            LIM,
            var!(h),
            sym!(ZERO),
            fun!(
                DIV,
                fun!(
                    SUB,
                    fun!(var!(F), fun!(ADD, var!(x), var!(h))),
                    fun!(var!(F), var!(x))
                ),
                var!(h)
            )
        )
    );

    println!("{}", differentiation);

    let square_diff = fun!(DIFF, fun!(SQUARE, var!(x)));

    let diffed = differentiation.apply(&square_diff);

    println!("{}", diffed);
}
