mod expr;
mod macros;
mod xform;

use expr::Expr;
use xform::XForm;

fn main() {
    let add_pttrn = fun!(ADD, var!(a), var!(b));
    let add_expr = fun!(
        ADD,
        fun!(MUL, var!(a), var!(b)),
        fun!(SUB, var!(c), var!(d))
    );

    let matches = add_pttrn.match_with(&add_expr).unwrap();

    println!("{}", add_pttrn);
    println!("{}", add_expr);
    for (k, v) in matches.iter() {
        println!("{} -> {}", k, v);
    }

    let comm_add = xform!(fun!(ADD, var!(a), var!(b)), fun!(ADD, var!(b), var!(a)));
    println!("{}", comm_add);

    let trasformed_add = comm_add.apply(&add_expr);
    println!("{}", trasformed_add);
}
