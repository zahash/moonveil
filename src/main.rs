mod expr;
mod macros;
mod xform;

use expr::Expr;
use xform::XForm;

fn main() {
    let add_pttrn = fun!(ADD, sym!(a), sym!(b));
    let add_expr = fun!(
        ADD,
        fun!(MUL, sym!(a), sym!(b)),
        fun!(SUB, sym!(c), sym!(d))
    );

    let matches = add_pttrn.match_with(&add_expr).unwrap();

    println!("{}", add_pttrn);
    println!("{}", add_expr);
    for (k, v) in matches.iter() {
        println!("{} -> {}", k, v);
    }

    let comm_add = xform!(fun!(ADD, sym!(a), sym!(b)), fun!(ADD, sym!(b), sym!(a)));
    println!("{}", comm_add);

    let trasformed_add = comm_add.apply(&add_expr);
    println!("{}", trasformed_add);
}
