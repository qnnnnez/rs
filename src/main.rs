use std::collections::{BTreeMap, BTreeSet};

mod sexpr;
mod interpreter;

fn main() {
    let sexpr = sexpr::parse("((let ([a 1]) (lambda (b) (+ a b))) 1)").unwrap();
    let mut interpreter = interpreter::Interpreter::new();
    let ast = interpreter.parse_sexpr(&sexpr[0], &mut BTreeSet::new()).unwrap();
    println!("{:?}", sexpr);
    println!("{:?}", ast);
    let result = interpreter.eval_ast(&ast);
    println!("{:?}", result);
}
