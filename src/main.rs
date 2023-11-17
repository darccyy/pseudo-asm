use std::fs;

use asm::*;

fn main() {
    let file = fs::read_to_string("example").unwrap();
    let program = parse_program(&file);
    println!("{:#?}", program);

    run_program(program);
}
