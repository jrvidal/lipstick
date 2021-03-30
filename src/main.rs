#[macro_use]
extern crate lalrpop_util;

use ast::grammar;

mod ast;
mod codegen;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file_name = std::env::args().skip(1).next().expect("No file given");
    let source = std::fs::read_to_string(&file_name).expect("Unable to read file");
    let program = grammar::ProgramParser::new()
        .parse(&source)
        .map_err(|e| format!("{}", e))?;

    let out = codegen::transform(program);
    println!("{}", out);
    Ok(())
}
