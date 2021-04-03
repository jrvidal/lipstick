#[macro_use]
extern crate lalrpop_util;

use std::io::Write;
use std::process::Command;

use ast::grammar;

use clap::{App, Arg};

type Error = Box<dyn std::error::Error>;

mod ast;
mod codegen;
mod types;

fn main() -> Result<(), Error> {
    let matches = App::new("lipstick")
        .about("Lipstick for C")
        .author("Roberto Vidal <vidal.roberto.j@gmail.com>")
        .setting(clap::AppSettings::ArgRequiredElseHelp)
        .subcommand(
            App::new("cc")
                .about("Invokes `cc` on the output")
                .arg(
                    Arg::with_name("filename")
                        .required(true)
                        .help("The file to be compiled"),
                )
                .arg(
                    Arg::with_name("flags")
                        .multiple(true)
                        .last(true)
                        .help("Flags for the cc compiler"),
                ),
        )
        .subcommand(
            App::new("print")
                .about("Prints the compiled C output")
                .arg(Arg::with_name("filename").required(true)),
        )
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("print") {
        let file_name = matches.value_of("filename").unwrap();
        let program = parse(file_name)?;
        let compiled = compile(program)?;

        println!("{}", compiled);
        return Ok(());
    }

    if let Some(matches) = matches.subcommand_matches("cc") {
        let file_name = matches.value_of("filename").unwrap();
        let program = parse(file_name)?;
        let compiled = compile(program)?;
        let mut temp = tempfile::Builder::new().suffix(".c").tempfile()?;
        write!(temp, "{}", compiled)?;

        let exit = Command::new("cc")
            .arg(temp.path())
            .args(matches.values_of("flags").into_iter().flatten())
            .spawn()?
            .wait()?;
        return Ok(());
    }

    Ok(())
}

fn parse(file_name: &str) -> Result<ast::Program, Error> {
    let source = std::fs::read_to_string(file_name)?;
    let program = grammar::ProgramParser::new()
        .parse(&source)
        .map_err(|e| format!("{}", e))?;
    Ok(program)
}

fn compile(program: ast::Program) -> Result<codegen::Program, Error> {
    types::check(&program)?;

    let compiled = codegen::transform(program);
    Ok(compiled)
}

#[test]
fn fixture() {
    let source = r#"
        fn main() {
            let x: u32 = &x as u64;
        }
    "#;
    let program = grammar::ProgramParser::new().parse(&source).unwrap();

    panic!("{:#?}", program);
}
