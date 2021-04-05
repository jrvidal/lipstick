use std::io::Write;
use std::process::Command;

use clap::{App, Arg};

type Error = Box<dyn std::error::Error>;

mod codegen;

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
        let file = parse(file_name)?;
        let compiled = compile(&file)?;

        println!("{}", compiled);
        return Ok(());
    }

    if let Some(matches) = matches.subcommand_matches("cc") {
        let file_name = matches.value_of("filename").unwrap();
        let file = parse(file_name)?;
        let compiled = compile(&file)?;
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

fn parse(file_name: &str) -> Result<syn::File, Error> {
    let source = std::fs::read_to_string(file_name)?;
    Ok(syn::parse_file(&source)?)
}

fn compile(file: &syn::File) -> Result<codegen::Program, Error> {
    Ok(codegen::transform(file)?)
}
