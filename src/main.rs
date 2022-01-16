use std::{fs, io::Write, process::Command};

use clap::{App, Arg};
use compiler::Session;

type Error = Box<dyn std::error::Error>;

fn main() -> Result<(), Error> {
    let app = App::new("lipstick")
        .version(clap::crate_version!())
        .about(
            "Lipstick for C\
           \n\
           \nCompile Rust-like syntax into C code",
        )
        .author("Roberto Vidal <vidal.roberto.j@gmail.com>")
        .setting(clap::AppSettings::ArgRequiredElseHelp)
        .arg(
            Arg::with_name("filename")
                .value_name("INPUT")
                .required(true)
                .help("The file to be compiled to C"),
        )
        .arg(
            Arg::with_name("compile_c")
                .short("C")
                .long("cc")
                .required(false)
                .help("Compile the output of lipstick by calling your C compiler"),
        )
        .arg(
            Arg::with_name("compiler")
                .long("compiler")
                .value_name("COMPILER")
                .help("Specifies which C compiler to invoke [default: cc]")
                .required(false)
                .requires("compile_c"),
        )
        .arg(
            Arg::with_name("out")
                .short("o")
                .long("out")
                .value_name("OUT")
                .help("The file to write the output to (\"-\" for stdout) [default: <INPUT>.c]")
                .takes_value(true)
                .allow_hyphen_values(true)
                .required(false),
        )
        .arg(
            Arg::with_name("flags")
                .value_name("COMPILER FLAGS")
                .multiple(true)
                .last(true)
                .requires("compile_c")
                .help("Flags for the `cc` compiler"),
        )
        .arg(
            Arg::with_name("Z")
                .value_name("FEATURE")
                .short("Z")
                .takes_value(true)
                .help("Debugging flags")
                .possible_values(&["syn"]),
        );

    let matches = app.get_matches();
    let file_name = matches.value_of("filename").unwrap();

    let source = std::fs::read_to_string(file_name)?;

    let sess = Session::new(file_name.to_string(), source);

    #[cfg(feature = "dev")]
    if matches.value_of("Z") == Some("syn") {
        println!("{}", sess.debug()?);
        return Ok(());
    }

    let compiled = sess.compile().unwrap_or_else(|e| e.report());

    let (mut outfile, out): (Box<dyn Write>, _) = match matches.value_of("out") {
        Some(out) => {
            if out == "-" {
                (Box::new(std::io::stdout()), None)
            } else {
                (Box::new(fs::File::open(out)?), Some(out.to_string()))
            }
        }
        None => {
            let out = format!("{}.c", file_name);
            (Box::new(fs::File::create(&out)?), Some(out))
        }
    };

    if matches.is_present("compile_c") && out.is_none() {
        clap::Error {
            message: "`cc` cannot be used with stdout output".to_string(),
            kind: clap::ErrorKind::InvalidValue,
            info: None,
        }
        .exit()
    }

    write!(outfile, "{}", compiled)?;

    if matches.is_present("compile_c") {
        let out = out.unwrap();

        let exit = Command::new(matches.value_of("compiler").unwrap_or("cc"))
            .arg(out)
            .args(matches.values_of("flags").into_iter().flatten())
            .spawn()?
            .wait()?;

        std::process::exit(exit.code().unwrap_or(1));
    }

    Ok(())
}
