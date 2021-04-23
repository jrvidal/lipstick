use std::{
    fmt::{self, Debug, Display},
    fs,
    io::Write,
    ops::Range,
    process::Command,
};

use clap::{App, Arg};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::{Files, SimpleFile},
    term,
};
use proc_macro2::{LineColumn, Span};

type Error = Box<dyn std::error::Error>;

mod c_lang;
mod codegen;
mod typecheck;

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

    #[cfg(feature = "dev")]
    if matches.value_of("Z") == Some("syn") {
        println!(
            "{:#?}",
            syn::parse_file(&std::fs::read_to_string(&file_name)?)?
        );
        return Ok(());
    }

    let sess = Session::new(file_name)?;
    let compiled = sess.compile().unwrap_or_else(|e| e.report(&sess));
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

struct Session<'a> {
    source: SimpleFile<&'a str, String>,
}

impl<'a> Session<'a> {
    fn new(file_name: &'a str) -> Result<Self, Error> {
        let source = std::fs::read_to_string(file_name)?;
        Ok(Self {
            source: SimpleFile::new(file_name, source),
        })
    }

    fn compile(&self) -> Result<c_lang::Program, SessionError> {
        let syn_file = syn::parse_file(self.source.source())
            .map_err(|e| {
                Diagnostic::error()
                    .with_message(format!("{}", e))
                    .with_labels(vec![Label::primary((), self.span_to_range(e.span()))])
            })
            .map_err(|d| SessionError {
                diagnostics: vec![d],
            })?;

        let type_info = typecheck::check(&syn_file).map_err(|e| self.map_err(e))?;
        match codegen::transform(&syn_file, type_info) {
            Ok(program) => Ok(program),
            Err(e) => Err(self.map_err(e)),
        }
    }

    fn map_err(&self, error: CompilationError) -> SessionError {
        let diagnostics = error
            .diagnostics
            .into_iter()
            .map(|d| {
                Diagnostic::error()
                    .with_message(d.msg)
                    .with_labels(vec![Label::primary((), self.span_to_range(d.span))])
            })
            .collect();

        SessionError { diagnostics }
    }

    fn span_to_range(&self, span: Span) -> Range<usize> {
        self.linecolumn_to_index(span.start())..self.linecolumn_to_index(span.end())
    }

    fn linecolumn_to_index(&self, linecol: LineColumn) -> usize {
        let range = self.source.line_range((), linecol.line - 1).unwrap();
        self.source.source()[range.start..]
            .char_indices()
            .skip(linecol.column)
            .next()
            .unwrap()
            .0
            + range.start
    }
}

struct SessionError {
    diagnostics: Vec<Diagnostic<()>>,
}

impl SessionError {
    fn report(self, session: &Session) -> ! {
        let mut writer =
            term::termcolor::StandardStream::stderr(term::termcolor::ColorChoice::Auto);

        for diagnostic in &self.diagnostics {
            term::emit(
                &mut writer,
                &term::Config::default(),
                &session.source,
                diagnostic,
            )
            .unwrap()
        }

        eprintln!(
            "Compilation failed due to {} errors",
            self.diagnostics.len()
        );
        std::process::exit(1);
    }
}

impl Display for SessionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        std::writeln!(f, "Failed due to {} errors", self.diagnostics.len())
    }
}

impl Debug for SessionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl std::error::Error for SessionError {}

#[derive(Debug)]
pub struct CodeError {
    pub span: Span,
    pub msg: String,
}

#[derive(Debug)]
pub struct CompilationError {
    pub diagnostics: Vec<CodeError>,
}

impl Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for error in &self.diagnostics {
            writeln!(f, "Error: {} ({:?})", error.msg, error.span)?;
        }

        Ok(())
    }
}

impl std::error::Error for CompilationError {}
