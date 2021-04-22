use std::{fmt::Debug, ops::Range, process::Command};
use std::{
    fmt::{self, Display},
    io::Write,
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
        .about("Lipstick for C")
        .author("Roberto Vidal <vidal.roberto.j@gmail.com>")
        .setting(clap::AppSettings::ArgRequiredElseHelp)
        // .setting(clap::AppSettings::ArgsNegateSubcommands)
        .arg(
            Arg::with_name("filename")
                .required(true)
                .help("The file to be compiled to C"),
        )
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
        );

    #[cfg(feature = "dev")]
    let app = app.subcommand(
        App::new("syn").arg(
            Arg::with_name("filename")
                .required(true)
                .help("The file to be compiled"),
        ),
    );

    let matches = app.get_matches();

    if let Some(matches) = matches.subcommand_matches("cc") {
        let file_name = matches.value_of("filename").unwrap();
        let sess = Session::new(file_name)?;
        let compiled = sess.compile().unwrap_or_else(|e| e.report(&sess));
        let mut temp = tempfile::Builder::new().suffix(".c").tempfile()?;
        write!(temp, "{}", compiled)?;

        let exit = Command::new("cc")
            .arg(temp.path())
            .args(matches.values_of("flags").into_iter().flatten())
            .spawn()?
            .wait()?;
        return Ok(());
    }

    #[cfg(feature = "dev")]
    if let Some(matches) = matches.subcommand_matches("syn") {
        let file_name = matches.value_of("filename").unwrap();
        println!(
            "{:#?}",
            syn::parse_file(&std::fs::read_to_string(&file_name)?)?
        );
        return Ok(());
    }

    let file_name = matches.value_of("filename").unwrap();
    let sess = Session::new(file_name)?;
    let compiled = sess.compile().unwrap_or_else(|e| e.report(&sess));

    write!(std::io::stdout(), "{}", compiled)?;
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

        let type_info = typecheck::check(&syn_file);
        let error = match codegen::transform(&syn_file, type_info) {
            Ok(program) => return Ok(program),
            Err(e) => e,
        };

        let diagnostics = error
            .diagnostics
            .into_iter()
            .map(|d| {
                Diagnostic::error()
                    .with_message(d.msg)
                    .with_labels(vec![Label::primary((), self.span_to_range(d.span))])
            })
            .collect();

        Err(SessionError { diagnostics })?
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
