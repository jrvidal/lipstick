use std::{
    fmt::{self, Debug, Display},
    ops::Range,
};

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

/// A compilation session
pub struct Session {
    files: SimpleFile<String, String>,
}

impl Session {
    /// Creates a compilation session from a source file
    pub fn new(file_name: &str) -> Result<Self, Error> {
        let source = std::fs::read_to_string(file_name)?;

        Ok(Self {
            files: SimpleFile::new(file_name.to_string(), source),
        })
    }

    #[cfg(feature = "dev")]
    pub fn debug(&self) -> Result<String, Error> {
        Ok(format!(
            "{:#?}",
            syn::parse_file(&self.files.source())?
        ))
    }

    /// Compiles the program into something ready to be printed as C code
    pub fn compile(&self) -> Result<impl Display, SessionError> {
        let syn_file = syn::parse_file(self.files.source())
            .map_err(|e| {
                Diagnostic::error()
                    .with_message(format!("{}", e))
                    .with_labels(vec![Label::primary((), self.span_to_range(e.span()))])
            })
            .map_err(|d| SessionError {
                diagnostics: vec![d],
                files: &self.files
            })?;

        let type_info = typecheck::check(&syn_file).map_err(|e| self.map_err(e))?;

        match codegen::transform(&syn_file, type_info) {
            Ok(program) => Ok(program),
            Err(e) => Err(self.map_err(e)),
        }
    }

    fn map_err(&self, error: CompilationError) -> SessionError<'_> {
        let diagnostics = error
            .diagnostics
            .into_iter()
            .map(|d| {
                Diagnostic::error()
                    .with_message(d.msg)
                    .with_labels(vec![Label::primary((), self.span_to_range(d.span))])
            })
            .collect();

        SessionError { diagnostics, files: &self.files }
    }

    fn span_to_range(&self, span: Span) -> Range<usize> {
        self.linecolumn_to_index(span.start())..self.linecolumn_to_index(span.end())
    }

    fn linecolumn_to_index(&self, linecol: LineColumn) -> usize {
        let range = self.files.line_range((), linecol.line - 1).unwrap();
        self.files.source()[range.start..]
            .char_indices()
            .skip(linecol.column)
            .next()
            .unwrap()
            .0
            + range.start
    }
}

/// An error during compilation
pub struct SessionError<'a> {
    diagnostics: Vec<Diagnostic<()>>,
    files: &'a SimpleFile<String, String>,
}

impl<'a> SessionError<'a> {
    /// Reports the error and exits the process
    pub fn report(self) -> ! {
        let mut writer =
            term::termcolor::StandardStream::stderr(term::termcolor::ColorChoice::Auto);

        self.report_to_writer(&mut writer);

        std::process::exit(1);
    }

    /// Reports the error to a given writer
    pub fn report_to_writer(self, writer: &mut dyn codespan_reporting::term::termcolor::WriteColor) {
        for diagnostic in &self.diagnostics {
            term::emit(
                writer,
                &term::Config::default(),
                self.files,
                diagnostic,
            )
            .unwrap()
        }

        write!(
            writer,
            "Compilation failed due to {} errors",
            self.diagnostics.len()
        ).unwrap();
    }
}

impl<'a> Display for SessionError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        std::writeln!(f, "Failed due to {} errors", self.diagnostics.len())
    }
}

impl<'a> Debug for SessionError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl<'a> std::error::Error for SessionError<'a> {}

#[derive(Debug)]
struct CodeError {
    pub span: Span,
    pub msg: String,
}

#[derive(Debug)]
struct CompilationError {
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
