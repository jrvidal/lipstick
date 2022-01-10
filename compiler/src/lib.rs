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

mod c_lang;
mod codegen;
mod typecheck;

/// A compilation session
pub struct Session {
    files: SimpleFile<String, String>,
}

impl Session {
    /// Creates a compilation session from a source file
    pub fn new(file_name: String, source: String) -> Self {
        Self {
            files: SimpleFile::new(file_name, source),
        }
    }

    #[cfg(feature = "dev")]
    pub fn debug(&self) -> Result<String, Box<dyn std::error::Error>> {
        Ok(format!("{:#?}", syn::parse_file(&self.files.source())?))
    }

    /// Compiles the program into something ready to be printed as C code
    pub fn compile(&self) -> Result<impl Display, SessionError> {
        let syn_file = syn::parse_file(self.files.source()).map_err(|e| {
            let error = CompilationError {
                msg: format!("{}", e),
                span: e.span(),
            };

            SessionError {
                errors: vec![error],
                syntax: true,
                files: &self.files,
            }
        })?;

        let type_info = typecheck::check(&syn_file).map_err(|e| SessionError {
            errors: e.errors,
            files: &self.files,
            syntax: false,
        })?;

        match codegen::transform(&syn_file, type_info) {
            Ok(program) => Ok(program),
            Err(e) => Err(SessionError {
                errors: e.errors,
                syntax: false,
                files: &self.files,
            }),
        }
    }
}

/// An error during compilation
pub struct SessionError<'a> {
    /// Whether the error is due to parsing
    pub syntax: bool,

    /// the individual errors
    pub errors: Vec<CompilationError>,

    // diagnostics: Vec<Diagnostic<()>>,
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

    pub fn into_ansi(mut self) -> Vec<(String, Span)> {
        let errors = {
            let mut errors = vec![];

            std::mem::swap(&mut errors, &mut self.errors);

            errors
        };

        errors
            .into_iter()
            .map(|error| (error.span, self.map_err(error)))
            .map(|(span, diagnostic)| {
                let mut buffer = term::termcolor::Buffer::ansi();

                term::emit(
                    &mut buffer,
                    &term::Config::default(),
                    self.files,
                    &diagnostic,
                )
                .unwrap();

                (String::from_utf8(buffer.into_inner()).unwrap(), span)
            })
            .collect()
    }

    fn report_to_writer(
        mut self,
        writer: &mut dyn codespan_reporting::term::termcolor::WriteColor,
    ) {
        let n_of_errors = self.errors.len();

        let errors = {
            let mut errors = vec![];

            std::mem::swap(&mut errors, &mut self.errors);

            errors
        };

        for diagnostic in errors.into_iter().map(|err| self.map_err(err)) {
            term::emit(writer, &term::Config::default(), self.files, &diagnostic).unwrap()
        }

        write!(writer, "Compilation failed due to {} errors", n_of_errors).unwrap();
    }

    fn map_err(&self, error: CompilationError) -> Diagnostic<()> {
        Diagnostic::error()
            .with_message(error.msg)
            .with_labels(vec![Label::primary((), self.span_to_range(error.span))])
    }

    fn span_to_range(&self, span: Span) -> Range<usize> {
        self.linecolumn_to_index(span.start())..self.linecolumn_to_index(span.end())
    }

    fn linecolumn_to_index(&self, linecol: LineColumn) -> usize {
        let range = self.files.line_range((), linecol.line - 1).unwrap();

        range.start + linecol.column
    }
}

impl<'a> Display for SessionError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        std::writeln!(f, "Failed due to {} errors", self.errors.len())
    }
}

impl<'a> Debug for SessionError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl<'a> std::error::Error for SessionError<'a> {}

/// A compilation error
#[derive(Debug)]
pub struct CompilationError {
    pub span: Span,
    pub msg: String,
}

#[derive(Debug)]
struct CompilationErrors {
    errors: Vec<CompilationError>,
}

impl Display for CompilationErrors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for error in &self.errors {
            writeln!(f, "Error: {} ({:?})", error.msg, error.span)?;
        }

        Ok(())
    }
}

impl std::error::Error for CompilationErrors {}
