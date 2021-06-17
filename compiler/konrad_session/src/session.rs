use konrad_err::diagnostic::*;
use konrad_span::span::*;

// TODO(Simon): Currently not threadsafe!
#[derive(Debug)]
pub struct Session {
    pub diagnostics: Vec<Diagnostic>,
}

impl Session {
    pub const fn new() -> Self { Self { diagnostics: vec![] } }

    pub fn span_err<S: Into<String>>(&mut self, msg: S, span: Span) -> &mut Diagnostic {
        self.build_diagnostic(msg, Level::Error(span))
    }

    pub fn span_warn<S: Into<String>>(&mut self, msg: S, span: Span) -> &mut Diagnostic {
        self.build_diagnostic(msg, Level::Warning(span))
    }

    pub fn span_internal_err<S: Into<String>>(&mut self, msg: S, span: Option<Span>) -> &mut Diagnostic {
        let diag = Diagnostic::builder().msg(msg).lvl(Level::Internal(span)).build();
        self.report_diagnostic(diag)
    }

    pub fn build_diagnostic<S: Into<String>>(&mut self, msg: S, lvl: Level) -> &mut Diagnostic {
        let diag = Diagnostic::builder().lvl(lvl).msg(msg).build();
        self.report_diagnostic(diag)
    }

    pub fn report_diagnostic(&mut self, diag: Diagnostic) -> &mut Diagnostic {
        self.diagnostics.push(diag);
        self.diagnostics.last_mut().unwrap()
    }

    pub fn emit_diagnostics(&mut self) {
        self.diagnostics.sort_by(|a, b| a.lvl.cmp(&b.lvl));
        self.diagnostics.iter().for_each(|d| eprintln!("{}", d));
    }
}
