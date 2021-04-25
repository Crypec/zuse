use konrad_span::span::*;

use typed_builder::TypedBuilder;

/// A Diagnostic represents a message to the user
#[derive(Debug, TypedBuilder)]
pub struct Diagnostic {
    pub lvl: Level,
    pub span: Span,

    #[builder(setter(into))]
    pub msg: String,

    #[builder(default, setter(strip_option, into))]
    pub note: Option<String>,

    #[builder(default)]
    pub suggestion: Vec<Suggestion>,
}

#[derive(Debug)]
/// Represents the kind of diagnostic the compiler is supposed to emmit
pub enum Level {
    /// Hard compile time errors
    Error,
    /// Soft compile time warnings. The compiler will still exit with sucessfully even if you emit
    /// a Warning.
    Warning,

    /// A Note can be used to give extra information to the user.
    Note,

    /// Internal bugs inside the compiler. The end user is never supposed to see this.
    Internal,
}

#[derive(Debug, TypedBuilder)]
pub struct Suggestion {
    #[builder(setter(into))]
    pub msg: String,
    #[builder(setter(into))]
    pub url: String,
}

impl Diagnostic {
    pub fn add_note<S: Into<String>>(&mut self, note: S) {
        self.note = Some(note.into());
    }
    pub fn set_msg<S: Into<String>>(&mut self, msg: S) {
        self.msg = msg.into();
    }
}
