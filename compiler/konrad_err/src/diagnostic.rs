use konrad_span::span::*;

use typed_builder::TypedBuilder;

/// A Diagnostic represents a message to the user
#[derive(Debug, TypedBuilder)]
pub struct Diagnostic {
    pub lvl: Level,

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
    Error(Span),
    /// Soft compile time warnings. The compiler will still exit with sucessfully even if you emit
    /// a Warning.
    Warning(Span),

    /// A Note can be used to give extra information to the user.
    Note(Span),

    /// Internal bugs inside the compiler. The end user is never supposed to see this.
    // NOTE(Simon): Because we also use diagnostics to represent internal errors we can't always
    // NOTE(Simon): guarantee that we know the src location which lead to the internal bug
    Internal(Option<Span>),
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
