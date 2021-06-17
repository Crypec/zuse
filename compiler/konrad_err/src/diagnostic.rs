use std::{cmp::Ordering, fmt};

use colored::*;
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

/// Represents the kind of diagnostic the compiler is supposed to emmit
#[derive(Debug, Eq, PartialEq)]
pub enum Level {
    /// Hard compile time errors
    Error(Span),
    /// Soft compile time warnings. The compiler will still exit with
    /// sucessfully even if you emit a Warning.
    Warning(Span),

    /// A Note can be used to give extra information to the user.
    Note(Span),

    /// Internal bugs inside the compiler. The end user is never supposed to see
    /// this.
    // NOTE(Simon): Because we also use diagnostics to represent internal errors we can't always
    // NOTE(Simon): guarantee that we know the src location which lead to the internal bug
    Internal(Option<Span>),
}

impl Ord for Level {
    fn cmp(&self, other: &Self) -> Ordering {
        if self == other {
            return Ordering::Equal;
        }

        match (self, other) {
            (Self::Internal(_), _) => Ordering::Greater,
            (_, Self::Internal(_)) => Ordering::Less,

            (Self::Error(_), _) => Ordering::Greater,
            (_, Self::Error(_)) => Ordering::Less,

            (Self::Warning(_), _) => Ordering::Greater,
            (_, Self::Warning(_)) => Ordering::Less,

            (Self::Note(_), _) => Ordering::Greater,
            #[allow(unreachable_patterns)]
            (_, Self::Note(_)) => Ordering::Less,
        }
    }
}

impl PartialOrd for Level {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) }
}

impl Level {
    pub const fn to_str(&self) -> &'static str {
        match self {
            Self::Error(_) => "error",
            Self::Warning(_) => "warning",
            Self::Note(_) => "note",
            Self::Internal(_) => "internal error",
        }
    }
}

#[derive(Debug, TypedBuilder)]
pub struct Suggestion {
    #[builder(setter(into))]
    pub msg: String,
    #[builder(setter(into))]
    pub url: String,
}

impl Diagnostic {
    pub fn add_note<S: Into<String>>(&mut self, note: S) { self.note = Some(note.into()); }

    pub fn set_msg<S: Into<String>>(&mut self, msg: S) { self.msg = msg.into(); }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f)?;
        if let Level::Error(ref sp) | Level::Warning(ref sp) | Level::Note(ref sp) = self.lvl {
            let file = std::fs::read_to_string(&sp.path).expect("failed to read src file while printing error");

            let color = match self.lvl {
                Level::Error(_) => Color::BrightRed,
                Level::Warning(_) | Level::Note(_) => Color::Yellow,
                Level::Internal(_) => Color::Cyan,
            };

            let lines: Vec<_> = file.lines().collect();

            if sp.start.line == sp.end.line {
                writeln!(f, "{}: {}", self.lvl.to_str().color(color).bold(), self.msg,)?;
                writeln!(
                    f,
                    " {} {}:{}:{}",
                    "-->".bright_blue().bold(),
                    sp.path.to_str().unwrap(),
                    sp.start.line + 1,
                    sp.start.col + 1,
                )?;

                emit_src_span(&sp, color, &lines, f)?;
                if let Some(note) = &self.note {
                    writeln!(f, "{}", note)?;
                }
            } else {
                todo!();
                /*
                let start = (&lines[sp.start.line][..sp.start.col]).bright_blue();
                let highlight: String = lines[sp.start.line..=sp.end.line]
                    .iter()
                    .copied()
                    .intersperse(&", ")
                    .collect();
                writeln!(
                    f,
                    "[{}] --> line: [{}..{} :: {}..{}]",
                    sp.path.to_str().unwrap(),
                    sp.start.line,
                    sp.end.line,
                    sp.start.col,
                    sp.end.col
                )?;
                */
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

fn emit_src_span(sp: &Span, color: Color, lines: &[&str], f: &mut fmt::Formatter) -> fmt::Result {
    let start = (&lines[sp.start.line][..sp.start.col]).bright_blue();
    let highlight = (&lines[sp.start.line][sp.start.col..=sp.end.col]).color(color).bold();

    let end = (&lines[sp.end.line][sp.end.col..][1..]).bright_blue();
    let line_num_len = sp.start.line.to_string().chars().count();

    writeln!(f, "{:>pad$}", "|".bold().bright_blue(), pad = line_num_len + 2)?;
    writeln!(
        f,
        "{} {:>pad$} {}{}{}",
        (sp.start.line + 1).to_string().bold().bright_blue(),
        "|".bold().bright_blue(),
        start,
        highlight,
        end,
        pad = line_num_len,
    )?;
    writeln!(f, "{:>pad$}", "|".bold().bright_blue(), pad = line_num_len + 2)
}
