use konrad_lexer::lexer::*;
use konrad_parser::parser::*;
use konrad_session::session::*;

use std::path::PathBuf;

use colored::*;

pub struct Workspace {
    pub path: PathBuf,
    pub files: Vec<PathBuf>,
}

impl Workspace {
    pub const fn new(path: PathBuf, files: Vec<PathBuf>) -> Self {
        Self { path, files }
    }

    pub fn build(&mut self, session: &mut Session) {
        let mut prog_ast = vec![];
        for file in &self.files {
            let src_buf = std::fs::read_to_string(file).expect("failed to read file");
            let tokens = Lexer::new(src_buf, file, session)
                .scan_tokens()
                .into_iter()
                .filter(|t| !t.kind.is_whitespace())
                .collect();

            let ast = Parser::new(tokens, session).parse_entrie_file(file.clone());
            println!(
                "[{}] :: {}",
                "compiling".bold().green(),
                file.to_str().unwrap()
            );
            prog_ast.push(ast);
        }
        prog_ast.iter().for_each(|n| {
            dbg!(n);
        });
    }
}
