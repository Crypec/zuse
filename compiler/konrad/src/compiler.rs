use glob::glob;
use konrad_session::session::*;

use crate::workspace::*;

pub struct Compiler {
    workspaces: Vec<Workspace>,
    session:    Session,
}

impl Compiler {
    pub fn new() -> Self {
        let current_directory = std::env::current_dir().expect("failed to get current directory");
        let zuse_files = glob("**/*.zs").expect("failed to get").filter_map(Result::ok).collect();
        let default_ws = Workspace::new(current_directory, zuse_files);

        Self {
            workspaces: vec![default_ws],
            session:    Session::new(),
        }
    }

    pub fn run(&mut self) {
        for ws in &mut self.workspaces {
            ws.build(&mut self.session);
        }
        self.session.emit_diagnostics();
    }
}
