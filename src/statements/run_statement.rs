use std::{
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

use crate::{
    parser::{FlowStatement, Line, PrintStatement, RuntimeError, Statement},
    support::Support,
};

#[derive(Debug)]
pub enum ProgramError {
    NotNumbered,
}

pub struct Program {
    lines: BTreeMap<usize, Line>,
    flattened: Option<Flattened>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            lines: BTreeMap::new(),
            flattened: None,
        }
    }

    pub fn add(&mut self, ln: Line) -> Result<(), ProgramError> {
        match ln.num {
            Some(n) => {
                self.lines.insert(n, ln);
                Ok(())
            }
            None => Err(ProgramError::NotNumbered),
        }
    }
}

struct Flattened {
    labels: HashMap<usize, usize>,
    statements: Vec<Rc<dyn Statement>>,
}

pub struct Context {
    pub pc: usize,
}

impl Context {
    pub fn new() -> Self {
        Self { pc: 0 }
    }
}

impl Context {
    fn prepare(&mut self) {
        self.pc = 0;
    }
}

impl Program {
    fn flatten(&self) -> Flattened {
        // FIXME: this does not include the special handling for IF-statements as the Bajsic version does.
        let mut f = Flattened {
            statements: vec![],
            labels: HashMap::new(),
        };

        // Create iterator with items ordered by key (as lines is a BTreeMap)
        let mut iter = self.lines.iter();

        while let Some((num, line)) = iter.next() {
            f.labels.insert(*num, f.statements.len());
            let mut stmt_iter = line.statements.iter();
            while let Some(stmt) = stmt_iter.next() {
                f.statements.push(stmt.clone());
            }

            // Add a flow statement so that debugger can stop at the end of a line
            // FIXME: would it be more practical to have a StartOfLine?
            f.statements
                .push(Rc::<FlowStatement>::from(FlowStatement::EndOfLine));
        }

        f
    }

    fn run(&self, ctx: &mut Context, sup: &mut dyn Support) -> RuntimeError {
        println!("RUNNING PROGRAM:");
        println!("----------------");

        // Flatten if required. Note that this will be a temporary
        // copy is the reference to self is immutable.
        let local_flattened: Flattened;
        let flattened = match self.flattened {
            Some(ref f) => f,
            None => {
                local_flattened = self.flatten();
                &local_flattened
            }
        };

        while ctx.pc < flattened.statements.len() {
            let stmt = &flattened.statements[ctx.pc];
            let next_pc = match stmt.exec(self, ctx, sup) {
                Ok(next_pc) => next_pc,
                Err(e) => return e,
            };
            ctx.pc = next_pc;
        }

        RuntimeError::EndOfProgram
    }
}

#[derive(Debug)]
pub struct RunStatement {}

impl Statement for RunStatement {
    fn exec(
        &self,
        prg: &Program,
        ctx: &mut Context,
        sup: &mut dyn Support,
    ) -> Result<usize, RuntimeError> {
        ctx.prepare();
        Err(prg.run(ctx, sup))
    }
}

impl RunStatement {}
