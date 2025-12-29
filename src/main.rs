pub mod asm;
pub mod ast;
pub mod ir;

use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::read_to_string;
use std::io::Write;
use std::{fmt, io};

lalrpop_mod!(sysy);

fn main() -> Result<(), Error> {
    let mut args = args();
    args.next();

    let mode = args.next().unwrap();
    let input = args.next().unwrap();

    args.next();
    let output = args.next().unwrap();

    let input = read_to_string(input).map_err(Error::Io)?;
    dbg!(&input);

    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    println!("{:#?}", ast);

    let program = ir::generate_program(ast).map_err(Error::Ir)?;

    match mode.as_str() {
        "-koopa" | "-k" => {
            KoopaGenerator::from_path(output)
                .map_err(Error::Io)?
                .generate_on(&program)
                .map_err(Error::Io)?;
        }
        "-riscv" | "-r" => {
            let mut output = std::fs::File::create(output).map_err(Error::Io)?;
            asm::generate_asm(&program, &mut output);
        }
        _ => {
            unimplemented!("unknown output file type")
        }
    }

    Ok(())
}

enum Error {
    Io(io::Error),
    Ir(ir::Error),
    // Asm(asm::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ir(e) => write!(f, "ir error: {}", e),
            Self::Io(e) => write!(f, "io error: {}", e),
        }
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
