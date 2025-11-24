pub mod ast;
pub mod codegen;
pub mod irgen;
pub mod macros;

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

    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    println!("{:#?}", ast);

    let program = irgen::generate_program(&ast).map_err(Error::Generate)?;

    match mode.as_str() {
        "-koopa" => {
            KoopaGenerator::from_path(output)
                .map_err(Error::Io)?
                .generate_on(&program)
                .map_err(Error::Io)?;
        }
        "-riscv" => {
            let mut output = std::fs::File::create(output).map_err(Error::Io)?;
            let mut output_buf = Vec::new();
            codegen::generate_asm(&program, &mut output_buf);
            output.write_all(&output_buf).map_err(Error::Io)?;
        }
        _ => {
            unimplemented!("unknown output file type")
        }
    }

    Ok(())
}

enum Error {
    Generate(irgen::Error),
    Io(io::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Generate(e) => write!(f, "generate error: {}", e),
            Self::Io(e) => write!(f, "io error: {}", e),
        }
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
