pub mod asm;
pub mod ast;
pub mod ir;

use koopa::back::KoopaGenerator;
use koopa::ir::Type;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::read_to_string;
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
    // println!("{}", input.clone());

    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    // println!("{:#?}", ast);

    // 设置 Koopa IR 适配 riscv32 的指针宽度, 而非宿主机的指针宽度
    Type::set_ptr_size(4);

    let program = ir::generate_program(ast).map_err(Error::Ir)?;

    // {
    //     KoopaGenerator::from_path(output.clone())
    //         .map_err(Error::Io)?
    //         .generate_on(&program)
    //         .map_err(Error::Io)?;
    //     let ir = read_to_string(output.clone()).unwrap();
    //     println!("{}", ir)
    // }

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
