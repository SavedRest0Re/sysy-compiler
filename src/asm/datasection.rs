use std::fs::File;
use std::io::Write;

use koopa::ir::{Program, TypeKind, ValueKind};

pub fn generate_data_section(program: &Program, buf: &mut File) {
    for &inst in program.inst_layout() {
        let inst_data = program.borrow_value(inst);
        let ValueKind::GlobalAlloc(alloc) = inst_data.kind() else {
            panic!("expected global alloc");
        };

        let name = inst_data.name().clone().unwrap();
        let name = name.strip_prefix('@').unwrap();
        writeln!(buf, "  .data").unwrap();
        writeln!(buf, "  .global {name}").unwrap();
        writeln!(buf, "{name}:").unwrap();

        let init_data = program.borrow_value(alloc.init());
        match init_data.kind() {
            ValueKind::Integer(i) => {
                writeln!(buf, "  .word {}\n", i.value()).unwrap();
            }
            ValueKind::ZeroInit(_) => {
                let size = match inst_data.ty().kind() {
                    TypeKind::Pointer(base_ty) => base_ty.size(),
                    _ => unimplemented!(),
                };
                writeln!(buf, "  .zero {}\n", size).unwrap();
            }
            _ => unimplemented!(),
        }
    }
}
