use std::fs::File;
use std::io::Write;

use koopa::ir::{Program, Value, ValueKind};

fn emit_init(program: &Program, v: Value, buf: &mut File) {
    let v_data = program.borrow_value(v);
    match v_data.kind() {
        ValueKind::Integer(i) => {
            writeln!(buf, "  .word {}", i.value()).unwrap();
        }
        ValueKind::ZeroInit(_) => {
            let size = v_data.ty().size();
            writeln!(buf, "  .zero {}", size).unwrap();
        }
        ValueKind::Aggregate(agg) => {
            for &e in agg.elems() {
                emit_init(program, e, buf);
            }
        }
        _ => unimplemented!(),
    }
}

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

        emit_init(program, alloc.init(), buf);
        writeln!(buf).unwrap();
    }
}
