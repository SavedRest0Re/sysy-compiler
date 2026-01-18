use std::fs::File;
use std::io::{Result, Write};

pub enum RVInst {
    Li {
        rd: &'static str,
        imm12: i32,
    },
    La {
        rd: &'static str,
        label: String,
    },
    Lw {
        rd: &'static str,
        rs1: &'static str,
        offset: i32,
    },
    Sw {
        rs2: &'static str,
        rs1: &'static str,
        offset: i32,
    },
    Xor {
        rd: &'static str,
        rs1: &'static str,
        rs2: &'static str,
    },
    Seqz {
        rd: &'static str,
        rs: &'static str,
    },
    Snez {
        rd: &'static str,
        rs: &'static str,
    },
    Sub {
        rd: &'static str,
        rs1: &'static str,
        rs2: &'static str,
    },
    Add {
        rd: &'static str,
        rs1: &'static str,
        rs2: &'static str,
    },
    Addi {
        rd: &'static str,
        rs1: &'static str,
        imm12: i32,
    },
    Mv {
        rd: &'static str,
        rs: &'static str,
    },
    Mul {
        rd: &'static str,
        rs1: &'static str,
        rs2: &'static str,
    },
    Div {
        rd: &'static str,
        rs1: &'static str,
        rs2: &'static str,
    },
    Rem {
        rd: &'static str,
        rs1: &'static str,
        rs2: &'static str,
    },
    Sgt {
        rd: &'static str,
        rs1: &'static str,
        rs2: &'static str,
    },
    Slt {
        rd: &'static str,
        rs1: &'static str,
        rs2: &'static str,
    },
    And {
        rd: &'static str,
        rs1: &'static str,
        rs2: &'static str,
    },
    Or {
        rd: &'static str,
        rs1: &'static str,
        rs2: &'static str,
    },
    Bnez {
        rs: &'static str,
        label: String,
    },
    J {
        label: String,
    },
    Ret,
    Call {
        label: String,
    },
}

impl RVInst {
    pub fn to_string(&self) -> String {
        match self {
            RVInst::Li { rd, imm12 } => format!("li {}, {}", rd, imm12),
            RVInst::La { rd, label } => format!("la {}, {}", rd, label),
            RVInst::Lw { rd, rs1, offset } => format!("lw {}, {}({})", rd, offset, rs1),
            RVInst::Sw { rs2, rs1, offset } => format!("sw {}, {}({})", rs2, offset, rs1),
            RVInst::Xor { rd, rs1, rs2 } => format!("xor {}, {}, {}", rd, rs1, rs2),
            RVInst::Seqz { rd, rs } => format!("seqz {}, {}", rd, rs),
            RVInst::Snez { rd, rs } => format!("snez {}, {}", rd, rs),
            RVInst::Sub { rd, rs1, rs2 } => format!("sub {}, {}, {}", rd, rs1, rs2),
            RVInst::Add { rd, rs1, rs2 } => format!("add {}, {}, {}", rd, rs1, rs2),
            RVInst::Addi { rd, rs1, imm12 } => format!("addi {}, {}, {}", rd, rs1, imm12),
            RVInst::Ret => "ret".to_string(),
            RVInst::Mv { rd, rs } => format!("mv {}, {}", rd, rs),
            RVInst::Mul { rd, rs1, rs2 } => format!("mul {}, {}, {}", rd, rs1, rs2),
            RVInst::Div { rd, rs1, rs2 } => format!("div {}, {}, {}", rd, rs1, rs2),
            RVInst::Rem { rd, rs1, rs2 } => format!("rem {}, {}, {}", rd, rs1, rs2),
            RVInst::Sgt { rd, rs1, rs2 } => format!("sgt {}, {}, {}", rd, rs1, rs2),
            RVInst::Slt { rd, rs1, rs2 } => format!("slt {}, {}, {}", rd, rs1, rs2),
            RVInst::And { rd, rs1, rs2 } => format!("and {}, {}, {}", rd, rs1, rs2),
            RVInst::Or { rd, rs1, rs2 } => format!("or {}, {}, {}", rd, rs1, rs2),
            RVInst::Bnez { rs, label } => format!("bnez {}, {}", rs, label),
            RVInst::J { label } => format!("j {}", label),
            RVInst::Call { label } => format!("call {}", label),
        }
    }

    pub fn to_string_indent2(&self) -> String {
        format!("  {}", self.to_string())
    }

    pub fn emit_indent2(&self, buf: &mut File) -> Result<()> {
        writeln!(buf, "  {}", self.to_string())
    }
}
