pub enum Instr {
    Raw(String),
    Li {
        rd: &'static str,
        imm12: i32,
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
    Ret,
    Mv {
        rd: &'static str,
        rs: &'static str,
    },
    Mul {
        rd: &'static str,
        rs1: &'static str,
        rs2: &'static str,
    },
    Sgt {
        rd: &'static str,
        rs1: &'static str,
        rs2: &'static str,
    },
}

impl Instr {
    pub fn to_asm(&self) -> String {
        match self {
            Instr::Raw(s) => s.clone(),
            Instr::Li { rd, imm12 } => format!("li {}, {}", rd, imm12),
            Instr::Xor { rd, rs1, rs2 } => format!("xor {}, {}, {}", rd, rs1, rs2),
            Instr::Seqz { rd, rs } => format!("seqz {}, {}", rd, rs),
            Instr::Sub { rd, rs1, rs2 } => format!("sub {}, {}, {}", rd, rs1, rs2),
            Instr::Add { rd, rs1, rs2 } => format!("add {}, {}, {}", rd, rs1, rs2),
            Instr::Addi { rd, rs1, imm12 } => format!("addi {}, {}, {}", rd, rs1, imm12),
            Instr::Ret => "ret".to_string(),
            Instr::Mv { rd, rs } => format!("mv {}, {}", rd, rs),
            Instr::Mul { rd, rs1, rs2 } => format!("mul {}, {}, {}", rd, rs1, rs2),
            Instr::Sgt { rd, rs1, rs2 } => format!("sgt {}, {}, {}", rd, rs1, rs2),
        }
    }
}
