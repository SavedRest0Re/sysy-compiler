use koopa::ir::{FunctionData, Type};

use crate::ir::ctx::Ctx;

pub fn load_builtins(ctx: &mut Ctx) {
    let builtins = [
        (Type::get_i32(), "getint", vec![]),
        (Type::get_i32(), "getch", vec![]),
        (
            Type::get_i32(),
            "getarray",
            vec![Type::get_pointer(Type::get_i32())],
        ),
        (Type::get_unit(), "putint", vec![Type::get_i32()]),
        (Type::get_unit(), "putch", vec![Type::get_i32()]),
        (
            Type::get_unit(),
            "putarray",
            vec![Type::get_i32(), Type::get_pointer(Type::get_i32())],
        ),
        (Type::get_unit(), "starttime", vec![]),
        (Type::get_unit(), "stoptime", vec![]),
    ];

    for (ret_ty, name, params) in builtins {
        let function =
            ctx.program
                .new_func(FunctionData::new_decl(format!("@{name}"), params, ret_ty));

        // `funcs` 没有 `@` prefix
        ctx.funcs.insert(name.into(), function);
    }
}
