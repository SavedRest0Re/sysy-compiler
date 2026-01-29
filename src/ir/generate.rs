use koopa::ir::{BinaryOp, Function, FunctionData, Type, TypeKind, Value, builder::ValueBuilder};

use crate::{
    ast::{
        AddExp, Block, BlockItem, CompUnit, ConstDef, ConstInitVal, Decl, EqExp, Exp, FuncDef,
        FuncType, GlobalItem, InitVal, LAndExp, LOrExp, LVal, MulExp, PrimaryExp, RelExp, Stmt,
        UnaryExp, UnaryOp, VarDef,
    },
    ir::{
        Error, IRGen, IRResult,
        array::{
            build_array_type, build_const_aggregate, build_param_ty, emit_elem_ptr_by_indices,
            eval_array_dims, flatten_const_init, flatten_init, flattenindex_to_indices,
            lval_address,
        },
        builtin::load_builtins,
        const_eval::{consteval, consteval_exp},
        ctx::{Ctx, Symbol},
    },
};

impl IRGen for CompUnit {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        load_builtins(ctx);

        for item in self.items.iter() {
            match item {
                GlobalItem::FuncDef(func_def) => {
                    let function = create_function(ctx, func_def);
                    ctx.funcs.insert(func_def.ident.clone(), function);
                }
                GlobalItem::GlobalDecl(decl) => {
                    decl.generate(ctx)?;
                }
            }
        }

        for item in self.items.iter() {
            match item {
                GlobalItem::FuncDef(func_def) => {
                    func_def.generate(ctx)?;
                }
                _ => {}
            }
        }

        Ok(())
    }
}

fn create_function(ctx: &mut Ctx, func_def: &FuncDef) -> Function {
    let ret_ty = func_def.ret_ty.generate(ctx).unwrap();
    let mut param_tys = vec![];
    if let Some(ref params) = func_def.params {
        for p in params.params.iter() {
            let ty = build_param_ty(p, ctx).unwrap();
            param_tys.push((Some(format!("@{}", p.ident)), ty));
        }
    }

    ctx.program.new_func(FunctionData::with_param_names(
        format!("@{}", func_def.ident),
        param_tys,
        ret_ty,
    ))
}

impl IRGen for FuncDef {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        let function = ctx.funcs.get(&format!("{}", self.ident)).unwrap();
        ctx.set_cur_func(*function);
        ctx.symbol_table.enter_scope(); // new scope

        // Create entry basic block and set it as current
        let entry_bb = ctx.create_bb(Some("%entry"));
        ctx.set_cur_bb(entry_bb);

        // gen parameter
        if let Some(ref params) = self.params {
            for (i, param) in params.params.iter().enumerate() {
                let ty = build_param_ty(param, ctx)?;
                let var_alloc = ctx.emit_alloc(ty);
                ctx.set_value_name(var_alloc, format!("%{}", param.ident));

                // copy parameter to local variable
                ctx.emit_store(ctx.func_params()[i], var_alloc);

                // add symbol
                ctx.symbol_table
                    .define(param.ident.clone(), Symbol::Var(var_alloc))?;
            }
        }

        self.block.generate(ctx)?;

        // 有些函数没有显示的 `return` 语句, 需要手动生成对应的 ir.
        if !ctx.is_cur_bb_terminated() {
            match self.ret_ty {
                FuncType::Void => {
                    ctx.emit_ret(None);
                }
                FuncType::Int => {
                    let zero = ctx.emit_integer(0);
                    ctx.emit_ret(Some(zero));
                }
            }
        }

        ctx.symbol_table.exit_scope();
        ctx.unset_cur_func();

        Ok(())
    }
}

impl IRGen for FuncType {
    type Output = Type;

    fn generate(&self, _ctx: &mut Ctx) -> IRResult<Self::Output> {
        Ok(match self {
            FuncType::Int => Type::get_i32(),
            FuncType::Void => Type::get_unit(),
        })
    }
}

impl IRGen for Block {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        ctx.symbol_table.enter_scope();
        for item in self.items.iter() {
            if ctx.is_cur_bb_terminated() {
                break;
            }
            match item {
                BlockItem::Decl(decl) => decl.generate(ctx)?,
                BlockItem::Stmt(stmt) => stmt.generate(ctx)?,
            };
        }
        ctx.symbol_table.exit_scope();
        Ok(())
    }
}

impl IRGen for Stmt {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            Stmt::Assign(lval, exp) => {
                let rhs_value = exp.generate(ctx)?;

                let addr = lval_address(ctx, lval)?;
                match ctx.value_type(addr).kind() {
                    TypeKind::Pointer(pointee) => match pointee.kind() {
                        TypeKind::Array(_, _) => return Err(Error::ArrayAssign),
                        _ => {}
                    },
                    _ => {}
                }
                ctx.emit_store(rhs_value, addr);
            }
            Stmt::Return(exp) => {
                let result = match exp {
                    Some(exp) => Some(exp.generate(ctx)?),
                    None => None,
                };
                ctx.emit_ret(result);
                // return must terminate the basic block
                // ctx.unset_cur_bb();
            }
            Stmt::Exp(exp) => {
                if let Some(exp) = exp {
                    exp.generate(ctx)?;
                }
            }
            Stmt::Block(block) => block.generate(ctx)?,
            Stmt::If(cond, then_stmt, else_stmt) => {
                let counter = ctx.fetch_counter();

                let cond_val = cond.generate(ctx)?;
                let then_bb = ctx.create_bb(Some(&format!("%then_{counter}")));
                let end_bb = ctx.create_bb(Some(&format!("%endif_{counter}")));

                if let Some(else_stmt) = else_stmt {
                    let else_bb = ctx.create_bb(Some(&format!("%else_{counter}")));
                    ctx.emit_cond_br(cond_val, then_bb, else_bb);

                    ctx.set_cur_bb(then_bb);
                    then_stmt.generate(ctx)?;
                    ctx.emit_br_if_needed(end_bb); // maybe `ret`

                    ctx.set_cur_bb(else_bb);
                    else_stmt.generate(ctx)?;
                    ctx.emit_br_if_needed(end_bb);
                } else {
                    ctx.emit_cond_br(cond_val, then_bb, end_bb);

                    ctx.set_cur_bb(then_bb);
                    then_stmt.generate(ctx)?;
                    ctx.emit_br_if_needed(end_bb);
                }

                ctx.set_cur_bb(end_bb);
            }
            Stmt::While(cond, body) => {
                let counter = ctx.fetch_counter();

                let cond_bb = ctx.create_bb(Some(&format!("%cond_{counter}")));
                let body_bb = ctx.create_bb(Some(&format!("%body_{counter}")));
                let end_bb = ctx.create_bb(Some(&format!("%end_{counter}")));

                ctx.loop_stack.push((cond_bb, end_bb));

                // jump to cond_bb from cur_bb
                ctx.emit_jump(cond_bb);

                // gen cond_bb
                ctx.set_cur_bb(cond_bb);
                let cond_val = cond.generate(ctx)?;
                ctx.emit_cond_br(cond_val, body_bb, end_bb);

                // gen body_bb
                ctx.set_cur_bb(body_bb);
                body.generate(ctx)?;
                ctx.emit_br_if_needed(cond_bb);

                // set end_bb as cur_bb
                ctx.set_cur_bb(end_bb);

                ctx.loop_stack.pop();
            }
            Stmt::Break => {
                let (_, end_bb) = ctx.loop_stack.last().unwrap();
                ctx.emit_jump(*end_bb);
            }
            Stmt::Continue => {
                let (cond_bb, _) = ctx.loop_stack.last().unwrap();
                ctx.emit_jump(*cond_bb);
            }
        }
        Ok(())
    }
}

impl IRGen for Decl {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        let is_global = ctx.is_global();
        if is_global {
            match self {
                Decl::Const(const_decl) => {
                    for def in const_decl.const_defs.iter() {
                        def.generate(ctx)?;
                    }
                }
                Decl::Var(var_decl) => {
                    for def in var_decl.defs.iter() {
                        let global_ident = format!("@{}", def.ident.clone());

                        let dims = eval_array_dims(&def.dims, ctx)?;
                        let (_ty, init) = if dims.is_empty() {
                            let mut init = ctx.program.new_value().zero_init(Type::get_i32());
                            if let Some(ref init_val) = def.init_val {
                                let InitVal::Exp(exp) = init_val else {
                                    return Err(Error::InvalidInit);
                                };
                                let init_val = consteval_exp(exp, ctx)?;
                                init = ctx.program.new_value().integer(init_val);
                            }
                            (Type::get_i32(), init)
                        } else {
                            let ty = build_array_type(&dims);
                            let flat = flatten_init(&dims, def.init_val.as_ref())?;
                            let mut vals = Vec::with_capacity(flat.len());
                            for e in flat {
                                let v = match e {
                                    Some(exp) => consteval_exp(exp, ctx)?,
                                    None => 0,
                                };
                                vals.push(v);
                            }
                            let init = build_const_aggregate(ctx, ty.clone(), &vals);
                            (ty, init)
                        };

                        let galloc = ctx.emit_global_alloc(init);
                        ctx.program
                            .set_value_name(galloc, Some(global_ident.clone()));

                        ctx.symbol_table
                            .define(def.ident.clone(), Symbol::Var(galloc))?;
                    }
                }
            }
        } else {
            match self {
                Decl::Const(const_decl) => {
                    for def in const_decl.const_defs.iter() {
                        def.generate(ctx)?;
                    }
                }
                Decl::Var(var_decl) => {
                    for def in var_decl.defs.iter() {
                        def.generate(ctx)?;
                    }
                }
            }
        }

        Ok(())
    }
}

impl IRGen for ConstDef {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        let dims = eval_array_dims(&self.dims, ctx)?;
        if dims.is_empty() {
            let result = self.init_val.generate(ctx)?;
            ctx.symbol_table
                .define(self.ident.clone(), Symbol::Const(result))?;
            return Ok(());
        }

        let ty = build_array_type(&dims);
        let flat = flatten_const_init(&dims, &self.init_val)?;
        let mut vals = Vec::with_capacity(flat.len());
        for e in flat {
            let v = match e {
                Some(ce) => consteval(ce, ctx)?,
                None => 0,
            };
            vals.push(v);
        }

        let var = if ctx.is_global() {
            let init = build_const_aggregate(ctx, ty.clone(), &vals);
            let galloc = ctx.emit_global_alloc(init);
            ctx.program
                .set_value_name(galloc, Some(format!("@{}", self.ident.clone())));
            galloc
        } else {
            let alloc = ctx.emit_alloc(ty.clone());
            let vir_reg_name = format!("@{}_{}", self.ident, ctx.fetch_counter());
            ctx.set_value_name(alloc, vir_reg_name);
            // 逐元素初始化
            for (i, v) in vals.iter().enumerate() {
                let indices = flattenindex_to_indices(&dims, i);
                let mut idx_vals = Vec::with_capacity(indices.len());
                for idx in indices {
                    idx_vals.push(ctx.emit_integer(idx as i32));
                }
                let elem_ptr = emit_elem_ptr_by_indices(ctx, alloc, &idx_vals);
                let iv = ctx.emit_integer(*v);
                ctx.emit_store(iv, elem_ptr);
            }
            alloc
        };

        // FIXME: const array 似乎不需要注册到 symbol table
        ctx.symbol_table
            .define(self.ident.clone(), Symbol::Var(var))?;
        Ok(())
    }
}

impl IRGen for ConstInitVal {
    type Output = i32;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            ConstInitVal::ConstExp(const_exp) => Ok(consteval(const_exp, ctx)?),
            ConstInitVal::ConstInitList(_) => Err(Error::InvalidInit),
        }
    }
}

impl IRGen for VarDef {
    type Output = ();

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        let dims = eval_array_dims(&self.dims, ctx)?;
        let ty = if dims.is_empty() {
            Type::get_i32()
        } else {
            build_array_type(&dims)
        };

        let var_alloc = ctx.emit_alloc(ty);
        let vir_reg_name = format!("@{}_{}", self.ident, ctx.fetch_counter());
        ctx.set_value_name(var_alloc, vir_reg_name);

        if dims.is_empty() {
            if let Some(ref init_val) = self.init_val {
                let InitVal::Exp(exp) = init_val else {
                    return Err(Error::InvalidInit);
                };
                let init_value = exp.generate(ctx)?;
                ctx.emit_store(init_value, var_alloc);
            }
        } else if self.init_val.is_some() {
            let flat = flatten_init(&dims, self.init_val.as_ref())?;
            let zero = ctx.emit_integer(0);
            for (i, e) in flat.into_iter().enumerate() {
                let indices = flattenindex_to_indices(&dims, i);
                let mut idx_vals = Vec::with_capacity(indices.len());
                for idx in indices {
                    idx_vals.push(ctx.emit_integer(idx as i32));
                }
                let elem_ptr = emit_elem_ptr_by_indices(ctx, var_alloc, &idx_vals);
                let v = match e {
                    Some(exp) => exp.generate(ctx)?,
                    None => zero,
                };
                ctx.emit_store(v, elem_ptr);
            }
        }

        ctx.symbol_table
            .define(self.ident.clone(), Symbol::Var(var_alloc))?;

        Ok(())
    }
}

impl IRGen for Exp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            Exp::LOr(lor_exp) => lor_exp.generate(ctx),
        }
    }
}

impl IRGen for LOrExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            LOrExp::LAnd(l_and_exp) => l_and_exp.generate(ctx),
            LOrExp::LOrLAnd(l, r) => {
                let counter = ctx.fetch_counter();

                let lor_rhs = ctx.create_bb(Some(&format!("%lor_rhs_{}", counter)));
                let lor_end = ctx.create_bb(Some(&format!("%lor_end_{}", counter)));

                let result = ctx.emit_alloc(Type::get_i32());

                let zero = ctx.emit_integer(0);

                // l || r => (l != 0) | (r != 0)
                // gen lhs
                let lhs = l.generate(ctx)?;
                let lnez = ctx.emit_binary(BinaryOp::NotEq, lhs, zero);
                ctx.emit_store(lnez, result);
                ctx.emit_cond_br(lnez, lor_end, lor_rhs);

                // gen rhs
                ctx.set_cur_bb(lor_rhs);
                let rhs = r.generate(ctx)?;
                let rnez = ctx.emit_binary(BinaryOp::NotEq, rhs, zero);
                ctx.emit_store(rnez, result);
                ctx.emit_jump(lor_end);

                // gen end
                ctx.set_cur_bb(lor_end);
                let result = ctx.emit_load(result);

                Ok(result)
            }
        }
    }
}

impl IRGen for LAndExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            LAndExp::Eq(eq_exp) => eq_exp.generate(ctx),
            LAndExp::LAndEq(l, r) => {
                let counter = ctx.fetch_counter();

                let land_rhs = ctx.create_bb(Some(&format!("%land_rhs_{}", counter)));
                let land_end = ctx.create_bb(Some(&format!("%land_end_{}", counter)));

                let result = ctx.emit_alloc(Type::get_i32());

                let zero = ctx.emit_integer(0);

                // l && r => (l != 0) & (r != 0)
                // gen lhs
                let lhs = l.generate(ctx)?;
                let lnez = ctx.emit_binary(BinaryOp::NotEq, lhs, zero);
                ctx.emit_store(lnez, result);
                ctx.emit_cond_br(lnez, land_rhs, land_end);

                // gen rhs
                ctx.set_cur_bb(land_rhs);
                let rhs = r.generate(ctx)?;
                let rnez = ctx.emit_binary(BinaryOp::NotEq, rhs, zero);
                ctx.emit_store(rnez, result);
                ctx.emit_jump(land_end);

                // gen end
                ctx.set_cur_bb(land_end);
                let result = ctx.emit_load(result);

                Ok(result)
            }
        }
    }
}

impl IRGen for EqExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            EqExp::Rel(rel_exp) => rel_exp.generate(ctx),
            EqExp::EqRel(l, eq_op, r) => {
                let l_val = l.generate(ctx)?;
                let r_val = r.generate(ctx)?;
                Ok(ctx.emit_binary((*eq_op).into(), l_val, r_val))
            }
        }
    }
}

impl IRGen for RelExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            RelExp::Add(add_exp) => add_exp.generate(ctx),
            RelExp::RelAdd(l, rel_op, r) => {
                let l_val = l.generate(ctx)?;
                let r_val = r.generate(ctx)?;
                Ok(ctx.emit_binary((*rel_op).into(), l_val, r_val))
            }
        }
    }
}

impl IRGen for AddExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            AddExp::Mul(mul_exp) => mul_exp.generate(ctx),
            AddExp::AddMul(l, add_op, r) => {
                let l_val = l.generate(ctx)?;
                let r_val = r.generate(ctx)?;
                Ok(ctx.emit_binary((*add_op).into(), l_val, r_val))
            }
        }
    }
}

impl IRGen for MulExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            MulExp::Unary(unary_exp) => unary_exp.generate(ctx),
            MulExp::MulUnary(l, mul_op, r) => {
                let l_val = l.generate(ctx)?;
                let r_val = r.generate(ctx)?;
                Ok(ctx.emit_binary((*mul_op).into(), l_val, r_val))
            }
        }
    }
}

impl IRGen for UnaryExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.generate(ctx),
            UnaryExp::Unary(op, exp) => {
                let exp_val = exp.generate(ctx)?;
                let zero = ctx.emit_integer(0);

                let result = match op {
                    UnaryOp::Plus => exp_val,
                    UnaryOp::Minus => ctx.emit_binary(BinaryOp::Sub, zero, exp_val),
                    UnaryOp::Not => ctx.emit_binary(BinaryOp::Eq, exp_val, zero),
                };

                Ok(result)
            }
            UnaryExp::FuncCall(fname, arg_exps) => {
                let function = ctx.funcs.get(fname).cloned().ok_or(Error::SymbolNotFound)?;

                let mut args = vec![];
                if let Some(arg_exps) = arg_exps {
                    for arg_exp in arg_exps.args.iter() {
                        args.push(arg_exp.generate(ctx)?);
                    }
                }

                Ok(ctx.emit_call(function, args))
            }
        }
    }
}

impl IRGen for PrimaryExp {
    type Output = Value;

    fn generate(&self, ctx: &mut Ctx) -> IRResult<Self::Output> {
        match self {
            PrimaryExp::Number(num) => Ok(ctx.emit_integer(*num)),
            PrimaryExp::Paren(exp) => exp.generate(ctx),
            PrimaryExp::LVal(lval) => {
                let LVal::GLVal(ident, dims) = lval;
                let sym = *ctx
                    .symbol_table
                    .resolve(ident)
                    .ok_or(Error::SymbolNotFound)?;

                match sym {
                    Symbol::Const(c) => Ok(ctx.emit_integer(c)),
                    Symbol::Var(var) => {
                        if dims.is_empty() {
                            match ctx.value_type(var).kind() {
                                TypeKind::Pointer(pointee) => match pointee.kind() {
                                    TypeKind::Array(_, _) => {
                                        // Suppose int a[3][4];
                                        // `a` 应该返回 `&a[0]`
                                        let zero = ctx.emit_integer(0);
                                        Ok(ctx.emit_get_elem_ptr(var, zero))
                                    }
                                    _ => Ok(ctx.emit_load(var)),
                                },
                                _ => Ok(ctx.emit_load(var)),
                            }
                        } else {
                            let addr = lval_address(ctx, lval)?;
                            match ctx.value_type(addr).kind() {
                                TypeKind::Pointer(pointee) => match pointee.kind() {
                                    TypeKind::Array(_, _) => {
                                        // Suppose int a[3][4];
                                        // `a[1]` should return `&a[1][0]`
                                        let zero = ctx.emit_integer(0);
                                        Ok(ctx.emit_get_elem_ptr(addr, zero))
                                    }
                                    _ => Ok(ctx.emit_load(addr)),
                                },
                                _ => Ok(ctx.emit_load(addr)),
                            }
                        }
                    }
                }
            }
        }
    }
}
