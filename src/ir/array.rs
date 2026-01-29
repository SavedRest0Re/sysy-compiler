use koopa::ir::{Type, TypeKind, Value, builder::ValueBuilder};

use crate::ast::{ConstExp, ConstInitVal, Exp, FuncFParam, InitVal, LVal};

use super::{Error, IRGen, IRResult};

use super::{
    const_eval::consteval,
    ctx::{Ctx, Symbol},
};

pub(crate) fn eval_array_dims(dims: &[ConstExp], ctx: &Ctx) -> IRResult<Vec<usize>> {
    let mut lens = Vec::with_capacity(dims.len());
    for d in dims {
        let v = consteval(d, ctx)?;
        if v <= 0 {
            return Err(Error::InvalidArrayLen);
        }
        lens.push(v as usize);
    }
    Ok(lens)
}

fn array_total_len(dims: &[usize]) -> usize {
    dims.iter().product()
}

pub(crate) fn build_array_type(dims: &[usize]) -> Type {
    let mut ty = Type::get_i32();
    for &len in dims.iter().rev() {
        ty = Type::get_array(ty, len);
    }
    ty
}

pub(crate) fn build_param_ty(param: &FuncFParam, ctx: &Ctx) -> IRResult<Type> {
    if param.dims.is_empty() {
        return Ok(Type::get_i32());
    }

    let mut ty = Type::get_i32();
    for d in param.dims.iter().skip(1).rev() {
        let Some(d) = d else {
            return Err(Error::InvalidArrayLen);
        };
        let v = consteval(d, ctx)?;
        if v <= 0 {
            return Err(Error::InvalidArrayLen);
        }
        ty = Type::get_array(ty, v as usize);
    }
    Ok(Type::get_pointer(ty))
}

fn fill_init_list<'a>(
    dims: &[usize],
    elems: &'a [InitVal],
    out: &mut [Option<&'a Exp>],
    pos: &mut usize,
) -> IRResult<()> {
    for elem in elems {
        if *pos >= out.len() {
            break;
        }

        match elem {
            InitVal::Exp(e) => {
                out[*pos] = Some(e);
                *pos += 1;
            }
            InitVal::InitList(sub) => {
                if dims.len() == 1 {
                    fill_init_list(dims, sub, out, pos)?;
                    continue;
                }

                let last = *dims.last().unwrap();
                if *pos % last != 0 {
                    return Err(Error::InvalidInit);
                }

                let total = out.len();
                let n = dims.len();
                let mut prod = 1usize;
                let mut chosen_k = 1usize;
                for k in 1..=n - 1 {
                    prod *= dims[n - k];
                    if prod < total && *pos % prod == 0 {
                        chosen_k = k;
                    }
                }
                let sub_dims = &dims[n - chosen_k..];
                let sub_len = array_total_len(sub_dims);
                if *pos + sub_len > out.len() {
                    break;
                }
                let mut sub_pos = 0usize;
                fill_init_list(sub_dims, sub, &mut out[*pos..*pos + sub_len], &mut sub_pos)?;
                *pos += sub_len;
            }
        }
    }
    Ok(())
}

pub(crate) fn flatten_init<'a>(
    dims: &[usize],
    init: Option<&'a InitVal>,
) -> IRResult<Vec<Option<&'a Exp>>> {
    let total = array_total_len(dims);
    let mut out = vec![None; total];
    let Some(init) = init else {
        return Ok(out);
    };
    match init {
        InitVal::Exp(e) => {
            if !out.is_empty() {
                out[0] = Some(e);
            }
        }
        InitVal::InitList(elems) => {
            let mut pos = 0usize;
            fill_init_list(dims, elems, &mut out, &mut pos)?;
        }
    }
    Ok(out)
}

fn fill_const_init_list<'a>(
    dims: &[usize],
    elems: &'a [ConstInitVal],
    out: &mut [Option<&'a ConstExp>],
    pos: &mut usize,
) -> IRResult<()> {
    for elem in elems {
        if *pos >= out.len() {
            break;
        }
        match elem {
            ConstInitVal::ConstExp(e) => {
                out[*pos] = Some(e);
                *pos += 1;
            }
            ConstInitVal::ConstInitList(sub) => {
                // 一维数组特判：不做对齐约束，继续顺序填
                // int a[5] = {{1,2}}; 这种在一维里就当作普通顺序初始化处理。
                if dims.len() == 1 {
                    fill_const_init_list(dims, sub, out, pos)?;
                    continue;
                }

                let last = *dims.last().unwrap();
                // 对齐检测
                if *pos % last != 0 {
                    return Err(Error::InvalidInit);
                }

                let total = out.len();
                let n = dims.len();
                let mut prod = 1usize;
                let mut chosen_k = 1usize;

                for k in 1..=n - 1 {
                    prod *= dims[n - k];
                    // 条件 *pos % prod == 0：表示当前位置 pos 正好落在该块大小的边界上
                    // 选“最大的”满足条件的 k：意味着 尽可能把这个子列表当成更高层的子数组来填充
                    if prod < total && *pos % prod == 0 {
                        chosen_k = k;
                    }
                }
                let sub_dims = &dims[n - chosen_k..];
                let sub_len = array_total_len(sub_dims);
                if *pos + sub_len > out.len() {
                    break;
                }
                let mut sub_pos = 0usize;

                // 递归填充
                fill_const_init_list(sub_dims, sub, &mut out[*pos..*pos + sub_len], &mut sub_pos)?;

                *pos += sub_len;
            }
        }
    }
    Ok(())
}

pub(crate) fn flatten_const_init<'a>(
    dims: &[usize],
    init: &'a ConstInitVal,
) -> IRResult<Vec<Option<&'a ConstExp>>> {
    let total = array_total_len(dims);
    let mut out = vec![None; total];
    match init {
        // int a[3] = 5;
        ConstInitVal::ConstExp(e) => {
            if !out.is_empty() {
                out[0] = Some(e);
            }
        }
        // int a[3] = {5};
        ConstInitVal::ConstInitList(elems) => {
            let mut pos = 0usize;
            fill_const_init_list(dims, elems, &mut out, &mut pos)?;
        }
    }
    Ok(out)
}

pub(crate) fn build_const_aggregate(ctx: &mut Ctx, ty: Type, flat: &[i32]) -> Value {
    if flat.iter().all(|&x| x == 0) {
        return ctx.program.new_value().zero_init(ty);
    }
    match ty.kind() {
        TypeKind::Int32 => ctx.program.new_value().integer(flat[0]),
        TypeKind::Array(base_ty, len) => {
            let base_ty = base_ty.clone();
            let sub_size = flat.len() / *len;
            let mut elems = Vec::with_capacity(*len);
            for i in 0..*len {
                let sub = &flat[i * sub_size..(i + 1) * sub_size];
                elems.push(build_const_aggregate(ctx, base_ty.clone(), sub));
            }
            ctx.program.new_value().aggregate(elems)
        }
        _ => unreachable!(),
    }
}

pub(crate) fn emit_elem_ptr_by_indices(ctx: &mut Ctx, mut base: Value, indices: &[Value]) -> Value {
    for idx in indices {
        base = ctx.emit_get_elem_ptr(base, *idx);
    }
    base
}

pub(crate) fn flattenindex_to_indices(dims: &[usize], mut idx: usize) -> Vec<usize> {
    let mut out = Vec::with_capacity(dims.len());
    for (i, &len) in dims.iter().enumerate() {
        let stride: usize = dims[i + 1..].iter().product();
        let cur = if stride == 0 { 0 } else { idx / stride };
        out.push(cur);
        if stride != 0 {
            idx %= stride;
        }
        let _ = len;
    }
    out
}

pub(crate) fn lval_address(ctx: &mut Ctx, lval: &LVal) -> IRResult<Value> {
    let LVal::GLVal(ident, dims) = lval;
    let sym = *ctx
        .symbol_table
        .resolve(ident)
        .ok_or(Error::SymbolNotFound)?;

    let Symbol::Var(var) = sym else {
        return Err(Error::FailedToEval);
    };

    if dims.is_empty() {
        return Ok(var);
    }

    // Q: 两重 Pointer
    // A: 在本编译器里，函数形参（包括数组形参）会先被 `alloc` 到一个局部槽位里保存
    // ty = build_param_ty(param, ctx)?
    // var_alloc = alloc ty
    // store %param_i, var_alloc
    // 符号表里保存的是 var_alloc

    // 对于普通 int x：
    // `ty = i32`
    // `alloc i32` 的结果类型是 `*i32`
    // 所以 var 的类型是 `*i32`（一重指针）

    // 对于数组形参 int a[][10]：
    // `build_param_ty` 返回的是 `*[i32, 10]`（注意：形参退化为指针）
    // `alloc *[i32, 10]` 的结果类型是 `**[i32, 10]`
    // 所以符号表里的 var 类型是 `**[i32, 10]`（两重指针）
    let mut base = var;
    let mut use_getptr_first = false;
    match ctx.value_type(var).kind() {
        TypeKind::Pointer(pointee) => match pointee.kind() {
            TypeKind::Pointer(_) => {
                // 数组形参的情况, 第一维需要 emit getptr
                base = ctx.emit_load(var);
                use_getptr_first = true;
            }
            _ => {}
        },
        _ => {}
    }

    for (i, d) in dims.iter().enumerate() {
        let idx = d.generate(ctx)?;
        if i == 0 && use_getptr_first {
            base = ctx.emit_get_ptr(base, idx);
        } else {
            base = ctx.emit_get_elem_ptr(base, idx);
        }
    }
    Ok(base)
}
