
---
References:
[Koopa IR 规范](https://pku-minic.github.io/online-doc/#/misc-app-ref/koopa)
[Crate koopa - DOCS.RS](https://docs.rs/koopa/latest/koopa/index.html)

---
Koopa IR 的设计理念
层次化结构：
- *Program* → *Function* → *DFG*/*Layout* 分离
- 明确的所有权关系：必须先将函数添加到程序，才能创建值
- 数据流和控制流分离：*DFG* 管理值/指令，*Layout* 管理基本块

教育目的：
- 强制理解 IR 结构：让你明确知道每个操作属于哪个层级
- 类型安全：整数常量需要函数上下文才能推断类型
- 防止悬空引用：严格的层次结构防止无效引用

```
Program
├── 全局变量列表
└── 函数列表
    └── Function
        ├── DFG (Data Flow Graph) - 数据流
        │   ├── 常量: integer(0), zero_init()
        │   └── 指令: load, store, binary, ret, call
        └── Layout - 控制流
            ├── 基本块列表: %entry, %cond_true
            └── 指令序列: 每个基本块内的指令顺序
```

顺序要求:
1. 先创建函数结构 → FunctionData::new()
2. 再添加到程序 → program.new_func(func_def)
3. 然后组织控制流 → layout_mut().bbs_mut().push_key_back()
4. 最后创建值/指令 → dfg_mut().new_value().xxx()

类型推断
- 整数常量必须在函数添加到程序后创建
- ret() 指令的返回值类型必须匹配函数返回类型
- 类型不匹配会触发 panic

---
在函数添加到程序后创建值: 因为类型推断需要函数上下文

---
```
Program
  全局变量列表:
    Value 1.
    Value 2.
    ...
  函数列表:
    Function 1.
      基本块列表:
        BasicBlock 1.
          指令列表:
            Value 1.
            Value 2.
        BasicBlock 2.
        ...
    Function 2.
    ...
```

`Value` 的种类:
- **各类常量**: 整数常量 (Integer), 零初始化器 (ZeroInit), 等等.
- **参数引用**: 函数参数引用 (FuncArgRef) 等, 用来指代传入的参数.
- **内存分配**: 全局内存分配 (GlobalAlloc, 所有的全局变量都是这个玩意) 和局部内存分配 (Alloc).
- **访存指令**: 加载 (Load) 和存储 (Store).
- **指针运算**: GetPtr 和 GetElemPtr.
- **二元运算**: Binary, 比如加减乘除模/比较之类的运算都属于此类.
- **控制转移**: 条件分支 (Branch) 和无条件跳转 (Jump).
- **函数相关**: 函数调用 (Call) 和函数返回 (Return).


Koopa IR 里的规定, **Function**, **BasicBlock**, **Value** 的名字必须以 **@** 或者 **%** 开头. 前者表示这是一个 "具名符号", 后者表示这是一个 "临时符号".
> 这两者其实没有任何区别, 但我们通常用前者表示 SysY 里出现的符号, 用后者表示你的编译器在生成 IR 的时候生成的符号.


