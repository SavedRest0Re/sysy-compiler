
---
References:
[北大编译实践在线文档](https://pku-minic.github.io/online-doc/#/)
[Koopa IR 规范](https://pku-minic.github.io/online-doc/#/misc-app-ref/koopa)
[Crate koopa - DOCS.RS](https://docs.rs/koopa/latest/koopa/index.html)

---
Koopa IR 的设计理念
层次化结构：
- *Program* → *Function* → *DFG*/*Layout* 分离
- 明确的所有权关系：必须先将函数添加到程序，才能创建值
- 数据流和控制流分离：*DFG* 管理值/指令，*Layout* 管理基本块

DataFlowGraph 管理函数内的所有值和基本块：
- 创建和删除值
- 创建和删除基本块
- 维护 Use-Def 链
- 提供值的查询接口

Layout 管理基本块和指令的顺序：
- 维护基本块的执行顺序
- 维护每个基本块内指令的顺序
- 支持高效的插入和删除

教育目的：
- 强制理解 IR 结构：让你明确知道每个操作属于哪个层级
- 类型安全：整数常量需要函数上下文才能推断类型
- 防止悬空引用：严格的层次结构防止无效引用

```
Program
    ├── Global Values (全局值)
    ├── Functions (函数)
    │       ├── DataFlowGraph (数据流图)
    │       │       ├── Values (值)
    │       │       └── BasicBlocks (基本块)
    │       └── Layout (布局)
    │               ├── BB List (基本块列表)
    │               └── Inst List (指令列表)
    └── Types (类型系统)
```

```
Value (值)
├── Constants (常量)
│   ├── Integer      - 整数常量
│   ├── ZeroInit     - 零初始化
│   ├── Undef        - 未定义值
│   └── Aggregate    - 聚合常量
│
├── References (引用)
│   ├── FuncArgRef   - 函数参数引用
│   └── BlockArgRef  - 基本块参数引用
│
└── Instructions (指令)
    ├── Memory
    │   ├── Alloc         - 局部内存分配
    │   ├── GlobalAlloc   - 全局内存分配
    │   ├── Load          - 内存读取
    │   └── Store         - 内存写入
    │
    ├── Pointer
    │   ├── GetPtr        - 指针偏移
    │   └── GetElemPtr    - 元素指针
    │
    ├── Arithmetic
    │   └── Binary        - 二元运算
    │
    └── Control Flow
        ├── Branch        - 条件分支
        ├── Jump          - 无条件跳转
        ├── Call          - 函数调用
        └── Return        - 函数返回
```

`Program`: 程序的顶层结构
`FunctionData`: 函数的完整定义
`BasicBlockData`: 基本块的数据
`ValueData`: 值的元数据

```
Builder Traits
├── EntityInfoQuerier    - 查询实体信息
├── ValueInserter        - 插入值
├── ValueBuilder         - 构建值（常量）
├── GlobalInstBuilder    - 构建全局指令
├── LocalInstBuilder     - 构建局部指令
└── BasicBlockBuilder    - 构建基本块
```

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
对于基本块的指令列表: 指令的数据并没有直接按照指令出现的顺序存储在列表中. 
指令的数据被统一存放在函数内的一个叫做 DataFlowGraph 的结构中, 同时每个指令具有一个指令 ID (或者也可以叫 handle), 你可以通过 ID 在这个结构中获取对应的指令. 指令的列表中存放的其实是指令的 ID.

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


