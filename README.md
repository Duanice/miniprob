# Mini Programming Language Interpreter

这是一个简单的编程语言解释器，支持基本的算术运算、条件语句、循环、概率抽样等功能。

## 语法特性

- **数据类型**: 整数(int)、布尔值(bool)
- **运算**: 加法(+)、减法(-)、比较(<, ==)
- **概率**: flip(p) - 以概率p返回true
- **控制流**: if-then-else, while-do
- **观测**: observe(expr) - 打印表达式的值

## 使用方法

### 编译
```bash
dune build
```

### 打印AST (调试用)
```bash
cat your_program.ppl | ./_build/default/main.exe
```

### 运行解释器
```bash
cat your_program.ppl | ./_build/default/main.exe --run
```

## 示例程序

查看 `example.ppl`：
```
x := 0;
while x < 5 do
  x := x + 1;
  observe x;
if flip(0.9) then
  observe 100;
else
  observe 200;
```

运行结果：
```
=== Running Interpreter ===
Observed: 1
Observed: 2
Observed: 3
Observed: 4
Observed: 5
Observed: 100

Final Environment:
  x = 5
```


## 文件结构

- `ast.ml` - AST定义和Pretty Printer
- `interpreter.ml` - 解释器实现（值系统、环境管理、求值函数）
- `parser.mly` - 语法解析器
- `lexer.mll` - 词法分析器
- `main.ml` - 主程序入口

## 扩展建议

- 添加更多数据类型（字符串、浮点数等）
- 添加函数定义和调用
- 添加数组或列表类型
- 实现静态类型检查
- 添加更多内置函数
