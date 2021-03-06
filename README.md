# 计算机系统要素 学习仓库

[官方网站](https://www.nand2tetris.org)

> projects 和 tools 下载自官方网站。很不幸的是，这里的示例代码和程序依赖旧版本的 Java 运行时（8），所以需要在 [Oracle Java 下载](https://www.oracle.com/java/technologies/javase/javase8-archive-downloads.html) 这里先下载 JRE，然后像这样执行程序，比如将 JRE 解压到 ~/jre8 目录下，在 project/0x 目录下运行硬件模拟器：`PATH=~/jre8/bin/:$PATH ../../tools/HardwareSimulator.sh`。

## Chapter01：布尔逻辑

See `projects/01`

## Chapter02：布尔运算

See `projects/02`

## Chapter03：时序逻辑

See `projects/03`

## Chapter04：机器语言

See `projects/04`

## Chapter05：计算机体系结构

See `projects/05`

## Chapter06：汇编编译器

See `projects/06`，使用 Clojure 实现的汇编编译器，代码参见 `impl/src/assembler.clj`。

## Chapter07：虚拟机 IR 后端（算术逻辑运算和内存访问）

See `projects/07`，使用 Clojure 实现的 IR 后端翻译器（部分），代码参见 `impl/src/vm_trans.clj`。

## Chapter08：虚拟机 IR 后端（控制和子过程切换）

See `projects/08`，使用 Clojure 实现的 IR 后端翻译器（部分），代码参见 `impl/src/vm_trans.clj`。

## Chapter09: 高级语言

See `projects/09`，注意 VM 模拟器关闭动画提高运行速度。

## Chapter10: 虚拟机 IR 前端（语法分析）

See `projects/10`，使用 Clojure 实现的 IR 前端编译器（部分），代码参见 `impl/src/analyzer.clj`。

## Chapter11: 虚拟机 IR 前端（代码生成）

See `projects/11`，使用 Clojure 实现的 IR 前端编译器，代码参见 `impl/src/compiler_st.clj`（中间符号表）和 `impl/src/compiler.clj`。

## Chapter12: 操作系统

See `projects/12`，在每个 XXXTest 文件夹下的 XXX.jack 为目标库，需要通过 `./tools/JackCompiler.bat ./projects/12/XXXTest` 编译后，打开 `./tools/VMEmulator.bat` 将文件夹整体加载到 VM 仿真器并运行测试。