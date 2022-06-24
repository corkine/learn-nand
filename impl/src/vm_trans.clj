(ns vm-trans
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.nio.file Paths)))

(defonce index (atom 0))

(defn next-int []
  (reset! index (+ @index 1))
  @index)

(defn read-vm
  "从文件中读入 .vm 程序，返回 [cmd]"
  [filename]
  (mapv #(str/replace % "\r" "")
        (str/split (slurp filename) #"\n")))

(defn save-to
  "将生成的 jack 二进制写入到文件中"
  [filename output]
  (with-open [w (io/writer filename)]
    (.write w (str/join "\n" output))))

(defn cmd-type
  "返回当前命令的类型，如果不是命令，返回 :ERROR"
  [cmd]
  (let [cmd (-> cmd (str/trim) (str/lower-case))]
    ;(println "Handling " cmd)
    ;算术命令，包括 add,sub,neg,eq,gt,lt,and,or,not
    (cond (contains? #{"add" "sub" "neg" "eq" "gt" "lt"
                       "and" "or" "not"} cmd) :C-ARITHMETIC
          ;压入堆栈命令，包括 push ?? ??
          (str/includes? cmd "push") :C-PUSH
          ;弹出堆栈命令，包括 pop ?? ??
          (str/includes? cmd "pop") :C-POP
          ;标签命令，包括 label !!
          (str/includes? cmd "label") :C-LABEL
          ;判断命令，包括 if-goto !!
          (str/includes? cmd "if-goto") :C-IF
          ;跳转命令，包括 goto !!
          (str/includes? cmd "goto") :C-GOTO
          ;函数定义，包括 function !! !!
          (str/includes? cmd "function") :C-FUNCTION
          ;返回命令，格式 return
          (str/includes? cmd "return") :C-RETURN
          ;调用命令，格式 call !! !!
          (str/includes? cmd "call") :C-CALL
          :else :ERROR)))

(defn arg1
  "返回命令第一个参数，如果没有参数返回 nil"
  [cmd]
  (let [arr (-> cmd (str/trim) (str/split #" "))]
    (if-let [arg (->> arr (filterv (comp not str/blank?)) second)]
      (str/trim arg))))

(defn arg2
  "返回命令第二个参数，如果没有参数返回 nil"
  [cmd]
  (let [arr (-> cmd (str/trim) (str/split #" "))
        arr (->> arr (filterv (comp not str/blank?)))]
    (if-let [arg (get arr 2)]
      (str/trim arg))))

(defn trans-arithmetic
  "将算数操作转换为汇编代码
  add,sub,neg,eq,gt,lt,and,or,not"
  [cmd]
  (let [cmd (-> cmd (str/trim) (str/lower-case))]
    (cond (= cmd "add")
          ["@SP" "M=M-1" "A=M" "D=M" "A=A-1" "M=D+M"]
          (= cmd "sub")
          ["@SP" "M=M-1" "A=M" "D=M" "A=A-1" "M=M-D"]
          (= cmd "and")
          ["@SP" "M=M-1" "A=M" "D=M" "A=A-1" "M=D&M"]
          (= cmd "or")
          ["@SP" "M=M-1" "A=M" "D=M" "A=A-1" "M=D|M"]
          (= cmd "neg")
          ["@SP" "A=M" "A=A-1" "M=-M"]
          (= cmd "not")
          ["@SP" "A=M" "A=A-1" "M=!M"]
          (contains? #{"eq" "gt" "lt"} cmd)
          (let [index (next-int)
                pass (str "pass" index)
                not-pass (str "notPass" index)
                end (str "end" index)]
            ["@SP" "M=M-1" "A=M" "D=M"                      ;出栈 y，栈想下移动一格
             "A=A-1" "M=M-D" "D=M"                          ;计算 x - y 保存到 x 处
             (str "@" pass)
             (cond (= cmd "eq") "D;JEQ"
                   (= cmd "gt") "D;JGT"
                   (= cmd "lt") "D;JLT")
             (str "@" not-pass)
             "0;JMP"
             (str "(" pass ")")
             "@SP" "A=M" "A=A-1" "M=-1"                     ;将栈第一个元素(计算结果) 更新
             (str "@" end)
             "0;JMP"
             (str "(" not-pass ")")
             "@SP" "A=M" "A=A-1" "M=0"
             (str "(" end ")")]))))

(defn trans-push-pop
  "将内存操作转换为汇编代码"
  [file-name cmd segment index]
  (let [reg (case segment "local" "@LCL" "argument" "@ARG"
                          "this" "@THIS" "that" "@THAT"
                          "should_not_be_here")
        edit-val-type #{"local" "argument" "this" "that"}
        edit-addr-type #{"pointer" "temp"}]
    (cond (and (= cmd "push") (= segment "constant"))
          [(str "@" index) "D=A" "@SP" "A=M" "M=D" "@SP" "M=M+1"]
          (and (= cmd "push") (contains? edit-val-type segment))
          [(str "@" index) "D=A" reg "A=M" "A=D+A" "D=M"
           "@SP" "A=M" "M=D" "@SP" "M=M+1"]
          (and (= cmd "pop") (contains? edit-val-type segment))
          ["@SP" "M=M-1" (str "@" index) "D=A" reg "A=M" "D=D+A" "@R13" "M=D"
           "@SP" "A=M" "D=M" "@R13" "A=M" "M=D"]
          (and (= cmd "push") (contains? edit-addr-type segment))
          [(str "@" index) "D=A"
           (if (= segment "pointer") "@THIS" "@R5") "A=D+A" "D=M"
           "@SP" "A=M" "M=D" "@SP" "M=M+1"]
          (and (= cmd "pop") (contains? edit-addr-type segment))
          [(str "@" index) "D=A"
           (if (= segment "pointer") "@THIS" "@R5") "A=D+A" "D=A"
           "@R13" "M=D" "@SP" "M=M-1" "A=M" "D=M" "@R13" "A=M" "M=D"]
          (and (= cmd "push") (= "static" segment))
          [(str "@" file-name "." index) "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"]
          (and (= cmd "pop") (= "static" segment))
          ["@SP" "M=M-1" "A=M" "D=M" (str "@" file-name "." index) "M=D"]
          :else (throw (RuntimeException. "未实现此转换")))))

;;TODO label 和 goto label 应该使用 functionName$label 的形式
;;TODO goto label 必须位于同一程序内，if-goto 必须位于同一函数内
(defn trans-label
  "翻译 label 命令"
  [label] [(str "(" label ")")])

(defn trans-goto
  "翻译 goto 命令"
  [label]
  [(str "@" label) "0;JMP"])

(defn trans-if
  "翻译条件跳转命令"
  [label] ["@SP" "M=M-1" "A=M" "D=M" (str "@" label) "D;JNE"])

(defn trans-call
  "翻译函数调用命令
  |arg0    | <- 压入被调用者需要的参数       -> |ret value|
  |arg1    | <- 压入被调用者需要的参数       -> |SP HERE  |
  |ret addr| <- 压入函数返回后的下一条指令位置
  |LCL     | <- 保存自身 LCL
  |ARG     | <- 保存自身 ARG
  |THIS    | <- 保存自身 THIS
  |THAT    | <- 保存自身 THAT
  |||| <- SP
  重设被调用者 ARG 为 SP - 5
  重设被调用者 LCL 为 SP
  跳转到函数入口并开始执行代码"
  [func-name ^Integer numArgs]
  (let [point (str "return" (next-int))]
    (flatten
      [;将返回值保存在堆栈中，并增长堆栈
       (str "@" point) "D=A" "@SP" "M=D" "M=M+1"
       ;保存当前调用者 LCL, ARG, THIS, THAT 并增长堆栈
       "@LCL" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"
       "@ARG" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"
       "@THIS" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"
       "@THAT" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"
       ;正确指向 ARG 的位置，SP - 上面五条 - numArgs
       (str "@" numArgs) "D=A" "@5" "D=D+A"
       "@SP" "A=M" "D=A-D" "@ARG" "M=D"
       ;重置 LCL 的位置为当前 SP
       "@SP" "D=M" "@LCL" "M=D"
       ;跳转到函数入口
       (str "@" func-name) "0;JEQ"
       ;继续执行调用者指令的标记
       (str "(" point  ")")])))

(defn trans-return
  "翻译返回命令
  |arg0    | <- 调用者压入被调用者的参数           -> |ret value|
  |arg1    | <- 调用者压入被调用者的参数           -> |SP HERE  |
  |ret addr| <- 被调用者压入，调用者获取执行结果
  |LCL     | <- 调用者压入，被调用者 return 前还原
  |ARG     | <- 调用者压入，被调用者 return 前还原
  |THIS    | <- 调用者压入，被调用者 return 前还原
  |THAT    | <- 调用者压入，被调用者 return 前还原
  |xxxx    | <- 被调用者 LCL 位置
  |SP HERE | <- 被调用者 SP 位置"
  []
  (let [val (next-int)
        tempVar (str "@temp" val)
        retVar (str "@ret" val)]
    (flatten [;保存 LCL 位置到临时变量 FRAME
              "@LCL" "D=M" tempVar "M=D"
              ;获取返回后下一条指令的地址 -- FRAME - 5 偏移处的值
              "@5" "D=A" tempVar "A=A-D" "D=M" retVar "M=D"
              ;将当前函数的返回值放到 ARG 位置(SP 上一个位置/栈顶)，以便调用者获取
              "@SP" "M=M-1" "A=M" "D=M" "@ARG" "M=D"
              ;恢复调用者 SP = ARG + 1
              "@ARG" "D=A+1" "@SP" "M=D"
              ;恢复 THAT, THIS, ARG, LCL -- FRAME - 1..4 偏移处的值
              tempVar "D=A" "@1" "A=D-A" "D=M" "@THAT" "M=D"
              tempVar "D=A" "@2" "A=D-A" "D=M" "@THIS" "M=D"
              tempVar "D=A" "@3" "A=D-A" "D=M" "@ARG" "M=D"
              tempVar "D=A" "@4" "A=D-A" "D=M" "@LCL" "M=D"
              ;跳转到调用者原来指令位置继续执行
              retVar "0;JEQ"])))

(defn trans-function
  "翻译函数定义命令，这里的函数名包含了文件名，格式为 文件名.函数名。
  根据参数的个数将 local 对应偏移位置的值进行 0 初始化"
  [filename func-name ^Integer numLocals]
  (vec (flatten [(str "(" func-name ")")
                 (map (fn [_] (trans-push-pop filename "push" "constant" 0))
                      (range 0 numLocals))
                 (map #(trans-push-pop filename "pop" "local" %)
                      (range 0 numLocals))])))

(defn trans-init
  "翻译初始化命令
  SP=256, call Sys.init"
  []
  (into ["@256" "D=A" "@SP" "M=D"] (trans-call "Sys.init" 0)))

(defn translate
  "对过滤后的纯命令执行翻译"
  [file-name cmds]
  (reduce
    (fn [agg cmd]
      (if-let [res
               (case (cmd-type cmd)
                 :C-ARITHMETIC (trans-arithmetic cmd)
                 :C-POP (trans-push-pop file-name "pop" (arg1 cmd) (arg2 cmd))
                 :C-PUSH (trans-push-pop file-name "push" (arg1 cmd) (arg2 cmd))
                 :C-LABEL (trans-label (arg1 cmd))
                 :C-GOTO (trans-goto (arg1 cmd))
                 :C-IF (trans-if (arg1 cmd))
                 :C-FUNCTION (trans-function file-name (arg1 cmd)
                                             (Integer/parseInt (arg2 cmd)))
                 :C-RETURN (trans-return)
                 (throw (RuntimeException. (str "尚未实现此命令处理 " cmd))))]
        (into (conj agg (str "//" cmd)) res)
        agg))
    [] cmds))

(defn pure-cmds
  "去除注释行和空行，以及每行指令后的 // 部分"
  [cmds]
  (reduce (fn [agg cmd]
            (let [cmd (str/trim cmd)
                  start-slash (str/starts-with? cmd "//")
                  empty-line (str/blank? cmd)]
              (if (or start-slash empty-line)
                agg
                (conj agg
                      (-> cmd (str/split #"//")
                          first (str/trim)))))) [] cmds))

(comment
  (let [file "C:\\Users\\mazhangjing\\Desktop\\learn-nand\\projects\\08\\FunctionCalls\\SimpleFunction\\SimpleFunction.vm"
        input (Paths/get file (into-array [""]))
        pure-name (-> (str (.getFileName input)) (str/split #"\.") first)
        output (.resolve (.getParent input) (str pure-name ".asm"))]
    (->> (read-vm file)
         (pure-cmds)
         (translate pure-name)
         (save-to (str output)))))