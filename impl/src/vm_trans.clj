(ns vm-trans
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

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
    ;算术命令，包括 add,sub,neg,eq,gt,lt,and,or,not
    (cond (contains? #{"add" "sub" "neg" "eq" "gt" "lt"
                       "and" "or" "not"} cmd) :C-ARITHMETIC
          ;压入堆栈命令，包括 push ?? ??
          (str/includes? cmd "push") :C-PUSH
          ;弹出堆栈命令，包括 pop ?? ??
          (str/includes? cmd "pop") :C-POP
          ;标签命令，包括 label !!
          (str/includes? cmd "label") :C-LABEL
          ;跳转命令，包括 goto !!
          (str/includes? cmd "goto") :C-GOTO
          ;判断命令，包括 if-goto !!
          (str/includes? cmd "if-goto") :C-IF
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

;cmd 需要保证去除 // 注释内容以及注释行、空行、只包含空格的行

(defonce index (atom 0))

(defn next-int []
  (reset! index (+ @index 1))
  @index)

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
           (if (= segment "pointer") "@THIS" "@R5") "A=D+A" "D=M"
           "@R13" "M=D" "@SP" "M=M-1" "A=M" "D=M" "@R13" "M=D"]
          (and (= cmd "push") (= "static" segment))
          [(str "@" file-name "." index) "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"]
          (and (= cmd "pop") (= "static" segment))
          ["@SP" "M=M-1" "A=M" "D=M" (str "@" file-name "." index) "M=D"]
          :else (throw (RuntimeException. "未实现此转换")))))

(defn translate
  "对过滤后的纯命令执行翻译"
  [file-name cmds]
  (reduce (fn [agg cmd]
            (println "now trans" cmd)
            (if-let [res
                     (case (cmd-type cmd)
                       :C-ARITHMETIC (trans-arithmetic cmd)
                       :C-POP (trans-push-pop file-name "pop" (arg1 cmd) (arg2 cmd))
                       :C-PUSH (trans-push-pop file-name "push" (arg1 cmd) (arg2 cmd))
                       (throw (RuntimeException. (str "尚未实现此命令处理" cmd))))]
              (into agg res)
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

#_(->> (read-vm "C:\\Users\\mazhangjing\\Desktop\\learn-nand\\projects\\07\\StackArithmetic\\StackTest\\StackTest.vm")
       (pure-cmds)
       (translate)
       (save-to "C:\\Users\\mazhangjing\\Desktop\\learn-nand\\projects\\07\\result.asm"))

(->> (read-vm "../projects/07/MemoryAccess/BasicTest/BasicTest.vm")
     (pure-cmds)
     (translate "BasicTest")
     (save-to "../projects/07/MemoryAccess/BasicTest/BasicTest.asm"))