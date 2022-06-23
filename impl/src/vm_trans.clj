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
  "返回当前命令的类型，有如下几种，如果不是命令，返回 :ERROR
  :C-ARITHMETIC 算术命令，包括 add,sub,neg,eq,gt,lt,and,or,not
  :C-PUSH 压入堆栈命令，包括 push ?? ??
  :C-POP 弹出堆栈命令，包括 pop ?? ??
  :C-LABEL 标签命令，包括 label !!
  :C-GOTO 跳转命令，包括 goto !!
  :C-IF 判断命令，包括 if-goto !!
  :C-FUNCTION 函数定义，包括 function !! !!
  :C-RETURN 返回命令，格式 return
  :C-CALL 调用命令，格式 call !! !!
  :ERROR 无法识别的其他命令"
  [cmd]
  (let [cmd (-> cmd (str/trim) (str/lower-case))]
    (cond (contains? #{"add" "sub" "neg" "eq" "gt" "lt" "and" "or" "not"} cmd)
          :C-ARITHMETIC
          (str/includes? cmd "push")
          :C-PUSH
          (str/includes? cmd "pop")
          :C-POP
          (str/includes? cmd "label")
          :C-LABEL
          (str/includes? cmd "goto")
          :C-GOTO
          (str/includes? cmd "if-goto")
          :C-IF
          (str/includes? cmd "function")
          :C-FUNCTION
          (str/includes? cmd "return")
          :C-RETURN
          (str/includes? cmd "call")
          :C-CALL
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
                pass (str "PASS." index)
                not-pass (str "NOT_PASS." index)
                end (str "END." index)]
            ["@SP" "M=M-1" "A=M" "D=M" ;出栈 y，栈想下移动一格
             "A=A-1" "M=M-D" "D=M" ;计算 x - y 保存到 x 处
             (str "@" pass)
             (cond (= cmd "eq") "D;JEQ"
                   (= cmd "gt") "D;JGT"
                   (= cmd "lt") "D;JLT")
             (str "@" not-pass)
             "0;JMP"
             (str "(" pass ")")
             "@SP" "A=M" "A=A-1" "M=-1" ;将栈第一个元素(计算结果) 更新
             (str "@" end)
             "0;JMP"
             (str "(" not-pass ")")
             "@SP" "A=M" "A=A-1" "M=0"
             (str "(" end ")")]))))

(defn trans-push-pop
  "将内存操作转换为汇编代码
  push constant x: @SP M=M+1 @x D=A @SP A=M M=D"
  [cmd segment index]
  (println "trans push/pop with" cmd segment index)
  (cond (and (= cmd "push")
             (= segment "constant")
             (any? (Integer/parseInt (str index))))
        [(str "@" index) "D=A" "@SP" "A=M" "M=D" "@SP" "M=M+1"]
        :else
        (throw (RuntimeException. "尚未实现此转换"))))

(defn translate
  "对过滤后的纯命令执行翻译"
  [cmds]
  (reduce (fn [agg cmd]
            (println "now trans" cmd)
            (if-let [res
                     (case (cmd-type cmd)
                       :C-ARITHMETIC (trans-arithmetic cmd)
                       :C-POP (trans-push-pop "pop" (arg1 cmd) (arg2 cmd))
                       :C-PUSH (trans-push-pop "push" (arg1 cmd) (arg2 cmd))
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

(->> (read-vm "C:\\Users\\mazhangjing\\Desktop\\learn-nand\\projects\\07\\StackArithmetic\\StackTest\\StackTest.vm")
    (pure-cmds)
    (translate)
    (save-to "C:\\Users\\mazhangjing\\Desktop\\learn-nand\\projects\\07\\result.asm"))