(ns assembler
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def is-a #(or (re-find #"^@[a-zA-Z0-9\-\+\&\|\!_\.]+$" %)
               (re-find #"^@\d+$" %)))
(def get-variable #(get (re-find #"^@([a-zA-Z].*?)$" %) 1 nil))

(def is-c #(let [find (re-find #"\w?=?\w{0,3};?\w{0,3}?" %)]
             (and (not (nil? find)) (not (str/blank? find)))))
(def is-c-jmp #(str/includes? % ";"))

(def is-l #(not (nil? (re-find #"\([a-zA-Z0-9\$\._-]+\)" %))))
(def get-symbol-from-l #(get (re-find #"\(([a-zA-Z0-9\$\._-]+)\)" %) 1))
(def get-symbol #(get (re-find #"^@([a-zA-Z0-9]+)$" %) 1))
(def get-dest #(get (re-find #"(\w+)?=\w{0,3};?\w{0,3}?" %) 1 nil))
(def get-comp
  #(get (re-find (if (str/includes? % ";")
                   #"(\w+);\w+" #"\w+=([a-zA-Z0-9\-\+\&\|\!]+)") %) 1 nil))
(def get-jump #(get (re-find #"\w?=?\w{0,3};?(\w{0,3})?" %) 1 nil))

(def imme-num->bin
  #(str/replace
    (format "%15s" (Integer/toBinaryString (Integer/parseInt %))) " " "0"))

(def comp-map
  "comp 符号表，7 位，包括 111a c1-6"
  {"0" "0101010"
   "1" "0111111"
   "-1" "0111010"
   "D" "0001100"
   "A" "0110000"
   "!D" "0001101"
   "!A" "0110001"
   "-D" "0001111"
   "-A" "0110011"
   "D+1" "0011111"
   "A+1" "0110111"
   "D-1" "0001110"
   "A-1" "0110010"
   "D+A" "0000010"
   "D-A" "0010011"
   "A-D" "0000111"
   "D&A" "0000000"
   "D|A" "0010101"
   "M" "1110000"
   "!M" "1110001"
   "-M" "1110011"
   "M+1" "1110111"
   "M-1" "1110010"
   "D+M" "1000010"
   "D-M" "1010011"
   "M-D" "1000111"
   "D&M" "1000000"
   "D|M" "1010101"})

(def dest-jump-map
  "dest 符号表，3 位，d1-3
   jump 符号表，3 位，j1-3"
  {"null" "000"
   "M" "001"
   "D" "010"
   "MD" "011"
   "A" "100"
   "AM" "101"
   "AD" "110"
   "AMD" "111"
   "JGT" "001"
   "JEQ" "010"
   "JGE" "011"
   "JLT" "100"
   "JNE" "101"
   "JLE" "110"
   "JMP" "111"})

(def pre-symbol-map
  "预定义符号映射地址表"
  {"SP" 0
   "LCL" 1
   "ARG" 2
   "THIS" 3
   "THAT" 4
   "R0" 0
   "R1" 1
   "R2" 2
   "R3" 3
   "R4" 4
   "R5" 5
   "R6" 6
   "R7" 7
   "R8" 8
   "R9" 9
   "R10" 10
   "R11" 11
   "R12" 12
   "R13" 13
   "R14" 14
   "R15" 15
   "SCREEN" 16384
   "KBD" 24576})

(defn parser
  "将输入的简单 .asm 汇编代码进行解析"
  [commands]
  (reduce 
   (fn [output new-command]
     (let [new-command (str/trim new-command)]
       (cond (str/blank? new-command) output
             (str/starts-with? new-command "//") output
             (is-a new-command)
             (let [numstr (get-symbol new-command)]
               (conj output (str "0" (imme-num->bin numstr))))
             (is-c new-command)
             (let [dest     (get-dest new-command)
                   comp     (get-comp new-command)
                   jump     (get-jump new-command)
                   dest-bin (get dest-jump-map dest "000")
                   comp-bin (get comp-map comp "0000000")
                   jump-bin (get dest-jump-map jump "000")]
               (conj output (str "111" comp-bin dest-bin jump-bin)))
             :else (conj output new-command)))) 
   [] commands))

(defn read-asm
  "从文件中读入汇编程序，返回 [cmd]"
  [filename]
  (map #(str/replace % "\r" "")
       (str/split (slurp filename) #"\n")))

(defn save-to
  "将生成的 hack 二进制写入到文件中"
  [filename output]
  (with-open [w (io/writer filename)]
    (.write w (str/join "\n" output))))

(defn get-all-commands
  "清洗并且过滤得到所有的命令"
  [commands]
  (let [commands (filter #(or (is-a %) (is-c %) (is-l %))
                         (map #(str/trim %) commands))
        commands (map #(get (str/split % #" ") 0) commands)]
    commands))


(defn mark-all-commands
  "第一遍标记分配 ROM 地址，且将 (xxx) 伪命令映射到正确的 ROM 地址
   [[false @R0  0]] 表示是否是伪命令，当前命令（伪命令去除括号）和其地址"
  [commands]
  (reduce 
   (fn [result-now cmd]
     (let [current-is-l (is-l cmd)
           last-item    (last result-now)
           last-is-l    (first last-item)
           last-index   (last last-item)
           current-item [current-is-l
                         (if current-is-l (get-symbol-from-l cmd) cmd)
                         (if (empty? result-now) 0
                             (if last-is-l
                               last-index
                               (+ last-index 1)))]]
       (conj result-now current-item))) [] commands))

(defn comp-asm->simple-asm
  "将复杂的 .asm 文件转换为不包含伪指令和变量的简单 .asm 指令，以供 parser 解析"
  [commands-from-file]
  (let [clean-commands     (get-all-commands commands-from-file)
        ;解析 (xxx) 伪指令并计算其位置
        mark-fake-commands (mark-all-commands clean-commands)
        ;将预定义的符号表和上述转换得到的 (xxx) 伪指令符号合并起来，得到 符号->位置 表
        sym-table          
        (merge pre-symbol-map
               (into {}
                     (mapv #(-> % rest vec)
                           (filter #(-> % first) mark-fake-commands))))
        sym-table          (atom sym-table)
        next-ram           (atom 16)
        answer             
        (reduce 
         (fn [agg [is-l cmd _]]
           (if is-l agg
               (conj agg
                     (if-let [sym (get-variable cmd)]
                       (if-let [find-in-st (get @sym-table sym)]
                         (str "@" find-in-st)
                         (let [next-addr @next-ram]
                           (reset! next-ram (+ next-addr 1))
                           (swap! sym-table assoc sym next-addr)
                           (str "@" next-addr)))
                       cmd)))) [] mark-fake-commands)]
    answer))


;; (save-to "C:\\Users\\mazhangjing\\Desktop\\learn-nand\\projects\\06\\Test.hack"
;;          (parser (read-asm "C:\\Users\\mazhangjing\\Desktop\\learn-nand\\projects\\06\\max\\Max.asm")))

(save-to "C:\\Users\\mazhangjing\\Desktop\\learn-nand\\projects\\06\\Test.hack"
         (parser (comp-asm->simple-asm
                  (read-asm "C:\\Users\\mazhangjing\\Desktop\\learn-nand\\projects\\06\\rect\\Rect.asm"))))

;; (comp-asm->simple-asm
;;  (read-asm "C:\\Users\\mazhangjing\\Desktop\\learn-nand\\projects\\06\\pong\\Pong.asm"))

;; (def file (read-asm "C:\\Users\\mazhangjing\\Desktop\\learn-nand\\projects\\06\\pong\\Pong.asm"))

;; (mark-all-commands (get-all-commands file))

;; (def mark-fake-commands (mark-all-commands (get-all-commands file)))

;; (save-to "C:\\Users\\mazhangjing\\Desktop\\learn-nand\\projects\\06\\temp.log" mark-fake-commands)

;; (def sym-table (merge pre-symbol-map
;;                       (into {}
;;                             (mapv #(-> % rest vec)
;;                                   (filter #(-> % first) mark-fake-commands)))))

;; sym-table

;; (get sym-table "sys.init")

;; (def next-ram (atom 16))

;; (reduce (fn [agg [is-l cmd _]]
;;           (if is-l agg
;;               (conj agg
;;                         ;对于符号表存在的 @x 元素，替换，反之则从 16 开始分配
;;                     (if-let [sym (get-variable cmd)]
;;                       (if-let [find-in-st (get sym-table sym)]
;;                         (str "@" find-in-st)
;;                         (let [next-addr @next-ram]
;;                           (reset! next-ram (+ next-addr 1))
;;                           (str "@" next-addr)))
;;                       cmd)))) [] mark-fake-commands)
