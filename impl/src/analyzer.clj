(ns analyzer
  (:require [clojure.data.xml :refer [emit sexp-as-element]]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io FileWriter)
           (java.nio.file Paths)
           (java.util Scanner)
           (java.util.regex Pattern)))

(defonce scanner (atom nil))

(defn read-jack
  "从文件中读入 .jack 程序，返回 token"
  [filename]
  #_(swap! scanner conj (Scanner. ^String (slurp filename)))
  (let [origin (slurp filename)
        input (-> origin
                  (str/replace "{" " { ")
                  (str/replace "}" " } ")
                  (str/replace "(" " ( ")
                  (str/replace ")" " ) ")
                  (str/replace "[" " [ ")
                  (str/replace "]" " ] ")
                  (str/replace "." " . ")
                  (str/replace "," " , ")
                  (str/replace ";" " ; ")
                  (str/replace "+" " + ")
                  (str/replace "=" " = ")
                  (str/replace "*" " * ")
                  (str/replace "/" " / ")
                  (str/replace "&" " & ")
                  (str/replace "|" " | ")
                  (str/replace "<" " < ")
                  (str/replace ">" " > ")
                  (str/replace "=" " = ")
                  (str/replace "~" " ~ "))]
    (reset! scanner (Scanner. input))))

(defn save-to
  "将生成的 list 数据写入到文件中"
  [filename output]
  (with-open [w (io/writer filename)]
    (.write w (str/join "\n" output))))

(defn save-xml-to
  "将 Hiccup 格式 XML 数据转换并保存到文件"
  [file tags]
  (with-open [out (FileWriter. ^String file)]
    (emit (sexp-as-element tags) out)))

(defn next-token
  "获取下一个字元的信息，包括其类型和数据 {:type :data}"
  []
  (let [sc @scanner
        id-pattern #"[a-zA-Z_][a-zA-Z0-9_]+"
        str-pattern #"\".*?\""]
    (cond (or (.hasNext sc "class")
              (.hasNext sc "constructor")
              (.hasNext sc "function")
              (.hasNext sc "method")
              (.hasNext sc "field")
              (.hasNext sc "static")
              (.hasNext sc "var")
              (.hasNext sc "int")
              (.hasNext sc "char")
              (.hasNext sc "boolean")
              (.hasNext sc "void")
              (.hasNext sc "true")
              (.hasNext sc "false")
              (.hasNext sc "null")
              (.hasNext sc "this")
              (.hasNext sc "let")
              (.hasNext sc "do")
              (.hasNext sc "if")
              (.hasNext sc "else")
              (.hasNext sc "while")
              (.hasNext sc "return"))
          {:type :keyword :data (.next sc)}
          (or (.hasNext sc "{")
              (.hasNext sc "}")
              (.hasNext sc "(")
              (.hasNext sc ")")
              (.hasNext sc "[")
              (.hasNext sc "]")
              (.hasNext sc ".")
              (.hasNext sc ",")
              (.hasNext sc ";")
              (.hasNext sc "+")
              (.hasNext sc "=")
              (.hasNext sc "*")
              (.hasNext sc "/")
              (.hasNext sc "&")
              (.hasNext sc "|")
              (.hasNext sc "<")
              (.hasNext sc ">")
              (.hasNext sc "=")
              (.hasNext sc "~"))
          {:type :symbol :data (.next sc)}
          (.hasNextInt sc)
          {:type :integerConstant :data (.nextInt sc)}
          (.hasNext sc id-pattern)
          {:type :identifier :data (.next sc id-pattern)}
          (.hasNext sc str-pattern)
          {:type :stringConstant :data (-> (.next sc str-pattern)
                                           (str/replace "\"" ""))}
          :else nil)))

(defn parser
  "将终结符转换为 <keyword/symbol/integerConstant/stringConstant/identifier> 无子节点
  将非终结符转换为 <class/classVarDec/subroutineDec/parameterList/subroutineBody/
  varDec/statements/whileStatement/ifStatement/returnStatement/letStatement/
  doStatement/expression/term/expressionList> 带子元素节点"
  [lines]
  [:foo {:foo-attr "FOO VALUE"}
   [:bar {:bar-attr "BAR VALUE"}
    [:baz {} "Baz Value"]]])

(comment                                                    ;单个 .jack 程序
  (let [file "/Users/corkine/Desktop/nand2tetris/projects/10/Square/Main.jack"
        input (Paths/get file (into-array [""]))
        pure-name (-> (str (.getFileName input)) (str/split #"\.") first)
        output (.resolve (.getParent input) (str pure-name ".cmp.xml"))]
    (->> (read-jack file)
         (parser)
         (save-xml-to (str output)))))