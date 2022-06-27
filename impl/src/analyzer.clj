(ns analyzer
  (:require [clojure.data.xml :refer [emit sexp-as-element]]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io FileWriter)
           (java.nio.file Paths)
           (java.util Scanner)
           (java.util.regex Pattern)))

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

(defn- get-scanner
  "从文件中读入 .jack 程序，去除注释和空行，返回符号易识别的程序"
  [filename]
  (let [file-data (slurp filename)
        sc (Scanner. file-data)
        skip-commit-fn (fn [_]
                         (cond (or (.hasNext sc "//")
                                   (.hasNext sc "/\\*")
                                   (.hasNext sc "/\\*\\*"))
                               (if (.hasNextLine sc)
                                 (do
                                   (.nextLine sc)
                                   (recur nil))
                                 nil)
                               (.hasNext sc) (.next sc)
                               :else nil))]
    (let [pure-data (->> (take-while (comp not nil?)
                                     (iterate skip-commit-fn ""))
                         (str/join " "))
          easy-scan-data (-> pure-data
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
                             (str/replace "~" " ~ ")
                             (str/replace "-" " - "))]
      (println easy-scan-data)
      (Scanner. easy-scan-data))))

(defn- next-token
  "获取下一个字元的信息，包括其类型和数据 {:type :data}"
  [sc]
  (let [id-pattern (Pattern/compile "[a-zA-Z_][a-zA-Z0-9_]*")
        str-start-pattern (Pattern/compile "\".*?")]
    (cond
      (or (.hasNext sc "//")
          (.hasNext sc "/\\*")
          (.hasNext sc "/\\*\\*")) (do (.nextLine sc)
                                       (next-token sc))
      (or (.hasNext sc "class")
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
      (or (.hasNext sc "\\{")
          (.hasNext sc "\\}")
          (.hasNext sc "\\(")
          (.hasNext sc "\\)")
          (.hasNext sc "\\[")
          (.hasNext sc "\\]")
          (.hasNext sc "\\.")
          (.hasNext sc "\\,")
          (.hasNext sc "\\;")
          (.hasNext sc "\\+")
          (.hasNext sc "-")
          (.hasNext sc "\\=")
          (.hasNext sc "\\*")
          (.hasNext sc "\\/")
          (.hasNext sc "\\&")
          (.hasNext sc "\\|")
          (.hasNext sc "\\<")
          (.hasNext sc "\\>")
          (.hasNext sc "\\=")
          (.hasNext sc "\\~"))
      {:type :symbol :data (.next sc)}
      (.hasNextInt sc)
      {:type :integerConstant :data (.nextInt sc)}
      (.hasNext sc id-pattern)
      {:type :identifier :data (.next sc id-pattern)}
      (.hasNext sc str-start-pattern)
      {:type :stringConstant :data (-> (.findInLine sc (Pattern/compile "\".*?\""))
                                       (str/replace "\"" ""))}
      :else
      (do
        (if (.hasNext sc)
          (println "no rule to parse next: " (.next sc))
          (println "end of scanner")) nil))))

(defn do-scan-token
  "扫描输入并将其转换为字元流"
  [filename]
  (let [sc (get-scanner filename)
        token-list (take-while (comp not nil?)
                               (iterate (fn [_] (next-token sc)) ""))]
    (filterv map? token-list)))

(defn- token->node [{:keys [type data]}] {(keyword type) data})

(defn- tokens->nodes [ts] (mapv token->node (filterv (comp not nil?) ts)))

(defn- assert-first [f & ts]
  (doseq [index (range 0 (count ts))]
    (let [{:keys [type] :as check} (get f index)
          need (get (vec ts) index)]
      (when (not= need type)
        (throw (RuntimeException. (str "无法解析字元 " check)))))))

(defn- check-first [f & ts]
  (empty? (filter #(not= (get (vec ts) %) (:type (get f %)))
                  (range 0 (count ts)))))

(defn- check!
  "usage: (check! [{:type :s :data 1} {:type :b :data 2}]
                  [:or (fn [t d] (= t :s2)) (fn [t d] (= t :s))] :b)"
  [f & ts]
  (= (count ts)
     (count (filter (fn [index]
                      (let [{targetType :type targetData :data :as target}
                            (get f index)
                            check (get (vec ts) index)]
                        (cond (keyword? check)              ;只检查一种类型, eg. :keyword
                              (= check targetType)
                              (and (set? check)             ;只检查多种类型, eg. #{:keyword :identifier}
                                   (empty? (filter (comp not keyword?) check)))
                              (contains? check targetType)
                              (fn? check)                   ;自定义检查, eg. (fn [type data] true)
                              (check targetType targetData)
                              (and (vector? check)          ;自定义多个检查, eg. [:and (fn [type data] true)]
                                   (= :and (first check))
                                   (empty? (filter (comp not fn?) (next check))))
                              (not-any? #(not (% targetType targetData)) check)
                              (and (vector? check)          ;自定义多个检查, eg. [:or (fn [type data] true)]
                                   (= :or (first check))
                                   (empty? (filter (comp not fn?) (next check))))
                              (some #(% targetType targetData) check))))
                    (range 0 (count ts))))))

(defonce ts (atom '()))

(defn- pop-ts [] (let [first-of-ts (first @ts)]
                  (swap! ts next)
                  first-of-ts))

(defn- peek-ts [] (first @ts))

(defn- top-ts [n] (take n @ts))

(defn- push-ts [& head]
  (reset! ts (into @ts head))
  nil)

(defn- push-ts-warn [& head]
  (println (str "WARN: can't parse from " head))
  (reset! ts (into @ts head))
  nil)

(defn- warn [message]
  (println (str "WARN: " message))
  nil)

(defn- type-and-contains? [type & data]
  #(and (= :keyword type) (contains? (set data) %2)))

(defn- type-and? [type data]
  #(and (= :keyword type) (= data %2)))

(defn- type? [type]
  (fn [t _] (= type t)))

(declare compileExpression)

(declare compileExpressionList)

(declare compileStatement)

(defn- compileClassVarDec
  "编译静态/字段声明，不匹配返回 nil
  static/filed type varName (, varName)*;
  type is: int/char/boolean(kw) or className(id)" []
  (let [sfNode (pop-ts) typeNode (pop-ts) varNode (pop-ts)]
    (if
      (check! [sfNode typeNode varNode]
              (type-and-contains? :keyword "static" "field")
              [:or (type-and-contains? :keyword "int" "char" "boolean") (type? :identifier)]
              (type? :identifier))
      (let [nextVars (take-while (fn [splitNode var2Node]
                                   (if (check! [splitNode var2Node]
                                               (type-and? :symbol ",")
                                               (type? :identifier))
                                     true
                                     (do (push-ts var2Node splitNode) false)))
                                 (repeatedly (fn [] [(pop-ts) (pop-ts)])))
            nextVarsNode (map second nextVars)
            endNode (pop-ts)]
        (when (check! [endNode] (type-and? :symbol ";"))
          (-> (tokens->nodes [sfNode typeNode varNode])
              (into (tokens->nodes nextVarsNode))
              (conj (token->node endNode)))))
      (push-ts varNode typeNode sfNode))))

(defn- compileParameterList
  "编译参数列表，可能为空，不包含 ()
  empty or 'int a' or 'int a, boolean b'" []
  (let [varTypeNode (pop-ts)
        varNameNode (pop-ts)]
    (if (check! [varTypeNode varNameNode]
                [:or (type-and-contains? :keyword "int" "char" "boolean") (type? :identifier)]
                (type? :identifier))
      (let [nextTypeVars (take-while (fn [splitNode varType2Node varName2Node]
                                       (if (check! [splitNode varType2Node varName2Node]
                                                   (type-and? :symbol ",")
                                                   [:or (type-and-contains? :keyword "int" "char" "boolean")
                                                    (type? :identifier)]
                                                   (type? :identifier))
                                         true
                                         (do (push-ts varName2Node varType2Node splitNode) false)))
                                     (repeatedly (fn [] [(pop-ts) (pop-ts) (pop-ts)])))
            ;may nil
            nextTypeVarsNodes (map second nextTypeVars)]
        (-> (tokens->nodes [varTypeNode varNameNode])
            (into (tokens->nodes nextTypeVarsNodes)))))
    (push-ts varNameNode varTypeNode)))

(defn- compileVarDec
  "编译 var 声明
  var type varName, varName,.. ;" []
  (let [varTypeNode (pop-ts)
        typeNode (pop-ts)
        varNameNode (pop-ts)]
    (if (check! [varTypeNode typeNode varNameNode]
                (type-and? :symbol "var")
                [:or (type-and-contains? :keyword "int" "char" "boolean") (type? :identifier)]
                (type? :identifier))
      (let [nextVars (take-while (fn [splitNode var2Node]
                                   (if (check! [splitNode var2Node]
                                               (type-and? :symbol ",")
                                               (type? :identifier))
                                     true
                                     (do (push-ts var2Node splitNode) false)))
                                 (repeatedly (fn [] [(pop-ts) (pop-ts)])))
            nextVarsNode (map second nextVars)              ;may nil
            endNode (pop-ts)]
        (when (check! [endNode] (type-and? :symbol ";"))
          (-> (tokens->nodes [varTypeNode typeNode varNameNode])
              (into (tokens->nodes nextVarsNode))
              (conj (token->node endNode))))
        (push-ts-warn varNameNode typeNode varTypeNode)))))

(defn- compileDo
  "编译 do 语句
  do subName ( expressionList )
  do className/varName . subName ( expressionList )" []
  (let [doNode (pop-ts)]
    (let [{c1Type :type :as varNameOrSubroutineName} (pop-ts)
          {c2Type :type c2Data :data} (peek-ts)]
      (if-not (= :identifier c1Type)
        (push-ts-warn varNameOrSubroutineName doNode)
        (cond
          (and (= :symbol c2Type)
               (= "(" c2Data))
          ;subName ( expressionList )
          (let [leftB (pop-ts)
                expressionList (compileExpressionList)
                rightB (pop-ts)]
            (if (and (check! [leftB rightB]
                             (type-and? :symbol "(")
                             (type-and? :symbol ")"))
                     (not (nil? expressionList)))
              (tokens->nodes [varNameOrSubroutineName
                              leftB expressionList rightB])
              (push-ts-warn rightB leftB varNameOrSubroutineName doNode)))
          (and (= :symbol c2Type)
               (= "." c2Data))
          ;className/varName . subName ( expressionList )
          (let [point (pop-ts)
                subName (pop-ts)
                leftB (pop-ts)
                expressionList (compileExpressionList)
                rightB (pop-ts)]
            (if (and (check! [point subName leftB rightB]
                             (type-and? :symbol ".")
                             (type? :identifier)
                             (type-and? :symbol "(")
                             (type-and? :symbol ")"))
                     (not (nil? expressionList)))
              (tokens->nodes [varNameOrSubroutineName
                              point subName
                              leftB expressionList rightB])
              (push-ts-warn rightB leftB subName point
                            varNameOrSubroutineName doNode)))
          :else (push-ts-warn varNameOrSubroutineName doNode))))))

(defn- compileLet
  "编译 let 语句
  let varName = expression ;
  let varName [ expression ] = expression ;" []
  (let [letNode (pop-ts)
        varNameNode (pop-ts)
        equalOrLeftMiddleBruceNode (pop-ts)]
    (if (check! [letNode varNameNode equalOrLeftMiddleBruceNode]
                (type-and? :keyword "let")
                (type? :identifier)
                (type-and-contains? :symbol "=" "["))
      (if (= (:data equalOrLeftMiddleBruceNode) "=")
        ;let varName = expression ;
        (let [expressionNode (compileExpression)
              endNode (pop-ts)]
          (if (and (not (nil? expressionNode))
                   (check! [endNode] (type-and? :symbol ";")))
            (tokens->nodes [letNode varNameNode
                            equalOrLeftMiddleBruceNode expressionNode])
            (push-ts-warn endNode)))
        ;let varName [ expression ] = expression ;
        (let [expressionNode (compileExpression)
              rightMiddleBruceNode (pop-ts)
              equalNode (pop-ts)
              expression2Node (compileExpression)
              endNode (pop-ts)]
          (if (and expressionNode
                   expression2Node
                   (check! [rightMiddleBruceNode equalNode endNode]
                           (type-and? :symbol "]")
                           (type-and? :symbol "=")
                           (type-and? :symbol ";")))
            (tokens->nodes [letNode varNameNode
                            equalOrLeftMiddleBruceNode
                            expressionNode rightMiddleBruceNode
                            equalNode expression2Node endNode])
            (push-ts-warn endNode equalNode rightMiddleBruceNode))))
      (push-ts-warn equalOrLeftMiddleBruceNode varNameNode letNode))))

(defn- compileWhile
  "编译 while 语句
  while ( expression ) { statement }" []
  (let [while (pop-ts)
        leftBruce (pop-ts)
        expression (compileExpression)
        rightBruce (pop-ts)
        leftBigBruce (pop-ts)
        statement (compileStatement)
        rightBigBruce (pop-ts)]
    (if (and (check! [while leftBruce rightBruce
                      leftBigBruce rightBigBruce]
                     (type-and? :keyword "while")
                     (type-and? :symbol "(")
                     (type-and? :symbol ")")
                     (type-and? :symbol "{")
                     (type-and? :symbol "}"))
             (not (nil? expression))
             (not (nil? statement)))
      (tokens->nodes [while leftBruce expression
                      rightBruce leftBigBruce
                      statement rightBigBruce])
      (push-ts-warn rightBigBruce leftBigBruce rightBruce
                    leftBruce while))))

(defn- compileReturn
  "编译 return 语句
  return expression ;
  return ;" []
  (let [return (pop-ts)
        {:keys [type data] :as mayEnd} (peek-ts)]
    (if (check! [return] (type-and? :keyword "return"))
      (if (and (= type :symbol) (= data ";"))
        (do
          (pop-ts)
          (tokens->nodes [return mayEnd]))
        (let [expression (compileExpression)
              endNode (pop-ts)]
          (if (and (not (nil? expression))
                   (check! [endNode] (type-and? :symbol ";")))
            (tokens->nodes [return expression endNode])
            (push-ts-warn endNode return))))
      (push-ts-warn return))))

(defn- compileIf
  "编译 if 语句
  if ( expression ) { statement }
  (else { statement })?" []
  (let [ifNode (pop-ts)
        leftB (pop-ts)
        expression (compileExpression)
        rightB (pop-ts)
        leftBigB (pop-ts)
        statement (compileStatement)
        rightBigB (pop-ts)]
    (if (and (check! [ifNode leftB rightB
                      leftBigB rightBigB]
                     (type-and? :keyword "if")
                     (type-and? :symbol "(")
                     (type-and? :symbol ")")
                     (type-and? :symbol "{")
                     (type-and? :symbol "}"))
             (not (nil? expression))
             (not (nil? statement)))
      (if-not (let [{:keys [data type]} (peek-ts)]
                (and (= "else" data) (= :keyword type)))
        ;没有 else 的 if 语句
        (tokens->nodes [ifNode leftB expression rightB
                        leftBigB statement rightBigB])
        (let [elseNode (pop-ts)
              leftBigB2 (pop-ts)
              statement2 (compileStatement)
              rightBigB2 (pop-ts)]
          (if (and (check! [elseNode leftBigB2 rightBigB2]
                           (type-and? :keyword "else")
                           (type-and? :symbol "{")
                           (type-and? :symbol "}"))
                   (not (nil? statement2)))
            ;包含 if 和 else 语句
            (tokens->nodes [ifNode leftB expression rightB
                            leftBigB statement rightBigB
                            elseNode leftBigB2 statement2 rightBigB2])
            (push-ts-warn rightBigB2 leftBigB2 elseNode))))
      (push-ts-warn rightBigB leftBigB rightB leftB ifNode))))

(defn- compileStatement
  "编译单条语句
  statement 包括 let/if/while/do/return Statement"
  []
  (let [{:keys [type data]} (peek-ts)]
    (if-not (= :keyword type)
      (warn (str "can't parse statement from location: " (top-ts 5)))
      (case data
        "let" (compileLet)
        "if" (compileIf)
        "while" (compileWhile)
        "do" (compileDo)
        "return" (compileReturn)
        (warn (str "can't parse statement from location: " (top-ts 5)))))))

(defn- compileStatements
  "编译语句，不包括 {}
  statement* 可能为空，返回 nil" []
  (take-while (comp not nil)
              (repeatedly compileStatement)))

(defn- compileSubroutine
  "编译方法、函数或构造函数" []
  (let [typeTypeNode (pop-ts)
        returnTypeNode (pop-ts)
        subroutineNameNode (pop-ts)
        leftSmallBruceNode (pop-ts)]
    (if (check! [typeTypeNode returnTypeNode subroutineNameNode leftSmallBruceNode]
                (type-and-contains? :keyword "constructor" "function" "method")
                [:or (type-and? :keyword "void") (type? :identifier)]
                (type-and? :symbol "("))
      (let [;空 parameterList 返回 nil
            paramListNode (compileParameterList)
            rightSmallBruceNode (pop-ts)
            ;subroutineBody { varDec* statements }
            leftBigBruceNode (pop-ts)
            ;var 声明可能为 nil
            allVarDecNode (take-while (comp not nil?) (repeatedly compileVarDec))
            statementsNode (compileStatements)              ;statementsNode 可能为 nil
            rightBigBruceNode (pop-ts)]
        (if (check! [leftBigBruceNode rightBigBruceNode rightSmallBruceNode]
                    (type-and? :symbol "{") (type-and? :symbol "}") (type-and? :symbol ")"))
          (tokens->nodes [typeTypeNode returnTypeNode subroutineNameNode
                          leftSmallBruceNode paramListNode rightSmallBruceNode
                          leftBigBruceNode allVarDecNode statementsNode rightBigBruceNode])
          ;并非本函数 pop 的数据由其自身确保压回到 ts
          (push-ts-warn rightBigBruceNode leftBigBruceNode rightSmallBruceNode)))
      (push-ts leftSmallBruceNode subroutineNameNode returnTypeNode typeTypeNode))))

(defn- compileClass
  "编译整个类，如果不是类结构，返回 nil
  class className { classVarDec* subroutineDec* }" []
  (let [classNode (pop-ts)
        classNameNode (pop-ts)
        leftBigBruceNode (pop-ts)]
    (if (check! [classNode classNameNode leftBigBruceNode]
                (type-and? :keyword "class") :identifier :symbol)
      (let [all-classVarDec
            (take-while (comp not nil?) (repeatedly compileClassVarDec))
            all-subroutineDec
            (take-while (comp not nil?) (repeatedly compileSubroutine))]
        (-> (tokens->nodes [classNameNode leftBigBruceNode])
            (into all-classVarDec)
            (into all-subroutineDec)))
      (push-ts-warn leftBigBruceNode classNameNode classNode))))

(defn- compileTerm
  "编译 term，包括对标识符字元区分变量、数组和子程序调用
  包括 integer/string/keywordConstant,
  varName, varName [ expression ], subroutineCall,
  ( expression ), unaryOp term" []
  ;决定是否是 integer/string/keywordConstant
  (let [{:keys [type] :as next-check} (peek-ts)]
    (if (or (= :integerConstant type)
            (= :stringConstant type)
            (check! [next-check]
                    (type-and-contains? :keyword
                                        "true" "false" "null" "this")))
      (token->node (pop-ts))
      ;决定是否是 ( expression ) 或 unaryOp term
      (let [{:keys [type data]} (peek-ts)]
        (cond (and (= :symbol type) (= "(" data))
              (let [leftBruce (pop-ts)
                    expressionNode (compileExpression)
                    rightBruce (pop-ts)]
                (if (and (not (nil? expressionNode))
                         (check! [leftBruce rightBruce]
                                 (type-and? :symbol ")")
                                 (type-and? :symbol ")")))
                  (tokens->nodes [leftBruce expressionNode rightBruce])
                  (push-ts-warn rightBruce leftBruce)))
              (and (= :symbol type) (contains? #{"-" "~"} data))
              (let [unary (pop-ts)
                    term (compileTerm)]
                (if (not (nil? term))
                  (tokens->nodes [unary term])
                  (push-ts-warn unary)))
              :else
              ;区分 varName
              ;    varName [ expression ],
              ;    subroutineName ( expressionList )
              ;    className/varName . subroutineName ( expressionList )
              (let [{c1Type :type :as varNameOrSubroutineName} (pop-ts)
                    {c2Type :type c2Data :data :as varNameOrSubroutineLeftBruceOrPoint}
                    (peek-ts)]
                (if-not (= :identifier c1Type)
                  (push-ts-warn varNameOrSubroutineName)
                  (cond (and (= :symbol c2Type)
                             (= "[" c2Data))
                        ;varName [ expression ]
                        (let [leftBB (pop-ts)
                              expression (compileExpression)
                              rightBB (pop-ts)]
                          (if (and (check! [leftBB rightBB]
                                           (type-and? :symbol "[")
                                           (type-and? :symbol "]"))
                                   (not (nil? expression)))
                            (tokens->nodes [varNameOrSubroutineName
                                            leftBB expression rightBB])
                            (push-ts-warn rightBB leftBB varNameOrSubroutineName)))
                        (and (= :symbol c2Type)
                             (= "(" c2Data))
                        ;subName ( expressionList )
                        (let [leftB (pop-ts)
                              expressionList (compileExpressionList)
                              rightB (pop-ts)]
                          (if (and (check! [leftB rightB]
                                           (type-and? :symbol "(")
                                           (type-and? :symbol ")"))
                                   (not (nil? expressionList)))
                            (tokens->nodes [varNameOrSubroutineName
                                            leftB expressionList rightB])
                            (push-ts-warn rightB leftB varNameOrSubroutineName)))
                        (and (= :symbol c2Type)
                             (= "." c2Data))
                        ;className/varName . subName ( expressionList )
                        (let [point (pop-ts)
                              subName (pop-ts)
                              leftB (pop-ts)
                              expressionList (compileExpressionList)
                              rightB (pop-ts)]
                          (if (and (check! [point subName leftB rightB]
                                           (type-and? :symbol ".")
                                           (type? :identifier)
                                           (type-and? :symbol "(")
                                           (type-and? :symbol ")"))
                                   (not (nil? expressionList)))
                            (tokens->nodes [varNameOrSubroutineName
                                            point subName
                                            leftB expressionList rightB])
                            (push-ts-warn rightB leftB subName point
                                          varNameOrSubroutineName)))
                        :else                               ;varName
                        (tokens->nodes [varNameOrSubroutineName])))))))))

(defn- compileExpression
  "编译表达式
  term (op term)*" []
  (let [termNode (compileTerm)
        isOP? #(check! [%] (type-and-contains?
                             :symbol "+" "-" "*" "/" "&" "|" "<" ">" "="))]
    (if (not (nil? termNode))
      (let [rest-op-term
            (take-while (fn [op term]
                          (let [isOpTerm? (and (not (nil? term)) (isOP? op))]
                            (if-not isOpTerm? (do (push-ts op) nil) true)))
                        (repeatedly (fn [] [(pop-ts) (compileTerm)])))
            ;may nil
            op-terms (reduce (fn [agg [op term]] (conj agg op term)) [] rest-op-term)]
        (into (tokens->nodes [termNode])
              (tokens->nodes op-terms)))
      (warn (str "can't parse expression at " (top-ts 5))))))

(defn- compileExpressionList
  "编译逗号分隔符分割的表达式列表（可空）
  expression
  expression, expression" []
  (if-let [exp (compileExpression)]
    (let [nextExps (take-while
                     (fn [{:keys [type data] :as sp} expNode]
                       (let [result (and (= :symbol type) (= "," data)
                                         (not (nil? expNode)))]
                         (if result true (do (push-ts-warn sp) nil))))
                     (fn [] [(pop-ts) (compileExpression)]))
          nextExpNodes (reduce (fn [agg [sp exp]] (conj agg sp exp))
                               [] nextExps)]                ;may nil
      (into (tokens->nodes [exp])
            (tokens->nodes nextExpNodes)))
    (warn (str "can't parse expression: " (top-ts 5)))))

(defn- do-compilation
  "执行整个翻译，class 是 Jack 基本单元，因此每个文件第一个 token 一定是 class" []
  (let [{:keys [type data]} (peek-ts)]
    (if (and (= :keyword type) (= "class" data))
      (compileClass)
      (throw (RuntimeException. "文件的第一个字元应该是 class")))))

(defn parser
  "将终结符转换为 <keyword/symbol/integerConstant/stringConstant/identifier> 无子节点
  将非终结符转换为 <class/classVarDec/subroutineDec/parameterList/subroutineBody/
  varDec/statements/whileStatement/ifStatement/returnStatement/letStatement/
  doStatement/expression/term/expressionList> 带子元素节点"
  [token-stream]
  (reset! ts (apply list token-stream))
  (do-compilation))

(comment                                                    ;单个 .jack 程序
  (let [file "../projects/10/Square/Main.jack"
        input (Paths/get file (into-array [""]))
        pure-name (-> (str (.getFileName input)) (str/split #"\.") first)
        output (.resolve (.getParent input) (str pure-name ".cmp.xml"))]
    (->> (do-scan-token file)
         (parser)
         (save-xml-to (str output)))))

;;TODO fixme 每个非终结符编译后应该形成一个外部嵌套的 XML 节点，而非只输出其子节点