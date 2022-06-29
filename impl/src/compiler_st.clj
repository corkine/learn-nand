(ns compiler-st
  (:require [clojure.data.xml :refer [emit sexp-as-element]]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.nio.file Paths)
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
  (with-open [out (clojure.java.io/writer file)]
    (emit (sexp-as-element tags) out)))

(defonce classVarTable (atom {}))

(defonce subroutineVarTable (atom {}))

(defonce status (atom {;用于标记当前 Scanner 是否处于多行注释中(可能为单行,/** */)
                       :in-multiline-comment false
                       ;用于将当前标识符插入到对应的 hashMap 中
                       :in-class             false
                       :in-subroutine        false
                       :in-statement         false
                       :current-class-name   nil}))

(defn in-status? [key] (not (nil? (get @status key))))

(defn set-status! [key]
  (cond (= :in-class key)
        (reset! classVarTable {})
        (= :in-subroutine key)
        (reset! subroutineVarTable {}))
  (swap! status assoc key true))

(defn set-class-name [name]
  (swap! status assoc :current-class-name name))

(defn unset-class-name []
  (swap! status dissoc :current-class-name))

(defn current-class-name []
  (:current-class-name @status))

(defn unset-status! [key]
  #_(swap! status dissoc key)
  (cond (= :in-class key)
        (reset! classVarTable {})
        (= :in-subroutine key)
        (reset! subroutineVarTable {}))
  (swap! status dissoc key))

(defn set-var! [name properties]
  (let [varTable (cond (in-status? :in-subroutine) subroutineVarTable
                       (in-status? :in-class) classVarTable
                       :else (throw (RuntimeException.
                                      "set-var! 状态错误，不属于 class or subroutine")))
        name (if (map? name) (:data name) name)             ;name 可能是 {:type :name} 格式
        nameUppercase? (if-not (str/blank? name)
                         (Character/isUpperCase ^char (.charAt name 0))
                         false)
        existedInVarTable (get @varTable name)
        current-kind (:kind properties)
        ;对于 #{:class :var} 类型的 identifier 进行区分，大写开头为 Class，小写为对象名
        unknown-class-var-kind? (= #{:class :var} current-kind)
        current-kind (if unknown-class-var-kind?
                       (if nameUppercase? :class :var)
                       current-kind)
        properties (assoc properties :kind current-kind)]
    (if (or existedInVarTable
            (not (contains? #{:static :field :argument :var} current-kind)))
      (swap! varTable assoc name (merge existedInVarTable properties))
      ;对于 :kind :static/:field/:argument/:var 四种的分别计数，设置 index 为当前的 count
      (let [index (count (filter #(= current-kind (:kind %)) (vals @varTable)))]
        (swap! varTable assoc name (assoc properties :index index))))))

(defn get-var! [name]
  (let [varTable (cond (in-status? :in-subroutine) subroutineVarTable
                       (in-status? :in-class) classVarTable
                       :else (throw (RuntimeException. "get-var! 状态错误，不属于 class or subroutine")))]
    (get @varTable name nil)))

(defn- get-scanner
  "从文件中读入 .jack 程序，去除注释和空行，返回符号易识别的程序"
  [filename]
  (let [file-data (slurp filename)
        sc (Scanner. file-data)
        skip-commit-fn (fn []
                         (cond
                           (or (.hasNext sc "//") (.hasNext sc "///"))
                           (do (.nextLine sc) (recur))
                           ;多行注释开始
                           (or (.hasNext sc "/\\*")
                               (.hasNext sc "/\\*\\*"))
                           (do (set-status! :in-multiline-comment)
                               (.next sc) (recur))
                           ;处于多行注释中
                           (and (.hasNext sc)
                                (in-status? :in-multiline-comment)
                                (not (.hasNext sc "\\*/")))
                           (do (.next sc) (recur))
                           ;多行注释结束
                           (.hasNext sc "\\*/")
                           (do (unset-status! :in-multiline-comment)
                               (.next sc "\\*/") (recur))
                           (.hasNext sc) (.next sc)
                           :else nil))]
    (let [pure-data (->> (take-while (comp not nil?)
                                     (repeatedly skip-commit-fn))
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

(defn- token->node
  "将 Scanner 扫描的 token {:type :data} 转换为 XML Node
  当 type 为 identifier 时，从 classVarTable 和 subroutineVarTable 中
  扫描符号表并为标识符生成属性信息"
  [{:keys [type data]}]
  (if-let [info (get-var! data)]
    [(keyword type) info data]
    [(keyword type) data]))

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

(defn- top-ts [n] (str/join " " (mapv :data (take n @ts))))

(defn- push-ts [& head]
  (reset! ts (into @ts head))
  nil)

(defn- push-ts-warn [& head]
  (println (str "WARN: can't parse from: "
                (str/join " " (mapv :data (reverse head)))))
  (reset! ts (into @ts head))
  nil)

(defn- push-ts-warn-unset [status & head]
  (unset-status! status)
  (apply push-ts-warn head))

(defn- warn [message]
  (println (str "WARN: " message))
  nil)

(defn- type-and-contains? [type & data]
  #(and (= type %1) (contains? (set data) %2)))

(defn- type-and? [type data]
  #(and (= type %1) (= data %2)))

(defn- type? [type]
  (fn [t _] (= type t)))

(declare compileExpression)
(declare compileExpressionList)
(declare compileStatement)
(declare compileStatements)

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
      (let [nextVars (doall
                       (take-while (fn [[splitNode var2Node]]
                                     (if (check! [splitNode var2Node]
                                                 (type-and? :symbol ",")
                                                 (type? :identifier))
                                       true
                                       (do (push-ts var2Node splitNode) false)))
                                   (repeatedly (fn [] [(pop-ts) (pop-ts)]))))
            nextVarsNode (map second nextVars)
            endNode (pop-ts)]
        (when (check! [endNode] (type-and? :symbol ";"))
          (do
            (set-var! varNode {:kind (keyword (:data sfNode))
                               :type (:data typeNode)})
            (doseq [name nextVarsNode]
              (set-var! name {:kind (keyword (:data sfNode))
                              :type (:data typeNode)}))
            (into [:classVarDec]
                  (-> (tokens->nodes [sfNode typeNode varNode])
                      (into (tokens->nodes nextVarsNode))
                      (conj (token->node endNode)))))))
      (push-ts varNode typeNode sfNode))))

(defn- compileParameterList
  "编译参数列表，不包含 ()。结果可能为空，空时依旧返回无子节点 Node，而非 nil。
  empty or 'int a' or 'int a, boolean b'" []
  (let [varTypeNode (pop-ts)
        varNameNode (pop-ts)]
    (if (check! [varTypeNode varNameNode]
                [:or (type-and-contains? :keyword "int" "char" "boolean") (type? :identifier)]
                (type? :identifier))
      (let [nextTypeVars
            (doall
              (take-while (fn [[splitNode varType2Node varName2Node]]
                            (if (check! [splitNode varType2Node varName2Node]
                                        (type-and? :symbol ",")
                                        [:or (type-and-contains?
                                               :keyword "int" "char" "boolean")
                                         (type? :identifier)]
                                        (type? :identifier))
                              true
                              (do (push-ts varName2Node varType2Node splitNode) false)))
                          (repeatedly (fn [] [(pop-ts) (pop-ts) (pop-ts)]))))
            nextTypeVarsNodesRes (reduce (fn [agg item]
                                           (into agg (tokens->nodes item)))
                                         [] nextTypeVars)]
        (set-var! varNameNode {:kind :argument
                               :type (:data varTypeNode)})
        (doseq [[_ type name] nextTypeVars]
          (set-var! name {:kind :argument :type (:data type)}))
        (-> [:parameterList]
            (into (tokens->nodes [varTypeNode varNameNode]))
            (into nextTypeVarsNodesRes)))
      (do (push-ts varNameNode varTypeNode)
          [:parameterList]))))

(defn- compileVarDec
  "编译 var 声明，可能不存在，返回 nil
  var type varName, varName,.. ;" []
  (let [varTypeNode (pop-ts)
        typeNode (pop-ts)
        varNameNode (pop-ts)]
    (if (check! [varTypeNode typeNode varNameNode]
                (type-and? :keyword "var")
                [:or (type-and-contains?
                       :keyword "int" "char" "boolean") (type? :identifier)]
                (type? :identifier))
      (let [nextVars (doall (take-while (fn [[splitNode var2Node]]
                                          (if (check! [splitNode var2Node]
                                                      (type-and? :symbol ",")
                                                      (type? :identifier))
                                            true
                                            (do (push-ts var2Node splitNode) false)))
                                        (repeatedly (fn [] [(pop-ts) (pop-ts)]))))
            nextVarsNode (map second nextVars)              ;may nil
            endNode (pop-ts)]
        (if (check! [endNode] (type-and? :symbol ";"))
          (do
            (if (= :identifier (:type typeNode))
              (set-var! typeNode {:kind :class
                                  :type (:data typeNode)}))
            (set-var! varNameNode {:kind       :var
                                   :type       (:data typeNode)
                                   :is-defined true})
            (doseq [name nextVarsNode]
              (set-var! name {:kind       :var
                              :type       (:data typeNode)
                              :is-defined true}))
            (into [:varDec]
                  (-> (tokens->nodes [varTypeNode typeNode varNameNode])
                      (into (tokens->nodes nextVarsNode))
                      (conj (token->node endNode)))))
          (push-ts-warn endNode varNameNode typeNode varTypeNode)))
      (push-ts varNameNode typeNode varTypeNode))))

(defn- compileDo
  "编译 do 语句
  do subName ( expressionList ) ;
  do className/varName . subName ( expressionList ) ;" []
  (let [doNode (pop-ts)]
    (let [{c1Type :type :as varNameOrSubroutineName} (pop-ts)
          {c2Type :type c2Data :data} (peek-ts)]
      (if-not (= :identifier c1Type)
        (push-ts-warn varNameOrSubroutineName doNode)
        (cond
          (and (= :symbol c2Type)
               (= "(" c2Data))
          ;subName ( expressionList ) ;
          (let [_ (set-var! varNameOrSubroutineName {:kind :subroutine :is-using true})
                leftB (pop-ts)
                expressionList (compileExpressionList)
                rightB (pop-ts)
                endNode (pop-ts)]
            (if (and (check! [leftB rightB endNode]
                             (type-and? :symbol "(")
                             (type-and? :symbol ")")
                             (type-and? :symbol ";"))
                     (not (nil? expressionList)))
              (-> [:doStatement]
                  (into (tokens->nodes [doNode varNameOrSubroutineName leftB]))
                  (conj expressionList)
                  (into (tokens->nodes [rightB endNode])))
              (push-ts-warn endNode rightB leftB varNameOrSubroutineName doNode)))
          ;className/varName . subName ( expressionList ) ;
          (and (= :symbol c2Type) (= "." c2Data))
          (let [_ (set-var! varNameOrSubroutineName {:kind #{:class :var} :is-using true})
                point (pop-ts)
                subName (pop-ts)
                _ (set-var! subName {:kind :subroutine :is-using true})
                leftB (pop-ts)
                expressionList (compileExpressionList)
                rightB (pop-ts)
                endNode (pop-ts)]
            (if (and (check! [point subName leftB rightB endNode]
                             (type-and? :symbol ".")
                             (type? :identifier)
                             (type-and? :symbol "(")
                             (type-and? :symbol ")")
                             (type-and? :symbol ";"))
                     (not (nil? expressionList)))
              (-> [:doStatement]
                  (into (tokens->nodes [doNode varNameOrSubroutineName
                                        point subName leftB]))
                  (conj expressionList)
                  (into (tokens->nodes [rightB endNode])))
              (push-ts-warn endNode rightB leftB subName point
                            varNameOrSubroutineName doNode)))
          :else (push-ts-warn varNameOrSubroutineName doNode))))))

(defn- compileLet
  "编译 let 语句
  let varName = expression ;
  let varName [ expression ] = expression ;" []
  (let [letNode (pop-ts)
        varNameNode (pop-ts)
        _ (set-var! varNameNode {:kind :var :is-using true})
        equalOrLeftMidBruceNode (pop-ts)]
    (if (check! [letNode varNameNode equalOrLeftMidBruceNode]
                (type-and? :keyword "let")
                (type? :identifier)
                (type-and-contains? :symbol "=" "["))
      (if (= (:data equalOrLeftMidBruceNode) "=")
        ;let varName = expression ;
        (let [expressionRes (compileExpression)
              endNode (pop-ts)]
          (if (and (not (nil? expressionRes))
                   (check! [endNode] (type-and? :symbol ";")))
            (-> [:letStatement]
                (into (tokens->nodes [letNode varNameNode equalOrLeftMidBruceNode]))
                (conj expressionRes)
                (conj (token->node endNode)))
            (push-ts-warn endNode)))
        ;let varName [ expression ] = expression ;
        (let [expressionRes (compileExpression)
              rightMiddleBruceNode (pop-ts)
              equalNode (pop-ts)
              expression2Res (compileExpression)
              endNode (pop-ts)]
          (if (and expressionRes
                   expression2Res
                   (check! [rightMiddleBruceNode equalNode endNode]
                           (type-and? :symbol "]")
                           (type-and? :symbol "=")
                           (type-and? :symbol ";")))
            (-> [:letStatement]
                (into (tokens->nodes [letNode varNameNode equalOrLeftMidBruceNode]))
                (conj expressionRes)
                (into (tokens->nodes [rightMiddleBruceNode equalNode]))
                (conj expression2Res)
                (conj (token->node endNode)))
            (push-ts-warn endNode equalNode rightMiddleBruceNode))))
      (push-ts-warn equalOrLeftMidBruceNode varNameNode letNode))))

(defn- compileWhile
  "编译 while 语句
  while ( expression ) { statement }" []
  (let [while (pop-ts)
        leftBruce (pop-ts)
        expressionRes (compileExpression)
        rightBruce (pop-ts)
        leftBigBruce (pop-ts)
        statementRes (compileStatements)
        rightBigBruce (pop-ts)]
    (if (and (check! [while leftBruce rightBruce
                      leftBigBruce rightBigBruce]
                     (type-and? :keyword "while")
                     (type-and? :symbol "(")
                     (type-and? :symbol ")")
                     (type-and? :symbol "{")
                     (type-and? :symbol "}"))
             (not (nil? expressionRes))
             (not (nil? statementRes)))
      (-> [:whileStatement]
          (into (tokens->nodes [while leftBruce]))
          (conj expressionRes)
          (into (tokens->nodes [rightBruce leftBigBruce]))
          (conj statementRes)
          (conj (token->node rightBigBruce)))
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
          (into [:returnStatement]
                (tokens->nodes [return mayEnd])))
        (let [expressionRes (compileExpression)
              endNode (pop-ts)]
          (if (and (not (nil? expressionRes))
                   (check! [endNode] (type-and? :symbol ";")))
            (-> [:returnStatement]
                (conj (token->node return))
                (conj expressionRes)
                (conj (token->node endNode)))
            (push-ts-warn endNode return))))
      (push-ts-warn return))))

(defn- compileIf
  "编译 if 语句
  if ( expression ) { statements }
  (else { statements })?" []
  (let [ifNode (pop-ts)
        leftB (pop-ts)
        expressionRes (compileExpression)
        rightB (pop-ts)
        leftBigB (pop-ts)
        statementRes (compileStatements)
        rightBigB (pop-ts)]
    (if (and (check! [ifNode leftB rightB
                      leftBigB rightBigB]
                     (type-and? :keyword "if")
                     (type-and? :symbol "(")
                     (type-and? :symbol ")")
                     (type-and? :symbol "{")
                     (type-and? :symbol "}"))
             (not (nil? expressionRes))
             (not (nil? statementRes)))
      (if-not (let [{:keys [data type]} (peek-ts)]
                (and (= "else" data) (= :keyword type)))
        ;没有 else 的 if 语句
        (-> [:ifStatement]
            (into (tokens->nodes [ifNode leftB]))
            (conj expressionRes)
            (into (tokens->nodes [rightB leftBigB]))
            (conj statementRes)
            (conj (token->node rightBigB)))
        (let [elseNode (pop-ts)
              leftBigB2 (pop-ts)
              statement2Res (compileStatements)
              rightBigB2 (pop-ts)]
          (if (and (check! [elseNode leftBigB2 rightBigB2]
                           (type-and? :keyword "else")
                           (type-and? :symbol "{")
                           (type-and? :symbol "}"))
                   (not (nil? statement2Res)))
            ;包含 if 和 else 语句
            (-> [:ifStatement]
                (into (tokens->nodes [ifNode leftB]))
                (conj expressionRes)
                (into (tokens->nodes [rightB leftBigB]))
                (conj statementRes)
                (into (tokens->nodes [rightBigB elseNode leftBigB2]))
                (conj statement2Res)
                (conj (token->node rightBigB2)))
            (push-ts-warn rightBigB2 leftBigB2 elseNode))))
      (push-ts-warn rightBigB leftBigB rightB leftB ifNode))))

(defn- compileStatement
  "编译单条语句
  statement 包括 let/if/while/do/return Statement"
  []
  (set-status! :in-statement)
  (let [{:keys [type data]} (peek-ts)
        return
        (if-not (= :keyword type)
          nil
          (case data
            "let" (compileLet)
            "if" (compileIf)
            "while" (compileWhile)
            "do" (compileDo)
            "return" (compileReturn)
            (warn (str "can't parse statement from: " (top-ts 10)))))]
    (unset-status! :in-statement)
    return))

(defn- compileStatements
  "编译语句，不包括 {}
  statement* 可能为空，返回 nil" []
  (if-let [children (doall
                      (take-while
                        (comp not nil?)
                        (repeatedly compileStatement)))]
    (into [:statements] children)
    [:statements]))

(defn- compileSubroutine
  "编译方法、函数或构造函数
  function void main() {
    do a.b();
  }" []
  (set-status! :in-subroutine)
  (let [typeTypeNode (pop-ts)
        returnTypeNode (pop-ts)
        subroutineNameNode (pop-ts)
        leftSmallBruceNode (pop-ts)]
    (let [return
          (if (check! [typeTypeNode returnTypeNode subroutineNameNode leftSmallBruceNode]
                      (type-and-contains? :keyword "constructor" "function" "method")
                      [:or (type-and? :keyword "void") (type? :identifier)]
                      (type? :identifier)
                      (type-and? :symbol "("))
            (let [_ (do
                      (set-var! "this"
                                {:kind :argument
                                 :type (current-class-name)})
                      (set-var! subroutineNameNode
                                {:kind :subroutine
                                 ;这里的 type 最好是 paramType,returnType
                                 :type (:data returnTypeNode)
                                 :class (current-class-name)}))
                  ;空 parameterList 返回 nil
                  paramListRes (compileParameterList)
                  rightSmallBruceNode (pop-ts)
                  ;subroutineBody { varDec* statements }
                  leftBigBruceNode (pop-ts)
                  ;var 声明可能为 nil
                  allVarDecRes (doall (take-while (comp not nil?) (repeatedly compileVarDec)))
                  statementsRes (compileStatements)         ;statementsRes 可能为 nil
                  rightBigBruceNode (pop-ts)]
              (if (check! [leftBigBruceNode rightBigBruceNode rightSmallBruceNode]
                          (type-and? :symbol "{") (type-and? :symbol "}") (type-and? :symbol ")"))
                (let [subroutineBodyRes
                      (-> [:subroutineBody]
                          (conj (token->node leftBigBruceNode))
                          (into allVarDecRes)
                          (conj statementsRes)
                          (conj (token->node rightBigBruceNode)))]
                  (-> [:subroutineDec]
                      (into (tokens->nodes [typeTypeNode returnTypeNode
                                            subroutineNameNode leftSmallBruceNode]))
                      (conj paramListRes)
                      (conj (token->node rightSmallBruceNode))
                      (conj subroutineBodyRes)))
                ;并非本函数 pop 的数据由其自身确保压回到 ts
                (push-ts-warn rightBigBruceNode leftBigBruceNode rightSmallBruceNode)))
            (push-ts leftSmallBruceNode subroutineNameNode returnTypeNode typeTypeNode))]
      (unset-status! :in-subroutine)
      return)))

(defn- compileClass
  "编译整个类，如果不是类结构，返回 nil
  class className { classVarDec* subroutineDec* }" []
  (set-status! :in-class)
  (let [classNode (pop-ts)
        classNameNode (pop-ts)
        leftBigBruceNode (pop-ts)]
    (let [return
          (if (check! [classNode classNameNode leftBigBruceNode]
                      (type-and? :keyword "class") :identifier :symbol)
            (let [_ (do
                      (set-var! classNameNode {:kind :class
                                               :type (:data classNameNode)})
                      (set-class-name (:data classNameNode)))
                  all-classVarDec
                  (doall (take-while (comp not nil?) (repeatedly compileClassVarDec)))
                  all-subroutineDec
                  (doall (take-while (comp not nil?) (repeatedly compileSubroutine)))
                  rightBigBruceNode (pop-ts)]
              (-> [:class]
                  (into (tokens->nodes [classNode classNameNode leftBigBruceNode]))
                  (into all-classVarDec)
                  (into all-subroutineDec)
                  (conj (token->node rightBigBruceNode))))
            (push-ts-warn leftBigBruceNode classNameNode classNode))]
      (unset-class-name)
      (unset-status! :in-class)
      return)))

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
      [:term (token->node (pop-ts))]
      ;决定是否是 ( expression ) 或 unaryOp term
      (let [{:keys [type data]} (peek-ts)]
        (cond (and (= :symbol type) (= "(" data))
              (let [leftBruce (pop-ts)
                    expressionRes (compileExpression)
                    rightBruce (pop-ts)]
                (if (and (not (nil? expressionRes))
                         (check! [leftBruce rightBruce]
                                 (type-and? :symbol "(")
                                 (type-and? :symbol ")")))
                  (-> [:term]
                      (conj (token->node leftBruce))
                      (conj expressionRes)
                      (conj (token->node rightBruce)))
                  (push-ts-warn rightBruce leftBruce)))
              (and (= :symbol type) (contains? #{"-" "~"} data))
              (let [unary (pop-ts)
                    termRes (compileTerm)]
                (if (not (nil? termRes))
                  (-> [:term] (conj (token->node unary)) (conj termRes))
                  (push-ts-warn unary)))
              :else
              ;区分 varName
              ;    varName [ expression ],
              ;    subroutineName ( expressionList )
              ;    className/varName . subroutineName ( expressionList )
              (let [{c1Type :type :as varNameOrSubroutineName} (pop-ts)
                    {c2Type :type c2Data :data} (peek-ts)]
                (if-not (= :identifier c1Type)
                  ;可能是空 expressionList: let a = A.new()，此时读入的是 )
                  (push-ts varNameOrSubroutineName)
                  (cond
                    ;varName [ expression ]
                    (and (= :symbol c2Type) (= "[" c2Data))
                    (let [_ (set-var! varNameOrSubroutineName {:kind     :var
                                                               :is-using true})
                          leftBB (pop-ts)
                          expressionRes (compileExpression)
                          rightBB (pop-ts)]
                      (if (and (check! [leftBB rightBB]
                                       (type-and? :symbol "[")
                                       (type-and? :symbol "]"))
                               (not (nil? expressionRes)))
                        (-> [:term]
                            (into (tokens->nodes [varNameOrSubroutineName leftBB]))
                            (conj expressionRes)
                            (conj (token->node rightBB)))
                        (push-ts-warn rightBB leftBB varNameOrSubroutineName)))
                    (and (= :symbol c2Type) (= "(" c2Data))
                    ;subName ( expressionList )
                    (let [_ (set-var! varNameOrSubroutineName {:kind     :subroutine
                                                               :is-using true})
                          leftB (pop-ts)
                          expressionListRes (compileExpressionList)
                          rightB (pop-ts)]
                      (if (and (check! [leftB rightB]
                                       (type-and? :symbol "(")
                                       (type-and? :symbol ")"))
                               (not (nil? expressionListRes)))
                        (-> [:term]
                            (into (tokens->nodes [varNameOrSubroutineName leftB]))
                            (conj expressionListRes)
                            (conj (token->node rightB)))
                        (push-ts-warn rightB leftB varNameOrSubroutineName)))
                    (and (= :symbol c2Type) (= "." c2Data))
                    ;className/varName . subName ( expressionList )
                    (let [point (pop-ts)
                          subName (pop-ts)
                          leftB (pop-ts)
                          _ (set-var! varNameOrSubroutineName {:kind     #{:class :var}
                                                               :is-using true})
                          _ (set-var! subName {:kind :subroutine :is-using true})
                          expressionListRes (compileExpressionList)
                          rightB (pop-ts)]
                      (if (and (check! [point subName leftB rightB]
                                       (type-and? :symbol ".")
                                       (type? :identifier)
                                       (type-and? :symbol "(")
                                       (type-and? :symbol ")"))
                               (not (nil? expressionListRes)))
                        (-> [:term]
                            (into (tokens->nodes [varNameOrSubroutineName
                                                  point subName leftB]))
                            (conj expressionListRes)
                            (conj (token->node rightB)))
                        (push-ts-warn rightB leftB subName point
                                      varNameOrSubroutineName)))
                    :else                                   ;varName
                    (do
                      (set-var! varNameOrSubroutineName {:kind :var :is-using true})
                      [:term (token->node varNameOrSubroutineName)])))))))))

(defn- compileExpression
  "编译表达式
  term (op term)*" []
  (let [termRes (compileTerm)
        isOP? #(check! [%] (type-and-contains?
                             :symbol "+" "-" "*" "/" "&" "|" "<" ">" "="))]
    (let [return
          (if (not (nil? termRes))
            (let [rest-op-term
                  (doall (take-while (fn [[op term]]
                                       (if (nil? term) (do (push-ts op) nil) true))
                                     (repeatedly (fn [] (let [a (pop-ts)]
                                                          (if (isOP? a) [a (compileTerm)] [a nil]))))))
                  ;may nil
                  opRes-terms (reduce (fn [agg [op term]]
                                        (conj agg (token->node op) term))
                                      [] rest-op-term)]
              (-> [:expression]
                  (conj termRes)
                  (into opRes-terms)))
            nil)]
      return)))

(defn- compileExpressionList
  "编译逗号分隔符分割的表达式列表（可空）
  expression
  expression, expression" []
  (let [expRes (compileExpression)]
    (if (not (nil? expRes))
      (let [nextExps (doall
                       (take-while
                         (fn [[sp expResInner]]
                           (if (not (nil? expResInner)) true (do (push-ts sp) nil)))
                         (repeatedly (fn [] (let [{:keys [type data] :as a} (pop-ts)]
                                              (if (and (= :symbol type) (= "," data))
                                                [a (compileExpression)] [a nil]))))))
            nextExpNodesRes (reduce (fn [agg [sp exp]]
                                      (conj agg (token->node sp) exp))
                                    [] nextExps)]           ;may nil
        (-> [:expressionList]
            (conj expRes)
            (into nextExpNodesRes)))
      [:expressionList])))

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
  doStatement/expression/term/expressionList> 带子元素节点

  注意，因为过程依赖于副作用，因此所有 lazy-seq 必须展开，即 take-while 必须 doall"
  [token-stream]
  (reset! ts (apply list token-stream))
  (do-compilation))

;.\tools\JackCompiler.bat .\projects\11\Seven
(defn do-test []
  (let [
        file "../projects/11/Average/Main.jack"
        file "../projects/11/Seven/Main.jack"
        input (Paths/get file (into-array [""]))
        pure-name (-> (str (.getFileName input)) (str/split #"\.") first)
        output (.resolve (.getParent input) (str pure-name ".xml"))]
    (->> (do-scan-token file)
         (parser)
         (save-xml-to (str output)))))
