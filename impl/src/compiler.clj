(ns compiler
  (:require [clojure.data.xml :refer [emit sexp-as-element]]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.nio.file Paths Files Path)
           (java.util Scanner)
           (java.util.regex Pattern)))

(declare set-status!)
(declare in-status?)
(declare unset-status!)
(declare get-var!)
(declare classVarTable)
(declare subroutineVarTable)
(declare compileExpression)
(declare compileExpressionList)
(declare compileStatement)
(declare compileStatements)

(defonce debug (atom false))

(defn- log [& objs]
  (if @debug (apply println objs)))

;;;;;;;;;;;;;;;;;; token scanner and token op ;;;;;;;;;;;;;;;;;;

(defn- get-scanner
  "从文件中读入 .jack 程序，去除注释和空行，返回符号易识别的程序"
  [filename]
  (unset-status! :in-multiline-comment)
  (let [file-data (slurp filename)
        sc (Scanner. file-data)
        skip-commit-fn (fn []
                         (cond
                           (or (.hasNext sc "//") (.hasNext sc "///"))
                           (do (log "skip comment line: " (.nextLine sc))
                               (recur))
                           ;多行注释开始
                           (or (.hasNext sc "/\\*")
                               (.hasNext sc "/\\*\\*"))
                           (do (set-status! :in-multiline-comment)
                               (log "reading multi comment: " (.next sc))
                               (recur))
                           ;处于多行注释中
                           (and (.hasNext sc)
                                (in-status? :in-multiline-comment)
                                (not (.hasNext sc "\\*/")))
                           (do (.next sc) (recur))
                           ;多行注释结束
                           (.hasNext sc "\\*/")
                           (do (unset-status! :in-multiline-comment)
                               (log "end of multi comment: " (.next sc "\\*/"))
                               (recur))
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
      (log "scan non comment programs:" "\n " easy-scan-data)
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
          (.hasNext sc "\\~"))
      {:type :symbol :data (.next sc)}
      (.hasNextInt sc)
      {:type :integerConstant :data (.nextInt sc)}
      (.hasNext sc id-pattern)
      {:type :identifier :data (.next sc id-pattern)}
      (.hasNext sc str-start-pattern)
      {:type :stringConstant :data (-> (.findInLine sc (Pattern/compile "\".*?\""))
                                       (str/replace "\"" "")
                                       (str/replace " { " "{")
                                       (str/replace " } " "}")
                                       (str/replace " [ " "[")
                                       (str/replace " ) " ")")
                                       (str/replace " ( " "(")
                                       (str/replace " ] " "]")
                                       (str/replace " . " ".")
                                       (str/replace " , " ",")
                                       (str/replace " ; " ";")
                                       (str/replace " + " "+")
                                       (str/replace " = " "=")
                                       (str/replace " * " "*")
                                       (str/replace " / " "/")
                                       (str/replace " & " "&")
                                       (str/replace " | " "|")
                                       (str/replace " < " "<")
                                       (str/replace " > " ">")
                                       (str/replace " = " "=")
                                       (str/replace " ~ " "~")
                                       (str/replace " - " "-"))}
      :else
      (do
        (if (.hasNext sc)
          (println "no rule to parse next: " (.next sc))
          (println "scan token done!")) nil))))

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
    [(keyword type)
     (if (:kind info)
       (assoc info :kind (str/replace-first (str (:kind info)) ":" ""))
       info) data]
    [(keyword type) data]))

(defn- tokens->nodes [ts]
  (mapv token->node (filterv (comp not nil?) ts)))

(defn- is-token-data-upper? [{:keys [type data]}]
  (and (= :identifier type)
       (if-not (str/blank? data)
         (Character/isUpperCase ^char (.charAt data 0))
         false)))

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

;;;;;;;;;;;;;;;;;; compile data-structure ;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;; program status operation ;;;;;;;;;;;;;;;;;;
(defonce status (atom {;用于标记当前 Scanner 是否处于多行注释中(可能为单行,/** */)
                       :in-multiline-comment false
                       ;用于将当前标识符插入到对应的 hashMap 中
                       :in-class             false
                       :in-subroutine        false
                       :in-statement         false
                       :current-class-name   nil}))

(defn- in-status? [key]
  (not (nil? (get @status key))))

(defn- set-status! [key]
  (cond (= :in-class key)
        (reset! classVarTable {})
        (= :in-subroutine key)
        (reset! subroutineVarTable {}))
  (swap! status assoc key true))

(defn- set-class-name [name]
  (swap! status assoc :current-class-name name))

(defn- unset-class-name []
  (swap! status dissoc :current-class-name))

(defn- current-class-name []
  (:current-class-name @status))

(defn- unset-status! [key]
  #_(swap! status dissoc key)
  (cond (= :in-class key)
        (reset! classVarTable {})
        (= :in-subroutine key)
        (reset! subroutineVarTable {}))
  (swap! status dissoc key))

;;;;;;;;;;;;;;;;;; varTable operation ;;;;;;;;;;;;;;;;;;

(defonce classVarTable (atom {}))

(defonce subroutineVarTable (atom {}))

(defn- set-var!
  "写入符号 Node 或符号字符串和其属性 properties map 信息到符号表
  重复写入同样的符号看做增量更新"
  ([name properties] (set-var! name properties nil))
  ([name properties useVarTable]
   (let [varTable (or useVarTable
                      (cond (in-status? :in-subroutine) subroutineVarTable
                            (in-status? :in-class) classVarTable
                            :else (throw (RuntimeException.
                                           "set-var! 状态错误，不属于 class or subroutine"))))
         name (if (map? name) (:data name) name)            ;name 可能是 {:type :name} 格式
         nameUppercase? (if-not (str/blank? name)
                          (Character/isUpperCase ^char (.charAt name 0))
                          false)
         existedInVarTable (get @varTable name)
         current-kind (or (:kind properties) (:kind existedInVarTable))
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
         (swap! varTable assoc name (assoc properties :index index)))))))

(defn- set-var-careful!
  "设置变量属性，如果不存在，则设置到上一级别（如果存在的话），
  比如 subroutine 不存在设置到 class varTable 中
  而如果当前即为 class，不存在则直接报错，因为不存在"
  [name properties]
  (let [name (if (map? name) (:data name) name)]
    (cond (in-status? :in-subroutine)
          (cond (get @subroutineVarTable name)
                (set-var! name properties subroutineVarTable)
                (get @classVarTable name)
                (set-var! name properties classVarTable)
                :else (set-var! name properties))
          (in-status? :in-class)
          (set-var! name properties classVarTable))))

(defn- get-var!
  "获取符号 Node 或者符号字符串的符号表数据
  如果在 subroutine 中，则依次从 subroutine 和 class varTable 中查找
  如果仅在 class 中，则仅从 class varTable 中查找"
  [name]
  (cond (in-status? :in-subroutine)
        (or (if (map? name)
              (get @subroutineVarTable (:data name) nil)
              (get @subroutineVarTable name nil))
            (if (map? name)
              (get @classVarTable (:data name) nil)
              (get @classVarTable name nil)))
        (in-status? :in-class)
        (if (map? name)
          (get @classVarTable (:data name) nil)
          (get @classVarTable name nil))
        :else (throw (RuntimeException.
                       "不在 subroutine 或 class 中! get-var! 失败"))))

(defn- count-class-varTable-field
  "统计 classVarTable 的 field 类型符号个数，用于分配内存" []
  (count (filter #(= :field (:kind (second %))) @classVarTable)))

(defn- count-subroutine-varTable-var
  "统计 subroutineTable 的 var 类型符号个数" []
  (count (filter #(= :var (:kind (second %))) @subroutineVarTable)))

;;;;;;;;;;;;;;;;;; gen vm command operation ;;;;;;;;;;;;;;;;;;

(defonce vm-cmds (atom []))

(defonce temp-cmds (atom {}))

(defn- write-format [& fmt]
  (swap! vm-cmds conj (apply format fmt)))

(defn- write-formats [cmds]
  (doseq [cmd cmds] (when cmd (write-format cmd))))

(defn- count-children
  "统计 Node 二级节点标签名为特定关键字的个数，比如
  [:expList [:exp xx] [:exp xx]] :exp 2"
  [node kw]
  (if (vector? node)
    (count (filter #(and (vector? %)
                         (= kw (first %)))
                   (next node)))
    0))

(defn- opToken->cmds [{:keys [data type] :as op}]
  (if (= :symbol type)
    (case data
      "+" ["add"]
      "-" ["sub"]
      "&" ["and"]
      "|" ["or"]
      "<" ["lt"]
      ">" ["gt"]
      "=" ["eq"]
      "*" ["call Math.multiply 2"]
      "/" ["call Math.divide 2"]
      (do (println "WARN:" "can't convert token" op "to cmds") []))
    (do (println "WARN:" "can't convert token" op "to cmds") [])))

(defn- set-temp [anything cmds]
  (swap! temp-cmds assoc (hash anything) cmds))

(defn- get-temp [anything]
  (get @temp-cmds (hash anything)))

(defn- pop-temp [anything]
  (if-let [res (get @temp-cmds (hash anything))]
    (do (swap! temp-cmds dissoc (hash anything))
        res)
    nil))

(defn id-kind->vm-str
  "将变量表 identifier 类别 kind 转换为 VM 指令类别"
  [kind]
  (case kind
    :var "local"
    :argument "argument"
    :static "static"
    :field "this"
    (do (println "WARN: can't turn identifier kind" kind "to vm keyword") nil)))

(defn- cache-nested-cmds-return-ast
  "缓存 VM 命令到 cache-cmds 中并返回 AST"
  [cmds return]
  (set-temp return (doall (flatten cmds)))
  return)

;;;;;;;;;;;;;;;;;; compiler core ;;;;;;;;;;;;;;;;;;

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
            (set-var! varNode {:kind (keyword (:data sfNode)) :type (:data typeNode)}
                      classVarTable)
            (doseq [name nextVarsNode]
              (set-var! name {:kind (keyword (:data sfNode)) :type (:data typeNode)}
                        classVarTable))
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
        (set-var! varNameNode {:kind :argument :type (:data varTypeNode)}
                  subroutineVarTable)
        (doseq [[_ type name] nextTypeVars]
          (set-var! name {:kind :argument :type (:data type)} subroutineVarTable))
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
          (do                                               ;更新符号表：标记某类已使用、某变量已定义，type 为其类型
            (when (= :identifier (:type typeNode))
              (set-var! typeNode {:kind :class :type (:data typeNode)} classVarTable))
            (set-var! varNameNode {:kind       :var
                                   :type       (:data typeNode)
                                   :is-defined true} subroutineVarTable)
            (doseq [name nextVarsNode]
              (set-var! name {:kind       :var
                              :type       (:data typeNode)
                              :is-defined true} subroutineVarTable))
            (into [:varDec]
                  (-> (tokens->nodes [varTypeNode typeNode varNameNode])
                      (into (tokens->nodes nextVarsNode))
                      (conj (token->node endNode)))))
          (push-ts-warn endNode varNameNode typeNode varTypeNode)))
      (push-ts varNameNode typeNode varTypeNode))))

(defn- compileDo
  "编译 do 语句
  do subName ( expressionList ) ;
  do className/varName . subName ( expressionList ) ;
  生成的 VM 指令保存在 temp-cmds 中。
  do 生成的 VM 指令中，expressionList 指令从 vm-temp 缓存列表中获取，
  其在编译 expressionList、expression、term 时已写入此 vm-temp 缓存
  现在读出，之后对 className.subName 执行方法调用，注意如果是方法则需要
  额外压入 this 并增加参数计数。" []
  (let [doNode (pop-ts)]
    (let [{c1Type :type :as varNameOrSubroutineName} (pop-ts)
          {c2Type :type c2Data :data} (peek-ts)]
      (if-not (= :identifier c1Type)
        (push-ts-warn varNameOrSubroutineName doNode)
        (cond
          (and (= :symbol c2Type)
               (= "(" c2Data))
          ;func/methodName ( expressionList ) ;
          (let [;更新子程序名：func/methodName
                _ (set-var! varNameOrSubroutineName {:kind :subroutine :is-using true}
                            classVarTable)
                leftB (pop-ts)
                expressionList (compileExpressionList)
                ;生成 VM 指令，先处理 expressionList（上述编译已处理）再将其结果压入后
                ;生成 call ClassName.functionB paramCount 调用
                ;注意如果是方法，要额外压入 this，扩充参数列表
                classNameStr (current-class-name)
                subroutineNameStr (:data varNameOrSubroutineName)
                ;这里一定是方法，因为哪怕是自己类的函数也需要 ClassName.subroutineName 调用
                ;is-method? (= "method" (:type (or (get-var! subroutineNameStr)
                ;{:type "method"})))
                is-method? true
                paramCountStr (+ (count-children expressionList :expression)
                                 (if is-method? 1 0))
                expCmds (pop-temp expressionList)
                rightB (pop-ts)
                endNode (pop-ts)]
            (if (and (check! [leftB rightB endNode]
                             (type-and? :symbol "(")
                             (type-and? :symbol ")")
                             (type-and? :symbol ";"))
                     (not (nil? expressionList)))
              (cache-nested-cmds-return-ast
                [(if is-method? ["push pointer 0"] [])
                 expCmds
                 (format "call %s.%s %d" classNameStr subroutineNameStr paramCountStr)
                 "pop temp 0"]
                (-> [:doStatement]
                    (into (tokens->nodes [doNode varNameOrSubroutineName leftB]))
                    (conj expressionList)
                    (into (tokens->nodes [rightB endNode]))))
              (push-ts-warn endNode rightB leftB varNameOrSubroutineName doNode)))
          ;className/varName . funName/methodName ( expressionList ) ;
          (and (= :symbol c2Type) (= "." c2Data))
          (let [point (pop-ts)
                subName (pop-ts)
                leftB (pop-ts)
                ;更新子程序或变量名和其方法名：ClassA.methodA & objectA.methodA
                _ (set-var-careful! varNameOrSubroutineName {:is-using true})
                _ (set-var! subName {:kind :subroutine :is-using true} classVarTable)
                expressionList (compileExpressionList)
                ;生成 VM 指令，先处理 expressionList（上述编译已处理）再将其结果压入后
                ;生成 call ClassName.subroutineName paramCount 调用
                ;如果第一个参数是对象，则需要扩充参数列表，找到对象变量位置压入其 this 基址
                isFirstClass? (is-token-data-upper? varNameOrSubroutineName)
                ;第一个参数是对象，则一定第二个是方法，反之一定是函数
                ;isSecondMethod? (= "method" (:type (get-var! subName)))
                isSecondMethod? (not isFirstClass?)
                {:keys [kind type index]} (get-var! varNameOrSubroutineName)
                classNameStr (if isFirstClass? (:data varNameOrSubroutineName) type)
                subroutineNameStr (:data subName)
                paramCountStr (+ (count-children expressionList :expression)
                                 (if isSecondMethod? 1 0))
                expCmds (pop-temp expressionList)
                rightB (pop-ts)
                endNode (pop-ts)]
            (if (and (check! [point subName leftB rightB endNode]
                             (type-and? :symbol ".")
                             (type? :identifier)
                             (type-and? :symbol "(")
                             (type-and? :symbol ")")
                             (type-and? :symbol ";"))
                     (not (nil? expressionList)))
              (cache-nested-cmds-return-ast
                ;如果是方法调用，找到对象的引用，对象可能是局部变量或参数（子程序符号表）或类实例（类符号表）
                [(if isFirstClass? []
                                   [(format "push %s %d" (id-kind->vm-str kind) index)])
                 ;其余参数压入堆栈（实际是表达式计算指令，计算结果会将表达式最后结果放在堆栈顶
                 expCmds
                 (format "call %s.%s %d" classNameStr subroutineNameStr paramCountStr)
                 "pop temp 0"]
                (-> [:doStatement]
                    (into (tokens->nodes [doNode varNameOrSubroutineName
                                          point subName leftB]))
                    (conj expressionList)
                    (into (tokens->nodes [rightB endNode]))))
              (push-ts-warn endNode rightB leftB subName point
                            varNameOrSubroutineName doNode)))
          :else (push-ts-warn varNameOrSubroutineName doNode))))))

(defn- compileLet
  "编译 let 语句
  let varName = expression ;
  let varName [ expression ] = expression ;
  VM 指令压入 cache-cmds 中
  对于第一种情况，解析 expression 并将结果压入堆栈
  对于第二种情况，先解析索引，计算偏移然后将结果压入堆栈" []
  (let [letNode (pop-ts)
        varNameNode (pop-ts)
        _ (set-var-careful! varNameNode {:is-using true})   ;因为可能在父级符号表
        equalOrLeftMidBruceNode (pop-ts)]
    (if (check! [letNode varNameNode equalOrLeftMidBruceNode]
                (type-and? :keyword "let")
                (type? :identifier)
                (type-and-contains? :symbol "=" "["))
      (if (= (:data equalOrLeftMidBruceNode) "=")
        ;let varName = expression ;
        (let [expressionRes (compileExpression)
              expCmds (pop-temp expressionRes)
              {:keys [index kind]} (get-var! varNameNode)
              endNode (pop-ts)]
          (if (and (not (nil? expressionRes))
                   (check! [endNode] (type-and? :symbol ";")))
            (cache-nested-cmds-return-ast
              [expCmds (format "pop %s %d" (id-kind->vm-str kind) index)]
              (-> [:letStatement]
                  (into (tokens->nodes [letNode varNameNode equalOrLeftMidBruceNode]))
                  (conj expressionRes)
                  (conj (token->node endNode))))
            (push-ts-warn endNode)))
        ;let varName [ expression ] = expression ;
        (let [expressionRes (compileExpression)
              expCmds (pop-temp expressionRes)
              rightMiddleBruceNode (pop-ts)
              equalNode (pop-ts)
              expression2Res (compileExpression)
              exp2Cmds (pop-temp expression2Res)
              {:keys [index kind]} (get-var! varNameNode)
              endNode (pop-ts)]
          (if (and expressionRes
                   expression2Res
                   (check! [rightMiddleBruceNode equalNode endNode]
                           (type-and? :symbol "]")
                           (type-and? :symbol "=")
                           (type-and? :symbol ";")))
            (cache-nested-cmds-return-ast
              ;压入索引，压入变量基址，计算得到偏移
              ;结果保存到 temp，弹出基址更新 that，压入 temp 更新内存
              [expCmds (format "push %s %d" (id-kind->vm-str kind) index)
               "add" exp2Cmds
               "pop temp 0" "pop pointer 1" "push temp 0" "pop that 0"]
              (-> [:letStatement]
                  (into (tokens->nodes [letNode varNameNode equalOrLeftMidBruceNode]))
                  (conj expressionRes)
                  (into (tokens->nodes [rightMiddleBruceNode equalNode]))
                  (conj expression2Res)
                  (conj (token->node endNode))))
            (push-ts-warn endNode equalNode rightMiddleBruceNode))))
      (push-ts-warn equalOrLeftMidBruceNode varNameNode letNode))))

(defn- compileWhile
  "编译 while 语句
  while ( expression ) { statements }
  计算结果保存在 cache-cmds 中
  生成 VM 代码：
  label l1
  ~(expression)
  if-goto l2
  statements
  goto l1
  label l2" []
  (let [mark (-> (random-uuid) str (str/split #"-") first)
        className (current-class-name)
        l1Label (str className "_WHILE_" "AGAIN_" mark)
        l2Label (str className "_WHILE_" "BREAK_" mark)
        while (pop-ts)
        leftBruce (pop-ts)
        expressionRes (compileExpression)
        condCmds (pop-temp expressionRes)
        rightBruce (pop-ts)
        leftBigBruce (pop-ts)
        statementsRes (compileStatements)
        statementsCmds (pop-temp statementsRes)
        rightBigBruce (pop-ts)]
    (if (and (check! [while leftBruce rightBruce
                      leftBigBruce rightBigBruce]
                     (type-and? :keyword "while")
                     (type-and? :symbol "(")
                     (type-and? :symbol ")")
                     (type-and? :symbol "{")
                     (type-and? :symbol "}"))
             (not (nil? expressionRes))
             (not (nil? statementsRes)))
      (cache-nested-cmds-return-ast
        [(str "label " l1Label)
         condCmds "not"
         (str "if-goto " l2Label)
         statementsCmds
         (str "goto " l1Label)
         (str "label " l2Label)]
        (-> [:whileStatement]
            (into (tokens->nodes [while leftBruce]))
            (conj expressionRes)
            (into (tokens->nodes [rightBruce leftBigBruce]))
            (conj statementsRes)
            (conj (token->node rightBigBruce))))
      (push-ts-warn rightBigBruce leftBigBruce rightBruce
                    leftBruce while))))

(defn- compileReturn
  "编译 return 语句
  return expression ;
  return ;
  生成的 VM 指令保存在 temp-cmds 中" []
  (let [return (pop-ts)
        {:keys [type data] :as mayEnd} (peek-ts)]
    (if (check! [return] (type-and? :keyword "return"))
      (if (and (= type :symbol) (= data ";"))
        (do
          (pop-ts)
          (cache-nested-cmds-return-ast
            ["push constant 0" "return"]                    ;生成 VM 指令
            (into [:returnStatement]
                  (tokens->nodes [return mayEnd]))))
        (let [expressionRes (compileExpression)
              ;生成 VM 指令，弹出表达式结果
              expressionCmd (pop-temp expressionRes)
              endNode (pop-ts)]
          (if (and (not (nil? expressionRes))
                   (check! [endNode] (type-and? :symbol ";")))
            (cache-nested-cmds-return-ast
              [expressionCmd "return"]
              (-> [:returnStatement]
                  (conj (token->node return))
                  (conj expressionRes)
                  (conj (token->node endNode))))
            (push-ts-warn endNode return))))
      (push-ts-warn return))))

(defn- compileIf
  "编译 if 语句
  if ( expression ) { statements }
  (else { statements })?
  计算结果保存在 cache-cmds 中
  先计算 ~expression，压入 if-goto L1
  label L2 statements
  label L1 else statements
  " []
  (let [mark (-> (random-uuid) str (str/split #"-") first)
        className (current-class-name)
        l1Label (str className "_IF_" "TRUE_" mark)
        l2Label (str className "_IF_" "FALSE_" mark)
        endLabel (str className "_IF_" "END_" mark)
        ifNode (pop-ts)
        leftB (pop-ts)
        expressionRes (compileExpression)
        condCmds (pop-temp expressionRes)
        rightB (pop-ts)
        leftBigB (pop-ts)
        statementRes (compileStatements)
        trueCmds (pop-temp statementRes)
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
        (cache-nested-cmds-return-ast                       ;没有 else 的 if 语句
          [condCmds
           (str "if-goto " l1Label)
           (str "goto " l2Label)
           (str "label " l1Label)
           trueCmds
           (str "label " l2Label)]
          (-> [:ifStatement]
              (into (tokens->nodes [ifNode leftB]))
              (conj expressionRes)
              (into (tokens->nodes [rightB leftBigB]))
              (conj statementRes)
              (conj (token->node rightBigB))))
        (let [elseNode (pop-ts)
              leftBigB2 (pop-ts)
              statement2Res (compileStatements)
              falseCmds (pop-temp statement2Res)
              rightBigB2 (pop-ts)]
          (if (and (check! [elseNode leftBigB2 rightBigB2]
                           (type-and? :keyword "else")
                           (type-and? :symbol "{")
                           (type-and? :symbol "}"))
                   (not (nil? statement2Res)))
            (cache-nested-cmds-return-ast                   ;包含 if 和 else 语句
              [condCmds
               (str "if-goto " l1Label)
               (str "goto " l2Label)
               (str "label " l1Label)
               trueCmds
               (str "goto " endLabel)
               (str "label " l2Label)
               falseCmds
               (str "label " endLabel)]
              (-> [:ifStatement]
                  (into (tokens->nodes [ifNode leftB]))
                  (conj expressionRes)
                  (into (tokens->nodes [rightB leftBigB]))
                  (conj statementRes)
                  (into (tokens->nodes [rightBigB elseNode leftBigB2]))
                  (conj statement2Res)
                  (conj (token->node rightBigB2))))
            (push-ts-warn rightBigB2 leftBigB2 elseNode))))
      (push-ts-warn rightBigB leftBigB rightB leftB ifNode))))

(defn- compileStatement
  "编译单条语句
  statement 包括 let/if/while/do/return Statement
  其生成的 VM 指令保存在 cache-cmd 中"
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
  statement* 可能为空，返回 nil
  其生成的 VM 指令保存在 cache-cmd 中" []
  (if-let [children (doall
                      (take-while
                        (comp not nil?)
                        (repeatedly compileStatement)))]
    (cache-nested-cmds-return-ast
      (mapv #(get-temp %) children)
      (into [:statements] children))
    [:statements]))

(defn- compileSubroutine
  "编译方法、函数或构造函数
  function void main() {
    do a.b();
  }
  如果在 function 中，无需特殊要求。
  如果在 method 中，保存 this 到符号表，函数指令参数长度 + 1，设置 this
  如果在 constructor 中，保存 this 到符号表，分配对象内存，设置 this" []
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
            (let [is-constructor? (= (:data typeTypeNode) "constructor")
                  is-method? (= (:data typeTypeNode) "method")
                  _ (set-var! subroutineNameNode            ;更新子程序符号表，类型 type 为 f/c/m
                              {:kind :subroutine :type (:data typeTypeNode)}
                              classVarTable)
                  _ (when is-method?
                      (set-var! "this" {:kind :argument :type (current-class-name)}
                                subroutineVarTable))
                  ;空 parameterList 返回 nil
                  paramListRes (compileParameterList)
                  ;写入指令 function ClassN.methodN 3
                  ;等待 statements 扫描结束后统计局部变量个数
                  funcNameCmd (format "function %s.%s %s"
                                      (current-class-name) (:data subroutineNameNode) "%d")
                  ;分配对象内存并正确指向 this
                  initThisCmds (cond is-constructor?
                                     [(format "push constant %d"
                                              (count-class-varTable-field))
                                      "call Memory.alloc 1"
                                      "pop pointer 0"]
                                     is-method?
                                     ["push argument 0" "pop pointer 0"]
                                     :else [])
                  rightSmallBruceNode (pop-ts)
                  ;subroutineBody { varDec* statements }
                  leftBigBruceNode (pop-ts)
                  ;var 声明可能为 nil
                  allVarDecRes (doall (take-while (comp not nil?) (repeatedly compileVarDec)))
                  statementsRes (compileStatements)         ;statementsRes 可能为 nil
                  statementsCmds (pop-temp statementsRes)
                  rightBigBruceNode (pop-ts)
                  ;写入命令到 vm-cmds
                  _ (write-formats (flatten [(format funcNameCmd
                                                     (count-subroutine-varTable-var))
                                             initThisCmds
                                             statementsCmds]))]
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
  (set-status! :in-class)                                   ;设置进入 class 状态
  (reset! vm-cmds [])                                       ;重置 VM 指令输出
  (reset! temp-cmds {})
  (let [classNode (pop-ts)
        classNameNode (pop-ts)
        leftBigBruceNode (pop-ts)]
    (let [return
          (if (check! [classNode classNameNode leftBigBruceNode]
                      (type-and? :keyword "class") :identifier :symbol)
            (let [_ (do                                     ;更新当前 className 标记，添加 className 符号
                      (set-var! classNameNode {:kind :class
                                               :type (:data classNameNode)}
                                classVarTable)
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
      (unset-class-name)                                    ;取消当前 className 标记
      (unset-status! :in-class)                             ;退出在 class 中的状态
      return)))

(defn- compileTerm
  "编译 term，包括对标识符字元区分变量、数组和子程序调用
  包括 integer/string/keywordConstant,
  varName, varName [ expression ], subroutineCall,
  ( expression ), unaryOp term
  对于 int/keywordConstant，VM 指令生成 push constant x
  对于 stringConstant，VM 指令生成 String.new(length) 调用
  对于 (expression)，VM 指令保存 expression 执行结果
  对于 unaryOp term，VM 指令先处理 term，后处理 unaryOp
  " []
  (let [{:keys [type data] :as next-check} (peek-ts)]
    (cond
      ;integerConstant
      (= :integerConstant type)
      (cache-nested-cmds-return-ast
        [(format "push constant %d" data)]
        [:term (token->node (pop-ts))])
      ;stringConstant
      (= :stringConstant type)
      (cache-nested-cmds-return-ast
        [(str "push constant " (count data))
         "call String.new 1"
         (reduce (fn [agg char]
                   (conj agg (format "push constant %d" (int char))
                         "call String.appendChar 2"))
                 [] (.toCharArray data))]
        [:term (token->node (pop-ts))])
      ;keywordConstant
      (check!
        [next-check]
        (type-and-contains? :keyword "true" "false" "null" "this"))
      (cache-nested-cmds-return-ast
        (case data
          "true" ["push constant 0" "not"]
          ("false" "null") ["push constant 0"]
          "this" ["push pointer 0"])
        [:term (token->node (pop-ts))])
      ;( expression )
      (and (= :symbol type) (= "(" data))
      (let [leftBruce (pop-ts)
            expressionRes (compileExpression)
            rightBruce (pop-ts)]
        (if (and (not (nil? expressionRes))
                 (check! [leftBruce rightBruce]
                         (type-and? :symbol "(")
                         (type-and? :symbol ")")))
          (cache-nested-cmds-return-ast
            (pop-temp expressionRes)
            (-> [:term]
                (conj (token->node leftBruce))
                (conj expressionRes)
                (conj (token->node rightBruce))))
          (push-ts-warn rightBruce leftBruce)))
      ;unaryOp term
      (and (= :symbol type) (contains? #{"-" "~"} data))
      (let [unary (pop-ts)
            termRes (compileTerm)]
        (if (not (nil? termRes))
          (cache-nested-cmds-return-ast
            [(pop-temp termRes) (if (= data "-") "neg" "not")]
            (-> [:term] (conj (token->node unary)) (conj termRes)))
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
            (let [_ (set-var-careful! varNameOrSubroutineName {:is-using true})
                  leftBB (pop-ts)
                  expressionRes (compileExpression)
                  ;计算 expression 得到目标偏移
                  ;获得 varName 在符号表的偏移，计算得到数组目标元素位置
                  expCmds (pop-temp expressionRes)
                  {:keys [kind index]} (get-var! varNameOrSubroutineName)
                  finalCmds (flatten [expCmds
                                      (format "push %s %d" (id-kind->vm-str kind) index)
                                      "add"
                                      "pop pointer 1"
                                      "push that 0"])
                  rightBB (pop-ts)]
              (if (and (check! [leftBB rightBB]
                               (type-and? :symbol "[")
                               (type-and? :symbol "]"))
                       (not (nil? expressionRes)))
                (cache-nested-cmds-return-ast
                  finalCmds
                  (-> [:term]
                      (into (tokens->nodes [varNameOrSubroutineName leftBB]))
                      (conj expressionRes)
                      (conj (token->node rightBB))))
                (push-ts-warn rightBB leftBB varNameOrSubroutineName)))
            (and (= :symbol c2Type) (= "(" c2Data))
            ;func/methodName ( expressionList )
            (let [_ (set-var! varNameOrSubroutineName {:kind     :subroutine
                                                       :is-using true}
                              classVarTable)
                  leftB (pop-ts)
                  expressionListRes (compileExpressionList)
                  ;生成 ClassName.subroutineName paramCount 调用
                  ;在调用前压入 expressionList 计算结果
                  ;如果是 method，则额外扩增参数长度，并压入 this
                  is-method? true
                  ;(= "method" (:type (get-var! varNameOrSubroutineName)))
                  classNameStr (current-class-name)
                  subroutineNameStr (:data varNameOrSubroutineName)
                  paramCount (+ (count-children expressionListRes :expression)
                                (if is-method? 1 0))
                  final-cmds (flatten
                               [(if is-method? ["pop pointer 0"] [])
                                (pop-temp expressionListRes)
                                (format "call %s.%s %d"
                                        classNameStr subroutineNameStr paramCount)])
                  rightB (pop-ts)]
              (if (and (check! [leftB rightB]
                               (type-and? :symbol "(")
                               (type-and? :symbol ")"))
                       (not (nil? expressionListRes)))
                (cache-nested-cmds-return-ast
                  final-cmds
                  (-> [:term]
                      (into (tokens->nodes [varNameOrSubroutineName leftB]))
                      (conj expressionListRes)
                      (conj (token->node rightB))))
                (push-ts-warn rightB leftB varNameOrSubroutineName)))
            ;className/varName . funcName/methodName ( expressionList )
            (and (= :symbol c2Type) (= "." c2Data))
            (let [point (pop-ts)
                  subName (pop-ts)
                  leftB (pop-ts)
                  _ (set-var-careful! varNameOrSubroutineName {:is-using true})
                  _ (set-var! subName {:kind :subroutine :is-using true} classVarTable)
                  expressionListRes (compileExpressionList)
                  expressionCmds (pop-temp expressionListRes)
                  ;生成 ClassName.subroutineName paramCount 调用
                  ;在调用前压入 expressionList 计算结果
                  ;如果是 method，则额外扩增参数长度，并压入 this
                  is-first-class? (is-token-data-upper? varNameOrSubroutineName)
                  is-second-method? (not is-first-class?)
                  {:keys [kind type index]} (get-var! varNameOrSubroutineName)
                  classNameStr (if is-first-class? (:data varNameOrSubroutineName)
                                                   type)
                  subroutineNameStr (:data subName)
                  paramCount (+ (count-children expressionListRes :expression)
                                (if is-second-method? 1 0))
                  final-cmds (flatten
                               [(if is-second-method?
                                  [(format "push %s %d" (id-kind->vm-str kind) index)]
                                  [])
                                expressionCmds
                                (format "call %s.%s %d"
                                        classNameStr subroutineNameStr paramCount)])
                  rightB (pop-ts)]
              (if (and (check! [point subName leftB rightB]
                               (type-and? :symbol ".")
                               (type? :identifier)
                               (type-and? :symbol "(")
                               (type-and? :symbol ")"))
                       (not (nil? expressionListRes)))
                (cache-nested-cmds-return-ast
                  final-cmds
                  (-> [:term]
                      (into (tokens->nodes [varNameOrSubroutineName
                                            point subName leftB]))
                      (conj expressionListRes)
                      (conj (token->node rightB))))
                (push-ts-warn rightB leftB subName point
                              varNameOrSubroutineName)))
            :else                                           ;varName
            (let [return [:term (token->node varNameOrSubroutineName)]
                  {:keys [kind index]} (get-var! varNameOrSubroutineName)]
              (set-var-careful! varNameOrSubroutineName {:is-using true})
              (set-temp return [(format "push %s %d" (id-kind->vm-str kind) index)])
              return)))))))

(defn- compileExpression
  "编译表达式
  term (op term)*
  1 * 2 + 3
  每个表达式都生成 VM 指令按照顺序 term1 (term2 term3 .. op)? 1 2 * 3 +
  执行后其结果是堆栈最顶层元素，expression 不写入 VM 指令，只保存 Node 对应的临时 VM 指令
  到 vm-temp 缓存中" []
  (let [termRes (compileTerm)
        termCmds (pop-temp termRes)
        isOP? #(check! [%] (type-and-contains?
                             :symbol "+" "-" "*" "/"
                             "&" "|" "<" ">" "="))]
    (let [return
          (if (not (nil? termRes))
            (let [try-next-fn (fn [] (let [a (pop-ts)]
                                       (if (isOP? a) [a (compileTerm)] [a nil])))
                  rest-op-termRes
                  (doall (take-while (fn [[op term]]
                                       (if (nil? term) (do (push-ts op) nil) true))
                                     (repeatedly try-next-fn)))
                  opRes-termRes                             ;may nil
                  (reduce (fn [agg [op term]]
                            (conj agg (token->node op) term))
                          [] rest-op-termRes)
                  rest-termCmds-opCmds
                  (reduce (fn [agg [op term]]
                            (conj agg (pop-temp term) (opToken->cmds op)))
                          [] rest-op-termRes)
                  final-res
                  (-> [:expression] (conj termRes) (into opRes-termRes))]
              (set-temp final-res (flatten [termCmds rest-termCmds-opCmds]))
              final-res)
            nil)]
      return)))

(defn- compileExpressionList
  "编译逗号分隔符分割的表达式列表（可空）
  expression
  expression, expression" []
  (let [expRes (compileExpression)
        expCmds (pop-temp expRes)]
    (if (not (nil? expRes))
      (let [nextExpFunc (fn [] (let [{:keys [type data] :as a} (pop-ts)]
                                 (if (and (= :symbol type) (= "," data))
                                   [a (compileExpression)] [a nil])))
            nextExps (doall
                       (take-while
                         (fn [[sp expResInner]]
                           (if (not (nil? expResInner)) true (do (push-ts sp) nil)))
                         (repeatedly nextExpFunc)))
            nextExpNodesRes (reduce (fn [agg [sp exp]]
                                      (conj agg (token->node sp) exp))
                                    [] nextExps)            ;may nil
            restExpCmds (reduce (fn [agg [_ exp]]
                                  (conj agg (pop-temp exp)))
                                [] nextExps)
            return (-> [:expressionList]
                       (conj expRes)
                       (into nextExpNodesRes))]             ;may nil
        (set-temp return (flatten [expCmds restExpCmds]))
        return)
      (cache-nested-cmds-return-ast [] [:expressionList]))))

(defn- compileStart
  "执行整个翻译，class 是 Jack 基本单元，因此每个文件第一个 token 一定是 class" []
  (let [{:keys [type data]} (peek-ts)]
    (if (and (= :keyword type) (= "class" data))
      (compileClass)
      (throw (RuntimeException. "文件的第一个字元应该是 class")))))

(defn do-parse-and-gen
  "将终结符转换为 <keyword/symbol/integerConstant/stringConstant/identifier> 无子节点
  将非终结符转换为 <class/classVarDec/subroutineDec/parameterList/subroutineBody/
  varDec/statements/whileStatement/ifStatement/returnStatement/letStatement/
  doStatement/expression/term/expressionList> 带子元素节点

  注意，因为过程依赖于副作用，因此所有 lazy-seq 必须展开，即 take-while 必须 doall"
  [token-stream]
  (reset! ts (apply list token-stream))
  (reset! vm-cmds {})
  (reset! temp-cmds {})
  (let [result (compileStart)]
    (println "parse ast and gen code done!")
    #_(println "generate vm cmds: ")
    #_(doall (map #(println " " %) @vm-cmds))
    result))

;;;;;;;;;;;;;;;;;; persistent operation ;;;;;;;;;;;;;;;;;;

(defn save-to
  "将生成的 list 数据写入到文件中"
  [filename output]
  (with-open [w (io/writer filename)]
    (.write w (str/join "\n" output))))

(defn save-xml-to
  "将 Hiccup 格式 XML 数据转换并保存到文件"
  [file tags]
  (println "saving ast to " file)
  (with-open [out (clojure.java.io/writer file)]
    (emit (sexp-as-element tags) out)))

(defn save-vm-cmds [output ast]
  (println "saving vm cmds to " output)
  (save-to output @vm-cmds)
  (reset! vm-cmds [])
  ast)

;;;;;;;;;;;;;;;;;; test ;;;;;;;;;;;;;;;;;;

;.\tools\JackCompiler.bat .\projects\11\Seven
(defn do-test []
  (reset! debug true)
  (let [file "../projects/11/Seven/Main.jack"
        file "../projects/11/Average/Main.jack"
        file "../projects/11/ComplexArrays/Main.jack"
        file "../projects/11/Square/Main.jack"
        file "../projects/11/Square/Square.jack"
        file "../projects/11/Square/SquareGame.jack"
        file "../projects/11/ConvertToBin/Main.jack"
        file "../projects/11/Pong/Main.jack"
        file "../projects/11/Pong/Ball.jack"
        file "../projects/11/Pong/Bat.jack"
        file "../projects/11/Pong/PongGame.jack"
        input (Paths/get file (into-array [""]))
        pure-name (-> (str (.getFileName input)) (str/split #"\.") first)
        output-vm (.resolve (.getParent input) (str pure-name ".cmp.vm"))
        output-ast (.resolve (.getParent input) (str pure-name ".xml"))]
    (->> (do-scan-token file)
         (do-parse-and-gen)
         (save-vm-cmds (str output-vm))
         (save-xml-to (str output-ast)))))

(defn do-compile
  "将某个文件夹下所有 .jack 文件进行编译"
  [folder]
  (reset! debug false)
  (let [folder (Paths/get folder (into-array [""]))
        files (-> (Files/list folder) .iterator iterator-seq)]
    (doseq [^Path input files]
      (if (.endsWith (.toString input) ".jack")
        (let [pure-name (-> (str (.getFileName input)) (str/split #"\.") first)
              output-vm (.resolve (.getParent input) (str pure-name ".cmp.vm"))
              output-ast (.resolve (.getParent input) (str pure-name ".xml"))]
          (println "handling " (str input))
          (->> (do-scan-token (str input))
               (do-parse-and-gen)
               (save-vm-cmds (str output-vm))
               (save-xml-to (str output-ast))))))))

;2022-7-1 下午
;修复了错误打印上一次 vm-cmds 的问题
;修复了函数标示没有显示局部变量正确个数的问题
;修复了分配内存时，错误统计了类局部变量个数导致 malloc 参数传入错误的问题
;修复了字符串中包含符号前后有额外空格的问题
;重新实现了 if 逻辑，使用三个标签
;修复了设置变量时错误的将类变量的更新状态更新到子程序变量表中
;修复了上述错误导致查找类变量先在子程序变量表中找到不完整的信息
;修复了 doStatement 中压入类变量对象调用方法时的错误：翻译的指令是 pointer/local 而非 this
;修复了 unary term 未翻译的问题
;修复了子程序没有将 this 添加到 subroutineVarTable 导致顺序出错的问题
;WIP 基于 hash 的 cache-cmds 容易导致错误，比如参数列表两个相同的字面量 0，hash 一致
;修复了 letStatement 中 field 变量被错误更新为 var 类型的问题
;修复了 letStatement 中 expression - term 压入类变量对象调用方法时的错误