(ns thorough.instrument
  (:require [clojure.tools.logging :as log]
            [clojure.tools.reader :as r]
            [cloverage.debug :as d]
            [cloverage.source :as s]
            [cloverage.rewrite :refer [unchunk]]
            [riddley.walk :refer [macroexpand-all]]
            [slingshot.slingshot :refer [throw+]]
            [cloverage.instrument :as inst]))

(defn iobj? [form]
  (and
   (instance? clojure.lang.IObj form)
   (not (instance? clojure.lang.AFunction form))))

(defn get-ctx
  ([form]
   (get-ctx form nil))
  ([form default]
   (let [meta (meta form)
         ctx (or (:ctx meta) default)]
     (merge ctx (select-keys meta [:line])))))

(defn propagate-ctx
  "Assign :ctx metadata to all possible elements in a form,
  using start as default."
  [start form]
  (if (iobj? form)
    (let [ctx (get-ctx form start)
          recs (if (and (seq? form) (seq form))
                ;; (seq '()) gives nil which makes us NPE. Bad.
                 (with-meta
                   (seq (map (partial propagate-ctx ctx) form))
                   (meta form))
                 form)
          ret  (if (and ctx
                        (not (map? (:ctx (meta form)))))
                 (vary-meta recs assoc :ctx ctx)
                 recs)]
      ret)
    form))

(defn add-original [old new]
  (if (iobj? new)
    (let [res (-> (propagate-ctx (:ctx (meta old)) new)
                  (vary-meta merge (meta old))
                  (vary-meta assoc :original old))]
      res)
    new))

(defn atomic-special? [sym]
  (contains? '#{quote var clojure.core/import* recur} sym))

;; Snipped from tools.reader
;; https://github.com/clojure/tools.reader/blob/de7b39c3/src/main/clojure/clojure/tools/reader.clj#L456
(defn- resolve-ns [sym]
  (or ((ns-aliases *ns*) sym)
      (find-ns sym)))

(defn- resolve-symbol [s]
  (if (pos? (.indexOf (name s) "."))
    s
    (if-let [ns-str (namespace s)]
      (let [ns (resolve-ns (symbol ns-str))]
        (if (or (nil? ns)
                (= (name (ns-name ns)) ns-str)) ;; not an alias
          s
          (symbol (name (ns-name ns)) (name s))))
      (if-let [o ((ns-map *ns*) s)]
        (if (class? o)
          (symbol (.getName ^Class o))
          (if (var? o)
            (let [^clojure.lang.Var o o]
              (symbol (-> o .ns .name name) (-> o .sym name)))))
        ;; changed to returned unnamespaced symbol if it fails to resolve
        s))))

(defn- maybe-resolve-symbol [expr]
  (if (symbol? expr)
    (resolve-symbol expr)
    expr))

(defn list-type [head]
  (condp #(%1 %2) (maybe-resolve-symbol head)
    ;; namespace-less specials
    '#{try}         :try     ; try has to special case catch/finally
    '#{if do throw} :do      ; these special forms can recurse on all args
    '#{let* loop*}  :let
    '#{def}         :def     ; def can recurse on initialization expr
    '#{fn*}         :fn
    '#{set!}        :set     ; set must not evaluate the target expr
    '#{.}           :dotjava
    '#{case*}       :case*
    '#{new}         :new
    '#{reify*}      :reify*
    ;; FIXME: monitor-enter monitor-exit
    ;; FIXME: import*?

    ;; namespaced macros
    `#{cond}        :cond    ; special case cond to avoid false partial
    `#{loop let}    :let
    `#{letfn}       :letfn
    `#{for doseq}   :for
    `#{fn}          :fn
    `#{defn}        :defn    ; don't expand defn to preserve stack traces
    `#{defmulti}    :defmulti ; special case defmulti to avoid boilerplate
    `#{defprotocol} :atomic   ; no code in protocols
    `#{defrecord}   :record
    `#{deftype*}    :deftype*
    `#{ns}          :atomic

    ;; http://dev.clojure.org/jira/browse/CLJ-1330 means AOT-compiled definlines
    ;; are broken when used indirectly. Work around - do not wrap the definline
    `#{booleans bytes chars shorts floats ints doubles longs} :inlined
    atomic-special?   :atomic
    ;; XXX: we used to not do anything with unknown specials, now we wrap them
    ;; in a macro, then macroexpand back to original form. Methinks it's ok.
    special-symbol?   :unknown
    (constantly true) :list))

(defn list-type-in-env [[head & _] env]
  (if (get env head)
    :list ; local variables can't be macros/special symbols
    (list-type head)))

(def ^:dynamic *exclude-calls*
  "The set of symbols that will suppress instrumentation when any are used in
  the calling position of a form. Useful for excluding problematic macro call
  sites from coverage metrics."
  nil)

(defn exclude? [form]
  (boolean (and *exclude-calls*
                (*exclude-calls* (maybe-resolve-symbol (first form))))))

(defn form-type
  "Classifies the given form"
  ([form env]
   (let [res (cond (and (seq? form)
                        (exclude? form)) :excluded
                   (seq? form)           (list-type-in-env form env)
                   (coll? form)          :coll
                   :else                 :atomic)]
     (d/tprnl "Type of" (class form) form "is" res)
     (d/tprnl "Meta of" form "is" (meta form))
     res)))

(defn- var->sym [^clojure.lang.Var fvar]
  (let [it (name (.sym fvar))
        nsn (name (ns-name (.ns fvar)))]
    (symbol nsn it)))

(defmulti do-wrap
  "Traverse the given form and wrap all its sub-forms in a function that evals
  the form and records that it was called."
  (fn [f ctx form env]
    (form-type form env)))

(defmacro wrapm
  "Helper macro for wrap.
  Takes advantage of &env to track lexical scope while walking `form`."
  [f-sym ctx-hint form]
  (let [f    (resolve f-sym)
        ctx (get-ctx form ctx-hint)
        result (do-wrap f ctx form &env)]
    result))

(defn wrap
  "Main interface for wrapping expressions using `f`.
  Wrap will return a form that during macroexpansion calls `f` on `form` and
  all sub-expressions of `form` that can be meaningfully wrapped.
  `f` should take an expression and return one that evaluates in exactly the
  same way, possibly with additional side effects."
  [f-var ctx-hint form]
  (when-not (var? f-var)
    (throw (Exception. (str "Wrap must be given a function var. Got " f-var " [" (type f-var) "] instead."))))
  `(wrapm ~(var->sym f-var) ~ctx-hint ~form))

(defn wrapper
  "Return a function that when called, wraps f through its argument."
  [f ctx]
  (partial wrap f ctx))

(defn wrap-binding
  "Wrap a let/loop binding
  e.g. - `a (+ a b)`       (let or loop)"
  [f ctx-hint [args & body :as form]]
  (d/tprnl "Wrapping overload" args body)
  (let [ctx (get-ctx form ctx-hint)]
    (let [wrapped (doall (map (wrapper f ctx) body))]
      `(~args ~@wrapped))))

(defn wrap-overload
  "Wrap a single function overload.
  e.g. - ([a b] (+ a b)) or
          ([n] {:pre [(> n 0)]} (/ 1 n))"
  [f ctx-hint [args & body :as form]]
  (d/tprnl "Wrapping function overload" args body)
  (let [ctx  (get-ctx form ctx-hint)
        conds (when (and (next body) (map? (first body)))
                (first body))
        conds (when conds
                (zipmap (keys conds)
                        (map (fn [exprs] (vec (map (wrapper f ctx) exprs)))
                             (vals conds)))) ; must not wrap the vector itself
        ;; i.e. [(> n 1)] -> [(do (> n 1))], not (do [...])
        ;; the message of AssertionErrors will be different, too bad.
        body  (if conds (next body) body)
        wrapped (doall (map (wrapper f ctx) body))]
    `(~args
      ~@(when conds (list conds))
      ~@wrapped)))

;; Wrap a list of function overloads, e.g.
;;   (([a] (inc a))
;;    ([a b] (+ a b)))
(defn wrap-overloads [f ctx-hint form]
  (d/tprnl "Wrapping overloads " form)
  (let [ctx (get-ctx form ctx-hint)]
    (if (vector? (first form))
      (wrap-overload f ctx form)
      (try
        (doall (map (partial wrap-overload f ctx) form))
        (catch Exception e
          (d/tprnl "ERROR: " form)
          (d/tprnl e)
          (throw
           (Exception. (pr-str "While wrapping" (:original (meta form)))
                       e)))))))

;; Don't wrap or descend into unknown forms
(defmethod do-wrap :unknown [f ctx form _]
  (log/warn (str "Unknown special form " (seq form)))
  form)

;; Don't wrap or descend into excluded forms
(defmethod do-wrap :excluded [_ _ form _]
  (log/info (str "Excluded form " (seq form)))
  form)

;; Don't wrap definline functions - see http://dev.clojure.org/jira/browse/CLJ-1330
(defmethod do-wrap :inlined [f ctx [inline-fn & body] _]
  `(~inline-fn ~@(map (wrapper f ctx) body)))

;; Don't descend into atomic forms, but do wrap them
(defmethod do-wrap :atomic [f ctx form _]
  (f ctx form))

;; Only here for Clojure 1.4 compatibility, 1.6 has record?
(defn- map-record? [x]
  (instance? clojure.lang.IRecord x))

;; For a collection, just recur on its elements.
(defmethod do-wrap :coll [f ctx form _]
  (d/tprn ":coll" form)
  (let [wrappee (map (wrapper f ctx) form)
        wrapped (cond (vector? form) `[~@wrappee]
                      (set? form) `#{~@wrappee}
                      (map-record? form) (merge form
                                                (zipmap
                                                 (doall (map (wrapper f ctx) (keys form)))
                                                 (doall (map (wrapper f ctx) (vals form)))))
                      (map? form) (zipmap
                                   (doall (map (wrapper f ctx) (keys form)))
                                   (doall (map (wrapper f ctx) (vals form))))
                      :else (do
                              (when (nil? (empty form))
                                (throw+ (str "Can't construct empty " (class form))))
                              `(into ~(empty form) [] ~(vec wrappee))))]
    (d/tprn ":wrapped" (class form) (class wrapped) wrapped)
    (f ctx wrapped)))

(defn wrap-fn-body [f ctx form]
  (let [fn-sym (first form)
        res    (if (symbol? (second form))
                ;; If the fn has a name, include it
                 `(~fn-sym ~(second form)
                           ~@(wrap-overloads f ctx (rest (rest form))))
                 `(~fn-sym ~@(wrap-overloads f ctx (rest form))))]
    (d/tprnl "Instrumented function" res)
    res))

;; Wrap a fn form
(defmethod do-wrap :fn [f ctx form _]
  (d/tprnl "Wrapping fn " form)
  (f ctx (wrap-fn-body f ctx form)))

(defmethod do-wrap :let [f ctx [let-sym bindings & body :as form] _]
  (f ctx
     `(~let-sym
       [~@(mapcat (partial wrap-binding f ctx)
                  (partition 2 bindings))]
       ~@(doall (map (wrapper f ctx) body)))))

(defmethod do-wrap :letfn [f ctx [_ bindings & _ :as form] _]
  ;; (letfn [(foo [bar] ...) ...] body) ->
  ;; (letfn* [foo (fn foo [bar] ...) ...] body)
  ;; must not wrap (fn foo [bar] ...)
  ;; we expand it manually to preserve function lines
  (let [[letfn*-sym exp-bindings & body] (macroexpand-1 form)]
    (f ctx
       `(~letfn*-sym
         [~@(mapcat
             (fn [[sym fun] orig-bind]
               `(~sym ~(wrap-fn-body f (:ctx (meta orig-bind)) fun)))
             (partition 2 exp-bindings)
             bindings)]
         ~@(doall (map (wrapper f ctx) body))))))

(defmethod do-wrap :def [f ctx [def-sym name & body :as form] _]
  (cond
    (empty? body)      (f ctx `(~def-sym ~name))
    (= 1 (count body)) (let [init (first body)]
                         (f ctx
                            `(~def-sym ~name ~(wrap f ctx init))))
    (= 2 (count body)) (let [docstring (first body)
                             init      (second body)]
                         (f ctx
                            `(~def-sym ~name ~docstring ~(wrap f ctx init))))))

(defmethod do-wrap :defn [f ctx form _]
  ;; do not wrap fn expressions in (defn name (fn ...))
  ;; to preserve function names in exception backtraces
  (let [[def-sym defn-sym fn-expr] (macroexpand-1 form)
        ctx (assoc ctx :defn-sym (name defn-sym))]
    (f ctx `(~def-sym ~defn-sym ~(wrap-fn-body f ctx (propagate-ctx ctx fn-expr))))))

(defmethod do-wrap :new [f ctx [new-sym class-name & args :as form] _]
  (f ctx `(~new-sym ~class-name ~@(doall (map (wrapper f ctx) args)))))

(defmethod do-wrap :dotjava [f ctx [dot-sym obj-name attr-name & args :as form] env]
  ;; either (. obj meth args*) or (. obj (meth args*))
  ;; I think we might have to not-wrap symbols here, or we might lose metadata
  ;; (like :tag type hints for reflection when resolving methods)
  (if (= :list (form-type attr-name env))
    (do
      (d/tprnl "List dotform, recursing on" (rest attr-name))
      (f ctx `(~dot-sym ~obj-name (~(first attr-name)
                                    ~@(doall (map (wrapper f ctx)
                                                  (rest attr-name)))))))
    (do
      (d/tprnl "Simple dotform, recursing on" args)
      (f ctx `(~dot-sym ~obj-name ~attr-name ~@(doall (map (wrapper f ctx) args)))))))

(defmethod do-wrap :set [f ctx [set-symbol target expr] _]
  ;; target cannot be wrapped or evaluated
  (f ctx `(~set-symbol ~target ~(wrap f ctx expr))))

(defmethod do-wrap :do [f ctx [do-symbol & body] _]
  (f ctx `(~do-symbol ~@(map (wrapper f ctx) body))))

(defmethod do-wrap :cond [f ctx [cond-symbol & body :as form] _]
  (if (and (= 2 (count body))
           (= :else (first body)))
    (f ctx (macroexpand `(~cond-symbol :else ~(wrap f ctx (second body)))))
    (wrap f ctx (macroexpand form))))

(defmethod do-wrap :case* [f ctx [case-symbol test-var a b else-clause case-map & stuff] _]
  (assert (= case-symbol 'case*))
  (let [wrap-it (wrapper f ctx)
        wrapped-else (wrap-it else-clause)
        wrapped-map (into (empty case-map)
                          (zipmap (keys case-map)
                                  (for [[k exp] (vals case-map)]
                                    [k (wrap-it exp)])))]
    (f ctx `(~case-symbol ~test-var ~a ~b ~wrapped-else
                           ~wrapped-map ~@stuff))))

(defn wrap-catch [f ctx [catch-symbol classname localname & body]]
  ;; can't transform into (try (...) (<capture> (finally ...)))
  ;; catch/finally must be direct children of try
  `(~catch-symbol ~classname ~localname ~@(map (wrapper f ctx) body)))

(defn wrap-finally [f ctx [finally-symbol & body]]
  ;; can't transform into (try (...) (<capture> (finally ...)))
  ;; catch/finally must be direct children of try
  `(~finally-symbol ~@(map (wrapper f ctx) body)))

(defmethod do-wrap :try [f ctx [try-symbol & body] _]
  (f ctx `(~try-symbol
            ~@(map (fn wrap-try-body [elem]
                     (if-not (seq? elem)
                       (f ctx elem)
                       (let [head (first elem)]
                         (cond
                           (= head 'finally) (wrap-finally f ctx elem)
                           (= head 'catch)   (wrap-catch f ctx elem)
                           :else             (wrap f ctx elem)))))
                   body))))

(defmethod do-wrap :for [f ctx form env]
  (do-wrap f ctx (unchunk form) env))

(defmethod do-wrap :list [f ctx form env]
  (d/tprnl "Wrapping " (class form) form)
  (let [expanded (macroexpand form)]
    (d/tprnl "Expanded" form "into" expanded)
    (d/tprnl "Meta on expanded is" (meta expanded))
    (if (= :list (form-type expanded env))
      (let [wrapped (doall (map (wrapper f ctx) expanded))]
        (f ctx (add-original form wrapped)))
      (wrap f ctx (add-original form expanded)))))

(defn wrap-deftype-defrecord-method [f ctx [meth-name args & body :as method-form]]
  (let [method-line (or (:ctx (meta method-form)) ctx)
        body        (for [form body
                          :let [ctx (get-ctx form method-line)]]
                      (wrap f ctx form))]
    `(~meth-name ~args ~@body)))

(defmethod do-wrap :record [f ctx [defr-symbol name fields & opts+specs] _]
  ;; (defrecord name [fields*] options* specs*)
  ;;
  ;; spec == thing-being-implemented (methodName [args*] body)*
  ;; we only want to recurse on the methods
  (let [wrapped-opts+specs (for [opt-or-spec opts+specs]
                             (if (list? opt-or-spec)
                               (wrap-deftype-defrecord-method f ctx opt-or-spec)
                               opt-or-spec))]
    (f ctx `(~defr-symbol ~name ~fields ~@wrapped-opts+specs))))

(defmethod do-wrap :deftype* [f ctx [deft-symbol name class-name fields implements interfaces & methods] _]
  ;; (deftype name [fields*] options* specs*)
  ;;
  ;; expands into
  ;;
  ;; (deftype* name class-name [fields*] :implements [interfaces] methods*)
  (let [wrapped-methods (for [method methods]
                          (wrap-deftype-defrecord-method f ctx method))]
    `(~deft-symbol ~name ~class-name ~fields ~implements ~interfaces ~@wrapped-methods)))

(defmethod do-wrap :defmulti [f ctx [defm-symbol name & other] _]
  ;; wrap defmulti to avoid partial coverage warnings due to internal
  ;; clojure code (stupid checks for wrong syntax)
  (let [docstring     (when (string? (first other)) (first other))
        other         (if docstring (next other) other)
        attr-map      (when (map? (first other)) (first other))
        other         (if (map? (first other)) (next other) other)
        dispatch-form (first other)
        other         (rest other)]
    (f ctx `(~defm-symbol ~name ~@(if docstring (list docstring) (list))
                           ~@(if attr-map  (list attr-map)  (list))
                           ~(wrap f ctx dispatch-form) ~@other))))

(defmethod do-wrap :reify* [f ctx [reify-symbol interfaces & methods] _]
  (f ctx `(~reify-symbol
            ~interfaces
            ~@(map (fn wrap-reify-method [method]
                     `(~(first method) ~@(wrap-overload f ctx (rest method))))
                   methods))))

(defn instrument
  "Instruments and evaluates a list of forms."
  ([f-var lib]
   (let [filename (s/resource-path lib)]
     (let [src (s/form-reader lib)]
       (loop [instrumented-forms nil]
         (if-let [form (binding [*read-eval* false]
                         (r/read {:eof nil
                                  :features #{:clj}
                                  :read-cond :allow}
                                 src))]
           (let [line-hint (:line (meta form))
                 form      (if (and (iobj? form)
                                    (nil? (:file (meta form))))
                             (vary-meta form assoc :file filename)
                             form)
                 wrapped   (try
                             (wrap f-var {:line line-hint} form)
                             (catch Throwable t
                               (throw+ t "Couldn't wrap form %s at ctx %s"
                                       form line-hint)))]
             (try
               (binding [*file*        filename
                         *source-path* filename]
                 (eval wrapped))
               (binding [*print-meta* true]
                 (d/tprn "Evalling" wrapped " with meta " (meta wrapped)))
               (catch Exception e
                 (throw (Exception.
                         (str "Couldn't eval form "
                              (binding [*print-meta* true]
                                (with-out-str (prn wrapped)))
                              (with-out-str (prn (macroexpand-all wrapped)))
                              (with-out-str (prn form)))
                         e))))
             (recur (conj instrumented-forms wrapped)))
           (let [rforms (reverse instrumented-forms)]
             (d/dump-instrumented rforms lib)
             rforms)))))))

(defn nop
  "Instrument form with expressions that do nothing."
  [ctx-hint form]
  `(do ~form))

(defn no-instr
  "Do not change form at all."
  [ctx-hint form]
  form)
