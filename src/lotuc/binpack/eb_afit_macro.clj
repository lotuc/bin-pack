(ns lotuc.binpack.eb-afit-macro
  (:require
   [clojure.core.async :as a]))

(defmacro go [& body]
  (let [ctx (gensym "ctx")
        stats (gensym "stats")]
    `(let [~ctx lotuc.binpack.eb-afit-async/*ctx*
           ~stats lotuc.binpack.eb-afit-async/*stats*]
       (a/go (binding [lotuc.binpack.eb-afit-async/*ctx* ~ctx
                       lotuc.binpack.eb-afit-async/*stats* ~stats]
               ~@body)))))

(defmacro thread! [& body]
  (let [ctx (gensym "ctx")
        stats (gensym "stats")]
    `(let [~ctx lotuc.binpack.eb-afit-async/*ctx*
           ~stats lotuc.binpack.eb-afit-async/*stats*]
       (a/go (when (a/<! (lotuc.binpack.eb-afit-async/continue?))
               (a/<! (a/thread (binding [lotuc.binpack.eb-afit-async/*ctx* ~ctx
                                         lotuc.binpack.eb-afit-async/*stats* ~stats]
                                 ~@body))))))))

(defmacro go! [& body]
  `(go (when (a/<! (lotuc.binpack.eb-afit-async/continue?)) ~@body)))

(defmacro swap-stats! [f & args]
  `(when lotuc.binpack.eb-afit-async/*stats* (swap! lotuc.binpack.eb-afit-async/*stats* ~f ~@args)))
