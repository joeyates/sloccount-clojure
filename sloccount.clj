;; This code was inspired by an example in "Programming Clojure" by Stuart Halloway
;; Copyright (c) 2009 Joe Yates, released under the MIT license

(ns sloccount
  (:gen-class
   :state app-state
   :init init)
  (:use [clojure.contrib.duck-streams :only (reader)] [sloccount.files])
  (:import (java.io.File)))

;; Utilities
(defn merge-sums [totals results]
  (merge-with + totals results))

(defn blank? [line]
  (re-find #"^\s*$" line))

;; Line counting

;; Mason state machine
(defn mason-html-state [line state]
  (cond
    (re-find #"<%(perl|shared|once)>" line) (and (reset! state :perl) :perl)
    (.startsWith line "%")                                            :perl
    true                                                              :html))
(defn mason-javascript-state [line state]
  (cond
    (re-find #"<%(perl|shared|once)>" line) (and (reset! state :perl) :perl)
    (.startsWith line "%")                                            :perl
    true                                                              :javascript))
(defn mason-perl-state [line state previous-state]
  (cond
    (re-find #"</%(perl|shared|once)>" line) (and (reset! state previous-state) previous-state)
    true                                                                        :perl))
(defn mason-html-line-type [line state]
  (if (= @state :html)
    (mason-html-state line state)
    (mason-perl-state line state :html)))
(defn mason-javascript-line-type [line state]
  (if (= @state :javascript)
    (mason-javascript-state line state)
    (mason-perl-state line state :javascript)))

;; line-types Returns a seq of keywords indicating line types
(defmulti line-types sloccount.files/file-type :default :text)

(defmethod line-types :clojure [file]
  (with-open [rdr (reader file)]
    (map
     (fn [line]
       (cond
         (blank? line)            :blank
         (.startsWith line "#!/") :shebang
         (.startsWith line ";")   :comment
         true                     :clojure))
     (doall (line-seq rdr)))))

(defmethod line-types :mason-html [file]
  (let [state (atom :html)]
    (with-open [rdr (reader file)]
      (map 
       (fn [line] (mason-html-line-type line state))
       (doall (line-seq rdr))))))

(defmethod line-types :mason-javascript [file]
  (let [state (atom :javascript)]
    (with-open [rdr (reader file)]
      (map 
       (fn [line] (mason-javascript-line-type line state))
       (doall (line-seq rdr))))))

(defmethod line-types :perl [file]
  (with-open [rdr (reader file)]
    (map
     (fn [line]
       (cond
         (blank? line)            :blank
         (.startsWith line "#!/") :shebang
         (.startsWith line "#")   :comment
         true                     :perl))
     (doall (line-seq rdr)))))

(defmethod line-types :ruby [file]
  (with-open [rdr (reader file)]
    (map
     (fn [line]
       (cond
         (blank? line)            :blank
         (.startsWith line "#!/") :shebang
         (.startsWith line "#")   :comment
         true                     :ruby))
     (doall (line-seq rdr)))))

(defmethod line-types :text [file]
  (let [file-type (sloccount.files/file-type file)]
    (with-open [rdr (reader file)]
      (map
       (fn [line]
         (cond
           (blank? line) :blank
           true          file-type))
       (doall (line-seq rdr))))))

(defmethod line-types :xml [file]
  (with-open [rdr (reader file)]
    (map
     (fn [line]
       (cond
         (blank? line) :blank
         true          :xml))
     (doall (line-seq rdr)))))

(defn count-lines [file]
  (reduce
   #(merge-sums %1 {%2 1})
   {} (line-types file)))

(defn loc [path]
  (reduce
   merge-sums
   (for [file (sloccount.files/source-files path)] 
     (count-lines file))))

(defn list-files-of-type [path type]
  (println
   (map
    #(str % "\n")
    (sloccount.files/files-of-type path type))))

(defn -main [& args]
  (cond
    (= 1 (count args))                                          (println (loc (first args)))
    (and (= 3 (count args)) (= "--files-of-type" (nth args 1))) (list-files-of-type (nth args 0) (keyword (nth args 2)))
    true                                                        (println "Usage: ...")))

(defn -init []
  [[] (atom [])])
