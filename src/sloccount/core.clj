;; This code was inspired by an example in "Programming Clojure" by Stuart Halloway
;; Copyright (c) 2009 Joe Yates, released under the MIT license

(ns sloccount.core
  (:gen-class
   :state app-state
   :init init)
  (:use [sloccount.files] [sloccount.line_types])
  (:import (java.io.File)))

;; Utilities
(defn merge-sums [totals results]
  (merge-with + totals results))

(defn count-lines [file]
  (reduce
    #(merge-sums %1 {%2 1})
    {} (sloccount.line_types/line-types file)))

(defn loc [path]
  (reduce
    merge-sums
    (for [file (sloccount.files/source-files path)]
      (count-lines file))))

(defn list-file-types [path]
  (doseq [p (sloccount.files/file-types path)]
    (println (str "\"" (first p) "\" " (second p)))))

(defn list-files-of-type [path type]
  (println
   (map
     #(str % "\n")
     (sloccount.files/files-of-type path type))))

(defn usage []
  (println "Usage:
lein run PATH [--file-types|--files-of-type TYPE]"))

(defn -main [& args]
  (cond
    (= 1 (count args))
      (println (loc (first args)))
    (and
      (= 3 (count args))
      (= "--files-of-type" (nth args 1)))
      (list-files-of-type (nth args 0) (keyword (nth args 2)))
    (and
      (= 2 (count args))
      (= "--file-types" (nth args 1)))
      (list-file-types (first args))
    true
       (usage)))

(defn -init []
  [[] (atom [])])
