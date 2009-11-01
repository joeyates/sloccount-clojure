;; This was inspired by an example in "Programming Clojure" by Stuart Halloway
;; Copyright (c) 2009 Joe Yates, released under the MIT license

(ns sloccount
  (:gen-class
   :state app-state
   :init init)
  (:use [clojure.contrib.duck-streams :only (reader)])
  (:import (java.io.File)))

;; Utilities
(defn merge-sums [totals results]
  (merge-with + totals results))

(defn blank? [line]
  (re-find #"^\s*$" line))

;; File selection
(defn version-control? [file] 
  (re-find #"\W(\.git|\.svn)\W" (.toString file)))
(defn settings? [file] 
  (re-find #"/(\.htaccess|\.gitignore)$" (.toString file)))
(defn binary? [file] 
  (re-find #"\.(doc|dot|exe|gif|jpe?g|ods|odt|ott|png|ttf|xls)$" (.toString file)))
(defn source-file? [file]
  (cond
    (version-control? file) false
    (settings? file)        false
    (binary? file)          false
    true                    true))

(defn files [path]
  (filter 
   #(.isFile %)
   (file-seq (java.io.File. (.toString path)))))

(defn source-files [path]
  (filter
   source-file?
   (files path)))

;; File type inference
(defn clojure-file-name? [filename] (.endsWith filename ".clj"))
(defn css-file-name? [filename] (.endsWith filename ".css"))
(defn html-file-name? [filename] (re-find #"\.html?$" filename))
(defn javascript-file-name? [filename] (.endsWith filename ".js"))
(defn mason-file-name? [filename] (or (re-find #"\.(mhtml|mcp)$" filename) (.endsWith filename "/autohandler") (.endsWith filename  "/dhandler")))
(defn perl-file-name? [filename] (re-find #"\.p[lm]$" filename))
(defn ruby-file-name? [filename] (or (.endsWith filename ".rb") (.endsWith filename "Rakefile")))
(defn sql-file-name? [filename] (.endsWith filename ".sql"))
(defn text-file-name? [filename] (or (.endsWith filename ".txt") (re-find #"\WREADME" filename)))
(defn xml-file-name? [filename] (re-find #"\.(xmi|xml|xsl)$" filename))
(defn yaml-file-name? [filename] (re-find #"\.ya?ml$" filename))

(defn is-shebang-of-type? [line type]
  (re-find (re-pattern (str "#![a-z/]*\\s?" type)) line))

(defn is-shebang? [rdr]
  (let [first (first (line-seq rdr))]
    (or
     (and (is-shebang-of-type? first "perl") :perl)
     (and (is-shebang-of-type? first "ruby") :ruby)
     false)))

(defn file-type-from-contents [file]
  (with-open [rdr (reader file)]
    (or
     (is-shebang? rdr)
     :unknown)))

(defn file-empty? [file]
  (= (.length (java.io.File. (.toString file))) 0))

(defn file-type [file]
  (let [filename (.toString file)]
    (or
     (and (clojure-file-name? filename)    :clojure)
     (and (css-file-name? filename)        :css)
     (and (html-file-name? filename)       :html)
     (and (javascript-file-name? filename) :javascript)
     (and (mason-file-name? filename)      :mason)
     (and (perl-file-name? filename)       :perl)
     (and (ruby-file-name? filename)       :ruby)
     (and (sql-file-name? filename)        :sql)
     (and (text-file-name? filename)       :text)
     (and (xml-file-name? filename)        :xml)
     (and (yaml-file-name? filename)       :yaml)
     (and (file-empty? file)               :empty)
     (file-type-from-contents file))))

;; Line counting

;; Perl Mason state machine
(defn mason-html-state [line state]
  (cond
    (re-find #"<%(perl|shared|once)>" line) (and (reset! state :perl) :perl)
    (.startsWith line "%")                                            :perl
    true                                                              :html))
(defn mason-perl-state [line state]
  (cond
    (re-find #"</%(perl|shared|once)>" line) (and (reset! state :html) :html)
    true                                                               :perl))
(defn mason-line-type [line state]
  (if (= @state :html)
    (mason-html-state line state)
    (mason-perl-state line state)))

;; line-types Returns a seq of keywords indicating line types
(defmulti line-types file-type :default :text)

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

(defmethod line-types :mason [file]
  (let [state (atom :html)]
    (with-open [rdr (reader file)]
      (map 
       (fn [line] (mason-line-type line state))
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
  (let [file-type (file-type file)]
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
   (for [file (source-files path)] 
     (count-lines file))))

(defn files-of-type [path type]
  (filter
   #(= (file-type %) type)
   (source-files path)))

(defn list-files-of-type [path type]
  (println
   (map
    #(str % "\n")
    (files-of-type path type))))

(defn -main [& args]
  (cond
    (= 1 (count args))                                          (println (loc (first args)))
    (and (= 3 (count args)) (= "--files-of-type" (nth args 1))) (list-files-of-type (nth args 0) (keyword (nth args 2)))
    true                                                        (println "Usage: ...")))

(defn -init []
  [[] (atom [])])
