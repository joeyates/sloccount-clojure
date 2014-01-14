;; Copyright (c) 2009 Joe Yates, released under the MIT license

(ns sloccount.files
  (:use [clojure.java.io :only (reader)])
  (:import (java.io.File)))

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
(defn clojurescript-file-name? [filename] (.endsWith filename ".cljs"))
(defn css-file-name? [filename] (.endsWith filename ".css"))
(defn html-file-name? [filename] (re-find #"\.html?$" filename))
(defn javascript-file-name? [filename] (.endsWith filename ".js"))
(defn mason-html-file-name? [filename]
  (or
   (re-find #"\.(mhtml|mcp)$" filename)
   (.endsWith filename "/autohandler")
   (.endsWith filename  "/dhandler")))
(defn mason-javascript-file-name? [filename] (re-find #"\.(mjs)$" filename))
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
     (and (clojure-file-name? filename)          :clojure)
     (and (clojurescript-file-name? filename)    :clojurescipt)
     (and (css-file-name? filename)              :css)
     (and (html-file-name? filename)             :html)
     (and (javascript-file-name? filename)       :javascript)
     (and (mason-html-file-name? filename)       :mason-html)
     (and (mason-javascript-file-name? filename) :mason-javascript)
     (and (perl-file-name? filename)             :perl)
     (and (ruby-file-name? filename)             :ruby)
     (and (sql-file-name? filename)              :sql)
     (and (text-file-name? filename)             :text)
     (and (xml-file-name? filename)              :xml)
     (and (yaml-file-name? filename)             :yaml)
     (and (file-empty? file)                     :empty)
     (file-type-from-contents file))))

(defn files-of-type [path type]
  (filter
   #(= (sloccount.files/file-type %) type)
   (source-files path)))
