(ns sloccount.files
  (:require [clojure.java.io]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Java stuff

(defn filenames [path]
  (map
    #(.toString %)
    (filter
      #(.isFile %)
      (file-seq (clojure.java.io/file (.toString path))))))

(defn file-empty? [file]
  (= (.length (clojure.java.io/file (.toString file))) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; File selection
(defn version-control? [filename]
  (re-find #"\W(\.git|\.svn)\W" filename))
(defn settings? [filename]
  (re-find #"/(\.htaccess|\.gitignore)$" filename))
(defn binary? [filename]
  (re-find #"\.(doc|dot|exe|gif|jpe?g|ods|odt|ott|png|ttf|xls)$" filename))
(defn source-file? [filename]
  (cond
    (version-control? filename) false
    (settings? filename)        false
    (binary? filename)          false
    true                        true))

(defn source-files [path]
  (filter
    source-file?
    (filenames path)))

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
(defn ruby-file-name? [filename]
  (or
    (.endsWith filename ".rb")
    (.endsWith filename ".gemspec")
    (.endsWith filename "/Gemfile")
    (.endsWith filename "/Gemfile.lock")
    (.endsWith filename "/Rakefile")))
(defn sql-file-name? [filename] (.endsWith filename ".sql"))
(defn text-file-name? [filename]
  (or
    (.endsWith filename ".txt")
    (.endsWith filename "/COPYING")
    (re-find #"\WREADME" filename)))
(defn xml-file-name? [filename] (re-find #"\.(xmi|xml|xsl)$" filename))
(defn yaml-file-name? [filename]
  (or
    (re-find #"\.ya?ml$" filename)
    (.endsWith filename "/.rspec")))

(defn is-shebang-of-type? [line type]
  (re-find (re-pattern (str "#![a-z/]*\\s?" type)) line))

(defn is-shebang? [rdr]
  (let [first (first (line-seq rdr))]
    (or
     (and (is-shebang-of-type? first "perl") :perl)
     (and (is-shebang-of-type? first "ruby") :ruby)
     false)))

(defn file-type-from-contents [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (or
     (is-shebang? rdr)
     :unknown)))

(defn file-type [file]
  (let [filename (.toString file)]
    (or
     (and (clojure-file-name? filename)          :clojure)
     (and (clojurescript-file-name? filename)    :clojurescript)
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

(defn file-types [path]
  (map
    #(list (.toString %) (file-type %))
    (source-files path)))

(defn files-of-type [path type]
  (filter
    #(= (sloccount.files/file-type %) type)
    (source-files path)))
