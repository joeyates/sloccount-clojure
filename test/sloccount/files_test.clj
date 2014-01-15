(ns sloccount.files-test
  (:use midje.sweet)
  (:require [sloccount.files :as files]))

(facts "about `source-files`"
  (fact "it returns files in the path"
    (files/source-files "path") => ["foo"]
      (provided (files/filenames "path") => ["foo"]))

  (fact "it excludes files in the .git directory"
    (files/source-files "path") => ["foo"]
      (provided (files/filenames "path") => [".git/bar" "/.git/baz" "foo"]))

  (fact "it excludes .gitignore"
    (files/source-files "path") => ["foo"]
      (provided (files/filenames "path") => [".git/bar" "foo"]))

  (fact "it excludes binary files"
    (files/source-files "path") => ["foo"]
      (provided (files/filenames "path") => ["text.doc" "foo"])))
