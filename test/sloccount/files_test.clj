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

(facts "about `files-of-type`"
  (fact "it returns filenames"
    (files/files-of-type "path" "foo") => ["foo_file"]
      (provided
        (files/filenames "path") => ["foo_file", "qux_file"],
        (sloccount.files/file-type "foo_file") => "foo",
        (sloccount.files/file-type "qux_file") => "qux")))
