;;; clojure-ts-mode-font-lock-test.el --- Clojure TS Mode: font lock test suite  -*- lexical-binding: t; -*-

;; Copyright © 2022-2025 Danny Freeman

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The unit test suite of Clojure TS Mode

(require 'clojure-ts-mode)
(require 'cl-lib)
(require 'buttercup)

;; (use-package buttercup)

;;;; Utilities

(defmacro with-fontified-clojure-ts-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENT."
  (declare (debug t)
           (indent 1))
  `(with-clojure-ts-buffer ,content
     (font-lock-ensure)
     (goto-char (point-min))
     ,@body))

(defun clojure-ts--uniform-face (start end)
  "Return the face from START to END if uniform, else `various-faces'.
Assumes the current buffer is already fontified."
  (let ((start-face (get-text-property start 'face))
        (all-faces (cl-loop for i from start to end collect (get-text-property
                                                              i 'face))))
    (if (cl-every (lambda (face) (equal face start-face)) all-faces)
        start-face
      'various-faces)))

(defun clojure-ts-get-face-at (start end content)
  "Get the face between START and END in CONTENT."
  (with-fontified-clojure-ts-buffer content
    (clojure-ts--uniform-face start end)))

(defun clojure-ts--check-faces (content face-specs)
  "Fontify CONTENT and check all FACE-SPECS.
Each spec is either (START END FACE) for positional checks or
\(SUBSTRING FACE) for substring-based checks.

Substring specs are matched sequentially through the buffer so
that repeated substrings resolve naturally in document order."
  (with-fontified-clojure-ts-buffer content
    (dolist (spec face-specs)
      (pcase spec
        (`(,(and (pred stringp) substr) ,face)
         (let ((found (search-forward substr nil t)))
           (expect found :not :to-be nil)
           (when found
             (let* ((end (1- (point)))
                    (start (- (point) (length substr))))
               (expect (clojure-ts--uniform-face start end)
                       :to-equal face)))))
        (`(,(and (pred numberp) start) ,end ,face)
         (expect (clojure-ts--uniform-face start end) :to-equal face))))))

(defun expect-face-at (content start end face)
  "Expect face in CONTENT between START and END to be equal to FACE."
  (expect (clojure-ts-get-face-at start end content) :to-equal face))

(defun expect-faces-at (content &rest faces)
  "Expect FACES in CONTENT.

FACES is a list of the form (content specs*) where each spec is either
\(start end expected-face) or (\"substring\" expected-face)."
  (clojure-ts--check-faces content faces))

(defmacro when-fontifying-it (description &rest tests)
  "Return a buttercup spec.

TESTS are lists of the form (content spec*).  Each spec is either
\(start end expected-face) for positional checks or
\(\"substring\" expected-face) for substring-based checks.

Substring specs are matched sequentially so that repeated
substrings resolve in document order.

DESCRIPTION is the description of the spec."
  (declare (indent 1))
  `(it ,description
     (dolist (test (quote ,tests))
       (let ((content (car test))
             (specs (cdr test)))
         (clojure-ts--check-faces content specs)))))

;;;; Font locking

(describe "clojure-ts-mode-syntax-table"
  (when-fontifying-it "should handle any known def form"
    ("(def a 1)" ("def" font-lock-keyword-face))
    ("(defonce a 1)" ("defonce" font-lock-keyword-face))
    ("(defn a [b])" ("defn" font-lock-keyword-face))
    ("(defmacro a [b])" ("defmacro" font-lock-keyword-face))
    ("(definline a [b])" ("definline" font-lock-keyword-face))
    ("(defmulti a identity)" ("defmulti" font-lock-keyword-face))
    ("(defmethod a :foo [b] (println \"bar\"))" ("defmethod" font-lock-keyword-face))
    ("(defprotocol a (b [this] \"that\"))" ("defprotocol" font-lock-keyword-face))
    ("(definterface a (b [c]))" ("definterface" font-lock-keyword-face))
    ("(defrecord a [b c])" ("defrecord" font-lock-keyword-face))
    ("(deftype a [b c])" ("deftype" font-lock-keyword-face))
    ("(defstruct a :b :c)" ("defstruct" font-lock-keyword-face))
    ("(deftest a (is (= 1 1)))" ("deftest" font-lock-keyword-face))


  ;; TODO: copied from clojure-mode, but failing
  ;; ("(defne [x y])" (2 6 font-lock-keyword-face))
  ;; ("(defnm a b)" (2 6 font-lock-keyword-face))
  ;; ("(defnu)" (2 6 font-lock-keyword-face))
  ;; ("(defnc [a])" (2 6 font-lock-keyword-face))
  ;; ("(defna)" (2 6 font-lock-keyword-face))
  ;; ("(deftask a)" (2 8 font-lock-keyword-face))
  ;; ("(defstate a :start \"b\" :stop \"c\")" (2 9 font-lock-keyword-face))

    )

  (when-fontifying-it "variable-def-string-with-docstring"
    ("(def foo \"usage\" \"hello\")"
     (10 16 font-lock-doc-face)
     (18 24 font-lock-string-face))

    ("(def foo \"usage\" \"hello\"   )"
     (18 24 font-lock-string-face))

    ("(def foo \"usage\" \n  \"hello\")"
     (21 27 font-lock-string-face))

    ("(def foo \n  \"usage\" \"hello\")"
     (13 19 font-lock-doc-face))

    ("(def foo \n  \"usage\" \n  \"hello\")"
     (13 19 font-lock-doc-face)
     (24 30 font-lock-string-face))

    ("(def test-string\n  \"this\\n\n  is\n  my\n  string\")"
     (20 24 font-lock-string-face)
     (25 26 font-lock-string-face)
     (27 46 font-lock-string-face)))

  (when-fontifying-it "variable-def-with-metadata-and-docstring"
    ("^{:foo bar}(def foo \n  \"usage\" \n  \"hello\")"
     (13 15 font-lock-keyword-face)
     (17 19 font-lock-variable-name-face)
     (24 30 font-lock-doc-face)
     (35 41 font-lock-string-face)))

  (when-fontifying-it "defn-with-metadata-and-docstring"
    ("^{:foo bar}(defn foo \n  \"usage\" \n [] \n \"hello\")"
     (13 16 font-lock-keyword-face)
     (18 20 font-lock-function-name-face)
     (25 31 font-lock-doc-face)
     (40 46 font-lock-string-face)))

  (when-fontifying-it "fn-with-name"
    ("(fn named-lambda [x] x)"
     ("fn" font-lock-keyword-face)
     ("named-lambda" font-lock-function-name-face)))

  (when-fontifying-it "single-keyword-metadata"
    ("(def ^:private my-private-var true)"
     ("def" font-lock-keyword-face)
     ("^" font-lock-operator-face)
     ("private" clojure-ts-keyword-face)
     ("my-private-var" font-lock-variable-name-face)
     ("true" font-lock-constant-face)))

  (when-fontifying-it "built-ins"
    ("(for [x [1 2 3]] x)"
     ("for" font-lock-keyword-face))

    ("(clojure.core/for [x [1 2 3]] x)"
     ("clojure.core" font-lock-type-face)
     ("for" font-lock-keyword-face)))

  (when-fontifying-it "non-built-ins-with-same-name"
    ("(h/for query {})"
     ("h" font-lock-type-face)
     ("for" nil)))

  (when-fontifying-it "special-forms-with-metadata"
    ("^long (if true 1 2)"
     ("long" font-lock-type-face)
     ("if" font-lock-keyword-face)))

  (when-fontifying-it "old-style-metadata"
    ("(def #^:private my-var true)"
     ("#^" font-lock-operator-face)
     ("private" clojure-ts-keyword-face)))

  (when-fontifying-it "dynamic-variables"
    ("*out*" ("*out*" font-lock-builtin-face))
    ("*in*" ("*in*" font-lock-builtin-face))
    ("*ns*" ("*ns*" font-lock-builtin-face))
    ("*err*" ("*err*" font-lock-builtin-face)))

  (when-fontifying-it "metadata-docstring-for-definline"
    ("(definline ^{:doc \"my doc\"} my-fn [x] x)"
     (":doc" clojure-ts-keyword-face)
     ("my doc" font-lock-doc-face)
     ("my-fn" font-lock-function-name-face)))

  (when-fontifying-it "function literals"
    ("#(or one two)"
     ("or" font-lock-keyword-face)))

  (when-fontifying-it "should highlight function name in all known forms"
    ("(letfn [(add [x y]
          (+ x y))
        (hello [user]
          (println \"Hello\" user))]
  (dotimes [_ (add 6 8)]
    (hello \"John Doe\")))"
     ("letfn" font-lock-keyword-face)
     ("add" font-lock-function-name-face)
     ("hello" font-lock-function-name-face))

    ("(reify
  AutoCloseable
  (close [this] (.close this)))"
     ("reify" font-lock-keyword-face)
     ;; Skip "close" in "AutoCloseable", match the method name
     ("close" nil)
     ("close" font-lock-function-name-face))

    ("(defrecord TestRecord [field]
  AutoCloseable
  (close [this]
    (.close this)))"
     ("defrecord" font-lock-keyword-face)
     ("TestRecord" font-lock-type-face)
     ;; Skip "close" in "AutoCloseable", match the method name
     ("close" nil)
     ("close" font-lock-function-name-face))

    ("(definterface MyInterface
  (^String name [])
  (^double mass []))"
     ("definterface" font-lock-keyword-face)
     ("MyInterface" font-lock-type-face)
     ("String" font-lock-type-face)
     ("name" font-lock-function-name-face)
     ("double" font-lock-type-face)
     ("mass" font-lock-function-name-face))

    ("(deftype ImageSelection [data]
  Transferable
  (getTransferDataFlavors
    [this]
    (into-array DataFlavor [DataFlavor/imageFlavor])))"
     ("deftype" font-lock-keyword-face)
     ("ImageSelection" font-lock-type-face)
     ("getTransferDataFlavors" font-lock-function-name-face))

    ("(defprotocol P
  (foo [this])
  (bar-me [this] [this y]))"
     ("defprotocol" font-lock-keyword-face)
     ("P" font-lock-type-face)
     ("foo" font-lock-function-name-face)
     ("bar-me" font-lock-function-name-face))

    ("(extend-protocol prepare/SettableParameter
    clojure.lang.IPersistentMap
    (set-parameter [m ^PreparedStatement s i]
      (.setObject s i (->pgobject m))))"
     ("set-parameter" font-lock-function-name-face))))

;;;; Numbers

(describe "number-highlighting"
  (when-fontifying-it "should highlight numeric literals"
    ("42" ("42" font-lock-number-face))
    ("3.14" ("3.14" font-lock-number-face))
    ("0xFF" ("0xFF" font-lock-number-face))
    ("1/3" ("1/3" font-lock-number-face))
    ("1e10" ("1e10" font-lock-number-face))
    ("36rCRAZY" ("36rCRAZY" font-lock-number-face))
    ("1N" ("1N" font-lock-number-face))
    ("1.0M" ("1.0M" font-lock-number-face))))

;;;; Characters

(describe "character-highlighting"
  (when-fontifying-it "should highlight character literals"
    ("\\a" ("\\a" clojure-ts-character-face))
    ("\\newline" ("\\newline" clojure-ts-character-face))
    ("\\space" ("\\space" clojure-ts-character-face))
    ("\\tab" ("\\tab" clojure-ts-character-face))
    ("\\u0041" ("\\u0041" clojure-ts-character-face))))

;;;; Constants

(describe "constant-highlighting"
  (when-fontifying-it "should highlight boolean and nil literals"
    ("true" ("true" font-lock-constant-face))
    ("false" ("false" font-lock-constant-face))
    ("nil" ("nil" font-lock-constant-face))))

;;;; Keywords

(describe "keyword-highlighting"
  (when-fontifying-it "should highlight keywords"
    (":foo" ("foo" clojure-ts-keyword-face))
    ("::foo" ("foo" clojure-ts-keyword-face))
    (":my.ns/bar" ("my.ns" font-lock-type-face)
                  ("bar" clojure-ts-keyword-face))))

;;;; Strings

(describe "string-highlighting"
  (when-fontifying-it "should highlight string literals"
    ("\"hello\"" (1 7 font-lock-string-face))
    ("\"hello\\nworld\"" (1 14 font-lock-string-face))))

;;;; Comments

(describe "comment-highlighting"
  (when-fontifying-it "should highlight comments"
    ("; comment" ("; comment" font-lock-comment-face))
    (";; comment" (";; comment" font-lock-comment-face))
    (";;; heading" (";;; heading" font-lock-comment-face)))

  (when-fontifying-it "should highlight discard expressions"
    ("#_foo" ("#_" font-lock-comment-delimiter-face)
            ("foo" font-lock-comment-face))
    ("#_(+ 1 2)" ("#_" font-lock-comment-delimiter-face)
                 ("(+ 1 2)" font-lock-comment-face)))

  (it "should highlight comment macro name as delimiter"
    (with-fontified-clojure-ts-buffer "(comment (+ 1 2))"
      (expect (get-text-property 2 'face)
              :to-equal 'font-lock-comment-delimiter-face))))

;;;; Quote operators

(describe "quote-highlighting"
  (when-fontifying-it "should highlight quote markers"
    ("'foo" (1 1 font-lock-delimiter-face))
    ("`foo" (1 1 font-lock-delimiter-face))
    ("~foo" (1 1 font-lock-delimiter-face))
    ("~@foo" (1 2 font-lock-delimiter-face))
    ("#'foo" (1 2 font-lock-delimiter-face))))

;;;; Level 4 features (bracket, deref, function, tagged-literals)

(describe "level-4-font-locking"
  (it "should highlight brackets at level 4"
    (with-temp-buffer
      (insert "(foo [1 2] {:a 1} #{3})")
      (let ((treesit-font-lock-level 4))
        (clojure-ts-mode))
      (font-lock-ensure)
      ;; Opening paren
      (expect (get-text-property 1 'face)
              :to-equal 'font-lock-bracket-face)
      ;; Opening bracket
      (expect (get-text-property 6 'face)
              :to-equal 'font-lock-bracket-face)
      ;; Opening brace
      (expect (get-text-property 12 'face)
              :to-equal 'font-lock-bracket-face)))

  (it "should highlight deref operator at level 4"
    (with-temp-buffer
      (insert "@my-atom")
      (let ((treesit-font-lock-level 4))
        (clojure-ts-mode))
      (font-lock-ensure)
      (expect (get-text-property 1 'face)
              :to-equal 'font-lock-warning-face)))

  (it "should highlight function calls at level 4"
    (with-temp-buffer
      (insert "(map inc [1 2 3])")
      (let ((treesit-font-lock-level 4))
        (clojure-ts-mode))
      (font-lock-ensure)
      (expect (get-text-property 2 'face)
              :to-equal 'font-lock-function-call-face)))

  (it "should highlight tagged literals at level 4"
    (with-temp-buffer
      (insert "#inst \"2024-01-01\"")
      (let ((treesit-font-lock-level 4))
        (clojure-ts-mode))
      (font-lock-ensure)
      ;; # marker
      (expect (get-text-property 1 'face)
              :to-equal 'font-lock-preprocessor-face)
      ;; inst tag
      (expect (get-text-property 2 'face)
              :to-equal 'font-lock-preprocessor-face)))

  (it "should highlight set literal # at level 4"
    (with-temp-buffer
      (insert "#{1 2 3}")
      (let ((treesit-font-lock-level 4))
        (clojure-ts-mode))
      (font-lock-ensure)
      (expect (get-text-property 1 'face)
              :to-equal 'font-lock-bracket-face))))

;;;; Regex

(describe "regex-highlighting"
  (when-fontifying-it "should highlight regex literals"
    ("#\"pattern\"" (1 10 font-lock-regexp-face))))

;;;; Builtin macros

(describe "builtin-macro-highlighting"
  (when-fontifying-it "should highlight all builtin macros as keywords"
    ("(io! (println \"hi\"))" ("io!" font-lock-keyword-face))
    ("(sync nil (println \"hi\"))" ("sync" font-lock-keyword-face))
    ("(in-ns 'foo)" ("in-ns" font-lock-keyword-face))
    ("(let [a 1] a)" ("let" font-lock-keyword-face))
    ("(when true 1)" ("when" font-lock-keyword-face))
    ("(fn [x] x)" ("fn" font-lock-keyword-face))
    ("(some->> x inc)" ("some->>" font-lock-keyword-face))
    ("(some-> x inc)" ("some->" font-lock-keyword-face))))

;;;; Extra def forms

(describe "clojure-ts-extra-def-forms"
  (it "should respect the value of clojure-ts-extra-def-forms"
    (with-clojure-ts-buffer "(defelem file-upload
  \"Creates a file upload input.\"
  [name]
  (input-field \"file\" name nil))"
      (setopt clojure-ts-extra-def-forms '("defelem"))
      (clojure-ts-mode)
      (font-lock-ensure)
      (goto-char (point-min))
      (expect (get-text-property 2 'face)
              :to-equal 'font-lock-keyword-face)
      (expect (get-text-property 10 'face)
              :to-equal 'font-lock-function-name-face)
      (expect (get-text-property 25 'face)
              :to-equal 'font-lock-doc-face))))

;;;; Docstrings in various def forms

(describe "docstring-highlighting"
  (when-fontifying-it "should highlight docstrings in defmacro"
    ("(defmacro my-macro\n  \"A macro docstring.\"\n  [& body] body)"
     ("A macro docstring." font-lock-doc-face)))

  (when-fontifying-it "should highlight docstrings in defmulti"
    ("(defmulti my-multi\n  \"A multi docstring.\"\n  :type)"
     ("A multi docstring." font-lock-doc-face)))

  (when-fontifying-it "should highlight docstrings in defprotocol"
    ("(defprotocol MyProto\n  \"A protocol docstring.\"\n  (foo [this]))"
     ("A protocol docstring." font-lock-doc-face)))

  (when-fontifying-it "should highlight docstrings in ns"
    ("(ns my.ns\n  \"A namespace docstring.\")"
     ("A namespace docstring." font-lock-doc-face)))

  (when-fontifying-it "should highlight docstrings in defprotocol methods"
    ("(defprotocol P\n  (foo [this]\n    \"Method docstring.\"))"
     ("Method docstring." font-lock-doc-face))))

;;;; Comment macro font-locking

(describe "clojure-ts-comment-macro-font-lock-body"
  (it "should not highlight comment body by default"
    (with-clojure-ts-buffer "(comment (+ 1 2))"
      (font-lock-ensure)
      ;; comment symbol itself should be comment-delimiter-face
      (expect (get-text-property 2 'face)
              :to-equal 'font-lock-comment-delimiter-face)
      ;; Body should NOT be comment-face (default is nil)
      (expect (get-text-property 10 'face)
              :not :to-equal 'font-lock-comment-face))))
