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

(defun clojure-ts-get-face-at (start end content)
  "Get the face between START and END in CONTENT."
  (with-fontified-clojure-ts-buffer content
    (let ((start-face (get-text-property start 'face))
          (all-faces (cl-loop for i from start to end collect (get-text-property
                                                               i 'face))))
      (if (cl-every (lambda (face) (eq face start-face)) all-faces)
          start-face
        'various-faces))))

(defun expect-face-at (content start end face)
  "Expect face in CONTENT between START and END to be equal to FACE."
  (expect (clojure-ts-get-face-at start end content) :to-equal face))

(defun expect-faces-at (content &rest faces)
  "Expect FACES in CONTENT.

FACES is a list of the form (content (start end expected-face)*)"
  (dolist (face faces)
    (apply (apply-partially #'expect-face-at content) face)))

(defmacro when-fontifying-it (description &rest tests)
  "Return a buttercup spec.

TESTS are lists of the form (content (start end expected-face)*).  For each test
check that each `expected-face` is found in `content` between `start` and `end`.

DESCRIPTION is the description of the spec."
  (declare (indent 1))
  `(it ,description
     (dolist (test (quote ,tests))
       (apply #'expect-faces-at test))))

;;;; Font locking

(describe "clojure-ts-mode-syntax-table"
  (when-fontifying-it "should handle any known def form"
    ("(def a 1)" (2 4 font-lock-keyword-face))
    ("(defonce a 1)" (2 8 font-lock-keyword-face))
    ("(defn a [b])" (2 5 font-lock-keyword-face))
    ("(defmacro a [b])" (2 9 font-lock-keyword-face))
    ("(definline a [b])" (2 10 font-lock-keyword-face))
    ("(defmulti a identity)" (2 9 font-lock-keyword-face))
    ("(defmethod a :foo [b] (println \"bar\"))" (2 10 font-lock-keyword-face))
    ("(defprotocol a (b [this] \"that\"))" (2 12 font-lock-keyword-face))
    ("(definterface a (b [c]))" (2 13 font-lock-keyword-face))
    ("(defrecord a [b c])" (2 10 font-lock-keyword-face))
    ("(deftype a [b c])" (2 8 font-lock-keyword-face))
    ("(defstruct a :b :c)" (2 10 font-lock-keyword-face))
    ("(deftest a (is (= 1 1)))" (2 8 font-lock-keyword-face))


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
     (2 3 font-lock-keyword-face)
     (5 16 font-lock-function-name-face)))

  (when-fontifying-it "single-keyword-metadata"
    ("(def ^:private my-private-var true)"
     (2 4 font-lock-keyword-face)
     (6 6 font-lock-operator-face)
     (7 14 clojure-ts-keyword-face)
     (16 29 font-lock-variable-name-face)
     (31 34 font-lock-constant-face)))

  (when-fontifying-it "built-ins"
    ("(for [x [1 2 3]] x)"
     (2 4 font-lock-keyword-face))

    ("(clojure.core/for [x [1 2 3]] x)"
     (2 13 font-lock-type-face)
     (15 17 font-lock-keyword-face)))

  (when-fontifying-it "non-built-ins-with-same-name"
    ("(h/for query {})"
     (2 2 font-lock-type-face)
     (4 6 nil)))

  (when-fontifying-it "special-forms-with-metadata"
    ("^long (if true 1 2)"
     (2 5 font-lock-type-face)
     (8 9 font-lock-keyword-face)))

  (when-fontifying-it "function literals"
    ("#(or one two)"
     (3 4 font-lock-keyword-face)))

  (when-fontifying-it "should highlight function name in all known forms"
    ("(letfn [(add [x y]
          (+ x y))
        (hello [user]
          (println \"Hello\" user))]
  (dotimes [_ (add 6 8)]
    (hello \"John Doe\")))"
     (2 6 font-lock-keyword-face)
     (10 12 font-lock-function-name-face)
     (48 52 font-lock-function-name-face))

    ("(reify
  AutoCloseable
  (close [this] (.close this)))"
     (2 6 font-lock-keyword-face)
     (27 31 font-lock-function-name-face))

    ("(defrecord TestRecord [field]
  AutoCloseable
  (close [this]
    (.close this)))"
     (2 10 font-lock-keyword-face)
     (12 21 font-lock-type-face)
     (50 54 font-lock-function-name-face))

    ("(definterface MyInterface
  (^String name [])
  (^double mass []))"
     (2 13 font-lock-keyword-face)
     (15 25 font-lock-type-face)
     (31 36 font-lock-type-face)
     (38 41 font-lock-function-name-face)
     (51 56 font-lock-type-face)
     (58 61 font-lock-function-name-face))

    ("(deftype ImageSelection [data]
  Transferable
  (getTransferDataFlavors
    [this]
    (into-array DataFlavor [DataFlavor/imageFlavor])))"
     (2 8 font-lock-keyword-face)
     (10 23 font-lock-type-face)
     (50 71 font-lock-function-name-face))

    ("(defprotocol P
  (foo [this])
  (bar-me [this] [this y]))"
     (2 12 font-lock-keyword-face)
     (14 14 font-lock-type-face)
     (19 21 font-lock-function-name-face)
     (34 39 font-lock-function-name-face))

    ("(extend-protocol prepare/SettableParameter
    clojure.lang.IPersistentMap
    (set-parameter [m ^PreparedStatement s i]
      (.setObject s i (->pgobject m))))"
     (81 93 font-lock-function-name-face))))

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
