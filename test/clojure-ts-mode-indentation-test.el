;;; clojure-ts-mode-indentation-test.el --- Clojure TS Mode: indentation test suite  -*- lexical-binding: t; -*-

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
(require 's nil t)               ;Don't burp if it's missing during compilation.


(defmacro when-indenting-with-point-it (description before after)
  "Return a buttercup spec.

Check whether the swift indentation command will correctly change the buffer.
Will also check whether point is moved to the expected position.

BEFORE is the buffer string before indenting, where a pipe (|) represents
point.

AFTER is the expected buffer string after indenting, where a pipe (|)
represents the expected position of point.

DESCRIPTION is a string with the description of the spec."
  (declare (indent 1))
  `(it ,description
    (let* ((after ,after)
           (expected-cursor-pos (1+ (clojure-ts--s-index-of "|" after)))
           (expected-state (delete ?| after)))
      (with-clojure-ts-buffer ,before
        (goto-char (point-min))
        (search-forward "|")
        (delete-char -1)
        (font-lock-ensure)
        (indent-according-to-mode)
        (expect (buffer-string) :to-equal expected-state)
        (expect (point) :to-equal expected-cursor-pos)))))



;; Backtracking indent
(defmacro when-indenting-it (description &rest forms)
  "Return a buttercup spec.

Check that all FORMS correspond to properly indented sexps.

DESCRIPTION is a string with the description of the spec."
  (declare (indent 1))
  `(it ,description
     (progn
       ,@(mapcar (lambda (form)
                   `(with-temp-buffer
                      (clojure-ts-mode)
                      (insert "\n" ,form);,(replace-regexp-in-string "\n +" "\n " form))
                      (indent-region (point-min) (point-max))
                      (expect (buffer-string) :to-equal ,(concat "\n" form))))
                 forms))))


;; Provide font locking for easier test editing.

(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(rx "(" (group "when-indenting-with-point-it") eow)
    (1 font-lock-keyword-face))
   (,(rx "("
         (group "when-indenting-with-point-it") (+ space)
         (group bow (+ (not space)) eow)
         )
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face))))


;; Mock `cider--get-symbol-indent' function

(defun cider--get-symbol-indent-mock (symbol-name)
  "Returns static mocked indentation specs for SYMBOL-NAME if available."
  (when (stringp symbol-name)
    (cond
     ((string-equal symbol-name "my-with-in-str") 1)
     ((string-equal symbol-name "my-letfn") '(1 ((:defn) (:form)))))))


(describe "indentation"
  (it "should not hang on end of buffer"
    (with-clojure-ts-buffer "(let [a b]"
      (goto-char (point-max))
      (expect
       (with-timeout (2)
         (newline-and-indent)
         t))))

  (when-indenting-with-point-it "should have no indentation at top level"
    "|x"

    "|x")

  (when-indenting-it "should handle non-symbol at start"
    "
{\"1\" 2
 *3 4}")

  (when-indenting-it "should have no indentation at top level lists with metadata"
    "
^{:foo true}
(def b 2)")

  (when-indenting-it "should have no indentation at top level vectors with metadata"
    "
^{:foo true}
[1 2]")

  (when-indenting-it "should have no indentation at top level maps with metadata"
    "
^{:foo true}
{:a 1}")

  (when-indenting-it "should have no indentation with metadata inside comment"
    "
(comment
  ^{:a 1}
  (def a 2))")

  (when-indenting-it "should have params, docstring and body correctly indented in presence of metadata"
    "
^{:foo true}
(defn c
  \"hello\"
  [_foo]
  (+ 1 1))")

(when-indenting-it "should support function calls via vars"
   "
(#'foo 5
       6)")

(when-indenting-it "should support block-0 expressions"
  "
(do (aligned)
    (vertically))"

  "
(do
  (indented)
  (with-2-spaces))"

  "
(future
  (body is indented))"

  "
(try
  (something)
  ;; A bit of block 2 rule
  (catch Exception e
    \"Third argument is indented with 2 spaces.\")
  (catch ExceptionInfo
         e-info
    \"Second argument is aligned vertically with the first one.\"))")

(when-indenting-it "should support block-1 expressions"
  "
(case x
  2 (print 2)
  3 (print 3)
  (print \"Default\"))"

  "
(cond-> {}
  :always (assoc :hello \"World\")
  false (do nothing))"

  "
(with-precision 32
  (/ (bigdec 20) (bigdec 30)))"

  "
(testing \"Something should work\"
  (is (something-working?)))")

(when-indenting-it "should support block-2 expressions"
  "
(are [x y]
     (= x y)
  2 3
  4 5
  6 6)"

  "
(as-> {} $
  (assoc $ :hello \"World\"))"

  "
(as-> {}
      my-map
  (assoc my-map :hello \"World\"))"

  "
(defrecord MyThingR []
  IProto
  (foo [this x] x))")

(when-indenting-it "should support inner-0 expressions"
  "
(fn named-lambda [x]
  (+ x x))"

  "
(defmethod hello :world
  [arg1 arg2]
  (+ arg1 arg2))"

  "
(reify
  AutoCloseable
  (close
    [this]
    (is properly indented)))")

(it "should prioritize custom semantic indentation rules"
  (with-clojure-ts-buffer "
(are [x y]
     (= x y)
  2 3
  4 5
  6 6)"
    (setopt clojure-ts-semantic-indent-rules '(("are" . (:block 1))))
    (indent-region (point-min) (point-max))
    (expect (buffer-string) :to-equal "
(are [x y]
  (= x y)
  2 3
  4 5
  6 6)")))

(it "should indent collections elements with metadata correctly"
  "
(def x
  [a b [c ^:foo
        d
        e]])"

  "
#{x
  y ^:foo
  z}"

  "
{:hello ^:foo
 \"world\"
 :foo
 \"bar\"}")

(it "should indent body of special forms correctly considering metadata"
  "
(let [result ^long
      (if true
        1
        2)])")

(it "should pick up dynamic indentation rules from clojure-ts-get-indent-function"
  (with-clojure-ts-buffer "
(defmacro my-with-in-str
  \"[DOCSTRING]\"
  {:style/indent 1}
  [s & body]
  ~@body)

(my-with-in-str \"34\"
(prompt \"How old are you?\"))"
    (setq-local clojure-ts-get-indent-function #'cider--get-symbol-indent-mock)
    (indent-region (point-min) (point-max))
    (expect (buffer-string) :to-equal "
(defmacro my-with-in-str
  \"[DOCSTRING]\"
  {:style/indent 1}
  [s & body]
  ~@body)

(my-with-in-str \"34\"
  (prompt \"How old are you?\"))"))

  (with-clojure-ts-buffer "
(defmacro my-letfn
  \"[DOCSTRING]\"
  {:style/indent [1 [[:defn]] :form]}
  [fnspecs & body]
  ~@body)

(my-letfn [(twice [x] (* x 2))
           (six-times [y] (* (twice y) 3))]
(println \"Twice 15 =\" (twice 15))
(println \"Six times 15 =\" (six-times 15)))"
    (setq-local clojure-ts-get-indent-function #'cider--get-symbol-indent-mock)
    (indent-region (point-min) (point-max))
    (expect (buffer-string) :to-equal "
(defmacro my-letfn
  \"[DOCSTRING]\"
  {:style/indent [1 [[:defn]] :form]}
  [fnspecs & body]
  ~@body)

(my-letfn [(twice [x] (* x 2))
           (six-times [y] (* (twice y) 3))]
  (println \"Twice 15 =\" (twice 15))
  (println \"Six times 15 =\" (six-times 15)))"))))
