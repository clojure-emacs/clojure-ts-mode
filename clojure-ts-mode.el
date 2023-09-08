;;; clojure-ts-mode.el --- Major mode for Clojure code -*- lexical-binding: t; -*-

;; Copyright © 2022-2023 Danny Freeman
;;
;; Authors: Danny Freeman <danny@dfreeman.email>
;; Maintainer: Danny Freeman <danny@dfreeman.email>
;; URL: http://github.com/clojure-emacs/clojure-ts-mode
;; Keywords: languages clojure clojurescript lisp
;; Version: 0.1.5
;; Package-Requires: ((emacs "29"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock, indentation, and navigation for the
;; Clojure programming language (http://clojure.org).

;; For the tree-sitter grammar this mode is based on,
;; see https://github.com/sogaiu/tree-sitter-clojure.

;; Using clojure-ts-mode with paredit or smartparens is highly recommended.

;; Here are some example configurations:

;;   ;; require or autoload paredit-mode
;;   (add-hook 'clojure-ts-mode-hook #'paredit-mode)

;;   ;; require or autoload smartparens
;;   (add-hook 'clojure-ts-mode-hook #'smartparens-strict-mode)

;; See inf-clojure (http://github.com/clojure-emacs/inf-clojure) for
;; basic interaction with Clojure subprocesses.

;; See CIDER (http://github.com/clojure-emacs/cider) for
;; better interaction with subprocesses via nREPL.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'treesit)
(require 'lisp-mnt)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-prev-sibling "treesit.c")

(defgroup clojure-ts nil
  "Major mode for editing Clojure code with tree-sitter."
  :prefix "clojure-ts-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/clojure-emacs/clojure-ts-mode")
  :link '(emacs-commentary-link :tag "Commentary" "clojure-mode"))

(defconst clojure-ts-mode-version
  (eval-when-compile
    (lm-version (or load-file-name buffer-file-name)))
  "The current version of `clojure-ts-mode'.")

(defcustom clojure-ts-comment-macro-font-lock-body nil
  "Highlight the entire body of a comment macro as a comment.

When set to a non-nil value, applies the comment font-locking face to the entire
body of comment macros.
When nil (the default), the body of comment macros uses default font-locking
rules for whatever expressions are in the body, except for the comment symbol
itself."
  :safe #'booleanp
  :type 'boolean
  :package-version '(clojure-ts-mode . "0.1.3"))

(defvar clojure-ts--debug nil
  "Enables debugging messages, shows current node in mode-line.
Only intended for use at development time.")

(defvar clojure-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Initialize ASCII charset as symbol syntax
    (modify-syntax-entry '(0 . 127) "_" table)

    ;; Word syntax
    (modify-syntax-entry '(?0 . ?9) "w" table)
    (modify-syntax-entry '(?a . ?z) "w" table)
    (modify-syntax-entry '(?A . ?Z) "w" table)

    ;; Whitespace
    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\xa0 " " table) ; non-breaking space
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\f " " table)
    ;; Setting commas as whitespace makes functions like `delete-trailing-whitespace' behave unexpectedly (#561)
    (modify-syntax-entry ?, "." table)

    ;; Delimiters
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    ;; Prefix chars
    (modify-syntax-entry ?` "'" table)
    (modify-syntax-entry ?~ "'" table)
    (modify-syntax-entry ?^ "'" table)
    (modify-syntax-entry ?@ "'" table)
    (modify-syntax-entry ?? "_ p" table) ; ? is a prefix outside symbols
    (modify-syntax-entry ?# "_ p" table) ; # is allowed inside keywords (#399)
    (modify-syntax-entry ?' "_ p" table) ; ' is allowed anywhere but the start of symbols

    ;; Others
    (modify-syntax-entry ?\; "<" table) ; comment start
    (modify-syntax-entry ?\n ">" table) ; comment end
    (modify-syntax-entry ?\" "\"" table) ; string
    (modify-syntax-entry ?\\ "\\" table) ; escape

    table)
  "Syntax table for clojure-ts-mode.")


(defconst clojure-ts--builtin-dynamic-var-regexp
  (eval-and-compile
    (concat "^"
            (regexp-opt
             '("*1" "*2" "*3" "*agent*"
               "*allow-unresolved-vars*" "*assert*" "*clojure-version*"
               "*command-line-args*" "*compile-files*"
               "*compile-path*" "*data-readers*" "*default-data-reader-fn*"
               "*e" "*err*" "*file*" "*flush-on-newline*"
               "*in*" "*macro-meta*" "*math-context*" "*ns*" "*out*"
               "*print-dup*" "*print-length*" "*print-level*"
               "*print-meta*" "*print-readably*"
               "*read-eval*" "*source-path*"
               "*unchecked-math*"
               "*use-context-classloader*" "*warn-on-reflection*"))
            "$")))

(defconst clojure-ts--builtin-symbol-regexp
  (eval-and-compile
    (concat "^"
            (regexp-opt
             '("do" "if" "let*" "var"
               "fn" "fn*" "loop*" "recur"
               "throw" "try" "catch" "finally"
               "set!" "new"
               "monitor-enter" "monitor-exit"
               "quote"

               "->" "->>" ".." "."
               "amap" "and" "areduce" "as->" "assert"
               "binding" "bound-fn"
               "case" "comment" "cond" "cond->" "cond->>" "condp"
               "declare" "def" "definline" "definterface" "defmacro" "defmethod"
               "defmulti" "defn" "defn-" "defonce" "defprotocol" "defrecord"
               "defstruct" "deftype"
               "delay" "doall" "dorun" "doseq" "dosync" "dotimes" "doto"
               "extend-protocol" "extend-type"
               "for" "future"
               "gen-class" "gen-interface"
               "if-let" "if-not" "if-some" "import" "in-ns""io!"
               "lazy-cat" "lazy-seq" "let" "letfn" "locking" "loop"
               "memfn" "ns" "or"
               "proxy" "proxy-super" "pvalues"
               "refer-clojure" "reify"
               "some->" "some->>""sync"
               "time" "vswap!"
               "when" "when-first" "when-let" "when-not" "when-some" "while"
               "with-bindings" "with-in-str" "with-loading-context"
               "with-local-vars" "with-open" "with-out-str" "with-precision"
               "with-redefs" "with-redefs-fn"))
            "$")))

(defface clojure-ts-keyword-face
  '((t (:inherit font-lock-constant-face)))
  "Face used to font-lock Clojure keywords (:something).")

(defface clojure-ts-character-face
  '((t (:inherit font-lock-string-face)))
  "Face used to font-lock Clojure character literals.")

(defconst clojure-ts--definition-keyword-regexp
  (rx
   line-start
   (or (group (or "ns" "fn"))
       (group "def"
              (+ (or alnum
                     ;; What are valid characters for symbols? is a negative match better?
                     "-" "_" "!" "@" "#" "$" "%" "^" "&" "*" "|" "?" "<" ">" "+" "=" ":"))))
   line-end))

(defconst clojure-ts--variable-keyword-regexp
  (rx line-start (or "def" "defonce") line-end))

(defconst clojure-ts--type-keyword-regexp
  (rx line-start
      (or "defprotocol" "defmulti" "deftype" "defrecord"
          "definterface" "defmethod" "defstruct")
      line-end))

(defun clojure-ts--font-lock-settings ()
  "Return font lock settings suitable for use in `treesit-font-lock-settings'."
  (treesit-font-lock-rules
   :feature 'string
   :language 'clojure
   '((str_lit) @font-lock-string-face
     (regex_lit) @font-lock-regexp-face)

   :feature 'regex
   :language 'clojure
   :override t
   '((regex_lit marker: _ @font-lock-property-face))

   :feature 'number
   :language 'clojure
   '((num_lit) @font-lock-number-face)

   :feature 'constant
   :language 'clojure
   '([(bool_lit) (nil_lit)] @font-lock-constant-face)

   :feature 'char
   :language 'clojure
   '((char_lit) @clojure-ts-character-face)

   :feature 'keyword
   :language 'clojure
   '((kwd_ns) @font-lock-type-face
     (kwd_name) @clojure-ts-keyword-face
     (kwd_lit
      marker: _ @clojure-ts-keyword-face
      delimiter: _ :? @default))

   :feature 'builtin
   :language 'clojure
   `(((list_lit :anchor (sym_lit (sym_name) @font-lock-keyword-face))
      (:match ,clojure-ts--builtin-symbol-regexp @font-lock-keyword-face))
     ((sym_name) @font-lock-builtin-face
      (:match ,clojure-ts--builtin-dynamic-var-regexp @font-lock-builtin-face)))

   ;; Any function calls, not built-ins.
   ;; This can give false positives (macros, quoted lists, namespace imports)
   ;; but is a level 4 feature and never enabled by default.
   :feature 'function
   :language 'clojure
   '((list_lit :anchor (sym_lit (sym_name) @font-lock-function-call-face)))

   :feature 'symbol
   :language 'clojure
   '((sym_ns) @font-lock-type-face)

   ;; How does this work for defns nested in other forms, not at the top level?
   ;; Should I match against the source node to only hit the top level? Can that be expressed?
   ;; What about valid usages like `(let [closed 1] (defn +closed [n] (+ n closed)))'??
   ;; No wonder the tree-sitter-clojure grammar only touches syntax, and not semantics
   :feature 'definition ;; defn and defn like macros
   :language 'clojure
   `(((list_lit :anchor (sym_lit (sym_name) @def)
                :anchor (sym_lit (sym_name) @font-lock-function-name-face))
      (:match ,clojure-ts--definition-keyword-regexp @def))
     ((anon_fn_lit
       marker: "#" @font-lock-property-face)))

   :feature 'variable ;; def, defonce
   :language 'clojure
   `(((list_lit :anchor (sym_lit (sym_name) @def)
                :anchor (sym_lit (sym_name) @font-lock-variable-name-face))
      (:match ,clojure-ts--variable-keyword-regexp @def)))

   ;; Can we support declarations in the namespace form?
   :feature 'type
   :language 'clojure
   `(;; Type Declarations
     ((list_lit :anchor (sym_lit (sym_name) @def)
                :anchor (sym_lit (sym_name) @font-lock-type-face))
      (:match ,clojure-ts--type-keyword-regexp @def))
     ;; Type Hints
     (meta_lit
      marker: "^" @font-lock-operator-face
      value: (sym_lit (sym_name) @font-lock-type-face))
     (old_meta_lit
      marker: "#^" @font-lock-operator-face
      value: (sym_lit (sym_name) @font-lock-type-face)))

   :feature 'metadata
   :language 'clojure
   :override t
   `((meta_lit
      marker: "^" @font-lock-operator-face
      value: (kwd_lit (kwd_name) @font-lock-property-name-face))
     (old_meta_lit
      marker: "#^" @font-lock-operator-face
      value: (kwd_lit (kwd_name) @font-lock-property-name-face)))

   :feature 'tagged-literals
   :language 'clojure
   :override t
   '((tagged_or_ctor_lit marker: "#" @font-lock-preprocessor-face
                         tag: (sym_lit) @font-lock-preprocessor-face))

   ;; TODO, also account for `def'
   ;; Figure out how to highlight symbols in docstrings.
   ;; Might require a markdown grammar
   :feature 'doc
   :language 'clojure
   :override t
   `(((list_lit :anchor (sym_lit) @def_symbol
                :anchor (sym_lit) @function_name
                :anchor (str_lit) @font-lock-doc-face)
      (:match ,clojure-ts--definition-keyword-regexp @def_symbol)))

   :feature 'quote
   :language 'clojure
   '((quoting_lit
      marker: _ @font-lock-delimiter-face)
     (var_quoting_lit
      marker: _ @font-lock-delimiter-face)
     (syn_quoting_lit
      marker: _ @font-lock-delimiter-face)
     (unquoting_lit
      marker: _ @font-lock-delimiter-face)
     (unquote_splicing_lit
      marker: _ @font-lock-delimiter-face)
     (var_quoting_lit
      marker: _ @font-lock-delimiter-face))

   :feature 'bracket
   :language 'clojure
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face
     (set_lit :anchor "#" @font-lock-bracket-face))

   :feature 'comment
   :language 'clojure
   :override t
   `((comment) @font-lock-comment-face
     (dis_expr
      marker: "#_" @font-lock-comment-delimiter-face
      value: _ @font-lock-comment-face)
     (,(append
        '(list_lit :anchor (sym_lit) @font-lock-comment-delimiter-face)
        (when clojure-ts-comment-macro-font-lock-body
          '(_ :* @font-lock-comment-face)))
      (:match "^\\(\\(clojure.core/\\)?comment\\)$" @font-lock-comment-delimiter-face)))

   :feature 'deref ;; not part of clojure-mode, but a cool idea?
   :language 'clojure
   '((derefing_lit
      marker: "@" @font-lock-warning-face))))

;; Node predicates

(defun clojure-ts--list-node-p (node)
  "Return non-nil if NODE is a Clojure list."
  (string-equal "list_lit" (treesit-node-type node)))

(defun clojure-ts--symbol-node-p (node)
  "Return non-nil if NODE is a Clojure symbol."
  (string-equal "sym_lit" (treesit-node-type node)))

(defun clojure-ts--keyword-node-p (node)
  "Return non-nil if NODE is a Clojure keyword."
  (string-equal "kwd_lit" (treesit-node-type node)))

(defun clojure-ts--named-node-text (node)
  "Gets the name of a symbol or keyword NODE.
This does not include the NODE's namespace."
  (treesit-node-text (treesit-node-child-by-field-name node "name")))

(defun clojure-ts--symbol-named-p (expected-symbol-name node)
  "Return non-nil if NODE is a symbol with text matching EXPECTED-SYMBOL-NAME."
  (and (clojure-ts--symbol-node-p node)
       (string-equal expected-symbol-name (clojure-ts--named-node-text node))))

(defun clojure-ts--symbol-matches-p (symbol-regexp node)
  "Return non-nil if NODE is a symbol that matches SYMBOL-REGEXP."
  (and (clojure-ts--symbol-node-p node)
       (string-match-p symbol-regexp (clojure-ts--named-node-text node))))

(defun clojure-ts--definition-node-p (definition-type-name node)
  "Return non-nil if NODE is a definition, defined by DEFINITION-TYPE-NAME.
DEFINITION-TYPE-NAME might be a string like defn, def, defmulti, etc.
See `clojure-ts--definition-node-match-p'  when an exact match is not desired."
  (and
   (clojure-ts--list-node-p node)
   (clojure-ts--symbol-named-p definition-type-name (treesit-node-child node 0 t))))

(defun clojure-ts--definition-node-match-p (definition-type-regexp node)
  "Return non-nil if NODE is a definition matching DEFINITION-TYPE-REGEXP.
DEFINITION-TYPE-REGEXP matched the symbol used to construct the definition,
like \"defn\".
See `clojure-ts--definition-node-p' when an exact match is possible."
  (and
   (clojure-ts--list-node-p node)
   (let* ((child (treesit-node-child node 0 t))
          (child-txt (clojure-ts--named-node-text child)))
     (and (clojure-ts--symbol-node-p child)
          (string-match-p definition-type-regexp child-txt)))))

(defun clojure-ts--standard-definition-node-name (node)
  "Return the definition name for the given NODE.
Returns nil if NODE is not a list with symbols as the first two children.
For example the node representing the expression (def foo 1) would return foo.
The node representing (ns user) would return user.
Does not does any matching on the first symbol (def, defn, etc), so identifying
that a node is a definition is intended to be done elsewhere.

Can be called directly, but intended for use as `treesit-defun-name-function'."
  (when (and (clojure-ts--list-node-p node)
             (clojure-ts--symbol-node-p (treesit-node-child node 0 t)))
    (let ((sym (treesit-node-child node 1 t)))
      (when (clojure-ts--symbol-node-p sym)
        ;; Extracts ns and name, and recreates the full var name.
        ;; We can't just get the node-text of the full symbol because
        ;; that could include metadata that isn't part of the name.
        (let ((ns (treesit-node-child-by-field-name sym "ns"))
              (name (treesit-node-child-by-field-name sym "name")))
          (if ns
              (concat (treesit-node-text ns) "/" (treesit-node-text name))
            (treesit-node-text name)))))))

(defvar clojure-ts--function-type-regexp
  (rx string-start (or "defn" "defmethod") string-end)
  "Regular expression for matching definition nodes that resemble functions.")

(defun clojure-ts--function-node-p (node)
  "Return non-nil if NODE is a defn form."
  (clojure-ts--definition-node-match-p clojure-ts--function-type-regexp node))

(defun clojure-ts--function-node-name (node)
  "Return the name of a function NODE.
Includes a dispatch value when applicable (defmethods)."
  (if (clojure-ts--definition-node-p "defmethod" node)
      (let ((dispatch-value (treesit-node-text (treesit-node-child node 2 t))))
        (concat (clojure-ts--standard-definition-node-name node)
                " "
                dispatch-value))
    (clojure-ts--standard-definition-node-name node)))

(defun clojure-ts--defmacro-node-p (node)
  "Return non-nil if NODE is a defmacro form."
  (clojure-ts--definition-node-p "defmacro" node))

(defun clojure-ts--ns-node-p (node)
  "Return non-nil if NODE is a ns form."
  (clojure-ts--definition-node-p "ns" node))

(defvar clojure-ts--variable-type-regexp
  (rx string-start (or "def" "defonce") string-end)
  "Regular expression for matching definition nodes that resemble variables.")

(defun clojure-ts--variable-node-p (node)
  "Return non-nil if NODE is a def or defonce form."
  (clojure-ts--definition-node-match-p clojure-ts--variable-type-regexp node))

(defvar clojure-ts--class-type-regexp
  (rx string-start (or "deftype" "defrecord" "defstruct") string-end)
  "Regular expression for matching definition nodes that resemble classes.")

(defun clojure-ts--class-node-p (node)
  "Return non-nil if NODE represents a type, record, or struct definition."
  (clojure-ts--definition-node-match-p clojure-ts--class-type-regexp node))

(defvar clojure-ts--interface-type-regexp
  (rx string-start (or "defprotocol" "definterface" "defmulti") string-end)
  "Regular expression for matching definition nodes that resemble interfaces.")

(defun clojure-ts--interface-node-p (node)
  "Return non-nil if NODE represents a protocol or interface definition."
  (clojure-ts--definition-node-match-p clojure-ts--interface-type-regexp node))


(defvar clojure-ts--imenu-settings
  `(("Namespace" "list_lit" clojure-ts--ns-node-p)
    ("Function" "list_lit" clojure-ts--function-node-p
     ;; Used instead of treesit-defun-name-function.
     clojure-ts--function-node-name)
    ("Macro" "list_lit" clojure-ts--defmacro-node-p)
    ("Variable" "list_lit" clojure-ts--variable-node-p)
    ("Interface" "list_lit" clojure-ts--interface-node-p)
    ("Class" "list_lit" clojure-ts--class-node-p))
  "The value for `treesit-simple-imenu-settings'.
By default `treesit-defun-name-function' is used to extract definition names.
See `clojure-ts--standard-definition-node-name' for the implementation used.")

(defcustom clojure-ts-indent-style 'semantic
  "Automatic indentation style to use when mode `clojure-ts-mode' is run.

The possible values for this variable are
    `semantic' - Semantic indentation.
        Tries to follow the same rules as cljfmt with default settings.
        See:
          - https://github.com/weavejester/cljfmt
          - https://guide.clojure.style/
    `fixed' - A simpler set of indentation rules that can be summarized as
        1. Multi-line lists that start with a symbol are always indented with
           two spaces.
        2. Other multi-line lists, vectors, maps and sets are aligned with the
           first element (1 or 2 spaces).
        See: https://tonsky.me/blog/clojurefmt/"
  :safe #'symbolp
  :type
  '(choice (const :tag "Semantic indentation rules." semantic)
           (const :tag "Simple fixed indentation rules." fixed))
  :package-version '(clojure-ts-mode . "0.2.0"))

(defvar clojure-ts--fixed-indent-rules
  ;; This is in contrast to semantic
  ;; fixed-indent-rules come from https://tonsky.me/blog/clojurefmt/
  `((clojure
     ((parent-is "source") parent-bol 0)
     ;; ((query "(list_lit . [(sym_lit) (kwd_lit)] _* @node)") parent 2)
     ;; Using the above `query' rule here doesn't always work because sometimes `node' is nil.
     ;; `query' requires `node' to be matched.
     ;; We really only care about the parent node being a function-call like list.
     ;; with it's first named child being a symbol
     ((lambda (node parent _)
        (and (clojure-ts--list-node-p parent)
             ;; Should we also check for keyword first child, as in (:k map) calls?
             (let ((first-child (treesit-node-child parent 0 t)))
               (or (clojure-ts--symbol-node-p first-child)
                   (clojure-ts--keyword-node-p first-child)))))
      parent 2)
     ((parent-is "vec_lit") parent 1)
     ((parent-is "map_lit") parent 1)
     ((parent-is "list_lit") parent 1)
     ((parent-is "set_lit") parent 2))))

(defvar clojure-ts--sexp-nodes
  '("#_" ;; transpose-sexp near a discard macro moves it around.
    "num_lit" "sym_lit" "kwd_lit" "nil_lit" "bool_lit"
    "regex_lit" "str_lit" "char_lit"
    "list_lit" "map_lit" "vec_lit" "set_lit" "ns_map_lit"
    "anon_fn_lit" "read_cond_lit"
    "var_quoting_lit" "sym_val_lit" "evaling_lit"
    "tagged_or_ctor_lit" "splicing_read_cond_lit"
    "derefing_lit" "quoting_lit" "syn_quoting_lit"
    "unquote_splicing_lit" "unquoting_lit")
  "A regular expression that matches nodes that can be treated as s-expressions.")

(defvar clojure-ts--thing-settings
  `((clojure
     ((sexp ,(regexp-opt clojure-ts--sexp-nodes))
      (text ,(regexp-opt '("comment")))))))

(defvar clojure-ts--symbols-with-body-expressions-regexp
  (eval-and-compile
    (rx (or
         ;; Match def* symbols,
         ;; we also explicitly do not match symbols beginning with
         ;; "default" "deflate" and "defer", like cljfmt
         (and line-start "def")
         ;; Match with-* symbols
         (and line-start "with-")
         ;; Exact matches
         (and line-start
              (or "alt!" "alt!!" "are" "as->"
                  "binding" "bound-fn"
                  "case" "catch" "comment" "cond" "condp" "cond->" "cond->>"
                  "delay" "do" "doseq" "dotimes" "doto"
                  "extend" "extend-protocol" "extend-type"
                  "fdef" "finally" "fn" "for" "future"
                  "go" "go-loop"
                  "if" "if-let" "if-not" "if-some"
                  "let" "letfn" "locking" "loop"
                  "match" "ns" "proxy" "reify" "struct-map"
                  "testing" "thread" "try"
                  "use-fixtures"
                  "when" "when-first" "when-let" "when-not" "when-some" "while")
              line-end))))
  "A regex to match symbols that are functions/macros with a body argument.
Taken from cljfmt:
https://github.com/weavejester/cljfmt/blob/fb26b22f569724b05c93eb2502592dfc2de898c3/cljfmt/resources/cljfmt/indents/clojure.clj")

(defun clojure-ts--match-function-call-arg (node parent _bol)
  "Match NODE if PARENT is a list expressing a function or macro call."
  (and (clojure-ts--list-node-p parent)
       ;; Can the following two clauses be replaced by checking indexes?
       ;; Does the second child exist, and is it not equal to the current node?
       (treesit-node-child parent 1 t)
       (not (treesit-node-eq (treesit-node-child parent 1 t) node))
       (let ((first-child (treesit-node-child parent 0 t)))
         (or (clojure-ts--symbol-node-p first-child)
             (clojure-ts--keyword-node-p first-child)))))

(defun clojure-ts--match-expression-in-body (_node parent _bol)
  "Match NODE if it is an expression used in a body argument.
PARENT is expected to be a list literal.
See `treesit-simple-indent-rules'."
  (and
   (clojure-ts--list-node-p parent)
   (let ((first-child (treesit-node-child parent 0 t)))
     (and
      (not
       (clojure-ts--symbol-matches-p
        ;; Symbols starting with this are false positives
        (rx line-start (or "default" "deflate" "defer"))
        first-child))
      (clojure-ts--symbol-matches-p
       clojure-ts--symbols-with-body-expressions-regexp
       first-child)))))

(defconst clojure-ts--reifying-symbol-regexp
  (eval-and-compile
    (rx line-start
        (or "deftype" "defrecord"
            "reify" "proxy" "extend-type" "extend-protocol")))
  "A regular expression matching a symbol used to define a concrete type.")

(defun clojure-ts--match-method-body (node parent _bol)
  "Matches a `NODE' in the body of a `PARENT' method implementation.
A method implementation referes to concrete implemntations being defined in
forms like deftype, defrecord, reify, proxy, etc."
  (and
   (clojure-ts--list-node-p parent)
   (let* ((grandparent (treesit-node-parent parent))
          ;; auncle: gender neutral sibling of parent, aka child of grandparent
          (first-auncle (treesit-node-child grandparent 0 t)))
     (and (clojure-ts--list-node-p grandparent)
          (clojure-ts--symbol-matches-p clojure-ts--reifying-symbol-regexp
                                        first-auncle)))))

(defvar clojure-ts--threading-macro
  (eval-and-compile
    (rx (and "->" (? ">") line-end)))
  "A regular expression matching a threading macro.")

(defun clojure-ts--match-threading-macro-arg (_node parent _)
  "Match NODE if it is an argument to a PARENT threading macro."
  ;; We want threading macros to indent 2 only if the ->> is on it's own line.
  ;; If not, then align function arg.
  (and (clojure-ts--list-node-p parent)
       (let ((first-child (treesit-node-child parent 0 t)))
         (clojure-ts--symbol-matches-p
          clojure-ts--threading-macro
          first-child))))

(defun clojure-ts--threading-macro-arg-offset (node _parent _bol)
  "Calculates the indentation offset for NODE, a threading macro argument."
  (if (and node (<= (treesit-node-index node t) 1))
      1 ;; NODE is the first arg, offset 1 from start of *->> symbol
    0)) ;; arg 2...n, match indentation of the previous argument

(defvar clojure-ts--semantic-indent-rules
  `((clojure
     ((parent-is "source") parent-bol 0)
     ;; https://guide.clojure.style/#body-indentation
     (clojure-ts--match-method-body parent 2)
     (clojure-ts--match-expression-in-body parent 2)
     ;; https://guide.clojure.style/#threading-macros-alignment
     (clojure-ts--match-threading-macro-arg prev-sibling 0)
     ;; https://guide.clojure.style/#vertically-align-fn-args
     (clojure-ts--match-function-call-arg (nth-sibling 2 nil) 0)
     ;; Literal Sequences
     ((parent-is "list_lit") parent 1) ;; https://guide.clojure.style/#one-space-indent
     ((parent-is "vec_lit") parent 1) ;; https://guide.clojure.style/#bindings-alignment
     ((parent-is "map_lit") parent 1) ;; https://guide.clojure.style/#map-keys-alignment
     ((parent-is "set_lit") parent 2))))

(defun clojure-ts--configured-indent-rules ()
  "Gets the configured choice of indent rules."
  (cond
   ((eq clojure-ts-indent-style 'semantic) (clojure-ts--semantic-indent-rules))
   ((eq clojure-ts-indent-style 'fixed) clojure-ts--fixed-indent-rules)
   (t (error
       (format
        "Invalid value for clojure-ts-indent-style. Expected one of '%S, but found '%S instead."
        '(semantic fixed)
        clojure-ts-indent-style)))))

(defvar clojure-ts-mode-map
  (let ((map (make-sparse-keymap)))
    ;(set-keymap-parent map clojure-mode-map)
    map))

(defvar clojurescript-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-ts-mode-map)
    map))

(defvar clojurec-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-ts-mode-map)
    map))

(defvar clojure-dart-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-ts-mode-map)
    map))

;;;###autolaod
(add-to-list
 'treesit-language-source-alist
 '(clojure "https://github.com/sogaiu/tree-sitter-clojure.git" "v0.0.12"))

(defun clojure-ts-mode-display-version ()
  "Display the current `clojure-mode-version' in the minibuffer."
  (interactive)
  (message "clojure-ts-mode (version %s)" clojure-ts-mode-version))

;;;###autoload
(define-derived-mode clojure-ts-mode prog-mode "Clojure[TS]"
  "Major mode for editing Clojure code.

\\{clojure-ts-mode-map}"
  :syntax-table clojure-ts-mode-syntax-table
  (unless (treesit-language-available-p 'clojure nil)
    (treesit-install-language-grammar 'clojure))
  (setq-local comment-start ";")
  (when (treesit-ready-p 'clojure)
    (treesit-parser-create 'clojure)
    (setq-local treesit-font-lock-settings (clojure-ts--font-lock-settings)
                treesit-defun-prefer-top-level t
                treesit-defun-tactic 'top-level
                treesit-defun-type-regexp (rx (or "list_lit" "vec_lit" "map_lit"))
                treesit-simple-indent-rules (clojure-ts--configured-indent-rules)
                treesit-defun-name-function #'clojure-ts--standard-definition-node-name
                treesit-simple-imenu-settings clojure-ts--imenu-settings
                treesit-font-lock-feature-list
                '((comment definition variable)
                  (keyword string char symbol builtin type)
                  (constant number quote metadata)
                  (bracket deref function regex tagged-literals)))
    (when (boundp 'treesit-thing-settings) ;; Emacs 30+
      (setq-local treesit-thing-settings clojure-ts--thing-settings))
    (when clojure-ts--debug
      (setq-local treesit--indent-verbose t
                  treesit--font-lock-verbose t)
      (treesit-inspect-mode))
    (treesit-major-mode-setup)))

;;;###autoload
(define-derived-mode clojurescript-ts-mode clojure-ts-mode "ClojureScript[TS]"
  "Major mode for editing ClojureScript code.

\\{clojurescript-ts-mode-map}")

;;;###autoload
(define-derived-mode clojurec-ts-mode clojure-ts-mode "ClojureC[TS]"
  "Major mode for editing ClojureC code.

\\{clojurec-ts-mode-map}")

;;;###autoload
(define-derived-mode clojure-dart-ts-mode clojure-ts-mode "ClojureDart[TS]"
  "Major mode for editing Clojure Dart code.

\\{clojure-dart-ts-mode-map}")

(defun clojure-ts--register-novel-modes ()
  "Set up Clojure modes not present in progenitor clojure-mode.el."
  (add-to-list 'auto-mode-alist '("\\.cljd\\'" . clojure-dart-ts-mode)))

;; Redirect clojure-mode to clojure-ts-mode if clojure-mode is present
(if (require 'clojure-mode nil 'noerror)
    (progn
      (add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode))
      (add-to-list 'major-mode-remap-alist '(clojurescript-mode . clojurescript-ts-mode))
      (add-to-list 'major-mode-remap-alist '(clojurec-mode . clojurec-ts-mode))
      (clojure-ts--register-novel-modes))
  ;; Clojure-mode is not present, setup auto-modes ourselves
  ;; Regular clojure/edn files
  ;; I believe dtm is for datomic queries and datoms, which are just edn.
  (add-to-list 'auto-mode-alist
               '("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojurec-ts-mode))
  ;; boot build scripts are Clojure source files
  (add-to-list 'auto-mode-alist '("\\(?:build\\|profile\\)\\.boot\\'" . clojure-ts-mode))
  ;; babashka scripts are Clojure source files
  (add-to-list 'interpreter-mode-alist '("bb" . clojure-ts-mode))
  ;; nbb scripts are ClojureScript source files
  (add-to-list 'interpreter-mode-alist '("nbb" . clojurescript-ts-mode))
  (clojure-ts--register-novel-modes))

(defvar clojure-ts--find-ns-query
  (treesit-query-compile
   'clojure
   '(((source (list_lit
               :anchor (sym_lit name: (sym_name) @ns)
               :anchor (sym_lit name: (sym_name) @ns-name)))
      (:equal @ns "ns")))))

(defun clojure-ts-find-ns ()
  "Return the name of the current namespace."
  (let ((nodes (treesit-query-capture 'clojure clojure-ts--find-ns-query)))
    (treesit-node-text (cdr (assoc 'ns-name nodes)))))

(provide 'clojure-ts-mode)

;;; clojure-ts-mode.el ends here
