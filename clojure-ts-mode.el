;;; clojure-ts-mode.el --- Major mode for Clojure code -*- lexical-binding: t; -*-

;; Copyright © 2022-2025 Danny Freeman
;;
;; Authors: Danny Freeman <danny@dfreeman.email>
;; Maintainer: Danny Freeman <danny@dfreeman.email>
;; URL: http://github.com/clojure-emacs/clojure-ts-mode
;; Keywords: languages clojure clojurescript lisp
;; Version: 0.3.0-snapshot
;; Package-Requires: ((emacs "30.1"))

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

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-eq "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")

(defgroup clojure-ts nil
  "Major mode for editing Clojure code with tree-sitter."
  :prefix "clojure-ts-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/clojure-emacs/clojure-ts-mode")
  :link '(emacs-commentary-link :tag "Commentary" "clojure-mode"))

(defconst clojure-ts-mode-version
  "0.3.0-snapshot"
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

(defcustom clojure-ts-ensure-grammars t
  "When non-nil, ensure required tree-sitter grammars are installed."
  :safe #'booleanp
  :type 'boolean
  :package-version '(clojure-ts-mode . "0.2.0"))

(defcustom clojure-ts-toplevel-inside-comment-form nil
  "Eval top level forms inside comment forms instead of the comment form itself."
  :type 'boolean
  :safe #'booleanp
  :package-version '(clojure-ts-mode . "0.2.1"))

(defcustom clojure-ts-docstring-fill-column fill-column
  "Value of `fill-column' to use when filling a docstring."
  :type 'integer
  :safe #'integerp
  :package-version '(clojure-ts-mode . "0.2.3"))

(defcustom clojure-ts-docstring-fill-prefix-width 2
  "Width of `fill-prefix' when filling a docstring.
The default value conforms with the de facto convention for
Clojure docstrings, aligning the second line with the opening
double quotes on the third column."
  :type 'integer
  :safe #'integerp
  :package-version '(clojure-ts-mode . "0.2.3"))

(defcustom clojure-ts-use-markdown-inline t
  "When non-nil, use Markdown inline grammar for docstrings."
  :type 'boolean
  :safe #'booleanp
  :package-version '(clojure-ts-mode . "0.2.3"))

(defcustom clojure-ts-auto-remap t
  "When non-nil, redirect all `clojure-mode' buffers to `clojure-ts-mode'."
  :safe #'booleanp
  :type 'boolean
  :package-version '(clojure-ts-mode . "0.3"))

(defvar clojure-ts-mode-remappings
  '((clojure-mode . clojure-ts-mode)
    (clojurescript-mode . clojure-ts-clojurescript-mode)
    (clojurec-mode . clojure-ts-clojurec-mode))
  "Alist of entries to `major-mode-remap-defaults'.

See also `clojure-ts-activate-mode-remappings' and
`clojure-ts-definition-docstring-symbols'.")

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
  "Syntax table for `clojure-ts-mode'.")

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
               "with-redefs" "with-redefs-fn"
               ;; Commonly used clojure.test functions
               "deftest" "deftest-" "is" "are" "testing"))
            "$")))

(defface clojure-ts-keyword-face
  '((t (:inherit font-lock-constant-face)))
  "Face used to font-lock Clojure keywords (:something).")

(defface clojure-ts-character-face
  '((t (:inherit font-lock-string-face)))
  "Face used to font-lock Clojure character literals.")

(defun clojure-ts-symbol-regexp (symbols)
  "Return a regular expression that matches one of SYMBOLS exactly."
  (concat "^" (regexp-opt symbols) "$"))

(defvar clojure-ts-function-docstring-symbols
  '("definline"
    "defmulti"
    "defmacro"
    "defn"
    "defn-"
    "defprotocol"
    "ns")
  "Symbols that accept an optional docstring as their second argument.")

(defvar clojure-ts-definition-docstring-symbols
  '("def")
  "Symbols that accept an optional docstring as their second argument.
Any symbols added here should only treat their second argument as a docstring
if a third argument (the value) is provided.
\"def\" is the only builtin Clojure symbol that behaves like this.")

(defconst clojure-ts--variable-definition-symbol-regexp
  (eval-and-compile
    (rx line-start (or "def" "defonce") line-end))
  "A regular expression matching a symbol used to define a variable.")

(defconst clojure-ts--typedef-symbol-regexp
  (eval-and-compile
    (rx line-start
        (or "defprotocol" "defmulti" "deftype" "defrecord"
            "definterface" "defmethod" "defstruct")
        line-end))
  "A regular expression matching a symbol used to define a type.")

(defconst clojure-ts--type-symbol-regexp
  (eval-and-compile
    (rx line-start
        (or "deftype" "defrecord"
            ;; While not reifying, helps with doc strings
            "defprotocol" "definterface"
            "reify" "proxy" "extend-type" "extend-protocol")
        line-end))
  "A regular expression matching a symbol used to define or instantiate a type.")

(defconst clojure-ts--interface-def-symbol-regexp
  (eval-and-compile
    (rx line-start (or "defprotocol" "definterface") line-end))
  "A regular expression matching a symbol used to define an interface.")

(defun clojure-ts--docstring-query (capture-symbol)
  "Return a query that captures docstrings with CAPTURE-SYMBOL."
  `(;; Captures docstrings in def
    ((list_lit :anchor (meta_lit) :?
               :anchor (sym_lit) @_def_symbol
               :anchor (comment) :?
               :anchor (sym_lit) ; variable name
               :anchor (comment) :?
               :anchor (str_lit) ,capture-symbol
               :anchor (_)) ; the variable's value
     (:match ,(clojure-ts-symbol-regexp clojure-ts-definition-docstring-symbols)
             @_def_symbol))
    ;; Captures docstrings in metadata of definitions
    ((list_lit :anchor (sym_lit) @_def_symbol
               :anchor (comment) :?
               :anchor (sym_lit
                        (meta_lit
                         value: (map_lit
                                 (kwd_lit) @_doc-keyword
                                 :anchor
                                 (str_lit) ,capture-symbol))))
     ;; We're only supporting this on a fixed set of defining symbols
     ;; Existing regexes don't encompass def and defn
     ;; Naming another regex is very cumbersome.
     (:match ,(clojure-ts-symbol-regexp
               '("def" "defonce" "defn" "defn-" "defmacro" "ns"
                 "defmulti" "definterface" "defprotocol"
                 "deftest" "deftest-"
                 "deftype" "defrecord" "defstruct"))
             @_def_symbol)
     (:equal @_doc-keyword ":doc"))
    ;; Captures docstrings defn, defmacro, ns, and things like that
    ((list_lit :anchor (meta_lit) :?
               :anchor (sym_lit) @_def_symbol
               :anchor (comment) :?
               :anchor (sym_lit) ; function_name
               :anchor (comment) :?
               :anchor (str_lit) ,capture-symbol)
     (:match ,(clojure-ts-symbol-regexp clojure-ts-function-docstring-symbols)
             @_def_symbol))
    ;; Captures docstrings in defprotcol, definterface
    ((list_lit :anchor (sym_lit) @_def_symbol
               (list_lit
                :anchor (sym_lit) (vec_lit) :*
                (str_lit) ,capture-symbol :anchor)
               :*)
     (:match ,clojure-ts--interface-def-symbol-regexp @_def_symbol))))

(defvar clojure-ts--treesit-range-settings
  (treesit-range-rules
   :embed 'markdown-inline
   :host 'clojure
   (clojure-ts--docstring-query '@capture)))

(defun clojure-ts--font-lock-settings (markdown-available)
  "Return font lock settings suitable for use in `treesit-font-lock-settings'.
When MARKDOWN-AVAILABLE is non-nil, includes rules for highlighting docstrings
with the markdown-inline grammar."
  (append
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

    ;; Highlight as built-in only if there is no namespace or namespace is
    ;; `clojure.core'.
    :feature 'builtin
    :language 'clojure
    `(((list_lit meta: _ :* :anchor (sym_lit !namespace name: (sym_name) @font-lock-keyword-face))
       (:match ,clojure-ts--builtin-symbol-regexp @font-lock-keyword-face))
      ((list_lit meta: _ :* :anchor
                 (sym_lit namespace: ((sym_ns) @ns
                                      (:equal "clojure.core" @ns))
                          name: (sym_name) @font-lock-keyword-face))
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
    `(((list_lit :anchor meta: _ :*
                 :anchor (sym_lit (sym_name) @font-lock-keyword-face)
                 :anchor (sym_lit (sym_name) @font-lock-function-name-face))
       (:match ,(rx-to-string
                 `(seq bol
                       (or
                        "fn"
                        "defn"
                        "defn-"
                        "defmulti"
                        "defmethod"
                        "deftest"
                        "deftest-"
                        "defmacro"
                        "definline")
                       eol))
               @font-lock-keyword-face))
      ((anon_fn_lit
        marker: "#" @font-lock-property-face))
      ;; Methods implementation
      ((list_lit
        ((sym_lit name: (sym_name) @def)
         ((:match ,(rx-to-string
                    `(seq bol
                          (or
                           "defrecord"
                           "definterface"
                           "deftype"
                           "defprotocol")
                          eol))
                  @def)))
        :anchor
        (sym_lit (sym_name) @font-lock-type-face)
        (list_lit
         (sym_lit name: (sym_name) @font-lock-function-name-face))))
      ((list_lit
        ((sym_lit name: (sym_name) @def)
         ((:equal "reify" @def)))
        (list_lit
         (sym_lit name: (sym_name) @font-lock-function-name-face))))
      ;; letfn
      ((list_lit
        ((sym_lit name: (sym_name) @symbol)
         ((:equal "letfn" @symbol)))
        (vec_lit
         (list_lit
          (sym_lit name: (sym_name) @font-lock-function-name-face))))))

    :feature 'variable ;; def, defonce
    :language 'clojure
    `(((list_lit :anchor meta: _ :*
                 :anchor (sym_lit (sym_name) @font-lock-keyword-face)
                 :anchor (sym_lit (sym_name) @font-lock-variable-name-face))
       (:match ,clojure-ts--variable-definition-symbol-regexp @font-lock-keyword-face)))

    ;; Can we support declarations in the namespace form?
    :feature 'type
    :language 'clojure
    `(;; Type Declarations
      ((list_lit :anchor (sym_lit (sym_name) @def)
                 :anchor (sym_lit (sym_name) @font-lock-type-face))
       (:match ,clojure-ts--typedef-symbol-regexp @def))
      ;; Type Hints
      (meta_lit
       marker: "^" @font-lock-operator-face
       value: (sym_lit (sym_name) @font-lock-type-face))
      (old_meta_lit
       marker: "#^" @font-lock-operator-face
       value: (sym_lit (sym_name) @font-lock-type-face))
      ;; Highlight namespace
      ((list_lit :anchor (sym_lit (sym_name) @def)
                 :anchor (sym_lit (sym_name) @font-lock-type-face))
       (:equal "ns" @def)))

    :feature 'metadata
    :language 'clojure
    :override t
    `((meta_lit
       marker: "^" @font-lock-operator-face
       value: (kwd_lit (kwd_name) @clojure-ts-keyword-face))
      (old_meta_lit
       marker: "#^" @font-lock-operator-face
       value: (kwd_lit (kwd_name) @clojure-ts-keyword-face)))

    :feature 'tagged-literals
    :language 'clojure
    :override t
    '((tagged_or_ctor_lit marker: "#" @font-lock-preprocessor-face
                          tag: (sym_lit) @font-lock-preprocessor-face))

    :feature 'doc
    :language 'clojure
    :override t
    (clojure-ts--docstring-query '@font-lock-doc-face))

   (when markdown-available
     (treesit-font-lock-rules
      :feature 'doc
      :language 'markdown-inline
      :override t
      `((code_span) @font-lock-constant-face)))

   (treesit-font-lock-rules
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
       marker: "@" @font-lock-warning-face)))))

;; Node predicates

(defun clojure-ts--list-node-p (node)
  "Return non-nil if NODE is a Clojure list."
  (string-equal "list_lit" (treesit-node-type node)))

(defun clojure-ts--symbol-node-p (node)
  "Return non-nil if NODE is a Clojure symbol."
  (string-equal "sym_lit" (treesit-node-type node)))

(defun clojure-ts--string-node-p (node)
  "Return non-nil if NODE is a Clojure string literal."
  (string-equal "str_lit" (treesit-node-type node)))

(defun clojure-ts--keyword-node-p (node)
  "Return non-nil if NODE is a Clojure keyword."
  (string-equal "kwd_lit" (treesit-node-type node)))

(defun clojure-ts--metadata-node-p (node)
  "Return non-nil if NODE is a Clojure metadata node."
  (string-equal "meta_lit" (treesit-node-type node)))

(defun clojure-ts--var-node-p (node)
  "Return non-nil if NODE is a var (eg.  #\\'foo)."
  (string-equal "var_quoting_lit" (treesit-node-type node)))

(defun clojure-ts--named-node-text (node)
  "Gets the name of a symbol or keyword NODE.
This does not include the NODE's namespace."
  (treesit-node-text (treesit-node-child-by-field-name node "name")))

(defun clojure-ts--node-namespace-text (node)
  "Gets the namespace of a symbol or keyword NODE.

If there is no namespace, returns nil."
  (treesit-node-text (treesit-node-child-by-field-name node "namespace")))

(defun clojure-ts--symbol-named-p (expected-symbol-name node)
  "Return non-nil if NODE is a symbol with text matching EXPECTED-SYMBOL-NAME."
  (and (clojure-ts--symbol-node-p node)
       (string-equal expected-symbol-name (clojure-ts--named-node-text node))))

(defun clojure-ts--node-child-skip-metadata (node n)
  "Return the Nth child of NODE like `treesit-node-child', sans metadata.
Skip the optional metadata node at pos 0 if present."
  (let ((first-child (treesit-node-child node 0 t)))
    (treesit-node-child
     node
     (if (clojure-ts--metadata-node-p first-child)
         (1+ n)
       n)
     t)))

(defun clojure-ts--node-with-metadata-parent (node)
  "Return parent for NODE only if NODE has metadata, otherwise return nil."
  (when-let* ((prev-sibling (treesit-node-prev-sibling node))
              ((clojure-ts--metadata-node-p prev-sibling)))
    (treesit-node-parent (treesit-node-parent node))))

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
   (let* ((child (clojure-ts--node-child-skip-metadata node 0))
          (child-txt (clojure-ts--named-node-text child))
          (name-sym (clojure-ts--node-child-skip-metadata node 1)))
     (and (clojure-ts--symbol-node-p child)
          (clojure-ts--symbol-node-p name-sym)
          (string-match-p definition-type-regexp child-txt)))))

(defun clojure-ts--kwd-definition-node-match-p (node)
  "Return non-nil if the NODE is a keyword definition."
  (and (clojure-ts--list-node-p node)
       (let* ((child (clojure-ts--node-child-skip-metadata node 0))
              (child-txt (clojure-ts--named-node-text child))
              (child-ns (clojure-ts--node-namespace-text child))
              (name-kwd (clojure-ts--node-child-skip-metadata node 1)))
         (and child-ns
              (clojure-ts--symbol-node-p child)
              (clojure-ts--keyword-node-p name-kwd)
              (string-equal child-txt "def")))))

(defun clojure-ts--standard-definition-node-name (node)
  "Return the definition name for the given NODE.

Returns nil if NODE is not a list with symbols as the first two
children.  For example the node representing the expression (def foo 1)
would return foo.  The node representing (ns user) would return user.
Does not do any matching on the first symbol (def, defn, etc), so
identifying that a node is a definition is intended to be done
elsewhere.

Can be called directly, but intended for use as `treesit-defun-name-function'."
  (when (and (clojure-ts--list-node-p node)
             (clojure-ts--symbol-node-p (clojure-ts--node-child-skip-metadata node 0)))
    (let ((sym (clojure-ts--node-child-skip-metadata node 1)))
      (when (clojure-ts--symbol-node-p sym)
        ;; Extracts ns and name, and recreates the full var name.
        ;; We can't just get the node-text of the full symbol because
        ;; that could include metadata that isn't part of the name.
        (let ((ns (treesit-node-child-by-field-name sym "ns"))
              (name (treesit-node-child-by-field-name sym "name")))
          (if ns
              (concat (treesit-node-text ns) "/" (treesit-node-text name))
            (treesit-node-text name)))))))

(defun clojure-ts--kwd-definition-node-name (node)
  "Return the keyword name for the given NODE.

Returns nil if NODE is not a list where the first element is a symbol
and the second is a keyword.  For example, a node representing the
expression (s/def ::foo int?) would return foo.

Can be called directly, but intended for use as
`treesit-defun-name-function'."
  (when (and (clojure-ts--list-node-p node)
             (clojure-ts--symbol-node-p (clojure-ts--node-child-skip-metadata node 0)))
    (let ((kwd (clojure-ts--node-child-skip-metadata node 1)))
      (when (clojure-ts--keyword-node-p kwd)
        (treesit-node-text (treesit-node-child-by-field-name kwd "name"))))))

(defvar clojure-ts--function-type-regexp
  (rx string-start (or (seq "defn" (opt "-")) "defmethod" "deftest") string-end)
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

(defvar clojure-ts--variable-definition-type-regexp
  (rx string-start (or "def" "defonce") string-end)
  "Regular expression for matching definition nodes that resemble variables.")

(defun clojure-ts--variable-definition-node-p (node)
  "Return non-nil if NODE is a def or defonce form."
  (clojure-ts--definition-node-match-p clojure-ts--variable-definition-type-regexp node))

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
    ("Variable" "list_lit" clojure-ts--variable-definition-node-p)
    ("Interface" "list_lit" clojure-ts--interface-node-p)
    ("Class" "list_lit" clojure-ts--class-node-p)
    ("Keyword"
     "list_lit"
     clojure-ts--kwd-definition-node-match-p
     clojure-ts--kwd-definition-node-name))
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

(defvar clojure-ts--semantic-indent-rules-defaults
  '(("alt!"            . ((:block 0)))
    ("alt!!"           . ((:block 0)))
    ("comment"         . ((:block 0)))
    ("cond"            . ((:block 0)))
    ("delay"           . ((:block 0)))
    ("do"              . ((:block 0)))
    ("finally"         . ((:block 0)))
    ("future"          . ((:block 0)))
    ("go"              . ((:block 0)))
    ("thread"          . ((:block 0)))
    ("try"             . ((:block 0)))
    ("with-out-str"    . ((:block 0)))
    ("defprotocol"     . ((:block 1) (:inner 1)))
    ("binding"         . ((:block 1)))
    ("case"            . ((:block 1)))
    ("cond->"          . ((:block 1)))
    ("cond->>"         . ((:block 1)))
    ("doseq"           . ((:block 1)))
    ("dotimes"         . ((:block 1)))
    ("doto"            . ((:block 1)))
    ("extend"          . ((:block 1)))
    ("extend-protocol" . ((:block 1) (:inner 1)))
    ("extend-type"     . ((:block 1) (:inner 1)))
    ("for"             . ((:block 1)))
    ("go-loop"         . ((:block 1)))
    ("if"              . ((:block 1)))
    ("if-let"          . ((:block 1)))
    ("if-not"          . ((:block 1)))
    ("if-some"         . ((:block 1)))
    ("let"             . ((:block 1)))
    ("letfn"           . ((:block 1) (:inner 2 0)))
    ("locking"         . ((:block 1)))
    ("loop"            . ((:block 1)))
    ("match"           . ((:block 1)))
    ("ns"              . ((:block 1)))
    ("struct-map"      . ((:block 1)))
    ("testing"         . ((:block 1)))
    ("when"            . ((:block 1)))
    ("when-first"      . ((:block 1)))
    ("when-let"        . ((:block 1)))
    ("when-not"        . ((:block 1)))
    ("when-some"       . ((:block 1)))
    ("while"           . ((:block 1)))
    ("with-local-vars" . ((:block 1)))
    ("with-open"       . ((:block 1)))
    ("with-precision"  . ((:block 1)))
    ("with-redefs"     . ((:block 1)))
    ("defrecord"       . ((:block 2) (:inner 1)))
    ("deftype"         . ((:block 2) (:inner 1)))
    ("are"             . ((:block 2)))
    ("as->"            . ((:block 2)))
    ("catch"           . ((:block 2)))
    ("condp"           . ((:block 2)))
    ("bound-fn"        . ((:inner 0)))
    ("def"             . ((:inner 0)))
    ("defmacro"        . ((:inner 0)))
    ("defmethod"       . ((:inner 0)))
    ("defmulti"        . ((:inner 0)))
    ("defn"            . ((:inner 0)))
    ("defn-"           . ((:inner 0)))
    ("defonce"         . ((:inner 0)))
    ("deftest"         . ((:inner 0)))
    ("fdef"            . ((:inner 0)))
    ("fn"              . ((:inner 0)))
    ("reify"           . ((:inner 0) (:inner 1)))
    ("proxy"           . ((:block 2) (:inner 1)))
    ("use-fixtures"    . ((:inner 0))))
  "Default semantic indentation rules.

The format reflects cljfmt indentation rules.  All the default rules are
aligned with
https://github.com/weavejester/cljfmt/blob/0.13.0/cljfmt/resources/cljfmt/indents/clojure.clj")

(defvar-local clojure-ts--semantic-indent-rules-cache nil)

(defun clojure-ts--compute-semantic-indentation-rules-cache (rules)
  "Compute the combined semantic indentation rules cache.

If RULES are not provided, this function computes the union of
`clojure-ts-semantic-indent-rules' and
`clojure-ts--semantic-indent-rules-defaults', prioritizing user-defined
rules.  If RULES are provided, this function uses them instead of
`clojure-ts-semantic-indent-rules'.

This function is called when the `clojure-ts-semantic-indent-rules'
variable is customized using setopt or the Emacs customization
interface.  It is also called when file-local variables are updated.
This ensures that updated indentation rules are always precalculated."
  (seq-union rules
             clojure-ts--semantic-indent-rules-defaults
             (lambda (e1 e2) (equal (car e1) (car e2)))))

(defun clojure-ts--set-semantic-indent-rules (symbol value)
  "Setter function for `clojure-ts-semantic-indent-rules' variable.

Sets SYMBOL's top-level default value to VALUE and updates the
`clojure-ts--semantic-indent-rules-cache' in all `clojure-ts-mode'
buffers, if any exist.

NOTE: This function is not meant to be called directly."
  (set-default-toplevel-value symbol value)
  ;; Update cache in every `clojure-ts-mode' buffer.
  (let ((new-cache (clojure-ts--compute-semantic-indentation-rules-cache value)))
    (dolist (buf (buffer-list))
      (when (buffer-local-boundp 'clojure-ts--semantic-indent-rules-cache buf)
        (setq clojure-ts--semantic-indent-rules-cache new-cache)))))

(defcustom clojure-ts-semantic-indent-rules nil
  "Custom rules to extend default indentation rules for `semantic' style.

Each rule is an alist entry which looks like `(\"symbol-name\"
. (rule-type rule-value))', where rule-type is one either `:block' or
`:inner' and rule-value is an integer.  The semantic is similar to
cljfmt indentation rules.

Default set of rules is defined in
`clojure-ts--semantic-indent-rules-defaults'."
  :safe #'listp
  :type '(alist :key-type string
                :value-type (repeat (choice (list (choice (const :tag "Block indentation rule" :block)
                                                          (const :tag "Inner indentation rule" :inner))
                                                  integer)
                                            (list (const :tag "Inner indentation rule" :inner)
                                                  integer
                                                  integer))))
  :package-version '(clojure-ts-mode . "0.3")
  :set #'clojure-ts--set-semantic-indent-rules)

(defun clojure-ts--match-block-0-body (bol first-child)
  "Match if expression body is not at the same line as FIRST-CHILD.

If there is no body, check that BOL is not at the same line."
  (let* ((body-pos (if-let* ((body (treesit-node-next-sibling first-child)))
                       (treesit-node-start body)
                     bol)))
    (< (line-number-at-pos (treesit-node-start first-child))
       (line-number-at-pos body-pos))))

(defun clojure-ts--node-pos-match-block (node parent bol block)
  "Return TRUE if NODE index in the PARENT matches requested BLOCK.

NODE might be nil (when we insert an empty line for example), in this
case we look for next available child node in the PARENT after BOL
position.

The first node in the expression is usually an opening paren, the last
node is usually a closing paren (unless some automatic parens mode is
not enabled).  If requested BLOCK is 1, the NODE index should be at
least 3 (first node is opening paren, second node is matched symbol,
third node is first argument, and the rest is body which should be
indented.)"
  (if node
      (> (treesit-node-index node) (1+ block))
    (when-let* ((node-after-bol (treesit-node-first-child-for-pos parent bol)))
      (> (treesit-node-index node-after-bol) (1+ block)))))

(defvar clojure-ts-get-indent-function nil
  "Function to get the indent spec of a symbol.

This function should take one argument, the name of the symbol as a
string.  This name will be exactly as it appears in the buffer, so it
might start with a namespace alias.

The returned value is expected to be the same as
`clojure-get-indent-function' from `clojure-mode' for compatibility
reasons.")

(defun clojure-ts--unwrap-dynamic-spec (spec current-depth)
  "Recursively unwrap SPEC, incrementally increasing the CURRENT-DEPTH.

This function accepts a list SPEC, like ((:defn)) and produce a proper
indent rule.  For example, ((:defn)) is converted to (:inner 2),
and (:defn) is converted to (:inner 1)."
  (if (consp spec)
      (clojure-ts--unwrap-dynamic-spec (car spec) (1+ current-depth))
    (cond
     ((equal spec :defn) (list :inner current-depth))
     (t nil))))

(defun clojure-ts--dynamic-indent-for-symbol (sym &optional ns)
  "Return the dynamic indentation specification for SYM, if found.

If the function `clojure-ts-get-indent-function' is defined, call it and
produce a valid indentation specification from its return value.

The `clojure-ts-get-indent-function' should return an indentation
specification compatible with `clojure-mode', which will then be
converted to a suitable `clojure-ts-mode' specification.

For example, (1 ((:defn)) nil) is converted to ((:block 1) (:inner 2)).

If NS is defined, then the fully qualified symbol is passed to
`clojure-ts-get-indent-function'."
  (when (and sym (functionp clojure-ts-get-indent-function))
    (let* ((full-symbol (if ns
                            (concat ns "/" sym)
                          sym))
           (spec (funcall clojure-ts-get-indent-function full-symbol)))
      (if (integerp spec)
          (list (list :block spec))
        (when (sequencep spec)
          (thread-last spec
                       (seq-map (lambda (el)
                                  (cond
                                   ((integerp el) (list :block el))
                                   ((equal el :defn) (list :inner 0))
                                   ((consp el) (clojure-ts--unwrap-dynamic-spec el 0))
                                   (t nil))))
                       (seq-remove #'null)
                       ;; Always put `:block' to the beginning.
                       (seq-sort (lambda (spec1 _spec2)
                                   (equal (car spec1) :block)))))))))

(defun clojure-ts--find-semantic-rule (node parent current-depth)
  "Return a suitable indentation rule for NODE, considering the CURRENT-DEPTH.

Attempts to find an indentation rule by examining the symbol name of the
PARENT's first child.  If a rule is not found, it navigates up the
syntax tree and recursively attempts to find a rule, incrementally
increasing the CURRENT-DEPTH.  If a rule is not found upon reaching the
root of the syntax tree, it returns nil.  A rule is considered a match
only if the CURRENT-DEPTH matches the rule's required depth."
  (let* ((first-child (clojure-ts--node-child-skip-metadata parent 0))
         (symbol-name (clojure-ts--named-node-text first-child))
         (symbol-namespace (clojure-ts--node-namespace-text first-child))
         (idx (- (treesit-node-index node) 2)))
    (if-let* ((rule-set (or (clojure-ts--dynamic-indent-for-symbol symbol-name symbol-namespace)
                            (alist-get symbol-name
                                       clojure-ts--semantic-indent-rules-cache
                                       nil
                                       nil
                                       #'equal))))
        (if (zerop current-depth)
            (let ((rule (car rule-set)))
              (if (equal (car rule) :block)
                  rule
                (pcase-let ((`(,_ ,rule-depth ,rule-idx) rule))
                  (when (and (equal rule-depth current-depth)
                             (or (null rule-idx)
                                 (equal rule-idx idx)))
                    rule))))
          (thread-last rule-set
                       (seq-filter (lambda (rule)
                                     (pcase-let ((`(,rule-type ,rule-depth ,rule-idx) rule))
                                       (and (equal rule-type :inner)
                                            (equal rule-depth current-depth)
                                            (or (null rule-idx)
                                                (equal rule-idx idx))))))
                       (seq-first)))
      (when-let* ((new-parent (treesit-node-parent parent)))
        (clojure-ts--find-semantic-rule parent
                                        new-parent
                                        (1+ current-depth))))))

(defun clojure-ts--match-form-body (node parent bol)
  "Match if NODE has to be indented as a for body.

PARENT not should be a list.  If first symbol in the expression has an
indentation rule in `clojure-ts--semantic-indent-rules-defaults' or
`clojure-ts-semantic-indent-rules' check if NODE should be indented
according to the rule.  If NODE is nil, use next node after BOL."
  (and (clojure-ts--list-node-p parent)
       (let* ((first-child (clojure-ts--node-child-skip-metadata parent 0)))
         (when-let* ((rule (clojure-ts--find-semantic-rule node parent 0)))
           (and (not (clojure-ts--match-with-metadata node))
                (let ((rule-type (car rule))
                      (rule-value (cadr rule)))
                  (if (equal rule-type :block)
                      (if (zerop rule-value)
                          ;; Special treatment for block 0 rule.
                          (clojure-ts--match-block-0-body bol first-child)
                        (clojure-ts--node-pos-match-block node parent bol rule-value))
                    ;; Return true for any inner rule.
                    t)))))))

(defun clojure-ts--match-function-call-arg (node parent _bol)
  "Match NODE if PARENT is a list expressing a function or macro call."
  (and (clojure-ts--list-node-p parent)
       ;; Can the following two clauses be replaced by checking indexes?
       ;; Does the second child exist, and is it not equal to the current node?
       (treesit-node-child parent 1 t)
       (not (treesit-node-eq (treesit-node-child parent 1 t) node))
       (let ((first-child (treesit-node-child parent 0 t)))
         (or (clojure-ts--symbol-node-p first-child)
             (clojure-ts--keyword-node-p first-child)
             (clojure-ts--var-node-p first-child)))))

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

(defun clojure-ts--match-fn-docstring (node)
  "Match NODE when it is a docstring for PARENT function definition node."
  ;; A string that is the third node in a function defn block
  (let ((parent (treesit-node-parent node)))
    (and (treesit-node-eq node (treesit-node-child parent 2 t))
         (let ((first-auncle (treesit-node-child parent 0 t)))
           (clojure-ts--symbol-matches-p
            (regexp-opt clojure-ts-function-docstring-symbols)
            first-auncle)))))

(defun clojure-ts--match-def-docstring (node)
  "Match NODE when it is a docstring for PARENT variable definition node."
  ;; A string that is the fourth node in a variable definition block.
  (let ((parent (treesit-node-parent node)))
    (and (treesit-node-eq node (treesit-node-child parent 2 t))
         ;; There needs to be a value after the string.
         ;; If there is no 4th child, then this string is the value.
         (treesit-node-child parent 3 t)
         (let ((first-auncle (treesit-node-child parent 0 t)))
           (clojure-ts--symbol-matches-p
            (regexp-opt clojure-ts-definition-docstring-symbols)
            first-auncle)))))

(defun clojure-ts--match-method-docstring (node)
  "Match NODE when it is a docstring in a method definition."
  (let* ((grandparent (treesit-node-parent ;; the protocol/interface
                       (treesit-node-parent node))) ;; the method definition
         (first-grandauncle (treesit-node-child grandparent 0 t)))
    (clojure-ts--symbol-matches-p
     clojure-ts--interface-def-symbol-regexp
     first-grandauncle)))

(defun clojure-ts--match-docstring (_node parent _bol)
  "Match PARENT when it is a docstring node."
  (and (clojure-ts--string-node-p parent) ;; We are IN a string
       (or (clojure-ts--match-def-docstring parent)
           (clojure-ts--match-fn-docstring parent)
           (clojure-ts--match-method-docstring parent))))

(defun clojure-ts--match-with-metadata (node &optional _parent _bol)
  "Match NODE when it has metadata."
  (let ((prev-sibling (treesit-node-prev-sibling node)))
    (and prev-sibling
         (clojure-ts--metadata-node-p prev-sibling))))

(defun clojure-ts--anchor-parent-skip-metadata (_node parent _bol)
  "Return position of PARENT start for NODE.

If PARENT has optional metadata we skip it and return starting position
of the first child's opening paren.

NOTE: This serves as an anchor function to resolve an indentation issue
for forms with type hints."
  (let ((first-child (treesit-node-child parent 0 t)))
    (if (clojure-ts--metadata-node-p first-child)
        ;; We don't need named node here
        (treesit-node-start (treesit-node-child parent 1))
      (treesit-node-start parent))))

(defun clojure-ts--match-collection-item-with-metadata (node-type)
  "Return a matcher for a collection item with metadata by NODE-TYPE.

The returned matcher accepts NODE, PARENT and BOL and returns true only
if NODE has metadata and its parent has type NODE-TYPE."
  (lambda (node _parent _bol)
    (string-equal node-type
                  (treesit-node-type
                   (clojure-ts--node-with-metadata-parent node)))))

(defun clojure-ts--semantic-indent-rules ()
  "Return a list of indentation rules for `treesit-simple-indent-rules'."
  `((clojure
     ((parent-is "source") parent-bol 0)
     (clojure-ts--match-docstring parent 0)
     ;; https://guide.clojure.style/#body-indentation
     (clojure-ts--match-form-body clojure-ts--anchor-parent-skip-metadata 2)
     ;; https://guide.clojure.style/#threading-macros-alignment
     (clojure-ts--match-threading-macro-arg prev-sibling 0)
     ;; https://guide.clojure.style/#vertically-align-fn-args
     (clojure-ts--match-function-call-arg (nth-sibling 2 nil) 0)
     ;; Collections items with metadata.
     ;;
     ;; This should be before `clojure-ts--match-with-metadata', otherwise they
     ;; will never be matched.
     (,(clojure-ts--match-collection-item-with-metadata "vec_lit") grand-parent 1)
     (,(clojure-ts--match-collection-item-with-metadata "map_lit") grand-parent 1)
     (,(clojure-ts--match-collection-item-with-metadata "set_lit") grand-parent 2)
     ;;
     ;; If we enable this rule for lists, it will break many things.
     ;; (,(clojure-ts--match-collection-item-with-metadata "list_lit") grand-parent 1)
     ;;
     ;; All other forms with metadata.
     (clojure-ts--match-with-metadata parent 0)
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

(defun clojure-ts--docstring-fill-prefix ()
  "The prefix string used by `clojure-ts--fill-paragraph'.
It is simply `clojure-ts-docstring-fill-prefix-width' number of spaces."
  (make-string clojure-ts-docstring-fill-prefix-width ? ))

(defun clojure-ts--fill-paragraph (&optional justify)
  "Like `fill-paragraph', but can handler Clojure docstrings.
If JUSTIFY is non-nil, justify as well as fill the paragraph."
  (let ((current-node (treesit-node-at (point))))
    (if (clojure-ts--match-docstring nil current-node nil)
        (let ((fill-column (or clojure-ts-docstring-fill-column fill-column))
              (fill-prefix (clojure-ts--docstring-fill-prefix))
              (beg-doc (treesit-node-start current-node))
              (end-doc (treesit-node-end current-node)))
          (save-restriction
            (narrow-to-region beg-doc end-doc)
            (fill-paragraph justify)))
      (or (fill-comment-paragraph justify)
          (fill-paragraph justify)))
    t))

(defconst clojure-ts--sexp-nodes
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

(defconst clojure-ts--list-nodes
  '("list_lit" "anon_fn_lit" "read_cond_lit" "splicing_read_cond_lit"
    "map_lit" "ns_map_lit" "vec_lit" "set_lit")
  "A regular expression that matches nodes that can be treated as lists.")

(defconst clojure-ts--thing-settings
  `((clojure
     (sexp ,(regexp-opt clojure-ts--sexp-nodes))
     (list ,(regexp-opt clojure-ts--list-nodes))
     (text ,(regexp-opt '("comment"))))))

(defvar clojure-ts-mode-map
  (let ((map (make-sparse-keymap)))
    ;;(set-keymap-parent map clojure-mode-map)
    map))

(defvar clojure-ts-clojurescript-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-ts-mode-map)
    map))

(defvar clojure-ts-clojurec-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-ts-mode-map)
    map))

(defvar clojure-ts-clojuredart-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-ts-mode-map)
    map))

(defvar clojure-ts-jank-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-ts-mode-map)
    map))

(defun clojure-ts-mode-display-version ()
  "Display the current `clojure-mode-version' in the minibuffer."
  (interactive)
  (let ((pkg-version (package-get-version)))
    (if pkg-version
        (message "clojure-ts-mode %s (package: %s)" clojure-ts-mode-version pkg-version)
      (message "clojure-ts-mode %s" clojure-ts-mode-version))))

(defconst clojure-ts-grammar-recipes
  '((clojure "https://github.com/sogaiu/tree-sitter-clojure.git"
             "v0.0.13")
    (markdown-inline "https://github.com/MDeiml/tree-sitter-markdown"
                     "v0.4.1"
                     "tree-sitter-markdown-inline/src"))
  "Intended to be used as the value for `treesit-language-source-alist'.")

(defun clojure-ts--ensure-grammars ()
  "Install required language grammars if not already available."
  (when clojure-ts-ensure-grammars
    (dolist (recipe clojure-ts-grammar-recipes)
      (let ((grammar (car recipe)))
        (unless (treesit-language-available-p grammar nil)
          (message "Installing %s tree-sitter grammar" grammar)
          ;; `treesit-language-source-alist' is dynamically scoped.
          ;; Binding it in this let expression allows
          ;; `treesit-install-language-gramamr' to pick up the grammar recipes
          ;; without modifying what the user has configured themselves.
          (let ((treesit-language-source-alist clojure-ts-grammar-recipes))
            (treesit-install-language-grammar grammar)))))))

(defun clojure-ts-reinstall-grammars ()
  "Install the required versions of language grammars.

If the grammars are already installed, they will be reinstalled.  This
function can also be used to upgrade the grammars if they are outdated."
  (interactive)
  (dolist (recipe clojure-ts-grammar-recipes)
    (let ((grammar (car recipe)))
      (message "Installing %s tree-sitter grammar" grammar)
      (let ((treesit-language-source-alist clojure-ts-grammar-recipes))
        (treesit-install-language-grammar grammar)))))

(defun clojure-ts-mode-variables (&optional markdown-available)
  "Initialize buffer-local variables for `clojure-ts-mode'.
See `clojure-ts--font-lock-settings' for usage of MARKDOWN-AVAILABLE."
  (setq-local comment-add 1)
  (setq-local comment-start ";")

  (setq-local treesit-font-lock-settings
              (clojure-ts--font-lock-settings markdown-available))
  (setq-local treesit-font-lock-feature-list
              '((comment definition variable)
                (keyword string char symbol builtin type)
                (constant number quote metadata doc)
                (bracket deref function regex tagged-literals)))

  (setq-local treesit-defun-prefer-top-level t)
  (setq-local treesit-defun-tactic 'top-level)
  (setq-local treesit-defun-type-regexp
              (cons
               ;; consider all clojure sexps as valid top level forms...
               (regexp-opt clojure-ts--sexp-nodes)
               ;; ...except `comment' forms if `clojure-ts-toplevel-inside-comment-form' is set
               (lambda (node)
                 (or (not clojure-ts-toplevel-inside-comment-form)
                     (not (clojure-ts--definition-node-p "comment" node))))))
  (setq-local treesit-defun-name-function
              #'clojure-ts--standard-definition-node-name)

  (setq-local treesit-simple-indent-rules
              (clojure-ts--configured-indent-rules))
  (setq-local fill-paragraph-function #'clojure-ts--fill-paragraph)

  (setq-local treesit-simple-imenu-settings
              clojure-ts--imenu-settings)

  (when (boundp 'treesit-thing-settings) ;; Emacs 30+
    (setq-local treesit-thing-settings clojure-ts--thing-settings)))

;;;###autoload
(define-derived-mode clojure-ts-mode prog-mode "Clojure[TS]"
  "Major mode for editing Clojure code.

\\{clojure-ts-mode-map}"
  :syntax-table clojure-ts-mode-syntax-table
  (clojure-ts--ensure-grammars)
  (let ((use-markdown-inline (and clojure-ts-use-markdown-inline
                                  (treesit-ready-p 'markdown-inline t))))
    (when use-markdown-inline
      (treesit-parser-create 'markdown-inline)
      (setq-local treesit-range-settings clojure-ts--treesit-range-settings))

    (when (treesit-ready-p 'clojure)
      (treesit-parser-create 'clojure)
      (clojure-ts-mode-variables use-markdown-inline)

      (when clojure-ts--debug
        (setq-local treesit--indent-verbose t)
        (when (eq clojure-ts--debug 'font-lock)
          (setq-local treesit--font-lock-verbose t))
        (treesit-inspect-mode))

      (treesit-major-mode-setup)

      ;; Initial indentation rules cache calculation.
      (setq clojure-ts--semantic-indent-rules-cache
            (clojure-ts--compute-semantic-indentation-rules-cache clojure-ts-semantic-indent-rules))

      ;; If indentation rules are set in `.dir-locals.el', it is advisable to
      ;; recalculate the buffer-local value whenever the value changes.
      (add-hook 'hack-local-variables-hook
                (lambda ()
                  (setq clojure-ts--semantic-indent-rules-cache
                        (clojure-ts--compute-semantic-indentation-rules-cache clojure-ts-semantic-indent-rules)))
                0
                t)

      ;; Workaround for treesit-transpose-sexps not correctly working with
      ;; treesit-thing-settings on Emacs 30.
      ;; Once treesit-transpose-sexps it working again this can be removed
      (when (and (fboundp 'transpose-sexps-default-function)
                 (< emacs-major-version 31))
        (setq-local transpose-sexps-function #'transpose-sexps-default-function)))))

;; For Emacs 30+, so that `clojure-ts-mode' is treated as deriving from
;; `clojure-mode' in the context of `derived-mode-p'
(derived-mode-add-parents 'clojure-ts-mode '(clojure-mode))

;;;###autoload
(define-derived-mode clojure-ts-clojurescript-mode clojure-ts-mode "ClojureScript[TS]"
  "Major mode for editing ClojureScript code.

\\{clojure-ts-clojurescript-mode-map}")

;;;###autoload
(define-derived-mode clojure-ts-clojurec-mode clojure-ts-mode "ClojureC[TS]"
  "Major mode for editing ClojureC code.

\\{clojure-ts-clojurec-mode-map}")

;;;###autoload
(define-derived-mode clojure-ts-clojuredart-mode clojure-ts-mode "ClojureDart[TS]"
  "Major mode for editing Clojure Dart code.

\\{clojure-ts-clojuredart-mode-map}")

;;;###autoload
(define-derived-mode clojure-ts-jank-mode clojure-ts-mode "Jank[TS]"
  "Major mode for editing Jank code.

\\{clojure-ts-jank-mode-map}")

(defun clojure-ts--register-novel-modes ()
  "Set up Clojure modes not present in progenitor clojure-mode.el."
  (add-to-list 'auto-mode-alist '("\\.cljd\\'" . clojure-ts-clojuredart-mode))
  (add-to-list 'auto-mode-alist '("\\.jank\\'" . clojure-ts-jank-mode)))

(defun clojure-ts-activate-mode-remappings ()
  "Remap all `clojure-mode' file-specified modes to use `clojure-ts-mode'.

Useful if you want to try out `clojure-ts-mode' without having to manually
update the mode mappings."
  (interactive)
  (dolist (entry clojure-ts-mode-remappings)
    (add-to-list 'major-mode-remap-defaults entry)))

(defun clojure-ts-deactivate-mode-remappings ()
  "Undo `clojure-ts-mode' file-specified mode remappings.

Useful if you want to switch to the `clojure-mode's mode mappings."
  (interactive)
  (dolist (entry clojure-ts-mode-remappings)
    (setq major-mode-remap-defaults (remove entry major-mode-remap-defaults))))

(if (treesit-available-p)
    ;; Redirect clojure-mode to clojure-ts-mode if clojure-mode is present
    (if (require 'clojure-mode nil 'noerror)
        (progn
          (when clojure-ts-auto-remap
            (clojure-ts-activate-mode-remappings))
          (clojure-ts--register-novel-modes))
      ;; When Clojure-mode is not present, setup auto-modes ourselves
      (progn
        ;; Regular clojure/edn files
        ;; I believe dtm is for datomic queries and datoms, which are just edn.
        (add-to-list 'auto-mode-alist
                     '("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-ts-mode))
        (add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-ts-clojurescript-mode))
        (add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojure-ts-clojurec-mode))
        ;; boot build scripts are Clojure source files
        (add-to-list 'auto-mode-alist '("\\(?:build\\|profile\\)\\.boot\\'" . clojure-ts-mode))
        ;; babashka scripts are Clojure source files
        (add-to-list 'interpreter-mode-alist '("bb" . clojure-ts-mode))
        ;; nbb scripts are ClojureScript source files
        (add-to-list 'interpreter-mode-alist '("nbb" . clojure-ts-clojurescript-mode))
        (clojure-ts--register-novel-modes)))
  (message "Clojure TS Mode will not be activated as tree-sitter support is missing."))

(defvar clojure-ts--find-ns-query
  (treesit-query-compile
   'clojure
   '(((source (list_lit
               :anchor (sym_lit name: (sym_name) @ns)
               :anchor (sym_lit name: (sym_name) @ns-name)))
      (:equal @ns "ns"))
     ((source (list_lit
               :anchor (sym_lit name: (sym_name) @in-ns)
               :anchor (quoting_lit
                        :anchor (sym_lit name: (sym_name) @ns-name))))
      (:equal @in-ns "in-ns")))))

(defun clojure-ts-find-ns ()
  "Return the name of the current namespace."
  (let ((nodes (treesit-query-capture 'clojure clojure-ts--find-ns-query)))
    (treesit-node-text (cdr (assoc 'ns-name nodes)) t)))

(provide 'clojure-ts-mode)

;;; clojure-ts-mode.el ends here
