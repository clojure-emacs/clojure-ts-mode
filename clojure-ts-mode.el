;;; clojure-ts-mode.el --- Major mode for Clojure code -*- lexical-binding: t; -*-

;; Copyright © 2022-2025 Danny Freeman, Bozhidar Batsov and contributors
;;
;; Authors: Danny Freeman <danny@dfreeman.email>
;;          Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/clojure-emacs/clojure-ts-mode
;; Keywords: languages clojure clojurescript lisp
;; Version: 0.5.0-snapshot
;; Package-Requires: ((emacs "30.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock, indentation, and navigation for the
;; Clojure programming language (http://clojure.org).

;; For the Tree-sitter grammar this mode is based on,
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
(require 'align)
(require 'subr-x)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-eq "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")

(defgroup clojure-ts nil
  "Major mode for editing Clojure code with Tree-sitter."
  :prefix "clojure-ts-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/clojure-emacs/clojure-ts-mode")
  :link '(emacs-commentary-link :tag "Commentary" "clojure-mode"))

(defconst clojure-ts-mode-version
  "0.5.0-snapshot"
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
  "When non-nil, ensure required Tree-sitter grammars are installed."
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

(defcustom clojure-ts-use-regex-parser t
  "When non-nil, use separate grammar to highlight regex syntax."
  :type 'boolean
  :safe #'booleanp
  :package-version '(clojure-ts-mode . "0.4"))

(defcustom clojure-ts-clojurescript-use-js-parser t
  "When non-nil, use JS grammar to highlight syntax in js* forms."
  :type 'boolean
  :safe #'booleanp
  :package-version '(clojure-ts-mode . "0.5"))

(defcustom clojure-ts-jank-use-cpp-parser t
  "When non-nil, use C++ grammar to highlight syntax in native/raw forms."
  :type 'boolean
  :safe #'booleanp
  :package-version '(clojure-ts-mode . "0.5"))

(defcustom clojure-ts-auto-remap t
  "When non-nil, redirect all `clojure-mode' buffers to `clojure-ts-mode'."
  :safe #'booleanp
  :type 'boolean
  :package-version '(clojure-ts-mode . "0.3"))

(defcustom clojure-ts-outline-variant 'comments
  "Determines how `clojure-ts-mode' integrates with `outline-minor-mode'.

If set to the symbol `comments', then top-level comments starting with
three or more semicolons will be treated as outline headings.  If set to
`imenu', then def-like forms are treated as outline headings."
  :safe #'symbolp
  :type '(choice (const :tag "Use special comments" comments)
                 (const :tag "Use imenu" imenu))
  :package-version '(clojure-ts-mode . "0.4"))

(defcustom clojure-ts-refactor-map-prefix "C-c C-r"
  "Clojure refactor keymap prefix."
  :type 'string
  :package-version '(clojure-ts-mode . "0.4"))

(defcustom clojure-ts-thread-all-but-last nil
  "Non-nil means do not thread the last expression.

This means that `clojure-ts-thread-first-all' and
`clojure-ts-thread-last-all' not thread the deepest sexp inside the
current sexp."
  :package-version '(clojure-ts-mode . "0.4")
  :safe #'booleanp
  :type 'boolean)

(defcustom clojure-ts-use-metadata-for-defn-privacy nil
  "If nil, `clojure-ts-cycle-privacy' will use (defn- f []).

If t, it will use (defn ^:private f [])."
  :package-version '(clojure-ts-mode . "0.4")
  :safe #'booleanp
  :type 'boolean)

(defcustom clojure-ts-align-reader-conditionals nil
  "Whether to align reader conditionals, as if they were maps."
  :package-version '(clojure-ts-mode . "0.4")
  :safe #'booleanp
  :type 'boolean)

(defcustom clojure-ts-align-binding-forms
  '("let"
    "when-let"
    "when-some"
    "if-let"
    "if-some"
    "binding"
    "loop"
    "doseq"
    "for"
    "with-open"
    "with-local-vars"
    "with-redefs"
    "clojure.core/let"
    "clojure.core/when-let"
    "clojure.core/when-some"
    "clojure.core/if-let"
    "clojure.core/if-some"
    "clojure.core/binding"
    "clojure.core/loop"
    "clojure.core/doseq"
    "clojure.core/for"
    "clojure.core/with-open"
    "clojure.core/with-local-vars"
    "clojure.core/with-redefs")
  "List of strings matching forms that have binding forms."
  :package-version '(clojure-ts-mode . "0.4")
  :safe #'listp
  :type '(repeat string))

(defconst clojure-ts--align-separator-newline-regexp "^ *$")

(defcustom clojure-ts-align-separator clojure-ts--align-separator-newline-regexp
  "Separator passed to `align-region' when performing vertical alignment."
  :package-version '(clojure-ts-mode . "0.4")
  :type `(choice (const :tag "Make blank lines prevent vertical alignment from happening."
                        ,clojure-ts--align-separator-newline-regexp)
                 (other :tag "Allow blank lines to happen within a vertically-aligned expression."
                        entire)))

(defcustom clojure-ts-align-cond-forms
  '("condp"
    "cond"
    "cond->"
    "cond->>"
    "case"
    "are"
    "clojure.core/condp"
    "clojure.core/cond"
    "clojure.core/cond->"
    "clojure.core/cond->>"
    "clojure.core/case"
    "clojure.core/are")
  "List of strings identifying cond-like forms."
  :package-version '(clojure-ts-mode . "0.4")
  :safe #'listp
  :type '(repeat string))

(defcustom clojure-ts-align-forms-automatically nil
  "If non-nil, vertically align some forms automatically.

Automatically means it is done as part of indenting code.  This applies
to binding forms (`clojure-ts-align-binding-forms'), to cond
forms (`clojure-ts-align-cond-forms') and to map literals.  For
instance, selecting a map a hitting
\\<clojure-ts-mode-map>`\\[indent-for-tab-command]' will align the
values like this:

{:some-key 10
 :key2     20}"
  :package-version '(clojure-ts-mode . "0.4")
  :safe #'booleanp
  :type 'boolean)

(defcustom clojure-ts-extra-def-forms nil
  "List of forms that should be fontified the same way as defn."
  :package-version '(clojure-ts-mode . "0.5")
  :safe #'listp
  :type '(repeat string))

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
  (eval-when-compile
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
  (eval-when-compile
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
               "extend-protocol" "extend-type" "extend"
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

(defconst clojure-ts-function-docstring-symbols
  (rx line-start
      (or "definline"
          "defmulti"
          "defmacro"
          "defn"
          "defn-"
          "defprotocol"
          "ns")
      line-end)
  "Symbols that accept an optional docstring as their second argument.")

(defconst clojure-ts-definition-docstring-symbols
  (rx line-start "def" line-end)
  "Symbols that accept an optional docstring as their second argument.
Any symbols added here should only treat their second argument as a docstring
if a third argument (the value) is provided.
\"def\" is the only builtin Clojure symbol that behaves like this.")

(defconst clojure-ts--variable-definition-symbol-regexp
  (rx line-start (or "def" "defonce") line-end)
  "A regular expression matching a symbol used to define a variable.")

(defconst clojure-ts--typedef-symbol-regexp
  (rx line-start
      (or "defprotocol" "defmulti" "deftype" "defrecord"
          "definterface" "defmethod" "defstruct")
      line-end)
  "A regular expression matching a symbol used to define a type.")

(defconst clojure-ts--type-symbol-regexp
  (rx line-start
      (or "deftype" "defrecord"
          ;; While not reifying, helps with doc strings
          "defprotocol" "definterface"
          "reify" "proxy" "extend-type" "extend-protocol")
      line-end)
  "A regular expression matching a symbol used to define or instantiate a type.")

(defconst clojure-ts--interface-def-symbol-regexp
  (rx line-start (or "defprotocol" "definterface") line-end)
  "A regular expression matching a symbol used to define an interface.")

(defun clojure-ts--docstring-query (capture-symbol)
  "Return a query that captures docstrings with CAPTURE-SYMBOL."
  `(;; Captures docstrings in def
    ((list_lit :anchor [(comment) (meta_lit) (old_meta_lit)] :*
               :anchor (sym_lit) @_def_symbol
               :anchor [(comment) (meta_lit) (old_meta_lit)] :*
               ;; Variable name
               :anchor (sym_lit)
               :anchor [(comment) (meta_lit) (old_meta_lit)] :*
               :anchor (str_lit (str_content) ,capture-symbol) @font-lock-doc-face
               ;; The variable's value
               :anchor (_))
     (:match ,clojure-ts-definition-docstring-symbols
             @_def_symbol))
    ;; Captures docstrings in metadata of definitions
    ((list_lit :anchor [(comment) (meta_lit) (old_meta_lit)] :*
               :anchor (sym_lit) @_def_symbol
               :anchor (comment) :*
               :anchor (meta_lit
                        value: (map_lit
                                (kwd_lit) @_doc-keyword
                                :anchor (str_lit (str_content) ,capture-symbol) @font-lock-doc-face)))
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
    ((list_lit :anchor [(comment) (meta_lit) (old_meta_lit)] :*
               :anchor (sym_lit) @_def_symbol
               :anchor [(comment) (meta_lit) (old_meta_lit)] :*
               ;; Function_name
               :anchor (sym_lit)
               :anchor [(comment) (meta_lit) (old_meta_lit)] :*
               :anchor (str_lit (str_content) ,capture-symbol) @font-lock-doc-face)
     (:match ,clojure-ts-function-docstring-symbols
             @_def_symbol))
    ((list_lit :anchor [(comment) (meta_lit) (old_meta_lit)] :*
               :anchor (sym_lit) @_def_symbol
               :anchor [(comment) (meta_lit) (old_meta_lit)] :*
               ;; Function_name
               :anchor (sym_lit)
               :anchor [(comment) (meta_lit) (old_meta_lit)] :*
               :anchor (str_lit (str_content) ,capture-symbol) @font-lock-doc-face)
     (:match ,(clojure-ts-symbol-regexp clojure-ts-extra-def-forms)
             @_def_symbol))
    ;; Captures docstrings in defprotcol, definterface
    ((list_lit :anchor [(comment) (meta_lit) (old_meta_lit)] :*
               :anchor (sym_lit) @_def_symbol
               (list_lit :anchor (sym_lit) (vec_lit) :*
                         (str_lit (str_content) ,capture-symbol) @font-lock-doc-face)
               :*)
     (:match ,clojure-ts--interface-def-symbol-regexp @_def_symbol))))

(defconst clojure-ts--match-docstring-query
  (treesit-query-compile 'clojure (clojure-ts--docstring-query '@font-lock-doc-face))
  "Precompiled query that matches a Clojure docstring.")

(defun clojure-ts--treesit-range-settings (use-markdown-inline use-regex)
  "Return value for `treesit-range-settings' for `clojure-ts-mode'.

When USE-MARKDOWN-INLINE is non-nil, include range settings for
markdown-inline parser.

When USE-REGEX is non-nil, include range settings for regex parser."
  (append
   (when use-markdown-inline
     (treesit-range-rules
      :embed 'markdown-inline
      :host 'clojure
      :local t
      (clojure-ts--docstring-query '@capture)))
   (when use-regex
     (treesit-range-rules
      :embed 'regex
      :host 'clojure
      :local t
      '((regex_content) @capture)))))

(defun clojure-ts--fontify-string (node override _start _end &optional _rest)
  "Fontify string content NODE with `font-lock-string-face'.

In order to support embedded syntax highlighting for JS in ClojureScript
and C++ in Jank we need to avoid fontifying string content in some
special forms, such as native/raw in Jank and js* in ClojureScript,
otherwise string face will interfere with embedded parser's faces.

This function respects OVERRIDE argument by passing it to
`treesit-fontify-with-override'.

START and END arguments that are passed to this function are not start
and end of the NODE, so we ignore them."
  (let* ((prev (treesit-node-prev-sibling (treesit-node-parent node)))
         (jank-native-p (and (derived-mode-p 'clojure-ts-jank-mode)
                             clojure-ts-jank-use-cpp-parser
                             (clojure-ts--symbol-node-p prev)
                             (string= (treesit-node-text prev) "native/raw")))
         (js-interop-p (and (derived-mode-p 'clojure-ts-clojurescript-mode)
                            clojure-ts-clojurescript-use-js-parser
                            (clojure-ts--symbol-node-p prev)
                            (string= (treesit-node-text prev) "js*"))))
    (when (not (or jank-native-p js-interop-p))
      (treesit-fontify-with-override (treesit-node-start node)
                                     (treesit-node-end node)
                                     'font-lock-string-face
                                     override))))

(defun clojure-ts--font-lock-settings (markdown-available regex-available)
  "Return font lock settings suitable for use in `treesit-font-lock-settings'.

When MARKDOWN-AVAILABLE is non-nil, includes rules for highlighting docstrings
with the markdown-inline grammar.

When REGEX-AVAILABLE is non-nil, includes rules for highlighting regex
literals with regex grammar."
  (append
   (treesit-font-lock-rules
    :feature 'string
    :language 'clojure
    '((str_lit open: _ @font-lock-string-face
               (str_content) @clojure-ts--fontify-string
               close: _ @font-lock-string-face)
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
    `(((list_lit :anchor [(comment) (meta_lit) (old_meta_lit)] :*
                 :anchor (sym_lit !namespace name: (sym_name) @font-lock-keyword-face))
       (:match ,clojure-ts--builtin-symbol-regexp @font-lock-keyword-face))
      ((list_lit :anchor [(comment) (meta_lit) (old_meta_lit)] :*
                 :anchor (sym_lit namespace: ((sym_ns) @ns
                                              (:equal "clojure.core" @ns))
                                  name: (sym_name) @font-lock-keyword-face))
       (:match ,clojure-ts--builtin-symbol-regexp @font-lock-keyword-face))
      ((anon_fn_lit :anchor [(comment) (meta_lit) (old_meta_lit)] :*
                    :anchor (sym_lit !namespace name: (sym_name) @font-lock-keyword-face))
       (:match ,clojure-ts--builtin-symbol-regexp @font-lock-keyword-face))
      ((anon_fn_lit :anchor [(comment) (meta_lit) (old_meta_lit)] :*
                    :anchor (sym_lit namespace: ((sym_ns) @ns
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
    `(((list_lit :anchor [(comment) (meta_lit) (old_meta_lit)] :*
                 :anchor (sym_lit (sym_name) @font-lock-keyword-face)
                 :anchor [(comment) (meta_lit) (old_meta_lit)] :*
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
                        "definline"
                        "defonce")
                       eol))
               @font-lock-keyword-face))
      ((list_lit :anchor [(comment) (meta_lit) (old_meta_lit)] :*
                 :anchor (sym_lit (sym_name) @font-lock-keyword-face)
                 :anchor [(comment) (meta_lit) (old_meta_lit)] :*
                 :anchor (sym_lit (sym_name) @font-lock-function-name-face))
       (:match ,(clojure-ts-symbol-regexp clojure-ts-extra-def-forms)
               @font-lock-keyword-face))
      ((anon_fn_lit
        marker: "#" @font-lock-property-face))
      ;; Methods implementation
      ((list_lit
        :anchor [(comment) (meta_lit) (old_meta_lit)] :*
        :anchor ((sym_lit name: (sym_name) @def)
                 ((:match ,(rx-to-string
                            `(seq bol
                                  (or
                                   "defrecord"
                                   "definterface"
                                   "deftype"
                                   "defprotocol")
                                  eol))
                          @def)))
        :anchor [(comment) (meta_lit) (old_meta_lit)] :*
        :anchor (sym_lit (sym_name) @font-lock-type-face)
        (list_lit
         (sym_lit name: (sym_name) @font-lock-function-name-face))))
      ((list_lit
        ((sym_lit name: (sym_name) @def)
         ((:match ,(rx-to-string
                    `(seq bol
                          (or "reify"
                              "extend-protocol"
                              "extend-type")
                          eol))
                  @def)))
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
    `(((list_lit :anchor [(comment) (meta_lit) (old_meta_lit)] :*
                 :anchor (sym_lit (sym_name) @font-lock-keyword-face)
                 :anchor [(comment) (meta_lit) (old_meta_lit)] :*
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
      :override 'prepend
      `([((image_description) @link)
         ((link_destination) @font-lock-constant-face)
         ((code_span) @font-lock-constant-face)
         ((emphasis) @underline)
         ((strong_emphasis) @bold)
         (inline_link (link_text) @link)
         (inline_link (link_destination) @font-lock-constant-face)
         (shortcut_link (link_text) @link)])))

   (when regex-available
     ;; Queries are adapted from
     ;; https://github.com/tree-sitter/tree-sitter-regex/blob/v0.24.3/queries/highlights.scm.
     (treesit-font-lock-rules
      :feature 'regex
      :language 'regex
      :override t
      '((["("
           ")"
           "(?"
           "(?:"
           "(?<"
           "(?P<"
           "(?P="
           ">"
           "["
           "]"
           "{"
           "}"
           "[:"
           ":]"] @font-lock-regexp-grouping-construct)
         (["*"
           "+"
           "?"
           "|"
           "="
           "!"] @font-lock-property-name-face)
         ((group_name) @font-lock-variable-name-face)
         ((count_quantifier
           [(decimal_digits) @font-lock-number-face
            "," @font-lock-delimiter-face]))
         ((flags) @font-lock-constant-face)
         ((character_class
           ["^" @font-lock-escape-face
            (class_range "-" @font-lock-escape-face)]))
         ((identity_escape) @font-lock-builtin-face)
         ([(start_assertion) (end_assertion)] @font-lock-constant-face))))

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
       meta: (meta_lit) :* @font-lock-comment-face
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

(defun clojure-ts--vec-node-p (node)
  "Return non-nil if NODE is a Clojure vector."
  (string-equal "vec_lit" (treesit-node-type node)))

(defun clojure-ts--anon-fn-node-p (node)
  "Return non-nil if NODE is a Clojure function literal."
  (string-equal "anon_fn_lit" (treesit-node-type node)))

(defun clojure-ts--opening-paren-node-p (node)
  "Return non-nil if NODE is an opening paren."
  (string-equal "(" (treesit-node-text node)))

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
  (or (string-equal "meta_lit" (treesit-node-type node))
      (string-equal "old_meta_lit" (treesit-node-type node))))

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

(defun clojure-ts--first-value-child (node)
  "Return the first value child of the given NODE.

In the syntax tree, there are a few types of possible child nodes:
unnamed standalone nodes (e.g., comments), anonymous nodes (e.g.,
opening or closing parentheses), and named nodes.  Named nodes are
standalone nodes that are labeled by a specific name.  The most common
names are meta and value.  This function skips any unnamed, anonymous,
and metadata nodes and returns the first value node."
  (treesit-node-child-by-field-name node "value"))

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

Return nil if NODE is not a list with symbols as the first two
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

Return nil if NODE is not a list where the first element is a symbol
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

;;; Outline settings

(defun clojure-ts--outline-predicate (node)
  "Return TRUE if NODE is an outline heading comment."
  (and (string= (treesit-node-type node) "comment")
       (string-match-p "^\\(?:;;;;* \\).*" (treesit-node-text node))))

(defun clojure-ts--outline-level ()
  "Return the current level of the outline heading at point."
  (let* ((node (treesit-outline--at-point))
         (node-text (treesit-node-text node)))
    (string-match ";;\\(;+\\) " node-text)
    (- (match-end 1) (match-beginning 1))))

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
    ("definterface"    . ((:block 1) (:inner 1)))
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

(defun clojure-ts--find-semantic-rules-for-node (node)
  "Return a list of semantic rules for NODE."
  (let* ((first-child (clojure-ts--node-child-skip-metadata node 0))
         (symbol-name (clojure-ts--named-node-text first-child))
         (symbol-namespace (clojure-ts--node-namespace-text first-child)))
    (or (clojure-ts--dynamic-indent-for-symbol symbol-name symbol-namespace)
        (alist-get symbol-name
                   clojure-ts--semantic-indent-rules-cache
                   nil
                   nil
                   #'equal))))

(defun clojure-ts--find-semantic-rule (node parent current-depth)
  "Return a suitable indentation rule for NODE, considering the CURRENT-DEPTH.

Attempts to find an indentation rule by examining the symbol name of the
PARENT's first child.  If a rule is not found, it navigates up the
syntax tree and recursively attempts to find a rule, incrementally
increasing the CURRENT-DEPTH.  If a rule is not found upon reaching the
root of the syntax tree, it returns nil.  A rule is considered a match
only if the CURRENT-DEPTH matches the rule's required depth."
  (let* ((idx (- (treesit-node-index node) 2)))
    (if-let* ((rule-set (clojure-ts--find-semantic-rules-for-node parent)))
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
      ;; Let's go no more than 3 levels up to avoid performance degradation.
      (when-let* (((< current-depth 3))
                  (new-parent (treesit-node-parent parent)))
        (clojure-ts--find-semantic-rule parent
                                        new-parent
                                        (1+ current-depth))))))

(defun clojure-ts--match-form-body (node parent bol)
  "Match if NODE has to be indented as a for body.

PARENT not should be a list.  If first symbol in the expression has an
indentation rule in `clojure-ts--semantic-indent-rules-defaults' or
`clojure-ts-semantic-indent-rules' check if NODE should be indented
according to the rule.  If NODE is nil, use next node after BOL."
  (and (or (clojure-ts--list-node-p parent)
           (clojure-ts--anon-fn-node-p parent))
       (let* ((first-child (clojure-ts--first-value-child parent)))
         (when-let* ((rule (clojure-ts--find-semantic-rule node parent 0)))
           (let ((rule-type (car rule))
                 (rule-value (cadr rule)))
             (if (equal rule-type :block)
                 (if (zerop rule-value)
                     ;; Special treatment for block 0 rule.
                     (clojure-ts--match-block-0-body bol first-child)
                   (clojure-ts--node-pos-match-block node parent bol rule-value))
               ;; Return true for any inner rule.
               t))))))

(defun clojure-ts--match-function-call-arg (node parent _bol)
  "Match NODE if PARENT is a list expressing a function or macro call."
  (and (or (clojure-ts--list-node-p parent)
           (clojure-ts--anon-fn-node-p parent))
       (let ((first-child (clojure-ts--first-value-child parent))
             (second-child (clojure-ts--node-child-skip-metadata parent 1)))
         (and first-child
              ;; Does the second child exist, and is it not equal to the current node?
              second-child
              (not (treesit-node-eq second-child node))
              (or (clojure-ts--symbol-node-p first-child)
                  (clojure-ts--keyword-node-p first-child)
                  (clojure-ts--var-node-p first-child))))))

(defvar clojure-ts--threading-macro
  (eval-and-compile
    (rx (and "->" (? ">") line-end)))
  "A regular expression matching a threading macro.")

(defun clojure-ts--match-threading-macro-arg (_node parent _)
  "Match NODE if it is an argument to a PARENT threading macro."
  ;; We want threading macros to indent 2 only if the ->> is on it's own line.
  ;; If not, then align function arg.
  (and (or (clojure-ts--list-node-p parent)
           (clojure-ts--anon-fn-node-p parent))
       (let ((first-child (clojure-ts--first-value-child parent)))
         (clojure-ts--symbol-matches-p
          clojure-ts--threading-macro
          first-child))))

(defun clojure-ts--match-docstring (_node parent _bol)
  "Match PARENT when it is a docstring node."
  (when-let* ((top-level-node (treesit-parent-until parent 'defun t))
              (result (treesit-query-capture top-level-node
                                             clojure-ts--match-docstring-query)))
    (seq-find (lambda (elt)
                (and (eq (car elt) 'font-lock-doc-face)
                     (treesit-node-eq (cdr elt) parent)))
              result)))

(defun clojure-ts--match-with-metadata (node &optional _parent _bol)
  "Match NODE when it has metadata."
  (when-let* ((prev-sibling (treesit-node-prev-sibling node)))
    (clojure-ts--metadata-node-p prev-sibling)))

(defun clojure-ts--anchor-parent-opening-paren (_node parent _bol)
  "Return position of PARENT start for NODE.

If PARENT has optional metadata we skip it and return starting position
of the first child's opening paren.

NOTE: This serves as an anchor function to resolve an indentation issue
for forms with type hints."
  (thread-first parent
                (treesit-search-subtree #'clojure-ts--opening-paren-node-p nil t 1)
                (treesit-node-start)))

(defun clojure-ts--anchor-nth-sibling (n)
  "Return the start of the Nth child of PARENT skipping metadata."
  (lambda (_n parent &rest _)
    (treesit-node-start (treesit-node-child parent n t))))

(defun clojure-ts--semantic-indent-rules ()
  "Return a list of indentation rules for `treesit-simple-indent-rules'.

NOTE: All built-in matchers (such as `parent-is' etc) expect a node type
regex.  Therefore, if the string map_lit is used, it will incorrectly
match both map_lit and ns_map_lit.  To prevent this, more precise
regexes with anchors matching the beginning and end of the line are
used."
  `((clojure
     ((parent-is "^source$") parent-bol 0)
     ;; Literal Sequences
     ((parent-is "^vec_lit$") parent 1) ;; https://guide.clojure.style/#bindings-alignment
     ((parent-is "^map_lit$") parent 1) ;; https://guide.clojure.style/#map-keys-alignment
     ((parent-is "^set_lit$") parent 2)
     ((parent-is "^splicing_read_cond_lit$") parent 4)
     ((parent-is "^read_cond_lit$") parent 3)
     ((parent-is "^tagged_or_ctor_lit$") parent 0)
     ((parent-is "^ns_map_lit$") (nth-sibling 2) 1)
     ;; https://guide.clojure.style/#body-indentation
     (clojure-ts--match-form-body clojure-ts--anchor-parent-opening-paren 2)
     ;; https://guide.clojure.style/#threading-macros-alignment
     (clojure-ts--match-threading-macro-arg prev-sibling 0)
     ;; https://guide.clojure.style/#vertically-align-fn-args
     (clojure-ts--match-function-call-arg ,(clojure-ts--anchor-nth-sibling 1) 0)
     ;; https://guide.clojure.style/#one-space-indent
     ((parent-is "^list_lit$") parent 1)
     ((parent-is "^anon_fn_lit$") parent 2)
     (clojure-ts--match-with-metadata parent 0)
     ;; This is slow and only matches when point is inside of a docstring and
     ;; only when Markdown grammar is disabled.  `indent-region' tries to match
     ;; all the rules from top to bottom, so order matters here (the slowest
     ;; rules should be at the bottom).
     (clojure-ts--match-docstring parent 0))))

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
  (let ((current-node (treesit-node-at (point) 'clojure)))
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

(defun clojure-ts--list-node-sym-text (node &optional include-anon-fn-lit)
  "Return text of the first child of the NODE if NODE is a list.

Return nil if the NODE is not a list or if the first child is not a
symbol.  Optionally if INCLUDE-ANON-FN-LIT is non-nil, return the text
of the first symbol of a functional literal NODE."
  (when (or (clojure-ts--list-node-p node)
            (and include-anon-fn-lit
                 (clojure-ts--anon-fn-node-p node)))
    (when-let* ((first-child (clojure-ts--first-value-child node))
                ((clojure-ts--symbol-node-p first-child)))
      (clojure-ts--named-node-text first-child))))

(defun clojure-ts--list-node-sym-match-p (node regex &optional include-anon-fn-lit)
  "Return TRUE if NODE is a list and its first symbol matches the REGEX.

Optionally if INCLUDE-ANON-FN-LIT is TRUE, perform the same check for a
function literal."
  (when-let* ((sym-text (clojure-ts--list-node-sym-text node include-anon-fn-lit)))
    (string-match-p regex sym-text)))

(defconst clojure-ts--sexp-nodes
  '("#_" ;; transpose-sexp near a discard macro moves it around.
    "num_lit" "sym_lit" "kwd_lit" "nil_lit" "bool_lit"
    "regex_lit" "str_lit" "char_lit"
    "list_lit" "map_lit" "vec_lit" "set_lit" "ns_map_lit"
    "anon_fn_lit" "read_cond_lit"
    "var_quoting_lit" "sym_val_lit" "evaling_lit"
    "tagged_or_ctor_lit" "splicing_read_cond_lit"
    "derefing_lit" "quoting_lit" "syn_quoting_lit"
    "unquote_splicing_lit" "unquoting_lit"
    "dis_expr")
  "A regular expression that matches nodes that can be treated as s-expressions.")

(defconst clojure-ts--list-nodes
  '("list_lit" "anon_fn_lit" "read_cond_lit" "splicing_read_cond_lit"
    "map_lit" "ns_map_lit" "vec_lit" "set_lit")
  "A regular expression that matches nodes that can be treated as lists.")

(defun clojure-ts--defun-node-p (node)
  "Return TRUE if NODE is a function or a var definition."
  (clojure-ts--list-node-sym-match-p node
                                     (rx bol
                                         (or "def"
                                             "defn"
                                             "defn-"
                                             "definline"
                                             "defrecord"
                                             "defmacro"
                                             "defmulti"
                                             "defonce"
                                             "defprotocol"
                                             "deftest"
                                             "deftest-"
                                             "ns"
                                             "definterface"
                                             "deftype"
                                             "defstruct")
                                         eol)))

(defconst clojure-ts--markdown-inline-sexp-nodes
  '("inline_link" "full_reference_link" "collapsed_reference_link"
    "uri_autolink" "email_autolink" "shortcut_link" "image"
    "code_span")
  "Nodes representing s-expressions in the `markdown-inline' parser.")

(defun clojure-ts--default-sexp-node-p (node)
  "Return TRUE if point is after the # marker of set or function literal NODE."
  (and (eq (char-before) ?\#)
       (string-match-p (rx bol (or "anon_fn_lit" "set_lit") eol)
                       (treesit-node-type (treesit-node-parent node)))))

(defconst clojure-ts--thing-settings
  `((clojure
     (sexp ,(regexp-opt clojure-ts--sexp-nodes))
     (list ,(regexp-opt clojure-ts--list-nodes))
     ;; `sexp-default' thing allows to fallback to the default implementation of
     ;; `forward-sexp' function where `treesit-forward-sexp' produces undesired
     ;; results.
     (sexp-default
      ;; For `C-M-f' in "#|(a)" or "#|{1 2 3}"
      (,(rx (or "(" "{")) . ,#'clojure-ts--default-sexp-node-p))
     (text ,(regexp-opt '("comment")))
     (defun ,#'clojure-ts--defun-node-p))
    (when clojure-ts-use-markdown-inline
      (markdown-inline
       (sexp ,(regexp-opt clojure-ts--markdown-inline-sexp-nodes))))))

;;; Vertical alignment

(defun clojure-ts--beginning-of-defun-pos ()
  "Return the point that represents the beginning of the current defun."
  (treesit-node-start (treesit-defun-at-point)))

(defun clojure-ts--end-of-defun-pos ()
  "Return the point that represends the end of the current defun."
  (treesit-node-end (treesit-defun-at-point)))

(defun clojure-ts--search-whitespace-after-next-sexp (root-node bound)
  "Move the point after all whitespace following the next s-expression.

Set match data group 1 to this region of whitespace and return the
point.

To move over the next s-expression, fetch the next node after the
current cursor position that is a direct child of ROOT-NODE and navigate
to its end.  The most complex aspect here is handling nodes with
metadata.  Some forms are represented in the syntax tree as a single
s-expression (for example, ^long my-var or ^String (str \"Hello\"
\"world\")), while other forms are two separate s-expressions (for
example, ^long 123 or ^String \"Hello\").  Expressions with two nodes
share some common features:

- The top-level node type is usually sym_lit

- They do not have value children, or they have an empty name.

Regular expression and syntax analysis code is borrowed from
`clojure-mode.'

BOUND bounds the whitespace search."
  (unwind-protect
      (let ((regex "\\([,\s\t]*\\)\\(;+.*\\)?"))
        ;; If we're on an empty line, we should return match, otherwise
        ;; `clojure-ts-align-separator' setting won't work.
        (if (and (bolp) (looking-at-p "[[:blank:]]*$"))
            (progn
              (search-forward-regexp regex bound)
              (point))
          (when-let* ((cur-sexp (treesit-node-first-child-for-pos root-node (point) t)))
            (goto-char (treesit-node-start cur-sexp))
            (if (clojure-ts--metadata-node-p cur-sexp)
                (progn
                  (treesit-end-of-thing 'sexp 1 'restricted)
                  (just-one-space)
                  (treesit-end-of-thing 'sexp 1 'restricted))
              (treesit-end-of-thing 'sexp 1 'restricted))
            (when (looking-at-p ",")
              (forward-char))
            ;; Move past any whitespace or comment.
            (search-forward-regexp regex bound)
            (pcase (syntax-after (point))
              ;; End-of-line, try again on next line.
              (`(12) (progn
                       (forward-char 1)
                       (clojure-ts--search-whitespace-after-next-sexp root-node bound)))
              ;; Closing paren, stop here.
              (`(5 . ,_) nil)
              ;; Anything else is something to align.
              (_ (point))))))
    (when (and bound (> (point) bound))
      (goto-char bound))))

(defun clojure-ts--region-node (beg end)
  "Return the smallest node that covers buffer positions BEG to END."
  (let* ((root-node (treesit-buffer-root-node 'clojure)))
    (treesit-node-descendant-for-range root-node beg end t)))

(defun clojure-ts--node-from-sexp-data (beg end sexp)
  "Return updated node using SEXP data in the region between BEG and END."
  (let* ((new-region-node (clojure-ts--region-node beg end))
         (sexp-beg (marker-position (plist-get sexp :beg-marker)))
         (sexp-end (marker-position (plist-get sexp :end-marker))))
    (treesit-node-descendant-for-range new-region-node
                                       sexp-beg
                                       sexp-end
                                       t)))

(defvar clojure-ts--align-query
  (treesit-query-compile 'clojure
                         `(((map_lit) @map)
                           ((ns_map_lit) @ns-map)
                           ((list_lit
                             ((sym_lit) @sym
                              (:match ,(clojure-ts-symbol-regexp clojure-ts-align-binding-forms) @sym))
                             (vec_lit) @bindings-vec))
                           ((list_lit
                             :anchor
                             ((sym_lit) @sym
                              (:match ,(rx bol (or "for" "doseq") eol) @sym))
                             (vec_lit
                              ((kwd_lit) @kwd
                               (:equal ":let" @kwd))
                              :anchor
                              (vec_lit) @bindings-vec)))
                           ((list_lit
                             ((sym_lit) @sym
                              (:match ,(clojure-ts-symbol-regexp clojure-ts-align-cond-forms) @sym)))
                            @cond)
                           ((anon_fn_lit
                             ((sym_lit) @sym
                              (:match ,(clojure-ts-symbol-regexp clojure-ts-align-binding-forms) @sym))
                             (vec_lit) @bindings-vec))
                           ((anon_fn_lit
                             ((sym_lit) @sym
                              (:match ,(clojure-ts-symbol-regexp clojure-ts-align-cond-forms) @sym)))
                            @cond))))

(defvar clojure-ts--align-reader-conditionals-query
  (treesit-query-compile 'clojure
                         '(((read_cond_lit) @read-cond)
                           ((splicing_read_cond_lit) @read-cond))))

(defun clojure-ts--get-nodes-to-align (beg end)
  "Return a plist of nodes data for alignment.

The search is limited by BEG, END.

Possible node types are: map, bindings-vec, cond or read-cond.

The returned value is a list of property lists.  Each property list
includes `:sexp-type', `:node', `:beg-marker', and `:end-marker'.
Markers are necessary to fetch the same nodes after their boundaries
have changed."
  ;; By default `treesit-query-capture' captures all nodes that cross the range.
  ;; We need to restrict it to only nodes inside of the range.
  (let* ((region-node (clojure-ts--region-node beg end))
         (nodes (append (treesit-query-capture region-node clojure-ts--align-query beg end)
                        (when clojure-ts-align-reader-conditionals
                          (treesit-query-capture region-node clojure-ts--align-reader-conditionals-query beg end)))))
    (thread-last nodes
                 (seq-remove (lambda (elt) (eq (car elt) 'sym)))
                 ;; Reverse the result to align the most deeply nested nodes
                 ;; first.  This way we can prevent breaking alignment of outer
                 ;; nodes.
                 (seq-reverse)
                 ;; When first node is reindented, all other nodes become
                 ;; outdated.  Executing the entire query everytime is very
                 ;; expensive, instead we use markers for every captured node to
                 ;; retrieve only a single node later.
                 (seq-map (lambda (elt)
                            (let* ((sexp-type (car elt))
                                   (node (cdr elt))
                                   (beg-marker (copy-marker (treesit-node-start node) t))
                                   (end-marker (copy-marker (treesit-node-end node))))
                              (list :sexp-type sexp-type
                                    :node node
                                    :beg-marker beg-marker
                                    :end-marker end-marker)))))))

(defun clojure-ts--point-to-align-position (sexp-type node)
  "Move point to the appropriate position to align NODE.

For NODE with SEXP-TYPE map or bindings-vec, the appropriate
position is after the first opening brace.

For NODE with SEXP-TYPE cond, we need to skip the first symbol and the
subsequent special arguments based on block indentation rules."
  (goto-char (treesit-node-start node))
  (when-let* ((cur-sexp (treesit-node-first-child-for-pos node (point) t)))
    (goto-char (treesit-node-start cur-sexp))
    ;; For namespaced maps we need to skip the namespace, which is the first
    ;; nested sexp.
    (when (equal sexp-type 'ns-map)
      (treesit-beginning-of-thing 'sexp -1 'nested))
    ;; For cond forms we need to skip first n + 1 nodes according to block
    ;; indentation rules.  First node to skip is the symbol itself.
    (when (equal sexp-type 'cond)
      (if-let* ((rule-set (clojure-ts--find-semantic-rules-for-node node))
                (rule (car rule-set))
                ((equal (car rule) :block)))
          (treesit-beginning-of-thing 'sexp (1- (- (cadr rule))) 'restrict)
        (treesit-beginning-of-thing 'sexp -1)))))

(defun clojure-ts-align (beg end)
  "Vertically align the contents of the sexp around point.

If region is active, align it.  Otherwise, align everything in the
current \"top-level\" sexp.  When called from lisp code align everything
between BEG and END."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (save-excursion
                   (if (not (treesit-defun-at-point))
                       (user-error "No defun at point")
                     (let ((start (clojure-ts--beginning-of-defun-pos))
                           (end (clojure-ts--end-of-defun-pos)))
                       (list start end))))))
  (setq end (copy-marker end))
  (let* ((sexps-to-align (clojure-ts--get-nodes-to-align beg (marker-position end)))
         ;; We have to disable it here to avoid endless recursion.
         (clojure-ts-align-forms-automatically nil))
    (save-excursion
      (indent-region beg end)
      (dolist (sexp sexps-to-align)
        ;; After reindenting a node, all other nodes in the `sexps-to-align'
        ;; list become outdated, so we need to fetch updated nodes for every
        ;; iteration.
        (let* ((node (clojure-ts--node-from-sexp-data beg (marker-position end) sexp))
               (sexp-type (plist-get sexp :sexp-type))
               (node-end (treesit-node-end node)))
          (clojure-ts--point-to-align-position sexp-type node)
          (align-region (point) node-end nil
                        `((clojure-align (regexp . ,(lambda (&optional bound _noerror)
                                                      (let ((updated-node (clojure-ts--node-from-sexp-data beg (marker-position end) sexp)))
                                                        (clojure-ts--search-whitespace-after-next-sexp updated-node bound))))
                                         (group . 1)
                                         (separate . ,clojure-ts-align-separator)
                                         (repeat . t)))
                        nil)
          ;; After every iteration we have to re-indent the s-expression,
          ;; otherwise some can be indented inconsistently.
          (indent-region (marker-position (plist-get sexp :beg-marker))
                         (plist-get sexp :end-marker))))
      ;; If `clojure-ts-align-separator' is used, `align-region' leaves trailing
      ;; whitespaces on empty lines.
      (delete-trailing-whitespace beg (marker-position end)))))

(defun clojure-ts-indent-region (beg end)
  "Like `indent-region', but also maybe align forms.

Forms between BEG and END are aligned according to
`clojure-ts-align-forms-automatically'."
  (prog1 (let ((indent-region-function #'treesit-indent-region))
           (indent-region beg end))
    (when clojure-ts-align-forms-automatically
      (clojure-ts-align beg end))))

;;; Refactoring

(defun clojure-ts--parent-until (pred)
  "Return the closest parent of node at point that satisfies PRED."
  (when-let* ((node-at-point (treesit-node-at (point) 'clojure t)))
    (treesit-parent-until node-at-point pred t)))

(defun clojure-ts--search-list-form-at-point (sym-regex &optional include-anon-fn-lit)
  "Return the list node at point which first symbol matches SYM-REGEX.

If INCLUDE-ANON-FN-LIT is non-nil, this function may also return a
functional literal node."
  (clojure-ts--parent-until
   (lambda (node)
     (clojure-ts--list-node-sym-match-p node sym-regex include-anon-fn-lit))))

(defun clojure-ts--threading-sexp-node ()
  "Return list node at point which is a threading expression."
  (clojure-ts--search-list-form-at-point (rx bol (* "some") "->" (* ">") eol) t))

(defun clojure-ts--delete-and-extract-sexp ()
  "Delete the surrounding sexp and return it."
  (let* ((sexp-node (treesit-thing-at-point 'sexp 'nested))
         (result (treesit-node-text sexp-node)))
    (delete-region (treesit-node-start sexp-node)
                   (treesit-node-end sexp-node))
    result))

(defun clojure-ts--ensure-parens-around-function-name ()
  "Insert parens around function name if necessary."
  (unless (string= (treesit-node-text (treesit-node-at (point))) "(")
    (insert-parentheses 1)
    (backward-up-list)))

(defun clojure-ts--multiline-sexp-p ()
  "Return TRUE if s-expression at point is multiline."
  (let ((sexp (treesit-thing-at-point 'sexp 'nested)))
    (not (= (line-number-at-pos (treesit-node-start sexp))
            (line-number-at-pos (treesit-node-end sexp))))))

(defun clojure-ts--unwind-thread-first ()
  "Unwind a thread first macro once."
  (let* ((threading-sexp (clojure-ts--threading-sexp-node))
         (first-child-start (thread-first threading-sexp
                                          (treesit-node-child 0 t)
                                          (treesit-node-start)
                                          (copy-marker))))
    (save-excursion
      (goto-char first-child-start)
      (treesit-beginning-of-thing 'sexp -1)
      (let ((contents (clojure-ts--delete-and-extract-sexp)))
        (when (looking-at-p " *\n")
          (join-line 'following))
        (just-one-space)
        (goto-char first-child-start)
        (treesit-beginning-of-thing 'sexp -1)
        (let ((multiline-p (clojure-ts--multiline-sexp-p)))
          (clojure-ts--ensure-parens-around-function-name)
          (down-list)
          (forward-sexp)
          (cond
           ((and multiline-p (looking-at-p " *\n"))
            (insert "\n" contents))
           (multiline-p (insert " " contents "\n"))
           (t (insert " " contents))))))))

(defun clojure-ts--unwind-thread-last ()
  "Unwind a thread last macro once."
  (let* ((threading-sexp (clojure-ts--threading-sexp-node))
         (first-child-start (thread-first threading-sexp
                                          (treesit-node-child 0 t)
                                          (treesit-node-start)
                                          (copy-marker))))
    (save-excursion
      (goto-char first-child-start)
      (treesit-beginning-of-thing 'sexp -1)
      (let ((contents (clojure-ts--delete-and-extract-sexp)))
        (when (looking-at-p " *\n")
          (join-line 'following))
        (just-one-space)
        (goto-char first-child-start)
        (treesit-beginning-of-thing 'sexp -1)
        (let ((multiline-p (clojure-ts--multiline-sexp-p)))
          (clojure-ts--ensure-parens-around-function-name)
          (forward-list)
          (down-list -1)
          (when multiline-p
            (insert "\n"))
          (insert " " contents))))))

(defun clojure-ts--node-threading-p (node)
  "Return non-nil if NODE is a threading macro s-expression."
  (and (or (clojure-ts--list-node-p node)
           (clojure-ts--anon-fn-node-p node))
       (let ((first-child (treesit-node-child node 0 t)))
         (clojure-ts--symbol-matches-p clojure-ts--threading-macro first-child))))

(defun clojure-ts--skip-first-child (parent)
  "Move point to the beginning of the first child of the PARENT node."
  (thread-first parent
                (treesit-node-child 1 t)
                (treesit-node-start)
                (goto-char)))

(defun clojure-ts--nothing-more-to-unwind ()
  "Return TRUE if threading expression at point has only one argument."
  (let ((threading-sexp (clojure-ts--threading-sexp-node)))
    (save-excursion
      (clojure-ts--skip-first-child threading-sexp)
      (not (treesit-end-of-thing 'sexp 2 'restricted)))))

(defun clojure-ts--raise-sexp ()
  "Raise current sexp one level higher up the tree.

The built-in `raise-sexp' function doesn't work well with a few Clojure
nodes (function literals, expressions with metadata etc.), it loses some
parenthesis."
  (when-let* ((sexp-node (treesit-thing-at (point) 'sexp))
              (beg (thread-first sexp-node
                                 (clojure-ts--node-start-skip-metadata)
                                 (copy-marker)))
              (end (thread-first sexp-node
                                 (treesit-node-end)
                                 (copy-marker))))
    (when-let* ((parent (treesit-node-parent sexp-node))
                ((not (string= (treesit-node-type parent) "source")))
                (parent-beg (thread-first parent
                                          (clojure-ts--node-start-skip-metadata)
                                          (copy-marker)))
                (parent-end (thread-first parent
                                          (treesit-node-end)
                                          (copy-marker))))
      (save-excursion
        (delete-region parent-beg beg)
        (delete-region end parent-end)))))

(defun clojure-ts--pop-out-of-threading ()
  "Raise a sexp up a level to unwind a threading form."
  (let* ((threading-sexp (clojure-ts--threading-sexp-node))
         (beg (thread-first threading-sexp
                            (treesit-node-child 0 t)
                            (treesit-node-start))))
    (save-excursion
      (clojure-ts--skip-first-child threading-sexp)
      (delete-region beg (point))
      ;; `raise-sexp' doesn't work properly for function literals (it loses one
      ;; of the parenthesis).  Seems like an Emacs' bug.
      (backward-up-list)
      (delete-pair))))

(defun clojure-ts--fix-sexp-whitespace ()
  "Fix whitespace after unwinding a threading form."
  (save-excursion
    (let ((beg (point)))
      (treesit-end-of-thing 'sexp)
      (indent-region beg (point))
      (delete-trailing-whitespace beg (point)))))

(defun clojure-ts--unwind-sexps-counter ()
  "Return total number of s-expressions of a threading form at point."
  (if-let* ((threading-sexp (clojure-ts--threading-sexp-node)))
      (save-excursion
        (clojure-ts--skip-first-child threading-sexp)
        (let ((n 0))
          (while (treesit-end-of-thing 'sexp 1 'restricted)
            (setq n (1+ n)))
          n))
    (user-error "No threading form to unwind at point")))

(defun clojure-ts-unwind (&optional n)
  "Unwind thread at point or above point by N levels.

With universal argument \\[universal-argument], fully unwinds thread."
  (interactive "P")
  (setq n (cond
           ((equal n '(4)) (clojure-ts--unwind-sexps-counter))
           (n)
           (1)))
  (if-let* ((threading-sexp (clojure-ts--threading-sexp-node))
            (sym (clojure-ts--list-node-sym-text threading-sexp t)))
      (save-excursion
        (let ((beg (thread-first threading-sexp
                                 (treesit-node-start)
                                 (copy-marker)))
              (end (thread-first threading-sexp
                                 (treesit-node-end)
                                 (copy-marker))))
          ;; If it's the last expression, just raise it out of the threading
          ;; macro.
          (if (clojure-ts--nothing-more-to-unwind)
              (progn
                (clojure-ts--pop-out-of-threading)
                (clojure-ts--fix-sexp-whitespace))
            (while (> n 0)
              (cond
               ((string-match-p (rx bol (* "some") "->" eol) sym)
                (clojure-ts--unwind-thread-first))
               ((string-match-p (rx bol (* "some") "->>" eol) sym)
                (clojure-ts--unwind-thread-last)))
              (setq n (1- n))
              ;; After unwinding we check if it is the last expression and maybe
              ;; splice it.
              (when (clojure-ts--nothing-more-to-unwind)
                (clojure-ts--pop-out-of-threading)
                (clojure-ts--fix-sexp-whitespace)
                (setq n 0))))
          (indent-region beg end)
          (delete-trailing-whitespace beg end)))
    (user-error "No threading form to unwind at point")))

(defun clojure-ts-unwind-all ()
  "Fully unwind thread at point or above point."
  (interactive)
  (clojure-ts-unwind '(4)))

(defun clojure-ts--remove-superfluous-parens ()
  "Remove extra parens from a form."
  (when-let* ((node (treesit-thing-at-point 'sexp 'nested))
              ((clojure-ts--list-node-p node))
              ((= 1 (treesit-node-child-count node t))))
    (let ((delete-pair-blink-delay 0))
      (delete-pair))))

(defun clojure-ts--thread-first ()
  "Thread a sexp using ->."
  (save-excursion
    (clojure-ts--skip-first-child (clojure-ts--threading-sexp-node))
    (down-list)
    (treesit-beginning-of-thing 'sexp -1)
    (let ((contents (clojure-ts--delete-and-extract-sexp)))
      (delete-char -1)
      (when (looking-at-p " *\n")
        (join-line 'following))
      (backward-up-list)
      (insert contents "\n")
      (clojure-ts--remove-superfluous-parens))))

(defun clojure-ts--thread-last ()
  "Thread a sexp using ->>."
  (save-excursion
    (clojure-ts--skip-first-child (clojure-ts--threading-sexp-node))
    (treesit-end-of-thing 'sexp)
    (down-list -1)
    (treesit-beginning-of-thing 'sexp)
    (let ((contents (clojure-ts--delete-and-extract-sexp)))
      (delete-char -1)
      (treesit-end-of-thing 'sexp -1 'restricted)
      (when (looking-at-p " *\n")
        (join-line 'following))
      (backward-up-list)
      (insert contents "\n")
      (clojure-ts--remove-superfluous-parens))))

(defun clojure-ts--threadable-p (node)
  "Return non-nil if expression NODE can be threaded.

First argument after threading symbol itself should be a list and it
should have more than one named child."
  (let ((second-child (treesit-node-child node 1 t)))
    (and (clojure-ts--list-node-p second-child)
         (> (treesit-node-child-count second-child t) 1))))

(defun clojure-ts-thread (&optional called-by-user-p)
  "Thread by one more level an existing threading macro.

If CALLED-BY-USER-P is non-nil (which is always TRUE when called
interactively), the function signals a `user-error' if threading form
cannot be found."
  (interactive "p")
  (if-let* ((threading-sexp (clojure-ts--threading-sexp-node))
            ((clojure-ts--threadable-p threading-sexp))
            (sym (clojure-ts--list-node-sym-text threading-sexp t)))
      (let ((beg (thread-first threading-sexp
                               (treesit-node-start)
                               (copy-marker)))
            (end (thread-first threading-sexp
                               (treesit-node-end)
                               (copy-marker))))
        (cond
         ((string-match-p (rx bol (* "some") "->" eol) sym)
          (clojure-ts--thread-first))
         ((string-match-p (rx bol (* "some") "->>" eol) sym)
          (clojure-ts--thread-last)))
        (indent-region beg end)
        (delete-trailing-whitespace beg end)
        t)
    (when called-by-user-p
      (user-error "No threading form at point"))))

(defun clojure-ts--thread-all (first-or-last-thread but-last)
  "Fully thread the form at point.

FIRST-OR-LAST-THREAD is either \"->\" or \"->>\".

When BUT-LAST is non-nil, the last expression is not threaded. Default
value is `clojure-ts-thread-all-but-last.'"
  (if-let* ((list-at-point (treesit-thing-at-point 'list 'nested)))
      (save-excursion
        (goto-char (treesit-node-start list-at-point))
        (insert-parentheses 1)
        (insert first-or-last-thread)
        (while (clojure-ts-thread))
        (when (or but-last clojure-ts-thread-all-but-last)
          (clojure-ts-unwind)))
    (user-error "No list to thread at point")))

(defun clojure-ts-thread-first-all (but-last)
  "Fully thread the form at point using ->.

When BUT-LAST is non-nil, the last expression is not threaded.  Default
value is `clojure-ts-thread-all-but-last'."
  (interactive "P")
  (clojure-ts--thread-all "-> " but-last))

(defun clojure-ts-thread-last-all (but-last)
  "Fully thread the form at point using ->>.

When BUT-LAST is non-nil, the last expression is not threaded.  Default
value is `clojure-ts-thread-all-but-last'."
  (interactive "P")
  (clojure-ts--thread-all "->> " but-last))

(defun clojure-ts-cycle-privacy ()
  "Make a definition at point public or private."
  (interactive)
  (if-let* ((node-at-point (treesit-node-at (point) 'clojure t))
            (defun-node (treesit-parent-until node-at-point 'defun t)))
      (save-excursion
        (goto-char (treesit-node-start defun-node))
        (search-forward-regexp (rx "def" (* letter) (? (group (or "-" " ^:private")))))
        (if (match-string 1)
            (replace-match "" nil nil nil 1)
          (goto-char (match-end 0))
          (insert (if (or clojure-ts-use-metadata-for-defn-privacy
                          (not (string= (match-string 0) "defn")))
                      " ^:private"
                    "-"))))
    (user-error "No defun at point")))

(defun clojure-ts--node-child (node predicate)
  "Return the first child of the NODE that matches the PREDICATE.

PREDICATE can be a symbol representing a thing in
`treesit-thing-settings', or a predicate, like regexp matching node
type, etc.  See `treesit-thing-settings' for more details."
  (thread-last (treesit-node-children node t)
               (seq-find (lambda (child)
                           (treesit-node-match-p child predicate t)))))

(defun clojure-ts--node-start-skip-metadata (node)
  "Return NODE start position optionally skipping metadata."
  (if (clojure-ts--metadata-node-p (treesit-node-child node 0 t))
      (treesit-node-start (treesit-node-child node 1))
    (treesit-node-start node)))

(defun clojure-ts--add-arity-internal (fn-node)
  "Add an arity to a function defined by FN-NODE."
  (let* ((first-coll (clojure-ts--node-child fn-node (rx bol (or "vec_lit" "list_lit") eol)))
         (coll-start (treesit-node-start first-coll))
         (line-parent (thread-first fn-node
                                    (clojure-ts--first-value-child)
                                    (treesit-node-start)
                                    (line-number-at-pos)))
         (line-args (line-number-at-pos coll-start))
         (same-line-p (= line-parent line-args))
         (single-arity-p (clojure-ts--vec-node-p first-coll)))
    (goto-char coll-start)
    (when same-line-p
      (newline-and-indent))
    (when single-arity-p
      (insert-pair 2 ?\( ?\))
      (backward-up-list))
    (insert "([])\n")
    ;; Put the point between square brackets.
    (down-list -2)))

(defun clojure-ts--add-arity-defprotocol-internal (fn-node)
  "Add an arity to a defprotocol function defined by FN-NODE."
  (let* ((args-vec (clojure-ts--node-child fn-node (rx bol "vec_lit" eol)))
         (args-vec-start (treesit-node-start args-vec))
         (line-parent (thread-first fn-node
                                    (clojure-ts--node-child-skip-metadata 0)
                                    (treesit-node-start)
                                    (line-number-at-pos)))
         (line-args-vec (line-number-at-pos args-vec-start))
         (same-line-p (= line-parent line-args-vec)))
    (goto-char args-vec-start)
    (insert "[]")
    (if same-line-p
        (insert " ")
      ;; If args vector is not at the same line, respect this and place each new
      ;; vector on a new line.
      (newline-and-indent))
    ;; Put the point between square brackets.
    (down-list -1)))

(defun clojure-ts--add-arity-reify-internal (fn-node)
  "Add an arity to a reify function defined by FN-NODE."
  (let* ((fn-name (clojure-ts--list-node-sym-text fn-node)))
    (goto-char (treesit-node-start fn-node))
    (insert "(" fn-name " [])")
    (newline-and-indent)
    ;; Put the point between sqare brackets.
    (down-list -2)))

(defun clojure-ts--letfn-defn-p (node)
  "Return non-nil if NODE is a function definition in a letfn form."
  (when-let* ((parent (treesit-node-parent node)))
    (and (clojure-ts--list-node-p node)
         (clojure-ts--vec-node-p parent)
         (let ((grandparent (treesit-node-parent parent)))
           (string= (clojure-ts--list-node-sym-text grandparent)
                    "letfn")))))

(defun clojure-ts--proxy-defn-p (node)
  "Return non-nil if NODE is a function definition in a proxy form."
  (when-let* ((parent (treesit-node-parent node)))
    (and (clojure-ts--list-node-p node)
         (string= (clojure-ts--list-node-sym-text parent) "proxy"))))

(defun clojure-ts--defprotocol-defn-p (node)
  "Return non-nil if NODE is a function definition in a defprotocol form."
  (when-let* ((parent (treesit-node-parent node)))
    (and (clojure-ts--list-node-p node)
         (string= (clojure-ts--list-node-sym-text parent) "defprotocol"))))

(defun clojure-ts--reify-defn-p (node)
  "Return non-nil if NODE is a function definition in a reify form."
  (when-let* ((parent (treesit-node-parent node)))
    (and (clojure-ts--list-node-p node)
         (string= (clojure-ts--list-node-sym-text parent) "reify"))))

(defun clojure-ts--extend-protocol-defn-p (node)
  "Return non-nil if NODE is a function definition in an extend-protocol form."
  (when-let* ((parent (treesit-node-parent node)))
    (and (clojure-ts--list-node-p node)
         (string= (clojure-ts--list-node-sym-text parent) "extend-protocol"))))

(defun clojure-ts-add-arity ()
  "Add an arity to a function or macro."
  (interactive)
  (if-let* ((sym-regex (rx bol
                           (or "defn"
                               "letfn"
                               "fn"
                               "defmacro"
                               "defmethod"
                               "defprotocol"
                               "extend-protocol"
                               "reify"
                               "proxy")
                           eol))
            (parent-def-node (clojure-ts--search-list-form-at-point sym-regex))
            (parent-def-sym (clojure-ts--list-node-sym-text parent-def-node))
            (fn-node (cond
                      ((string= parent-def-sym "letfn")
                       (clojure-ts--parent-until #'clojure-ts--letfn-defn-p))
                      ((string= parent-def-sym "proxy")
                       (clojure-ts--parent-until #'clojure-ts--proxy-defn-p))
                      ((string= parent-def-sym "defprotocol")
                       (clojure-ts--parent-until #'clojure-ts--defprotocol-defn-p))
                      ((string= parent-def-sym "reify")
                       (clojure-ts--parent-until #'clojure-ts--reify-defn-p))
                      ((string= parent-def-sym "extend-protocol")
                       (clojure-ts--parent-until #'clojure-ts--extend-protocol-defn-p))
                      (t parent-def-node))))
      (let ((beg-marker (copy-marker (treesit-node-start parent-def-node)))
            (end-marker (copy-marker (treesit-node-end parent-def-node))))
        (cond
         ((string= parent-def-sym "defprotocol")
          (clojure-ts--add-arity-defprotocol-internal fn-node))
         ((or (string= parent-def-sym "reify")
              (string= parent-def-sym "extend-protocol"))
          (clojure-ts--add-arity-reify-internal fn-node))
         (t (clojure-ts--add-arity-internal fn-node)))
        (indent-region beg-marker end-marker))
    (user-error "No suitable form to add an arity at point")))

(defun clojure-ts-cycle-keyword-string ()
  "Convert the string at point to a keyword, or vice versa."
  (interactive)
  (let ((node (treesit-thing-at-point 'sexp 'nested))
        (pos (point)))
    (cond
     ((clojure-ts--string-node-p node)
      (if (string-match-p " " (treesit-node-text node t))
          (user-error "Cannot convert a string containing spaces to keyword")
        (insert ?: (substring (clojure-ts--delete-and-extract-sexp) 1 -1))))
     ((clojure-ts--keyword-node-p node)
      (insert ?\" (substring (clojure-ts--delete-and-extract-sexp) 1) ?\"))
     (t
      (user-error "No string or keyword at point")))
    (goto-char pos)))

(defun clojure-ts--collection-node-at-point ()
  "Return node at point that represent a collection."
  (when-let* ((node (thread-first (point)
                                  (treesit-node-at 'clojure)
                                  (treesit-parent-until (rx bol
                                                            (or "map_lit"
                                                                "vec_lit"
                                                                "set_lit"
                                                                "list_lit"
                                                                "quoting_lit")
                                                            eol)))))
    (cond
     ;; If node is a list, check if it's quoted.
     ((string= (treesit-node-type node) "list_lit")
      (if-let* ((parent (treesit-node-parent node))
                ((string= (treesit-node-type parent) "quoting_lit")))
          parent
        node))
     ;; If the point is at the quote character, check if the child node is a
     ;; list.
     ((string= (treesit-node-type node) "quoting_lit")
      (when-let* ((first-child (clojure-ts--node-child-skip-metadata node 0))
                  ((string= (treesit-node-type first-child) "list_lit")))
        node))
     (t node))))

(defun clojure-ts--convert-collection (delim-open &optional prefix)
  "Convert collection at point to another collection type.

The original collection is being unwrapped and wrapped between
DELIM-OPEN and its matching paren.  If PREFIX is non-nil it's inserted
before DELIM-OPEN."
  (if-let* ((coll-node (clojure-ts--collection-node-at-point)))
      (save-excursion
        (goto-char (treesit-node-start coll-node))
        (when (string-match-p (rx (or "set_lit" "quoting_lit"))
                              (treesit-node-type coll-node))
          (delete-char 1))
        (let ((parens-require-spaces nil)
              (delete-pair-blink-delay 0))
          (when prefix
            (insert-char prefix))
          (insert-pair 1 delim-open (matching-paren delim-open))
          (delete-pair 1)))
    (user-error "No collection at point to convert")))

(defun clojure-ts-convert-collection-to-list ()
  "Convert collection at point to list."
  (interactive)
  (clojure-ts--convert-collection ?\())

(defun clojure-ts-convert-collection-to-quoted-list ()
  "Convert collection at point to quoted list."
  (interactive)
  (clojure-ts--convert-collection ?\( ?'))

(defun clojure-ts-convert-collection-to-map ()
  "Convert collection at point to map."
  (interactive)
  (clojure-ts--convert-collection ?{))

(defun clojure-ts-convert-collection-to-vector ()
  "Convert collection at point to vector."
  (interactive)
  (clojure-ts--convert-collection ?\[))

(defun clojure-ts-convert-collection-to-set ()
  "Convert collection at point to set."
  (interactive)
  (clojure-ts--convert-collection ?{ ?#))

(defun clojure-ts-cycle-conditional ()
  "Change a surrounding conditional form to its negated counterpart, or vice versa."
  (interactive)
  (if-let* ((sym-regex (rx bol
                           (or "if" "if-not" "when" "when-not")
                           eol))
            (cond-node (clojure-ts--search-list-form-at-point sym-regex t))
            (cond-sym (clojure-ts--list-node-sym-text cond-node)))
      (let ((beg (treesit-node-start cond-node))
            (end-marker (copy-marker (treesit-node-end cond-node)))
            (new-sym (pcase cond-sym
                       ("if" "if-not")
                       ("if-not" "if")
                       ("when" "when-not")
                       ("when-not" "when"))))
        (save-excursion
          (goto-char (treesit-node-start cond-node))
          (down-list 1)
          (delete-char (length cond-sym))
          (insert new-sym)
          (when (member cond-sym '("if" "if-not"))
            (forward-sexp 2)
            (transpose-sexps 1))
          (indent-region beg end-marker)))
    (user-error "No conditional expression found")))

(defun clojure-ts-cycle-not ()
  "Add or remove a not form around the current form."
  (interactive)
  (if-let* ((list-node (clojure-ts--parent-until (rx bol "list_lit" eol))))
      (let ((beg (treesit-node-start list-node))
            (end-marker (copy-marker (treesit-node-end list-node)))
            (pos (copy-marker (point) t)))
        (goto-char (clojure-ts--node-start-skip-metadata list-node))
        (if-let* ((list-parent (treesit-node-parent list-node))
                  ((clojure-ts--list-node-sym-match-p list-parent (rx bol "not" eol))))
            (clojure-ts--raise-sexp)
          (insert-pair 1 ?\( ?\))
          (insert "not "))
        (indent-region beg end-marker)
        ;; `save-excursion' doesn't work well when point is at the opening
        ;; paren.
        (goto-char pos))
    (user-error "Must be invoked inside a list")))

(defvar clojure-ts-refactor-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-t" #'clojure-ts-thread)
    (keymap-set map "t" #'clojure-ts-thread)
    (keymap-set map "C-u" #'clojure-ts-unwind)
    (keymap-set map "u" #'clojure-ts-unwind)
    (keymap-set map "C-f" #'clojure-ts-thread-first-all)
    (keymap-set map "f" #'clojure-ts-thread-first-all)
    (keymap-set map "C-l" #'clojure-ts-thread-last-all)
    (keymap-set map "l" #'clojure-ts-thread-last-all)
    (keymap-set map "C-p" #'clojure-ts-cycle-privacy)
    (keymap-set map "p" #'clojure-ts-cycle-privacy)
    (keymap-set map "C-(" #'clojure-ts-convert-collection-to-list)
    (keymap-set map "(" #'clojure-ts-convert-collection-to-list)
    (keymap-set map "C-'" #'clojure-ts-convert-collection-to-quoted-list)
    (keymap-set map "'" #'clojure-ts-convert-collection-to-quoted-list)
    (keymap-set map "C-{" #'clojure-ts-convert-collection-to-map)
    (keymap-set map "{" #'clojure-ts-convert-collection-to-map)
    (keymap-set map "C-[" #'clojure-ts-convert-collection-to-vector)
    (keymap-set map "[" #'clojure-ts-convert-collection-to-vector)
    (keymap-set map "C-#" #'clojure-ts-convert-collection-to-set)
    (keymap-set map "#" #'clojure-ts-convert-collection-to-set)
    (keymap-set map "C-c" #'clojure-ts-cycle-conditional)
    (keymap-set map "c" #'clojure-ts-cycle-conditional)
    (keymap-set map "C-o" #'clojure-ts-cycle-not)
    (keymap-set map "o" #'clojure-ts-cycle-not)
    (keymap-set map "C-a" #'clojure-ts-add-arity)
    (keymap-set map "a" #'clojure-ts-add-arity)
    map)
  "Keymap for `clojure-ts-mode' refactoring commands.")

(defvar clojure-ts-mode-map
  (let ((map (make-sparse-keymap)))
    ;;(set-keymap-parent map clojure-mode-map)
    (keymap-set map "C-:" #'clojure-ts-cycle-keyword-string)
    (keymap-set map "C-c SPC" #'clojure-ts-align)
    (keymap-set map clojure-ts-refactor-map-prefix clojure-ts-refactor-map)
    (easy-menu-define clojure-ts-mode-menu map "Clojure[TS] Mode Menu"
      '("Clojure"
        ["Toggle between string & keyword" clojure-ts-cycle-keyword-string]
        ["Align expression" clojure-ts-align]
        ["Cycle privacy" clojure-ts-cycle-privacy]
        ["Cycle conditional" clojure-ts-cycle-conditional]
        ["Cycle not" clojure-ts-cycle-not]
        ["Add function/macro arity" clojure-ts-add-arity]
        ("Convert collection"
         ["Convert to list" clojure-ts-convert-collection-to-list]
         ["Convert to quoted list" clojure-ts-convert-collection-to-quoted-list]
         ["Convert to map" clojure-ts-convert-collection-to-map]
         ["Convert to vector" clojure-ts-convert-collection-to-vector]
         ["Convert to set" clojure-ts-convert-collection-to-set])
        ("Refactor -> and ->>"
         ["Thread once more" clojure-ts-thread]
         ["Fully thread a form with ->" clojure-ts-thread-first-all]
         ["Fully thread a form with ->>" clojure-ts-thread-last-all]
         "--"
         ["Unwind once" clojure-ts-unwind]
         ["Fully unwind a threading macro" clojure-ts-unwind-all])
        ["Version" clojure-mode-display-version]))
    map)
  "Keymap for `clojure-ts-mode'.")

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
             "unstable-20250526")
    (markdown-inline "https://github.com/MDeiml/tree-sitter-markdown"
                     "v0.4.1"
                     "tree-sitter-markdown-inline/src")
    (regex "https://github.com/tree-sitter/tree-sitter-regex"
           "v0.24.3"))
  "Intended to be used as the value for `treesit-language-source-alist'.")

;; TODO: Eventually this should be replaced with `treesit-query-valid-p'
(defun clojure-ts--query-valid-p (query)
  "Return non-nil if QUERY is valid in Clojure, nil otherwise."
  (ignore-errors
    (treesit-query-compile 'clojure query t)
    t))

(defun clojure-ts--clojure-grammar-outdated-p ()
  "Return TRUE if currently installed grammar is outdated.

This function checks if `clojure-ts-mode' is compatible with the
currently installed grammar.  The simplest way to do this is to validate
a query that is valid in a previous grammar version but invalid in the
required version."
  (clojure-ts--query-valid-p '((sym_lit (meta_lit)))))

(defun clojure-ts--ensure-grammars ()
  "Install required language grammars if not already available."
  (when clojure-ts-ensure-grammars
    (dolist (recipe clojure-ts-grammar-recipes)
      (let ((grammar (car recipe)))
        (when (or (not (treesit-language-available-p grammar nil))
                  ;; If Clojure grammar is available, but outdated, re-install
                  ;; it.
                  (and (equal grammar 'clojure)
                       (clojure-ts--clojure-grammar-outdated-p)))
          (message "Installing %s Tree-sitter grammar" grammar)
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
      (message "Installing %s Tree-sitter grammar" grammar)
      (let ((treesit-language-source-alist clojure-ts-grammar-recipes))
        (treesit-install-language-grammar grammar)))))

(defun clojure-ts--harvest-treesit-configs (mode)
  "Harvest tree-sitter configs from MODE.
Return a plist with the following keys and value:

    :font-lock (from `treesit-font-lock-settings')
    :simple-indent (from `treesit-simple-indent-rules')"
  (with-temp-buffer
    (funcall mode)
    (list :font-lock treesit-font-lock-settings
          :simple-indent treesit-simple-indent-rules)))

(defun clojure-ts--add-config-for-mode (mode)
  "Add configurations for MODE to current buffer.

Configuration includes font-lock and indent.  For font-lock rules, use
the same features enabled in MODE."
  (let ((configs (clojure-ts--harvest-treesit-configs mode)))
    (setq treesit-font-lock-settings
          (append treesit-font-lock-settings
                  (plist-get configs :font-lock)))
    ;; FIXME: This works a bit aggressively.  `indent-region' always tries to
    ;; use rules for embedded parser.  Without it users can format embedded code
    ;; in an arbitrary way.
    ;;
    ;; (setq treesit-simple-indent-rules
    ;;       (append treesit-simple-indent-rules
    ;;               (plist-get configs :simple-indent)))
    ))

(defun clojure-ts-mode-variables (&optional markdown-available regex-available)
  "Initialize buffer-local variables for `clojure-ts-mode'.

See `clojure-ts--font-lock-settings' for usage of MARKDOWN-AVAILABLE and
REGEX-AVAILABLE."
  (setq-local indent-tabs-mode nil)
  (setq-local comment-add 1)
  (setq-local comment-start ";")
  (when (equal clojure-ts-outline-variant 'comments)
    ;; NOTE: If `imenu' option is selected for `clojure-ts-outline-variant', all
    ;; necessary variables will be set automatically by
    ;; `treesit-major-mode-setup'.
    (setq-local treesit-outline-predicate #'clojure-ts--outline-predicate
                outline-search-function #'treesit-outline-search
                outline-level #'clojure-ts--outline-level))

  (setq-local treesit-font-lock-settings
              (clojure-ts--font-lock-settings markdown-available regex-available))
  (setq-local treesit-font-lock-feature-list
              '((comment definition variable)
                (keyword string char symbol builtin type)
                (constant number quote metadata doc regex)
                (bracket deref function tagged-literals)))

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
                                  (treesit-ready-p 'markdown-inline t)))
        (use-regex (and clojure-ts-use-regex-parser
                        (treesit-ready-p 'regex t))))
    (setq-local treesit-range-settings
                (clojure-ts--treesit-range-settings use-markdown-inline
                                                    use-regex))

    (when (treesit-ready-p 'clojure)
      (treesit-parser-create 'clojure)
      (clojure-ts-mode-variables use-markdown-inline use-regex)

      (when clojure-ts--debug
        (setq-local treesit--indent-verbose t)
        (when (eq clojure-ts--debug 'font-lock)
          (setq-local treesit--font-lock-verbose t))
        (treesit-inspect-mode))

      (treesit-major-mode-setup)

      ;; We should assign this after calling `treesit-major-mode-setup',
      ;; otherwise it will be owerwritten.
      (when clojure-ts-align-forms-automatically
        (setq-local indent-region-function #'clojure-ts-indent-region))

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

\\{clojure-ts-clojurescript-mode-map}"
  (when (and clojure-ts-clojurescript-use-js-parser
             (treesit-ready-p 'javascript t))
    (setq-local treesit-range-settings
                (append treesit-range-settings
                        (treesit-range-rules
                         :embed 'javascript
                         :host 'clojure
                         :local t
                         '(((list_lit (sym_lit) @_sym-name
                                      :anchor (str_lit (str_content) @capture))
                            (:equal @_sym-name "js*"))))))
    (clojure-ts--add-config-for-mode 'js-ts-mode)
    (treesit-major-mode-setup)))

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

\\{clojure-ts-jank-mode-map}"
  (when (and clojure-ts-jank-use-cpp-parser
             (treesit-ready-p 'cpp t))
    (setq-local treesit-range-settings
                (append treesit-range-settings
                        (treesit-range-rules
                         :embed 'cpp
                         :host 'clojure
                         :local t
                         '(((list_lit (sym_lit) @_sym-name
                                      :anchor (str_lit (str_content) @capture))
                            (:equal @_sym-name "native/raw"))))))
    (clojure-ts--add-config-for-mode 'c++-ts-mode)
    (treesit-major-mode-setup)))

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
  (message "Clojure TS Mode will not be activated as Tree-sitter support is missing."))

(defvar clojure-ts--find-ns-query
  (treesit-query-compile
   'clojure
   '(((source (list_lit
               :anchor [(comment) (meta_lit) (old_meta_lit)] :*
               :anchor (sym_lit name: (sym_name) @ns)
               :anchor [(comment) (meta_lit) (old_meta_lit)] :*
               :anchor (sym_lit name: (sym_name) @ns-name)))
      (:equal @ns "ns"))
     ((source (list_lit
               :anchor [(comment) (meta_lit) (old_meta_lit)] :*
               :anchor (sym_lit name: (sym_name) @in-ns)
               :anchor [(comment) (meta_lit) (old_meta_lit)] :*
               :anchor (quoting_lit
                        :anchor (sym_lit name: (sym_name) @ns-name))))
      (:equal @in-ns "in-ns")))))

(defun clojure-ts-find-ns ()
  "Return the name of the current namespace."
  (let ((nodes (treesit-query-capture 'clojure clojure-ts--find-ns-query)))
    (treesit-node-text (cdr (assoc 'ns-name nodes)) t)))

(provide 'clojure-ts-mode)

;;; clojure-ts-mode.el ends here
