;;; clojure-ts-mode.el --- Major mode for Clojure code -*- lexical-binding: t; -*-

;; Copyright © 2022-2023 Danny Freeman
;;
;; Authors: Danny Freeman <danny@dfreeman.email>
;; Maintainer: Danny Freeman <danny@dfreeman.email>
;; URL: http://github.com/clojure-emacs/clojure-ts-mode
;; Keywords: languages clojure clojurescript lisp
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock, indentation, and navigation for the
;; Clojure programming language (http://clojure.org).

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

(defconst clojure-ts-mode-version
  (eval-when-compile
    (lm-version (or load-file-name buffer-file-name)))
  "The current version of `clojure-ts-mode'.")

(defvar clojure-ts--debug nil
  "Enables debugging messages, shows current node in mode-line.
Only intended for use at development time.")

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
             '("do"
               "if"
               "let*"
               "var"
               "fn"
               "fn*"
               "loop*"
               "recur"
               "throw"
               "try"
               "catch"
               "finally"
               "set!"
               "new"
               "."
               "monitor-enter"
               "monitor-exit"
               "quote"

               "->"
               "->>"
               ".."
               "amap"
               "and"
               "areduce"
               "as->"
               "assert"
               "binding"
               "bound-fn"
               "case"
               "comment"
               "cond"
               "cond->"
               "cond->>"
               "condp"
               "declare"
               "delay"
               "doall"
               "dorun"
               "doseq"
               "dosync"
               "dotimes"
               "doto"
               "extend-protocol"
               "extend-type"
               "for"
               "future"
               "gen-class"
               "gen-interface"
               "if-let"
               "if-not"
               "if-some"
               "import"
               "in-ns"
               "io!"
               "lazy-cat"
               "lazy-seq"
               "let"
               "letfn"
               "locking"
               "loop"
               "memfn"
               "ns"
               "or"
               "proxy"
               "proxy-super"
               "pvalues"
               "refer-clojure"
               "reify"
               "some->"
               "some->>"
               "sync"
               "time"
               "vswap!"
               "when"
               "when-first"
               "when-let"
               "when-not"
               "when-some"
               "while"
               "with-bindings"
               "with-in-str"
               "with-loading-context"
               "with-local-vars"
               "with-open"
               "with-out-str"
               "with-precision"
               "with-redefs"
               "with-redefs-fn"))
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
  (rx line-start (or "defprotocol"
                     "defmulti"
                     "deftype"
                     "defrecord"
                     "definterface"
                     "defmethod"
                     "defstruct")
      line-end))

(defvar clojure-ts--font-lock-settings
  (treesit-font-lock-rules
   :feature 'string
   :language 'clojure
   '((str_lit) @font-lock-string-face
     (regex_lit) @font-lock-string-face)

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

   :feature 'symbol
   :language 'clojure
   '((sym_ns) @font-lock-type-face)

   ;; How does this work for defns nested in other forms, not at the top level?
   ;; Should I match against the source node to only hit the top level? Can that be expressed?
   ;; What about valid usages like `(let [closed 1] (defn +closed [n] (+ n closed)))'??
   ;; No wonder the tree-sitter-clojure grammar only touches syntax, and not semantics
   :feature 'definition ;; defn and defn like macros
   :language 'clojure
   `(((list_lit :anchor (sym_lit (sym_name) @font-lock-keyword-face)
                :anchor (sym_lit (sym_name) @font-lock-function-name-face))
      (:match ,clojure-ts--definition-keyword-regexp
              @font-lock-keyword-face))
     ((anon_fn_lit
       marker: "#" @font-lock-property-face)))

   :feature 'variable ;; def, defonce
   :language 'clojure
   `(((list_lit :anchor (sym_lit (sym_name) @font-lock-keyword-face)
                :anchor (sym_lit (sym_name) @font-lock-variable-name-face))
      (:match ,clojure-ts--variable-keyword-regexp @font-lock-keyword-face)))

   :feature 'type ;; deftype, defmulti, defprotocol, etc
   :language 'clojure
   `(((list_lit :anchor (sym_lit (sym_name) @font-lock-keyword-face)
                :anchor (sym_lit (sym_name) @font-lock-type-face))
      (:match ,clojure-ts--type-keyword-regexp @font-lock-keyword-face)))

   :feature 'metadata
   :language 'clojure
   :override t
   `((meta_lit marker: "^" @font-lock-property-face)
     (meta_lit value: (kwd_lit) @font-lock-property-face) ;; metadata
     (meta_lit value: (sym_lit (sym_name) @font-lock-type-face)) ;; typehint
     (old_meta_lit marker: "#^" @font-lock-property-face)
     (old_meta_lit value: (kwd_lit) @font-lock-property-face) ;; metadata
     (old_meta_lit value: (sym_lit (sym_name) @font-lock-type-face))) ;; typehint

   :feature 'tagged-literals
   :language 'clojure
   :override t
   '((tagged_or_ctor_lit marker: "#" @font-lock-preprocessor-face
                         tag: (sym_lit) @font-lock-preprocessor-face))

   ;; TODO, also account for `def'
   ;; Figure out how to highlight symbols in docstrings.
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
   '((comment) @font-lock-comment-face
     (dis_expr
      marker: "#_" @font-lock-comment-delimiter-face
      value: _ @font-lock-comment-face)
     ((list_lit :anchor (sym_lit (sym_name) @font-lock-comment-delimiter-face))
      (:match "^comment$" @font-lock-comment-delimiter-face)))

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

(defvar clojure-ts-mode-map
  (let ((map (make-sparse-keymap)))
    ;(set-keymap-parent map clojure-mode-map)
    map))

;;;###autolaod
(add-to-list
 'treesit-language-source-alist
 '(clojure "https://github.com/sogaiu/tree-sitter-clojure.git"))

(defun clojure-ts-mode-display-version ()
  "Display the current `clojure-mode-version' in the minibuffer."
  (interactive)
  (message "clojure-ts-mode (version %s)" clojure-ts-mode-version))

;;;###autoload
(define-derived-mode clojure-ts-mode prog-mode "Clojure[TS]"
  "Major mode for editing Clojure code.
Requires Emacs 29 and libtree-sitter-clojure.so available somewhere in
`treesit-extra-load-path'.

\\{clojure-ts-mode-map}"
  (unless (treesit-language-available-p 'clojure nil)
    (treesit-install-language-grammar 'clojure))
  (setq-local comment-start ";")
  (when (treesit-ready-p 'clojure)
    (treesit-parser-create 'clojure)
    (setq-local treesit-font-lock-settings clojure-ts--font-lock-settings
                treesit-defun-prefer-top-level t
                treesit-defun-tactic 'top-level
                treesit-defun-type-regexp (rx (or "list_lit" "vec_lit" "map_lit"))
                treesit-simple-indent-rules clojure-ts--fixed-indent-rules
                treesit-defun-name-function #'clojure-ts--standard-definition-node-name
                treesit-simple-imenu-settings clojure-ts--imenu-settings
                treesit-font-lock-feature-list
                '((comment string char number)
                  (keyword constant symbol bracket builtin)
                  (deref quote metadata definition variable type doc regex tagged-literals)))
    (when clojure-ts--debug
      (setq-local treesit--indent-verbose t
                  treesit--font-lock-verbose t)
      (treesit-inspect-mode))
    (treesit-major-mode-setup)))

;; Redirect clojure-mode to clojure-ts-mode if clojure-mode is present
(if (require 'clojure-mode nil 'noerror)
    (progn
      (add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode))
      (add-to-list 'major-mode-remap-alist '(clojurescript-mode . clojure-ts-mode))
      (add-to-list 'major-mode-remap-alist '(clojurec-mode . clojure-ts-mode)))
  ;; clojure-mode is not present
  (add-to-list 'auto-mode-alist
               '("\\.\\(clj\\|cljd\\|cljc\\|cljs\\|dtm\\|edn\\)\\'" . clojure-ts-mode))
  ;; boot build scripts are Clojure source files
  (add-to-list 'auto-mode-alist '("\\(?:build\\|profile\\)\\.boot\\'" . clojure-ts-mode))
  ;; babashka scripts are Clojure source files
  (add-to-list 'interpreter-mode-alist '("bb" . clojure-ts-mode))
  ;; nbb scripts are ClojureScript source files
  (add-to-list 'interpreter-mode-alist '("nbb" . clojure-ts-mode)))

(provide 'clojure-ts-mode)

;;; clojure-ts-mode.el ends here
