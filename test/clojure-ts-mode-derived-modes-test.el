;;; clojure-ts-mode-derived-modes-test.el --- Clojure TS Mode: derived modes test suite  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov

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

;; Tests for the modes derived from `clojure-ts-mode', and for the way they
;; borrow the settings of the modes for their embedded languages.

;;; Code:

(require 'clojure-ts-mode)
(require 'buttercup)

(defvar clojure-ts-test--harvested-install-setting nil
  "The value of `treesit-auto-install-grammar' the harvested mode saw.")

(define-derived-mode clojure-ts-test-harvested-mode prog-mode "Harvested"
  "Stand-in for `js-ts-mode' on Emacs 31 without the jsdoc grammar.
It sets up its font-lock rules, one of them for a grammar that isn't
installed, and then fails to load that grammar."
  (setq clojure-ts-test--harvested-install-setting
        (bound-and-true-p treesit-auto-install-grammar))
  (setq-local treesit-font-lock-settings
              (list (list '((sym_lit) @font-lock-keyword-face)
                          t 'symbol nil nil 'clojure)
                    (list '((tag) @font-lock-doc-face)
                          t 'tag nil nil 'clojure-ts-test-missing)))
  (signal 'treesit-load-language-error '(not-found "clojure-ts-test-missing")))

(describe "clojure-ts--harvest-treesit-configs"
  (before-all
    (unless (treesit-language-available-p 'clojure)
      (signal 'buttercup-pending "tree-sitter Clojure grammar is not available")))

  (it "doesn't let the mode install grammars"
    (clojure-ts--harvest-treesit-configs 'clojure-ts-test-harvested-mode)
    (expect clojure-ts-test--harvested-install-setting :to-be 'never))

  (it "keeps the settings of a mode that fails to load a grammar"
    (expect (plist-get (clojure-ts--harvest-treesit-configs
                        'clojure-ts-test-harvested-mode)
                       :font-lock)
            :not :to-be nil))

  (it "leaves out the font-lock rules for missing grammars"
    (assume (fboundp 'treesit-font-lock-setting-language)
            "Font-lock rules carry their language only on Emacs 31+")
    (expect (mapcar #'treesit-font-lock-setting-language
                    (plist-get (clojure-ts--harvest-treesit-configs
                                'clojure-ts-test-harvested-mode)
                               :font-lock))
            :to-equal '(clojure))))

;;; clojure-ts-mode-derived-modes-test.el ends here
