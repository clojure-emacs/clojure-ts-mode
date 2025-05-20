;;; clojure-ts-mode-navigation-test.el --- Clojure[TS] Mode: code navigation test suite  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Roman Rudakov

;; Author: Roman Rudakov <rrudakov@fastmail.com>

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

;; Verify that Emacs' built-in navigation commands work as expected.

;;; Code:

(require 'clojure-ts-mode)
(require 'buttercup)
(require 'test-helper "test/test-helper")

(describe "function literals"
  (describe "forward-sexp"
    (when-refactoring-with-point-it "should navigate to a closing paren if point is before an opening paren"
      "#|(-> (.-value (.-target %)))"
      "#(-> (.-value (.-target %)))|"
      (forward-sexp))
    (when-refactoring-with-point-it "should navigate to a closing paren if point is before a # character"
      "|#(-> (.-value (.-target %)))"
      "#(-> (.-value (.-target %)))|"
      (forward-sexp)))

  (describe "backward-up-list"
    (when-refactoring-with-point-it "should navigate to the beginning of a parent expression"
      "#(-> |(.-value (.-target %)))"
      "|#(-> (.-value (.-target %)))"
      (backward-up-list)))

  (describe "raise-sexp"
    (when-refactoring-with-point-it "should keep balanced parenthesis"
      "#(-> |(.-value (.-target %)))"
      "|(.-value (.-target %))"
      (raise-sexp))))

(describe "sets"
  (describe "forward-sexp"
    (when-refactoring-with-point-it "should navigate to a closing paren if point is before an opening paren"
      "#|{1 2 3}"
      "#{1 2 3}|"
      (forward-sexp))
    (when-refactoring-with-point-it "should navigate to a closing paren if point is before a # character"
      "|#{1 2 3}"
      "#{1 2 3}|"
      (forward-sexp)))

  (describe "backward-up-list"
    (when-refactoring-with-point-it "should navigate to the beginning of a parent expression"
      "#{1 |2 3}"
      "|#{1 2 3}"
      (backward-up-list)))

  (describe "raise-sexp"
    (when-refactoring-with-point-it "should not keep unwanted characters"
      "#{1 |2 3}"
      "|2"
      (raise-sexp))))

(describe "nodes with metadata"
  (describe "forward-sexp"
    (when-refactoring-with-point-it "should navigate to a closing paren if point is before an opening paren"
      "^String |[arg]"
      "^String [arg]|"
      (forward-sexp))
    ;; This is not perfect, but with the current grammar we cannot handle it better.
    (when-refactoring-with-point-it "should naigate to a closing paren if point is before metadata"
      "|^String [arg]"
      "^String [arg]|"
      (forward-sexp))))

(provide 'clojure-ts-mode-navigation-test)
;;; clojure-ts-mode-navigation-test.el ends here
