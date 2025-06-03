;;; clojure-ts-mode-completion.el --- clojure-ts-mode: completion tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Roman Rudakov

;; Author: Roman Rudakov <roman.rudakov@adgoji.com>
;; Keywords:

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

;; Completion is a unique `clojure-ts-mode' feature.

;;; Code:

(require 'clojure-ts-mode)
(require 'buttercup)
(require 'test-helper "test/test-helper")

(describe "clojure-ts-complete-at-point-function"
  ;; NOTE: This function returns unfiltered candidates, so prefix doesn't really
  ;; matter here.

  (it "should complete global vars"
    (with-clojure-ts-buffer-point "
(def foo :first)

(def bar :second)

(defn baz
  []
  (println foo bar))

b|"
      (expect (nth 2 (clojure-ts-completion-at-point-function))
              :to-equal '(("foo" . defun-candidate)
                          ("bar" . defun-candidate)
                          ("baz" . defun-candidate)))))

  (it "should complete function arguments"
    (with-clojure-ts-buffer-point "
(def foo :first)

(def bar :second)

(defn baz
  [username]
  (println u|))"
      (expect (nth 2 (clojure-ts-completion-at-point-function))
              :to-equal '(("foo" . defun-candidate)
                          ("bar" . defun-candidate)
                          ("baz" . defun-candidate)
                          ("username" . local-candidate)))))

  (it "should not complete function arguments outside of function"
    (with-clojure-ts-buffer-point "
(def foo :first)

(def bar :second)

(defn baz
  [username]
  (println bar))

u|"
      (expect (nth 2 (clojure-ts-completion-at-point-function))
              :to-equal '(("foo" . defun-candidate)
                          ("bar" . defun-candidate)
                          ("baz" . defun-candidate)))))

  (it "should complete destructured function arguments"
    (with-clojure-ts-buffer-point "
(defn baz
  [{:keys [username]}]
  (println u|))"
      (expect (nth 2 (clojure-ts-completion-at-point-function))
              :to-equal '(("baz" . defun-candidate)
                          ("username" . local-candidate))))

    (with-clojure-ts-buffer-point "
(defn baz
  [{:strs [username]}]
  (println u|))"
      (expect (nth 2 (clojure-ts-completion-at-point-function))
              :to-equal '(("baz" . defun-candidate)
                          ("username" . local-candidate))))

    (with-clojure-ts-buffer-point "
(defn baz
  [{:syms [username]}]
  (println u|))"
      (expect (nth 2 (clojure-ts-completion-at-point-function))
              :to-equal '(("baz" . defun-candidate)
                          ("username" . local-candidate))))

    (with-clojure-ts-buffer-point "
(defn baz
  [{username :name}]
  (println u|))"
      (expect (nth 2 (clojure-ts-completion-at-point-function))
              :to-equal '(("baz" . defun-candidate)
                          ("username" . local-candidate))))

    (with-clojure-ts-buffer-point "
(defn baz
  [[first-name last-name]]
  (println f|))"
      (expect (nth 2 (clojure-ts-completion-at-point-function))
              :to-equal '(("baz" . defun-candidate)
                          ("first-name" . local-candidate)
                          ("last-name" . local-candidate)))))

  (it "should complete vector bindings"
    (with-clojure-ts-buffer-point "
(defn baz
  [first-name]
  (let [last-name \"Doe\"
        address {:street \"Whatever\" :zip-code 2222}
        {:keys [street zip-code]} address]
    a|))"
      (expect (nth 2 (clojure-ts-completion-at-point-function))
              :to-equal '(("baz" . defun-candidate)
                          ("first-name" . local-candidate)
                          ("last-name" . local-candidate)
                          ("address" . local-candidate)
                          ("street" . local-candidate)
                          ("zip-code" . local-candidate)))))

  (it "should not complete called function names"
    (with-clojure-ts-buffer-point "
(defn baz
  [first-name]
  (let [full-name (str first-name \"Doe\")]
    s|))"
      ;; `str' should not be among the candidates.
      (expect (nth 2 (clojure-ts-completion-at-point-function))
              :to-equal '(("baz" . defun-candidate)
                          ("first-name" . local-candidate)
                          ("full-name" . local-candidate))))))

(provide 'clojure-ts-mode-completion)
;;; clojure-ts-mode-completion.el ends here
