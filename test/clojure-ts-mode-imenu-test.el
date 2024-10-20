;;; clojure-ts-mode-util-test.el --- Clojure TS Mode: util test suite  -*- lexical-binding: t; -*-

;; Copyright © 2022-2024 Danny Freeman

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
(require 'buttercup)
(require 'imenu)


(describe "clojure-ts-mode imenu integration"
  (it "should index def with meta data"
    (with-clojure-ts-buffer "^{:foo 1}(def a 1)"
      (expect (imenu--in-alist "a" (imenu--make-index-alist))
              :not :to-be nil)))

  (it "should index defn with meta data"
    (with-clojure-ts-buffer "^{:foo 1}(defn a [])"
      (expect (imenu--in-alist "a" (imenu--make-index-alist))
              :not :to-be nil))))
