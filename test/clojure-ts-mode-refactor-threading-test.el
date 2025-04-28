;;; clojure-ts-mode-refactor-threading-test.el --- clojure-ts-mode: refactor  threading tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Roman Rudakov

;; Author: Roman Rudakov <rrudakov@fastmail.com>
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

;; The threading refactoring code is adapted from clojure-mode.el.

;;; Code:

(require 'clojure-ts-mode)
(require 'buttercup)
(require 'test-helper "test/test-helper")

(describe "clojure-unwind"

  (when-refactoring-it "should unwind -> one step"
    "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))"

    "(-> (assoc {} :key \"value\")
    (dissoc :lock))"

    (clojure-ts-unwind))

  (when-refactoring-it "should unwind -> completely"
    "(-> {}
    (assoc :key \"value\")
    (dissoc :lock))"

    "(dissoc (assoc {} :key \"value\") :lock)"

    (clojure-ts-unwind)
    (clojure-ts-unwind))

  (when-refactoring-it "should unwind ->> one step"
    "(->> [1 2 3 4 5]
     (filter even?)
     (map square))"

    "(->> (filter even? [1 2 3 4 5])
     (map square))"

    (clojure-ts-unwind))

  (when-refactoring-it "should unwind ->> completely"
    "(->> [1 2 3 4 5]
     (filter even?)
     (map square))"

    "(map square (filter even? [1 2 3 4 5]))"

    (clojure-ts-unwind)
    (clojure-ts-unwind))

  (when-refactoring-it "should unwind N steps with numeric prefix arg"
    "(->> [1 2 3 4 5]
     (filter even?)
     (map square)
     sum)"

    "(->> (map square (filter even? [1 2 3 4 5]))
     sum)"

    (clojure-ts-unwind 2))

  (when-refactoring-it "should unwind completely with universal prefix arg"
    "(->> [1 2 3 4 5]
     (filter even?)
     (map square)
     sum)"

    "(sum (map square (filter even? [1 2 3 4 5])))"

    (clojure-ts-unwind '(4)))

  (when-refactoring-it "should unwind correctly when multiple ->> are present on same line"
    "(->> 1 inc) (->> [1 2 3 4 5]
     (filter even?)
     (map square))"

    "(->> 1 inc) (map square (filter even? [1 2 3 4 5]))"

    (clojure-ts-unwind)
    (clojure-ts-unwind))

  (when-refactoring-it "should unwind with function name"
    "(->> [1 2 3 4 5]
     sum
     square)"

    "(->> (sum [1 2 3 4 5])
     square)"

    (clojure-ts-unwind))

  (when-refactoring-it "should unwind with function name twice"
    "(-> [1 2 3 4 5]
     sum
     square)"

    "(square (sum [1 2 3 4 5]))"

    (clojure-ts-unwind)
    (clojure-ts-unwind))

  (when-refactoring-it "should thread-issue-6-1"
    "(defn plus [a b]
  (-> a (+ b)))"

    "(defn plus [a b]
  (+ a b))"

    (clojure-ts-unwind))

  (when-refactoring-it "should thread-issue-6-2"
    "(defn plus [a b]
  (->> a (+ b)))"

    "(defn plus [a b]
  (+ b a))"

    (clojure-ts-unwind))

  (when-refactoring-it "should unwind some->"
    "(some-> {:a 1}
        (find :b)
        val
        (+ 5))"

    "(some-> (val (find {:a 1} :b))
        (+ 5))"

    (clojure-ts-unwind)
    (clojure-ts-unwind))

  (when-refactoring-it "should unwind some->>"
    "(some->> :b
         (find {:a 1}) val
         (+ 5))"

    "(some->> (val (find {:a 1} :b))
         (+ 5))"

    (clojure-ts-unwind)
    (clojure-ts-unwind)))

(provide 'clojure-ts-mode-refactor-threading-test)
;;; clojure-ts-mode-refactor-threading-test.el ends here
