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
(require 'test-helper "test/utils/test-helper")

(describe "clojure-ts-mode-version"
  (it "should not be nil"
    (expect clojure-ts-mode-version)))

(describe "clojure-ts-find-ns"
  (it "should find common namespace declarations"
    (with-clojure-ts-buffer "(ns foo)"
                            (expect (clojure-ts-find-ns) :to-equal "foo"))
    (with-clojure-ts-buffer "(ns
    foo)"
                            (expect (clojure-ts-find-ns) :to-equal "foo"))
    (with-clojure-ts-buffer "(ns foo.baz)"
                            (expect (clojure-ts-find-ns) :to-equal "foo.baz"))
    (with-clojure-ts-buffer "(ns ^:bar foo)"
                            (expect (clojure-ts-find-ns) :to-equal "foo"))
    (with-clojure-ts-buffer "(ns ^:bar ^:baz foo)"
                            (expect (clojure-ts-find-ns) :to-equal "foo")))

  (it "should find namespaces with spaces before ns form"
    (with-clojure-ts-buffer "  (ns foo)"
                            (expect (clojure-ts-find-ns) :to-equal "foo")))

  (it "should skip namespaces within any comment forms"
    (with-clojure-ts-buffer "(comment
      (ns foo))"
                            (expect (clojure-ts-find-ns) :to-equal nil))
    (with-clojure-ts-buffer " (ns foo)
     (comment
      (ns bar))"
                            (expect (clojure-ts-find-ns) :to-equal "foo"))
    (with-clojure-ts-buffer " (comment
      (ns foo))
     (ns bar)
    (comment
      (ns baz))"
                            (expect (clojure-ts-find-ns) :to-equal "bar")))

  (it "should find namespace declarations with nested metadata and docstrings"
    (with-clojure-ts-buffer "(ns ^{:bar true} foo)"
                            (expect (clojure-ts-find-ns) :to-equal "foo"))
    (with-clojure-ts-buffer "(ns #^{:bar true} foo)"
                            (expect (clojure-ts-find-ns) :to-equal "foo"))
    (with-clojure-ts-buffer "(ns #^{:fail {}} foo)"
                            (expect (clojure-ts-find-ns) :to-equal "foo"))
    (with-clojure-ts-buffer "(ns ^{:fail2 {}} foo.baz)"
                            (expect (clojure-ts-find-ns) :to-equal "foo.baz"))
    (with-clojure-ts-buffer "(ns ^{} foo)"
                            (expect (clojure-ts-find-ns) :to-equal "foo"))
    (with-clojure-ts-buffer "(ns ^{:skip-wiki true}
      aleph.netty)"
                            (expect (clojure-ts-find-ns) :to-equal "aleph.netty"))
    (with-clojure-ts-buffer "(ns ^{:foo {:bar :baz} :fake (ns in.meta)} foo
  \"docstring
(ns misleading)\")"
                            (expect (clojure-ts-find-ns) :to-equal "foo")))

  (it "should support non-alphanumeric characters"
    (with-clojure-ts-buffer "(ns foo+)"
                            (expect (clojure-ts-find-ns) :to-equal "foo+"))
    (with-clojure-ts-buffer "(ns bar**baz$-_quux)"
                            (expect (clojure-ts-find-ns) :to-equal "bar**baz$-_quux"))
    (with-clojure-ts-buffer "(ns aoc-2019.puzzles.day14)"
                            (expect (clojure-ts-find-ns) :to-equal "aoc-2019.puzzles.day14")))

  (it "should support in-ns forms"
    (with-clojure-ts-buffer "(in-ns 'bar.baz)"
                            (expect (clojure-ts-find-ns) :to-equal "bar.baz")))

  (it "should take the first ns instead of closest unlike clojure-mode"
    (with-clojure-ts-buffer " (ns foo1)

(ns foo2)"
                            (expect (clojure-ts-find-ns) :to-equal "foo1"))
    (with-clojure-ts-buffer-point " (in-ns foo1)
(ns 'foo2)
(in-ns 'foo3)
|
(ns foo4)"
                                  (expect (clojure-ts-find-ns) :to-equal "foo3"))
    (with-clojure-ts-buffer "(ns foo)
(ns-unmap *ns* 'map)
(ns.misleading 1 2 3)"
                            (expect (clojure-ts-find-ns) :to-equal "foo")))

  (it "should skip leading garbage"
    (with-clojure-ts-buffer " (ns foo)"
                            (expect (clojure-ts-find-ns) :to-equal "foo"))
    (with-clojure-ts-buffer "1(ns foo)"
                            (expect (clojure-ts-find-ns) :to-equal "foo"))
    (with-clojure-ts-buffer "1 (ns foo)"
                            (expect (clojure-ts-find-ns) :to-equal "foo"))
    (with-clojure-ts-buffer "1
(ns foo)"
                            (expect (clojure-ts-find-ns) :to-equal "foo"))
    (with-clojure-ts-buffer "[1]
(ns foo)"
                            (expect (clojure-ts-find-ns) :to-equal "foo"))
    (with-clojure-ts-buffer "[1] (ns foo)"
                            (expect (clojure-ts-find-ns) :to-equal "foo"))
    (with-clojure-ts-buffer "[1](ns foo)"
                            (expect (clojure-ts-find-ns) :to-equal "foo"))
    (with-clojure-ts-buffer "(ns)(ns foo)"
                            (expect (clojure-ts-find-ns) :to-equal "foo"))
    (with-clojure-ts-buffer "(ns 'foo)(ns bar)"
                            (expect (clojure-ts-find-ns) :to-equal "bar"))))
