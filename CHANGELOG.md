# Changelog

## main (unreleased)

- Add support for font locking regex literals
    - [c62ac120ff64307debd4bca463ed822fd241a006](https://github.com/clojure-emacs/clojure-ts-mode/commit/c62ac120ff64307debd4bca463ed822fd241a006)
    - Requires installation of the tree-sitter-regex grammar
    - If the grammar is not available, clojure-ts-mode SHOULD fall back to simple uniform font-locking of the entire regex literal.
    - Includes a command `clojure-ts-install-regex-grammar` to aid in installation.

## 0.1.4

- Fix misplaced defcustom form in hastily release 0.1.3 [6cba90c556c7e658b815cdbb9b4243bde3273203](https://github.com/clojure-emacs/clojure-ts-mode/commit/6cba90c556c7e658b815cdbb9b4243bde3273203)

## 0.1.3

- Add custom option for highlighting comment macro body forms as comments. [ae3790adc0fc40ad905b8c30b152122991592a4e](https://github.com/clojure-emacs/clojure-ts-mode/commit/ae3790adc0fc40ad905b8c30b152122991592a4e)
    - Defaults to OFF, highlighting comment body forms like any other expressions.
    - Additionally, does a better job of better detecting comment macros by reducing false positives from forms like (not.clojure.core/comment)

## 0.1.2

- Add a syntax table from clojure-mode. [712dc772fd38111c1e35fe60e4dbe7ac83032bd6](https://github.com/clojure-emacs/clojure-ts-mode/commit/712dc772fd38111c1e35fe60e4dbe7ac83032bd6). 
    - Better support for `thing-at-point` driven functionality.
    - Thank you @jasonjckn for this contribution.
- Add 3 derived major modes [4dc853df16ba09d10dc3a648865e681679c17606](https://github.com/clojure-emacs/clojure-ts-mode/commit/4dc853df16ba09d10dc3a648865e681679c17606)
    - clojurescript-ts-mode
    - clojurec-ts-mode
    - clojure-dart-ts-mode

## 0.1.1

- Fix bug with required emacs version [19b8e4260bfd459ad5771e06afb22c9af0ebc370](https://github.com/clojure-emacs/clojure-ts-mode/commit/19b8e4260bfd459ad5771e06afb22c9af0ebc370)

## 0.1.0

Initial release. Includes:

- Auto install of language grammar
- Font locking (syntax highlighting)
- Fixed style indentation
- imenu support
- which-function support
