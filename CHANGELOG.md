# Changelog

## main (unreleased)

- Re-enable treesit-transpose-sexps on Emacs 30 after fixes released by @casouri.
- Pin grammar revision in treesit-language-source-alist
- Make font lock feature list more conforming with recommendations
   - (See treesit-font-lock-level documentation for more information.)
- Highlight docstrings in interface, protocol, and variable definitions
- Add support for semantic indentation (now the default)

## 0.1.5

- Disable treesit-transpose-sexps on Emacs 30 in favor of the default implementation (#17) [623c98292f9207a95169cdeae6f8595c016c6320](https://github.com/clojure-emacs/clojure-ts-mode/commit/623c98292f9207a95169cdeae6f8595c016c6320)
- Implement clojure-ts-find-ns function (mostly as a demonstration). [d630cd63af8022d5a1fee0e7aa05450b6e0fd75e](https://github.com/clojure-emacs/clojure-ts-mode/commit/d630cd63af8022d5a1fee0e7aa05450b6e0fd75e)

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
