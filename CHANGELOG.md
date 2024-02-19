# Changelog

## main (unreleased)

- [#38]: Add support for `in-ns` forms in `clojure-ts-find-ns`.

## 0.2.2 (2024-02-16)

- [#37]: Fix derived modes broken with [#36].

## 0.2.1 (2024-02-14)

- [#36]: Rename all derived mode vars to match the package prefix.
    - `clojurescript-ts-mode` -> `clojure-ts-clojurescript-mode`
    - `clojurec-ts-mode` -> `clojure-ts-clojurec-mode`
    - `clojure-dart-ts-mode` -> `clojure-ts-clojuredart-mode`
    - `clojure-jank-ts-mode` -> `clojure-ts-jank-mode`
- [#30]: Add custom option `clojure-ts-toplevel-inside-comment-form` as an equivalent to `clojure-toplevel-inside-comment-form` in `clojure-mode`.
- [#32]: Change behavior of `beginning-of-defun` and `end-of-defun` to consider all Clojure sexps as defuns.

## 0.2.0

- Pin grammar revision in treesit-language-source-alist
    - [bd61a7fb281b7b0b1d2e20d19ab5d46cbcdc6c1e](https://github.com/clojure-emacs/clojure-ts-mode/commit/bd61a7fb281b7b0b1d2e20d19ab5d46cbcdc6c1e)
- Make font lock feature list more conforming with recommendations
    - (See treesit-font-lock-level documentation for more information.)
    - [2225190ee57ef667d69f2cd740e0137810bc38e7](https://github.com/clojure-emacs/clojure-ts-mode/commit/2225190ee57ef667d69f2cd740e0137810bc38e7)
- Highlight docstrings in interface, protocol, and variable definitions
    - [9af0a6b35c708309acdfeb4c0c79061b0fd4eb44](https://github.com/clojure-emacs/clojure-ts-mode/commit/9af0a6b35c708309acdfeb4c0c79061b0fd4eb44)
- Add support for semantic indentation (now the default)
    - [ae2e2486010554cfeb12f06a1485b4d81609d964](https://github.com/clojure-emacs/clojure-ts-mode/commit/ae2e2486010554cfeb12f06a1485b4d81609d964)
    - [ca3914aa7aa9645ab244658f8db781cc6f95111e](https://github.com/clojure-emacs/clojure-ts-mode/commit/ca3914aa7aa9645ab244658f8db781cc6f95111e)
    - [85871fdbc831b3129dae5762e9c247d453c35e15](https://github.com/clojure-emacs/clojure-ts-mode/commit/85871fdbc831b3129dae5762e9c247d453c35e15)
    - [ff5d7e13dc53cc5da0e8139b04e02d90f61d9065](https://github.com/clojure-emacs/clojure-ts-mode/commit/ff5d7e13dc53cc5da0e8139b04e02d90f61d9065)
- Highlight "\`quoted-symbols\`  in docs strings like this."
   - This feature uses a nested markdown parser.
     If the parser is not available this feature should be silently disabled.
    - [9af0a6b35c708309acdfeb4c0c79061b0fd4eb44](https://github.com/clojure-emacs/clojure-ts-mode/commit/9af0a6b35c708309acdfeb4c0c79061b0fd4eb44)
- Highlight methods for `deftype`, `defrecord`, `defprotocol`, `reify` and `definterface`
  forms ([#20](https://github.com/clojure-emacs/clojure-ts-mode/issues/20)).
    - [5231c348e509cff91edd1ec59d7a59645395da15](https://github.com/clojure-emacs/clojure-ts-mode/commit/5231c348e509cff91edd1ec59d7a59645395da15)
    - Thank you rrudakov for this contribution.
- Add derived `clojure-jank-ts-mode` for the [Jank](https://github.com/jank-lang/jank) dialect of clojure
    - [a7b9654488693cdc9057a91410f74de42a397d1b](https://github.com/clojure-emacs/clojure-ts-mode/commit/a7b9654488693cdc9057a91410f74de42a397d1b)

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
