[![NonGNU ELPA][nongnu-elpa-badge]][nongnu-elpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]
[![MELPA][melpa-badge]][melpa-package]
[![License GPL 3][badge-license]][copying]
[![Lint Status](https://github.com/clojure-emacs/clojure-ts-mode/actions/workflows/lint-emacs.yml/badge.svg)](https://github.com/clojure-emacs/clojure-ts-mode/actions/workflows/lint-emacs.yml)

# Clojure Tree-Sitter Mode

`clojure-ts-mode` is an Emacs major mode that provides font-lock (syntax
highlighting), indentation, and navigation support for the
[Clojure(Script) programming language](http://clojure.org), powered by the
[tree-sitter-clojure](https://github.com/sogaiu/tree-sitter-clojure)
[tree-sitter](https://tree-sitter.github.io/tree-sitter/) grammar.

## Configuration

To see a list of available configuration options do `M-x customize-group <RET> clojure-ts`.

Most configuration changes will require reverting any active clojure-ts-mode buffers.

### Indentation

clojure-ts-mode currently supports 2 different indentation strategies
- `semantic`, the default, which tries to match the indentation of clojure-mode and cljfmt
- `fixed`, [a simple indentation strategy outlined by Tonsky in a blog post](https://tonsky.me/blog/clojurefmt/)

Set the var `clojure-ts-indent-style` to change it.
``` emacs-lisp
(setq clojure-ts-indent-style 'fixed)
```

### Font Locking

Too highlight entire rich `comment` expression with the comment font face, set
``` emacs-lisp
(setq clojure-ts-comment-macro-font-lock-body t)
```

By default this is `nil`, so that anything within a `comment` expression is
highlighted like regular clojure code.

## Rationale

[clojure-mode](https://github.com/clojure-emacs/clojure-mode) has served us well
for a very long time, but it suffers from a few [long-standing
problems](https://github.com/clojure-emacs/clojure-mode#caveats), related to
Emacs limitations baked into its design. The introduction of built-in support
for Tree-sitter in Emacs 29 provides a natural opportunity to address many of
them. Enter `clojure-ts-mode`.

Keep in mind that the transition to `clojure-ts-mode` won't happen overnight for several reasons:

- getting to feature parity with `clojure-mode` will take some time
- tools that depend on `clojure-mode` will need to be updated to work with `clojure-ts-mode`
- we still need to support users of older Emacs versions that don't support Tree-sitter

That's why `clojure-ts-mode` is being developed independently of `clojure-mode` and will one day replace it when the time is right. (e.g. 3 major Emacs version down the road, so circa Emacs 32)

You can read more about the vision for `clojure-ts-mode` [here](https://metaredux.com/posts/2023/03/12/clojure-mode-meets-tree-sitter.html).

## Current Status

**This library is still under development. Breaking changes should be expected.**

## Installation

### Emacs 29

This package requires Emacs 29 built with tree-sitter support from the [emacs-29 branch](https://git.savannah.gnu.org/cgit/emacs.git/log/?h=emacs-29).

If you decide to build Emacs from source there's some useful information on this in the Emacs repository:
- [Emacs tree-sitter starter-guide](https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=emacs-29)
- [Emacs install instructions](https://git.savannah.gnu.org/cgit/emacs.git/tree/INSTALL.REPO).

### Install clojure-ts-mode

clojure-ts-mode is available on [MElPA](https://melpa.org/#/clojure-ts-mode) and
[NonGNU ELPA](https://elpa.nongnu.org/nongnu/clojure-ts-mode.html).
It can be installed with

``` emacs-lisp
(package-install 'clojure-ts-mode)
```

#### package-vc

Emacs 29 also includes `package-vc-install`, so you can run

``` emacs-lisp
(package-vc-install "https://github.com/clojure-emacs/clojure-ts-mode")
```

to install this package from source.

#### Manual installation

You can install it by cloning the repository and adding it to your load path.

```bash
git clone https://github.com/clojure-emacs/clojure-ts-mode.git
```

```emacs-lisp
(add-to-list 'load-path "~/path/to/clojure-ts-mode/")
```

Once installed, evaluate clojure-ts-mode.el and you should be ready to go.

### Install tree-sitter grammars

The compile tree-sitter clojure shared library must be available to Emacs.
Additionally, the tree-sitter [markdown_inline](https://github.com/MDeiml/tree-sitter-markdown) shared library will also be used for docstrings if available.

If you have `git` and a C compiler (`cc`) available on your system's `PATH`, **then these steps should not be necessary**.
clojure-ts-mode will install the grammars when you first open a Clojure file and
`clojure-ts-ensure-grammars` is set to `t` (the default).

If clojure-ts-mode fails to automatically install the grammar, you have the option to install it manually.

#### From your OS

Some distributions may package the tree-sitter-clojure grammar in their package repositories.
If yours does you may be able to install tree-sitter-clojure with your system package manager.

If the version packaged by your OS is out of date, you may see errors in the `*Messages*` buffer or your clojure buffers will not have any syntax highlighting.

If this happens you should install the grammar manually with `M-x treesit-install-language-grammar <RET> clojure` and follow the prompts.
Recommended values for these prompts can be seen in `clojure-ts-grammar-recipes`.

#### Compile From Source

If all else fails, you can attempt to download and compile manually.
All you need is `git` and a C compiler (GCC works well).

To start, clone [tree-sitter-clojure](https://github.com/sogaiu/tree-sitter-clojure).

Then run the following code (depending on your OS) from the tree-sitter-clojure repository on your machine.

#### Linux

```bash
mkdir -p dist
cc -c -I./src src/parser.c -o "parser.o"
cc -fPIC -shared src/parser.o -o "dist/libtree-sitter-clojure.so"
```

#### macOS

```bash
mkdir -p dist
cc -c -I./src src/parser.c -o "parser.o"
cc -fPIC -shared src/parser.o -o "dist/libtree-sitter-clojure.dylib"
```

#### Windows

I don't know how to do this on Windows. Patches welcome!

#### Finally, in emacs

Then tell Emacs where to find the shared library by adding something like this to your init file

```emacs-lisp
(setq treesit-extra-load-path '( "~/path/to/tree-sitter-clojure/dist"))
```

OR you can move the `libtree-sitter-clojure.so`/`libtree-sitter-clojure.dylib` to a directory named `tree-sitter`
under your `user-emacs-directory` (typically `~/.emacs.d` on Unix systems).

## License

Copyright © 2022-2023 Danny Freeman and [contributors][].

Distributed under the GNU General Public License; type <kbd>C-h C-c</kbd> to view it.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[nongnu-elpa-badge]: https://elpa.nongnu.org/nongnu/clojure-ts-mode.svg
[nongnu-elpa-package]: https://elpa.nongnu.org/nongnu/clojure-ts-mode.html
[melpa-badge]: http://melpa.org/packages/clojure-ts-mode-badge.svg
[melpa-package]: http://melpa.org/#/clojure-ts-mode
[melpa-stable-badge]: http://stable.melpa.org/packages/clojure-ts-mode-badge.svg
[melpa-stable-package]: http://stable.melpa.org/#/clojure-ts-mode
[COPYING]: http://www.gnu.org/copyleft/gpl.html
[contributors]: https://github.com/clojure-emacs/clojure-ts-mode/contributors
