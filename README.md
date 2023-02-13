[![License GPL 3][badge-license]][copying]

# Clojure Tree-Sitter Mode

`clojure-ts-mode` is an Emacs major mode that provides font-lock (syntax
highlighting), indentation, and navigation support for the
[Clojure(Script) programming language](http://clojure.org), powered by the
[tree-sitter-clojure](https://github.com/sogaiu/tree-sitter-clojure)
[tree-sitter](https://tree-sitter.github.io/tree-sitter/) grammar.

## Installation

### Emacs 29

This package requires Emacs 29 built with tree-sitter support from the [emacs-29 branch](https://git.savannah.gnu.org/cgit/emacs.git/log/?h=emacs-29).
As of right now, users must install Emacs from source with tree-sitter installed on their system.
More information on this can be found in the Emacs repository:
- [Emacs tree-sitter starter-guide](https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=emacs-29)
- [Emacs install instructions](https://git.savannah.gnu.org/cgit/emacs.git/tree/INSTALL.REPO).

### Install clojure-ts-mode

This package is not yet avaialble through package.el.
You can install it by cloning the repository and adding it to your load path.

```bash
git clone https://github.com/clojure-emacs/clojure-ts-mode.git
```

```emacs-lisp
(add-to-list 'load-path "~/path/to/clojure-ts-mode/")
```

Once installed, evaluate clojure-ts-mode.el and you should be ready to go.

### Install libtree-sitter-clojure shared library

The tree-sitter clojure shared library must be available to Emacs.
If you have `git` and a c compiler (`cc`) available on your system's `PATH`, **then these steps are not necessary**.
clojure-ts-mode will install the grammar when you first open a Clojure file.

If clojure-ts-mode fails to automatically install the grammar, you have the option to install it manually.
All you need is `git` and a c compiler (gcc works well).

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
under your `user-emacs-directory`.

## License

Copyright © 2022 Danny Freeman and [contributors][].

Distributed under the GNU General Public License; type <kbd>C-h C-c</kbd> to view it.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[COPYING]: http://www.gnu.org/copyleft/gpl.html
[contributors]: https://github.com/clojure-emacs/clojure-ts-mode/contributors
