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

## Rationale

[clojure-mode](https://github.com/clojure-emacs/clojure-mode) has served us well
for a very long time, but it suffers from a few [long-standing
problems](https://github.com/clojure-emacs/clojure-mode#caveats), related to
Emacs limitations baked into its design. The introduction of built-in support
for Tree-sitter in Emacs 29 presents a natural opportunity to address many of
them. Enter `clojure-ts-mode`, which makes use of TreeSitter to provide:

- fast, accurate and more granular font-locking
- fast indentation
- common Emacs functionality like structured navigation, `imenu` (an outline of a source buffer), current form inference (used internally by various Emacs modes and utilities), etc

Working with TreeSitter is significantly easier than the legacy Emacs APIs for font-locking and
indentation, which makes it easier to contribute to `clojure-ts-mode`, and to improve it in general.

Keep in mind that the transition to `clojure-ts-mode` won't happen overnight for several reasons:

- getting to feature parity with `clojure-mode` will take some time
- tools that depend on `clojure-mode` will need to be updated to work with `clojure-ts-mode`
- we still need to support users of older Emacs versions that don't support Tree-sitter

That's why `clojure-ts-mode` is being developed independently of `clojure-mode`
and will one day replace it when the time is right. (e.g. 3 major Emacs version
down the road, so circa Emacs 32)

You can read more about the vision for `clojure-ts-mode` [here](https://metaredux.com/posts/2023/03/12/clojure-mode-meets-tree-sitter.html).

## Current Status

> [!WARNING]
>
> This library is still under active development. Breaking changes should be expected.

The currently provided functionality should cover the needs of most Clojure programmers, but you
can expect to encounter some bugs and missing functionality here and there.

Those will be addressed over the time, as more and more people use `clojure-ts-mode`.

## Installation

### Requirements

For `clojure-ts-mode` to work, you need Emacs 30+ built with TreeSitter support.
To check if your Emacs supports TreeSitter run the following (e.g. by using `M-:`):

``` emacs-lisp
(treesit-available-p)
```

Additionally, you'll need to have Git and some C compiler (`cc`) installed and available
in your `$PATH` (or Emacs's `exec-path`), for `clojure-ts-mode` to be able to install the required
TreeSitter grammars automatically.

> [!TIP]
>
> As the TreeSitter support in Emacs is still fairly new and under active development itself, for optimal
> results you should use the latest stable Emacs release or even the development version of Emacs.
> See the "Caveats" section for more on the subject.

### Install clojure-ts-mode

> [!NOTE]
>
> That's the recommended way to install `clojure-ts-mode`.

If you have `git` and a C compiler (`cc`) available on your system's `PATH`,
`clojure-ts-mode` will install the
grammars

clojure-ts-mode is available on [MElPA](https://melpa.org/#/clojure-ts-mode) and
[NonGNU ELPA](https://elpa.nongnu.org/nongnu/clojure-ts-mode.html).
It can be installed with:

``` emacs-lisp
(package-install 'clojure-ts-mode)
```

#### package-vc

Emacs also includes `package-vc-install`, so you can run:

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

Once installed, evaluate `clojure-ts-mode.el` and you should be ready to go.

### Install tree-sitter grammars

> [!NOTE]
>
> `clojure-ts-mode` install the required grammars automatically, so for most
> people no manual actions will be required.

`clojure-ts-mode` makes use of two TreeSitter grammars to work properly:

- The Clojure grammar, mentioned earlier
- [markdown-inline](https://github.com/MDeiml/tree-sitter-markdown), which
will be used for docstrings if available and if `clojure-ts-use-markdown-inline` is enabled.

If you have `git` and a C compiler (`cc`) available on your system's `PATH`,
`clojure-ts-mode` will install the
grammars when you first open a Clojure file and `clojure-ts-ensure-grammars` is
set to `t` (the default).

If `clojure-ts-mode` fails to automatically install the grammar, you have the
option to install it manually, Please, refer to the installation instructions of
each required grammar and make sure you're install the versions expected. (see
`clojure-ts-grammar-recipes` for details)

### Upgrading tree-sitter grammars

To reinstall or upgrade TreeSitter grammars, you can execute:

```emacs-lisp
M-x clojure-ts-reinstall-grammars
```

This will install the latest compatible grammars, even if they are already
installed.

## Configuration

To see a list of available configuration options do `M-x customize-group <RET> clojure-ts`.

Most configuration changes will require reverting any active `clojure-ts-mode` buffers.

### Remapping of `clojure-mode` buffers

By default, `clojure-ts-mode` assumes command over all buffers and file extensions previously associated with `clojure-mode` (and derived major modes like `clojurescript-mode`). To disable this remapping, set

``` emacs-lisp
(setopt clojure-ts-auto-remap nil)
```

You can also use the commands `clojure-ts-activate` / `clojure-ts-deactivate` to interactively change this behavior.

### Indentation

`clojure-ts-mode` currently supports 2 different indentation strategies:

- `semantic`, the default, which tries to match the indentation of `clojure-mode` and `cljfmt`
- `fixed`, [a simple indentation strategy outlined by Tonsky in a blog post](https://tonsky.me/blog/clojurefmt/)

Set the var `clojure-ts-indent-style` to change it.

``` emacs-lisp
(setq clojure-ts-indent-style 'fixed)
```

> [!TIP]
>
> You can find [this article](https://metaredux.com/posts/2020/12/06/semantic-clojure-formatting.html) comparing semantic and fixed indentation useful.

#### Customizing semantic indentation

The indentation of special forms and macros with bodies is controlled via
`clojure-ts-semantic-indent-rules`. Nearly all special forms and built-in macros
with bodies have special indentation settings in clojure-ts-mode, which are
aligned with cljfmt indent rules. You can add/alter the indentation settings in
your personal config. Let's assume you want to indent `->>` and `->` like this:

```clojure
(->> something
  ala
  bala
  portokala)
```

You can do so by putting the following in your config:

```emacs-lisp
(setopt clojure-ts-semantic-indent-rules '(("->" . ((:block 1)))
                                           ("->>" . ((:block 1)))))
```

This means that the body of the `->`/`->>` is after the first argument.

The default set of rules is defined as
`clojure-ts--semantic-indent-rules-defaults`, any rule can be overridden using
customization option.

Two types of rules are supported: `:block` and `:inner`, mirroring those in
cljfmt. When a rule is defined as `:block n`, `n` represents the number of
arguments preceding the body. When a rule is defined as `:inner n`, each form
within the expression's body, nested `n` levels deep, is indented by two
spaces. These rule definitions fully reflect the [cljfmt rules](https://github.com/weavejester/cljfmt/blob/0.13.0/docs/INDENTS.md).

For example:
- `do` has a rule `((:block 0))`.
- `when` has a rule `((:block 1))`.
- `defn` and `fn` have a rule `((:inner 0))`.
- `letfn` has a rule `((:block 1) (:inner 2 0))`.

Note that `clojure-ts-semantic-indent-rules` should be set using the
customization interface or `setopt`; otherwise, it will not be applied
correctly.

#### Project local indentation

Custom indentation rules can be set for individual projects. To achieve this,
you need to create a `.dir-locals.el` file in the project root. The content
should look like:

```emacs-lisp
((clojure-ts-mode . ((clojure-ts-semantic-indent-rules . (("with-transaction" . ((:block 1)))
                                                          ("with-retry" . ((:block 1))))))))
```

In order to apply directory-local variables to existing buffers, they must be
reverted.

### Font Locking

To highlight entire rich `comment` expression with the comment font face, set

``` emacs-lisp
(setq clojure-ts-comment-macro-font-lock-body t)
```

By default this is `nil`, so that anything within a `comment` expression is
highlighted like regular clojure code.

> [!TIP]
>
> You can customize the exact level of font-locking via the variables
> `treesit-font-lock-level` (the default value is 3) and
> `treesit-font-lock-features-list`. Check [this
> section](https://www.gnu.org/software/emacs/manual/html_node/emacs/Parser_002dbased-Font-Lock.html)
> of the Emacs manual for more details.

### Highlight markdown syntax in docstrings

By default markdown syntax is highlighted in the docstrings using
`markdown_inline` grammar. To disable this feature set

``` emacs-lisp
(setopt clojure-ts-use-markdown-inline nil)
```

### Navigation and Evaluation

To make forms inside of `(comment ...)` forms appear as top-level forms for evaluation and navigation, set

``` emacs-lisp
(setq clojure-ts-toplevel-inside-comment-form t)
```

### Fill paragraph

To change the maximal line length used by `M-x prog-fill-reindent-defun` (also
bound to `M-q` by default) to reformat docstrings and comments it's possible to
customize `clojure-ts-fill-paragraph` variable (by default set to the value of
Emacs' `fill-paragraph` value).

Every new line in the docstrings is indented by
`clojure-ts-docstring-fill-prefix-width` number of spaces (set to 2 by default
which matches the `clojure-mode` settings).

#### imenu

`clojure-ts-mode` supports various types of definition that can be navigated
using `imenu`, such as:

- namespace
- function
- macro
- var
- interface (forms such as `defprotocol`, `definterface` and `defmulti`)
- class (forms such as `deftype`, `defrecord` and `defstruct`)
- keyword (for example, spec definitions)

## Migrating to clojure-ts-mode

If you are migrating to `clojure-ts-mode` note that `clojure-mode` is still required for cider and clj-refactor packages to work properly.

After installing the package do the following.

- Check the value of `clojure-mode-hook` and copy all relevant hooks to `clojure-ts-mode-hook`.

``` emacs-lisp
(add-hook 'clojure-ts-mode-hook #'cider-mode)
(add-hook 'clojure-ts-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-ts-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-ts-mode-hook #'clj-refactor-mode)
```

- Update `.dir-locals.el` in all of your Clojure projects to activate directory local variables in `clojure-ts-mode`.

``` emacs-lisp
((clojure-mode
  (cider-clojure-cli-aliases . ":test:repl"))
 (clojure-ts-mode
  (cider-clojure-cli-aliases . ":test:repl")))
```

## Caveats

As the TreeSitter Emacs APIs are new and keep evolving there are some
differences in the behavior of `clojure-ts-mode` on different Emacs versions.
Here are some notable examples:

- On Emacs 29 the parent mode is `prog-mode`, but on Emacs 30+ it's both `prog-mode`
and `clojure-mode` (this is very helpful when dealing with `derived-mode-p` checks)
- Navigation by sexp/lists might work differently on Emacs versions lower
  than 31. Starting with version 31, Emacs uses TreeSitter 'things' settings, if
  available, to rebind some commands.
- The indentation of list elements with metadata is inconsistent with other
  collections. This inconsistency stems from the grammar's interpretation of
  nearly every definition or function call as a list. Therefore, modifying the
  indentation for list elements would adversely affect the indentation of
  numerous other forms.

## Frequently Asked Questions

### What `clojure-mode` features are currently missing?

As of version 0.2.x, the most obvious missing feature are the various
refactoring commands in `clojure-mode`.

### Does `clojure-ts-mode` work with CIDER?

Yes! Preliminary support for `clojure-ts-mode` was released in [CIDER
1.14](https://github.com/clojure-emacs/cider/releases/tag/v1.14.0). Note that
`clojure-mode` is still needed for some APIs that haven't yet been ported to
`clojure-ts-mode`.

For now, when you take care of the keybindings for the CIDER commands you use
and ensure `cider-mode` is enabled for `clojure-ts-mode` buffers in your config,
most functionality should already work:

```emacs-lisp
(add-hook 'clojure-ts-mode-hook #'cider-mode)
```

Check out [this article](https://metaredux.com/posts/2024/02/19/cider-preliminary-support-for-clojure-ts-mode.html) for more details.

> [!NOTE]
>
> The dynamic indentation feature in CIDER requires clojure-ts-mode 0.3+.
> Dynamic font-locking currently doesn't work with clojure-ts-mode.

### Does `clojure-ts-mode` work with `inf-clojure`?

Currently, there is an [open PR](https://github.com/clojure-emacs/inf-clojure/pull/215) adding support for inf-clojure.

## License

Copyright © 2022-2025 Danny Freeman, Bozhidar Batsov and [contributors][].

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
