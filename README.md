[![NonGNU ELPA][nongnu-elpa-badge]][nongnu-elpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]
[![MELPA][melpa-badge]][melpa-package]
[![License GPL 3][badge-license]][copying]
[![Lint Status](https://github.com/clojure-emacs/clojure-ts-mode/actions/workflows/lint-emacs.yml/badge.svg)](https://github.com/clojure-emacs/clojure-ts-mode/actions/workflows/lint-emacs.yml)

# Clojure Tree-sitter Mode

`clojure-ts-mode` is an Emacs major mode that provides font-lock (syntax
highlighting), indentation, and navigation support for the
[Clojure(Script) programming language](http://clojure.org), powered by the
[tree-sitter-clojure](https://github.com/sogaiu/tree-sitter-clojure)
[Tree-sitter](https://tree-sitter.github.io/tree-sitter/) grammar.

## Rationale

[clojure-mode](https://github.com/clojure-emacs/clojure-mode) has served us well
for a very long time, but it suffers from a few [long-standing
problems](https://github.com/clojure-emacs/clojure-mode#caveats), related to
Emacs limitations baked into its design. The introduction of built-in support
for Tree-sitter in Emacs 29 presents a natural opportunity to address many of
them. Enter `clojure-ts-mode`, which makes use of Tree-sitter to provide:

- fast, accurate and more granular font-locking
- fast indentation
- common Emacs functionality like structured navigation, `imenu` (an outline of
  a source buffer), current form inference (used internally by various Emacs
  modes and utilities), etc

Working with Tree-sitter is significantly easier than the legacy Emacs APIs for font-locking and
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

For `clojure-ts-mode` to work, you need Emacs 30+ built with Tree-sitter support.
To check if your Emacs supports Tree-sitter run the following (e.g. by using `M-:`):

``` emacs-lisp
(treesit-available-p)
```

Additionally, you'll need to have Git and some C compiler (`cc`) installed and available
in your `$PATH` (or Emacs's `exec-path`), for `clojure-ts-mode` to be able to install the required
Tree-sitter grammars automatically.

> [!TIP]
>
> As the Tree-sitter support in Emacs is still fairly new and under active development itself, for optimal
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

### Install Tree-sitter grammars

> [!NOTE]
>
> `clojure-ts-mode` install the required grammars automatically, so for most
> people no manual actions will be required.

`clojure-ts-mode` makes use of the following Tree-sitter grammars:

- The [experimental](https://github.com/sogaiu/tree-sitter-clojure/tree/unstable-20250526) version Clojure grammar. This version includes a few
  improvements, which potentially will be promoted to a stable release (See [the
  discussion](https://github.com/sogaiu/tree-sitter-clojure/issues/65)). This grammar is required for proper work of `clojure-ts-mode`.
- [markdown-inline](https://github.com/MDeiml/tree-sitter-markdown), which will be used for docstrings if available and if
  `clojure-ts-use-markdown-inline` is enabled.
- [tree-sitter-regex](https://github.com/tree-sitter/tree-sitter-regex/releases/tag/v0.24.3), which will be used for regex literals if available and if
  `clojure-ts-use-regex-parser` is not `nil`.

`clojure-ts-clojurescript-mode` can optionally use `tree-sitter-javascript` grammar
to highlight JS syntax in `js*` forms.  This is enabled by default and can be
turned off by setting `clojure-ts-clojurescript-use-js-parser` to `nil`.

`clojure-ts-jank-mode` can optionally use `tree-sitter-cpp` grammar to highlight C++
syntax in `native/raw` forms.  This is enabled by default and can be turned off by
setting `clojure-ts-jank-use-cpp-parser` to `nil`.

If you have `git` and a C compiler (`cc`) available on your system's `PATH`,
`clojure-ts-mode` will install the
grammars when you first open a Clojure file and `clojure-ts-ensure-grammars` is
set to `t` (the default). macOS users can install the required tools like this:

```shell
xcode-select --install
```

Similarly, Debian/Ubuntu users can do something like:

```shell
sudo apt install build-essential
```

This installs GCC, G++, `make`, and other essential development tools.

If `clojure-ts-mode` fails to automatically install the grammar, you have the
option to install it manually. Please, refer to the installation instructions of
each required grammar and make sure you're install the versions expected (see
`clojure-ts-grammar-recipes` for details).

If `clojure-ts-ensure-grammars` is enabled, `clojure-ts-mode` will try to upgrade
the Clojure grammar if it's outdated. This might happen, when you activate
`clojure-ts-mode` for the first time after package update. If grammar was
previously installed, you might need to restart Emacs, because it has to reload
the grammar binary.

### Upgrading Tree-sitter grammars

To reinstall or upgrade Tree-sitter grammars, you can execute:

```emacs-lisp
M-x clojure-ts-reinstall-grammars
```

This will install the latest compatible grammars, even if they are already
installed.

## Configuration

To see a list of available configuration options do `M-x customize-group <RET> clojure-ts`.

Most configuration changes will require reverting any active `clojure-ts-mode` buffers.

### Remapping of `clojure-mode` buffers

By default, `clojure-ts-mode` assumes command over all buffers and file
extensions previously associated with `clojure-mode` (and derived major modes
like `clojurescript-mode`). To disable this remapping, set

``` emacs-lisp
(setopt clojure-ts-auto-remap nil)
```

You can also use the commands `clojure-ts-activate` / `clojure-ts-deactivate` to
interactively change this behavior.

### Indentation

`clojure-ts-mode` currently supports 2 different indentation strategies:

- `semantic`, the default, which tries to match the indentation of `clojure-mode` and `cljfmt`
- `fixed`, [a simple indentation strategy outlined by Tonsky in a blog post](https://tonsky.me/blog/clojurefmt/)

Set the var `clojure-ts-indent-style` to change it.

``` emacs-lisp
(setopt clojure-ts-indent-style 'fixed)
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

#### Project-specific indentation

Custom indentation rules can be set for individual projects. To achieve this,
you need to create a `.dir-locals.el` file in the project root. The content
should look like:

```emacs-lisp
((clojure-ts-mode . ((clojure-ts-semantic-indent-rules . (("with-transaction" . ((:block 1)))
                                                          ("with-retry" . ((:block 1))))))))
```

In order to apply directory-local variables to existing buffers, they must be
"reverted" (reloaded).

### Vertical alignment

You can vertically align sexps with `C-c SPC`. For instance, typing this combo
on the following form:

```clojure
(def my-map
  {:a-key 1
   :other-key 2})
```

Leads to the following:

```clojure
(def my-map
  {:a-key     1
   :other-key 2})
```

This can also be done automatically (as part of indentation) by turning on
`clojure-ts-align-forms-automatically`. This way it will happen whenever you
select some code and hit `TAB`.

Forms that can be aligned vertically are configured via the following variables:

- `clojure-ts-align-reader-conditionals` - align reader conditionals as if they
  were maps.
- `clojure-ts-align-binding-forms` - a customizable list of forms with let-like
  bindings that can be aligned vertically.
- `clojure-ts-align-cond-forms` - a customizable list of forms whose body
  elements can be aligned vertically. These forms respect the block semantic
  indentation rule (if configured) and align only the body forms, skipping N
  special arguments.
- `clojure-ts-align-separator` - determines whether blank lines prevent vertical
  alignment.

### Font Locking

To highlight entire rich `comment` expression with the comment font face, set

``` emacs-lisp
(setopt clojure-ts-comment-macro-font-lock-body t)
```

By default this is `nil`, so that anything within a `comment` expression is
highlighted like regular Clojure code.

> [!TIP]
>
> You can customize the exact level of font-locking via the variables
> `treesit-font-lock-level` (the default value is 3) and
> `treesit-font-lock-features-list`. Check [this
> section](https://www.gnu.org/software/emacs/manual/html_node/emacs/Parser_002dbased-Font-Lock.html)
> of the Emacs manual for more details.

#### Extending font-lock rules

In `clojure-ts-mode` it is possible to specify additional defn-like forms that
should be fontified.  For example to highlight the following form from Hiccup
library as a function definition:

```clojure
(defelem file-upload
  "Creates a file upload input."
  [name]
  (input-field "file" name nil))
```

You can add `defelem` to `clojure-ts-extra-def-forms` list like this:

```emacs-lisp
(add-to-list 'clojure-ts-extra-def-forms "defelem")
```

or set this variable using `setopt`:

```emacs-lisp
(setopt clojure-ts-extra-def-forms '("defelem"))
```

This setting will highlight `defelem` symbol, function name and the docstring.

> [!IMPORTANT]
>
> Setting `clojure-ts-extra-def-forms` won't change the indentation rule for
> these forms.  For indentation rules you should use
> `clojure-ts-semantic-indent-rules` variable (see [semantic
> indentation](#customizing-semantic-indentation) section).

### Highlight markdown syntax in docstrings

By default Markdown syntax is highlighted in the docstrings using
`markdown-inline` grammar. To disable this feature use:

``` emacs-lisp
(setopt clojure-ts-use-markdown-inline nil)
```

Example of Markdown syntax highlighting:

<img width="512" src="/screenshots/markdown-syntax-dark-theme.png">

### Highlight regular expression syntax

By default syntax inside regex literals is highlighted using
[regex](https://github.com/tree-sitter/tree-sitter-regex) grammar. To disable
this feature use:

```emacs-lisp
(setopt clojure-ts-use-regex-parser nil)
```

Example of regex syntax highlighting:

<img width="512" src="/screenshots/regex-syntax-dark-theme.png">

### Navigation and Evaluation

To make forms inside of `(comment ...)` forms appear as top-level forms for evaluation and navigation, set

``` emacs-lisp
(setopt clojure-ts-toplevel-inside-comment-form t)
```

### Fill paragraph

To change the maximal line length used by `M-x prog-fill-reindent-defun` (also
bound to `M-q` by default) to reformat docstrings and comments it's possible to
customize `clojure-ts-fill-paragraph` variable (by default set to the value of
Emacs' `fill-paragraph` value).

Every new line in the docstrings is indented by
`clojure-ts-docstring-fill-prefix-width` number of spaces (set to 2 by default
which matches the `clojure-mode` settings).

### imenu

`clojure-ts-mode` supports various types of definition that can be navigated
using `imenu`, such as:

- namespace
- function
- macro
- var
- interface (forms such as `defprotocol`, `definterface` and `defmulti`)
- class (forms such as `deftype`, `defrecord` and `defstruct`)
- keyword (for example, spec definitions)

### Integration with `outline-minor-mode`

`clojure-ts-mode` supports two integration variants with
`outline-minor-mode`. The default variant uses special top-level comments (level
1 heading starts with three semicolons, level 2 heading starts with four,
etc.). The other variant treats def-like forms (the same forms produced by the
`imenu` command) as outline headings. To use the second option, use the
following customization:

```emacs-lisp
(setopt clojure-ts-outline-variant 'imenu)
```

## Refactoring support

### Threading macros related features

There are a bunch of commands for threading and unwinding threaded Clojure forms:

- `clojure-ts-thread`: Thread another form into the surrounding thread. Both
`->>`/`some->>` and `->`/`some->` variants are supported.
- `clojure-ts-unwind`: Unwind a threaded expression. Supports both `->>`/`some->>`
and `->`/`some->`.
- `clojure-ts-thread-first-all`: Introduce the thread first macro (`->`) and
rewrite the entire form. With a prefix argument do not thread the last form.
- `clojure-ts-thread-last-all`: Introduce the thread last macro and rewrite the
entire form. With a prefix argument do not thread the last form.
- `clojure-ts-unwind-all`: Fully unwind a threaded expression removing the
threading macro.

#### Customize threading refactoring behavior

By default `clojure-ts-thread-first-all` and `clojure-ts-thread-last-all` will
thread all nested expressions. For example this expression:

```clojure
(->map (assoc {} :key "value") :lock)
```

After executing `clojure-ts-thread-last-all` will be converted to:

```clojure
(-> {}
    (assoc :key "value")
    (->map :lock))
```

This behavior can be changed by setting:

```emacs-lisp
(setopt clojure-ts-thread-all-but-last t)
```

Then the last expression will not be threaded and the result will be:

```clojure
(-> (assoc {} :key "value")
    (->map :lock))
```

### Cycling things

- `clojure-ts-cycle-keyword-string`: Convert the string at point to a keyword and
vice versa.
- `clojure-ts-cycle-privacy`: Cycle privacy of `def`s or `defn`s. Use metadata
explicitly with setting `clojure-ts-use-metadata-for-defn-privacy` to `t` for
`defn`s too.
- `clojure-ts-cycle-conditional`: Change a surrounding conditional form to its
  negated counterpart, or vice versa (supports `if`/`if-not` and
  `when`/`when-not`). For `if`/`if-not` also transposes the else and then
  branches, keeping the semantics the same as before.
- `clojure-ts-cycle-not`: Add or remove a `not` form around the current form.

### Convert collection

Convert any given collection at point to list, quoted list, map, vector or
set. The following commands are available:

- `clojure-ts-convert-collection-to-list`
- `clojure-ts-convert-collection-to-quoted-list`
- `clojure-ts-convert-collection-to-map`
- `clojure-ts-convert-collection-to-vector`
- `clojure-ts-convert-collection-to-set`

### Add arity to a function or macro

`clojure-ts-add-arity`: Add a new arity to an existing single-arity or
multi-arity function or macro. Function can be defined using `defn`, `fn` or
`defmethod` form. This command also supports functions defined inside forms like
`letfn`, `defprotol`, `reify`, `extend-protocol` or `proxy`.

### Default keybindings

| Keybinding                  | Command                                        |
|:----------------------------|:-----------------------------------------------|
| `C-:`                       | `clojure-ts-cycle-keyword-string`              |
| `C-c SPC`                   | `clojure-ts-align`                             |
| `C-c C-r t` / `C-c C-r C-t` | `clojure-ts-thread`                            |
| `C-c C-r u` / `C-c C-r C-u` | `clojure-ts-unwind`                            |
| `C-c C-r f` / `C-c C-r C-f` | `clojure-ts-thread-first-all`                  |
| `C-c C-r l` / `C-c C-r C-l` | `clojure-ts-thread-last-all`                   |
| `C-c C-r p` / `C-c C-r C-p` | `clojure-ts-cycle-privacy`                     |
| `C-c C-r (` / `C-c C-r C-(` | `clojure-ts-convert-collection-to-list`        |
| `C-c C-r '` / `C-c C-r C-'` | `clojure-ts-convert-collection-to-quoted-list` |
| `C-c C-r {` / `C-c C-r C-{` | `clojure-ts-convert-collection-to-map`         |
| `C-c C-r [` / `C-c C-r C-[` | `clojure-ts-convert-collection-to-vector`      |
| `C-c C-r #` / `C-c C-r C-#` | `clojure-ts-convert-collection-to-set`         |
| `C-c C-r c` / `C-c C-r C-c` | `clojure-ts-cycle-conditional`                 |
| `C-c C-r o` / `C-c C-r C-o` | `clojure-ts-cycle-not`                         |
| `C-c C-r a` / `C-c C-r C-a` | `clojure-ts-add-arity`                         |

### Customize refactoring commands prefix

By default prefix for all refactoring commands is `C-c C-r`. It can be changed
by customizing `clojure-ts-refactor-map-prefix` variable.

## Code completion

`clojure-ts-mode` provides basic code completion functionality.  Completion only
works for the current source buffer and includes completion of top-level
definitions and local bindings.  This feature can be turned off by setting:

```emacs-lisp
(setopt clojure-ts-completion-enabled nil)
```

Here's the short video illustrating the feature with Emacs's built-in completion UI (it
should also work well with more advanced packages like `company` and `corfu`):

https://github.com/user-attachments/assets/7c37179f-5a5d-424f-9bd6-9c8525f6b2f7

## Migrating to clojure-ts-mode

If you are migrating to `clojure-ts-mode` note that `clojure-mode` is still
required for CIDER and `clj-refactor` packages to work properly.

After installing the package do the following:

- Check the value of `clojure-mode-hook` and copy all relevant hooks to `clojure-ts-mode-hook`.

``` emacs-lisp
(add-hook 'clojure-ts-mode-hook #'cider-mode)
(add-hook 'clojure-ts-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-ts-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-ts-mode-hook #'clj-refactor-mode)
```

- Update `.dir-locals.el` in all of your Clojure projects to activate directory
  local variables in `clojure-ts-mode`.

``` emacs-lisp
((clojure-mode
  (cider-clojure-cli-aliases . ":test:repl"))
 (clojure-ts-mode
  (cider-clojure-cli-aliases . ":test:repl")))
```

## Caveats

As the Tree-sitter Emacs APIs are new and keep evolving there are some
differences in the behavior of `clojure-ts-mode` on different Emacs versions.
Here are some notable examples:

- On Emacs 29 the parent mode is `prog-mode`, but on Emacs 30+ it's both `prog-mode`
and `clojure-mode` (this is very helpful when dealing with `derived-mode-p` checks)
- Navigation by sexp/lists might work differently on Emacs versions lower
  than 31. Starting with version 31, Emacs uses Tree-sitter 'things' settings, if
  available, to rebind some commands.
- If you set `clojure-ts-extra-def-forms`, `clojure-ts-mode` will highlight the
  specified forms, including their docstrings, in a manner similar to Clojure's
  `defn`.  However, Markdown syntax will not be highlighted within these custom
  docstrings.

## Frequently Asked Questions

### What `clojure-mode` features are currently missing?

As of version 0.5.x, `clojure-ts-mode` provides almost all `clojure-mode` features.
Currently only a few refactoring commands are missing.

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

### Does `clojure-ts-mode` work with `inf-clojure`?

Yes, it does. `inf-clojure` 3.3+ supports `clojure-ts-mode`.

### Why does `clojure-ts-mode` require Emacs 30?

You might be wondering why does `clojure-ts-mode` require Emacs 30 instead of
Emacs 29, which introduced the built-in Tree-sitter support. The answer is
simple - the initial Tree-sitter support in Emacs 29 had quite a few issues and
we felt it's better to nudge most people interested in using it to Emacs 30,
which fixed a lot of the problems.

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
