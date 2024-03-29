# Contributing

If you discover issues, have ideas for improvements or new features,
please report them to the [issue tracker][1] of the repository or
submit a pull request. Please, try to follow these guidelines when you
do so.

## Issue reporting

* Check that the issue has not already been reported.
* Check that the issue has not already been fixed in the latest code
  (a.k.a. `master`).
* Be clear, concise and precise in your description of the problem.
* Open an issue with a descriptive title and a summary in grammatically correct,
  complete sentences.
* Mention your Emacs version and operating system.
* Mention `clojure-ts-mode`'s version info (`M-x clojure-ts-mode-version-info`), e.g.:

```el
clojure-ts-mode (version 2.1.1)
```

* Include any relevant code to the issue summary.

## Pull requests

* Read [how to properly contribute to open source projects on Github][2].
* Use a topic branch to easily amend a pull request later, if necessary.
* Write [good commit messages][3].
* Mention related tickets in the commit messages (e.g. `[Fix #N] Font-lock properly ...`)
* Update the [changelog][6].
* Use the same coding conventions as the rest of the project.
* Verify your Emacs Lisp code with `checkdoc` (<kbd>C-c ? d</kbd>).
* [Squash related commits together][5].
* Open a [pull request][4] that relates to *only* one subject with a clear title
and description in grammatically correct, complete sentences.

## I don't have a github account

or maybe you would rather use email. That is okay.

If you prefer you can also send a message to the [mailing list][7].
This mailing list is not the [primary issue tracker][1].
All the same etiquette rules above apply to the mailing list as well.
Submitted patches will be turned into pull requests.
Any issues reported on the mailing list will be copied to the issue tracker
where the primary work will take place.

[1]: https://github.com/clojure-emacs/clojure-ts-mode/issues
[2]: http://gun.io/blog/how-to-github-fork-branch-and-pull-request
[3]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[4]: https://help.github.com/articles/using-pull-requests
[5]: http://gitready.com/advanced/2009/02/10/squashing-commits-with-rebase.html
[6]: https://github.com/clojure-emacs/clojure-ts-mode/blob/master/CHANGELOG.md
[7]: https://lists.sr.ht/~dannyfreeman/clojure-ts-mode
