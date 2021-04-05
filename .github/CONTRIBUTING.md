# Contributing to CauDEr

Thanks for contributing to CauDEr!

## Code of Conduct

This project and everyone participating in it is governed by
a [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to
uphold this code.

## I just have a question!

Please don't file an issue to ask a question, use the
official [CauDEr Mailing List][mailing-list].

## How Can I Contribute?

### Bug Reports

Issues are a quick way to point out a bug. If you find a bug in CauDEr then
please make sure that:

* There is already an open [Issue][issues].
* The issue has not already been fixed (check the `dev` branch or look for
  [closed Issues][issues-closed]).

When submitting a bug report explain the problem and include additional details
to help maintainers reproduce the problem:

* Use a clear and descriptive title for the issue to identify the problem.
* Describe the exact steps which reproduce the problem in as many details as
  possible.
* Describe the behavior you observed after following the steps and point out
  what exactly is the problem with that behavior.
* Explain which behavior you expected to see instead and why.
* Include screenshots and animated GIFs which show you following the described
  steps and clearly demonstrate the problem.

### Pull Requests

The process described here has several goals:

- Maintain CauDEr's quality
- Fix problems that are important to users
- Engage the community in working toward the best possible CauDEr
- Enable a sustainable system for CauDEr's maintainers to review contributions

Please follow these steps to have your contribution considered by the
maintainers:

1. Follow all instructions in [the template](PULL_REQUEST_TEMPLATE.md)
2. Follow the [guidelines](#guidelines)
3. After you submit your pull request, verify that
   all [status checks][status-checks] are passing.
   <details><summary>What if the status checks are failing?</summary>If a status
   check is failing, and you believe that the failure is unrelated to your
   change, please leave a comment on the pull request explaining why you believe
   the failure is unrelated. A maintainer will re-run the status check for you.
   If we conclude that the failure was a false positive, then we will open an
   issue to track that problem with our status check suite.</details>

While the prerequisites above must be satisfied prior to having your pull
request reviewed, the reviewer(s) may ask you to complete additional design
work, tests, or other changes before your pull request can be ultimately
accepted.

## Guidelines

### Compatibility

CauDEr requires [Erlang/OTP 23.0][erlang].

### Branching

CauDEr uses the [Git-Flow][git-flow] branching model which requires all Pull
Requests to be sent to the `dev` branch; this is where the next planned version
will be developed.

The `master` branch will always contain the latest stable version and is kept
clean, so a "hotfix" can be applied to the `master` branch to create a new
version, without worrying about other features holding it up. For this reason,
all commits need to be made to the `dev` branch, and any sent to the `master`
branch will be closed automatically. If you have multiple changes to submit,
please place all changes into their own branch on your fork.

**One thing at a time:** A pull request should only contain one change. That
does not mean only one commit, but one change - however many commits it took.
The reason for this is that if you change X and Y, but send a pull request for
both at the same time, we might really want X but disagree with Y, meaning we
cannot merge the request. Using the Git-Flow branching model you can create new
branches for both of these features and send two requests.

A reminder: **please use separate branches for each of your PRs** - it will make
it easier for you to keep changes separate from each other and from whatever
else you are doing with your repository!

### Git Commit Messages

* Use the present tense ("Add feature" not "Added feature")
* Use the imperative mood ("Move cursor to..." not "Moves cursor to...")
* Limit the first line to 72 characters or fewer
* Reference issues and pull requests liberally after the first line

### Erlang Styleguide

This project mostly follows [Inaka's Erlang Coding Guidelines][inaka]. However,
you should try to keep the style of your code similar to that of existing code.

In the future we may use a linter.

### Keep your fork up-to-date

Within your local repository, Git will have created an alias, **origin**, for
the Github repository it is bound to. You want to create an alias for the shared
repository as well, so that you can "sync" the two, making sure that your
repository includes any other contributions that have been merged by us into the
shared repo:

```
git remote add upstream https://github.com/mistupv/cauder.git
```

Then synchronizing is done by pulling from us and pushing to you. This is
normally done locally, so that you can resolve any merge conflicts. For
instance, to synchronize `dev` branches:

```
git pull upstream dev
git push origin dev
```

Your fork is now up to date. This should be done regularly and, at the least,
before you submit a pull request.


[mailing-list]: https://listas.upv.es/mailman/listinfo/cauder
[issues]: https://github.com/mistupv/cauder/issues
[issues-closed]: https://github.com/mistupv/cauder/issues?q=is%3Aissue+is%3Aclosed
[status-checks]: https://help.github.com/articles/about-status-checks/
[erlang]: https://www.erlang.org/downloads/23.0
[git-flow]: http://nvie.com/posts/a-successful-git-branching-model/
[inaka]: https://github.com/inaka/erlang_guidelines
