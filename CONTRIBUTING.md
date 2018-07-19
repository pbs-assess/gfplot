# Contributing

We love pull requests from everyone. By participating in this project, you
agree to abide by our [code of conduct](CONDUCT.md).

## Getting Started

* Make sure you have a [GitHub account](https://github.com/signup/free). If you are not familar with git and GitHub, take a look at <http://happygitwithr.com/> to get started.
* [Submit a post for your issue](https://github.com/pbs-assess/gfplot/issues/), assuming one does not already exist.
* Clearly describe your issue, including steps to reproduce when it is a bug, or some justification for a proposed improvement.
* [Fork](https://github.com/pbs-assess/gfplot/#fork-destination-box) the repository on GitHub to make a copy of the repository on your account. Or use this line in your shell terminal:

    `git clone git@github.com:your-username/gfplot.git`

## Making changes

* Edit the files, save often, and make commits of logical units, where each commit indicates one concept
* Follow our [style guide](http://adv-r.had.co.nz/Style.html).
* Make sure you write [good commit messages](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).
* Make sure you have added the necessary tests for your code changes.
* Run _all_ the tests using `devtools::check()` to assure nothing else was accidentally broken.
* If you need help or unsure about anything, post an update to [your issue](https://github.com/pbs-assess/gfplot/issues/).

## Submitting your changes

Push to your fork and [submit a pull request](https://github.com/pbs-assess/gfplot/compare/).

At this point you're waiting on us. We like to at least comment on pull requests
within a few days (and, typically, one business day). We may suggest
some changes or improvements or alternatives.

Some things you can do that will increase the chance that your pull request is accepted:

* Engage in discussion on [your issue](https://github.com/pbs-assess/gfplot/issues/).
* Write tests that pass `devtools::check()`.
* Follow our [code style guide](http://adv-r.had.co.nz/Style.html).
* Write a [good commit message](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).

## Building the package

The best resource on developing R packages is <http://r-pkgs.had.co.nz/>.

Having cloned the repository onto your computer, open R in the `gfplot/` directory (if you open the `gfplot.Rproj` file in RStudio your working directory will be automatically set) and run:

```r
devtools::install()
```

which will (re)build and install the package.

When working on the package (e.g. editing functions) use:


```r
devtool::load_all()
```

which is quicker as it simulates an install but does not put it into your permanent library.

If you've added a new function or edited documentation and then the run

```r
devtool::document()
```

to rebuild the documentation files.

To also update the [online version of the documentation](https://pbs-assess.github.io/gfplot/index.html), run

```r
pkgdown::build_site()
```

To download all the PBS data for specific species (as long as you have access to the SQL servers) use

```r
gfplot::cache_pbs_data("lingcod")
```

replacing the species with your species of interest.
