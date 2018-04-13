gfplot: An R package for data extraction (at PBS) and plotting (of any) groundfish data
=======================================================================================

[![Travis-CI Build
Status](https://travis-ci.org/seananderson/gfplot.svg?branch=master)](https://travis-ci.org/seananderson/gfplot)
[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

Facilitates the creation of an annual groundfish data synopsis report
with [gfsynopsis](https://github.com/seananderson/gfsynopsis).

Installation
============

The gfplot package is *not* ready for use yet. In particular, the
documentation is far from complete. However, it can be installed and
loaded with:

``` r
# install.packages("devtools")
devtools::install_github("seananderson/gfplot")
```

``` r
library("gfplot")
```

Functions specific to PBS:

``` r
fns <- ls("package:gfplot")
sort(fns[grepl("get", fns)])
#>  [1] "get_age_methods"     "get_age_precision"   "get_catch"
#>  [4] "get_comm_samples"    "get_cpue_index"      "get_cpue_spatial"
#>  [7] "get_cpue_spatial_ll" "get_major_areas"     "get_sara_dat"
#> [10] "get_ssids"           "get_survey_index"    "get_survey_samples"
#> [13] "get_survey_sets"
```

Generic functions for any similarly formatted data:

``` r
sort(fns[grepl("tidy", fns)])
#>  [1] "tidy_age_precision"    "tidy_ages_raw"
#>  [3] "tidy_ages_weighted"    "tidy_catch"
#>  [5] "tidy_comps_commercial" "tidy_comps_survey"
#>  [7] "tidy_cpue_index"       "tidy_lengths_raw"
#>  [9] "tidy_lengths_weighted" "tidy_maturity_months"
#> [11] "tidy_sample_avail"     "tidy_survey_index"
#> [13] "tidy_survey_sets"
```

``` r
sort(fns[grepl("fit", fns)])
#> [1] "fit_cpue_index"    "fit_glmmfields"    "fit_length_weight"
#> [4] "fit_mat_ogive"     "fit_survey_sets"   "fit_vb"
```

``` r
sort(fns[grepl("plot", fns)])
#>  [1] "plot_age_precision"    "plot_ages"
#>  [3] "plot_catch"            "plot_cpue_index"
#>  [5] "plot_cpue_index_coefs" "plot_cpue_index_jk"
#>  [7] "plot_cpue_spatial"     "plot_growth"
#>  [9] "plot_length_weight"    "plot_lengths"
#> [11] "plot_mat_ogive"        "plot_maturity_months"
#> [13] "plot_sample_avail"     "plot_survey_index"
#> [15] "plot_survey_sets"      "plot_vb"
```
