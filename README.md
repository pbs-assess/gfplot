# gfplot: An R package for data extraction (at PBS) and plotting (of any) groundfish data

<!-- [![Travis-CI Build Status](https://travis-ci.org/pbs-assess/gfplot.svg?branch=master)](https://travis-ci.org/pbs-assess/gfplot) -->

<!-- [![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) -->

Facilitates the creation of an annual groundfish data synopsis report
with [gfsynopsis](https://github.com/pbs-assess/gfsynopsis).

Note that the documentation is incomplete in some places. Please post in
the [issue tracker](https://github.com/pbs-assess/gfplot/issues) if you
have questions or suggestions on how the package or its documentation
could be improved. We welcome pull requests\!

In addition to the help available through the R console, a [web version
of the documentation is
available](https://pbs-assess.github.io/gfplot/index.html).

# Installation

The gfplot package can be installed and loaded with:

``` r
install.packages("INLA", repos = c(getOption("repos"), 
  INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)
# install.packages("devtools")
devtools::install_github("pbs-assess/gfplot")
```

``` r
library(gfplot)
```

Functions specific to PBS:

``` r
fns <- ls("package:gfplot")
sort(fns[grepl("get", fns)])
#>  [1] "get_age_methods"        "get_age_precision"     
#>  [3] "get_catch"              "get_commercial_samples"
#>  [5] "get_cpue_historic"      "get_cpue_historical"   
#>  [7] "get_cpue_index"         "get_cpue_spatial"      
#>  [9] "get_cpue_spatial_ll"    "get_fishery_ids"       
#> [11] "get_gear_types"         "get_iphc_1995"         
#> [13] "get_iphc_1996to2002"    "get_iphc_2013"         
#> [15] "get_iphc_hooks"         "get_iphc_sets"         
#> [17] "get_iphc_sets_info"     "get_iphc_skates_info"  
#> [19] "get_iphc_spp_name"      "get_major_areas"       
#> [21] "get_management"         "get_management_areas"  
#> [23] "get_most_common_level"  "get_other_surveys"     
#> [25] "get_sara_dat"           "get_species"           
#> [27] "get_species_groups"     "get_ssids"             
#> [29] "get_survey_index"       "get_survey_samples"    
#> [31] "get_survey_sets"        "iphc_get_calc_plot"
```

Generic functions for any similarly formatted data:

``` r
sort(fns[grepl("tidy", fns)])
#>  [1] "tidy_age_precision"            "tidy_ages_raw"                
#>  [3] "tidy_ages_weighted"            "tidy_catch"                   
#>  [5] "tidy_comps"                    "tidy_comps_commercial"        
#>  [7] "tidy_comps_survey"             "tidy_cpue_historic"           
#>  [9] "tidy_cpue_historical"          "tidy_cpue_index"              
#> [11] "tidy_cpue_index_coefs"         "tidy_cpue_index_coefs_tweedie"
#> [13] "tidy_iphc_survey"              "tidy_lengths_raw"             
#> [15] "tidy_lengths_weighted"         "tidy_maturity_months"         
#> [17] "tidy_sample_avail"             "tidy_survey_index"            
#> [19] "tidy_survey_sets"
```

``` r
sort(fns[grepl("fit", fns)])
#> [1] "fit_cpue_index"         "fit_cpue_index_glmmtmb"
#> [3] "fit_cpue_index_tweedie" "fit_length_weight"     
#> [5] "fit_mat_ogive"          "fit_survey_sets"       
#> [7] "fit_vb"
```

``` r
sort(fns[grepl("plot", fns)])
#>  [1] "iphc_get_calc_plot"     "plot_age_precision"    
#>  [3] "plot_ages"              "plot_catch"            
#>  [5] "plot_cpue_index"        "plot_cpue_index_coefs" 
#>  [7] "plot_cpue_index_jk"     "plot_cpue_spatial"     
#>  [9] "plot_growth"            "plot_length_weight"    
#> [11] "plot_lengths"           "plot_mat_ogive"        
#> [13] "plot_maturity_months"   "plot_predictor_bubbles"
#> [15] "plot_qres_histogram"    "plot_qres_qq"          
#> [17] "plot_sample_avail"      "plot_survey_index"     
#> [19] "plot_survey_sets"       "plot_vb"
```
