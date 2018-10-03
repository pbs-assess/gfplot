# gfplot: An R package for data extraction (at PBS) and plotting (of any) groundfish data

[![Travis-CI Build
Status](https://travis-ci.org/pbs-assess/gfplot.svg?branch=master)](https://travis-ci.org/pbs-assess/gfplot)
[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

Facilitates the creation of an annual groundfish data synopsis report
with [gfsynopsis](https://github.com/pbs-assess/gfsynopsis).

Note that the documentation is incomplete in some places. Please post in
the [issue tracker](https://github.com/pbs-assess/gfplot/issues) if you
have questions or suggestions on how the package or its documentation
could be improved. We welcome pull requests\!

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
#> [11] "get_gear_types"         "get_iphc_hooks"        
#> [13] "get_iphc_sets"          "get_iphc_sets_info"    
#> [15] "get_iphc_skates_info"   "get_major_areas"       
#> [17] "get_management"         "get_management_areas"  
#> [19] "get_most_common_level"  "get_other_surveys"     
#> [21] "get_sara_dat"           "get_species_groups"    
#> [23] "get_ssids"              "get_survey_index"      
#> [25] "get_survey_samples"     "get_survey_sets"
```

Generic functions for any similarly formatted data:

``` r
sort(fns[grepl("tidy", fns)])
#>  [1] "tidy_age_precision"            "tidy_ages_raw"                
#>  [3] "tidy_ages_weighted"            "tidy_catch"                   
#>  [5] "tidy_comps"                    "tidy_comps_commercial"        
#>  [7] "tidy_comps_survey"             "tidy_cpue_historic"           
#>  [9] "tidy_cpue_index"               "tidy_cpue_index_coefs"        
#> [11] "tidy_cpue_index_coefs_tweedie" "tidy_lengths_raw"             
#> [13] "tidy_lengths_weighted"         "tidy_maturity_months"         
#> [15] "tidy_sample_avail"             "tidy_survey_index"            
#> [17] "tidy_survey_sets"
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
#>  [1] "plot_age_precision"     "plot_ages"             
#>  [3] "plot_catch"             "plot_cpue_index"       
#>  [5] "plot_cpue_index_coefs"  "plot_cpue_index_jk"    
#>  [7] "plot_cpue_spatial"      "plot_growth"           
#>  [9] "plot_length_weight"     "plot_lengths"          
#> [11] "plot_mat_ogive"         "plot_maturity_months"  
#> [13] "plot_predictor_bubbles" "plot_qres_histogram"   
#> [15] "plot_qres_qq"           "plot_sample_avail"     
#> [17] "plot_survey_index"      "plot_survey_sets"      
#> [19] "plot_vb"
```
