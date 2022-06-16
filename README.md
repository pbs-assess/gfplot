
# gfplot: An R package for data extraction and plotting of British Columbia groundfish data

<!-- badges: start -->

[![R-CMD-check](https://github.com/pbs-assess/gfplot/workflows/R-CMD-check/badge.svg)](https://github.com/pbs-assess/gfplot/actions)
<!-- badges: end -->

Facilitates the creation of an annual groundfish data synopsis report
with [gfsynopsis](https://github.com/pbs-assess/gfsynopsis).

Note that the documentation is incomplete in some places. Please post in
the [issue tracker](https://github.com/pbs-assess/gfplot/issues) if you
have questions or suggestions on how the package or its documentation
could be improved.

In addition to the help available through the R console, a [web version
of the documentation is
available](https://pbs-assess.github.io/gfplot/index.html).

# Installation

The gfplot package can then be installed and loaded with:

``` r
# install.packages("remotes")
remotes::install_github("pbs-assess/gfplot")
```

``` r
library(gfplot)
```

Functions specific to PBS (now in gfdata):

``` r
library(gfdata)
fns <- ls("package:gfdata")
sort(fns[grepl("get", fns)])
#>  [1] "get_active_survey_blocks"  "get_age_methods"          
#>  [3] "get_age_precision"         "get_all_stomachs"         
#>  [5] "get_catch"                 "get_catch_spatial"        
#>  [7] "get_comm_gear_types"       "get_commercial_samples"   
#>  [9] "get_cpue_historical"       "get_cpue_historical_hake" 
#> [11] "get_cpue_historical_hl"    "get_cpue_index"           
#> [13] "get_cpue_index_hl"         "get_cpue_spatial"         
#> [15] "get_cpue_spatial_ll"       "get_eulachon_specimens"   
#> [17] "get_fishery_ids"           "get_fishery_sectors"      
#> [19] "get_gear_types"            "get_hake_catch"           
#> [21] "get_hake_survey_samples"   "get_ll_hook_data"         
#> [23] "get_major_areas"           "get_management"           
#> [25] "get_management_areas"      "get_other_surveys"        
#> [27] "get_sable_landings"        "get_sablefish_surveys"    
#> [29] "get_sensor_attributes"     "get_sensor_data_ll_ctd"   
#> [31] "get_sensor_data_ll_ctd_fe" "get_sensor_data_ll_td"    
#> [33] "get_sensor_data_ll_td_fe"  "get_sensor_data_trawl"    
#> [35] "get_sensor_data_trawl_fe"  "get_species"              
#> [37] "get_species_groups"        "get_ssids"                
#> [39] "get_strata_areas"          "get_survey_blocks"        
#> [41] "get_survey_gear_types"     "get_survey_ids"           
#> [43] "get_survey_index"          "get_survey_samples"       
#> [45] "get_survey_sets"           "get_survey_stomachs"      
#> [47] "get_table"
```

Generic functions for any similarly formatted data:

``` r
fns <- ls("package:gfplot")
sort(fns[grepl("tidy", fns)])
#>  [1] "tidy_age_precision"    "tidy_ages_raw"         "tidy_ages_weighted"   
#>  [4] "tidy_catch"            "tidy_comps"            "tidy_comps_commercial"
#>  [7] "tidy_comps_survey"     "tidy_cpue_historical"  "tidy_cpue_index"      
#> [10] "tidy_lengths_raw"      "tidy_lengths_weighted" "tidy_maturity_months" 
#> [13] "tidy_sample_avail"     "tidy_survey_index"     "tidy_survey_sets"
```

``` r
sort(fns[grepl("fit", fns)])
#> [1] "fit_cpue_index_glmmtmb" "fit_length_weight"      "fit_mat_ogive"         
#> [4] "fit_survey_sets"        "fit_vb"
```

``` r
sort(fns[grepl("plot", fns)])
#>  [1] "plot_age_precision"     "plot_ages"              "plot_catch"            
#>  [4] "plot_catch_spatial"     "plot_cpue_spatial"      "plot_growth"           
#>  [7] "plot_length_weight"     "plot_lengths"           "plot_mat_annual_ogives"
#> [10] "plot_mat_ogive"         "plot_maturity_months"   "plot_predictor_bubbles"
#> [13] "plot_qres_histogram"    "plot_qres_qq"           "plot_sample_avail"     
#> [16] "plot_survey_index"      "plot_survey_sets"       "plot_vb"
```
