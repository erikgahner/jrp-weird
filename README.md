# Just as WEIRD?
---

### Description and data sources

Replication material for 'Just as WEIRD? Personality Traits and Political Attitudes Among Immigrant Minorities' published in the Journal of Research in Personality. This repository contains all files required to produce the figures, tables and numerical information provided in the manuscript and supplementary material.

### Repository content

- `01_create-data.R` = R script used to create the datasets used for the analysis (requires original data)
- `02_analysis.R` = R script used for all analyses in the article and supplementary material
- `immWEIRD.csv` = Data from the LISS-I panel (generated via `01_create-data.R`)
- `sessionInfo.txt` = Output from sessionInfo() in R

### Session info

The analyses were made with [RStudio](http://www.rstudio.com/) (Version 1.1.463) with the following R session:

```
## R version 3.5.2 (2018-12-20)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS  10.15.3

## Matrix products: default
## BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib

## locale:
## [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8

## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     

## other attached packages:
##  [1] psych_1.9.12.31  haven_2.2.0      conflicted_1.0.4 stargazer_5.2.2  magrittr_1.5     rstatix_0.4.0    forcats_0.5.0   
##  [8] stringr_1.4.0    dplyr_0.8.5      purrr_0.3.3      readr_1.3.1      tidyr_1.0.2      tibble_2.1.3     ggplot2_3.3.0   
## [15] tidyverse_1.3.0

## loaded via a namespace (and not attached):
##  [1] tidyselect_1.0.0   lattice_0.20-40    carData_3.0-3      colorspace_1.4-1   vctrs_0.2.3        generics_0.0.2    
##  [7] yaml_2.2.1         rlang_0.4.5        pillar_1.4.3       foreign_0.8-76     glue_1.3.1         withr_2.1.2       
## [13] DBI_1.1.0          RColorBrewer_1.1-2 dbplyr_1.4.2       modelr_0.1.6       readxl_1.3.1       lifecycle_0.2.0   
## [19] munsell_0.5.0      gtable_0.3.0       cellranger_1.1.0   rvest_0.3.5        zip_2.0.4          memoise_1.1.0     
## [25] labeling_0.3       rio_0.5.16         parallel_3.5.2     curl_4.3           fansi_0.4.1        broom_0.5.5       
## [31] Rcpp_1.0.3         scales_1.1.0       backports_1.1.5    jsonlite_1.6.1     abind_1.4-5        farver_2.0.3      
## [37] fs_1.3.2           mnormt_1.5-6       digest_0.6.25      hms_0.5.3          stringi_1.4.6      openxlsx_4.1.4    
## [43] grid_3.5.2         cli_2.0.2          tools_3.5.2        crayon_1.3.4       car_3.0-6          pkgconfig_2.0.3   
## [49] data.table_1.12.8  xml2_1.2.2         reprex_0.3.0       lubridate_1.7.4    assertthat_0.2.1   httr_1.4.1        
## [55] rstudioapi_0.11    R6_2.4.1           nlme_3.1-145       compiler_3.5.2    



```
