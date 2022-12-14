# This repository includes the Code for the Master Thesis (MDM Mannheim) "Variable Selection using Dynamic Elastic Net - An approach for 'Occupational Health and Safety' Compliance in the Garment Sector"
## The code is provided by the authors: Malik Hebbat, Benjamin Schumacher, Pinar Ucar, Fabio Thoma. 

## R-Skripts

There is only one R-script that can be used. The script include all data loadings, manipulations and modelling.
The script was tested on 14/12/2022 with the following configurations:

R version 4.2.1 (2022-06-23)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS Ventura 13.0.1

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] grid      stats4    stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] directlabels_2021.1.13 scales_1.2.0           hts_6.0.2              forecast_8.17.0       
 [5] vip_0.3.2              tempdisagg_1.0         plyr_1.8.7             imputeTS_3.3          
 [9] rnaturalearth_0.1.0    Rilostat_1.1.8         wbstats_1.0.4          countrycode_1.4.0     
[13] haven_2.5.1            DT_0.25                rvest_1.0.3            missForest_1.5        
[17] ggrepel_0.9.1          semPlot_1.1.6          lavaan_0.6-12          corrplot_0.92         
[21] stargazer_5.2.3        progress_1.2.2         xtable_1.8-4           vars_1.5-6            
[25] lmtest_0.9-40          urca_1.3-3             strucchange_1.5-3      sandwich_3.0-2        
[29] MASS_7.3-57            reshape2_1.4.4         pheatmap_1.0.12        ranger_0.14.1         
[33] glmnet_4.1-4           gtrendsR_1.5.1         yuima_1.15.15          mvtnorm_1.1-3         
[37] cubature_2.0.4.5       expm_0.999-6           data.table_1.14.2      quantmod_0.4.20       
[41] TTR_0.24.3             xts_0.12.1             zoo_1.8-11             fasstr_0.5.0          
[45] plm_2.6-2              mice_3.14.0            Hmisc_4.7-1            Formula_1.2-4         
[49] survival_3.3-1         lattice_0.20-45        readxl_1.4.0           lubridate_1.8.0       
[53] forcats_0.5.2          stringr_1.4.0          dplyr_1.0.9            purrr_0.3.4           
[57] readr_2.1.2            tidyr_1.2.1            tibble_3.1.8           ggplot2_3.3.6         
[61] tidyverse_1.3.2        Matrix_1.5-1          

loaded via a namespace (and not attached):
  [1] SparseM_1.81         GGally_2.1.2         ModelMetrics_1.2.2.2 maxLik_1.5-2         coda_0.19-4         
  [6] ragg_1.2.2           bit64_4.0.5          knitr_1.40           rpart_4.1.16         hardhat_1.2.0       
 [11] RCurl_1.98-1.9       generics_0.1.3       fixest_0.10.4        proxy_0.4-27         future_1.28.0       
 [16] bit_4.0.4            tzdb_0.3.0           xml2_1.3.3           assertthat_0.2.1     gargle_1.2.1        
 [21] gower_1.0.0          xfun_0.32            jquerylib_0.1.4      hms_1.1.1            evaluate_0.16       
 [26] fansi_1.0.3          dbplyr_2.2.1         igraph_1.3.5         DBI_1.1.3            htmlwidgets_1.5.4   
 [31] reshape_0.8.9        lares_5.1.4          googledrive_2.0.0    ellipsis_0.3.2       backports_1.4.1     
 [36] pbivnorm_0.6.0       RcppParallel_5.1.5   deldir_1.0-6         vctrs_0.4.1          abind_1.4-5         
 [41] cachem_1.0.6         caret_6.0-93         withr_2.5.0          itertools_0.1-3      collapse_1.8.8      
 [46] vroom_1.5.7          bdsmatrix_1.3-6      checkmate_2.1.0      fdrtool_1.2.17       prettyunits_1.1.1   
 [51] mnormt_2.1.1         cluster_2.1.3        mi_1.1               crayon_1.5.1         labeling_0.4.2      
 [56] units_0.8-0          recipes_1.0.1        pkgconfig_2.0.3      nlme_3.1-157         nnet_7.3-17         
 [61] rlang_1.0.4          globals_0.16.1       lifecycle_1.0.1      stinepack_1.4        h2o_3.38.0.1        
 [66] kutils_1.70          modelr_0.1.9         cellranger_1.1.0     randomForest_4.7-1.1 rngtools_1.5.2      
 [71] carData_3.0-5        boot_1.3-28          reprex_2.0.2         base64enc_0.1-3      googlesheets4_1.0.1 
 [76] png_0.1-7            bitops_1.0-7         KernSmooth_2.23-20   pROC_1.18.0          doRNG_1.8.2         
 [81] shape_1.4.6          classInt_0.4-8       arm_1.13-1           parallelly_1.32.1    jpeg_0.1-9          
 [86] rockchalk_1.8.157    calculus_1.0.0       magrittr_2.0.3       compiler_4.2.1       miscTools_0.6-26    
 [91] RColorBrewer_1.1-3   lme4_1.1-30          cli_3.3.0            listenv_0.8.0        patchwork_1.1.2     
 [96] pbapply_1.5-0        htmlTable_2.4.1      tidyselect_1.1.2     stringi_1.7.8        lisrelToR_0.1.5     
[101] textshaping_0.3.6    sem_3.1-15           tseries_0.10-51      yaml_2.3.5           OpenMx_2.20.7       
[106] latticeExtra_0.6-30  sass_0.4.2           tools_4.2.1          future.apply_1.9.1   parallel_4.2.1      
[111] rstudioapi_0.14      foreach_1.5.2        foreign_0.8-82       gridExtra_2.3        prodlim_2019.11.13  
[116] farver_2.1.1         rpart.plot_3.1.1     digest_0.6.29        ggtext_0.1.2         lava_1.6.10         
[121] quadprog_1.5-8       gridtext_0.1.5       Rcpp_1.0.9           broom_1.0.1          httr_1.4.4          
[126] sf_1.0-8             psych_2.2.9          Rdpack_2.4           colorspace_2.0-3     XML_3.99-0.10       
[131] fs_1.5.2             splines_4.2.1        sp_1.5-0             systemfonts_1.0.4    dreamerr_1.2.3      
[136] jsonlite_1.8.0       nloptr_2.0.3         corpcor_1.6.10       timeDate_4021.104    glasso_1.11         
[141] ipred_0.9-13         R6_2.5.1             pillar_1.8.0         htmltools_0.5.3      glue_1.6.2          
[146] fastmap_1.1.0        minqa_1.2.4          class_7.3-20         codetools_0.2-18     utf8_1.2.2          
[151] bslib_0.4.0          numDeriv_2016.8-1.1  curl_4.3.2           gtools_3.9.3         zip_2.2.0           
[156] openxlsx_4.2.5       interp_1.1-3         rmarkdown_2.16       qgraph_1.9.2         munsell_0.5.0       
[161] e1071_1.7-11         iterators_1.0.14     fracdiff_1.5-1       gtable_0.3.0         rbibutils_2.2.9 


## Data 

There is no data stored in the github repository as data protection does not allow the publishing. 
The data is cited in the paper. For use of the scripts with data we recommend to directly contact the reference institutions.

## plots and tables

There are also no plots and tables stored here for publishing as data protection does not allow for publishing. 
All plots and tables will be produced in the R-script file and can be stored separately.






