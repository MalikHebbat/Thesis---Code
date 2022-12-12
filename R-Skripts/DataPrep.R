#Authors: Malik Hebbat, Benjamin Schumacher, Fabio Thoma,  Pinar Ucar
#The script loads the raw data, does manipulations, modelling and plotting of paper described methods 

#The script was tested on 12th of december 2022 with the following configuration

# R version 4.2.1 (2022-06-23)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Ventura 13.0.1
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] stats4    stats     graphics  grDevices utils     datasets  methods  
# [8] base     
# 
# other attached packages:
#   [1] vip_0.3.2           tempdisagg_1.0      plyr_1.8.7         
# [4] imputeTS_3.3        rnaturalearth_0.1.0 Rilostat_1.1.8     
# [7] wbstats_1.0.4       countrycode_1.4.0   haven_2.5.1        
# [10] DT_0.25             rvest_1.0.3         missForest_1.5     
# [13] ggrepel_0.9.1       semPlot_1.1.6       lavaan_0.6-12      
# [16] corrplot_0.92       stargazer_5.2.3     progress_1.2.2     
# [19] xtable_1.8-4        vars_1.5-6          lmtest_0.9-40      
# [22] urca_1.3-3          strucchange_1.5-3   sandwich_3.0-2     
# [25] MASS_7.3-57         reshape2_1.4.4      pheatmap_1.0.12    
# [28] ranger_0.14.1       glmnet_4.1-4        gtrendsR_1.5.1     
# [31] yuima_1.15.15       mvtnorm_1.1-3       cubature_2.0.4.5   
# [34] expm_0.999-6        data.table_1.14.2   quantmod_0.4.20    
# [37] TTR_0.24.3          xts_0.12.1          zoo_1.8-11         
# [40] fasstr_0.5.0        plm_2.6-2           mice_3.14.0        
# [43] Hmisc_4.7-1         Formula_1.2-4       survival_3.3-1     
# [46] lattice_0.20-45     readxl_1.4.0        lubridate_1.8.0    
# [49] forcats_0.5.2       stringr_1.4.0       dplyr_1.0.9        
# [52] purrr_0.3.4         readr_2.1.2         tidyr_1.2.1        
# [55] tibble_3.1.8        ggplot2_3.3.6       tidyverse_1.3.2    
# [58] Matrix_1.5-1       
# 
# loaded via a namespace (and not attached):
#   [1] GGally_2.1.2         ModelMetrics_1.2.2.2 maxLik_1.5-2        
# [4] coda_0.19-4          bit64_4.0.5          knitr_1.40          
# [7] rpart_4.1.16         hardhat_1.2.0        RCurl_1.98-1.9      
# [10] generics_0.1.3       fixest_0.10.4        proxy_0.4-27        
# [13] future_1.28.0        bit_4.0.4            tzdb_0.3.0          
# [16] xml2_1.3.3           assertthat_0.2.1     gargle_1.2.1        
# [19] gower_1.0.0          xfun_0.32            hms_1.1.1           
# [22] evaluate_0.16        fansi_1.0.3          dbplyr_2.2.1        
# [25] igraph_1.3.5         DBI_1.1.3            htmlwidgets_1.5.4   
# [28] reshape_0.8.9        googledrive_2.0.0    ellipsis_0.3.2      
# [31] backports_1.4.1      pbivnorm_0.6.0       RcppParallel_5.1.5  
# [34] deldir_1.0-6         vctrs_0.4.1          abind_1.4-5         
# [37] caret_6.0-93         withr_2.5.0          itertools_0.1-3     
# [40] collapse_1.8.8       vroom_1.5.7          bdsmatrix_1.3-6     
# [43] checkmate_2.1.0      fdrtool_1.2.17       prettyunits_1.1.1   
# [46] mnormt_2.1.1         cluster_2.1.3        mi_1.1              
# [49] crayon_1.5.1         units_0.8-0          recipes_1.0.1       
# [52] pkgconfig_2.0.3      nlme_3.1-157         nnet_7.3-17         
# [55] rlang_1.0.4          globals_0.16.1       lifecycle_1.0.1     
# [58] stinepack_1.4        kutils_1.70          modelr_0.1.9        
# [61] cellranger_1.1.0     randomForest_4.7-1.1 rngtools_1.5.2      
# [64] carData_3.0-5        boot_1.3-28          reprex_2.0.2        
# [67] base64enc_0.1-3      googlesheets4_1.0.1  png_0.1-7           
# [70] bitops_1.0-7         KernSmooth_2.23-20   pROC_1.18.0         
# [73] doRNG_1.8.2          shape_1.4.6          classInt_0.4-8      
# [76] arm_1.13-1           parallelly_1.32.1    jpeg_0.1-9          
# [79] rockchalk_1.8.157    calculus_1.0.0       scales_1.2.0        
# [82] magrittr_2.0.3       compiler_4.2.1       miscTools_0.6-26    
# [85] RColorBrewer_1.1-3   lme4_1.1-30          cli_3.3.0           
# [88] listenv_0.8.0        pbapply_1.5-0        htmlTable_2.4.1     
# [91] tidyselect_1.1.2     stringi_1.7.8        lisrelToR_0.1.5     
# [94] sem_3.1-15           tseries_0.10-51      yaml_2.3.5          
# [97] OpenMx_2.20.7        latticeExtra_0.6-30  grid_4.2.1          
# [100] tools_4.2.1          future.apply_1.9.1   parallel_4.2.1      
# [103] rstudioapi_0.14      foreach_1.5.2        foreign_0.8-82      
# [106] gridExtra_2.3        prodlim_2019.11.13   digest_0.6.29       
# [109] ggtext_0.1.2         lava_1.6.10          quadprog_1.5-8      
# [112] gridtext_0.1.5       Rcpp_1.0.9           broom_1.0.1         
# [115] httr_1.4.4           sf_1.0-8             psych_2.2.9         
# [118] Rdpack_2.4           colorspace_2.0-3     XML_3.99-0.10       
# [121] fs_1.5.2             splines_4.2.1        sp_1.5-0            
# [124] dreamerr_1.2.3       jsonlite_1.8.0       nloptr_2.0.3        
# [127] corpcor_1.6.10       timeDate_4021.104    glasso_1.11         
# [130] ipred_0.9-13         R6_2.5.1             pillar_1.8.0        
# [133] htmltools_0.5.3      glue_1.6.2           fastmap_1.1.0       
# [136] minqa_1.2.4          class_7.3-20         codetools_0.2-18    
# [139] utf8_1.2.2           numDeriv_2016.8-1.1  curl_4.3.2          
# [142] gtools_3.9.3         forecast_8.17.0      zip_2.2.0           
# [145] openxlsx_4.2.5       interp_1.1-3         rmarkdown_2.16      
# [148] qgraph_1.9.2         munsell_0.5.0        e1071_1.7-11        
# [151] iterators_1.0.14     fracdiff_1.5-1       gtable_0.3.0        
# [154] rbibutils_2.2.9     


###load packages
library(fasstr) #fasstr: Analyze, Summarize, and Visualize Daily Streamflow Data ##used to add season variable
library(tidyverse)
library(lubridate)
library(readxl)
library(Hmisc)
library(xts)
library(quantmod)
library(data.table)
library(yuima)
library(gtrendsR)
library(glmnet)
library(ranger)    
library(pheatmap)  
library(ggplot2)   
library(reshape2)  
library(vars)     
library(dplyr)
library(xtable)      
library(progress)    
library(stargazer)   
library(corrplot)    
library(lavaan)      
library(semPlot)     
library(ggrepel) 
library(missForest)
library(readr)
library(fasstr)
library(tidyverse)
library(rvest)
library(readxl)
library(ggplot2)
library(DT)
library(haven)
library(dplyr)
library(countrycode)
library(wbstats)
library(Rilostat)
library(rnaturalearth)
library(imputeTS) 
library(plyr)
require(tempdisagg)
require(lubridate)
library(vip)

## 1. Load the data and make  additional date variables

load("/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Data Sets/All Better Work Assessments through 31 May 2022.RData")

data <- all_bw_assessments_through_31_may_2022_with_further_detail
rm(all_bw_assessments_through_31_may_2022_with_further_detail)


data$date_final <- as.Date(data$Assesment.Start.Date, "%d/%m/%Y")
data$Year <- as.numeric(format(data$date_final, "%Y"))
data$Month <- as.numeric(format(data$date_final, "%m"))

###add seasons from date variable
data <- data%>%
  add_seasons(dates="date_final",seasons_length = 3,water_year_start = 3)

### make a season variable from it
data$season_clim <- ifelse(data$Season=="Dec-Feb","Winter",
                           ifelse(data$Season=="Mar-May","Spring",
                                  ifelse(data$Season=="Jun-Aug","Summer",
                                         ifelse(data$Season=="Sep-Nov","Autumn",""))))

### make a season variable with numbers for order purpose
data$season_num<-factor(ifelse(data$Season=="Dec-Feb",4,
                               ifelse(data$Season=="Mar-May",1,
                                      ifelse(data$Season=="Jun-Aug",2,
                                             ifelse(data$Season=="Sep-Nov",3,"")))),ordered = T)

###final season variable as timeseries date variable
data$seasnum_year <- paste(data$season_clim,data$Year,sep=" ")
data$seasnum_year <- paste(data$season_num,data$Year,sep="/")
data$seasnum_year <- zoo::as.yearqtr(data$seasnum_year, format = "%q/%Y") 

##2. clean string variables of Questions and read in CAT Document

data$Q.Label <- gsub("\r\n","",data$Q.Label)


cat_document <- read_excel('/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/cycle-trend-panel-creation-main/2022-02-01_All CAT questions with with Global Identifiers and all Tags.xlsx')%>%
  group_by(country_name, cluster_name, compliance_point_name,first_original_question_id,question_id,
           text,finding,question_type,global_question_mapping_id,global_text,global_finding)%>%
  summarise(n=n())%>%
  select(-n)


##3. Look for interesting keywords in FGW Questions 

###a) Uninions/Bargaining
#####Looking for keywords in cat_document
unique(cat_document$text[str_detect(cat_document$text ,"refuse to bargain")])


#####Filter the questions referring to keywords
#####Employer refuse to bargain
refBarg<- data%>%
  select(Country,seasnum_year,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,seasnum_year)%>%
  filter(Q.Label== "Does the employer refuse to bargain collectively or refuse to bargain in good faith with the union, worker representatives, union federations or confederations?"   |                                  
           Q.Label==  "Does the employer refuse to bargain collectively or refuse to bargain in good faith with the union, shop stewards, union federations or confederations?"       |                                       
           Q.Label==  "Does the employer refuse to bargain collectively or refuse to bargain in good faith with the union or worker representatives?"     |                                                                   
           Q.Label==  "Does the employer refuse to bargain collectively with union federations and confederations?"                       |                                                                                   
           Q.Label==  "Does the employer refuse to bargain collectively or refuse to bargain in good faith with the union(s)?"                            |                                                                   
           Q.Label==  "Does the employer refuse to bargain collectively or refuse to bargain in good faith with the union?"                                       |                                                           
           Q.Label==  "Does the employer refuse to bargain collectively or refuse to bargain in good faith with the union,  worker representatives, union federations or confederations?"    |                                
           Q.Label== "Does the employer refuse to bargain collectively or refuse to bargain in good faith with the union or provisional union?"                                    |                                         
           Q.Label==  "Does the employer refuse to bargain collectively in accordance with legal requirements, or refuse to bargain in good faith with the union or provisional union?"  |                                    
           Q.Label==  "Does the employer refuse to bargain collectively in accordance with legal requirements, or refuse to bargain in good faith with the union, workers representation, union federation or confederation?")%>%
  
  rename("refBarg"=Finding)

#####Looking for keywords in cat_document
unique(cat_document$text[str_detect(cat_document$text ,"freely form and join")])

#####Filter the questions referring to keywords
#####Workers can freely form and join unions
formUnion<- data%>%
  select(Country,seasnum_year,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,seasnum_year)%>%
  filter(Q.Label=="Can workers freely form and join the union of their choice?"    |                       
           Q.Label== "Can workers freely form and join a union?"                |                             
           Q.Label== "Can the union(s) freely form and join federations and confederations of their choice?" |
           Q.Label== "Can workers freely form and join the union of their choice? [differentiation question]"|
           Q.Label== "Can workers freely form and join the union of their choice? *"          |               
           Q.Label== "Can workers freely form and join the union of their choice ?" )%>%
  rename("formUnion"=Finding)

#####
unique(cat_document$text[str_detect(cat_document$text ,"punish workers")])

#####Filter the questions referring to keywords
#####Employer punish workers for joining unions
punUnion<- data%>%
  select(Country,seasnum_year,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,seasnum_year)%>%
  filter(Q.Label=="Does the employer punish workers for joining a union or engaging in union activities?"                           |
           Q.Label== "Does the employer punish workers for joining a union or engaging in union activities? [differentiation question]"|
           Q.Label== "Does the employer punish workers for joining a union or engaging in union activities? *"|                         
           Q.Label== "Does the employer punish workers for joining a union or engaging in union activities ?")%>%
  rename("punUnion"=Finding)

#####
unique(cat_document$text[str_detect(cat_document$text ,"percentage of workers")])

#####Filter the questions referring to keywords
#####percentage of workers in unions
percUnion<- data%>%
  select(Country,seasnum_year,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,seasnum_year)%>%
  filter(Q.Label=="What percentage of workers are union members?")%>%
  rename("percUnion"=Finding)


#####
unique(cat_document$text[str_detect(cat_document$text ,"have access to the workers")])

#####Filter the questions referring to keywords
#####Union members have access to the workers
accessUnion<- data%>%
  select(Country,seasnum_year,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,seasnum_year)%>%
  filter(Q.Label=="Do union representatives have access to the workers in the workplace?")%>%
  rename("AccessUnion"=Finding)

#####
unique(cat_document$text[str_detect(cat_document$text ,"collective bargaining agreements")])

#####Filter the questions referring to keywords
#####Number of collective bargaiing agreements
NummBarg<- data%>%
  select(Country,seasnum_year,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,seasnum_year)%>%
  filter(Q.Label=="How many collective bargaining agreements are in effect in the factory?" )%>%
  rename("NumBarg"=Finding)



###b) Training

#####
unique(cat_document$text[str_detect(cat_document$text ,"training")])

#####Filter the questions referring to keywords
#####Is Gender a factor for access to training
trainGen<- data%>%
  select(Country,seasnum_year,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,seasnum_year)%>%
  filter(Q.Label== "Is the gender of a worker a factor in decisions regarding opportunities for promotion or access to training?"   |                                                                       Q.Label== "Is the gender or marital status of a worker a factor in decisions regarding opportunities for promotion or access to training?")%>%
  rename("trainGen"=Finding)

#####
unique(cat_document$text[str_detect(cat_document$text ,"chemicals")])

#####Filter the questions referring to keywords
#####Training in chemicals
trainChem<- data%>%
  select(Country,seasnum_year,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,seasnum_year)%>%
  filter(Q.Label=="Has the employer effectively trained workers who work with chemicals and hazardous substances?" |
           Q.Label=="Has the employer effectively trained workers and supervisor who work with or are responsible for chemicals and hazardous substances?" |
           Q.Label=="Has the employer effectively trained workers and supervisor who work with or are responsible for hazardous chemicals?"|
           Q.Label== "Has the employer effectively trained workers and supervisors who work with or are responsible for hazardous chemicals?")%>%
  rename("trainChem"=Finding)

#####
unique(cat_document$text[str_detect(cat_document$text ,"first-aid")])

#####Filter the questions referring to keywords
#####First aid training
trainFirst<- data%>%
  select(Country,seasnum_year,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,seasnum_year)%>%
  filter(Q.Label=="Has the employer provided first-aid training for workers?" |
           Q.Label==  "Has the employer trained workers on first aid and formed a first-aid team?" |
           Q.Label==  "Has the employer provided rescue and first-aid training for workers?")%>%
  rename("trainFirst"=Finding)

#####
unique(cat_document$text[str_detect(cat_document$text ,"under age 18")])

#####Filter the questions referring to keywords
#####training under age 18/15
trainu16<- data%>%
  select(Country,seasnum_year,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,seasnum_year)%>%
  filter(Q.Label=="Are all workers who are under age 18 and doing hazardous work (i) at least 16 years old; (ii) working in accordance with Jordanian law; (iii) working in such a way that their health, safety and morals are fully protected; and (iv) adequately trained to do the work safely?"| 
           Q.Label=="Are there any workers under age 18 who are doing hazardous work and who are (i) under 16 years old; (ii) working more than 7 hours/day or 42 hours/week; (vi) working overtime or at night; (iii) working in such a way that their health, safety and morals are not fully protected; or (iv) not adequately trained to do the work safely?"|
           Q.Label=="Are all workers who are under age 15 working (a) in accordance with national regulations regarding light work or (b) in a government-approved training program?" 
  )%>%
  rename("trainu16"=Finding)

#####
unique(cat_document$text[str_detect(cat_document$text ,"training workers")])

#####Filter the questions referring to keywords
#####Number of training workers
trainWorker<- data%>%
  select(Country,seasnum_year,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,seasnum_year)%>%
  filter(Q.Label=="How many training workers are employed by the factory?")%>%
  rename("trainWorker"=Finding)

#####Filter the questions referring to keywords
#####Number of training workers are men
trainMen<- data%>%
  select(Country,seasnum_year,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,seasnum_year)%>%
  filter(Q.Label=="How many of the training workers are men?" 
  )%>%
  rename("trainMen"=Finding)

#####
unique(cat_document$text[str_detect(cat_document$text ,"apprentice")])
#####Filter the questions referring to keywords
#####Number of apprentices
trainAppr<- data%>%
  select(Country,seasnum_year,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,seasnum_year)%>%
  filter(Q.Label=="How many training/apprentice workers are employed by the factory?" |
           Q.Label=="How many apprentices are employed by the factory in production?" |
           Q.Label=="How many apprentices are training in the factory?" |
           Q.Label=="How many apprentices are employed by the factory?")%>%
  rename("trainAppr"=Finding)
#####Filter the questions referring to keywords
#####Number of apprentices are men
trainApprMen<- data%>%
  select(Country,seasnum_year,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,seasnum_year)%>%
  filter(Q.Label=="How many training/apprentice workers are men?" |
           Q.Label=="How many of the apprentices in production are men?"|
           Q.Label=="How many of the apprentices workers are men?" |
           Q.Label=="How many of the apprentices are men?"
  )%>%
  rename("trainApprMen"=Finding)

###c) Minimum wage
#####
unique(cat_document$text[str_detect(cat_document$text ,"minimum wage")])
#####Filter the questions referring to keywords
#####Minimum wage yes/no
minimumWage<- data%>%
  select(Country,seasnum_year,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,seasnum_year)%>%
  filter(Q.Label=="Does the employer pay the correct district minimum wage for ordinary hours of work to permanent full time workers (PKWTT) ?" |
           Q.Label=="Does the employer pay at least minimum wage for ordinary hours of work to regular full time workers?"|
           Q.Label=="Does the employer pay at least the applicable legal minimum wage for ordinary hours of work to regular full time workers?")%>%
  rename("tminWage"=Finding)



#4.) Split the data in the clusters

Cl <- split(data,data$Cluster)

summary(Cl)
#                                                  Length Class  Mode
# Child Labour                                     34     tbl_df list
# Compensation                                     34     tbl_df list
# Contracts and Human Resources                    34     tbl_df list
# Discrimination                                   34     tbl_df list
# Forced Labour                                    34     tbl_df list
# Freedom of Association and Collective Bargaining 34     tbl_df list
# Key Strengths and Process Integrity              34     tbl_df list
# Learning                                         34     tbl_df list
# Occupational Safety and Health                   34     tbl_df list
# Supplier Information                             34     tbl_df list
# Systems                                          34     tbl_df list
# Working Time                                     34     tbl_df list


#5.) Calculate factory_cp_level_nc_rate

##function to calculate over all 12 clusters if anything to calculate 
temp_func <- function(data) {
  ##group by and summary of latest Quest.id
  latest_date_for_each_factory <- data %>%
    group_by( Cluster,Country,Factory.Assessed.ID,Factory.Assessed.Name,seasnum_year)%>%
    summarise(Quest.ID = max(Quest.ID))
  ##join with with raw data
  temp_df <- inner_join(data,latest_date_for_each_factory,
                        by = c('Cluster','Country',"Factory.Assessed.ID",'Factory.Assessed.Name','seasnum_year','Quest.ID'))
  #filter "FGW Questions
  temp_df <- temp_df[temp_df$Question.type != "FGW",]
  #get answers of questions
  temp_df$Finding <- as.integer(temp_df$Finding)
  #calculate non compliance by summing up the findings
  compliance_calc <- temp_df%>%
    group_by(Cluster,Country,Factory.Assessed.ID,Factory.Assessed.Name, seasnum_year,Quest.ID,CP)%>%
    summarise(findings = sum(Finding))
  #binary variable if non compliant for topic or not 0=no,1=yes
  compliance_calc$non_compliant <-ifelse(compliance_calc$findings >0,1,0)
 ###calculation of factory_cp_nc_rate number of non-compliant points/total compliance points
   compliance_df <- compliance_calc%>%
    group_by(Cluster,Country,Factory.Assessed.ID,Factory.Assessed.Name,seasnum_year,CP)%>%
    summarise(total_factories=n(),number_non_compliant= sum(non_compliant), percent_non_cp = sum(non_compliant)/n())
  factory_level_average_nc <- compliance_df%>%
    group_by(Cluster,Country,Factory.Assessed.ID,Factory.Assessed.Name,seasnum_year)%>%
    summarise(total_cp_nc = sum(number_non_compliant), total_cps = n(), factory_cp_level_nc_rate = sum(number_non_compliant)/n())
}

##run the function over all clusters
cps2<- list()
for (i in 1:length(Cl)) {
  cps2[[i]] <-  temp_func(Cl[[i]])
}

##bind the clusters
cps2_all <- bind_rows(cps2)


#6. Load the entreprise report and join with raw data, join FGW questions with original data, clean the data 

##set end period to filter
end_period_2021 <- as.Date("31/12/2021","%d/%m/%Y")

##read in the excel file for entreprise report 
all_factories_ever_in_programme <- read_excel('/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/cycle-trend-panel-creation-main/Enterprise_Report_All_01-02-2022.xlsx', skip = 1)%>%
  rename(Creation..Data = `Creation  Data`)%>%
  rename(Country = Country...11)%>%
  mutate(ID = as.integer(ID))%>%
  filter(Status %in% 'Active')%>%
  filter(as.Date(Creation..Data,"%d/%m/%Y") <= end_period_2021)
all_factories_ever_in_programme <- all_factories_ever_in_programme %>%
  rename("Factory.Assessed.Name"=Name)

##join the assesment data with the enterprise report and arrange by Cluster,Country,seasnum_year,Factory.Assessed.ID
cpsx <- left_join(cps2_all,all_factories_ever_in_programme,by=c('Country',"Factory.Assessed.Name"))

cpsx <- cpsx %>% arrange(Cluster,Country,seasnum_year,Factory.Assessed.ID) 

## join the assesment data and entreprise report with the FGW Questions
cpsx_temp <- cpsx %>%
  
  left_join(.,refBarg[,c("Factory.Assessed.ID","Country","seasnum_year","refBarg")],
            by=c("seasnum_year",'Country',"Factory.Assessed.ID")) %>%
  
  
  left_join(.,formUnion[,c("Country","seasnum_year","Factory.Assessed.ID","formUnion")],
            by=c("seasnum_year",'Country',"Factory.Assessed.ID")) %>%
  
  left_join(.,accessUnion[,c("Country","seasnum_year","Factory.Assessed.ID","AccessUnion")],
            by=c("seasnum_year",'Country',"Factory.Assessed.ID"))%>%
  
  left_join(.,NummBarg[,c("Country","seasnum_year","Factory.Assessed.ID","NumBarg")],
            by=c("seasnum_year",'Country',"Factory.Assessed.ID"))%>%
  
  left_join(.,minimumWage[,c("Country","seasnum_year","Factory.Assessed.ID","tminWage")],
            by=c("seasnum_year",'Country',"Factory.Assessed.ID"))%>%
  
  
  
  left_join(.,punUnion[,c("Country","seasnum_year","Factory.Assessed.ID","punUnion")],
            by=c("seasnum_year",'Country',"Factory.Assessed.ID"))%>%
  
  left_join(.,percUnion[,c("Country","seasnum_year","Factory.Assessed.ID","percUnion")],
            by=c("seasnum_year",'Country',"Factory.Assessed.ID"))%>%
  
  left_join(.,trainAppr[,c("Country","seasnum_year","Factory.Assessed.ID","trainAppr")],
            by=c("seasnum_year",'Country',"Factory.Assessed.ID"))%>%
  
  left_join(.,trainApprMen[,c("Country","seasnum_year","Factory.Assessed.ID","trainApprMen")],
            by=c("seasnum_year",'Country',"Factory.Assessed.ID"))%>%
  
  left_join(.,trainChem[,c("Country","seasnum_year","Factory.Assessed.ID","trainChem")],
            by=c("seasnum_year",'Country',"Factory.Assessed.ID"))%>%
  
  left_join(.,trainFirst[,c("Country","seasnum_year","Factory.Assessed.ID","trainFirst")],
            by=c("seasnum_year",'Country',"Factory.Assessed.ID"))%>%
  
  left_join(.,trainGen[,c("Country","seasnum_year","Factory.Assessed.ID","trainGen")],
            by=c("seasnum_year",'Country',"Factory.Assessed.ID"))%>%
  
  
  left_join(.,trainMen[,c("Country","seasnum_year","Factory.Assessed.ID","trainMen")],
            by=c("seasnum_year",'Country',"Factory.Assessed.ID"))%>%
  
  
  left_join(.,trainu16[,c("Country","seasnum_year","Factory.Assessed.ID","trainu16")],
            by=c("seasnum_year",'Country',"Factory.Assessed.ID"))%>%
  
  left_join(.,trainWorker[,c("Country","seasnum_year","Factory.Assessed.ID","trainWorker")],
            by=c("seasnum_year",'Country',"Factory.Assessed.ID"))


cpsx <- cpsx_temp[!is.na(cpsx_temp$ID),]

## remove unneccessary variables and double entries (we want to have one line per firm per per seasnum year)

rem <- c("Original Factory ID",
         "Address 1",
         "Address 2...7",
         "City...8",
         "State",
         "Contact Name...12",
         "Office Telephone Number",
         "Fax Number",
         "Email",
         "GPS Coordinates X",
         "GPS Coordinates Y",
         "Full legal name of the enterprise that owns the supplier",
         "Address",
         "Address 2...21",
         "City...22",
         "Country...23",
         "Contact Name...24",
         "Second Product",
         "Customer 1 - Name",
         "Customer 1 - Length of Business Relationship",
         "Customer 2 - Name",
         "Customer 2 - Length of Business Relationship",
         "Customer 2 - Preferred Supplier",
         "Customer 2 - Contractor",
         "Customer 2 - Subcontractor",
         "Customer 3 - Name",
         "Customer 3 - Length of Business Relationship",
         "Customer 3 - Preferred Supplier",
         "Customer 3 - Contractor",
         "Customer 3 - Subcontractor",
         "Obsoletion Reason",
         "Assigned Advisor")
cpsxc <- cpsx[!(names(cpsx) %in% rem)]
cpsxc <- cpsxc %>%
  group_by(Cluster,Factory.Assessed.ID) %>%
  slice(which(!duplicated(seasnum_year)))


##Editing of FGW variables, parsing numbers and creating new vars

###parse number from text
cpsxc$NumBarg <- readr::parse_number(cpsxc$NumBarg,na=character())
cpsxc$percUnion <-  readr::parse_number(cpsxc$percUnion)/100
##sometimes no good parse thus again dividing throug 10 to get right percentage
cpsxc$percUnion <-  ifelse(cpsxc$percUnion>1,cpsxc$percUnion/10,cpsxc$percUnion)


###calculating firm age 
cpsxc$Year <- parse_number(as.character(cpsxc$seasnum_year))
cpsxc$firm_age <- ifelse(as.numeric(cpsxc$`In what year did this supplier begin operations in this country?`)>0,
                         cpsxc$Year-as.numeric(cpsxc$`In what year did this supplier begin operations in this country?`),
                         1)
cpsxc$`In what year did this supplier begin operations in this country?` <- NULL


### rename size variables

cpsxc <- cpsxc%>%
  rename("firm_size"=Total)%>%
  mutate(firm_size=as.numeric(firm_size),
         Female=as.numeric(Female),
         Male=as.numeric(Male))

###calculating female Ratio in firm
cpsxc$femRatio <- (cpsxc$Female/cpsxc$firm_size)

###binary variables 0,1
cpsxc$prefSup <- ifelse(cpsxc$`Customer 1 - Preferred Supplier`=="Yes",1,0)
cpsxc$contr <- ifelse(cpsxc$`Customer 1 - Contractor`=="Yes",1,0)
cpsxc$subContr <- ifelse(cpsxc$`Customer 1 - Subcontractor`=="Yes",1,0)

##clean the data
cpsxc$`Customer 1 - Preferred Supplier` <- cpsxc$`Customer 1 - Contractor` <-cpsxc$`Customer 1 - Subcontractor` <- NULL 


cpsxc$Creation..Data <- as.Date(cpsxc$Creation..Data,"%d/%m/%Y")
cpsxc$lenPart <- cpsxc$Year-lubridate::year(cpsxc$Creation..Data) 

##make readable names and mutate the data to prefered data class
names(cpsxc) <- make.names(names(cpsxc))
cpsxc <- cpsxc%>%
  mutate(
    
    formUnion=as.factor(formUnion),
    refBarg=as.factor(refBarg),
    AccessUnion=as.factor(AccessUnion),
    punUnion=as.factor(punUnion),
    trainFirst=as.factor(trainFirst),
    trainChem=as.factor(trainChem),
    trainGen=as.factor(trainGen),
    trainu16=as.factor(trainu16),
    trainAppr=as.numeric(trainAppr),
    trainApprMen=as.numeric(trainApprMen),
    trainMen=as.numeric(trainMen),
    tminWage=as.numeric(tminWage),
    Peer.Grouping.Enabled = as.factor(ifelse(Peer.Grouping.Enabled=="No",1,0)),
    Cluster = as.factor(Cluster),
    Factory.Assessed.ID = as.numeric(Factory.Assessed.ID),
    factory_cp_level_nc_rate = as.numeric(factory_cp_level_nc_rate),
    total_cp_nc = as.numeric(total_cp_nc),
    total_cps = as.numeric(total_cps),
    trainWorker =as.numeric(trainWorker)
  )

##clean the data 
cpsxc$Creation..Data <- NULL
cpsxc$Country.Code..Alpha.2. <- NULL
cpsxc$Year <- NULL
cpsxc$Zip.Code <- NULL


##split new data again
CL <- split(cpsxc,cpsxc$Cluster)


##remove unneccessary data from environment

rm(list=ls()[-c(5,11)])


#7. imputation and aggregation of OSH Cluster

##Build dataframe only for "Occupational Safety an Health"OSH Cluster OSH Cluster and clean it

OSH <- CL$`Occupational Safety and Health`

OSH$Country <- as.factor(OSH$Country)
OSH$Factory.Assessed.Name <- NULL
OSH$Section <- NULL
OSH$Division <- NULL
OSH$First.Product <- NULL
OSH$Datasets <- NULL
OSH$Status <- NULL
OSH$Latest.Cycle <- NULL
OSH$Cycle.Start.Date <- NULL
OSH$Continent <- NULL
OSH$ISO <- NULL


###building additional seasonal variables for ordering
OSH$Season <- NA
OSH$Season[grepl("Q1", OSH$seasnum_year, fixed = TRUE)] <- 1
OSH$Season[grepl("Q2", OSH$seasnum_year, fixed = TRUE)] <- 2
OSH$Season[grepl("Q3", OSH$seasnum_year, fixed = TRUE)] <- 3
OSH$Season[grepl("Q4", OSH$seasnum_year, fixed = TRUE)] <- 4

OSH$Season <- factor(OSH$Season,labels = c("Spring","Summer","Autmn","Winter"),ordered=T)

OSH$Season_year <- factor(paste(readr::parse_number(as.character(OSH$seasnum_year)),OSH$Season,sep="-"))


##missForest (random Forest imputation), setup iterations=5, trees per Forest=4000 ###takes a few hours
set.seed(500)
OSH$seasnum_year <- factor(OSH$seasnum_year,ordered = T)
OSH_imp <- missForest(as.data.frame(OSH),maxiter=5,ntree=4000,verbose=T,replace=T)$ximp


## aggreagtion (mean over Quarter)
###numeric columns aggregation
num_cols <- OSH_imp[,unlist(lapply(OSH_imp,is.numeric))]
num_cols$Country <- OSH_imp$Country
num_cols$seasnum_year <- OSH_imp$seasnum_year

num_cols_aggr <- aggregate(num_cols,by=list(num_cols$Country,num_cols$seasnum_year),mean,na.rm=T)

###binary columns aggregation
bin_cols <- OSH_imp[,unlist(lapply(OSH_imp,is.factor))]
bin_cols$Country <- bin_cols$Cluster <- bin_cols$season_year <- NULL
bin_cols<- as.data.frame(bind_rows(lapply(bin_cols,
                                          function(x) {as.numeric(as.character(x))})))

bin_cols$seasnum_year<- as.factor(OSH_imp$seasnum_year)
bin_cols$Country<- as.factor(OSH_imp$Country)
bin_cols_aggr <- aggregate(bin_cols,by=list(bin_cols$Country,bin_cols$seasnum_year),mean,na.rm=T)

##join aggregated data by country and date
temp <- inner_join(num_cols_aggr,bin_cols_aggr,by=c("Group.1","Group.2"))

temp
temp <- temp%>%
  dplyr::rename(date=Group.2,
                Country=Group.1)%>%
  mutate(date=as.yearqtr(as.character(date)))%>%
  arrange(Country,date)


#8. load and clean, edit, arrange supplementary data  (ViEWS,ILO)

## ViEWS
###load and clean the data
temp1 <- read_csv("/Users/malik/Downloads/views_pred_comp_data_20200324/cm.csv")
viet <- na.omit(temp1[temp1$name=="Vietnam",])
###Creating seasonal variable and aggreagte for vietnam
viet$date <- paste(viet$year,viet$month,sep="-")
viet$date <- as.Date(as.yearmon(viet$date))
viet <- viet %>% add_seasons(dates="date",seasons_length = 3,water_year_start = 3)

viet$Season
viet$season_clim <- ifelse(viet$Season=="Dec-Feb","Winter",
                           ifelse(viet$Season=="Mar-May","Spring",
                                  ifelse(viet$Season=="Jun-Aug","Summer",
                                         ifelse(viet$Season=="Sep-Nov","Autumn",""))))


viet$season_num<-factor(ifelse(viet$Season=="Dec-Feb",4,
                               ifelse(viet$Season=="Mar-May",1,
                                      ifelse(viet$Season=="Jun-Aug",2,
                                             ifelse(viet$Season=="Sep-Nov",3,"")))),ordered = T)

viet$seasnum_year <- paste(viet$season_clim,viet$year,sep=" ")
viet$seasnum_year <- paste(viet$season_num,viet$year,sep="/")
viet$seasnum_year <- zoo::as.yearqtr(viet$seasnum_year, format = "%q/%Y")

viet_aggr <- aggregate(viet,by=list(viet$year,viet$seasnum_year),mean)
viet_aggr <- viet_aggr[viet_aggr$year>=2010,]
viet_aggr$Country <- "Vietnam"


###Creating seasonal variable for indonesia
indo <- na.omit(temp1[temp1$name=="Indonesia",])
indo$date <- paste(indo$year,indo$month,sep="-")
indo$date <- as.Date(as.yearmon(indo$date))
indo <- indo %>% add_seasons(dates="date",seasons_length = 3,water_year_start = 3)

indo$Season
indo$season_clim <- ifelse(indo$Season=="Dec-Feb","Winter",
                           ifelse(indo$Season=="Mar-May","Spring",
                                  ifelse(indo$Season=="Jun-Aug","Summer",
                                         ifelse(indo$Season=="Sep-Nov","Autumn",""))))


indo$season_num<-factor(ifelse(indo$Season=="Dec-Feb",4,
                               ifelse(indo$Season=="Mar-May",1,
                                      ifelse(indo$Season=="Jun-Aug",2,
                                             ifelse(indo$Season=="Sep-Nov",3,"")))),ordered = T)

indo$seasnum_year <- paste(indo$season_clim,indo$year,sep=" ")
indo$seasnum_year <- paste(indo$season_num,indo$year,sep="/")
indo$seasnum_year <- zoo::as.yearqtr(indo$seasnum_year, format = "%q/%Y")

indo_aggr <- aggregate(indo,by=list(indo$year,indo$seasnum_year),mean)
indo_aggr <- indo_aggr[indo_aggr$year>=2010,]
indo_aggr$Country <- "Indonesia"

###Creating seasonal variable for Jordan
jord <- na.omit(temp1[temp1$name=="Jordan",])
jord$date <- paste(jord$year,jord$month,sep="-")
jord$date <- as.Date(as.yearmon(jord$date))
jord <- jord %>% add_seasons(dates="date",seasons_length = 3,water_year_start = 3)

jord$Season
jord$season_clim <- ifelse(jord$Season=="Dec-Feb","Winter",
                           ifelse(jord$Season=="Mar-May","Spring",
                                  ifelse(jord$Season=="Jun-Aug","Summer",
                                         ifelse(jord$Season=="Sep-Nov","Autumn",""))))


jord$season_num<-factor(ifelse(jord$Season=="Dec-Feb",4,
                               ifelse(jord$Season=="Mar-May",1,
                                      ifelse(jord$Season=="Jun-Aug",2,
                                             ifelse(jord$Season=="Sep-Nov",3,"")))),ordered = T)

jord$seasnum_year <- paste(jord$season_clim,jord$year,sep=" ")
jord$seasnum_year <- paste(jord$season_num,jord$year,sep="/")
jord$seasnum_year <- zoo::as.yearqtr(jord$seasnum_year, format = "%q/%Y")

jord_aggr <- aggregate(jord,by=list(jord$year,jord$seasnum_year),mean)
jord_aggr <- jord_aggr[jord_aggr$year>=2010,]
jord_aggr$Country <- "Jordan"

###bind the three countries and clean up
add_data <- rbind(viet_aggr,indo_aggr,jord_aggr)
add_data$Group.1 <- add_data$Group.2 <- NULL

### additional date variable and joining with BW-Data
add_data$date <- add_data$seasnum_year
temp_m <- left_join(temp,add_data,by=c("Country","date"))


## ILO data

### Labour data


#### Function
##### General formula to retrieve variables (2021 until 2020)
##### x = ILO variable of concern
##### y = classif1 
##### z = Name of variable
r_stat_funtion <- function(ILO_variable,classif=NULL,name){
  require(Rilostat)
  df <- get_ilostat(ILO_variable, cache = FALSE)
  # Not disaggregated men/women and only 2012 to 2022
  if("sex" %in% colnames(df)){
    df <- df %>% dplyr::filter(sex == "SEX_T") 
  }
  df <- df %>% dplyr::filter(time %in% (2012:2022))
  if("classif1" %in% colnames(df)){
    df <- df %>% dplyr::filter(classif1 == classif)
  }
  df <- df %>% dplyr::select(ref_area,time,obs_value)
  names(df)[3] <- name
  clean_ilostat_cache()
  df
}

#### Working poor
##### SDG indicator 1.1.1 - Working poverty rate (percentage of employed living below US$1.90 PPP) 
##### Starting with the working poor, all persons +15 years old 
Working_poverty <- list()
df <- df_temp <-pred <- temp <-  list()
countries <- c("VNM","IDN","JOR")
for (country in countries) {
  Working_poverty[[country]] <- r_stat_funtion("SDG_0111_SEX_AGE_RT_A","AGE_YTHADULT_YGE15","Working Poverty rate (%)")%>%
    filter(ref_area==country)
  df[[country]] <- ts(Working_poverty[[country]],start=2010)
  df_temp[[country]] <- df[[country]][,3]
  pred[[country]] <- predict(td(df_temp[[country]]~1,method="denton-cholette",conversion = "mean"),na.action=na.omit) #denton-cholette method for interpolation Sax, C. und Steiner, P. (2013). Temporal Disaggregation of Time Series. The R Journal, 5(2), 80-88. https://doi.org/10.32614/RJ-2013-028
  temp[[country]]<- data.frame(Y=as.matrix(pred[[country]]), date=time(pred[[country]]))
  temp[[country]]$date <- as.yearqtr(temp[[country]]$date, format = "%Y.%q")
  names(temp[[country]]) <- c("ILO_Working_poverty","date")
  temp[[country]]$Country=country
}
ILO_Working_poverty <- bind_rows(temp)

##### Minimum wage (2017 PPP $) as variable 
Minimum_wage <- list()
Minimum_wage_1 <- list()
df <- df_temp <-pred <- temp <-  list()
countries <- c("VNM","IDN","JOR")
for (country in countries) {
  Minimum_wage[[country]] <- r_stat_funtion("EAR_4MMN_CUR_NB_A","CUR_TYPE_PPP","Minimum_wage")%>%
    filter(ref_area==country)
  
  Minimum_wage[[country]]$time <- as.numeric(Minimum_wage[[country]]$time)
  
  Minimum_wage_1[[country]] <- data.frame(ref_area=country,time=as.numeric(2010:max(Minimum_wage[[country]]$time)),  "Minimum_wage" = NA)
  
  Minimum_wage[[country]]  <-  Minimum_wage_1[[country]] %>% 
    left_join(Minimum_wage[[country]], by = c("ref_area","time")) %>% 
    mutate(Minimum_wage = coalesce(Minimum_wage.x, Minimum_wage.y)) %>% 
    dplyr::select(-Minimum_wage.x, -Minimum_wage.y)
  
  Minimum_wage[[country]]$Minimum_wage <- na_interpolation(Minimum_wage[[country]]$Minimum_wage)
  
  
  
  df[[country]] <- ts(Minimum_wage[[country]],start=2010)
  
  df_temp[[country]] <- df[[country]][,3]
  pred[[country]] <- predict(td(df_temp[[country]]~1,method="denton-cholette",conversion = "mean"),na.action=na.omit)
  temp[[country]]<- data.frame(Y=as.matrix(pred[[country]]), date=time(pred[[country]]))
  temp[[country]]$date <- as.yearqtr(temp[[country]]$date, format = "%Y.%q")
  names(temp[[country]]) <- c("ILO_Minimum_wage","date")
  temp[[country]]$Country=country
}
ILO_Minimum_wage <- bind_rows(temp)


#####SDG indicator 8.3.1 - Proportion of informal employment in total employment

Informal_empl <- list()
Informal_empl_1 <- list()
df <- df_temp <-pred <- temp <-  list()
countries <- c("VNM","IDN","JOR")
for (country in countries) {
  Informal_empl[[country]] <- r_stat_funtion("SDG_0831_SEX_ECO_RT_A","ECO_AGNAG_TOTAL", "Informal")%>%
    filter(ref_area==country)
  
  
  Informal_empl[[country]]$time <- as.numeric(Informal_empl[[country]]$time)
  
  Informal_empl_1[[country]] <- data.frame(ref_area=country,time=as.numeric(2010:max(Informal_empl[[country]]$time)),  "Informal" = NA)
  
  Informal_empl[[country]]  <-  Informal_empl_1[[country]] %>% 
    left_join(Informal_empl[[country]], by = c("ref_area","time")) %>% 
    mutate(Informal = coalesce(Informal.x, Informal.y)) %>% 
    dplyr::select(-Informal.x, -Informal.y)
  
  Informal_empl[[country]]$Informal <- na_interpolation(Informal_empl[[country]]$Informal)
  
  
  
  
  df[[country]] <- ts(Informal_empl[[country]],start=2010)
  
  df_temp[[country]] <- df[[country]][,3]
  pred[[country]] <- predict(td(df_temp[[country]]~1,method="denton-cholette",conversion = "mean"),na.action=na.omit)
  temp[[country]]<- data.frame(Y=as.matrix(pred[[country]]), date=time(pred[[country]]))
  temp[[country]]$date <- as.yearqtr(temp[[country]]$date, format = "%Y.%q")
  names(temp[[country]]) <- c("ILO_Informal_empl","date")
  temp[[country]]$Country=country
}

ILO_Informal_empl <- bind_rows(temp)

##### SDG 8.5.2 Unemployment rate 

#x <- get_ilostat("SDG_0852_SEX_AGE_RT_A", cache = FALSE)
# All persons above 15 years old
Unemployment_rate <-  list()
Unemployment_rate_1 <-  list()
df <- df_temp <-pred <- temp <-  list()
countries <- c("VNM","IDN","JOR")
for (country in countries) {
  Unemployment_rate[[country]] <- r_stat_funtion("SDG_0852_SEX_AGE_RT_A","AGE_YTHADULT_YGE15", "Unemployment")%>%
    filter(ref_area==country)
  
  
  
  Unemployment_rate[[country]]$time <- as.numeric(Unemployment_rate[[country]]$time)
  
  Unemployment_rate_1[[country]] <- data.frame(ref_area=country,time=as.numeric(2010:max(Unemployment_rate[[country]]$time)),  "Unemployment" = NA)
  
  Unemployment_rate[[country]]  <-  Unemployment_rate_1[[country]] %>% 
    left_join(Unemployment_rate[[country]], by = c("ref_area","time")) %>% 
    mutate(Unemployment = coalesce(Unemployment.x, Unemployment.y)) %>% 
    dplyr::select(-Unemployment.x, -Unemployment.y)
  
  Unemployment_rate[[country]]$Unemployment <- na_interpolation(Unemployment_rate[[country]]$Unemployment)
  
  
  
  
  
  df[[country]] <- ts(Unemployment_rate[[country]],start=2010)
  
  df_temp[[country]] <- df[[country]][,3]
  pred[[country]] <- predict(td(df_temp[[country]]~1,method="denton-cholette",conversion = "mean"),na.action=na.omit)
  temp[[country]]<- data.frame(Y=as.matrix(pred[[country]]), date=time(pred[[country]]))
  temp[[country]]$date <- as.yearqtr(temp[[country]]$date, format = "%Y.%q")
  names(temp[[country]]) <- c("ILO_Unemployment_rate","date")
  temp[[country]]$Country=country
}

ILO_Unemployment_rate <- bind_rows(temp)



ilo_data <- plyr::join_all(list(ILO_Working_poverty,ILO_Minimum_wage,ILO_Informal_empl,ILO_Unemployment_rate), by=c('Country','date'), type='full')
ilo_data$Country <- gsub("VNM","Vietnam",ilo_data$Country)
ilo_data$Country <- gsub("IDN","Indonesia",ilo_data$Country)
ilo_data$Country <- gsub("JOR","Jordan",ilo_data$Country)

temp_m1 <- left_join(temp_m,ilo_data,by=c("Country","date"))

temp_m1


##9. Modelling the DynENet
######################################################################
# VIETNAM MODEL                                                      #
######################################################################

###Extract the data for Vietnam
OSH_viet <- temp_m1[temp_m1$Country=="Vietnam" & temp_m1$date>="2011 Q2" & temp_m1$date<2020  ,]
OSH_viet$Year <- readr::parse_number(as.character(OSH_viet$date))
OSH_viet <- OSH_viet %>%
  arrange(year,date)

##Delete unneccessary columns
rownames(OSH_viet) <- OSH_viet$date
OSH_viet$Country <-OSH_viet$date <- OSH_viet$Country.x <- OSH_viet$Country.y <- OSH_viet$season_year.y <- OSH_viet$season_year.x <- OSH_viet$Factory.Assessed.ID <- OSH_viet$total_cps <- OSH_viet$total_cp_nc <- OSH_viet$Year <- OSH_viet$ID <- OSH_viet$Male<- OSH_viet$Female<-OSH_viet$Pop<-OSH_viet$HPI <-OSH_viet$HPI_rank<-OSH_viet$seasnum_year.x <- OSH_viet$seasnum_year.y <- OSH_viet$Season <-OSH_viet$Season_year <- OSH_viet$GDP_per_capita <- OSH_viet$lenPart <-OSH_viet$seasnum_year<- OSH_viet$reign_country_month_id <- OSH_viet$month_id <- OSH_viet$month <- OSH_viet$year <- OSH_viet$Season.x <- OSH_viet$Season.y <- OSH_viet$season_clim <- OSH_viet$season_num <- OSH_viet$Life_Exp   <-OSH_viet$Wellbeing<-OSH_viet$Biocapacity <-OSH_viet$Ecological_Footprint<-OSH_viet$Life_Exp<- OSH_viet$fvp_lnpop200 <-OSH_viet$counts_conflict <- OSH_viet$name <-   NULL
#columns_to_remove <- grep("wdi_", names(OSH_viet))
#OSH_viet <-OSH_viet[,-columns_to_remove]


#function to remove all columns containing only zeros
remove_zero_cols <- function(df) {
  rem_vec <- NULL
  for(i in 1:ncol(df)){
    this_sum <- summary(df[,i])
    zero_test <- length(which(this_sum == 0))
    if(zero_test == 6) {
      rem_vec[i] <- names(df)[i]
    }
  }
  features_to_remove <- rem_vec[!is.na(rem_vec)]
  rem_ind <- which(names(df) %in% features_to_remove)
  df <- df[,-rem_ind]
  return(df)
}



OSH_viet <- remove_zero_cols(OSH_viet)

##DyENet with benchmark lasso and ridge
###First round to get best alpha
vdates <- rownames(OSH_viet)
alpha <- seq(0.1,0.9,length=25)
lambda <- seq(1e-08,100,length=2500)
vmsem <- NULL
vmse1 <- NULL
vmsela <- NULL
intercepti <- FALSE
for(ialpha in  alpha){
  #for(ialpha in seq(0,1,length=5)){
  vcf1 <- NULL
  vla1 <- NULL
  vlap <- NULL
  vcfm <- NULL
  vrcf1 <- NULL
  vrcfm <- NULL
  vp1 <- NULL
  vpm <- NULL
  vl1 <- NULL
  vlm <- NULL
  var <- NULL
  ni <- nrow(OSH_viet)
  ###holdout of 20% of data
  ioffset <-0.8*ni
  for(i in ioffset:(ni-1)){
    ###rolling window selector
    rg <- (i-ioffset+1):i
    ###dependent variable 
    y <- OSH_viet$factory_cp_level_nc_rate[rg]
    ##Predictor Matrix 
    x <- as.matrix(OSH_viet[rg,colnames(OSH_viet)!="factory_cp_level_nc_rate"])
    ##Predictor Matrix with one step ahead
    x.new <- as.matrix(OSH_viet[i+1,colnames(OSH_viet)!="factory_cp_level_nc_rate"],nrow=1)
    set.seed(123)
    ### Cross Validation (CV) for LASSO, best lambda
    mod_cv <-  cv.glmnet(x=x, y=y, family="gaussian",
                         intercept = intercepti, alpha=1,lamda=lambda)
    set.seed(123)
    ###Lasso estimation with best lambda
    vla.eq <- glmnet(x=x, y=y, intercept=intercepti, lambda=mod_cv$lambda.1se,alpha=1)
    set.seed(123)
    ### CV for Ridge, best lambda
    mod_cv1 <-  cv.glmnet(x=x, y=y, family="gaussian",
                          intercept = intercepti, alpha=0)
    set.seed(123)
    ###Ridge estimation with best lambda 
    vri.eq <- glmnet(x=x, y=y, family="gaussian", intercept=intercepti, lambda=mod_cv1$lambda.1se,alpha=0)
    ###estimated coefs for Lasso, lambda min and lambda + 1sd
    vla.1se <- coef(vla.eq, s=mod_cv$lambda.1se)
    vla.min <- coef(vla.eq, s=mod_cv$lambda.min)
    ###data.frame with dates for relative importance Lasso
    a1 <- data.frame(date=vdates[i], t(as.matrix(vla.1se)))
    colnames(a1)[2] <- "Intercept"
    vla1 <- rbind(vla1, a1)
    ###prediction model with Lasso
    vlap <- c(vlap, predict(vla.eq, newx = x.new,s = mod_cv$lambda.1se))
    set.seed(123)
    ##CV DynENet find best lambda
    cvi <- cv.glmnet(x=x, y=y, alpha=ialpha, intercept=intercepti, lambda=lambda, grouped=FALSE)
    set.seed(123)
    ##estimate DynENet with best lambda
    vnet <- glmnet(x=x, y=y, alpha=ialpha,  intercept=intercepti, lambda=cvi$lambda.1se)
    ###estimated coefs for Lasso, lambda min and lambda + 1sd DYENET
    vcf.1se <- coef(vnet, s=cvi$lambda.1se)
    vcf.min <- coef(vnet, s=cvi$lambda.min)
    t1 <- data.frame(date=vdates[i], t(as.matrix(vcf.1se)))
    colnames(t1)[2] <- "Intercept"
    t2 <- data.frame(date=vdates[i], t(as.matrix(vcf.min)))
    colnames(t2)[2] <- "Intercept"
    vcf1 <- rbind(vcf1, t1)
    vcfm <- rbind(vcfm, t2)
    ###prediction model with DynENet
    vp1 <- c(vp1, predict(vnet, newx = x.new,s = cvi$lambda.1se))
    vpm <- c(vpm, predict(vnet, newx = x.new,s = cvi$lambda.min))
    vl1 <- c(vl1, cvi$lambda.1se)
    vlm <- c(vlm, cvi$lambda.min)
    
    ###Selected vars 
    vv <- c("factory_cp_level_nc_rate",colnames(x)[which(as.numeric(vcf.1se)!=0) -1])
    ###Selected vars to random forest 
    if(length(vv)>1){
      set.seed(123)
      rang <- ranger(factory_cp_level_nc_rate~., data=OSH_viet[rg,vv], importance='impurity',num.trees = 4000)
      ###get relative importance
      imp <- sort(importance(rang), decreasing = TRUE)
      ###get ranking
      rank <- (length(imp):1)/length(imp)
      names(rank) <- names(imp)
      tmprank <- t1[,-2]
      tmprank[1,-1] <- 0
      tmprank[1, names(rank)] <- rank
      vrcf1 <- rbind(vrcf1, tmprank)
      ###variable importance plot for inspection of behaviour of alpha
      vipv <- vip(rang,num_features = length(vv),metric="RMSE")
      pdf(file=paste("/Users/malik/Documents/GitHub/Thesis---Code/Plots/ranking/Vietnam/rang_",ialpha,"_",rg,".pdf",sep=""))
      print(vipv)
      dev.off()
    }
    ###same with model of minimum lambda
    vv <- c("factory_cp_level_nc_rate",colnames(x)[which(as.numeric(vcf.min)!=0) -1])
    if(length(vv)>1){
      set.seed(123)
      rang <- ranger(factory_cp_level_nc_rate~., data=OSH_viet[rg,vv], importance="impurity")
      imp <- sort(importance(rang), decreasing = TRUE)
      rank <- (length(imp):1)/length(imp)
      names(rank) <- names(imp)
      tmprank <- t2[,-2]
      tmprank[1,-1] <- 0
      tmprank[1, names(rank)] <- rank
      vrcfm <- rbind(vrcfm, tmprank)
      
      
    }
    
  }
  ###mse for all models and correlation matrix to evaluate which alpha to choose
  vpt <- OSH_viet$factory_cp_level_nc_rate[(ioffset+1):ni]
  vd <- data.frame(true=vpt,p1=vp1,pm=vpm,la=vlap)
  vmse1 <- c(vmse1, mean((vpt-vp1)^2))
  vmsem <- c(vmsem, mean((vpt-vpm)^2))
  vmsela <- c(vmsela, mean((vpt-vlap)^2,na.rm=TRUE))
  print(cor(vd,use="pairwise"))
}

###MSE Dynenet lambda 1se
vmse1
###MSE Dynenet lambda min
vmsem
###MSE Dynenet Lasso
vmsela
#### effenciency ratio MSE DynENet/MSE Lasso
veff_ratio <- vmse1/vmsela
###get the best alpha for lowest mse of lambda 1se in DyEnet
vbest_alpha <- alpha[which(vmse1==min(vmse1))]

###plot mse evolution Gradient descent
p <- ggplot(data.frame(x=alpha,y=vmse1),aes(x=x,y=y))+geom_histogram(sta="identity")+
  xlab("alpha")+ylab("MSE")
setwd("~/Documents/GitHub/Thesis---Code/Plots")
ggsave("MSE_alpha_vietnam.png",plot=p)


####second round with best alpha
vmsem  <- NULL
vmse1 <- NULL
vmsela <- NULL
intercepti <- FALSE
###now alpha is no sequence but the best alpha chosen, following code has the same structure as above
for(ialpha in  vbest_alpha){
  #for(ialpha in seq(0,1,length=5)){
  vcf1 <- NULL
  vla1 <- NULL
  vlap <- NULL
  vcfm <- NULL
  vrcf1 <- NULL
  vrcfm <- NULL
  vp1 <- NULL
  vpm <- NULL
  vl1 <- NULL
  vlm <- NULL
  var <- NULL
  ni <- nrow(OSH_viet)
  ioffset <-0.8*ni
  for(i in ioffset:(ni-1)){
    rg <- (i-ioffset+1):i
    y <- OSH_viet$factory_cp_level_nc_rate[rg]
    x <- as.matrix(OSH_viet[rg,colnames(OSH_viet)!="factory_cp_level_nc_rate"])
    x.new <- as.matrix(OSH_viet[i+1,colnames(OSH_viet)!="factory_cp_level_nc_rate"],nrow=1)
    set.seed(123)
    mod_cv <-  cv.glmnet(x=x, y=y, family="gaussian",
                         intercept = intercepti, alpha=1,lamda=lambda)
    set.seed(123)
    vla.eq <- glmnet(x=x, y=y, intercept=intercepti, lambda=mod_cv$lambda.1se,alpha=1)
    set.seed(123)
    mod_cv1 <-  cv.glmnet(x=x, y=y, family="gaussian",
                          intercept = intercepti, alpha=0)
    set.seed(123)  
    vri.eq <- glmnet(x=x, y=y, family="gaussian", intercept=intercepti, lambda=mod_cv1$lambda.1se,alpha=0)
    vla.1se <- coef(vla.eq, s=mod_cv$lambda.1se)
    vla.min <- coef(vla.eq, s=mod_cv$lambda.min)
    a1 <- data.frame(date=vdates[i], t(as.matrix(vla.1se)))
    colnames(a1)[2] <- "Intercept"
    vla1 <- rbind(vla1, a1)
    vlap <- c(vlap, predict(vla.eq, newx = x.new,s = mod_cv$lambda.1se))
    set.seed(123)
    cvi <- cv.glmnet(x=x, y=y, alpha=ialpha, intercept=intercepti, lambda=lambda, grouped=FALSE)
    set.seed(123)
    vnet <- glmnet(x=x, y=y, alpha=ialpha,  intercept=intercepti, lambda=cvi$lambda.1se)
    vcf.1se <- coef(vnet, s=cvi$lambda.1se)
    vcf.min <- coef(vnet, s=cvi$lambda.min)
    t1 <- data.frame(date=vdates[i], t(as.matrix(vcf.1se)))
    colnames(t1)[2] <- "Intercept"
    t2 <- data.frame(date=vdates[i], t(as.matrix(vcf.min)))
    colnames(t2)[2] <- "Intercept"
    vcf1 <- rbind(vcf1, t1)
    vcfm <- rbind(vcfm, t2)
    vp1 <- c(vp1, predict(vnet, newx = x.new,s = cvi$lambda.1se))
    vpm <- c(vpm, predict(vnet, newx = x.new,s = cvi$lambda.min))
    vl1 <- c(vl1, cvi$lambda.1se)
    vlm <- c(vlm, cvi$lambda.min)
    
    vv <- c("factory_cp_level_nc_rate",colnames(x)[which(as.numeric(vcf.1se)!=0) -1])
    if(length(vv)>1){
      set.seed(123)
      rang <- ranger(factory_cp_level_nc_rate~., data=OSH_viet[rg,vv], importance="impurity",num.trees = 4000)
      imp <- sort(importance(rang), decreasing = TRUE)
      rank <- (length(imp):1)/length(imp)
      names(rank) <- names(imp)
      tmprank <- t1[,-2]
      tmprank[1,-1] <- 0
      tmprank[1, names(rank)] <- rank
      vrcf1 <- rbind(vrcf1, tmprank)
      
      vipv <- vip(rang,num_features = length(vv))
      pdf(file=paste("/Users/malik/Documents/GitHub/Thesis---Code/Plots/ranking/Vietnam/rang_best_",rg,".pdf",sep=""))
      print(vipv)
      dev.off()
      
      
      
      vv <- c("factory_cp_level_nc_rate",colnames(x)[which(as.numeric(vcf.min)!=0) -1])
      if(length(vv)>1){
        set.seed(123)
        rang <- ranger(factory_cp_level_nc_rate~., data=OSH_viet[rg,vv], importance='impurity')
        imp <- sort(importance(rang), decreasing = TRUE)
        rank <- (length(imp):1)/length(imp)
        names(rank) <- names(imp)
        tmprank <- t2[,-2]
        tmprank[1,-1] <- 0
        tmprank[1, names(rank)] <- rank
        vrcfm <- rbind(vrcfm, tmprank)
      }
    }
  }
  vpt <- OSH_viet$factory_cp_level_nc_rate[(ioffset+1):ni]
  vd <- data.frame(true=vpt,p1=vp1,pm=vpm,la=vlap,lambda=cvi$lambda.1se)
  vmse1 <- c(vmse1, mean((vpt-vp1)^2))
  vmsem <- c(vmsem, mean((vpt-vpm)^2))
  vmsela <- c(vmsela, mean((vpt-vlap)^2,na.rm=TRUE))
  print(cor(vd,use="pairwise"))
}



###VISUALIZATION VIETNAM
#some color adjustments for visualization 
makeColorRampPalette <- function(colors, cutoff.fraction, num.colors.in.palette)
{
  stopifnot(length(colors) == 4)
  ramp1 <- colorRampPalette(colors[1:2])(num.colors.in.palette * cutoff.fraction)
  ramp2 <- colorRampPalette(colors[3:4])(num.colors.in.palette * (1 - cutoff.fraction))
  return(c(ramp1, ramp2))
}
cutoff.distance <- 0.5  
cols <- makeColorRampPalette(c("white", "steelblue",    # distances 0 to 3 colored from white to red
                               "steelblue", "red"), # distances 3 to max(distmat) colored from green to black
                             cutoff.distance  ,
                             100)

###time series plot of predicted and true value
vtoplot <- xts(vd[,c("true","p1","la")],order.by=as.Date(as.yearqtr(vdates[(ioffset+1):ni])))
colnames(vtoplot) <- c("FacCPNC","Elastic Net","Lasso")
autoplot(vtoplot, facets = NULL) +xlab("")


###Heatmap
####Matrix with relative importance and dates
vrcf2 <- vrcf1[, colSums(vrcf1 != 0) > 0]   
bigmat <- vrcf2[,-1]
rownames(bigmat) <- vrcf2$date
#bigmat <- bigmat[,-match("fac",colnames(bigmat))]
bigmat[is.na(bigmat) | bigmat==0] <- 0
dp <- colSums(bigmat)
dp
dim(bigmat)
#####transform matrix to get time series structure
bigmat <- t(bigmat)

####Annotation row and groouping of variables for visualization
Firm_general <- c("firm_size","femRatio","firm_age")
Union <- c("AccessUnion","formUnion","punUnion","refBarg","NumBarg","percUnion")
Wage <- c("tminWage")
Safety <- c("trainFirst","trainu16","trainAppr","trainApprMen","trainMen","trainWorker","trainChem")
Country <- c("HPI","Wellbeing","Ecological_Footprint","Biocapacity","GDP_per_capita","Life_Exp","Pop","counts_air","counts_conflict","GDP.per.capita","Access_electricity","Co2_emissions","Working_poverty","Minimum_wage","Informal_empl") 
business <- c("Peer.Grouping.Enabled","prefSup","contr","subContr")
wdi <- names(OSH_viet)[grep("wdi_",names(OSH_viet))]
fvp <- names(OSH_viet)[grep("fvp_",names(OSH_viet))]
reign <- names(OSH_viet)[grep("reign_",names(OSH_viet))]
vdem <- names(OSH_viet)[grep("vdem_",names(OSH_viet))]
ilo <- names(OSH_viet)[grep("ILO_",names(OSH_viet))]



group <- rep("", nrow(bigmat))

group[match(Firm_general, rownames(bigmat))] <- "BW-Data: General"
group[match(Union, rownames(bigmat))] <- "BW-Data: Union"
group[match(Wage, rownames(bigmat))] <- "BW-Data: Minimum Wage"
group[match(Safety, rownames(bigmat))] <- "BW-Data: Safety"
#group[match(Country, rownames(bigmat))] <- "Country"
group[match(business, rownames(bigmat))] <- "BW-Data: Business" 
group[match(vdem, rownames(bigmat))] <- "Vdem-Data"
group[match(reign, rownames(bigmat))] <- "Reign-Data"
group[match(fvp, rownames(bigmat))] <- "FVP-Data"
group[match(wdi, rownames(bigmat))] <- "WDI-Data"
group[match(ilo, rownames(bigmat))] <- "ILO-Data"

####annotation data frame
annotation_row = data.frame( DataClass = factor(group,ordered=T))
rownames(annotation_row) = rownames(bigmat)


##cloumn labels
clabels <- colnames(bigmat)
#ipos <- unique(c(seq(1,length(clabels),by=7),length(clabels)))

#order big_mat with annotation row (Data Sources)
big_mat_ord<- bigmat[rownames(annotation_row), ]

###Heatmap with relativve importance
pheatmap(big_mat_ord, cluster_cols = FALSE, cluster_rows = F,
         #  clustering_distance_rows =  "correlation",
         #  color = c("white", "steelblue", "red"),
         color=cols,
         # legend = FALSE,
         cellheight = 10,
         cellwidth=40,
         labels_col = clabels,
         annotation_row = annotation_row,
         treeheight_row = 0, treeheight_col = 0,
         fontsize = 10,
         fontsize_col = 10,
         border_color = "gray",
         main = sprintf("%s : relative importarnce of predictors selected by Dynamic Elastic Net","Vietnam"),
         angle_col=45,filename = "/Users/malik/Documents/GitHub/Thesis---Code/Plots/Heatmap_vietnam1.pdf",
         width=17,height =10
)


###standardized coefficients
bigmat <- vcf1[,-(1:2)]
rownames(bigmat) <- vcfm$date


vcols <- makeColorRampPalette(c("red", "white",    # distances 0 to 3 colored from white to red
                                "white", "green"), # distances 3 to max(distmat) colored from green to black
                              1-max(bigmat)/diff(range(bigmat))  ,
                              100)

bigmat <- bigmat %>% select_if(colSums(.) != 0)
dp <- colSums(bigmat)
dp
dim(bigmat)
bigmat <- t(bigmat)

row.order <- try(hclust(dist(bigmat))$order, TRUE)
dat_new <- bigmat[row.order, ] # re-order matrix accoring to clustering

nba.m <- reshape2::melt(dat_new)

mycol <- colorRampPalette(c("red", "white", "blue"))(7)

###graphical arrangements of cuts and bbreaks
Mx <- max(range(bigmat))*1.05
mx <- min(range(bigmat))*1.05
sq <- c(seq(mx,-1e-5, length= 4),  0,seq(1e-5,Mx,length=4))
nba.m$cat <- cut(nba.m$value,breaks=sq)
lv <- levels(nba.m$cat)
nba.m$cat <- as.character(nba.m$cat)
nba.m$cat[nba.m$value==0] <- "0"
nba.m$cat[nba.m$cat == "(-1e-05,0]"] <- lv[3]
nba.m$cat[nba.m$cat == "(0,1e-05]"] <- lv[6]
nba.m$cat <- sub("-1e-05","0",nba.m$cat)
nba.m$cat <- sub("1e-05","0",nba.m$cat)
lv2 <- lv
lv2 <- sub("-1e-05","0",lv2)
lv2 <- sub("1e-05","0",lv2)
lv2 <- c(lv2[1:3],"0",lv2[-(1:5)])

nba.m$cat2 <- factor(nba.m$cat, levels = lv2, ordered = TRUE )
table(nba.m$cat2)
hclust(dist(bigmat)) -> oo
nba.m$Var1 <- factor(nba.m$Var1, 
                     levels = rev(oo$labels),ordered = TRUE)

###Heatmap with standardized coefficients
aa <- ggplot(nba.m, aes(Var2, Var1)) + 
  geom_tile( aes(fill = cat2),
             colour = "darkgray") + 
  scale_fill_manual(values=mycol, 
                    breaks=levels(nba.m$cat2) )+
  labs(x = "", y = "Variable", fill="Size of\nstandardized\ncoefficents",
       title="FacCPNC-Vietnam")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
aa
pdf("/Users/malik/Documents/GitHub/Thesis---Code/Plots/Coefs_vietnam.pdf",width=17, height=10, pointsize = 12)
print(aa)
dev.off()



######################################################################
# Indonesia MODEL                                                      #
######################################################################
####Model structure and visualization is the same as for Vietnam.
#####No further comments

OSH_indo <- temp_m1[temp_m1$Country=="Indonesia" & temp_m1$date>="2011 Q1" & temp_m1$date<2020  ,]
OSH_indo$Year <- readr::parse_number(as.character(OSH_indo$date))
OSH_indo <- OSH_indo %>%
  arrange(year,date)

##Delete unneccessary columns
rownames(OSH_indo) <- OSH_indo$date
OSH_indo$Country <-OSH_indo$date <- OSH_indo$Country.x <- OSH_indo$Country.y <- OSH_indo$season_year.y <- OSH_indo$season_year.x <- OSH_indo$Factory.Assessed.ID <- OSH_indo$total_cps <- OSH_indo$total_cp_nc <- OSH_indo$Year <- OSH_indo$ID <- OSH_indo$Male<- OSH_indo$Female<-OSH_indo$Pop<-OSH_indo$HPI <-OSH_indo$HPI_rank<-OSH_indo$seasnum_year.x <- OSH_indo$seasnum_year.y <- OSH_indo$Season <-OSH_indo$Season_year <- OSH_indo$GDP_per_capita <- OSH_indo$lenPart <-OSH_indo$seasnum_year<- OSH_indo$reign_country_month_id <- OSH_indo$month_id <- OSH_indo$month <- OSH_indo$year <- OSH_indo$Season.x <- OSH_indo$Season.y <- OSH_indo$season_clim <- OSH_indo$season_num <- OSH_indo$Life_Exp <-OSH_indo$counts_conflict <-OSH_indo$fvp_lnpop200 <-OSH_indo$Wellbeing<-OSH_indo$Biocapacity <-OSH_indo$Ecological_Footprint<-OSH_indo$Life_Exp<- OSH_indo$country_id<- OSH_indo$gwno<-OSH_indo$name<-NULL
#columns_to_remove <- grep("wdi_", names(OSH_indo))
#OSH_indo <-OSH_indo[,-columns_to_remove]
OSH_indo <-  remove_zero_cols(OSH_indo)
##DyENet with benchmark lasso and ridge
###First round to get best alpha
idates <- rownames(OSH_indo)
alpha <- seq(0.1,0.9,length=25)
lambda <- seq(1e-08,100,length=2500)
imsem <- NULL
imse1 <- NULL
imsela <- NULL
intercepti <- FALSE
for(ialpha in  alpha){
  #for(ialpha in seq(0,1,length=5)){
  icf1 <- NULL
  ila1 <- NULL
  ilap <- NULL
  icfm <- NULL
  ircf1 <- NULL
  ircfm <- NULL
  ip1 <- NULL
  ipm <- NULL
  il1 <- NULL
  ilm <- NULL
  iar <- NULL
  
  ioffset <-0.8*ni
  for(i in ioffset:(ni-1)){
    rg <- (i-ioffset+1):i
    y <- OSH_indo$factory_cp_level_nc_rate[rg]
    x <- as.matrix(OSH_indo[rg,colnames(OSH_indo)!="factory_cp_level_nc_rate"])
    x.new <- as.matrix(OSH_indo[i+1,colnames(OSH_indo)!="factory_cp_level_nc_rate"],nrow=1)
    set.seed(123)
    mod_cv <-  cv.glmnet(x=x, y=y, family="gaussian",
                         intercept = intercepti, alpha=1,lamda=lambda)
    set.seed(123)
    la.eq <- glmnet(x=x, y=y, intercept=intercepti, lambda=mod_cv$lambda.1se,alpha=1)
    set.seed(123)
    mod_cv1 <-  cv.glmnet(x=x, y=y, family="gaussian",
                          intercept = intercepti, alpha=0)
    set.seed(123)  
    ri.eq <- glmnet(x=x, y=y, family="gaussian", intercept=intercepti, lambda=mod_cv1$lambda.1se,alpha=0)
    ila.1se <- coef(la.eq, s=mod_cv$lambda.1se)
    ila.min <- coef(la.eq, s=mod_cv$lambda.min)
    a1 <- data.frame(date=idates[i], t(as.matrix(ila.1se)))
    colnames(a1)[2] <- "Intercept"
    ila1 <- rbind(ila1, a1)
    ilap <- c(ilap, predict(la.eq, newx = x.new,s = mod_cv$lambda.1se))
    set.seed(123)
    cvv <- cv.glmnet(x=x, y=y, alpha=ialpha, intercept=intercepti, lambda=lambda, grouped=FALSE)
    set.seed(123)
    inet <- glmnet(x=x, y=y, alpha=ialpha,  intercept=intercepti, lambda=cvv$lambda.1se)
    icf.1se <- coef(inet, s=cvv$lambda.1se)
    icf.min <- coef(inet, s=cvv$lambda.min)
    t1 <- data.frame(date=idates[i], t(as.matrix(icf.1se)))
    colnames(t1)[2] <- "Intercept"
    t2 <- data.frame(date=idates[i], t(as.matrix(icf.min)))
    colnames(t2)[2] <- "Intercept"
    icf1 <- rbind(icf1, t1)
    icfm <- rbind(icfm, t2)
    ip1 <- c(ip1, predict(inet, newx = x.new,s = cvv$lambda.1se))
    ipm <- c(ipm, predict(inet, newx = x.new,s = cvv$lambda.min))
    il1 <- c(il1, cvv$lambda.1se)
    ilm <- c(ilm, cvv$lambda.min)
    
    vv <- c("factory_cp_level_nc_rate",colnames(x)[which(as.numeric(icf.1se)!=0) -1])
    if(length(vv)>1){
      set.seed(123)
      rang <- ranger(factory_cp_level_nc_rate~., data=OSH_indo[rg,vv], importance="impurity",num.trees = 4000)
      imp <- sort(importance(rang), decreasing = TRUE)
      rank <- (length(imp):1)/length(imp)
      names(rank) <- names(imp)
      tmprank <- t1[,-2]
      tmprank[1,-1] <- 0
      tmprank[1, names(rank)] <- rank
      ircf1 <- rbind(ircf1, tmprank)
      vipi <- vip(rang,num_features = length(vv))
      pdf(file=paste("/Users/malik/Documents/GitHub/Thesis---Code/Plots/ranking/Indonesia/rang_",ialpha,"_",rg,".pdf",sep=""))
      print(vipi)
      dev.off()
    }
    
    vv <- c("factory_cp_level_nc_rate",colnames(x)[which(as.numeric(icf.min)!=0) -1])
    if(length(vv)>1){
      set.seed(123)
      rang <- ranger(factory_cp_level_nc_rate~., data=OSH_indo[rg,vv], importance="impurity")
      imp <- sort(importance(rang), decreasing = TRUE)
      rank <- (length(imp):1)/length(imp)
      names(rank) <- names(imp)
      tmprank <- t2[,-2]
      tmprank[1,-1] <- 0
      tmprank[1, names(rank)] <- rank
      ircfm <- rbind(ircfm, tmprank)
    }
    
  }
  ipt <- OSH_indo$factory_cp_level_nc_rate[(ioffset+1):ni]
  id <- data.frame(true=ipt,p1=ip1,pm=ipm,la=ilap)
  imse1 <- c(imse1, mean((ipt-ip1)^2))
  imsem <- c(imsem, mean((ipt-ipm)^2))
  imsela <- c(imsela, mean((ipt-ilap)^2,na.rm=TRUE))
  print(cor(id,use="pairwise"))
}



imse1
imsem
imsela
eff_ratio <- imse1/imsela
best_alpha <- alpha[which(eff_ratio==min(eff_ratio))]

imsem <- NULL
imse1 <- NULL
imsela <- NULL
intercepti <- FALSE
for(ialpha in  best_alpha){
  #for(ialpha in seq(0,1,length=5)){
  icf1 <- NULL
  ila1 <- NULL
  ilap <- NULL
  icfm <- NULL
  ircf1 <- NULL
  ircfm <- NULL
  ip1 <- NULL
  ipm <- NULL
  il1 <- NULL
  ilm <- NULL
  iar <- NULL
  ni <- nrow(OSH_indo)
  n <- 0.8
  ioffset <-0.8*ni
  for(i in ioffset:(ni-1)){
    rg <- (i-ioffset+1):i
    y <- OSH_indo$factory_cp_level_nc_rate[rg]
    x <- as.matrix(OSH_indo[rg,colnames(OSH_indo)!="factory_cp_level_nc_rate"])
    x.new <- as.matrix(OSH_indo[i+1,colnames(OSH_indo)!="factory_cp_level_nc_rate"],nrow=1)
    set.seed(123)
    mod_cv <-  cv.glmnet(x=x, y=y, family="gaussian",
                         intercept = intercepti, alpha=1,lamda=lambda)
    set.seed(123)
    la.eq <- glmnet(x=x, y=y, intercept=intercepti, lambda=lambda,alpha=1)
    set.seed(123)
    mod_cv <-  cv.glmnet(x=x, y=y, family="gaussian",
                         intercept = intercepti, alpha=0)
    set.seed(123)  
    ri.eq <- glmnet(x=x, y=y, family="gaussian", intercept=intercepti, lambda=mod_cv$lambda.1se,alpha=0)
    ila.1se <- coef(la.eq, s=mod_cv$lambda.1se)
    ila.min <- coef(la.eq, s=mod_cv$lambda.min)
    a1 <- data.frame(date=idates[i], t(as.matrix(ila.1se)))
    colnames(a1)[2] <- "Intercept"
    ila1 <- rbind(ila1, a1)
    ilap <- c(ilap, predict(la.eq, newx = x.new,s = mod_cv$lambda.1se))
    set.seed(123)
    cvi <- cv.glmnet(x=x, y=y, alpha=ialpha, intercept=intercepti, lambda=lambda, grouped=FALSE)
    set.seed(123)
    inet <- glmnet(x=x, y=y, alpha=ialpha,  intercept=intercepti, lambda=cvi$lambda.1se)
    icf.1se <- coef(inet, s=cvi$lambda.1se)
    icf.min <- coef(inet, s=cvi$lambda.min)
    t1 <- data.frame(date=idates[i], t(as.matrix(icf.1se)))
    colnames(t1)[2] <- "Intercept"
    t2 <- data.frame(date=idates[i], t(as.matrix(icf.min)))
    colnames(t2)[2] <- "Intercept"
    icf1 <- rbind(icf1, t1)
    icfm <- rbind(icfm, t2)
    ip1 <- c(ip1, predict(inet, newx = x.new,s = cvi$lambda.1se))
    ipm <- c(ipm, predict(inet, newx = x.new,s = cvi$lambda.min))
    il1 <- c(il1, cvi$lambda.1se)
    ilm <- c(ilm, cvi$lambda.min)
    
    vv <- c("factory_cp_level_nc_rate",colnames(x)[which(as.numeric(icf.1se)!=0) -1])
    if(length(vv)>1){
      set.seed(123)
      rang <- ranger(factory_cp_level_nc_rate~., data=OSH_indo[rg,vv], importance="impurity",num.trees = 4000)
      imp <- sort(importance(rang), decreasing = TRUE)
      rank <- (length(imp):1)/length(imp)
      names(rank) <- names(imp)
      tmprank <- t1[,-2]
      tmprank[1,-1] <- 0
      tmprank[1, names(rank)] <- rank
      ircf1 <- rbind(ircf1, tmprank)
      vipi <- vip(rang,num_features = length(vv))
      pdf(file=paste("/Users/malik/Documents/GitHub/Thesis---Code/Plots/ranking/Indonesia/rang_best_",rg,".pdf",sep=""))
      print(vipi)
      dev.off()
    }
    
    
    vv <- c("factory_cp_level_nc_rate",colnames(x)[which(as.numeric(icf.min)!=0) -1])
    if(length(vv)>1){
      set.seed(123)
      rang <- ranger(factory_cp_level_nc_rate~., data=OSH_indo[rg,vv], importance="impurity")
      imp <- sort(importance(rang), decreasing = TRUE)
      rank <- (length(imp):1)/length(imp)
      names(rank) <- names(imp)
      tmprank <- t2[,-2]
      tmprank[1,-1] <- 0
      tmprank[1, names(rank)] <- rank
      ircfm <- rbind(ircfm, tmprank)
    }
  }
  
  ipt <- OSH_indo$factory_cp_level_nc_rate[(ioffset+1):ni]
  id <- data.frame(true=ipt,p1=ip1,pm=ipm,la=ilap)
  imse1 <- c(imse1, mean((ipt-ip1)^2))
  imsem <- c(imsem, mean((ipt-ipm)^2))
  imsela <- c(imsela, mean((ipt-ilap)^2,na.rm=TRUE))
  print(cor(id,use="pairwise"))
}


date <- OSH_indo$date
rownames(id) <- rownames(OSH_indo)[(ioffset+1):ni]

###Visualization Indonesia
itoplot <- xts(id[,c("true","p1","la")],order.by=as.Date(as.yearqtr(vdates[(ioffset+1):ni])))
colnames(itoplot) <- c("FacCPNC","Elastic Net","Lasso")
autoplot(itoplot, facets = NULL) +xlab("")

ircf2 <- ircf1[, colSums(ircf1 != 0) > 0]   
bigmat <- ircf2[,-1]
rownames(bigmat) <- ircf2$date
#bigmat <- bigmat[,-match("fac",colnames(bigmat))]
bigmat[is.na(bigmat) | bigmat==0] <- 0
dp <- colSums(bigmat)
dp
dim(bigmat)
bigmat <- t(bigmat)



Firm_general <- c("firm_size","femRatio","firm_age")
Union <- c("AccessUnion","formUnion","punUnion","refBarg","NumBarg","percUnion")
Wage <- c("tminWage")
Safety <- c("trainFirst","trainu16","trainAppr","trainApprMen","trainMen","trainWorker","trainChem")
Country <- c("HPI","Wellbeing","Ecological_Footprint","Biocapacity","GDP_per_capita","Life_Exp","Pop","counts_air","counts_conflict","GDP.per.capita","Access_electricity","Co2_emissions","Working_poverty","Minimum_wage","Informal_empl") 
business <- c("Peer.Grouping.Enabled","prefSup","contr","subContr")
wdi <- names(OSH_indo)[grep("wdi_",names(OSH_indo))]
fvp <- names(OSH_indo)[grep("fvp_",names(OSH_indo))]
reign <- names(OSH_indo)[grep("reign_",names(OSH_indo))]
vdem <- names(OSH_indo)[grep("vdem_",names(OSH_indo))]
ilo <- names(OSH_indo)[grep("ILO_",names(OSH_indo))]



group <- rep("", nrow(bigmat))

group[match(Firm_general, rownames(bigmat))] <- "BW-Data: General"
group[match(Union, rownames(bigmat))] <- "BW-Data: Union"
group[match(Wage, rownames(bigmat))] <- "BW-Data: Minimum Wage"
group[match(Safety, rownames(bigmat))] <- "BW-Data: Safety"
#group[match(Country, rownames(bigmat))] <- "Country"
group[match(business, rownames(bigmat))] <- "BW-Data: Business" 
group[match(vdem, rownames(bigmat))] <- "Vdem-Data"
group[match(reign, rownames(bigmat))] <- "Reign-Data"
group[match(fvp, rownames(bigmat))] <- "FVP-Data"
group[match(wdi, rownames(bigmat))] <- "WDI-Data"
group[match(ilo, rownames(bigmat))] <- "ILO-Data"


annotation_row = data.frame( DataClass = factor(group,ordered=T))
rownames(annotation_row) = rownames(bigmat)
clabels <- colnames(bigmat)
big_mat_ord<- bigmat[rownames(annotation_row), ]

pheatmap(big_mat_ord, cluster_cols = FALSE, cluster_rows = F,
         #  clustering_distance_rows =  "correlation",
         #  color = c("white", "steelblue", "red"),
         color=cols,
         # legend = FALSE,
         cellheight = 5,
         cellwidth=25,
         labels_col = clabels,
         annotation_row = annotation_row,
         treeheight_row = 0, treeheight_col = 0,
         fontsize = 7,
         fontsize_col = 12,
         border_color = "gray",
         main = sprintf("%s : relative importarnce of predictors selected by Dynamic Elastic Net","Indonesia"),
         angle_col=45,filename = "/Users/malik/Documents/GitHub/Thesis---Code/Plots/Heatmap_indonesia1.pdf",
         width=10,height =17
)

bigmat <- icf1[,-(1:2)]
rownames(bigmat) <- icfm$date


icols <- makeColorRampPalette(c("red", "white",    # distances 0 to 3 colored from white to red
                                "white", "green"), # distances 3 to max(distmat) colored from green to black
                              1-max(bigmat)/diff(range(bigmat))  ,
                              100)

bigmat <- bigmat %>% select_if(colSums(.) != 0)
dp <- colSums(bigmat)
dp
dim(bigmat)
bigmat <- t(bigmat)
row.order <- try(hclust(dist(bigmat))$order, TRUE)
dat_new <- bigmat[row.order, ] # re-order matrix accoring to clustering

nba.m <- reshape2::melt(dat_new)

mycol <- colorRampPalette(c("red", "white", "blue"))(7)

Mx <- max(range(bigmat))*1.05
mx <- min(range(bigmat))*1.05
sq <- c(seq(mx,-1e-5, length= 4),  0,seq(1e-5,Mx,length=4))
nba.m$cat <- cut(nba.m$value,breaks=sq)
lv <- levels(nba.m$cat)
nba.m$cat <- as.character(nba.m$cat)
nba.m$cat[nba.m$value==0] <- "0"
nba.m$cat[nba.m$cat == "(-1e-05,0]"] <- lv[3]
nba.m$cat[nba.m$cat == "(0,1e-05]"] <- lv[6]
nba.m$cat <- sub("-1e-05","0",nba.m$cat)
nba.m$cat <- sub("1e-05","0",nba.m$cat)
lv2 <- lv
lv2 <- sub("-1e-05","0",lv2)
lv2 <- sub("1e-05","0",lv2)
lv2 <- c(lv2[1:3],"0",lv2[-(1:5)])

nba.m$cat2 <- factor(nba.m$cat, levels = lv2, ordered = TRUE )
table(nba.m$cat2)
nba.m$Var2 <- as.Date(nba.m$Var2)
hclust(dist(bigmat)) -> oo
nba.m$Var1 <- factor(nba.m$Var1, 
                     levels = rev(oo$labels),ordered = TRUE)

aa <- ggplot(nba.m, aes(Var2, Var1)) + 
  geom_tile( aes(fill = cat2),
             colour = "darkgray") + 
  scale_fill_manual(values=mycol, 
                    breaks=levels(nba.m$cat2) )+
  labs(x = "", y = "Variable", fill="Size of\nstandardized\ncoefficents",
       title="FacCPNC-Indonesia")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.text.y = element_text(size=8))
aa
pdf("/Users/malik/Documents/GitHub/Thesis---Code/Plots/Coefs_Indonesia.pdf",width=17, height=10, pointsize = 12)
print(aa)
dev.off()

#####################################################################
# Jordan MODEL                                                      #
######################################################################
###same as models before just for Jordan

OSH_jord <- temp_m1[temp_m1$Country=="Jordan" & temp_m1$date>="2011 Q1" & temp_m1$date<2020  ,]
OSH_jord$Year <- readr::parse_number(as.character(OSH_jord$date))
OSH_jord <- OSH_jord %>%
  arrange(Year,date)

##Delete unneccessary columns
rownames(OSH_jord) <- OSH_jord$date
OSH_jord$Country <-OSH_jord$date <- OSH_jord$Country.x <- OSH_jord$Country.y <- OSH_jord$season_year.y <- OSH_jord$season_year.x <- OSH_jord$Factory.Assessed.ID <- OSH_jord$total_cps <- OSH_jord$total_cp_nc <- OSH_jord$Year <- OSH_jord$ID <- OSH_jord$Male<- OSH_jord$Female<-OSH_jord$Pop<-OSH_jord$HPI <-OSH_jord$HPI_rank<-OSH_jord$seasnum_year.x <- OSH_jord$seasnum_year.y <- OSH_jord$Season <-OSH_jord$Season_year <- OSH_jord$GDP_per_capita <- OSH_jord$lenPart <-OSH_jord$seasnum_year<- OSH_jord$reign_country_month_id <- OSH_jord$month_id <- OSH_jord$month <- OSH_jord$year <- OSH_jord$Season.x <- OSH_jord$Season.y <- OSH_jord$season_clim <- OSH_jord$season_num <- OSH_jord$Life_Exp <-OSH_jord$counts_conflict <-OSH_jord$fvp_lnpop200 <- OSH_jord$name <- OSH_jord$country_id <- OSH_jord$Wellbeing <- OSH_jord$Biocapacity <- OSH_jord$Ecological_Footprint <- OSH_jord$gwno <-   NULL
#columns_to_remove <- grep("wdi_", names(OSH_jord))
#OSH_jord <-OSH_jord[,-columns_to_remove]
OSH_jord <- remove_zero_cols(OSH_jord)
##DyENet with benchmark lasso and ridge
###First round to get best alpha
jdates <- rownames(OSH_jord)
alpha <- seq(0.10,0.9,length=25)
lambda <- seq(1e-08,1,length=2500)
jmsem <- NULL
jmse1 <- NULL
jmsela <- NULL
jmseri <- NULL
intercepti <- FALSE
for(ialpha in  alpha){
  #for(ialpha in seq(0,1,length=5)){
  jcf1 <- NULL
  jla1 <- NULL
  jlap <- NULL
  jcfm <- NULL
  jrcf1 <- NULL
  jrcfm <- NULL
  jp1 <- NULL
  jpm <- NULL
  jl1 <- NULL
  jlm <- NULL
  jar <- NULL
  jrip <- NULL
  jri1 <- NULL
  
  
  ni <- nrow(OSH_jord)
  ioffset <-0.8*ni
  for(i in ioffset:(ni-1)){
    rg <- (i-ioffset+1):i
    y <- OSH_jord$factory_cp_level_nc_rate[rg]
    x <- as.matrix(OSH_jord[rg,colnames(OSH_jord)!="factory_cp_level_nc_rate"])
    x.new <- as.matrix(OSH_jord[i+1,colnames(OSH_jord)!="factory_cp_level_nc_rate"],nrow=1)
    set.seed(123)
    jmod_cv <-  cv.glmnet(x=x, y=y, family="gaussian",
                          intercept = intercepti, alpha=1,lamda=lambda)
    set.seed(123)
    jla.eq <- glmnet(x=x, y=y, intercept=intercepti, lambda=jmod_cv$lambda,alpha=1)
    set.seed(123)
    jmod_cv1 <-  cv.glmnet(x=x, y=y, family="gaussian",
                           intercept = intercepti, alpha=0)
    set.seed(123)  
    jri.eq <- glmnet(x=x, y=y, family="gaussian", intercept=intercepti, lambda=jmod_cv1$lambda.1se,alpha=0)
    jla.1se <- coef(jla.eq, s=jmod_cv$lambda.1se)
    jla.min <- coef(jla.eq, s=jmod_cv$lambda.min)
    a1 <- data.frame(date=jdates[i], t(as.matrix(jla.1se)))
    colnames(a1)[2] <- "Intercept"
    jla1 <- rbind(jla1, a1)
    jlap <- c(jlap, predict(jla.eq, newx = x.new,s = jmod_cv$lambda.1se))
    jri.1se <- coef(jri.eq, s=jmod_cv1$lambda.1se)
    jri.min <- coef(jri.eq, s=jmod_cv1$lambda.min)
    b1 <- data.frame(date=jdates[i], t(as.matrix(jri.1se)))
    colnames(b1)[2] <- "Intercept"
    jla1 <- rbind(jri1, b1)
    jrip <- c(jrip, predict(jri.eq, newx = x.new,s = jmod_cv1$lambda.1se))
    set.seed(123)
    cvi <- cv.glmnet(x=x, y=y, alpha=ialpha, intercept=intercepti, lambda=lambda, grouped=FALSE)
    set.seed(123)
    jnet <- glmnet(x=x, y=y, alpha=ialpha,  intercept=intercepti, lambda=cvi$lambda.1se)
    jcf.1se <- coef(jnet, s=cvi$lambda.1se)
    jcf.min <- coef(jnet, s=cvi$lambda.min)
    t1 <- data.frame(date=jdates[i], t(as.matrix(jcf.1se)))
    colnames(t1)[2] <- "Intercept"
    t2 <- data.frame(date=jdates[i], t(as.matrix(jcf.min)))
    colnames(t2)[2] <- "Intercept"
    jcf1 <- rbind(jcf1, t1)
    jcfm <- rbind(jcfm, t2)
    jp1 <- c(jp1, predict(jnet, newx = x.new,s = cvi$lambda.1se))
    jpm <- c(jpm, predict(jnet, newx = x.new,s = cvi$lambda.min))
    jl1 <- c(jl1, cvi$lambda.1se)
    jlm <- c(jlm, cvi$lambda.min)
    
    vv <- c("factory_cp_level_nc_rate",colnames(x)[which(as.numeric(jcf.1se)!=0) -1])
    if(length(vv)>1){
      set.seed(123)
      rang <- ranger(factory_cp_level_nc_rate~., data=OSH_jord[rg,vv], importance="impurity",num.trees = 4000)
      imp <- sort(importance(rang), decreasing = TRUE)
      rank <- (length(imp):1)/length(imp)
      names(rank) <- names(imp)
      tmprank <- t1[,-2]
      tmprank[1,-1] <- 0
      tmprank[1, names(rank)] <- rank
      jrcf1 <- rbind(jrcf1, tmprank)
    }
    
    vv <- c("factory_cp_level_nc_rate",colnames(x)[which(as.numeric(jcf.min)!=0) -1])
    if(length(vv)>1){
      set.seed(123)
      rang <- ranger(factory_cp_level_nc_rate~., data=OSH_jord[rg,vv], importance="impurity")
      imp <- sort(importance(rang), decreasing = TRUE)
      rank <- (length(imp):1)/length(imp)
      names(rank) <- names(imp)
      tmprank <- t2[,-2]
      tmprank[1,-1] <- 0
      tmprank[1, names(rank)] <- rank
      jrcfm <- rbind(jrcfm, tmprank)
    }
    
  }
  jpt <- OSH_jord$factory_cp_level_nc_rate[(ioffset+1):ni]
  jd <- data.frame(true=jpt,p1=jp1,pm=jpm,la=jlap)
  jmse1 <- c(jmse1, mean((jpt-jp1)^2))
  jmsem <- c(jmsem, mean((jpt-jpm)^2))
  jmsela <- c(jmsela, mean((jpt-jlap)^2,na.rm=TRUE))
  jmseri<- c(jmseri, mean((jpt-jlap)^2,na.rm=TRUE))
  
  print(cor(jd,use="pairwise"))
}


jmse1
jmsem
jmsela
jeff_ratio <- jmse1/jmsela
jbest_alpha <- alpha[which(jmse1==min(jmse1))]

jmsem <- NULL
jmse1 <- NULL
jmsela <- NULL
jmseri <- NULL
intercepti <- FALSE
for(ialpha in jbest_alpha){
  #for(ialpha in seq(0,1,length=5)){
  jcf1 <- NULL
  jla1 <- NULL
  jlap <- NULL
  jcfm <- NULL
  jrcf1 <- NULL
  jrcfm <- NULL
  jp1 <- NULL
  jpm <- NULL
  jl1 <- NULL
  jlm <- NULL
  jar <- NULL
  jrip <- NULL
  jri1 <- NULL
  
  
  ni <- nrow(OSH_jord)
  ioffset <-0.8*ni
  for(i in ioffset:(ni-1)){
    rg <- (i-ioffset+1):i
    y <- OSH_jord$factory_cp_level_nc_rate[rg]
    x <- as.matrix(OSH_jord[rg,colnames(OSH_jord)!="factory_cp_level_nc_rate"])
    x.new <- as.matrix(OSH_jord[i+1,colnames(OSH_jord)!="factory_cp_level_nc_rate"],nrow=1)
    set.seed(123)
    jmod_cv <-  cv.glmnet(x=x, y=y, family="gaussian",
                          intercept = intercepti, alpha=1,lamda=lambda)
    set.seed(123)
    jla.eq <- glmnet(x=x, y=y, intercept=intercepti, lambda=lambda,alpha=1)
    set.seed(123)
    jmod_cv1 <-  cv.glmnet(x=x, y=y, family="gaussian",
                           intercept = intercepti, alpha=0)
    set.seed(123)  
    jri.eq <- glmnet(x=x, y=y, family="gaussian", intercept=intercepti, lambda=jmod_cv1$lambda.1se,alpha=0)
    jla.1se <- coef(jla.eq, s=jmod_cv$lambda.1se)
    jla.min <- coef(jla.eq, s=jmod_cv$lambda.min)
    a1 <- data.frame(date=jdates[i], t(as.matrix(jla.1se)))
    colnames(a1)[2] <- "Intercept"
    jla1 <- rbind(jla1, a1)
    jlap <- c(jlap, predict(jla.eq, newx = x.new,s = jmod_cv$lambda.1se))
    jri.1se <- coef(jri.eq, s=jmod_cv1$lambda.1se)
    jri.min <- coef(jri.eq, s=jmod_cv1$lambda.min)
    b1 <- data.frame(date=jdates[i], t(as.matrix(jri.1se)))
    colnames(b1)[2] <- "Intercept"
    jla1 <- rbind(jri1, b1)
    jrip <- c(jrip, predict(jri.eq, newx = x.new,s = jmod_cv1$lambda.1se))
    set.seed(123)
    cvi <- cv.glmnet(x=x, y=y, alpha=ialpha, intercept=intercepti, lambda=lambda, grouped=FALSE)
    set.seed(123)
    jnet <- glmnet(x=x, y=y, alpha=ialpha,  intercept=intercepti, lambda=cvi$lambda.1se)
    jcf.1se <- coef(jnet, s=cvi$lambda.1se)
    jcf.min <- coef(jnet, s=cvi$lambda.min)
    t1 <- data.frame(date=jdates[i], t(as.matrix(jcf.1se)))
    colnames(t1)[2] <- "Intercept"
    t2 <- data.frame(date=jdates[i], t(as.matrix(jcf.min)))
    colnames(t2)[2] <- "Intercept"
    jcf1 <- rbind(jcf1, t1)
    jcfm <- rbind(jcfm, t2)
    jp1 <- c(jp1, predict(jnet, newx = x.new,s = cvi$lambda.1se))
    jpm <- c(jpm, predict(jnet, newx = x.new,s = cvi$lambda.min))
    jl1 <- c(jl1, cvi$lambda.1se)
    jlm <- c(jlm, cvi$lambda.min)
    
    vv <- c("factory_cp_level_nc_rate",colnames(x)[which(as.numeric(jcf.1se)!=0) -1])
    if(length(vv)>1){
      set.seed(123)
      rang <- ranger(factory_cp_level_nc_rate~., data=OSH_jord[rg,vv], importance="impurity",num.trees = 4000)
      imp <- sort(importance(rang), decreasing = TRUE)
      rank <- (length(imp):1)/length(imp)
      names(rank) <- names(imp)
      tmprank <- t1[,-2]
      tmprank[1,-1] <- 0
      tmprank[1, names(rank)] <- rank
      jrcf1 <- rbind(jrcf1, tmprank)
    }
    
    vv <- c("factory_cp_level_nc_rate",colnames(x)[which(as.numeric(jcf.min)!=0) -1])
    if(length(vv)>1){
      set.seed(123)
      rang <- ranger(factory_cp_level_nc_rate~., data=OSH_jord[rg,vv], importance="impurity")
      imp <- sort(importance(rang), decreasing = TRUE)
      rank <- (length(imp):1)/length(imp)
      names(rank) <- names(imp)
      tmprank <- t2[,-2]
      tmprank[1,-1] <- 0
      tmprank[1, names(rank)] <- rank
      jrcfm <- rbind(jrcfm, tmprank)
    }
    
  }
  jpt <- OSH_jord$factory_cp_level_nc_rate[(ioffset+1):ni]
  jd <- data.frame(true=jpt,p1=jp1,pm=jpm,la=jlap,ri=jrip)
  jmse1 <- c(jmse1, mean((jpt-jp1)^2))
  jmsem <- c(jmsem, mean((jpt-jpm)^2))
  jmsela <- c(jmsela, mean((jpt-jlap)^2,na.rm=TRUE))
  jmseri<- c(jmseri, mean((jpt-jlap)^2,na.rm=TRUE))
  
  print(cor(jd,use="pairwise"))
}


jrcf2 <- jrcf1[, colSums(jrcf1 != 0) > 0]   
bigmat <- jrcf2[,-1]
rownames(bigmat) <- jrcf2$date
#bigmat <- bigmat[,-match("fac",colnames(bigmat))]
bigmat[is.na(bigmat) | bigmat==0] <- 0
dp <- colSums(bigmat)
dp
dim(bigmat)
bigmat <- t(bigmat)



Firm_general <- c("firm_size","femRatio","firm_age")
Union <- c("AccessUnion","formUnion","punUnion","refBarg","NumBarg","percUnion")
Wage <- c("tminWage")
Safety <- c("trainFirst","trainu16","trainAppr","trainApprMen","trainMen","trainWorker","trainChem")
Country <- c("HPI","Wellbeing","Ecological_Footprint","Biocapacity","GDP_per_capita","Life_Exp","Pop","counts_air","counts_conflict","GDP.per.capita","Access_electricity","Co2_emissions","Working_poverty","Minimum_wage","Informal_empl") 
business <- c("Peer.Grouping.Enabled","prefSup","contr","subContr")
wdi <- names(OSH_jord)[grep("wdi_",names(OSH_jord))]
fvp <- names(OSH_jord)[grep("fvp_",names(OSH_jord))]
reign <- names(OSH_jord)[grep("reign_",names(OSH_jord))]
vdem <- names(OSH_jord)[grep("vdem_",names(OSH_jord))]
ilo <- names(OSH_jord)[grep("ILO_",names(OSH_jord))]



group <- rep("", nrow(bigmat))

group[match(Firm_general, rownames(bigmat))] <- "BW-Data: General"
group[match(Union, rownames(bigmat))] <- "BW-Data: Union"
group[match(Wage, rownames(bigmat))] <- "BW-Data: Minimum Wage"
group[match(Safety, rownames(bigmat))] <- "BW-Data: Safety"
#group[match(Country, rownames(bigmat))] <- "Country"
group[match(business, rownames(bigmat))] <- "BW-Data: Business" 
group[match(vdem, rownames(bigmat))] <- "Vdem-Data"
group[match(reign, rownames(bigmat))] <- "Reign-Data"
group[match(fvp, rownames(bigmat))] <- "FVP-Data"
group[match(wdi, rownames(bigmat))] <- "WDI-Data"
group[match(ilo, rownames(bigmat))] <- "ILO-Data"


annotation_row = data.frame( DataClass = factor(group,ordered=T))
rownames(annotation_row) = rownames(bigmat)
clabels <- colnames(bigmat)
#ipos <- unique(c(seq(1,length(clabels),by=7),length(clabels)))

#annotation_row <- annotation_row%>%
# arrange(DataClass)

big_mat_ord<- bigmat[rownames(annotation_row), ]

#clabels[-ipos] <- ""
pheatmap(big_mat_ord, cluster_cols = FALSE, cluster_rows = F,
         #  clustering_distance_rows =  "correlation",
         #  color = c("white", "steelblue", "red"),
         color=cols,
         # legend = FALSE,
         cellheight = 20,
         cellwidth=25,
         labels_col = clabels,
         annotation_row = annotation_row,
         treeheight_row = 0, treeheight_col = 0,
         fontsize = 12,
         fontsize_col = 12,
         border_color = "gray",
         main = sprintf("%s : relative importarnce of predictors selected by Dynamic Elastic Net","Jordan"),
         angle_col=45,filename = "/Users/malik/Documents/GitHub/Thesis---Code/Plots/Heatmap_jord1.pdf",
         width=17,height =10
)

bigmat <- jcf1[,-(1:2)]
bigmat <- as.data.frame(bigmat) %>% select_if(colSums(.) != 0)
rownames(bigmat) <- jcfm$date


jcols <- makeColorRampPalette(c("red", "white",    # distances 0 to 3 colored from white to red
                                "white", "green"), # distances 3 to max(distmat) colored from green to black
                              1-max(bigmat)/diff(range(bigmat))  ,
                              100)

dp <- colSums(bigmat)
dp
dim(bigmat)
bigmat <- t(bigmat)

row.order <- try(hclust(dist(bigmat))$order, TRUE)
dat_new <- bigmat[row.order, ] # re-order matrix accoring to clustering

nba.m <- reshape2::melt(dat_new)

mycol <- colorRampPalette(c("red", "white", "blue"))(7)

Mx <- max(range(bigmat))*1.05
mx <- min(range(bigmat))*1.05
sq <- c(seq(mx,Mx,length=3))

nba.m$cat <- cut(nba.m$value,breaks=sq)
lv <- levels(nba.m$cat)
nba.m$cat <- as.character(nba.m$cat)
nba.m$cat[nba.m$value==0] <- lv[1]
nba.m$cat[nba.m$cat == "(-1e-05,0]"] <- 
nba.m$cat[nba.m$cat == "(0,1e-05]"] <- lv[6]
nba.m$cat <- sub("-1e-05","0",nba.m$cat)
nba.m$cat <- sub("1e-05","0",nba.m$cat)
lv2 <- lv
lv2 <- sub("-1e-05","0",lv2)
lv2 <- sub("1e-05","0",lv2)
lv2 <- c(lv2[1:3],"0",lv2[-(1:5)])

nba.m$cat2 <- factor(nba.m$cat, levels = lv2, ordered = TRUE )
table(nba.m$cat2)
#nba.m$Var2 <- as.Date(nba.m$Var2)
hclust(dist(bigmat)) -> oo
nba.m$Var1 <- factor(nba.m$Var1, 
                     levels = rev(oo$labels[oo$order]),ordered = TRUE)
mycol <- colorRampPalette(c("red", "white", "blue"))(2)
aa <- ggplot(nba.m, aes(Var2, Var1)) + 
  geom_tile( aes(fill = cat2),
             colour = "gray") + 
  scale_fill_manual(values=mycol, 
                    breaks=levels(nba.m$cat2) )+
  labs(x = "", y = "Variable", fill="Size of\nstandardized\ncoefficents",
       title="FacCPNC-Indonesia")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
aa
pdf("/Users/malik/Documents/GitHub/Thesis---Code/Plots/Coefs_jordnesia.pdf",width=17, height=10, pointsize = 12)
print(aa)
dev.off()


##10.Additional graphs, descriptives and analysis

##graph 

temp<- bind_rows(CL)
temp <- temp[temp$Cluster!="Systems",]
temp <- aggregate(temp$factory_cp_level_nc_rate,by=list(temp$Cluster,temp$seasnum_year),mean)
number_ticks <- function(n) {function(limits) pretty(limits, n)}

temp <- temp%>%
  dplyr::rename("Cluster"="Group.1",
         "Period"="Group.2",
         "NC.Rate"="x")

temp$Periods <- as.Date(temp$Period)
p <- ggplot(temp,aes(x=Periods,y=NC.Rate,color=Cluster))+
  geom_line()+ 
  theme(legend.position="bottom")+
  guides(fill=guide_legend(nrow=3, byrow=TRUE))+
  scale_x_date(labels=date_format("%Y"), limits=as.Date(c('2011-01-01','2020-01-01')))+
  xlab("period")+
  ylab("NC Rate")+
  geom_dl(aes(label = Cluster), method = list(dl.combine("first.points", "last.points")), cex = 0.8) 


ggsave(p,filename = "/Users/malik/Documents/GitHub/Thesis---Code/Plots/course.png")

