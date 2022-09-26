library(tidyverse)
library(lubridate)
library(readxl)

load("/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Data Sets/dataset_Vers1.RDATA")

#get the question data
data$Q.Label <- gsub("\r\n","",data$Q.Label)

cat_document <- read_excel('/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/cycle-trend-panel-creation-main/2022-02-01_All CAT questions with with Global Identifiers and all Tags.xlsx')%>%
  group_by(country_name, cluster_name, compliance_point_name,first_original_question_id,question_id,
           text,finding,question_type,global_question_mapping_id,global_text,global_finding)%>%
  summarise(n=n())%>%
  select(-n)

unique(cat_document$text[str_detect(cat_document$text ,"refuse to bargain")& (cat_document$country_name=="Vietnam" |cat_document$country_name=="Indonesia" | cat_document$country_name=="Jordan")])
unique(cat_document$text[str_detect(cat_document$text ,"refuse to bargain")& (cat_document$country_name=="Vietnam")])

refBarg<- data%>%
  select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,Cycle)%>%
  filter(Q.Label=="Does the employer refuse to bargain collectively or refuse to bargain in good faith with the union, worker representatives, union federations or confederations?" |
        Q.Label==  "Does the employer refuse to bargain collectively or refuse to bargain in good faith with the union(s)?" |
          Q.Label== "Does the employer refuse to bargain collectively or refuse to bargain in good faith with the union?" |
          Q.Label=="Does the employer refuse to bargain collectively in accordance with legal requirements, or refuse to bargain in good faith with the union, workers representation, union federation or confederation?"
      & (Country=="Indonesia" | Country=="Jordan"| Country=="Vietnam"))%>%
  rename("refBarg"=Finding)

unique(cat_document$text[str_detect(cat_document$text ,"freely form and join")& (cat_document$country_name=="Jordan")])
unique(cat_document$text[str_detect(cat_document$text ,"freely form and join")& (cat_document$country_name=="Indonesia")])
unique(cat_document$text[str_detect(cat_document$text ,"freely form and join")& (cat_document$country_name=="Vietnam")])

formUnion<- data%>%
  select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,Cycle)%>%
  filter(Q.Label=="Can workers freely form and join the union of their choice?" |
           Q.Label=="Can workers freely form and join the union of their choice ?"
         & (Country=="Indonesia" | Country=="Jordan"| Country=="Vietnam"))%>%
  rename("formUnion"=Finding)

unique(cat_document$text[str_detect(cat_document$text ,"punish workers")& (cat_document$country_name=="Jordan")])
unique(cat_document$text[str_detect(cat_document$text ,"punish workers")& (cat_document$country_name=="Indonesia")])
unique(cat_document$text[str_detect(cat_document$text ,"punish workers")& (cat_document$country_name=="Vietnam")])


punUnion<- data%>%
  select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,Cycle)%>%
  filter(Q.Label=="Does the employer punish workers for joining a union or engaging in union activities?"|
         Q.Label=="Does the employer punish workers for joining a union or engaging in union activities ?"
         & (Country=="Indonesia" | Country=="Jordan" | Country=="Vietnam"))%>%
  rename("punUnion"=Finding)

unique(cat_document$text[str_detect(cat_document$text ,"percentage of workers")& (cat_document$country_name=="Jordan")])
unique(cat_document$text[str_detect(cat_document$text ,"percentage of workers")& (cat_document$country_name=="Indonesia")])
unique(cat_document$text[str_detect(cat_document$text ,"percentage of workers")& (cat_document$country_name=="Vietnam")])



percUnion<- data%>%
  select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,Cycle)%>%
  filter(Q.Label=="What percentage of workers are union members?" 
         & (Country=="Indonesia" | Country=="Jordan" | Country=="Vietnam"))%>%
  rename("percUnion"=Finding)


unique(cat_document$text[str_detect(cat_document$text ,"percentage of women")& (cat_document$country_name=="Jordan")])
unique(cat_document$text[str_detect(cat_document$text ,"percentage of women")& (cat_document$country_name=="Indonesia")])
unique(cat_document$text[str_detect(cat_document$text ,"percentage of women")& (cat_document$country_name=="Vietnam")])


# percFemUnion<- data%>%
#   select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
#   group_by(Country,Factory.Assessed.ID,Cycle)%>%
#   filter(Q.Label=="What is the percentage of women serving on the trade union executive committee (for each union)?" 
#          & (Country=="Indonesia" | Country=="Jordan" | Country=="Vietnam"))%>%
#   rename("percFemUnion"=Finding)

unique(cat_document$text[str_detect(cat_document$text ,"have access to the workers")& (cat_document$country_name=="Jordan")])
unique(cat_document$text[str_detect(cat_document$text ,"have access to the workers")& (cat_document$country_name=="Indonesia")])
unique(cat_document$text[str_detect(cat_document$text ,"have access to the workers")& (cat_document$country_name=="Vietnam")])


accessUnion<- data%>%
  select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,Cycle)%>%
  filter((Q.Label=="Do union representatives have access to the workers in the workplace?" 
          & (Country=="Indonesia" | Country=="Jordan" | Country=="Vietnam")))%>%
  rename("AccessUnion"=Finding)

unique(cat_document$text[str_detect(cat_document$text ,"collective bargaining agreements")& (cat_document$country_name=="Jordan")])
unique(cat_document$text[str_detect(cat_document$text ,"collective bargaining agreements")& (cat_document$country_name=="Indonesia")])
unique(cat_document$text[str_detect(cat_document$text ,"collective bargaining agreements")& (cat_document$country_name=="Vietnam")])


NumBarg<- data%>%
  select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,Cycle)%>%
  filter(Q.Label=="How many collective bargaining agreements are in effect in the factory?" 
         & (Country=="Indonesia" | Country=="Jordan" | Country=="Vietnam"))%>%
  rename("NumBarg"=Finding)


unique(cat_document$text[str_detect(cat_document$text ,"PICC")& (cat_document$country_name=="Jordan")])
unique(cat_document$text[str_detect(cat_document$text ,"PICC")& (cat_document$country_name=="Indonesia")])
unique(cat_document$text[str_detect(cat_document$text ,"PICC")& (cat_document$country_name=="Vietnam")])

# PICC<- data%>%
#   select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
#   group_by(Country,Factory.Assessed.ID,Cycle)%>%
#   filter(Q.Label=="Did workers freely choose their representatives on the bipartite committee (PICC), and do workers know who their representatives are?" 
#          & (Country=="Indonesia" | Country=="Jordan"| Country=="Vietnam"))%>%
#   rename("Picc"=Finding)


unique(cat_document$text[str_detect(cat_document$text ,"Is gender a factor")& (cat_document$country_name=="Jordan")])
unique(cat_document$text[str_detect(cat_document$text ,"Is gender a factor")& (cat_document$country_name=="Indonesia")])
unique(cat_document$text[str_detect(cat_document$text ,"gender a factor")& (cat_document$country_name=="Vietnam")])


trainGen<- data%>%
  select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,Cycle)%>%
  filter(Q.Label=="Is gender a factor in decisions regarding opportunities for promotion or access to training?" 
         & (Country=="Indonesia" | Country=="Jordan" | Country=="Vietnam"))%>%
  rename("trainGen"=Finding)


unique(cat_document$text[str_detect(cat_document$text ,"chemicals")& (cat_document$country_name=="Jordan")])
unique(cat_document$text[str_detect(cat_document$text ,"chemicals")& (cat_document$country_name=="Indonesia")])
unique(cat_document$text[str_detect(cat_document$text ,"chemicals")& (cat_document$country_name=="Vietnam")])



trainChem<- data%>%
  select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,Cycle)%>%
  filter(Q.Label=="Has the employer effectively trained workers who work with chemicals and hazardous substances?" 
         & (Country=="Indonesia" | Country=="Jordan" | Country=="Vietnam"))%>%
  rename("trainChem"=Finding)


unique(cat_document$text[str_detect(cat_document$text ,"first-aid training")& (cat_document$country_name=="Jordan")])
unique(cat_document$text[str_detect(cat_document$text ,"first-aid")& (cat_document$country_name=="Indonesia")])
unique(cat_document$text[str_detect(cat_document$text ,"first-aid training")& (cat_document$country_name=="Vietnam")])


trainFirst<- data%>%
  select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,Cycle)%>%
  filter(Q.Label=="Has the employer provided first-aid training for workers?" 
         & (Country=="Indonesia" | Country=="Jordan" | Country=="Vietnam"))%>%
  rename("trainFirst"=Finding)

unique(cat_document$text[str_detect(cat_document$text ,"under age 18")& (cat_document$country_name=="Jordan")])
unique(cat_document$text[str_detect(cat_document$text ,"under age 18")& (cat_document$country_name=="Indonesia")])
unique(cat_document$text[str_detect(cat_document$text ,"under age 18")& (cat_document$country_name=="Vietnam")])


trainu16<- data%>%
  select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,Cycle)%>%
  filter(Q.Label=="Are all workers who are under age 18 and doing hazardous work (i) at least 16 years old; (ii) working in accordance with Jordanian law; (iii) working in such a way that their health, safety and morals are fully protected; and (iv) adequately trained to do the work safely?"| 
           Q.Label=="Are there any workers under age 18 who are doing hazardous work and who are (i) under 16 years old; (ii) working more than 7 hours/day or 42 hours/week; (vi) working overtime or at night; (iii) working in such a way that their health, safety and morals are not fully protected; or (iv) not adequately trained to do the work safely?"|
           Q.Label=="Are all workers who are under age 15 working (a) in accordance with national regulations regarding light work or (b) in a government-approved training program?" 
         & (Country=="Indonesia" | Country=="Jordan" | Country=="Vietnam"))%>%
  rename("trainu16"=Finding)

# unique(cat_document$text[str_detect(cat_document$text ,"How many vocational")& (cat_document$country_name=="Jordan")])
# unique(cat_document$text[str_detect(cat_document$text ,"vocational")& (cat_document$country_name=="Indonesia")])
# unique(cat_document$text[str_detect(cat_document$text ,"vocational")& (cat_document$country_name=="Vietnam")])
# 
# 
# trainVoc<- data%>%
#   select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
#   group_by(Country,Factory.Assessed.ID,Cycle)%>%
#   filter(Q.Label=="How many vocational trainees are employed by the factory?" 
#          & (Country=="Indonesia" | Country=="Jordan" | Country=="Vietnam"))%>%
#   rename("trainVoc"=Finding)


unique(cat_document$text[str_detect(cat_document$text ,"training workers")& (cat_document$country_name=="Jordan")])
unique(cat_document$text[str_detect(cat_document$text ,"training")& (cat_document$country_name=="Indonesia")])
unique(cat_document$text[str_detect(cat_document$text ,"training workers")& (cat_document$country_name=="Vietnam")])


trainWorker<- data%>%
  select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,Cycle)%>%
  filter(Q.Label=="How many training workers are employed by the factory?" 
         & (Country=="Indonesia" | Country=="Jordan" | Country=="Vietnam"))%>%
  rename("trainWorker"=Finding)

trainMen<- data%>%
  select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,Cycle)%>%
  filter(Q.Label=="How many of the training workers are men?" 
         & (Country=="Indonesia" | Country=="Jordan" | Country=="Vietnam"))%>%
  rename("trainMen"=Finding)

# trainVocMen<- data%>%
#   select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
#   group_by(Country,Factory.Assessed.ID,Cycle)%>%
#   filter(Q.Label=="How many of the vocational trainees are men?" 
#          & (Country=="Indonesia" | Country=="Jordan" | Country=="Vietnam"))%>%
#   rename("trainVocMen"=Finding)


unique(cat_document$text[str_detect(cat_document$text ,"apprentice")& (cat_document$country_name=="Jordan")])
unique(cat_document$text[str_detect(cat_document$text ,"apprentice")& (cat_document$country_name=="Indonesia")])
unique(cat_document$text[str_detect(cat_document$text ,"training/apprentice")& (cat_document$country_name=="Vietnam")])

trainAppr<- data%>%
  select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,Cycle)%>%
  filter(Q.Label=="How many training/apprentice workers are employed by the factory?" |
           Q.Label=="How many apprentices are employed by the factory in production?"
         & (Country=="Indonesia" | Country=="Jordan" | Country=="Vietnam"))%>%
  rename("trainAppr"=Finding)

trainApprMen<- data%>%
  select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,Cycle)%>%
  filter(Q.Label=="How many training/apprentice workers are men?" |
           Q.Label=="How many of the apprentices in production are men?"
         & (Country=="Indonesia" | Country=="Jordan" | Country=="Vietnam"))%>%
  rename("trainApprMen"=Finding)

trainOSH<- data%>%
  select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,Cycle)%>%
  filter(Q.Label=="Are workers effectively trained on occupational health and safety?" 
         & (Country=="Indonesia" | Country=="Jordan" | Country=="Vietnam"))%>%
  rename("trainOSH3"=Finding)

unique(cat_document$text[str_detect(cat_document$text ,"minimum wage")& (cat_document$country_name=="Jordan")])
unique(cat_document$text[str_detect(cat_document$text ,"minimum wage")& (cat_document$country_name=="Indonesia")])
unique(cat_document$text[str_detect(cat_document$text ,"minimum wage")& (cat_document$country_name=="Vietnam")])


minimumWage<- data%>%
  select(Country,Cycle,Year,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,Cycle)%>%
  filter(Q.Label=="Does the employer pay the correct district minimum wage for ordinary hours of work to permanent full time workers (PKWTT) ?" |
           Q.Label=="Does the employer pay at least minimum wage for ordinary hours of work to regular full time workers?"|
         Q.Label=="Does the employer pay at least the applicable legal minimum wage for ordinary hours of work to regular full time workers?"
         & (Country=="Indonesia" | Country=="Jordan" | Country=="Vietnam"))%>%
  rename("tminWage"=Finding)




countries <- c("Indonesia","Jordan","Vietnam")

temp_func <- function(data) {
  latest_date_for_each_factory <- data %>%
    group_by( Cluster,Country,Factory.Assessed.ID,Factory.Assessed.Name,Cycle,Year)%>%
    summarise(Quest.ID = max(Quest.ID))
  temp_df <- inner_join(data,latest_date_for_each_factory,
                        by = c('Cluster','Country',"Factory.Assessed.ID",'Factory.Assessed.Name','Cycle','Quest.ID','Year'))
  temp_df <- temp_df[temp_df$Question.type != "FGW",]
  temp_df$Finding <- as.integer(temp_df$Finding)
  compliance_calc <- temp_df%>%
    group_by(Cluster,Country,Factory.Assessed.ID,Factory.Assessed.Name, Cycle, Year,Quest.ID,CP)%>%
    summarise(findings = sum(Finding))
  compliance_calc$non_compliant <-ifelse(compliance_calc$findings >0,1,0)
  compliance_df <- compliance_calc%>%
    group_by(Cluster,Country,Factory.Assessed.ID,Factory.Assessed.Name,Cycle,Year,CP)%>%
    summarise(total_factories=n(),number_non_compliant= sum(non_compliant), percent_non_cp = sum(non_compliant)/n())
  factory_level_average_nc <- compliance_df%>%
    group_by(Cluster,Country,Factory.Assessed.ID,Factory.Assessed.Name,Cycle,Year)%>%
    summarise(total_cp_nc = sum(number_non_compliant), total_cps = n(), factory_cp_level_nc_rate = sum(number_non_compliant)/n())
}

cps2<- list()
for (i in 1:length(Cl)) {
  cps2[[i]] <-  temp_func(Cl[[i]])
}

cps2_all <- bind_rows(cps2)


cps2_all_fil <- cps2_all %>% filter(Country %in% countries)
cps2_all_fil%>%filter(Cycle==3 & Year == 2020)

end_period_2021 <- as.Date("31/12/2021","%d/%m/%Y")
all_factories_ever_in_programme <- read_excel('/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/cycle-trend-panel-creation-main/Enterprise_Report_All_01-02-2022.xlsx', skip = 1)%>%
  rename(Creation..Data = `Creation  Data`)%>%
   rename(Country = Country...11)%>%
   mutate(ID = as.integer(ID))%>%
   filter(Status %in% 'Active')%>%
  filter(as.Date(Creation..Data,"%d/%m/%Y") <= end_period_2021)
all_factories_ever_in_programme <- all_factories_ever_in_programme %>%
  rename("Factory.Assessed.Name"=Name)
cpsx <- left_join(cps2_all_fil,all_factories_ever_in_programme,by=c('Country',"Factory.Assessed.Name"))

cpsx <- cpsx %>%
  arrange(Cluster,Year,Cycle) %>%
  group_by(Cluster,Factory.Assessed.ID) %>%
  mutate(ratio = factory_cp_level_nc_rate-first(factory_cp_level_nc_rate))
cpsx <- cpsx %>% arrange(Cluster,Country,Factory.Assessed.ID) 

cpsx <- cpsx %>%
  
  left_join(.,refBarg[,c("Factory.Assessed.ID","Country","Cycle","Year","refBarg")],
              by=c('Cycle','Country','Year',"Factory.Assessed.ID")) %>%
  
  
  left_join(.,formUnion[,c("Country","Cycle","Year","Factory.Assessed.ID","formUnion")],
            by=c('Cycle','Country','Year',"Factory.Assessed.ID")) %>%
  
  left_join(.,accessUnion[,c("Country","Cycle","Year","Factory.Assessed.ID","AccessUnion")],
            by=c('Cycle','Country','Year',"Factory.Assessed.ID"))%>%

  left_join(.,NumBarg[,c("Country","Cycle","Year","Factory.Assessed.ID","NumBarg")],
                  by=c('Cycle','Country','Year',"Factory.Assessed.ID"))%>%
        
        left_join(.,minimumWage[,c("Country","Cycle","Year","Factory.Assessed.ID","tminWage")],
                  by=c('Cycle','Country','Year',"Factory.Assessed.ID"))%>%
        
        
  
  left_join(.,punUnion[,c("Country","Cycle","Year","Factory.Assessed.ID","punUnion")],
            by=c('Cycle','Country','Year',"Factory.Assessed.ID"))%>%
  
  left_join(.,percUnion[,c("Country","Cycle","Year","Factory.Assessed.ID","percUnion")],
            by=c('Cycle','Country','Year',"Factory.Assessed.ID"))%>%
  
  left_join(.,trainAppr[,c("Country","Cycle","Year","Factory.Assessed.ID","trainAppr")],
            by=c('Cycle','Country','Year',"Factory.Assessed.ID"))%>%
  
  left_join(.,trainApprMen[,c("Country","Cycle","Year","Factory.Assessed.ID","trainApprMen")],
            by=c('Cycle','Country','Year',"Factory.Assessed.ID"))%>%
  
  left_join(.,trainChem[,c("Country","Cycle","Year","Factory.Assessed.ID","trainChem")],
            by=c('Cycle','Country','Year',"Factory.Assessed.ID"))%>%
  
  left_join(.,trainFirst[,c("Country","Cycle","Year","Factory.Assessed.ID","trainFirst")],
            by=c('Cycle','Country','Year',"Factory.Assessed.ID"))%>%
  
  left_join(.,trainGen[,c("Country","Cycle","Year","Factory.Assessed.ID","trainGen")],
            by=c('Cycle','Country','Year',"Factory.Assessed.ID"))%>%
  
  
  left_join(.,trainMen[,c("Country","Cycle","Year","Factory.Assessed.ID","trainMen")],
            by=c('Cycle','Country','Year',"Factory.Assessed.ID"))%>%
  
  left_join(.,trainOSH[,c("Country","Cycle","Year","Factory.Assessed.ID","trainOSH3")],
            by=c('Cycle','Country','Year',"Factory.Assessed.ID"))%>%
  
  left_join(.,trainu16[,c("Country","Cycle","Year","Factory.Assessed.ID","trainu16")],
            by=c('Cycle','Country','Year',"Factory.Assessed.ID"))%>%
  
  left_join(.,trainWorker[,c("Country","Cycle","Year","Factory.Assessed.ID","trainWorker")],
            by=c('Cycle','Country','Year',"Factory.Assessed.ID"))


cpsx <- cpsx[!is.na(cpsx$ID),]


happy_ind <- happy_ind%>%
  rename(Year=year)
cpsx <- cpsx %>%
  
left_join(.,happy_ind,
            by=c('Country','Year'))

counts_confl <- counts_confl%>%
  rename(Year=year,
         Country=country,
         counts_conflict=rowCount)

cpsx <- cpsx %>%
  
  left_join(.,counts_confl,
            by=c('Country','Year'))
counts_air <- counts_air%>%
  rename(
         Country=country,
         counts_air=rowCount)

cpsx <- cpsx %>%
  
  left_join(.,counts_air,
            by=c('Country'))

# edu_df <- edu_df%>%
#   rename(
#     Country=country,
#     Year=year)
# 
# cpsx <- cpsx %>%
#   
#   left_join(.,edu_df,
#             by=c('Country','Year'))
# factory_ids_we_want <- cpsx%>%
#   group_by(Factory.Assessed.ID)%>%
#   summarise(largest_cycle = max(Cycle))%>%
#   filter(largest_cycle >= 10)

cpsx <- cpsx[!is.na(cpsx$ID),]


save(cpsx,data,happy_ind,edu_df,counts_air,counts_confl,file="/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Data Sets/dataset_Vers1.RDATA")


  