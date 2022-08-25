library(tidyverse)
library(ggbeeswarm)
library(ggforce)
library(sjPlot)
library(lubridate)

firm_size <- data%>%
  select(Country,Cycle,Factory.Assessed.ID,Q.Label,Question.ID,Finding)%>%
  group_by(Country,Factory.Assessed.ID,Cycle)%>%
  filter(Q.Label=="How many total workers are employed by the factory?" | (Country=="Ethiopia" & Question.ID==14611 ) | (Country=="Indonesia" & Question.ID==12004))

firm_size$Finding <- gsub(",","",firm_size$Finding)
firm_size$Finding <- gsub(".0","",firm_size$Finding)
firm_size$Finding <- as.numeric(parse_number(firm_size$Finding))


temp_func <- function(data) {
  latest_date_for_each_factory <- data %>%
    group_by( Cluster,Country,Factory.Assessed.ID,Cycle)%>%
    summarise(Quest.ID = max(Quest.ID))
  temp_df <- inner_join(data,latest_date_for_each_factory,
                        by = c('Cluster','Country','Factory.Assessed.ID','Cycle','Quest.ID'))
  temp_df <- temp_df[temp_df$Question.type != "FGW",]
  temp_df$Finding <- as.integer(temp_df$Finding)
  compliance_calc <- temp_df%>%
    group_by(Cluster,Country,Factory.Assessed.ID, Cycle, Quest.ID,CP)%>%
    summarise(findings = sum(Finding))
  compliance_calc$non_compliant <-ifelse(compliance_calc$findings >0,1,0)
  compliance_df <- compliance_calc%>%
    group_by(Cluster,Country,Factory.Assessed.ID,Cycle,CP)%>%
    summarise(total_factories=n(),number_non_compliant= sum(non_compliant), percent_non_cp = sum(non_compliant)/n())
  factory_level_average_nc <- compliance_df%>%
    group_by(Cluster,Country,Factory.Assessed.ID,Cycle)%>%
    summarise(total_cp_nc = sum(number_non_compliant), total_cps = n(), factory_cp_level_nc_rate = sum(number_non_compliant)/n())
}



cps2<- list()
for (i in 1:length(Cl)) {
  cps2[[i]] <-  temp_func(Cl[[i]])
}

cps2_all <- bind_rows(cps2)


cps_data_new <-left_join(cps2_all,firm_size,by=c('Country',"Factory.Assessed.ID","Cycle"))


cps_data_new$size_cat <- ifelse(cps_data_new$Finding<=10,"micro",
                                ifelse(cps_data_new$Finding<=50,"small",
                                       ifelse(cps_data_new$Finding<=250,"medium",
                                              ifelse(cps_data_new$Finding>250,"large",""))))

cps_data_new$size_cat <- cps_data_new$size_cat


cps_data_new <- na.omit(cps_data_new[cps_data_new$Finding>0,])
cps_data_new <- cps_data_new[cps_data_new$Cluster!="Systems",]


ggplot(cps_data_new, aes(x=Cycle, y=factory_cp_level_nc_rate)) +
  geom_point(aes(size=Finding,color=size_cat)) +
  geom_smooth(method="loess") +
  ggtitle('')+
  facet_wrap(~Cluster)

ggsave("/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Plots/trendBW_C.png")

ggplot(cps_data_new,aes(y=factory_cp_level_nc_rate,x=Country)) +
  geom_point(aes(size=Finding, color=size_cat))+
  facet_wrap( ~Cluster)+
  coord_flip()

ggsave("/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Plots/dependency.png")


ggplot(cps_data_new,aes(x=factory_cp_level_nc_rate)) +
  geom_density()+
  facet_wrap( ~Country)


ggplot(cps_data_new,aes(x=factory_cp_level_nc_rate, fill=Country)) +
  geom_density()+
  facet_wrap( ~Cluster)+
  ylim(0,50)+
  theme(legend.position="bottom")

ggsave("/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Plots/distrBW_C.png")

ggplot(cps_data_new,aes(x=Finding)) +
  geom_density()+
  facet_wrap( ~Country)


Widt_of_ts <- data %>% 
group_by(Country) %>% 
  summarize(Years = max(year(date_final))-min(year(date_final)),
            
            Cycles=max(Cycle),
            width=max(Cycle)*Years,)
write.csv(Widt_of_ts, "/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Plots/Tables/width.csv", row.names = T)


summary(cps_data_new)

sum_cp <- tapply(cps_data_new$factory_cp_level_nc_rate, cps_data_new$Country, summary)
sum_cp_f <- as.data.frame(bind_rows(sum_cp))
rownames(sum_cp_f) <- names(sum_cp) 

write.csv(sum_cp_f, "/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Plots/Tables/Orange_summ.csv", row.names = T)


summary(cps_data_new$factory_cp_level_nc_rate
        )
sum_cp <- tapply(cps_data_new$Finding, cps_data_new$Country, summary)
sum_cp_f <- as.data.frame(bind_rows(sum_cp))
rownames(sum_cp_f) <- names(sum_cp) 
write.csv(sum_cp_f, "/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Plots/Tables/size_distr.csv", row.names = T)
summary(cps_data_new$Finding
)

temp <- cps_data_new[!duplicated(cps_data_new$Factory.Assessed.ID),]
cat_tab <- table(temp$size_cat)



write.csv(cat_tab, "/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Plots/Tables/cat_tab.csv", row.names = F)


skew <- cps_data_new %>% 
  group_by(Cluster,Country) %>%
  summarise(Skew = skewness(factory_cp_level_nc_rate,na.rm = T), Kurtosis = kurtosis(factory_cp_level_nc_rate))
write.csv(skew, "/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Plots/Tables/skew.csv", row.names = F)


temp <- data %>%
  group_by(Factory.Assessed.ID)%>%
  summarise("length_participation"=max(Cycle))

temp1 <- inner_join(cps_data_new,temp,by=c("Factory.Assessed.ID"))

ggplot(temp1,aes(x=length_participation,y=factory_cp_level_nc_rate))+
  geom_point(aes(size=Finding))+
  facet_wrap(~Cluster)
plot(temp)
