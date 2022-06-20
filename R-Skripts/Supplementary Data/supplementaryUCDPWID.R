library("wid")
library(conflictr)
library(states)
library(ggplot2)


#WID World inequality database
#use prepared BWdata and set wd to "wid_all_data" folder
load("/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Data Sets/BetterWorksClustered.RData")
setwd("/Users/malik/Downloads/wid_all_data")
country_iso <- countrycode::countrycode(sourcevar = data_range$Country,
                         origin = "country.name", destination = "iso2c")
list.files <- list()
for(country in country_iso) {
list.files <- append(list.files, paste0("WID_DATA","_",country,".csv"))
}
x <- lapply(list.files, read.csv2)

c <- dplyr::bind_rows(x)
c1 <- c[c$percentile=="p0p100" & c$age=="999" & c$pop=="i",]
c1$value <- round(as.numeric(c1$value),3)
c$value <- round(as.numeric(c$value),3)


### Some interesting plots using the WID package, not feasable to download the whole set
###Bottom 50% pre-tax national income

aptinc <- download_wid(
  indicators = "aptinc", # Average pre-tax national income
  areas = country_iso, 
  perc = "p0p50", # Bottom half of the population
  pop = "j", # Equal-split individuals
  year = 2010:2022
) %>% rename(value_lcu = value)

ppp <- download_wid(
  indicators = "xlcusp", # US PPP
  areas = country_iso, 
  year = 2021 # Reference year only
) %>% rename(ppp = value) %>% select(-year, -percentile)

aptinc <- merge(aptinc, ppp, by = "country") %>%
  mutate(value_ppp = value_lcu/ppp)


ggplot(aptinc) +
  geom_line(aes(x = year, y = value_ppp, color = country)) +
  ylab("2021 $ PPP") +
  ggtitle("Bottom 50% pre-tax national income")

#top wealth shares

shareweal <- download_wid(
  indicators = "shweal", # Shares of personal wealth
  areas = country_iso, 
  perc = c("p90p100", "p99p100"), # Top 1% and top 10%
  year = 2010:2022
)

ggplot(shareweal) +
  geom_line(aes(x = year, y = value, color = percentile)) +
  ylab("top share") +
  scale_color_discrete(labels = c("p90p100" = "top 10%", "p99p100" = "top 1%")) + 
  ggtitle("Top 1% and top 10% personal wealth shares")+
  facet_wrap(~country)


## Evolution of national income over long period
anninc <- download_wid(
  indicators = "anninc", # Average net national income
  areas = country_iso,
  ages = 992, # Adults
  year = 2010:2022
) %>% rename(value_lcu = value)
# Purchasing power parities with US dollar

ppp <- download_wid(
  indicators = "xlcusp", # US PPP
  areas = country_iso, # France, China and United States
  year = 2020 # Reference year only
) %>% rename(ppp = value) %>% select(-year, -percentile)
# Convert from local currency to PPP US dollar

anninc <- merge(anninc, ppp, by = "country") %>%
  mutate(value_ppp = value_lcu/ppp)

ggplot(anninc) +
  geom_line(aes(x = year, y = value_ppp, color = country)) +
  scale_y_log10(breaks = c(2e3, 5e3, 1e4, 2e4, 5e4)) +
  ylab("2016 $ PPP") +
  ggtitle("Average net national income per adult")

  
## Divergence of incomes 

tpinc <- download_wid(
  indicators = "tptinc", # Thresholds of pre-tax national income
  areas = country_iso,
  perc = c("p10p100", "p50p100", "p90p100", "p99p100", "p99.9p100"),
  year=2010:2022
)

tpinc2010 <- tpinc %>% filter(year == 2010) %>%
  rename(value2010 = value) %>%
  select(-year)

tpinc <- merge(tpinc, tpinc2010, by = c("country", "percentile")) %>%
  mutate(value = 100*value/value2010)


ggplot(tpinc) +
  geom_line(aes(x = year, y = value, color = percentile)) +
  ylab("2010 = 100") +
  scale_color_discrete(
    labels = c("p10p100" = "P10", "p50p100" = "P50", "p90p100" = "P90",
               "p99p100" = "P99", "p99.9p100" = "P99.9")
  ) + 
  ggtitle("Divergence of pre-tax national income")+
  facet_wrap(~country)



#Conflict Data

#LOad Glieditsch-Ward codes of independent countries
data("gwstates")
gwstates <- gwstates[order(gwstates$country_name),]
countries <- data_range$Country
countries[countries=="Vietnam"] <- "Vietnam, Democratic Republic of"
countries[countries=="Cambodia"] <- "Cambodia (Kampuchea)"
states <- subset(gwstates, country_name %in% countries)
gwcode <- unique(states$gwcode)
ucdp.ged<-getUCDP(db="gedevents", version="18.1",location=gwcode)
years=c(2010:2017)
events2010 <- ucdp.ged[which(ucdp.ged$year %in% years),]
counts <- events2010[, .(rowCount = .N), by = list(year,country)]
counts

#Number of Conflicts per region and year
ggplot(counts,aes(x=year,y=rowCount))+geom_bar(stat="identity")+
ylab("Conflict counts (all events)") +
ggtitle("Number of Conflicts 2010-2017")+
facet_wrap(~country)

#WIID

wiid <- readxl::read_excel("/Users/malik/Downloads/wiidcountry_0_0.xlsx")
wiid <- wiid[which(wiid$c2 %in% country_iso ),]


