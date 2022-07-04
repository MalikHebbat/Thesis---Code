##Version 1.5 (Malik, 22/06/2022)
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
library("wid")
library(conflictr)
library(states)
library(ggplot2)
library(dplyr)
library(WDI)
library(tidygeocoder)
library(data.table)
library(factoextra)
library(FactoMineR)
library(readr)
library(NbClust)
library(sf)
library(osmdata)
library(cluster)

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
counts_confl <- events2010[, .(rowCount = .N), by = list(year,country)]
counts_confl

#Number of Conflicts per region and year
ggplot(counts_confl,aes(x=year,y=rowCount))+geom_bar(stat="identity")+
  ylab("Conflict counts (all events)") +
  ggtitle("Number of Conflicts 2010-2017")+
  facet_wrap(~country)

#WIID

wiid <- readxl::read_excel("/Users/malik/Downloads/wiidcountry_0_0.xlsx")
wiid <- wiid[which(wiid$c2 %in% country_iso ),]


#geospatial health facalities

library(rhealthsites)
library(sf)
hs_set_api_key("99d44d44df3139072691b371f98d74b1001e1b9e")


hs <- list()
for (country in data_range$Country) {
  hs[[country]] <- hs_facilities(country = country)
  hs[[country]]$country <- country
}

hs <- dplyr::bind_rows(hs)

ggplot(hs,aes(x=amenity,fill=amenity))+
  geom_bar()+
  facet_wrap( ~country,drop = TRUE)+
  coord_flip()


#Education and work conditions (only examples, a lot more, we have to choose)
#seee edu vector
edu <- WDIsearch("Education")
View(edu)

x <- WDI(indicator=c("fin1.t.a.5",
                     "fin15.t.a.5",
                     "JI.EMP.1524.HE.ZS",
                     "JI.EMP.1564.HE.ZS",
                     "JI.EMP.CONT.HE.ZS",
                     "JI.EMP.UNPD.LE.ZS",
                     "SE.SEC.ENRL.GC.FE.ZS"))

edu_df <- x[which(x$country %in% data_range$Country),]


#airports near capital (100 km)
library(maps)
library(airportr)
library(countrycode)

data(world.cities)
city <- unique(world.cities$name[which(world.cities$country.etc %in% data_range$Country &
                                         world.cities$capital==1)])
osm <- list()
for (city in city) {
  address_components <- tribble(
    ~cty,
    city
    
  )
  
  osm[[city]] <- geo(
    address = address_components$cty, method = "osm",
    lat = latitude, long = longitude
  )
}

city <- bind_rows(osm)

airp <- list()

for ( i in 1:NROW(city)) {
  
  airp[[i]] <- airports_around(lat=city[i,2],
                               lon=city[i,3],
                               distance=100)
  
}

airp <- bind_rows(airp)
airp <- airp[airp$`Country Code (Alpha-2)`!="IN" & 
               airp$`Country Code (Alpha-2)`!="DO" & 
               airp$`Country Code (Alpha-2)`!="IL" & 
               airp$`Country Code (Alpha-2)`!="PS",] 
airp <- as.data.table(airp)
counts_air  = airp[, .(rowCount = .N), by = `Country Code (Alpha-2)`]
counts_air$country <- countrycode(counts_air$`Country Code (Alpha-2)`, "iso2c", "country.name")

ggplot(counts_air,aes(x=country,y=rowCount))+geom_bar(stat="identity")+
  ggtitle("Number of airports near capital (100 km)")+
  ylab("Number of airports")+
  xlab("")

library(xlsx)

setwd("/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Data Sets/Happy")
list.files <- list()
for (year in 2010:2020) {
  
  list.files <- append(list.files, paste0("Kopie von happy-planet-index","-",year,".xlsx"))
}

list.files <- unlist(list.files)
x <- list()

for (file in list.files) {
  
  x[[file]] <- read.xlsx(file=file,sheetIndex = 2)
  x[[file]] <- x[[file]][-c(1:7),]
  x[[file]] <- x[[file]]%>%
    select(-'NA..2')
  x[[file]] <- x[[file]] %>%
    rename(HPI_rank = 'NA.') %>%
    rename(Country = 'X1..Rankings.for.all.countries..2006...2020') %>%
    rename(ISO = 'NA..1') %>%
    rename(Continent = 'NA..3') %>%
    rename(Pop = 'NA..4') %>%
    rename(Life_Exp = 'NA..5') %>%
    rename(Wellbeing = 'NA..6') %>%
    rename(Ecological_Footprint = 'NA..7') %>%
    rename(HPI = 'NA..8') %>%
    rename(Biocapacity = 'NA..9') %>%
    rename(GDP_per_capita = 'NA..10')
  x[[file]] <- x[[file]] %>%
    mutate(HPI_rank = as.numeric(HPI_rank)) %>%
    mutate(Pop = as.numeric(Pop)) %>%
    mutate(Life_Exp = as.numeric(Life_Exp)) %>%
    mutate(Wellbeing = as.numeric(Wellbeing)) %>%
    mutate(Ecological_Footprint = as.numeric(Ecological_Footprint)) %>%
    mutate(HPI = as.numeric(HPI)) %>%
    mutate(Biocapacity = as.numeric(Biocapacity)) %>%
    mutate(GDP_per_capita = as.numeric(GDP_per_capita))
  x[[file]]$year <- parse_number(gsub("-"," ",file))
}


happy_ind <- bind_rows(x)
happy_ind <- happy_ind[which(happy_ind$Country %in% data_range$Country),]


###just a few visualizations to check the data

ggplot(happy_ind, aes(x=GDP_per_capita, y=HPI)) +
  geom_point(aes(size=Pop, color=Country)) +
  geom_smooth(method = 'loess') +
  ggtitle('GDP per Capita(log10) and Happy Planet Index Score') + 
  coord_trans(x = 'log10')+
  facet_wrap(~year)

qplot(x=Var1, y=Var2, data=reshape2::melt(cor(happy_ind[, 5:11], use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Heatmap of Correlation Matrix", 
       x=NULL, y=NULL)

happy_ind.pca <- PCA(happy_ind[, 5:11], graph=FALSE)
print(happy_ind.pca)

fviz_screeplot(happy_ind.pca, addlabels = TRUE, ylim = c(0, 65))
fviz_pca_var(happy_ind.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
)

number <- NbClust(happy_ind[, 5:11], distance="euclidean",
                  min.nc=2, max.nc=15, method='ward.D', index='all', alphaBeale = 0.1)

set.seed(2017)
pam <- pam(na.omit(happy_ind[, 5:11]), diss=FALSE, 2, keep.data=TRUE)
fviz_silhouette(pam)
fviz_cluster(pam, geom = "point",
             ellipse.type = "norm")
happy_ind$Country[pam$id.med]


#geospatial schools in capitals

city <- unique(world.cities$name[which(world.cities$country.etc %in% data_range$Country & (world.cities$capital==1 ))]) 

q <- list()
for (cit in city) {
  q[[cit]] <- getbb(cit,limit=20) %>%
    opq() %>%
    add_osm_feature("amenity","school") %>%
    osmdata_sf()
  if (NROW(q[[cit]]$osm_polygons!=0)) {
    q[[cit]] <- q[[cit]]$osm_polygons[,c("name","amenity")]
    q[[cit]]$city <- cit
  } else { 
    q[[cit]] <- NULL
  }
}

schools <- bind_rows(q)
names_w<- names(world.cities)
names_w[c(1,2)] <- c("city","country") 
names(world.cities) <- names_w
tmp <- world.cities[,c(1,2)]

#schools in capitals
schools <- merge(schools,tmp,by="city")

save(c,c1,Cl,hs,edu_df,events2010,counts_confl,wiid,airp,counts_air,happy_ind,schools,file="/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Data Sets/BW_extende.RDATA")
save(c,c1,hs,edu_df,events2010,counts_air,wiid,airp,counts_confl,happy_ind,schools,file="/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Data Sets/supplementaryConflictWIDHSEDU.RDATA")
