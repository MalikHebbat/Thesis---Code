library(readxl)
library(dplyr)
library(tidyr)
library(fuzzyjoin)

setwd("/Users/fabiothoma/Library/Mobile Documents/com~apple~CloudDocs/Documents/Master/Thesis/")
load("Data/Splitted_v5Nov.RData") # This is the latest file

# Only for OSH now

OSH <- CL$`Occupational Safety and Health`

# For now only for Vietnam

OSH_Viet <- OSH %>% filter(Country == "Vietnam")

Vietnam_factories <- read_excel("Data/Vietnam_factories.xlsx")

# Delete NAs

Vietnam_factories <- Vietnam_factories %>% drop_na(name)

#OSH_Vietnam <- left_join(Vietnam, Vietnam_factories, by = c("Factory.Assessed.Name"= "name"))

OSH_Viet <- OSH_Viet %>%
  stringdist_inner_join(Vietnam_factories, c("Factory.Assessed.Name"= "name"), max_dist = 2,
                        distance_col = "distance")


#OSH_Viet  %>% select(Factory.Assessed.Name, name, distance) %>% filter(distance==2) %>% View()
# Issues encountered:
# 1)  3Q Vina Co., Ltd.

# Lat 10.713750523238282, lng 106.6189660395102


#OSH_Viet <- OSH_Viet %>%                               # Replacing values
#  mutate(lat = replace(lat, Factory.Assessed.Name == "3Q Vina Co., Ltd.", 10.713750523238282)) %>%
#  mutate(lng = replace(lng, Factory.Assessed.Name == "3Q Vina Co., Ltd.", 106.6189660395102)) 

change_lat_lng <- function(Firm_name,lat,lng){
  OSH_Viet <- OSH_Viet %>%                               # Replacing values
    mutate(lat = replace(lat, Factory.Assessed.Name == Firm_name, lat)) %>%
    mutate(lng = replace(lng, Factory.Assessed.Name == Firm_name, lng)) 
}


OSH_Viet <- change_lat_lng("3Q Vina Co., Ltd.",10.713750523238282,106.6189660395102)


# Double check
#OSH_Viet %>% filter(Factory.Assessed.Name == "3Q Vina Co., Ltd.") %>% select(Factory.Assessed.Name, name, lat,lng)


# 2) K.J. Vina Co., Ltd.
  # Km 9 +700 Provincial Road 379, Dai Hanh, Hoan Long Commune, Yen My District, Hung Yen Province
  #Lat 20.904501098198132, lng 105.97881490493396


OSH_Viet <- change_lat_lng("K.J. Vina Co., Ltd.",20.904501098198132,105.97881490493396)

# GG Vietnam Co., Ltd.

# 20.92112106647287, 106.11759822329604

OSH_Viet <- change_lat_lng("GG Vietnam Co., Ltd.",20.92112106647287,106.11759822329604)


# KY Vina Co., Ltd.

# 11.049505956691295, 106.62866353068159

OSH_Viet <- change_lat_lng("KY Vina Co., Ltd.",11.049505956691295,106.62866353068159)


# UDY Vina Co., Ltd.

# 10.921055494630878, 106.76318944052515

OSH_Viet <- change_lat_lng("UDY Vina Co., Ltd.",10.921055494630878,106.76318944052515)



# JS Vina Co., Ltd.

# 11.11675126465964, 106.79535348236207

OSH_Viet <- change_lat_lng("JS Vina Co., Ltd.",11.11675126465964,106.79535348236207)


# Nam Son Company Limited

# 21.02954671087933, 105.95695014135171

OSH_Viet <- change_lat_lng("Nam Son Company Limited",21.02954671087933,105.95695014135171)


# Check result
mean(complete.cases(OSH_Viet$lat))
mean(complete.cases(OSH_Viet$lat))
# Looks good (100%)


#mean(complete.cases(OSH_Viet$parent_company))

# Just 12 %, so can be rather disregarded


# Merge with other df 

# 1) World Cities
#

cities <- read.csv("Data/City/World_Cities.csv")

# Only cities above 500k (does that make sense?)
#library(reshape2)
cities1 <- cities %>% filter(POP>500000)

vietnam_cities <- cities1 %>% filter(CNTRY_NAME == "Vietnam")

vietnam_cities1 <- vietnam_cities %>% select(CITY_NAME,X,Y)

#vietnam_cities1 <- dcast(melt(vietnam_cities1, id.var="CITY_NAME"), 1~CITY_NAME+variable)

#joined_dists <- cbind(joined_dists,vietnam_cities1)


# Calculate distance 

library(geosphere)

# Using Haversine distance

output <- matrix(ncol=nrow(vietnam_cities1),nrow=nrow(OSH_Viet))


for(j in 1:nrow(OSH_Viet)){

  for (i in 1:nrow(vietnam_cities1)) {
    
    output[j,i] <-  distm(c(OSH_Viet$lng[j], OSH_Viet$lat[j]), c(vietnam_cities1$X[i], vietnam_cities1$Y[i]), fun = distHaversine)
    
  }
  
}


output <- data.frame(output)
output$Min<-apply(output,1,FUN=min)

OSH_Viet$dist_city_short <-output$Min


##
# 2 ) Distance airport 
###


Airports <- read.csv("Data/Airport/World_Airports.csv")

# Only taking medium-sized and large airports.

Airports_medium_large <- Airports %>% filter(type == "medium_airport" | type == "large_airport" ) 

Airports_medium_large_Vietnam <- Airports_medium_large %>%  filter(iso_country == "VN")

output_air <- matrix(ncol=nrow(Airports_medium_large_Vietnam),nrow=nrow(OSH_Viet))

for(j in 1:nrow(OSH_Viet)){
  
  for (i in 1:nrow(Airports_medium_large_Vietnam)) {
    
    output_air[j,i] <-  distm(c(OSH_Viet$lng[j], OSH_Viet$lat[j]), c(Airports_medium_large_Vietnam$X[i], Airports_medium_large_Vietnam$Y[i]), fun = distHaversine)
    
  }
  
}

output_air <- data.frame(output_air)
output_air$Min<-apply(output_air,1,FUN=min)

OSH_Viet$dist_airport_short <-output_air$Min


# 3) Buyer data

Buyer_data <- read_excel("Data/Buyer/Buyer_data_6_Nov.xlsx")

# For this example, I am deleting the NA is buyer

Buyer_data <- Buyer_data %>% drop_na(`Buyer_country`)

table(Buyer_data$Buyer_country)

library(countrycode)

Buyer_data$region <- countrycode::countrycode(Buyer_data$Buyer_country,"country.name","region")



Buyer_data %>% count(Supplier,region) 

# Create wide w/ percentages

Buyer_data_count <- Buyer_data %>%
  group_by(Supplier) %>% count(region)

Buyer_data_count

Buyer_data_per <- Buyer_data_count %>% group_by(Supplier) %>%
  mutate(per =  100 *n/sum(n)) %>% 
  ungroup

# Not too sure this makes sense, maybe we have to take binaries instead of percentages.


#x <- Buyer_data_per %>% select(Supplier,region,per) %>% 
#reshape( idvar = "Supplier", timevar = "region", direction = "wide")

Buyer_data_wide <- Buyer_data_per %>% select(Supplier,region,per) %>% 
dcast( Supplier ~ region)
Buyer_data_wide$Supplier

# Merge with dataset 

OSH_Viet_buyer <-  left_join(OSH_Viet,Buyer_data_wide,by=c("Factory.Assessed.Name"="Supplier"))
  
OSH_Viet_buyer %>% select(Factory.Assessed.Name,74:80) %>% View()

#  stringdist_inner_join(y, c("Factory.Assessed.Name"= "Supplier"), max_dist = 2,
#                       distance_col = "distance_2")













