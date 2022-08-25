load("/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Data Sets/BW_extende.RDATA")


#example cluster 1

x <- Cl[[1]]
names(x) <- tolower(names(x))

x1 <-left_join(x,wiid,by=c("country","year"))

counts_air <- counts_air %>%
  rename( "airports"="rowCount")

x2 <- left_join(x1,counts_air,by="country")

counts_confl <- counts_confl %>%
  rename( "conflicts"="rowCount")

x3 <- left_join(x2,counts_confl,by=c("country","year"))


edu_df <- edu_df[edu_df$year>=2010,-1]
x4 <- left_join(x3,edu_df,by=c("country","year"))




