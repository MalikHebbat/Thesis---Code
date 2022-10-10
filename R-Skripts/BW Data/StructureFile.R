
load("/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Data Sets/Splitted.RData")


split_cluster<- function(data,Cluster) {
  require(tidyverse)
  
  num_cols <- bin_cols <- bin_cols_aggr <- num_cols_aggr <- temp <- dem <- list()  
  
  
  for(name in names(data)) {
    
    data[[name]]$Factory.Assessed.ID <- as.numeric(as.character(data[[name]]$Factory.Assessed.ID))
    num_cols[[name]] <- data[[name]][unlist(lapply(data[[name]],is.numeric))]  
    num_cols_aggr[[name]] <- aggregate(num_cols[[name]],list(num_cols[[name]]$Factory.Assessed.ID), mean, na.rm=T,na.action = na.pass)
    
    
    data[[name]]$Factory.Assessed.ID <- as.factor(data[[name]]$Factory.Assessed.ID)
    bin_cols[[name]] <- data[[name]][unlist(lapply(data[[name]],is.factor))]
    bin_cols[[name]]$Cluster <- NULL
    bin_cols[[name]]$Country <- NULL
    bin_cols[[name]]$Cycle <- NULL
    bin_cols[[name]]$Peer.Grouping.Enabled <- NULL
    bin_cols[[name]] <- as.data.frame(bind_rows(lapply(bin_cols[[name]], function(x) {as.numeric(as.character(x))})))
    bin_cols_aggr[[name]] <- aggregate(bin_cols[[name]],list(bin_cols[[name]]$Factory.Assessed.ID), mean, na.rm=T,na.action = na.pass)
    dem[[name]] <-  unique(data[[name]][,c("Country","Factory.Assessed.ID")])
    temp[[name]] <-as.data.frame(left_join(num_cols_aggr[[name]],bin_cols_aggr[[name]],by="Factory.Assessed.ID"))
    dem[[name]]$Factory.Assessed.ID <- as.numeric(as.character(dem[[name]]$Factory.Assessed.ID))
    temp[[name]] <- as.data.frame(left_join(temp[[name]],dem[[name]],by ="Factory.Assessed.ID"))
  }
  return(temp) 
}

split_cluster(CL)
