
library(tidyverse)
load("/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Data Sets/BetterWorksClustered.RData")

load("/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Data Sets/dataset_Vers1.RDATA")
names(cpsx)

#clean the data 

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


cpsxc$NumBarg <- readr::parse_number(cpsxc$NumBarg)


cpsxc$percUnion <-  readr::parse_number(cpsx$percUnion)/100
cpsxc$percUnion <-  ifelse(cpsxc$percUnion>1,cpsxc$percUnion/10,cpsxc$percUnion)

cpsxc$firm_age <- ifelse(as.numeric(cpsxc$`In what year did this supplier begin operations in this country?`)>0,
                                    cpsxc$Year-as.numeric(cpsxc$`In what year did this supplier begin operations in this country?`),
                                                    1)
cpsxc$`In what year did this supplier begin operations in this country?` <- NULL

cpsxc <- cpsxc%>%
  rename("firm_size"=Total)%>%
  mutate(firm_size=as.numeric(firm_size),
         Female=as.numeric(Female),
         Male=as.numeric(Male))

cpsxc$femRatio <- (cpsxc$Female/cpsxc$firm_size)*100


cpsxc$prefSup <- ifelse(cpsxc$`Customer 1 - Preferred Supplier`=="Yes",1,0)
cpsxc$contr <- ifelse(cpsxc$`Customer 1 - Contractor`=="Yes",1,0)
cpsxc$subContr <- ifelse(cpsxc$`Customer 1 - Subcontractor`=="Yes",1,0)

cpsxc$`Customer 1 - Preferred Supplier` <- cpsxc$`Customer 1 - Contractor` <-cpsxc$`Customer 1 - Subcontractor` <- NULL 


#cpsxc <- cpsxc[cpsxc$Year!=2022 & cpsxc$Year!=2009 & cpsxc$Year!=2010,]


# meanCountry <- aggregate(cpsxc$factory_cp_level_nc_rate,by=list(cpsxc$Country,cpsxc$Year),mean)%>%
#   rename(Country=Group.1,
#          Year=Group.2,
#          mean_cp_level_nc_rate_country=x)
# 
# meanFirm <- aggregate(cpsxc$factory_cp_level_nc_rate,by=list(cpsxc$Cluster,cpsxc$Factory.Assessed.ID),mean)%>%
#   rename(Cluster=Group.1,
#          Firm=Group.2,
#          mean_cp_level_nc_rate_country=x)
# 
# cpsxc <- left_join(cpsxc,meanCountry,by=c("Country", "Year"))

library(Hmisc)

Output <- cpsxc %>% 
  group_by(Cluster,Country,Factory.Assessed.ID,Cycle) %>% 
  summarise(Value = mean(factory_cp_level_nc_rate,na.rm=T)) %>% # ensure only 1 Value per Cycle/Factory ID
  mutate(PrevValue = Lag(Value, shift=1), # create column that is previous cycle value                            
         DiffToPrevCycle = PrevValue - Value)

cpsxc <- left_join(cpsxc,Output,by=c("Country","Cluster","Cycle" , "Factory.Assessed.ID"))
cpsxc$Creation..Data <- as.Date(cpsxc$Creation..Data,"%d/%m/%Y")
cpsxc$lenPart <- cpsxc$Year-lubridate::year(cpsxc$Creation..Data) 


#Imputation
library(mice)
library(plm)

names(cpsxc) <- make.names(names(cpsxc))
temp <- cpsxc%>%
  mutate(
    formUnion=numeric(formUnion),
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
         trainOSH=as.factor(trainOSH3),
         Peer.Grouping.Enabled = as.factor(ifelse(Peer.Grouping.Enabled=="No",1,0)),
         Cluster = as.factor(Cluster),
         Factory.Assessed.ID = as.numeric(Factory.Assessed.ID),
         factory_cp_level_nc_rate = as.numeric(factory_cp_level_nc_rate),
         total_cp_nc = as.numeric(total_cp_nc),
         total_cps = as.numeric(total_cps),
         trainWorker =as.numeric(trainWorker)
  )

x <-  pdata.frame(temp,index = c("Country" ,"Factory.Assessed.ID","Cycle"))
pdata <- unique(x, by = c("Country","Factory.Assessed.ID","Cycle"))





pdata <- as.data.frame(pdata)

#fast mice


pdata$Creation..Data <- NULL
pdata$Country.Code..Alpha.2. <- NULL
pdata$Year <- NULL



tempData <- mice(pdata,m=5,method="rf",maxit=5,cluster.seed=500)
c <- complete(tempData)


rem_imp <- c("Factory.Assessed.Name","Zip.Code", 
                       "Datasets","Section", "Division" ,
                         "Latest.Cycle", "Cycle.Start.Date","ID",  "Status", "ISO", "Continent"," Country.Code..Alpha.2.")
pdata<- pdata[,!names(pdata) %in% rem_imp]
pdata$Factory.Assessed.ID <- as.numeric(as.character(pdata$Factory.Assessed.ID))
pdata$First.Product <- NULL
pdata$Country <- as.factor(temp$Country)
pdata$trainOSH3 <- NULL
#missForest
library(missForest)
set.seed(500)
tempDataForest<- missForest(pdata,maxiter=5,ntree=100,verbose=T)
tempDataForest$OOBerror


save(list=c("implist","tempData","tempDataForest","pdata"),file="/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Data Sets/dataset_Vers1_2_imputed.RDATA")

# #complex amelia
# 
# 
# library(Amelia)
# rem_imp <- c("Factory.Assessed.Name","Zip.Code", 
#                "Datasets","Section", "Division" ,
#              "Latest.Cycle", "Cycle.Start.Date","ID","Country",  "Status", "ISO", "Continent"," Country.Code..Alpha.2.")
# temp <- temp[,!names(temp) %in% rem_imp]
# temp$Country.Code..Alpha.2. <- NULL
# temp$Year <- NULL
# 
# d

#Base Model with plm 
library(plm)
library(sjstats)

 # x <-  pdata.frame(c,index = c("Factory.Assessed.ID","Cycle"))
 # x <- unique(x, by = c("Factory.Assessed.ID","Cycle"))
 # 
 # unique(pdata.frame(implist[[1]],index = c("Factory.Assessed.ID","Cycle")),by = c("Factory.Assessed.ID","Cycle"))
 # unique(pdata.frame(implist[[2]],index = c("Factory.Assessed.ID","Cycle")),by = c("Factory.Assessed.ID","Cycle"))
 # 
 # X <- list()
 # X[[1]] <-  unique(pdata.frame(implist[[1]],index = c("Factory.Assessed.ID","Cycle")),by = c("Factory.Assessed.ID","Cycle"))
 
implist <- mitools::imputationList(
   lapply(1:tempData$m, function(n) mice::complete(tempData, action = n)))
 
# test <-  lapply(implist, function(x) {temp <- pdata.frame(x,index = c("Factory.Assessed.ID","Cycle"))
# return(unique(temp, by = c("Factory.Assessed.ID","Cycle")))})
#  

fit1 <- lapply(implist$imputations, function(x){ plm(DiffToPrevCycle ~ firm_age + (1|firm_age^2) +lenPart+firm_size+percUnion +prefSup+femRatio+trainChem+trainFirst+NumBarg+formUnion+Ecological_Footprint+refBarg,
                                                      data=x,index = c("Factory.Assessed.ID","Cycle"),model="pooling",pooling=T)})

gravity_pois = lapply(implist$imputations, function(x){fepois(factory_cp_level_nc_rate ~ Peer.Grouping.Enabled+firm_age + lenPart+firm_size+
                       percUnion + prefSup+femRatio+trainChem+trainFirst+
                       NumBarg+formUnion+
                       refBarg|Country+Cycle+Cluster, x)}) 
summary(fepois(factory_cp_level_nc_rate ~ Peer.Grouping.Enabled+firm_age + lenPart+firm_size+
         percUnion + prefSup+femRatio+trainChem+trainFirst+
         NumBarg+formUnion+
         refBarg|Cycle+Cluster+Country, tempDataForest$ximp))
summary(pool(fit1))
summary(pool(gravity_pois))
# 
# fit.random <- plm(DiffToPrevCycle ~ firm_age + firm_size+percUnion + NumBarg+factor(formUnion)+Ecological_Footprint+factor(refBarg),
#              data= x,model="random",pooling=T)
# fit.fixEF <- plm(DiffToPrevCycle ~ firm_age + firm_size+percUnion + NumBarg+factor(formUnion)+Ecological_Footprint+factor(refBarg),
#                   data= x,model="within",pooling=T)
# fit.fixEF2 <- plm(DiffToPrevCycle ~ firm_age + firm_size+percUnion + NumBarg+factor(formUnion)+Ecological_Footprint+factor(refBarg),
#                  data= x,effect="twoways",model="within",pooling=T)
fit.pool<- plm(factory_cp_level_nc_rate  ~ firm_age + lenPart+firm_size+percUnion + prefSup+femRatio+trainChem+factor(trainFirst)+NumBarg+factor(formUnion)+factor(refBarg),
                 data= tempDataForest$ximp,model="pooling",pooling=T)
summary(fit.pool)
fit.within<- plm(factory_cp_level_nc_rate ~ firm_age +(firm_age:Cluster) +lenPart+(lenPart:Cluster)+firm_size+(firm_size:Cluster)+percUnion +(percUnion:Cluster)+ factor(prefSup)+femRatio+factor(trainChem)+factor(trainFirst)+NumBarg+factor(formUnion)+Ecological_Footprint+factor(refBarg),
               data= tempDataForest$ximp,effect="twoway",model="within",index=c("Factory.Assessed.ID","Cycle"),pooling=T)

summary(fit.within)

fit.random<- plm(factory_cp_level_nc_rate ~ Cluster+firm_age +lenPart+firm_size+percUnion +factor(prefSup)+femRatio+factor(trainChem)+factor(trainFirst)+NumBarg+factor(formUnion)+Ecological_Footprint+factor(refBarg),
                 data= tempDataForest$ximp,model="random",index=c("Factory.Assessed.ID","Cycle"),pooling=T)


# 
# 
# t <- coeftest(fit.fixEF2, vcov=vcovHC(fit.fixEF2, method = "arellano", type="HC3"))
# fixef(fit.fixEF2)
# summary(fit.pool)
# 
# predict.out.plm<-function(
#     estimate,
#     formula,
#     data,
#     model="fd",
#     pname="y",
#     pindex=NULL,
#     levelconstr=T
# ){
#   # estimate=e.fe
#   # formula=f
#   # data=d
#   # model="within"
#   # pname="y"
#   # pindex=NULL
#   # levelconstr=T
#   #get index of panel data
#   if (is.null(pindex) && class(data)[1]=="pdata.frame") {
#     pindex<-names(attributes(data)$index)
#   } else {
#     pindex<-names(data)[1:2]
#   }
#   if (class(data)[1]!="pdata.frame") { 
#     data<-pdata.frame(data)
#   }
#   #model frame
#   mf<-model.frame(formula,data=data)
#   #model matrix - transformed data
#   mn<-model.matrix(formula,mf,model)
#   
#   #define variable names
#   y.t.hat<-paste0(pname,".t.hat")
#   y.l.hat<-paste0(pname,".l.hat")
#   y.l<-names(mf)[1]
#   
#   #transformed data of explanatory variables 
#   #exclude variables that were droped in estimation
#   n<-names(estimate$aliased[estimate$aliased==F])
#   i<-match(n,colnames(mn))
#   X<-mn[,i]
#   
#   #predict transformed outcome with X * beta
#   # p<- X %*% coef(estimate)
#   p<-crossprod(t(X),coef(estimate))
#   colnames(p)<-y.t.hat
#   
#   if (levelconstr==T){
#     #old dataset with original outcome
#     od<-data.frame(
#       attributes(mf)$index,
#       data.frame(mf)[,1]
#     )
#     rownames(od)<-rownames(mf) #preserve row names from model.frame
#     names(od)[3]<-y.l
#     
#     #merge old dataset with prediciton
#     nd<-merge(
#       od,
#       p,
#       by="row.names",
#       all.x=T,
#       sort=F
#     )
#     nd$Row.names<-as.integer(nd$Row.names)
#     nd<-nd[order(nd$Row.names),]
#     
#     #construct predicted level outcome for FD estiamtions
#     if (model=="fd"){
#       #first observation from real data
#       i<-which(is.na(nd[,y.t.hat]))
#       nd[i,y.l.hat]<-NA
#       nd[i,y.l.hat]<-nd[i,y.l]
#       #fill values over all years
#       ylist<-unique(nd[,pindex[2]])[-1]
#       ylist<-as.integer(as.character(ylist))
#       for (y in ylist){
#         nd[nd[,pindex[2]]==y,y.l.hat]<-
#           nd[nd[,pindex[2]]==(y-1),y.l.hat] + 
#           nd[nd[,pindex[2]]==y,y.t.hat]
#       }
#     } 
#     if (model=="within"){
#       #group means of outcome
#       gm<-aggregate(nd[, pname], list(nd[,pindex[1]]), mean)
#       gl<-aggregate(nd[, pname], list(nd[,pindex[1]]), length)
#       nd<-cbind(nd,groupmeans=rep(gm$x,gl$x))
#       #predicted values + group means
#       nd[,y.l.hat]<-nd[,y.t.hat] + nd[,"groupmeans"]
#     } 
#     if (model!="fd" && model!="within") {
#       stop('funciton works only for FD and FE estimations')
#     }
#   }
#   #results
#   results<-p
#   if (levelconstr==T){
#     results<-list(results,nd)
#     names(results)<-c("p","df")
#   }
#   return(results)
# }
# 
# cl1 <- function(modl,clust) {
#   # model is the regression model
#   # clust is the clustervariable
#   # id is a unique identifier in ids
#   library(plm)
#   library(lmtest)
#   #  Get Formula
#   form <- formula(modl$call)
#   # Get Data frame
#   dat <- eval(modl$call$data)
#   dat$row <- rownames(dat)
#   dat$id <- ave(dat$row, dat[[deparse(substitute(Cluster))]], FUN =seq_along)       
#   pdat <- pdata.frame(dat, 
#                       index=c("id", deparse(substitute(Cluster)))
#                       , drop.index= F, row.names= T)
#   # # Regression
#   reg <- plm(form, data=pdat, model="pooling")  
#   # # Adjustments
#   G <- length(unique(dat[, deparse(substitute(Cluster))]))
#   N <- length(dat[,deparse(substitute(Cluster))])
#   # # Resid degrees of freedom, adjusted
#   dfa <- (G/(G-1))*(N-1)/reg$df.residual
#   d.vcov <- dfa* vcovHC(reg, type="HC3", cluster="group", adjust=T)
#   table <- coeftest(reg, vcov=d.vcov)
#   # #  Output: se, t-stat and p-val
#   cl1out <- data.frame(table[, 2:4])
#   names(cl1out) <- c("se", "tstat", "pval")
#   # # Cluster VCE
#   return(cl1out)
# 
# }
# 
# cl1(fit.lm,Cluster)
# predict(fit.pool)
# 
# 
# library(caret)
# library(randomForest)
# library(varImp)
# 
# 
# 
# 
# 
# regressor <- randomForest(factory_cp_level_nc_rate ~ ., 
#                           data= x, 
#                           importance=TRUE,
#                           na.action = "na.pass") # fit the random forest with default parameter
# varImp(regressor) # get variable importance, based on mean decrease in accuracy
# varImp(regressor, conditional=TRUE) # conditional=True, adjusts for correlations between predictors
# varimpAUC(regressor) # more robust towards class imbalance.
# 
rm(list=setdiff(ls(), c("cpsxc","cpsx","Cl","tempData","tempDataForest")))
save(ls(),file="/Users/malik/Dropbox (GALILEO)/Master Thesis/RManual-master/Data Sets/dataset_Vers1_2_imputed.RDATA")
