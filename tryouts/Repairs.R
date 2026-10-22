###SVAS REPAIR ELRM####

#dont forget to set wd

#Download elrm package
require(elrm)

#Edit "mydata" from logreg into something easy to work on
mydata <- read.csv(file="RepairsELRMdata", 
                   header=TRUE,
                   sep =",")
head(mydata)
summary(mydata)

#turn categorical data into numbers
mydata[mydata == "Yes"] <- 1
mydata[mydata == "No"] <- 0
mydata$Post.SVAS.repair <- as.numeric(mydata$Post.SVAS.repair)
mydata$Adverse.Events <- as.numeric(mydata$Adverse.Events)

summary(mydata)

#making exact data
exactdata <- mydata
exactdata$Case.Series.Table <- NULL
exactdata$No.More.Coronary.Artery.Anomalies. <- NULL
exactdata$No.More.BVOT.Disease. <- NULL
exactdata$No.More.Prolonged.QTc. <- NULL
exactdata$Grew.Older.than.3. <- NULL
exactdata$Fixed.Arrhythmia. <- NULL

summary(exactdata)

#Making collapsed data set
#cdat <- cdat <- data.frame(Severe.SVAS = rep(0:1,2), Coronary.artery.anomalies..Kopparapu. = rep(0:1,each = 2),Severe.LVH = rep(0:1,each = 2),Adverse.Events = x[2, ],ncases = colSums(x))

cdat <- aggregate(Adverse.Events ~
                    Post.SVAS.repair,
                  data = exactdata,
                  FUN = function(x) c(adverse=sum(x),ncases = length(x)))

cdat <- data.frame(
  SVAS.Repair = cdat$Post.SVAS.repair,
  adverse = cdat$Adverse.Events[,"adverse"],
  ncases = cdat$Adverse.Events[,"ncases"])

#View aggregated data set
cdat

## model with each predictor one at a time
modelSVASrepair <- elrm(adverse/ncases ~ 
                    SVAS.Repair, 
                  interest = ~ SVAS.Repair, r = 2,
                  iter = 15000, burnIn = 5000, dataset = cdat)

summary(modelSVASrepair)
#Calculate odds ratio = e^estimate
exp(-0.65031)



###CORONARY REPAIR ELRM####

#dont forget to set wd

#Download elrm package
require(elrm)

#Edit "mydata" from logreg into something easy to work on
mydata <- read.csv(file="RepairsELRMdata", 
                   header=TRUE,
                   sep =",")
head(mydata)
summary(mydata)

#turn categorical data into numbers
mydata[mydata == "Yes"] <- 1
mydata[mydata == "No"] <- 0
mydata$No.More.Coronary.Artery.Anomalies. <- as.numeric(mydata$Post.SVAS.repair)
mydata$Adverse.Events <- as.numeric(mydata$Adverse.Events)

summary(mydata)

#making exact data
exactdata <- mydata
exactdata$Case.Series.Table <- NULL
exactdata$Post.SVAS.repair <- NULL
exactdata$No.More.BVOT.Disease. <- NULL
exactdata$No.More.Prolonged.QTc. <- NULL
exactdata$Grew.Older.than.3. <- NULL
exactdata$Fixed.Arrhythmia. <- NULL

summary(exactdata)

#Making collapsed data set
#cdat <- cdat <- data.frame(Severe.SVAS = rep(0:1,2), Coronary.artery.anomalies..Kopparapu. = rep(0:1,each = 2),Severe.LVH = rep(0:1,each = 2),Adverse.Events = x[2, ],ncases = colSums(x))

cdat <- aggregate(Adverse.Events ~
                    No.More.Coronary.Artery.Anomalies.,
                  data = exactdata,
                  FUN = function(x) c(adverse=sum(x),ncases = length(x)))

cdat <- data.frame(
  Coronary = cdat$No.More.Coronary.Artery.Anomalies.,
  adverse = cdat$Adverse.Events[,"adverse"],
  ncases = cdat$Adverse.Events[,"ncases"])

#View aggregated data set
cdat

## model with each predictor one at a time
modelCorrepair <- elrm(adverse/ncases ~ 
                          Coronary, 
                        interest = ~ Coronary, r = 2,
                        iter = 15000, burnIn = 5000, dataset = cdat)

summary(modelCorrepair)
#Calculate odds ratio = e^estimate
exp(-0.64201)


##### BVOT REPAIR ELRM #####

#dont forget to set wd

#Download elrm package
require(elrm)

#Edit "mydata" from logreg into something easy to work on
mydata <- read.csv(file="RepairsELRMdata", 
                   header=TRUE,
                   sep =",")
head(mydata)
summary(mydata)

#turn categorical data into numbers
mydata[mydata == "Yes"] <- 1
mydata[mydata == "No"] <- 0
mydata$No.More.BVOT.Disease. <- as.numeric(mydata$No.More.BVOT.Disease.)
mydata$Adverse.Events <- as.numeric(mydata$Adverse.Events)

summary(mydata)

#making exact data
exactdata <- mydata
exactdata$Case.Series.Table <- NULL
exactdata$Post.SVAS.repair <- NULL
exactdata$No.More.Coronary.Artery.Anomalies. <- NULL
exactdata$No.More.Prolonged.QTc. <- NULL
exactdata$Grew.Older.than.3. <- NULL
exactdata$Fixed.Arrhythmia. <- NULL

summary(exactdata)

#Making collapsed data set
#cdat <- cdat <- data.frame(Severe.SVAS = rep(0:1,2), Coronary.artery.anomalies..Kopparapu. = rep(0:1,each = 2),Severe.LVH = rep(0:1,each = 2),Adverse.Events = x[2, ],ncases = colSums(x))

cdat <- aggregate(Adverse.Events ~
                    No.More.BVOT.Disease.,
                  data = exactdata,
                  FUN = function(x) c(adverse=sum(x),ncases = length(x)))

cdat <- data.frame(
  BVOT = cdat$No.More.BVOT.Disease.,
  adverse = cdat$Adverse.Events[,"adverse"],
  ncases = cdat$Adverse.Events[,"ncases"])

#View aggregated data set
cdat

## model with each predictor one at a time
modelBVOTrepair <- elrm(adverse/ncases ~ 
                         BVOT, 
                       interest = ~ BVOT, r = 2,
                       iter = 15000, burnIn = 5000, dataset = cdat)

summary(modelBVOTrepair)
#Calculate odds ratio = e^estimate
exp(-0.04121)




##### QTC REPAIR ELRM #####
#dont forget to set wd

#Download elrm package
require(elrm)

#Edit "mydata" from logreg into something easy to work on
mydata <- read.csv(file="RepairsELRMdata", 
                   header=TRUE,
                   sep =",")
head(mydata)
summary(mydata)

#turn categorical data into numbers
mydata[mydata == "Yes"] <- 1
mydata[mydata == "No"] <- 0
mydata$No.More.Prolonged.QTc. <- as.numeric(mydata$No.More.Prolonged.QTc.)
mydata$Adverse.Events <- as.numeric(mydata$Adverse.Events)

summary(mydata)

#making exact data
exactdata <- mydata
exactdata$Case.Series.Table <- NULL
exactdata$Post.SVAS.repair <- NULL
exactdata$No.More.Coronary.Artery.Anomalies. <- NULL
exactdata$No.More.BVOT.Disease. <- NULL
exactdata$Grew.Older.than.3. <- NULL
exactdata$Fixed.Arrhythmia. <- NULL

summary(exactdata)

#Making collapsed data set
#cdat <- cdat <- data.frame(Severe.SVAS = rep(0:1,2), Coronary.artery.anomalies..Kopparapu. = rep(0:1,each = 2),Severe.LVH = rep(0:1,each = 2),Adverse.Events = x[2, ],ncases = colSums(x))

cdat <- aggregate(Adverse.Events ~
                    No.More.Prolonged.QTc.,
                  data = exactdata,
                  FUN = function(x) c(adverse=sum(x),ncases = length(x)))

cdat <- data.frame(
  qtc = cdat$No.More.Prolonged.QTc.,
  adverse = cdat$Adverse.Events[,"adverse"],
  ncases = cdat$Adverse.Events[,"ncases"])

#View aggregated data set
cdat

## model with each predictor one at a time
modelQTCrepair <- elrm(adverse/ncases ~ 
                          qtc, 
                        interest = ~ qtc, r = 2,
                        iter = 15000, burnIn = 5000, dataset = cdat)

summary(modelQTCrepair)
#Calculate odds ratio = e^estimate
exp(-0.8452)




##### AGE REPAIR ELRM #####
#dont forget to set wd

#Download elrm package
require(elrm)

#Edit "mydata" from logreg into something easy to work on
mydata <- read.csv(file="RepairsELRMdata", 
                   header=TRUE,
                   sep =",")
head(mydata)
summary(mydata)

#turn categorical data into numbers
mydata[mydata == "Yes"] <- 1
mydata[mydata == "No"] <- 0
mydata$Grew.Older.than.3. <- as.numeric(mydata$Grew.Older.than.3.)
mydata$Adverse.Events <- as.numeric(mydata$Adverse.Events)

summary(mydata)

#making exact data
exactdata <- mydata
exactdata$Case.Series.Table <- NULL
exactdata$Post.SVAS.repair <- NULL
exactdata$No.More.Coronary.Artery.Anomalies. <- NULL
exactdata$No.More.BVOT.Disease. <- NULL
exactdata$No.More.Prolonged.QTc. <- NULL
exactdata$Fixed.Arrhythmia. <- NULL

summary(exactdata)

#Making collapsed data set
#cdat <- cdat <- data.frame(Severe.SVAS = rep(0:1,2), Coronary.artery.anomalies..Kopparapu. = rep(0:1,each = 2),Severe.LVH = rep(0:1,each = 2),Adverse.Events = x[2, ],ncases = colSums(x))

cdat <- aggregate(Adverse.Events ~
                    Grew.Older.than.3.,
                  data = exactdata,
                  FUN = function(x) c(adverse=sum(x),ncases = length(x)))

cdat <- data.frame(
  age = cdat$Grew.Older.than.3.,
  adverse = cdat$Adverse.Events[,"adverse"],
  ncases = cdat$Adverse.Events[,"ncases"])

#View aggregated data set
cdat

## model with each predictor one at a time
modelagerepair <- elrm(adverse/ncases ~ 
                         age, 
                       interest = ~ age, r = 2,
                       iter = 15000, burnIn = 5000, dataset = cdat)

summary(modelagerepair)
#Calculate odds ratio = e^estimate
exp(-1.31849)



##### ARRHYTHMIA REPAIR ELRM #####
#dont forget to set wd

#Download elrm package
require(elrm)

#Edit "mydata" from logreg into something easy to work on
mydata <- read.csv(file="RepairsELRMdata", 
                   header=TRUE,
                   sep =",")
head(mydata)
summary(mydata)

#turn categorical data into numbers
mydata[mydata == "Yes"] <- 1
mydata[mydata == "No"] <- 0
mydata$Fixed.Arrhythmia. <- as.numeric(mydata$Fixed.Arrhythmia.)
mydata$Adverse.Events <- as.numeric(mydata$Adverse.Events)

summary(mydata)

#making exact data
exactdata <- mydata
exactdata$Case.Series.Table <- NULL
exactdata$Post.SVAS.repair <- NULL
exactdata$No.More.Coronary.Artery.Anomalies. <- NULL
exactdata$No.More.BVOT.Disease. <- NULL
exactdata$No.More.Prolonged.QTc. <- NULL
exactdata$Grew.Older.than.3. <- NULL

summary(exactdata)

#Making collapsed data set
#cdat <- cdat <- data.frame(Severe.SVAS = rep(0:1,2), Coronary.artery.anomalies..Kopparapu. = rep(0:1,each = 2),Severe.LVH = rep(0:1,each = 2),Adverse.Events = x[2, ],ncases = colSums(x))

cdat <- aggregate(Adverse.Events ~
                    Fixed.Arrhythmia.,
                  data = exactdata,
                  FUN = function(x) c(adverse=sum(x),ncases = length(x)))

cdat <- data.frame(
  arr = cdat$Fixed.Arrhythmia.,
  adverse = cdat$Adverse.Events[,"adverse"],
  ncases = cdat$Adverse.Events[,"ncases"])

#View aggregated data set
cdat

## model with each predictor one at a time
modelarrrepair <- elrm(adverse/ncases ~ 
                         arr, 
                       interest = ~ arr, r = 2,
                       iter = 15000, burnIn = 5000, dataset = cdat)

summary(modelarrrepair)
#Calculate odds ratio = e^estimate
exp(-0.64733)

