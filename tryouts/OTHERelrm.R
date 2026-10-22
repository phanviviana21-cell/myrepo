###PROC VS AE####

#dont forget to set wd

#Download elrm package
require(elrm)

#Edit "mydata" from logreg into something easy to work on
mydata <- read.csv(file="Procdata", 
                   header=TRUE,
                   sep =",")
head(mydata)
summary(mydata)

#turn categorical data into numbers
mydata[mydata == "Yes"] <- 1
mydata[mydata == "No"] <- 0
mydata$Surgical.Procedures<- as.numeric(mydata$Surgical.Procedures)
mydata$Imaging.Procedures<- as.numeric(mydata$Imaging.Procedures)
mydata$Minimally.Invasive.Procedures<- as.numeric(mydata$Minimally.Invasive.Procedures)
mydata$Adverse.Events <- as.numeric(mydata$Adverse.Events)

summary(mydata)

#making exact data
exactdata <- mydata
exactdata$Case.Series.Table <- NULL

#Making collapsed data set
#cdat <- cdat <- data.frame(Severe.SVAS = rep(0:1,2), Coronary.artery.anomalies..Kopparapu. = rep(0:1,each = 2),Severe.LVH = rep(0:1,each = 2),Adverse.Events = x[2, ],ncases = colSums(x))

cdat <- aggregate(Adverse.Events ~
                    Surgical.Procedures+
                    Imaging.Procedures+
                    Minimally.Invasive.Procedures,
                  data = exactdata,
                  FUN = function(x) c(adverse=sum(x),ncases = length(x)))

cdat <- data.frame(
  Surgical.Procedures = cdat$Surgical.Procedures,
  Imaging.Procedures = cdat$Imaging.Procedures,
  Minimally.Invasive.Procedures = cdat$Minimally.Invasive.Procedures,
  adverse = cdat$Adverse.Events[,"adverse"],
  ncases = cdat$Adverse.Events[,"ncases"])

#View aggregated data set
cdat

## model with each predictor one at a time
modelsurg <- elrm(adverse/ncases ~ 
                    Surgical.Procedures, 
                  interest = ~ Surgical.Procedures,
                  iter = 15000, burnIn = 5000, dataset = cdat)
summary(modelsurg)
#p-value = 1
#Calculate odds ratio = e^estimate
exp(1.81139)

modelimag <- elrm(adverse/ncases ~ 
                    Imaging.Procedures, 
                  interest = ~ Imaging.Procedures,
                  iter = 15000, burnIn = 5000, dataset = cdat)
summary(modelimag)
#p-value = 1
#Calculate odds ratio = e^estimate
exp(-1.22437)

modelmi <- elrm(adverse/ncases ~ 
                    Minimally.Invasive.Procedures, 
                  interest = ~ Minimally.Invasive.Procedures,
                  iter = 15000, burnIn = 5000, dataset = cdat)
summary(modelmi)
#p-value = 1
#Calculate odds ratio = e^estimate
exp(-0.43041)






###DX VS AE####

#dont forget to set wd

#Download elrm package
require(elrm)

#Edit "mydata" from logreg into something easy to work on
mydata <- read.csv(file="Dxelrmdata", 
                   header=TRUE,
                   sep =",")
head(mydata)
summary(mydata)

#turn categorical data into numbers
mydata[mydata == "Yes"] <- 1
mydata[mydata == "No"] <- 0
mydata[mydata == "Before"] <- 1
mydata[mydata == "After"] <- 0
  
mydata$Ind.Time <- as.numeric(mydata$Ind.Time)
mydata$Inhale <- as.numeric(mydata$Inhale)
mydata$IV <- as.numeric(mydata$IV)
mydata$Adverse.Events <- as.numeric(mydata$Adverse.Events)

summary(mydata)

#making exact data
exactdata <- mydata
exactdata$Case.Series.Table <- NULL

#Making collapsed data set
#cdat <- cdat <- data.frame(Severe.SVAS = rep(0:1,2), Coronary.artery.anomalies..Kopparapu. = rep(0:1,each = 2),Severe.LVH = rep(0:1,each = 2),Adverse.Events = x[2, ],ncases = colSums(x))

cdat <- aggregate(Adverse.Events ~
                    IV+
                    Inhale+
                    Ind.Time,
                  data = exactdata,
                  FUN = function(x) c(adverse=sum(x),ncases = length(x)))

cdat <- data.frame(
  Ind.Time = cdat$Ind.Time,
  Inhale = cdat$Inhale,
  IV = cdat$IV,
  adverse = cdat$Adverse.Events[,"adverse"],
  ncases = cdat$Adverse.Events[,"ncases"])

#View aggregated data set
cdat

## model with each predictor one at a time
modeldx <- elrm(adverse/ncases ~ 
                  Ind.Time, 
                interest = ~ Ind.Time,
                iter = 15000, burnIn = 5000, dataset = cdat)
summary(modeldx)
#p-value = 1
#Calculate odds ratio = e^estimate
exp(0.52334)

modeliv <- elrm(adverse/ncases ~ 
                  IV, 
                interest = ~ IV,
                iter = 15000, burnIn = 5000, dataset = cdat)
summary(modeliv)
#p-value = 1
#Calculate odds ratio = e^estimate
exp(1.16081)

modelinhale <- elrm(adverse/ncases ~ 
                  Inhale, 
                interest = ~ Inhale,
                iter = 15000, burnIn = 5000, dataset = cdat)
summary(modelinhale)
#p-value = 1
#Calculate odds ratio = e^estimate
exp(-2.14132)






###SVAS VS AE (LINreg)####

#dont forget to set wd

#Download elrm package
require(elrm)

#Edit "mydata" from logreg into something easy to work on
mydata <- read.csv(file="SVASdata", 
                   header=TRUE,
                   sep =",")
head(mydata)
summary(mydata)

#turn categorical data into numbers
mydata[mydata == "Yes"] <- 0
mydata[mydata == "No"] <- 1

mydata$SVAS <- as.numeric(mydata$SVAS)

summary(mydata)

#making exact data
exactdata <- mydata
exactdata$Case.Series.Table <- NULL

#making the model
modellinprocreg <- glm(X..of.Adverse.Events ~ SVAS, 
                       data = mydata)

#summary of model
summary(modellinprocreg)

#CI
confint(modellinprocreg,level=0.95)





###SVAS VS AE (elrm)####

#dont forget to set wd

#Download elrm package
require(elrm)

#Edit "mydata" from logreg into something easy to work on
mydata <- read.csv(file="SVASelrmdata", 
                   header=TRUE,
                   sep =",")
head(mydata)
summary(mydata)

#turn categorical data into numbers
mydata[mydata == "Yes"] <- 0
mydata[mydata == "No"] <- 1

mydata$SVAS <- as.numeric(mydata$SVAS)
mydata$Adverse.Events <- as.numeric(mydata$Adverse.Events)

summary(mydata)

#making exact data
exactdata <- mydata
exactdata$Case.Series.Table <- NULL

#Making collapsed data set
#cdat <- cdat <- data.frame(Severe.SVAS = rep(0:1,2), Coronary.artery.anomalies..Kopparapu. = rep(0:1,each = 2),Severe.LVH = rep(0:1,each = 2),Adverse.Events = x[2, ],ncases = colSums(x))

cdat <- aggregate(Adverse.Events ~
                    SVAS,
                  data = exactdata,
                  FUN = function(x) c(adverse=sum(x),ncases = length(x)))

cdat <- data.frame(
  SVAS = cdat$SVAS,
  adverse = cdat$Adverse.Events[,"adverse"],
  ncases = cdat$Adverse.Events[,"ncases"])

#View aggregated data set
cdat

## model with each predictor one at a time
modelsvas <- elrm(adverse/ncases ~ 
                  SVAS, 
                interest = ~ SVAS, r = 2,
                iter = 15000, burnIn = 5000, dataset = cdat)
summary(modelsvas)
#p-value = 1
#Calculate odds ratio = e^estimate
exp(-0.85385)




##### IV Hydration vs AE #####
#Reading CSV file
IVdata <- read.csv(file="IVhydlogdata", 
                   header=TRUE,
                   sep =",")

head(IVdata)
summary(IVdata)

#turn categorical data into numbers
IVdata[IVdata == "Yes"] <- 1
IVdata[IVdata == "No"] <- 0

IVdata$Case.Series.Table <- NULL

IVdata$IV.Hydration <- as.numeric(IVdata$IV.Hydration)
IVdata$Adverse.Events <- as.numeric(IVdata$Adverse.Events)

summary(IVdata)

#making exact data
exactdata <- IVdata
exactdata$Case.Series.Table <- NULL

#Making collapsed data set
#cdat <- cdat <- data.frame(Severe.SVAS = rep(0:1,2), Coronary.artery.anomalies..Kopparapu. = rep(0:1,each = 2),Severe.LVH = rep(0:1,each = 2),Adverse.Events = x[2, ],ncases = colSums(x))

cdat <- aggregate(Adverse.Events ~
                    IV.Hydration,
                  data = exactdata,
                  FUN = function(x) c(adverse=sum(x),ncases = length(x)))

cdat <- data.frame(
  IV.Hydration = cdat$IV.Hydration,
  adverse = cdat$Adverse.Events[,"adverse"],
  ncases = cdat$Adverse.Events[,"ncases"])

#View aggregated data set
cdat

#making the model
modelivhyd <- elrm(adverse/ncases ~ 
                    IV.Hydration, 
                  interest = ~ IV.Hydration, r = 2,
                  iter = 15000, burnIn = 5000, dataset = cdat)
summary(modelivhyd)
#Calculate odds ratio = e^estimate
exp(2.48484)
