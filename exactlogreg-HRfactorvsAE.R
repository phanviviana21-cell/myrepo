#Download elrm package
require(elrm)

#Edit "mydata" from logreg into something easy to work on
library(dplyr)
mydata <- read.csv(file="DATAHRfactorvsAElogreg", 
         header=TRUE,
         sep =",")
head(mydata)
summary(mydata)

#turn categorical data into numbers
mydata[mydata == "Yes"] <- 1
mydata[mydata == "No"] <- 0
mydata$Severe.SVAS <- as.numeric(mydata$Severe.SVAS)
mydata$Signs.of.Ischemia.in.ECG <- as.numeric(mydata$Signs.of.Ischemia.in.ECG)
mydata$Coronary.artery.anomalies..Kopparapu. <- as.numeric(mydata$Coronary.artery.anomalies..Kopparapu.)
mydata$Severe.LVH <- as.numeric(mydata$Severe.LVH)
mydata$Biventricular.outflow.tract.disease <- as.numeric(mydata$Biventricular.outflow.tract.disease)
mydata$Prolonged.QTc...500.ms. <- as.numeric(mydata$Prolonged.QTc...500.ms.)
mydata$Age..3 <- as.numeric(mydata$Age..3)
mydata$Arrhythmia <- as.numeric(mydata$Arrhythmia)
mydata$Adverse.Events <- as.numeric(mydata$Adverse.Events)

#making exact data
exactdata <- rename(mydata, 
                    ischemia.ECG = Signs.of.Ischemia.in.ECG,
                    coronary.anomalies = Coronary.artery.anomalies..Kopparapu.,
                    BVOT.disease = Biventricular.outflow.tract.disease,
                    prolonged.QTc = Prolonged.QTc...500.ms.)

exactdata$Case.Series.Table <- NULL

#Check length of exactdata before it becomes collapsed
length(exactdata$Severe.SVAS)
length(exactdata$ischemia.ECG)
length(exactdata$coronary.anomalies)
length(exactdata$Severe.LVH)
length(exactdata$BVOT.disease)
length(exactdata$prolonged.QTc)
length(exactdata$Age..3)
length(exactdata$Arrhythmia)
length(exactdata$Adverse.Events)

#Look at frequency tables
xtabs(~Severe.SVAS+
        ischemia.ECG+
        coronary.anomalies+
        Severe.LVH+
        BVOT.disease+
        prolonged.QTc+
        Age..3+
        Arrhythmia, data = exactdata)

#Approx exact logistic regression
x <- xtabs(~Adverse.Events + interaction(Severe.SVAS,
                                         ischemia.ECG,
                                         coronary.anomalies,
                                         Severe.LVH,
                                         BVOT.disease,
                                         prolonged.QTc,
                                         Age..3,
                                         Arrhythmia), data = exactdata)
#View cross tabs
x

#Making collapsed data set
cdat <- cdat <- data.frame(Severe.SVAS = rep(0:1,2),
                   ischemia.ECG = rep(0:1,2),
                   coronary.anomalies = rep(0:1,2),
                   Severe.LVH = rep(0:1,2),
                   BVOT.disease = rep(0:1,2),
                   prolonged.QTc = rep(0:1,2),
                   Age..3 = rep(0:1,2),
                   Arrhythmia = rep(0:1,2),
                   Adverse.Events = x[2, ],
                   ncases = colSums(x))
#View collapsed data set
cdat

#Check rows of cdat
length(cdat$Severe.SVAS)
length(cdat$ischemia.ECG)
length(cdat$coronary.anomalies)
length(cdat$Severe.LVH)
length(cdat$BVOT.disease)
length(cdat$prolonged.QTc)
length(cdat$Age..3)
length(cdat$Arrhythmia)
length(cdat$Adverse.Events)
length(cdat$ncases)

## model with Severe.SVAS predictor only
modelSVAS <- elrm(formula= Adverse.Events/ncases ~ Severe.SVAS, interest = ~Severe.SVAS, 
                 dataset = cdat)

#Error: columns have different # of rows
#