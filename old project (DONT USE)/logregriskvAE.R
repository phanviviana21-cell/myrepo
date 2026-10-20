#set working directory
setwd("~/Research/WS Project")

#Reading CSV file
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

#making the model
modellogreg <- glm(Adverse.Events ~ Severe.SVAS + 
                     Signs.of.Ischemia.in.ECG + 
                     Coronary.artery.anomalies..Kopparapu. +
                     Severe.LVH +
                     Biventricular.outflow.tract.disease +
                     Prolonged.QTc...500.ms.+
                     Age..3 +
                     Arrhythmia, data = mydata, family = binomial())

#summary of model
summary(modellogreg)
