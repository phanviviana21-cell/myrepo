#dont forget to set wd

#Download elrm package
require(elrm)

#Edit "mydata" from logreg into something easy to work on
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
exactdata <- mydata
exactdata$Case.Series.Table <- NULL

#Check length of exactdata before it becomes collapsed
length(exactdata$Severe.SVAS)
length(exactdata$Signs.of.Ischemia.in.ECG)
length(exactdata$Coronary.artery.anomalies..Kopparapu.)
length(exactdata$Severe.LVH)
length(exactdata$Biventricular.outflow.tract.disease)
length(exactdata$Prolonged.QTc...500.ms.)
length(exactdata$Age..3)
length(exactdata$Arrhythmia)
length(exactdata$Adverse.Events)

#Look at various tables
xtabs(~Severe.SVAS+Coronary.artery.anomalies..Kopparapu., data = exactdata)
xtabs(~Severe.SVAS+Severe.LVH, data = exactdata)
xtabs(~Coronary.artery.anomalies..Kopparapu.+Severe.LVH, data = exactdata)

xtabs(~Severe.SVAS+Adverse.Events, data = exactdata)
xtabs(~Coronary.artery.anomalies..Kopparapu.+Adverse.Events, data = exactdata)
xtabs(~Severe.LVH+Adverse.Events, data = exactdata)

xtabs(~Severe.SVAS + Coronary.artery.anomalies..Kopparapu. + Severe.LVH + Adverse.Events, data = exactdata)

#Approx exact logistic regression
#x <- xtabs(~Adverse.Events + interaction(Severe.SVAS,Coronary.artery.anomalies..Kopparapu.,Severe.LVH), data = exactdata)
#View cross tabs
#x

#Making collapsed data set
#cdat <- cdat <- data.frame(Severe.SVAS = rep(0:1,2), Coronary.artery.anomalies..Kopparapu. = rep(0:1,each = 2),Severe.LVH = rep(0:1,each = 2),Adverse.Events = x[2, ],ncases = colSums(x))

cdat <- aggregate(Adverse.Events ~ 
                    Severe.SVAS+Signs.of.Ischemia.in.ECG+Coronary.artery.anomalies..Kopparapu.+
                    Severe.LVH+Biventricular.outflow.tract.disease+Prolonged.QTc...500.ms.+
                    Age..3+Arrhythmia,
                  data = exactdata,
                  FUN = function(x) c(adverse=sum(x),ncases = length(x)))

cdat <- data.frame(
  Severe.SVAS = cdat$Severe.SVAS,
  Signs.of.Ischemia.in.ECG = cdat$Signs.of.Ischemia.in.ECG,
  Coronary.artery.anomalies..Kopparapu. = cdat$Coronary.artery.anomalies..Kopparapu.,
  Severe.LVH = cdat$Severe.LVH,
  Biventricular.outflow.tract.disease = cdat$Biventricular.outflow.tract.disease,
  Prolonged.QTc...500.ms. = cdat$Prolonged.QTc...500.ms.,
  Age..3 = cdat$Age..3,
  Arrhythmia = cdat$Arrhythmia,
  adverse = cdat$Adverse.Events[,"adverse"],
  ncases = cdat$Adverse.Events[,"ncases"])

#View aggregated data set
cdat

#Check rows of cdat
length(cdat$Severe.SVAS)
length(cdat$Signs.of.Ischemia.in.ECG)
length(cdat$Coronary.artery.anomalies..Kopparapu.)
length(cdat$Severe.LVH)
length(cdat$Biventricular.outflow.tract.disease)
length(cdat$Prolonged.QTc...500.ms.)
length(cdat$Age..3)
length(cdat$Arrhythmia)
length(cdat$adverse)
length(cdat$ncases)

modelallFactors <- elrm(adverse/ncases ~ 
                          Severe.SVAS + Signs.of.Ischemia.in.ECG +
                          Coronary.artery.anomalies..Kopparapu. + Severe.LVH +
                          Biventricular.outflow.tract.disease + Prolonged.QTc...500.ms. + 
                          Age..3 + Arrhythmia, 
                        interest = ~ Severe.SVAS + Signs.of.Ischemia.in.ECG +
                          Coronary.artery.anomalies..Kopparapu. + Severe.LVH + Biventricular.outflow.tract.disease +
                          Prolonged.QTc...500.ms. + Age..3 + Arrhythmia,
                        iter = 15000, burnIn = 5000, dataset = cdat)

summary(modelallFactors)
#Created warning messages
#1: 'Severe.SVAS' conditional distribution of the sufficient statistic was found to be degenerate 
#2: 'Signs.of.Ischemia.in.ECG' conditional distribution of the sufficient statistic was found to be degenerate 
#3: 'Coronary.artery.anomalies..Kopparapu.' conditional distribution of the sufficient statistic was found to be degenerate 
#4: 'Severe.LVH' conditional distribution of the sufficient statistic was found to be degenerate 
#5: 'Biventricular.outflow.tract.disease' conditional distribution of the sufficient statistic was found to be degenerate 
#6: 'Prolonged.QTc...500.ms.' extracted sample is too small for inference (less than 1000) 
#7: 'Age..3' conditional distribution of the sufficient statistic was found to be degenerate 
#8: 'Arrhythmia' extracted sample is too small for inference (less than 1000) 
#9: observed value of the sufficient statistics for 'joint' was not sampled 
#all p-values were "NA"

## model with each predictor one at a time
modelSVAS <- elrm(adverse/ncases ~ 
                    Severe.SVAS, 
                  interest = ~ Severe.SVAS,
                  iter = 15000, burnIn = 5000, dataset = cdat)
summary(modelSVAS)
#p-value = 1

modelCoronary <- elrm(adverse/ncases ~ Coronary.artery.anomalies..Kopparapu., 
                      interest = ~ Coronary.artery.anomalies..Kopparapu.,
                      iter = 15000, burnIn = 5000, dataset = cdat)
summary(modelCoronary)
#p-value = 1

modelLVH <- elrm(adverse/ncases ~ Severe.LVH, 
                 interest = ~ Severe.LVH,
                 iter = 15000, burnIn = 5000, dataset = cdat)
summary(modelLVH)
#p-value = 1

modelBVOT <- elrm(adverse/ncases ~ Biventricular.outflow.tract.disease, 
                  interest = ~ Biventricular.outflow.tract.disease,
                  iter = 15000, burnIn = 5000, dataset = cdat)
summary(modelBVOT)
#p-value = 1

modelQTc <- elrm(adverse/ncases ~ Prolonged.QTc...500.ms., 
                 interest = ~ Prolonged.QTc...500.ms.,
                 iter = 15000, burnIn = 5000, dataset = cdat)
summary(modelQTc)
#p-value = 0.2068

modelage <- elrm(adverse/ncases ~ Age..3, 
                 interest = ~ Age..3,
                 iter = 15000, burnIn = 5000, dataset = cdat)
summary(modelage)
#p-value = 0.7143

modelarr <- elrm(adverse/ncases ~ Arrhythmia, 
                 interest = ~ Arrhythmia,
                 iter = 15000, burnIn = 5000, dataset = cdat)
summary(modelarr)
#p-value = 0.6128