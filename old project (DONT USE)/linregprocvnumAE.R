#set working directory
setwd("~/Research/WS Project")

#Reading CSV file
procdata <- read.csv(file="DATAprocvsnumAElinreg", 
                   header=TRUE,
                   sep =",")
head(procdata)
summary(procdata)

#making the model
modellinprocreg <- glm(X..of.Adverse.Events ~ Surgical.Procedures +
                         Imaging.Procedures +
                         Minimally.Invasive.Procedures, data = procdata)

#summary of model
summary(modellinprocreg)

#Visualizing procedure data??
