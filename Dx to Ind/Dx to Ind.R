#set wd

#Edit "mydata" from logreg into something easy to work on
inddata <- read.csv(file="Dxinddata", 
                         header=TRUE,
                         sep =",")
require(ggplot2)

#drafts

#plot for all pts
ggplot(data = inddata,
       mapping = aes(x = Dx.to.Ind..yrs.,
                     y = patient,
                     fill=Ind.Type))+
  geom_point(shape=21,size=5,stroke=1)+
  scale_y_continuous(breaks=c(1,2,4,5,6,7,8),
                     labels = c('1721','1775','5499','6239','6268','6397','7003'))+
  labs(x = "Time between Dx and Induction (yrs)",
       y = "Patient Identifier",
       legend = "Induction Type",
       title= "Anesthetic Induction Time vs Induction Type"
  )
