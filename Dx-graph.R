#set wd

#Edit "mydata" from logreg into something easy to work on
timelinedata <- read.csv(file="Dx.Timeline.Graph.Data", 
                   header=TRUE,
                   sep =",")

youngerptdata <- timelinedata[7:162,]
olderptdata <- timelinedata[-c(7:162),]

require(ggplot2)

#drafts

#plot for all pts
ggplot(data = timelinedata,
       mapping = aes(x = age,
                     y = patient,
                     fill=event))+
  geom_line(mapping=aes(group=patient),
            color="black",
            linewidth = 1)+
  geom_point(shape=21,size=2,stroke=1)+
  scale_y_continuous(breaks=c(1,2,4,5,6,7,8),
                     labels = c('1721','1775','5499','6239','6268','6397','7003'))+
  labs(x = "Age(yrs)",
       y = "Patient Identifier",
       title= "Line Plot Chronology of WS Pt Medical Events"
  )






#plot for younger pts
ggplot(data = youngerptdata,
       mapping = aes(x = age,
                     y = patient,
                     color=event))+
  geom_line(mapping=aes(group=patient),
            color="black",
            linewidth = 1.5)+
  geom_point(shape=1,size=5,stroke=2)+
  labs(x = "Age(yrs)",
       y = "Patient",
       title= "Line Plot Chronology of WS Pt Medical Events (Child Pts)"
       )

#plot for way older pts
ggplot(data = olderptdata,
       mapping = aes(x = age,
                     y = patient,
                     color=event))+
  geom_line(mapping=aes(group=patient),
            color="black",
            linewidth = 1.5)+
  geom_point(shape=1,size=5,stroke=2)+
  labs(x = "Age(yrs)",
       y = "Patient",
       title= "Line Plot Chronology of WS Pt Medical Events (Adult Pts)"
  )


#Saving ggplots
ggsave(plot = timeline, filename = "timeline.png",
       dpi = 300, #I generally recommend ~300 for something you wish to print
       width = 5,height = 6,units = "in")
