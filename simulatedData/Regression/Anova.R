#TESTING SOME ANOVA STUFF
folder = "C:/Regression/ShortInitTime/"
Datafile = "ResultsShortInitTime.csv"
Datafile = paste(folder, Datafile ,sep="")
MyData <- read.table(Datafile, head=TRUE,sep=',')

MyAnova <- aov(PopulationNumber~factor(aValue)*Simlength*factor(LandscapeType),data=MyData) #


layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(MyAnova) # diagnostic plots

summary(MyAnova)
