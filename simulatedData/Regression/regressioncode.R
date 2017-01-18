#CODE FOR THE REGRESSION TREES AND FIGURES
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


#LOAD DATA
#folder = "C:/Regression/LongInitNoMortality/"
#Datafile = "ResultsWithoutMortality.csv"
folder = "C:/Regression/ShortInitTime/"
Datafile = "ResultsShortInitTime.csv"
Datafile = paste(folder, Datafile ,sep="")
MyData <- read.table(Datafile, head=TRUE,sep=',')


#USE MyData TO BUILD TREES AND EXPORT THEM
#Build Tree
MyTree <- rpart(PatchesOccupied~factor(LandscapeType) + aValue + 
                  Simlength, data=MyData,  parms = list(split = 'information'),control = rpart.control(cp = 0.001, xval = 10  ))
PrunedTree <- prune(MyTree, cp= 0.0028)
# Save PNG
Title = "Patches Occ After Climatic Shift"
png={
  filename <- paste(folder,Title,".png",sep="")
  png(file=filename,width=10,height=7,units="in",res=300)
  
}
fancyRpartPlot(PrunedTree)
title(Title,outer=T,cex.main = 1, font.main = 1, line=-2)
dev.off()
#Also Save R-sq graph
Title = "R-square Patches Occ After Climatic Shift"
png={
  filename <- paste(folder,Title,".png",sep="")
  png(file=filename,width=10,height=7,units="in",res=300)
  
}
par(mfrow=c(1,2))
rsq.rpart(MyTree)
title(Title,outer=T,cex.main = 1, font.main = 1, line=-2)
dev.off()


# END PNG SAVe
#Build Tree
MyTree <- rpart(PatchesAtSimEnd~factor(LandscapeType) + aValue + 
                  Simlength, data=MyData,  parms = list(split = 'information'),control = rpart.control(cp = 0.001, xval = 10  ))
PrunedTree <- prune(MyTree, cp= 0.005)
# Save PNG
Title = "Patches Occ At Sim End"
png={
  filename <- paste(folder,Title,".png",sep="")
  png(file=filename,width=10,height=7,units="in",res=300)
  
}
fancyRpartPlot(PrunedTree)
title(Title,outer=T,cex.main = 1, font.main = 1, line=-2)
dev.off()
#Also Save R-sq graph
Title = "R-square Patches Occ At Sim End "
png={
  filename <- paste(folder,Title,".png",sep="")
  png(file=filename,width=10,height=7,units="in",res=300)
  
}
par(mfrow=c(1,2))
rsq.rpart(MyTree)
title(Title,outer=T,cex.main = 1, font.main = 1, line=-2)
dev.off()
# END PNG SAVe
#Build Tree
MyTree <- rpart(PopulationNumber~factor(LandscapeType) + aValue + 
                  Simlength, data=MyData,  parms = list(split = 'information'),control = rpart.control(cp = 0.001, xval = 10  ))
PrunedTree <- prune(MyTree, cp= 0.004)
# Save PNG
Title = "Pop. density After Climatic Shift"
png={
  filename <- paste(folder,Title,".png",sep="")
  png(file=filename,width=10,height=7,units="in",res=300)
  
}
fancyRpartPlot(PrunedTree)
title(Title,outer=T,cex.main = 1, font.main = 1, line=-2)
dev.off()
# END PNG SAVe
#Also Save R-sq graph
Title = "R-square Pop. density After Climatic Shift"
png={
  filename <- paste(folder,Title,".png",sep="")
  png(file=filename,width=10,height=7,units="in",res=300)
  
}
par(mfrow=c(1,2))
rsq.rpart(MyTree)
title(Title,outer=T,cex.main = 1, font.main = 1, line=-2)
dev.off()
#Build Tree
MyTree <- rpart(PopulationAtSimEnd~factor(LandscapeType) + aValue + 
                  Simlength, data=MyData,  parms = list(split = 'information'),control = rpart.control(cp = 0.001, xval = 10  ))
PrunedTree <- prune(MyTree, cp= 0.0053)
# Save PNG
Title = "Pop. density At Sim End"
png={
  filename <- paste(folder,Title,".png",sep="")
  png(file=filename,width=10,height=7,units="in",res=300)
  
}
fancyRpartPlot(PrunedTree)
title(Title,outer=T,cex.main = 1, font.main = 1, line=-2)
dev.off()
#Also Save R-sq graph
Title = "R-square Pop. density At Sim End"
png={
  filename <- paste(folder,Title,".png",sep="")
  png(file=filename,width=10,height=7,units="in",res=300)
  
}
par(mfrow=c(1,2))
rsq.rpart(MyTree)
title(Title,outer=T,cex.main = 1, font.main = 1, line=-2)
dev.off()
# END PNG SAVe
