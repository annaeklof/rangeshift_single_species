#CODE FOR THE REGRESSION TREES AND FIGURES
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#LOAD DATA
#folder = "C:/Regression/LongInitNoMortality/"
folder = "/Users/annek/Dropbox/JONATAN_MS/TillAnna/Regression/LongInitNoMortality/"
Datafile = "ResultsWithoutMortality_nov2016.csv"
#folder = "C:/Regression/ShortInitTime/"
#Datafile = "ResultsShortInitTime.csv"
Datafile = paste(folder, Datafile ,sep="")
MyData <- read.table(Datafile, head=TRUE,sep=',')
# use next line for only analysing heterogenous landscapes
#MyData <- MyData[-(which(MyData$LandscapeType==2)),]
MyData$fracpop_ShiftEnd_ShiftStart <- MyData$PopulationAtShiftEnd/MyData$PopulationAtShiftStart
MyData$fracpop_SimEnd_ShiftStart <- MyData$PopulationAtSimEnd/MyData$PopulationAtShiftStart
MyData$fracpatch_SimEnd_ShiftStart <- MyData$PatchesAtSimEnd/MyData$PatchesAtShiftStart
MyData$fracpatch_ShiftEnd_ShiftStart <- MyData$PatchesAtShiftEnd/MyData$PatchesAtShiftStart

sort(unique(MyData$aValue))
MyData$DispGroups <- 0
# Below if all dispersal values (grouped into four groups) are used
# MyData$DispGroups[MyData$aValue==0.001 | MyData$aValue==0.002
#                   | MyData$aValue==0.004 | MyData$aValue==0.008] = 1
# MyData$DispGroups[MyData$aValue==0.032 | MyData$aValue==0.064
#                   | MyData$aValue==0.016 | MyData$aValue==0.128] = 2
# MyData$DispGroups[MyData$aValue==0.256 | MyData$aValue==0.512
#                   | MyData$aValue==1.024] = 3
# MyData$DispGroups[MyData$aValue==2.048 | MyData$aValue==4.096
#                   | MyData$aValue==8.192 | MyData$aValue==16.384] = 4

# Bleow if only four of the dispersal values are used
MyData$DispGroups[MyData$aValue==0.008] = 1
MyData$DispGroups[MyData$aValue==0.064] = 2
MyData$DispGroups[MyData$aValue==0.256] = 3
MyData$DispGroups[MyData$aValue==2.048] = 4
MyData <-MyData[-which(MyData$DispGroups==0),]

#Build Tree
##MyTree1 <- rpart(PopulationNumber~factor(LandscapeType) + aValue + 
  ##                Simlength, data=MyData,  parms = list(split = 'information'),control = rpart.control(cp = 0.001, xval = 10  ))

MyTree1 <- rpart((fracpop_SimEnd_ShiftStart)~factor(LandscapeType) + factor(DispGroups) + 
                   Simlength, data=MyData,  parms = list(split = 'information'),control = rpart.control(cp = 0.001, xval = 10  ))
PrunedTree1 <- prune(MyTree1, cp = MyTree1$cptable[which.min(MyTree1$cptable[,"xerror"]),"CP"]) # cp=0.006
# Save PNG
Title = "PopulationAtSimEnd_PopulationAtShiftStart"
pdf={
  filename <- paste(folder,Title,".pdf",sep="")
  pdf(file=filename,width=10,height=7)
  
}
fancyRpartPlot(PrunedTree1)
title(Title,outer=T,cex.main = 1, font.main = 1, line=-2)
dev.off()

#Build Tree
##MyTree2 <- rpart(PopulationNumber~factor(LandscapeType) + factor(DispGroups) + 
##                  Simlength, data=MyData,  parms = list(split = 'information'),control = rpart.control(cp ##= 0.001, xval = 10  ))

MyTree2 <- rpart((fracpop_ShiftEnd_ShiftStart)~factor(LandscapeType) + factor(DispGroups) + 
                   Simlength, data=MyData,  parms = list(split = 'information'),control = rpart.control(cp = 0.001, xval = 10  ))
PrunedTree2 <- prune(MyTree2, cp = MyTree2$cptable[which.min(MyTree2$cptable[,"xerror"]),"CP"]) #cp= 0.0098
# Save PNG
Title = "PopulationAtShiftEnd_PopulationAtShiftStart"
pdf={
  filename <- paste(folder,Title,".pdf",sep="")
  pdf(file=filename,width=10,height=7)
  
}
fancyRpartPlot(PrunedTree2)
title(Title,outer=T,cex.main = 1, font.main = 1, line=-2)
dev.off()

#Build Tree: patch occupancy
##MyTree3 <- rpart(PatchesAtSimEnd~factor(LandscapeType) + factor(DispGroups) + 
  ##                Simlength, data=MyData,  parms = list(split = 'information'),control = rpart.control(cp = 0.001, xval = 10  ))

MyTree3 <- rpart((fracpatch_SimEnd_ShiftStart)~factor(LandscapeType) + factor(DispGroups) + 
                   Simlength, data=MyData,  parms = list(split = 'information'),control = rpart.control(cp = 0.001, xval = 10  ))
PrunedTree3 <- prune(MyTree3, cp = MyTree3$cptable[which.min(MyTree3$cptable[,"xerror"]),"CP"]) # cp= 0.014
# Save PNG
Title = "PatchesAtSimEnd_PatchesAtShiftStart"
pdf={
  filename <- paste(folder,Title,".pdf",sep="")
  pdf(file=filename,width=10,height=7)
  
}
fancyRpartPlot(PrunedTree3)
title(Title,outer=T,cex.main = 1, font.main = 1, line=-2)
dev.off()

MyTree4 <- rpart((PopulationAtSimEnd)~factor(LandscapeType) + factor(DispGroups) + 
                   Simlength, data=MyData,  parms = list(split = 'information'),control = rpart.control(cp = 0.001, xval = 10  ))
PrunedTree3 <- prune(MyTree3, cp = MyTree3$cptable[which.min(MyTree3$cptable[,"xerror"]),"CP"]) # cp= 0.014
# Save PNG
Title = "PopulationAtSimEnd"
pdf={
  filename <- paste(folder,Title,".pdf",sep="")
  pdf(file=filename,width=10,height=7)
  
}
fancyRpartPlot(PrunedTree3)
title(Title,outer=T,cex.main = 1, font.main = 1, line=-2)
dev.off()

MyTree5 <- rpart((PopulationAtShiftEnd)~factor(LandscapeType) + factor(DispGroups) + 
                   Simlength, data=MyData,  parms = list(split = 'information'),control = rpart.control(cp = 0.001, xval = 10  ))
PrunedTree3 <- prune(MyTree3, cp = MyTree3$cptable[which.min(MyTree3$cptable[,"xerror"]),"CP"]) # cp= 0.014
# Save PNG
Title = "PopulationAtShiftEnd"
pdf={
  filename <- paste(folder,Title,".pdf",sep="")
  pdf(file=filename,width=10,height=7)
  
}
fancyRpartPlot(PrunedTree3)
title(Title,outer=T,cex.main = 1, font.main = 1, line=-2)
dev.off()

MyTree6 <- rpart((fracpatch_ShiftEnd_ShiftStart)~factor(LandscapeType) + factor(DispGroups) + 
                   Simlength, data=MyData,  parms = list(split = 'information'),control = rpart.control(cp = 0.001, xval = 10  ))
PrunedTree3 <- prune(MyTree3, cp = MyTree3$cptable[which.min(MyTree3$cptable[,"xerror"]),"CP"]) # cp= 0.014
# Save PNG
Title = "PatchesAtShiftEnd_PatchesShiftStart"
pdf={
  filename <- paste(folder,Title,".pdf",sep="")
  pdf(file=filename,width=10,height=7)
  
}
fancyRpartPlot(PrunedTree3)
title(Title,outer=T,cex.main = 1, font.main = 1, line=-2)
dev.off()

