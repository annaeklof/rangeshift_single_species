#CODE FOR THE REGRESSION TREES AND FIGURES
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#LOAD DATA
#folder = "C:/Regression/LongInitNoMortality/"
folder = "/Users/annek/Dropbox/JONATAN_MS/TillAnna/Regression/LongInitNoMortality/"
Datafile = "ResultsWithoutMortality_new_mod.csv"
#folder = "C:/Regression/ShortInitTime/"
#Datafile = "ResultsShortInitTime.csv"
Datafile = paste(folder, Datafile ,sep="")
MyData <- read.table(Datafile, head=TRUE,sep=',')
sort(unique(MyData$aValue))
MyData$DispGroups <- 0
MyData$DispGroups[MyData$aValue==0.001 | MyData$aValue==0.002
                  | MyData$aValue==0.004 | MyData$aValue==0.008] = 1
MyData$DispGroups[MyData$aValue==0.032 | MyData$aValue==0.064
                  | MyData$aValue==0.016 | MyData$aValue==0.128] = 2
MyData$DispGroups[MyData$aValue==0.256 | MyData$aValue==0.512
                  | MyData$aValue==1.024] = 3
MyData$DispGroups[MyData$aValue==2.048 | MyData$aValue==4.096
                  | MyData$aValue==8.192 | MyData$aValue==16.384] = 4
#Build Tree
MyTree1 <- rpart(PopulationNumber~factor(LandscapeType) + aValue + 
                  Simlength, data=MyData,  parms = list(split = 'information'),control = rpart.control(cp = 0.001, xval = 10  ))
PrunedTree1 <- prune(MyTree1, cp=0.004) #cp = MyTree1$cptable[which.min(MyTree1$cptable[,"xerror"]),"CP"]) # cp=0.006
# Save PNG
Title = "A_Pop. density After Climatic Shift"
pdf={
  filename <- paste(folder,Title,"_test.pdf",sep="")
  pdf(file=filename,width=10,height=7)
  
}
fancyRpartPlot(PrunedTree1)
title(Title,outer=T,cex.main = 1, font.main = 1, line=-2)
dev.off()

#Build Tree
MyTree2 <- rpart(PopulationNumber~factor(LandscapeType) + factor(DispGroups) + 
                  Simlength, data=MyData,  parms = list(split = 'information'),control = rpart.control(cp = 0.001, xval = 10  ))
PrunedTree2 <- prune(MyTree2, cp=0.01)#cp = MyTree2$cptable[which.min(MyTree2$cptable[,"xerror"]),"CP"]) #cp= 0.0098
# Save PNG
Title = "A_Pop. density After Climatic Shift"
pdf={
  filename <- paste(folder,Title,"V2_test.pdf",sep="")
  pdf(file=filename,width=10,height=7)
  
}
fancyRpartPlot(PrunedTree2)
title(Title,outer=T,cex.main = 1, font.main = 1, line=-2)
dev.off()

#Build Tree: patch occupancy
MyTree3 <- rpart(PatchesAtSimEnd~factor(LandscapeType) + factor(DispGroups) + 
                  Simlength, data=MyData,  parms = list(split = 'information'),control = rpart.control(cp = 0.001, xval = 10  ))
PrunedTree3 <- prune(MyTree3, cp=0.0014) #(cp = MyTree3$cptable[which.min(MyTree3$cptable[,"xerror"]),"CP"]) # cp= 0.014
# Save PNG
Title = "A_PatchesAtSimEnd"
pdf={
  filename <- paste(folder,Title,"V2_test.pdf",sep="")
  pdf(file=filename,width=10,height=7)
  
}
fancyRpartPlot(PrunedTree3)
title(Title,outer=T,cex.main = 1, font.main = 1, line=-2)
dev.off()



