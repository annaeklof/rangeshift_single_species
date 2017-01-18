library("MASS")
data(Cars93)
# Regression Tree Example
library(rpart)
library(rattle)
library(RColorBrewer)

 # grow tree. How does mileage vary depending on price, country etc of car?
 fit <- rpart(Mileage~Price + Country + Reliability + Type, 
                               method='anova', data=cu.summary)
 
 printcp(fit) # display the results 
 
 plotcp(fit) # visualize cross-validation results 
 summary(fit) # detailed summary of splits
 
  # create additional plots 
  par(mfrow=c(1,2)) # two plots on one page 
  rsq.rpart(fit) # visualize cross-validation results  	
  
# plot (ugly default) 
     plot(fit, uniform=TRUE, 
                 main="Regression Tree ")
   text(fit, use.n=TRUE, all=TRUE, cex=.8)
# plot (fancy)
   fancyRpartPlot(fit) #You can choose colorscheme with palettes = "hmmhmmhmm". Use display.brewer.all() to see color schemes
   
   
   
   
   #My code
   
   library("MASS")
   data(Cars93)
   library(rpart)
   library(rattle)
   library(rpart.plot)
   library(RColorBrewer)
   myfit <- rpart(Cars93$Price ~Cars93$Horsepower + Cars93$Wheelbase, method='anova')
   
   
   #More tut code (http://gormanalysis.com/decision-trees-in-r-using-rpart/)
   train <- data.frame(ClaimID = c(1,2,3,4,5,6,7),
                       RearEnd = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
                       Whiplash = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
                       Fraud = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
   myfit <- rpart(Fraud ~ RearEnd, data = train, method = "class",minsplit=2, minbucket=1)
   fancyRpartPlot(myfit)

   
      
#Growing a full tree and then pruning
     train <- data.frame(ClaimID = c(1,2,3,4,5,6,7,8,9,10),
                         RearEnd = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
                         Whiplash = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE),
                         Activity = factor(c("active", "very active", "very active", "inactive", "very inactive", "inactive", "very inactive", "active", "active", "very active"),
                                           levels=c("very inactive", "inactive", "active", "very active"),
                                           ordered=TRUE),
                         Fraud = c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE))
     train
     
     mytree <- rpart(Fraud ~ RearEnd + Whiplash + Activity, data = train, method = "class", minsplit = 2, minbucket = 1, cp=-1)
     fancyRpartPlot(mytree) 
     #Pruning  the tree using the smallest tree's CP which xerror is within 1 st. dev. of
     #the tree with smallest xerror (0.4 + 0.25 = 0.65).
     mytree <- prune(mytree, cp=.21)
     fancyRpartPlot(mytree)
     
   
   
   
   
   
   
   # More shit
     data("Cars93")
     Horsepower <- Cars93$Horsepower - mean(Cars93$Horsepower)
     Horsepower <- Horsepower / sd(Horsepower)
     Wheelbase <- Cars93$Wheelbase - mean(Cars93$Wheelbase)
     Wheelbase <- Wheelbase / sd(Wheelbase)
     Price <- Cars93$Price - mean(Cars93$Price)
     Price <- Price / sd(Price)
     
     myfit <- rpart(Price ~Horsepower + Wheelbase, method='anova')
        #prune
      myfit <- rpart(Price ~Horsepower + Wheelbase, method='anova', cp=0.082)
   
   
   
   
   
   #http://www.unc.edu/courses/2010spring/ecol/562/001/docs/lectures/lecture22.htm TUTORIAL
      Bahama<-read.table( 'http://www.unc.edu/courses/2010spring/ecol/562/001/data/lab11/Bahama.txt', header=T)
      Ditch<-read.table('http://www.unc.edu/courses/2010spring/ecol/562/001/data/lab11/Ditch.txt', header=T)
   
      par(mfrow=c(1,2))
      rsq.rpart(fit)

  #BAHAMA thing
      #Months is factors... create labels
      set.seed(50)
      ditch_tree<-rpart(factor(Site)~., data=Ditch[,c(1,4:18)], control=rpart.control(cp=0.001, minsplit=15))
   
   
   
   #MY STUFF
      folder = "C:/Regression/"
      #load data
      Datafile = "OutputSubTypes.csv"
      Datafile = paste(folder, Datafile ,sep="")
      MyData <- read.table(Datafile, head=TRUE,sep=',')
      MyTree <- rpart(PatchesOccupied~factor(LandscapeType) + aValue + 
                        factor(Simlength), data=MyData,control = rpart.control(cp = 0.001, xval = 10  ))
      par(mfrow=c(1,2))
      rsq.rpart(MyTree)
      PrunedTree <- prune(MyTree, cp= 0.005)
  # Save PNG
      folder = "C:/Regression/"
      Title = "test"
      png={
        filename <- paste(folder,Title,".png",sep="")
        png(file=filename,width=10,height=7,units="in",res=300)
        
      }
      #PlotSometing here
      
      fancyRpartPlot(MyTree)
      title(Title,outer=T,cex.main = 1, font.main = 1, line=-2)
      dev.off()
   
      
      
      #CODE FOR THE SHORT INIT TIME (OLD RESULTS)
      #MY STUFF
      
      folder = "C:/Regression/LongInitNoMortality/"
      #load data
      Datafile = "ResultsWithoutMortality.csv"
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
      