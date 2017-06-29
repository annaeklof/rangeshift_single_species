# script for extracting landscapes from .mat result files

require(R.matlab)
require(spatstat)

analyseSimlandsacpes <- function(filename, replicate){
# read the file
MatFile <- readMat(filename)
Res <- matrix(NA, 1,13)
colnames(Res) <- c("cell", "cover","mean_pairdist", "sd_pairdist", "mean_nndist", "sd_nndist", "X2", "pvalue", "ce", "ce_pvalue", "mean_diff_env", "landscape_type", "percent_middle")
Res[1,1] <- paste(cells[i],"-rep-",replicate,  sep="")

# the x-koordinates
X<-MatFile$results[4]
X <- as.data.frame(X)
#write.table("mat_x.txt", X, col.names=TRUE)
#newMat_x <- read.table("mat_x.txt", sep=" ")
#land_x <- newMat_x[1:500,1] # first 500 points, replicate 1
land_x <- X[1:1000,replicate] # first 500 points, replicate 1

# the y-koordinates
Y<-MatFile$results[5]
Y <- as.data.frame(Y)
#write.table("mat_y.txt", Y)
#newMat_y <- read.table("mat_y.txt", sep=" ")
#land_y <- newMat_y[1:500,1] # first 500 points, replicate 1
land_y <- Y[1:1000,replicate] # first 500 points, replicate 1

# merge x- and y-koordinates
land <- cbind(land_x, land_y)

# two landscapes adjacent
land_new <- cbind(land_x+1, land_y)
land_all <- rbind(land, land_new)

# merge the patches that are cose enough together
land_simple <- unique(round(land,2))
plot(land_simple) 
grid()

# make to point pattern
p_norm<-ppp(land_simple[,1], land_simple[,2])

Kc <- Kest(p_norm)
ce <- clarkevans.test(p_norm)

En <-envelope(p_norm, fun=Lest)

te <- quadrat.test(p_norm, 4)
plot(te)
names(te)

# summary measures for the ppp
Res[1,2] <- dim(land_simple)[1]

Res[1,3] <- mean(pairdist(p_norm))
Res[1,4] <- sd(pairdist(p_norm))

Res[1,5] <- mean(nndist(p_norm))
Res[1,6] <- sd(nndist(p_norm))

Res[1,7] <- te$statistic
Res[1,8] <- te$p.value

Res[1,9] <- ce$statistic
Res[1,10] <- ce$p.value

Res[1,11] <- mean(En$obs-En$theo)

# calculate landscape type (6A, B or C)
lower <-land_x[which(land_x>0.67)]
lowhigh<-lower[which(lower<1.34)]
perc <- length(lowhigh)/length(land_x)

# save wish landscape type
first <-substring(filename, 1, 1)
if(first=="0"){
  print("Type 2 landscape")
  Res[1,12] <- "2"
}

else if(first =="2"){
  print("Type 6 landscape")
if(perc<0.23){
  Res[1,12] <- "6A"
}
else if(perc>0.43){
  Res[1,12] <- "6C"
}

else {
  Res[1,12] <- "6B"
}
}
Res[1,13]<-perc
  
# plot the landscapes
pdf(paste("../SummaryFigures/LandscapePlots/Single/",filename,"_rep=",replicate,"_plot",Res[1,12],".pdf"))
plot(land_simple, pch=16) 
grid()  
dev.off()

return(Res)

}

# starts here
setwd("~/Dropbox/JONATAN_MS/TillAnna/ResultsExample/res")
cells <- list.files("~/Dropbox/JONATAN_MS/TillAnna/ResultsExample/res")
reps <- 30
#Res <- matrix(NA, length(cells)*reps, 13)
#colnames(Res) <- c("cell", "cover","mean_pairdist", "sd_pairdist", "mean_nndist", "sd_nndist", "X2", "pvalue", "ce", "ce_pvalue", "mean_diff_env", "landscape_type", "percent_middle")
# save the final results
Results <- data.frame()

for(i in 1:length(cells)){
  print(cells[i])
  filename <- cells[i]
  for(j in 1:reps){
    print(paste("replicate = ",j), sep="")
  Res <- analyseSimlandsacpes(filename,j)
  Results <- rbind(Results, Res)
  }
  
}

write.table(Results, "../SummaryFigures/SummarySimulatedLandscapesNew.csv", sep=",", row.names=FALSE, col.names=TRUE)

## read the table
#tabsim <- read.table("../SummaryFigures/SummarySimulatedLandscapes_adjacentlandscapes.csv", sep=",", header=TRUE)





