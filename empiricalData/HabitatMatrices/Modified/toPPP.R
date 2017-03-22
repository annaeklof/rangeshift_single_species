# script for making a point pattern from the empirical raster data

require(spatstat)

# loop over all result files
analyze.files <- function(sp){

  cells <- list.files(paste("/Users/annek/Projects/bitbuck/RangShiftSingelSp/rangeshiftsinglesp/empiricalData/HabitatMatrices/Modified/",sp, sep=""))
  Res <- matrix(NA, length(cells), 11)
  colnames(Res) <- c("cell", "cover","mean_pairdist", "sd_pairdist", "mean_nndist", "sd_nndist", "X2", "pvalue", "ce", "ce_pvalue", "mean_diff_env")
  
  for(i in 1:length(cells)){
    # initiate empty data frame for saving results
    Res[i,1] <- cells[i]
    print(cells[i])
    
    Res[i,1] <- cells[i]
    print(cells[i])
    if(sp=="v83"){
      mat <- read.table(cells[i])
      print(dim(mat))
    } else {
      mat <- read.table(cells[i], skip=6)
      print(dim(mat))
    }
    
    mat[mat==-9999]<-0
    Res[i,2] <- sum(mat)
    
    # below is for sp=147
   ## mat <- as.matrix(read.table(cells[i], sep= " ", skip=6))
#     con <- file(cells[i],"r")
#     first_line <- readLines(con,n=1)
#     second_line <- readLines(con, n=2)[1]
#     dim1  <-  unlist(strsplit(second_line, split =" "))[2]
#     dim2 <-  unlist(strsplit(first_line, split =" "))[2]
#     mat <- as.numeric(as.matrix(mat))

   ## mat[mat<0]<-0
#     mat <- matrix(mat, mat_dims[2],mat_dims[1])
  ##  Res[i,2] <- sum(mat)
  
    # plot as a grid (matrix)
    pdfname <- (paste("/Users/annek/Projects/bitbuck/RangShiftSingelSp/rangeshiftsinglesp/empiricalData/HabitatMatrices/Modified/Figures/",cells[i],"_matrixpic.pdf", sep=""))
    pdf(file=pdfname)
    image(as.matrix(mat), col=c("white", "forestgreen"))
    print("plot1")
    dev.off()
    
    # plot as points
print(sum(mat))
    p<- which(mat==1, arr.ind=TRUE)
if(length(p)<1){
  Res[i,c(3:11)] <- NA
  
next

}
    plot(p)
    print("plot2")
    
    # normalize to [0 1] landscape
    p[,1]<-p[,1]/dim(mat)[1]
    p[,2]<-p[,2]/dim(mat)[2]
    
    # make to point pattern
p_norm<-ppp(p[,1], p[,2])
print(p_norm)
pdfname <- (paste("/Users/annek/Projects/bitbuck/RangShiftSingelSp/rangeshiftsinglesp/empiricalData/HabitatMatrices/Modified/Figures/",cells[i],"_pnorm.pdf", sep=""))
pdf(file=pdfname, width=12, height=12)
plot(p_norm, cols="black", pch=16, cex=2, main=" ")

p_norm<-ppp(p[,1], p[,2])
print("bye")
dev.off()

density(p_norm)
Kc <- Kest(p_norm)
pdfname <- (paste("/Users/annek/Projects/bitbuck/RangShiftSingelSp/rangeshiftsinglesp/empiricalData/HabitatMatrices/Modified/Figures/",cells[i],"_Kc.pdf", sep=""))
pdf(file=pdfname)
plot(Kc, correction="Ripley")
dev.off()

ce <- clarkevans.test(p_norm)

En <-envelope(p_norm, fun=Lest)


# This test?
te <- quadrat.test(p_norm, 4)
#plot(te)
names(te)

# summary measures for the ppp
Res[i,3] <- mean(pairdist(p_norm))
Res[i,4] <- sd(pairdist(p_norm))

Res[i,5] <- mean(nndist(p_norm))
Res[i,6] <- sd(nndist(p_norm))

Res[i,7] <- te$statistic
Res[i,8] <- te$p.value

Res[i,9] <- ce$statistic
Res[i,10] <- ce$p.value

Res[i,11] <- mean(En$obs-En$theo)
  }
    return(Res)

}


species <- c("v83","v67", "v188")
for(sp in species){
setwd(paste("/Users/annek/Projects/bitbuck/RangShiftSingelSp/rangeshiftsinglesp/empiricalData/HabitatMatrices/Modified/",sp, sep=""))
Result <- analyze.files(sp)
filename <- paste("/Users/annek/Projects/bitbuck/RangShiftSingelSp/rangeshiftsinglesp/empiricalData/HabitatMatrices/Modified/Figures/SummaryStats_",sp,".csv", sep="")
write.table(Result, filename, sep=",", row.names=FALSE, col.names=TRUE)
}


