# script for making a point pattern from the empirical raster data

require(spatstat)

# loop over all result files
analyze.files <- function(sp){

  cells <- list.files(paste("/Users/annek/Projects/bitbuck/RangShiftSingelSp/rangeshiftsinglesp/empiricalData/HabitatMatrices/Modified/",sp, sep=""))
  Res <- matrix(NA, length(cells), 6)
  colnames(Res) <- c("cell", "cover","mean_pairdist", "sd_pairdist", "mean_nndist", "sd_nndist")
  
  
  for(i in 1:length(cells)){
    # initiate empty data frame for saving results
    Res[i,1] <- cells[i]
    print(cells[i])
    mat <- read.table(cells[i], sep= " ")
    mat_dims <- mat[1:2,2]
    mat <- mat[-c(1:6),]
    mat <- as.numeric(as.matrix(mat))

    mat[mat<0]<-0
    mat <- matrix(mat, mat_dims[2],mat_dims[1])
    Res[i,2] <- sum(mat)
  
    # plot as a grid (matrix)
    pdfname <- (paste("/Users/annek/Projects/bitbuck/RangShiftSingelSp/rangeshiftsinglesp/empiricalData/HabitatMatrices/Modified/Figures/",cells[i],"_matrixpic.pdf", sep=""))
    pdf(file=pdfname)
    image(as.matrix(mat), col=c("white", "forestgreen"))
    dev.off()
    
    # plot as points
    p<- which(mat==1, arr.ind=TRUE)
    plot(p)
    
    # normalize to [0 1] landscape
    p[,1]<-p[,1]/dim(mat)[1]
    p[,2]<-p[,2]/dim(mat)[2]
    
    # make to point pattern
    p_norm<-ppp(p[,1], p[,2])
   
    pdfname <- (paste("/Users/annek/Projects/bitbuck/RangShiftSingelSp/rangeshiftsinglesp/empiricalData/HabitatMatrices/Modified/Figures/",cells[i],"_pnorm.pdf", sep=""))
    pdf(file=pdfname)
    plot(p_norm)
    dev.off()
    
    density(p_norm)
    Kc <- Kest(p_norm)
    pdfname <- (paste("/Users/annek/Projects/bitbuck/RangShiftSingelSp/rangeshiftsinglesp/empiricalData/HabitatMatrices/Modified/Figures/",cells[i],"_Kc.pdf", sep=""))
    pdf(file=pdfname)
    plot(Kc)
    dev.off()
    
    # summary measures for the ppp
    Res[i,3] <- mean(pairdist(p_norm))
    Res[i,4] <- sd(pairdist(p_norm))
    
    Res[i,5] <- mean(nndist(p_norm))
    Res[i,6] <- sd(nndist(p_norm))
  }
    return(Res)

}


species <- c("v147") #,"v67", "v188")
for(sp in species){
setwd(paste("/Users/annek/Projects/bitbuck/RangShiftSingelSp/rangeshiftsinglesp/empiricalData/HabitatMatrices/Modified/",sp, sep=""))
Result <- analyze.files(sp)
filename <- paste("/Users/annek/Projects/bitbuck/RangShiftSingelSp/rangeshiftsinglesp/empiricalData/HabitatMatrices/Modified/Figures/SummaryStats_",sp,".csv", sep="")
write.table(Result, filename, sep=",", row.names=FALSE, col.names=TRUE)
}








