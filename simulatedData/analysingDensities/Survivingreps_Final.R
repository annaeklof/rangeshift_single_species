rm(list=ls())
library(grid)
#######################################################
#
# De flesta figurer loopar Simlength_print för antal plots osv. och använder sedan ett index fran
#  match(Simlength_print[i],Simlength) for att hamta ratt data från fil
#
#######################FOLDERS#########################
#savefolder = "Z:/TillAnna/FigurerManuskript/Figurer/"
#loadfolder = "Z:/TillAnna/FigurerManuskript/DATA2/Normal/Rexport/"
savefolder = "/Users/annek/Dropbox/jonatan_ms/TillAnna/FigurerManuskript/NewMay/"
loadfolder = "/Users/annek/Dropbox/jonatan_ms/TillAnna/FigurerManuskript/DATA2/Normal/Rexport/"
########################GRAPHICS########################
plotter ='pdf' # ='pdf' to save. ='graphics' to plot or 'png'

##########################PARAMETERS##########################
#These should be loaded from a file containing the variables
#used instead of having them here.

symlist <- c("A","B","C","D")
symbol = 'D'


{
  #These variables must be equal to what was used in simulations!
  PaddingTime = 100 + 100;
  HalfPaddingTime = PaddingTime/2;
  scale <- 1
  #Simlength <- c(10,20,40,80,160,320)
  Simlength <- c(20,80,160)
  Simlength <- Simlength + PaddingTime
  A <- c(0.001,0.002, 0.004, 0.008, 0.016, 0.032, 0.064, 0.128,
         0.256, 0.512, 1.024, 2.048, 4.096, 8.192, 16.3840)
  #A <- c(0.002, 0.032, 0.064, 0.128, 4.096)
  continuity <- c(2)
  contrast <- c(3)
  A.nrinsim = length(A)
  Simlength.nrinsim = length(Simlength)
}
c <- 1;
Simlength_print <- c(20,80,160)
ClimateScenario <- c("A","B","C")
Simlength_print <- Simlength_print + PaddingTime
A_print <- c(0.002, 0.032, 0.064, 0.128,
             4.096)
A_print.nrinsim = length(A_print)

######### 
for(c in 1:length(continuity)){
  #####################LOAD DATA FROM FILE#####################
  ######### 
  #Data stored so Odd columns are mean and Even are the SD around the mean.
  # The first A*2 columns are for Simlength[1] etc.
  filename = paste(loadfolder,"/dataexport_survivingreps_",continuity[c],"_",contrast[c],"_", symbol, ".csv",sep="")
  mydata <- read.table(filename,header = F, sep =",")
  filename = paste(loadfolder,"/dataexport_existingpops_",continuity[c],"_",contrast[c],"_", symbol,".csv",sep="")
  mydata.densities <- read.table(filename,header = F, sep =",")
  columns <- A.nrinsim * Simlength.nrinsim * 2
  index <- which(1:columns %% 2==1)
  mydata.densities <- mydata.densities[,index]
  
  
  #########
  ########################
  ############################PLOTS############################
  #Loop and create a plot with A subplots for each value of simlength.
  
  switch(plotter,
         pdf={
           filename <- paste(savefolder,"Survivingreps_colored_",continuity[c],"_",contrast[c],"_","_",length(A),".pdf",sep="")
           pdf(file=filename,width=8,height=3.5)
         },
         graphics={
           x11(width=8,height=3.5)
         },
         png={
           filename <- paste(savefolder,"Survivingreps_colored_",continuity[c],"_",contrast[c],"_", symbol,".png",sep="")
           png(file=filename,width=8,height=3.5,units="in",res=600)
         }
  )
  opar <- par();
  ##  par(mfrow=c(2,3), oma=c(0,0,2,0)) 
  if(length(Simlength_print) > 3){
    par(mfrow=c(2,3), oma=c(0,0,2,0))  
  }
  else{
    par(mfrow=c(1,4), oma=c(0,0,2.5,0))
    #layout(matrix(c(1,2,3,4), 1 , 4, byrow=TRUE),widths = c(3,3,3,2), heights=c(1,1,1,1)) Only works with 'graphics', and badly
  }
  for(i in 1:length(Simlength_print)){
    
    j <- match(Simlength_print[i],Simlength)                       #Current 
    #Set plot options background color and how many subplots
    
    Start = ((j-1) * length(A)) + 1
    End = Start - 1 + length(A)
    y1 = mydata[1,Start:End]
    #y2 = mydata[2,Start:End]
    x = 1:length(A)

    
    rows <- (1+A.nrinsim*(j-1)) : ((1+A.nrinsim*(j-1))+(A.nrinsim-1))
    densities <- mydata.densities[Simlength[j],rows] #Densities at end of simlength
    scalingDensities <- mydata.densities[HalfPaddingTime,]
    scalingDensities <- scalingDensities[!is.na(scalingDensities)]
    scalingDensities <- mean(scalingDensities)
    #scalingDensities <- 50000
    z1=which(densities >= scalingDensities)
    z2=which(densities >= scalingDensities*0.8 & densities < scalingDensities)
    z3=which(densities >= scalingDensities*0.6 & densities < scalingDensities*0.8)
    z4=which(densities >= scalingDensities*0.4 & densities < scalingDensities*0.6)
    z5=which(densities >= scalingDensities*0.2 & densities < scalingDensities*0.4)
    z6=which(densities >= scalingDensities*0.001 & densities < scalingDensities*0.2)
    #z7=which(densities == 'NaN' || densities < scalingDensities*0.01)
    z7=which(densities == 'NaN' | densities < scalingDensities*0.001)
    #colors = c("#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C") #from colorbrewer
    colors = c("#eff3ff", "#c6dbf0", "#9dcae1", "#6baed8", "#4292c5", "#2172b4", "#084595") #org colors
    plot(x[z1],y1[z1],bty="L",col="black",bg=colors[7],pch=21,xlab="",
         ylab="", yaxt="n",
         ylim=c(-0.1,1.2),xlim=c(0,15.5),cex=3)
    axis(2,at = c(0,0.2,.4,.6,.8,1,1.2), labels = c(0,0.2,.4,.6,.8,1," "))
    title(main=paste("Climate Scenario ", ClimateScenario[i]),cex.main=1)
    if(i==1){
      title(ylab = "Survival rate", cex.lab = 1.4,
            line = 2.8,font.lab = 1)
    }
  # if(i==2){
  #    title(xlab = "Dispersal kernel width", cex.lab = 1.4,
  #          line = 2.8,font.lab = 1)
  #    
  #  }
    points(x[z1],y1[z1],pch=21,col='black',bg=colors[7],cex=3)
    points(x[z2],y1[z2],pch=21,col='black',bg=colors[6],cex=3)
    points(x[z3],y1[z3],pch=21,col='black',bg=colors[5],cex=3)
    points(x[z4],y1[z4],pch=21,col='black',bg=colors[4],cex=3)
    points(x[z5],y1[z5],pch=21,col='black',bg=colors[3],cex=3)
    points(x[z6],y1[z6],pch=21,col='black',bg=colors[2],cex=3)
    points(x[z7],y1[z7],pch=21,col='black',bg=colors[1],cex=3)
    #My colorspace: colors = list("#eff3ff", "#c6dbf0", "#9dcae1", "#6baed8", "#4292c5", "#2172b4", "#084595")
    
    
  } #End of first 3 plots. Now comes legends
  plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='',ylim=c(0,1),xlim=c(0,1))
  #plot.new()
  # places a legend at the appropriate place c("Health","Defense"), # puts text in the legend
  legend(0,0.9,  
         c("0 %","0-20 %","20-40 %","40-60 %","60-80 %", "80-100 %",">100 %"),
         
         #pch=c(21,21,21,21,21,21,21), # gives the legend appropriate symbols (lines)
         #pt.bg = colors,
         fill = colors,
         border = colors,
         title = "Densities",
         cex = 1
  )
  
  maintitle = paste("Surviving replicates", '\n',  "Continuity =", continuity[c], "Contrast =", contrast[c])
  title(main=maintitle,outer=T)
  
  
  lab <- textGrob(label = "Dispersal kernel width", x = unit(0.5,"npc"),y=unit(0.06,"npc"),gp = gpar(cex=0.95))
  grid.draw(lab)
  if(plotter == 'pdf'){
    dev.off()
  }
  if(plotter == 'png'){
    dev.off()
  }
} #end of continuity for-loop
