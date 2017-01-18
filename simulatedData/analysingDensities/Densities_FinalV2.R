rm(list=ls())
#######################################################
#
#   Plottar Densiteter (bl?-r?da plots)
#
# De flesta figurer loopar Simlength_print f?r antal plots osv. och anv?nder sedan ett index fran
#  match(Simlength_print[i],Simlength) for att hamta ratt data fr?n fil
#
#######################FOLDERS#########################
savefolder = "~/Dropbox/JONATAN_MS/TillAnna/FigurerManuskript/NewMay"
loadfolder = "~/Dropbox/JONATAN_MS/TillAnna/FigurerManuskript/DATA2/Normal/Rexport/"
########################GRAPHICS########################
plotter ='graphics' # ='pdf' to save. ='graphics' to plot or 'png'

symbol = 'D' # different landscape types (A-D, D=alla)
#Simlength_print <- c(20,80,160)
Simlength_print <- c(160)
continuity <- c(2)
contrast <- c(3)


##########################PARAMETERS##########################
#These should be loaded from a file containing the variables
#used instead of having them here.
{
  PaddingTime = 100 + 100;
  HalfPaddingTime = PaddingTime/2;
  scale <- 1
  #Simlength <- c(10,20,40,80,160,320)
  Simlength <- c(20,80,160)
  Simlength <- Simlength + PaddingTime
  A <- c(0.001, 0.002, 0.004, 0.008, 0.016, 0.032, 0.064, 0.128,
        0.256, 0.512, 1.024, 2.048, 4.096, 8.192, 16.3840)
  #A <- c(0.002, 0.032, 0.064, 0.128, 4.096) 
  A.nrinsim = length(A)
  Simlength.nrinsim = length(Simlength)
  symlist <- c("A","B","C","D")
}
c <- 1;

Simlength_print <- Simlength_print + PaddingTime
A_print <- c(0.008, 0.064, 0.256,2.048)
A_type <- c("1","2","3","4")


#START OF A FIGURE BLOCK 5
# Figur som plottar densityovertime fast lite mer i en
######### 
scale <- 1/10000
for(c in 1:length(continuity)){
  #####################LOAD DATA FROM FILE#####################
  ######## #
  #Data stored so Odd columns are mean and Even are the SD around the mean.
  # The first A*2 columns are for Simlength[1] etc.
  filename = paste(loadfolder,"dataexport_pops_",continuity[c],"_",contrast[c],"_", symbol,".csv",sep="")
  mydata <- read.table(filename,header = F, sep =",")
  #filename = paste("C:/Rdata/results/dataexport_pops_",continuity[c],"_",contrast[c],".csv",sep="")
  #mydata <- read.table(filename,header = F, sep =",")
  
  #########
  ########################
  ############################PLOTS############################
  #Loop and create a plot with A subplots for each value of simlength."
  switch(plotter,
         pdf={
           filename <- paste(savefolder,"Densitiesallinone_",continuity[c],"_",contrast[c],"_", symbol, "_", Simlength_print-PaddingTime    ,".pdf",sep="")
           pdf(file=filename,width=7,height = 7)
         },
         graphics={
           x11(width=7,height=7)
         },
         png={
           filename <- paste(savefolder,"Densitiesallinone_",continuity[c],"_",contrast[c],"_", symbol, "_", Simlength_print-PaddingTime    ,".png",sep="")
           png(file=filename,width=8,height=8,units="in",res=300)
           
         }
  ) 
  opar <- par();
  #Set plot options background color and how many subplots
  ##  par(mfrow=c(2,3), oma=c(0,0,2,0)) 
  #par(mfrow=c(3,5), oma=c(0,0,2,0),mar= c(3.4,4,3,1)+0.1) # def mar c(5, 4, 4, 2) + 0.1.
  layout(matrix(c(1:12),3,4,byrow=T))
  par(oma=c(0,4,0,0),mar= c(3.4,1,3,1)+0.1) # def mar c(5, 4, 4, 2) + 0.1.
  
  for(i in 1:length(Simlength_print)){
    
    l <- match(Simlength_print[i],Simlength)  
    j <- Simlength_print[i]                       #Current simlength.
    
    
    
    
    for(m in 1:length(A_print)){
      
      k <- match(A_print[m],A)
      start <- 2*A.nrinsim*(l-1) + k*2 - 2
      
      ymin = (mydata[,start+1][1:j]-mydata[,start+2][1:j])*scale
      ymax = (mydata[,start+1][1:j]+mydata[,start+2][1:j])*scale
      y <- c(ymax, ymin[j:1])
      x <- c(1:j,j:1)
      
      plot(x,y,type='l',bty="L",col="#586e75",xlim =c(0,Simlength[l]),
           ylim=c(0,30),cex.axis=1,yaxt="n",xaxt="n", xlab ="", ylab = "")
      if (m == 1 && i == 1){
        par(xpd=NA)
        title(ylab = "Population density", cex.lab = 1.4,
              line = 3,font.lab = 1)
        par(xpd=FALSE)
      }
      #axis(2,at = c(0,5,10,15,20,25,30), labels = c(0," ",10," ",20," ",30))
      axis(2,at = c(0,10,20,30), labels = c(0,10,20,30))
      axis(2,at = c(5,15,25), labels = c(" "," "," "),tck=-0.03)
      axis(1,at = c((j-HalfPaddingTime),HalfPaddingTime), labels = c(" "," "))
      mtext(as.character(HalfPaddingTime),side=1,at = HalfPaddingTime-20, padj = 1,cex = 0.66)
      mtext(as.character((j-HalfPaddingTime)),side=1,at = (j-HalfPaddingTime)+20, padj = 1,cex = 0.66)
      mtext("t",side=1,at = j, padj = 0.5,cex = 0.66)
      #subTitle= paste('Disp. =', A_print[m])
      #subTitle= paste('Disp. Type ' , A_type[A_print == A_print[m]])
      
      #title(subTitle,outer=F,cex.main = 1.4, font.main = 1) #removed these and put this in "header" code
  
      #Needed to put them in two statements, too close to each other.
      #axis(1,at = c((j-HalfPaddingTime),HalfPaddingTime), labels = c((j-HalfPaddingTime),HalfPaddingTime))
      #axis(1,c((j-HalfPaddingTime)))
      #axis(1,c(HalfPaddingTime))
      
      maincol <- "#B2182B" #original: "#ac434c"
      sidecol <- "#2166AC" #original: "#2e6293"
      
      x1 = c(x[1:HalfPaddingTime], x[(length(x)-HalfPaddingTime-1):length(x)])
      y1 = c(y[1:HalfPaddingTime], y[(length(x)-HalfPaddingTime-1):length(x)])
      polygon(x1,y1,col=sidecol, border = sidecol)
      
      x2 = c(x[HalfPaddingTime:(j-HalfPaddingTime)], x[(length(x)-HalfPaddingTime-1-(j-PaddingTime)):(length(x)-HalfPaddingTime-1)])
      y2 = c(y[HalfPaddingTime:(j-HalfPaddingTime)], y[(length(x)-HalfPaddingTime-1-(j-PaddingTime)):(length(x)-HalfPaddingTime-1)])
      polygon(x2,y2,col=maincol, border =maincol )
      
      x3 = c(x[(HalfPaddingTime+j-PaddingTime):j], x[(j+1):(j+HalfPaddingTime+1)])
      y3 = c(y[(HalfPaddingTime+j-PaddingTime):j], y[(j+1):(j+HalfPaddingTime+1)])
      lines(x3,y3,col="black")
      polygon(x3,y3,col=sidecol, border = sidecol)
      lines(mydata[,start+1][1:j]*scale,col="#073642")
      abline(v=HalfPaddingTime,lwd=1.3,xpd=F)
      abline(v=j-HalfPaddingTime,lwd=1.3,xpd=F)
      
    }
    
  }
  #mainTitle = paste("Population Densities")
  #title(main=mainTitle,outer=T)
  #subTitle = paste('\n', '\n',  "Continuity =", continuity[c], "Contrast =", contrast[c])
  
  if((Simlength_print[i]-PaddingTime) == 80){
    mainTitle = paste('Climate Scenario B')
  } else if((Simlength_print[i]-PaddingTime) == 20){ 
    mainTitle = paste('Climate Scenario A')
  } else if((Simlength_print[i]-PaddingTime) == 160){
    mainTitle = paste('Climate Scenario C')
  } else{
    stop("Non-known simlength time")
  }
  
  title(mainTitle,outer=T,cex.main = 1.4,line=-1.75,font.main = 1)
 
    
  if(plotter == 'pdf'){
    dev.off()
  }
  if(plotter == 'png'){
    dev.off()
  }
} #end of continuity for-loop

#END OF A FIGURE BLOCK 5