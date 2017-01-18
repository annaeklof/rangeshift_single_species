rm(list=ls())
library(grImport)
library(grid)
#######################################################
#
#   Plottar Densiteter (blå-röda plots)
#
# De flesta figurer loopar Simlength_print för antal plots osv. och använder sedan ett index fran
#  match(Simlength_print[i],Simlength) for att hamta ratt data från fil
#
#######################FOLDERS#########################
savefolder = "C:/Rdata/FigurerManuskript/Figurer/"
########################GRAPHICS########################
plotter ='pdf' # ='pdf' to save. ='graphics' to plot or 'png'
arrow <- readPicture("C:/trash/DownArrow.eps.xml") 


##########################PARAMETERS##########################
#These should be loaded from a file containing the variables
#used instead of having them here.
A_group <- c("1","2","3","4")

  #####################LOAD DATA FROM FILE#####################
Species <- read.csv("C:/temp/tabell.csv",header=TRUE,sep=",")

  ############################PLOTS############################
  #Loop and create a plot with A subplots for each value of simlength."
  switch(plotter,
         pdf={
           filename <- paste(savefolder,"Densitiesallinone_header.pdf",sep="")
           pdf(file=filename,width=7,height = 1.5)
         },
         graphics={
           x11(width=7,height = 2)
         }
  )
  opar <- par();
  #Set plot options background color and how many subplots
  ##  par(mfrow=c(2,3), oma=c(0,0,2,0)) 
  #par(mfrow=c(3,5), oma=c(0,0,2,0),mar= c(3.4,4,3,1)+0.1) # def mar c(5, 4, 4, 2) + 0.1.
  layout(matrix(c(1:4),1,4,byrow=T))
  par(oma=c(0,4,0,0),mar= c(4,1,1,1)+0.1) # def mar c(5, 4, 4, 2) + 0.1. #c(bottom,left,top,right)
  
for(i in 1:4){
    
    
#     for(m in 1:length(A_print)){
#       k <- match(A_print[m],A)
#       start <- 2*A.nrinsim*(l-1) + k*2 - 2
#       
#       ymin = (mydata[,start+1][1:j]-mydata[,start+2][1:j])*scale
#       ymax = (mydata[,start+1][1:j]+mydata[,start+2][1:j])*scale
#       y <- c(ymax, ymin[j:1])
#       x <- c(1:j,j:1)
#       
#       plot(x,y,type='l',bty="L",col="#586e75",xlim =c(0,Simlength[l]),
#            ylim=c(0,30),cex.main=1,cex.axis=1.3,yaxt="n",xaxt="n", xlab ="", ylab = "")
#       if (m == 1 && i == 1){
#         title(ylab = "Population density", cex.lab = 1.5,
#               line = 2.5,font.lab = 1)
#       }
#       #axis(2,at = c(0,5,10,15,20,25,30), labels = c(0," ",10," ",20," ",30))
#       axis(2,at = c(0,10,20,30), labels = c(0,10,20,30))
#       axis(2,at = c(5,15,25), labels = c(" "," "," "),tck=-0.03)
#       axis(1,at = c((j-HalfPaddingTime),HalfPaddingTime), labels = c(" "," "))
#       mtext(as.character(HalfPaddingTime),side=1,at = HalfPaddingTime-20, padj = 1,cex = 0.73)
#       mtext(as.character((j-HalfPaddingTime)),side=1,at = (j-HalfPaddingTime)+20, padj = 1,cex = 0.73)
#       mtext("t",side=1,at = j, padj = 0.5,cex = 0.73)
#       #subTitle= paste('Disp. =', A_print[m])
#       subTitle= paste('Disp. Type ' , A_type[A_print == A_print[m]])
#       
#       title(subTitle,outer=F,cex.main = 1.25, font.main = 1)
#       
  #Where to put arrows:
  #|----x---x---x---x----|
  #In x npc units: 0-0.15327-0.40327-0.65327-0.90327
  #0.1235
  #arrow.Xcoords <- c(0.15327,0.40327,0.65327,0.90327) old ones
  arrow.Xcoords <- c(0.1905,0.4196,0.65327,0.88244) 
  arrow.Ycoord <- 0.05
  logo <- pictureGrob(arrow,x=unit(arrow.Xcoords[i], "npc"),y=unit(arrow.Ycoord, "npc"),
                      width=unit(1.5,"cm"), 
                      height=unit(1.5,"cm"), 
                      distort = TRUE,
                      just= c("centre","bottom"))
   #plot(1:10, rnorm(10)) 
  plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='',ylim=c(0,1),xlim=c(0,1),panel.first = grid.draw(logo))  
  #mtext("|",side=1,at = 0.5, padj = 1,cex = 0.73)
  #mtext(paste('Disp. Type ' , A_group[i]),side=3,at = 0.5, adj = 0.5,cex = 0.66)
  text(0.5,0.37, labels = paste(Species$CommonName[Species$Type==i][1]), cex = 1)
  text(0.5,0.57, labels = paste(Species$CommonName[Species$Type==i][2]), cex = 1)
  subTitle= paste('Disp. Group ' , A_group[i])
  #subTitle= paste('Disp. test ')
  title(subTitle,outer=F,cex.main = 1.4, font.main = 1,line=-1)

}
  mainTitle = paste('Species Groups')
#   title(mainTitle,outer=T,cex.main = 1.3)
#   sidetitle = paste("Simlength")
#   title(ylab=sidetitle,outer=T) 
  if(plotter == 'pdf'){
    dev.off()
  }
