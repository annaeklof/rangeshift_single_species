#color schemes
library(RColorBrewer)

type = "pdf" #= "pdf" for printing

#Load data
Datafile = "C:/bitbucket/rangeshiftsinglesp/dispersalData/DispersalMammals.csv"
MyData <- read.table(Datafile, head=TRUE,sep=';')
filename = "C:/bitbucket/rangeshiftsinglesp/Figures/PlotWithLegendsMini.pdf"

if(type == "pdf"){
pdf(file=filename,width=4,height=4)
}
par(cex=0.66,oma=c(0,0,0,1))
distance <- seq(from = 0, to = 2, by = 0.001)
plot(1,type='n',bty="L",xlim=c(0,2),ylim=c(0,1),xlab='', ylab='',yaxt="n")
title(ylab = 'Dispersal probability',cex.lab=1,line=2.8)
title(xlab = 'Distance (landscape units)',cex.lab=1,line=2.8)
axis(2,at = c(0,.2,.4,.6,.8,1), labels = c(0,.2,.4,.6,.8,1))
axis(2,at = c(.1,.3,.5,.7,.9), labels = c(" "," "," "," "," "),tck=-0.01)
axis(1,at = c(.25,.75,1.25,1.75), labels = c(" "," "," "," "),tck=-0.01)
#cols <- brewer.pal(8,"Dark2") #looks pretty good
#cols <- brewer.pal(8,"Set1") #looks pretty good
#cols <- brewer.pal(8,"Pastel1") #looks pretty good
cols = c("#d7191c","#fdae61","#abd9e9","#2c7bb6")

#Plot the European species
i = 0
for (a in MyData$A..LSize.50.km.){
  i = i+1
  P <- exp(-(distance/a))
  lines(distance,P,lwd=1,lty=2)
}
#Type grouped
DispersalGroups <- c(0.008, 0.064,0.256,1.5)
#GroupLabels <- c("a","b","c","d")
GroupLabels <- c("Disp. Group 1","Disp. Group 2","Disp. Group 3","Disp. Group 4")
i = 0
for (a in DispersalGroups){
  i = i+1
  P <- exp(-(distance/a))
  lines(distance,P,lwd=1.5,lty=1, col=cols[i])
}

#legend
#Use locator function to find where to put them
#legend(1.60,1, GroupLabels,lty=1,lwd=3, col=cols) #for cex=0.66
#legend(1.34,1, GroupLabels,lty=1,lwd=1.5, col=cols,cex=0.83)
par(xpd=TRUE)
legend(1.34,1, c(GroupLabels,"European mammals"),lty=c(1,1,1,1,2),lwd=c(1.5,1.5,1.5,1.5,1), col=c(cols,"black"),
       bty="n")

if(type == "pdf"){
dev.off()
}

#P <- exp(-(distance/0.064))
#lines(distance,P,lwd=2)