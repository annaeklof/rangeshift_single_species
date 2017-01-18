#color schemes
library(RColorBrewer)

type = "pdf" #= "pdf" for printing
Datafile = "C:/Box Sync/JONATAN_MS/DispersalData/DispersalMammals.csv"
MyData <- read.table(Datafile, head=TRUE,sep=';')
filename = "C:/temp/PlotWithLegends.pdf"

if(type == "pdf"){
pdf(file=filename,width=7,height=7)
}
par(cex=1)
distance <- seq(from = 0, to = 2, by = 0.001)
plot(1,type='n',xlim=c(0,2),ylim=c(0,1),xlab='Distance', ylab='Probability')
#cols <- brewer.pal(8,"Dark2") #looks pretty good
cols <- brewer.pal(8,"Set1") #looks pretty good
#cols <- brewer.pal(8,"Pastel1") #looks pretty good

i = 0
for (a in MyData$A..LSize.50.km.){
  i = i+1
  P <- exp(-(distance/a))
  lines(distance,P,lwd=1.5,lty=2)
}
#Type grouped
DispersalGroups <- c(0.008, 0.064,0.256,1.5)
#GroupLabels <- c("a","b","c","d")
GroupLabels <- c("Disp. Group 1","Disp. Group 2","Disp. Group 3","Disp. Group 4")
i = 0
for (a in DispersalGroups){
  i = i+1
  P <- exp(-(distance/a))
  lines(distance,P,lwd=3,lty=1, col=cols[i])
}

#legend
#Use locator function to find where to put them
#legend(1.60,1, GroupLabels,lty=1,lwd=3, col=cols) #for cex=0.66
legend(1.37,1, GroupLabels,lty=1,lwd=3, col=cols)

if(type == "pdf"){
dev.off()
}

#P <- exp(-(distance/0.064))
#lines(distance,P,lwd=2)