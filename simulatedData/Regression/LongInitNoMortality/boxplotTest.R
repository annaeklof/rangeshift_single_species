# code for overview figure

require(ggplot2)
require(corrplot)

#data <- read.csv("ResultsWithoutMortality_new_mod.csv")
data <- read.csv("ResultsWithoutMortality_nov2016.csv")
# some a-values included
dispersal <- c(0.008, 0.064, 0.256, 2.048)

data1<-data[data$aValue==dispersal[1],]
data2<-data[data$aValue==dispersal[2],]
data3<-data[data$aValue==dispersal[3],]
data4<-data[data$aValue==dispersal[4],]
data_new <- rbind(data1, data2, data3, data4)

# Below if one wants to use only four dispersal values
data_all <- read.csv("data_bae.csv", sep="\t")
data_all_patches <- read.csv("data_bae_patches.csv")
data_all_patches <- data_all_patches[,-1]

# Below if one wants to use all dispersal values
dataA<-data[,c(1,2,3,8)]
colnames(dataA)[4] <- "popDensity"
dataA$time <- "1-shiftStart"
dataA$aValue[dataA$aValue==0.001 | dataA$aValue==0.002 | dataA$aValue==0.004 | dataA$aValue==0.008]=1
dataA$aValue[dataA$aValue==0.016 | dataA$aValue==0.032 | dataA$aValue==0.064 | dataA$aValue==0.128]=2
dataA$aValue[dataA$aValue==0.256 | dataA$aValue==0.512 | dataA$aValue==1.024]=3
dataA$aValue[dataA$aValue==2.048 | dataA$aValue==4.096 | dataA$aValue==8.192 | dataA$aValue==16.384]=4

dataB<-data[,c(1,2,3,4)]
colnames(dataB)[4] <- "popDensity"
dataB$time <- "2-shiftEnd"
dataB$aValue[dataB$aValue==0.001 | dataB$aValue==0.002 | dataB$aValue==0.004 | dataB$aValue==0.008]=1
dataB$aValue[dataB$aValue==0.016 | dataB$aValue==0.032 | dataB$aValue==0.064 | dataB$aValue==0.128]=2
dataB$aValue[dataB$aValue==0.256 | dataB$aValue==0.512 | dataB$aValue==1.024]=3
dataB$aValue[dataB$aValue==2.048 | dataB$aValue==4.096 | dataB$aValue==8.192 | dataB$aValue==16.384]=4

dataC<-data[,c(1,2,3,6)]
colnames(dataC)[4] <- "popDensity"
dataC$time <- "3-simEnd"
dataC$aValue[dataC$aValue==0.001 | dataC$aValue==0.002 | dataC$aValue==0.004 | dataC$aValue==0.008]=1
dataC$aValue[dataC$aValue==0.016 | dataC$aValue==0.032 | dataC$aValue==0.064 | dataC$aValue==0.128]=2
dataC$aValue[dataC$aValue==0.256 | dataC$aValue==0.512 | dataC$aValue==1.024]=3
dataC$aValue[dataC$aValue==2.048 | dataC$aValue==4.096 | dataC$aValue==8.192 | dataC$aValue==16.384]=4

data_AllDisp <- rbind(dataA, dataB, dataC)
disp008<- data_all[which(data_all[,2]==2.048),]

# # BOXPLOTS
# # K-value and landscape type
# pdf("K-pop.pdf")
# plot(data$KValue, data$PopulationAtShiftEnd, ylab="Population density")
# points(data$KValue, data$PopulationAtShiftStart, ylab="Population density", col="blue")
# dev.off()
# 
# pdf("K-landscape.pdf")
# plot(data$LandscapeType, data$KValue, ylab="K-value")
# dev.off()
# 
# 
# # Population density at different times
# p <- ggplot(data=disp008, aes(x=time, y=popDensity))+ facet_grid(aValue+Simlength~LandscapeType)
# p <- p + geom_boxplot(outlier.colour = "black", outlier.size = 1) +  labs(x="Time in simulation", y = "Population density") + theme(axis.text = element_text(size = 12), axis.title=element_text(size=14)) 
# 
# ggsave(file="boxplot_density_disp2048.pdf", width=16, height=10, useDingbats=FALSE)
# 
# ##################
# 
# 
# # Patch occupancy at different times
# p <- ggplot(data=data_all_patches, aes(x=time, y=patchesOccupied))+ facet_grid(aValue~LandscapeType)
# p <- p + geom_boxplot(color= c("darkblue", "darkred", "darkblue"), fill=c("white", "white", "white"),  outlier.colour = "black", outlier.size = 1) +  labs(x="Time in simulation", y = "Occupied habitat patches") + theme(axis.text = element_text(size = 14), axis.title=element_text(size=16)) + ggtitle("Number of patches occupied")
# 
# ggsave(file="boxplot_patches.pdf", width=16, height=10, useDingbats=FALSE)
# 
# # Population density at shift start
# p <- ggplot(data=data_new, aes(factor(aValue), data_new$PopulationAtShiftStart))+ facet_grid(Simlength~LandscapeType)
# p <- p + geom_boxplot(outlier.colour = "black", outlier.size = 1, fill="grey")  +  labs(x="Time in simulation",y = "Population density")+theme_bw() + theme(axis.text = element_text(size = 14), axis.title=element_text(size=16)) 
# ggsave(file="boxplot_densityShiftStart.pdf", width=20, height=8, useDingbats=FALSE)
# 
# # Population density at shift end
# p <- ggplot(data=data_new, aes(factor(aValue), data_new$PopulationAtShiftEnd))+ facet_grid(Simlength~LandscapeType)
# p <- p + geom_boxplot(outlier.colour = "black", outlier.size = 1, fill="grey")  +  labs(x="Time in simulation",y = "Population density")+theme_bw() + theme(axis.text = element_text(size = 14), axis.title=element_text(size=16)) 
# ggsave(file="boxplot_densityShiftEnd.pdf", width=20, height=8, useDingbats=FALSE)
# 
# # Population density at simulation end
# p <- ggplot(data=data_new, aes(factor(aValue), data_new$PopulationAtSimEnd))+ facet_grid(Simlength~LandscapeType)
# p <- p + geom_boxplot(outlier.colour = "black", outlier.size = 1, fill="grey")  +  labs(x="Time in simulation",y = "Population density")+theme_bw() + theme(axis.text = element_text(size = 14), axis.title=element_text(size=16)) 
# ggsave(file="boxplot_densitySimEnd.pdf", width=20, height=8, useDingbats=FALSE)
# 
# # Patches at shift start
# p <- ggplot(data=data_new, aes(factor(aValue), data_new$PatchesAtShiftStart))+ facet_grid(Simlength~LandscapeType)
# p <- p + geom_boxplot(outlier.colour = "black", outlier.size = 1, fill="grey")  +  labs(x="Time in simulation",y = "Population density")+theme_bw() + theme(axis.text = element_text(size = 14), axis.title=element_text(size=16)) 
# ggsave(file="boxplot_patchesShiftStart.pdf", width=20, height=8, useDingbats=FALSE)
# 
# # Patches at shift end
# p <- ggplot(data=data_new, aes(factor(aValue), data_new$PatchesAtShiftEnd))+ facet_grid(Simlength~LandscapeType)
# p <- p + geom_boxplot(outlier.colour = "black", outlier.size = 1, fill="grey")  +  labs(x="Time in simulation",y = "Population density")+theme_bw() + theme(axis.text = element_text(size = 14), axis.title=element_text(size=16)) 
# ggsave(file="boxplot_patchesShiftEnd.pdf", width=20, height=8, useDingbats=FALSE)
# 
# # Patches at simulation end
# p <- ggplot(data=data_new, aes(factor(aValue), data_new$PatchesAtSimEnd))+ facet_grid(Simlength~LandscapeType)
# p <- p + geom_boxplot(outlier.colour = "black", outlier.size = 1, fill="grey")  +  labs(x="Time in simulation",y = "Population density")+theme_bw() + theme(axis.text = element_text(size = 14), axis.title=element_text(size=16)) 
# ggsave(file="boxplot_patchesSimEnd.pdf", width=20, height=8, useDingbats=FALSE)
# 
# 
# 
# 
# # New boxplots
# p <- ggplot(data=data, aes(factor(LandscapeType), data$PopulationAtShiftEnd/data$PopulationAtShiftStart))#+ facet_grid(Simlength~aValue)
# #boxplots
# p <- p + geom_boxplot(outlier.colour = "black", outlier.size = 1, fill="grey")  +  labs(x="Number of traits",y = "Overlap")+theme_bw() + theme(axis.text = element_text(size = 14), axis.title=element_text(size=16)) 
# ggsave(file="boxplots_landscapes.pdf", width=20, height=8, useDingbats=FALSE)


# mean density
landscape <- c("2", "6A", "6B", "6C")
# original:
#speed <- c(20,80,160) # this is the number of time steps the climatic shift is happening over -> lower number = faster speed
# reordered
speed <- c(160,80,20) # this is the number of time steps the climatic shift is happening over -> lower number = faster speed
dispersal <- c(0.008, 0.064, 0.256, 2.048)
ResultAll <- matrix(NA,4,12)
ResultAll_median <- matrix(NA,4,12)
ResultAll_sd <- matrix(NA,4,12)

for(i in 1:length(landscape)){
  print(landscape[i])
  Res <- matrix(NA,4,3)
  Res_median <- matrix(NA,4,3)
  Res_sd <- matrix(NA,4,3)
  for(j in 1:length(dispersal)){
    #print(dispersal[j])
    #print(j)
    for(y in 1:length(speed)){
     # print(y)
    part <- data[data$LandscapeType==landscape[i] & data$Simlength==speed[y] & data$aValue==dispersal[j],]
    part$fraction <- part$PopulationAtSimEnd/part$PopulationAtShiftStart
    print(part$fraction)
    change <- which(part$fraction>2)
    if(length(change)>0){
       part$fraction[change]<-2
      print("CHANGES!!!")
     #new_set <- part$fraction[-remove]
     #print("REMOVALS!!!")
    }
    #else{
    #  new_set <- part$fraction
    #}
   #if(length(new_set)==0){
  #   new_set <-1
   #}
    new_set <- part$fraction
   # print(new_set)
  #  readline(prompt="Press [enter] to continue")
    print(y)
    Res[j,y] <- mean(new_set)
    print(Res)
    
    Res_median[j,y] <- median(new_set)
    Res_sd[j,y] <- sd(new_set)
    
    }
  }
  if(i==1){
  ResultAll[,1:3]<-Res
  ResultAll_median[,1:3] <- Res_median
  ResultAll_sd[,1:3] <- Res_sd
  }
  if(
    i==2){
    ResultAll[,4:6]<-Res
    ResultAll_median[,4:6] <- Res_median
    ResultAll_sd[,4:6] <- Res_sd
    
  }
  if(i==3){
    ResultAll[,7:9]<-Res
    ResultAll_median[,7:9] <- Res_median
    ResultAll_sd[,7:9] <- Res_sd
  }
  if(i==4){
    ResultAll[,10:12]<-Res
    ResultAll_median[,10:12] <- Res_median
    ResultAll_sd[,10:12] <- Res_sd
  }
}

colnames(ResultAll)<- c("2_A", "2_B", "2_C", "6A_A", "6A_B", "6A_C", "6B_A", "6B_B", "6B_C", "6C_A", "6C_B", "6C_C")
rownames(ResultAll)<- c("L", "M1", "M2", "H")
write.csv(round(ResultAll, 2), "ResultAll_mean_ShiftEnd.csv")

colnames(ResultAll_median)<- c("2_A", "2_B", "2_C", "6A_A", "6A_B", "6A_C", "6B_A", "6B_B", "6B_C", "6C_A", "6C_B", "6C_C") ###original one, probably speeds are named in wrong order

#colnames(ResultAll_median)<- c("2_f", "2_m", "2_s", "6A_f", "6A_m", "6A_s", "6B_f", "6B_m", "6B_s", "6C_f", "6C_m", "6C_s")

rownames(ResultAll_median)<- c("L", "M1", "M2", "H")
write.csv(round(ResultAll_median,2), "ResultAll_median_ShiftEnd.csv")

colnames(ResultAll_sd)<- c("2_A", "2_B", "2_C", "6A_A", "6A_B", "6A_C", "6B_A", "6B_B", "6B_C", "6C_A", "6C_B", "6C_C")
rownames(ResultAll_sd)<- c("L", "M1", "M2", "H")
write.csv(round(ResultAll_sd,2), "ResultAll_sd.csv_ShiftEnd.csv")

pdf("image.pdf")
image(ResultAll, axes=FALSE, lwd=0)

mtext(text=c("L", "M1", "M2", "H"), side=1, line=0.3, at=c(0,0.35,0.7,1), las=1, cex=1.2)
#mtext(text=c("2_f", "2_m", "2_s", "6A_f", "6A_m", "6A_s", "6B_f", "6B_m", "6B_s", "6C_f", "6C_m", "6C_s"), side=2, line=0.3, at=c(seq(0, 1, length.out = 12)), las=1, cex=1.2)
mtext(text=c("2_A", "2_B", "2_C", "6A_A", "6A_B", "6A_C", "6B_A", "6B_B", "6B_C", "6C_A", "6C_B", "6C_C"), side=2, line=0.3, at=c(seq(0, 1, length.out = 12)), las=1, cex=1.2)  # original one, probably speeds are named in wrong order

dev.off()

# regular heat map
pdf("heat.pdf")
heatmap(ResultAll)
dev.off()

# correlation plot
col4 <- colorRampPalette(c("darkred", "red","lightblue", "darkblue"))
pdf("corrplot_simEnd.pdf")
adjRes <- ResultAll_median#mean(ResultAll)
corrplot(adjRes, is.corr=FALSE, tl.col="black", col=col4(20), cl.lim=c(0,2))
#corrplot(adjRes, is.corr=FALSE, tl.col="black", method="color", tl.pos="n", cl.lim=c(0,3), col=colorRampPalette(c("blue","white","red"))(5))
dev.off()



#+ geom_jitter(position=position_jitter(width=.2),size=1)

