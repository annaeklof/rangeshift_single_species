# compare simulated and empirical landscapes

setwd("~/Dropbox/JONATAN_MS/data_for_anna/HabitatMatrices/Modified")
tab_67<- read.table("Figures/SummaryStatsEmpiricalLandscapes_v67.csv", header=TRUE, sep=",")
tab_83<- read.table("Figures/SummaryStatsEmpiricalLandscapes_v83.csv", header=TRUE, sep=",")
tab_188<- read.table("Figures/SummaryStatsEmpiricalLandscapes_v188.csv", header=TRUE, sep=",")

species <-rep("sp67",dim(tab_67)[1])
tab_67 <-cbind(tab_67, species)
species <-rep("sp83",dim(tab_83)[1])
tab_83 <-cbind(tab_83, species)
species <-rep("sp188",dim(tab_188)[1])
tab_188 <-cbind(tab_188, species)

tab_emp <- rbind(tab_67, tab_83, tab_188)

# If one wants to remove those landcapes with very high and very low number of patches
lower <- which(tab_emp$cover<20)
upper <- which(tab_emp$cover>600)
remove <- c(lower, upper)
tab_emp<-tab_emp[-remove,]

#setwd("~/Dropbox/JONATAN_MS/TillAnna/ResultsExample/res")
tab_sim <- read.table("../SummaryFigures/SummarySimulatedLandscapes.csv", sep=",", header=TRUE)

setwd("~/Dropbox/JONATAN_MS/data_for_anna/HabitatMatrices/Modified/SummaryFigures/")
# compare the mean pairwise distance
pdf("SummaryFigure_pairDist.pdf", width=18, height=10)
par(mfrow=c(1,2))
t=16
plot((tab_sim[,3]), ylim=c(0.1,0.65), pch=t, cex=2, xaxt = "n", ylab="pairwise distance")
#par(new="add")
points((tab_emp[,3]),ylim=c(0.1,0.65), cex=2, col="grey", pch=t)

boxplot(tab_sim[,3],tab_emp[,3], names=c("simulated","empirical"))
#beanplot(tab_sim[,3],tab_emp[,3], names=c("simulated","empirical"))

dev.off()

plot((tab_sim[,3]), ylim=c(0.1,0.65))
par(new="add")
plot((tab_emp[,3]), ylim=c(0.1,0.65), col="forestgreen")

# compare the mean nn distance
pdf("SummaryFigure_NN.pdf", width=18, height=10)
par(mfrow=c(1,2))
t=16
plot((tab_sim[,5]), ylim=c(0,0.06), cex=2, pch=t, xaxt = "n", ylab="nn index")
#par(new="add")
points((tab_emp[,5]),ylim=c(0,0.06), cex=2, col="grey", pch=t)

boxplot(tab_sim[,5],tab_emp[,5], names=c("simulated","empirical"))
#beanplot(tab_sim[,5],tab_emp[,5], names=c("simulated","empirical"))
dev.off()



pdf("SummaryFigure_clarkevans.pdf", width=18, height=10)
par(mfrow=c(1,3))
t=16
plot((tab_sim[,9]), ylim=c(0,1.5), cex=2, pch=t, xaxt = "n", ylab="Clark evans index")
#par(new="add")
points((tab_emp[,9]),ylim=c(0,1.5), cex=2, col="darkgrey", pch=t)

boxplot(tab_sim[,9],tab_emp[,9], names=c("simulated","empirical"))
means <- c(mean(tab_sim[,9]),mean(tab_emp[,9]))
points(means,col="grey",pch=18, cex=4)
#beanplot(tab_sim[,9],tab_emp[,9], names=c("simulated","empirical"))
dev.off()

pdf("Boxplot_clarkevans.pdf", width=10, height=12)
t=16
par(cex.lab=2)
par(cex.axis=2)
boxplot(tab_sim[,9],tab_emp[,9], names=c("Generated","Empirical"), ylab="Clark-Evans index")
means <- c(mean(tab_sim[,9]),mean(tab_emp[,9]))
points(means,col="red",pch=18, cex=4)
dev.off()

# prepare data for statistical test
# compare clark evans
name_sim <- rep("simulated", dim(tab_sim)[1])
sim_clarkevans <-cbind(name_sim, tab_sim[,9])
name_emp <- rep("empirical", dim(tab_emp)[1])
emp_clarkevans<-cbind(name_emp, tab_emp[,9])
data<-rbind(sim_clarkevans, emp_clarkevans)
colnames(data)<- c("name", "clarkevans")
#data$clarkevans <- as.numeric(data[,2])
#data$names <- data[,1]
data <- data.frame(data)

# compare nn

# do the tests

# Kcross

#Kcross()

# Kolmogorov-Smirnov test
#ks <- ks.test(data[,1], as.numeric(data[,2]))
ks <- ks.test(data$name, as.numeric(data$clarkevans))






