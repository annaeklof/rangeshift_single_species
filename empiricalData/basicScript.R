# from grid to point pattern data

library("rworldmap")
m <- read.table("V67_corine_suit50_colonisable.csv", sep=",", header=TRUE)
#m <- read.table("V188_corine_suit50_colonisable.csv", sep=",", header=TRUE)
m <- m[8000:9000,]
M <- subset(m, m$hab==1)
#plot(newmap, xlim = c(-20, 59), ylim = c(35, 71), asp = 1)
#par(new=TRUE) # MAP IS NOT THE RIGHT ONE
plot(0, xlim=range(m$XKM), ylim=range(m$YKM))#, add=TRUE)
#plot(0, ylim=c(300000,6000000), xlim=c(-2000000,-1600000))#, add=TRUE)


points(M$XKM, M$YKM, col="blue", pch=19, cex=0.01)


# just do a certain part of the data
