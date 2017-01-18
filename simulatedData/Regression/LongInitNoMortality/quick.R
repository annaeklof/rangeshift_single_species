
data2 <- (data[(which(data$LandscapeType=="2")), ])
data6a <- (data[(which(data$LandscapeType=="6A")), ])
data6b <- (data[(which(data$LandscapeType=="6B")), ])
data6c <- (data[(which(data$LandscapeType=="6C")), ])

mean(data2$PopulationAtSimEnd/data2$PopulationAtShiftStart)
mean(data6a$PopulationAtSimEnd/data6a$PopulationAtShiftStart)
mean(data6b$PopulationAtSimEnd/data6b$PopulationAtShiftStart)
mean(data6c$PopulationAtSimEnd/data6c$PopulationAtShiftStart)

median(data2$PopulationAtSimEnd/data2$PopulationAtShiftStart)
median(data6a$PopulationAtSimEnd/data6a$PopulationAtShiftStart)
median(data6b$PopulationAtSimEnd/data6b$PopulationAtShiftStart)
median(data6c$PopulationAtSimEnd/data6c$PopulationAtShiftStart)

sd(data2$PopulationAtSimEnd/data2$PopulationAtShiftStart)
sd(data6a$PopulationAtSimEnd/data6a$PopulationAtShiftStart)
sd(data6b$PopulationAtSimEnd/data6b$PopulationAtShiftStart)
sd(data6c$PopulationAtSimEnd/data6c$PopulationAtShiftStart)

pdf("boxplotsFractions_2.pdf")
boxplot(data2$PopulationAtSimEnd/data2$PopulationAtShiftStart)
dev.off()
pdf("boxplotsFractions_6a.pdf")
boxplot(data6a$PopulationAtSimEnd/data6a$PopulationAtShiftStart)
dev.off()
pdf("boxplotsFractions_6b.pdf")
boxplot(data6b$PopulationAtSimEnd/data6b$PopulationAtShiftStart)
dev.off()
pdf("boxplotsFractions_6c.pdf")
boxplot(data6c$PopulationAtSimEnd/data6c$PopulationAtShiftStart)
dev.off()



