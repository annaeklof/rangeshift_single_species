# script for calculating average results for the different landscape types

tab <- read.csv("LongInitNoMortality/ResultsWithoutMortality_new_mod.csv")

# empty table for the results
summaryLandscapeTypes <- matrix(NA, 4,4)
rownames(summaryLandscapeTypes)<- c("2", "6A", "6B", "6C")
colnames(summaryLandscapeTypes)<- c("averagePopDens", "sdPopDens", "averagePatchOcc", "sdPatchOcc")

types <- c("2", "6A", "6B", "6C")


for(i in types){
  
  ent <- which(tab$LandscapeType==i)
  summaryLandscapeTypes[i,1] <- mean(tab$PopulationAtSimEnd[ent])
  summaryLandscapeTypes[i,2] <- sd(tab$PopulationAtSimEnd[ent])
  summaryLandscapeTypes[i,3] <- mean(tab$PatchesAtSimEnd[ent])
  summaryLandscapeTypes[i,4] <- sd(tab$PatchesAtSimEnd[ent])
}

write.table( summaryLandscapeTypes, "summaryLandscapeTypes.csv")

pdf("landscapeBoxplots_PopDens.pdf")
ent1 <- which(tab$LandscapeType=="2")
ent2 <- which(tab$LandscapeType=="6A")
ent3 <- which(tab$LandscapeType=="6B")
ent4 <- which(tab$LandscapeType=="6C")

boxplot(tab$PopulationAtSimEnd[ent1],tab$PopulationAtSimEnd[ent2],tab$PopulationAtSimEnd[ent3], tab$PopulationAtSimEnd[ent4],  names=c("type 2","type 6A", "type 6B", "type 6C"))
dev.off()


pdf("landscapeBoxplots_PatchOcc.pdf")
ent1 <- which(tab$LandscapeType=="2")
ent2 <- which(tab$LandscapeType=="6A")
ent3 <- which(tab$LandscapeType=="6B")
ent4 <- which(tab$LandscapeType=="6C")

boxplot(tab$PatchesAtSimEnd[ent1],tab$PatchesAtSimEnd[ent2],tab$PatchesAtSimEnd[ent3], tab$PatchesAtSimEnd[ent4],  names=c("type 2","type 6A", "type 6B", "type 6C"))
dev.off()


