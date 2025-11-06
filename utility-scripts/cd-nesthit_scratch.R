source(here("utility-scripts/cd-hit-cluster.R"))
cd80 <- cd_cluster(here("data/inphared_db/0a_clusters/nested/14Apr2025_0A_80sim.out.clstr"))
cd85 <- cd_cluster("data/inphared_db/0a_clusters/nested/14Apr2025_0A_85sim.out.clstr")
cd90 <- cd_cluster("data/inphared_db/0a_clusters/nested/14Apr2025_0A_90sim.out.clstr")
cd95 <- cd_cluster(here("data/inphared_db/0a_clusters/nested/14Apr2025_0A_95sim.out.clstr"))
cd99 <- cd_cluster(here("data/inphared_db/0a_clusters/nested/14Apr2025_0A_99sim.out.clstr"))

write.csv(cd80, here("data/inphared_db/0a_clusters/nested/14Apr2025_0A_80.csv"))
write.csv(cd85, here("data/inphared_db/0a_clusters/nested/14Apr2025_0A_85.csv"))
write.csv(cd90, here("data/inphared_db/0a_clusters/nested/14Apr2025_0A_90.csv"))
write.csv(cd95, here("data/inphared_db/0a_clusters/nested/14Apr2025_0A_95.csv"))
write.csv(cd99, here("data/inphared_db/0a_clusters/nested/14Apr2025_0A_99.csv"))


colnames(cd99) <- c("Sequence_ID", "Cluster99")
cd99$Cluster99<- paste0("99_", cd99$Cluster99)

colnames(cd95) <- c("Sequence_ID", "Cluster95")
cd95$Cluster95<- paste0("95_", cd95$Cluster95)

colnames(cd90) <- c("Sequence_ID", "Cluster90")
cd90$Cluster90<- paste0("90_", cd90$Cluster90)

colnames(cd85) <- c("Sequence_ID", "Cluster85")
cd85$Cluster85<- paste0("85_", cd85$Cluster85)

colnames(cd80) <- c("Sequence_ID", "Cluster80")
cd80$Cluster80<- paste0("80_", cd80$Cluster80)


cd <- merge(cd99, cd95, by="Sequence_ID", all=TRUE) %>%
  merge(cd90, by="Sequence_ID", all=TRUE) %>%
  merge(cd85, by="Sequence_ID", all=TRUE) %>%
  merge(cd80, by="Sequence_ID", all=TRUE)
  
#write.csv(cd, here("data/inphared_db/0a_clusters/nested/14Apr2025_0A_nested.csv"))

cd <- read.csv(here("data/inphared_db/0a_clusters/nested/14Apr2025_0A_nested.csv"),row.names=1)
cd99 <- read.csv(here("data/inphared_db/0a_clusters/nested/14Apr2025_0A_99.csv"),row.names=1)
cd95 <- read.csv(here("data/inphared_db/0a_clusters/nested/14Apr2025_0A_95.csv"),row.names=1)
cd90 <- read.csv(here("data/inphared_db/0a_clusters/nested/14Apr2025_0A_90.csv"),row.names=1)
cd85 <- read.csv(here("data/inphared_db/0a_clusters/nested/14Apr2025_0A_85.csv"),row.names=1)
cd80 <- read.csv(here("data/inphared_db/0a_clusters/nested/14Apr2025_0A_80.csv"),row.names=1)



cd.o <- na.omit(cd)
cd.o <- unique(cd.o[,c(2:6)])

cd.9995 <- unique(cd[,c(2:3)])
cd.9995 <- na.omit(cd.9995)

cd.9590 <- unique((cd[,c(3:4)]))
cd.9590 <- na.omit(cd.9590)
                  
                  
cd.9085 <- unique(cd[,c(4:5)])
cd.9085 <- na.omit(cd.9085)

cd.8580 <- unique(cd[,c(5:6)])
cd.8580 <- na.omit(cd.8580)

cd.nest <- merge(cd99, cd.9995, by="Cluster99", all=TRUE) %>%
  merge(cd.9590, by="Cluster95", all=TRUE) %>%
  merge(cd.9085, by="Cluster90", all=TRUE) %>%
  merge(cd.8580, by="Cluster85", all=TRUE)

cd.nest2 <- merge(cd.nest, cd.8580, by="Cluster85", all=TRUE)
  merge(cd.8580, by="Cluster85", all=TRUE) 
 

cd.nest.u <- unique(cd.nest)
duplicate_rows <- cd.nest[duplicated(cd.nest$Sequence_ID), ]

cd.nest.u <- cd.nest.u[c("Sequence_ID", "Cluster99", "Cluster95", "Cluster90", "Cluster85", "Cluster80")]
#write.csv(cd.nest.u, here("data/inphared_db/0a_clusters/nested/14Apr2025_0A_nested.csv"))



library(DECIPHER)
set.seed(123)
# specify the path to the FASTA file (in quotes)
fas <- here("data/inphared_db/0a_clusters/nested/14Apr2025_0A_80sim.out")

# load the sequences from the file
# change "DNA" to "RNA" or "AA" as needed
seqs <- readDNAStringSet(fas)




clust50 <- Clusterize(seqs,
cutoff=0.5, # > 50% similar
minCoverage=0.5, # > 50% coverage
processors=NULL) # use all CPUs

clust.super <- merge()
