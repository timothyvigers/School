setwd("/Users/timvigers/GitHub/School/Statistical Genomics/final_project")
# Import original data
load("./data/Mmatrix.platformAdj.regressOut.Rdata")
pheno = read.csv("./data/Infant_Diet_Methylation_Phenotype_Final.csv",
                 na.strings = c("","-999"))
# Subset methylation data
M.adj = M.adj[,colnames(M.adj) %in% pheno$Array]
# Merge
M.adj = t(M.adj) 
probes = colnames(M.adj)
df = cbind(pheno,M.adj[match(pheno$Array,rownames(M.adj)),])
# Write merged data
save(df,probes,file = "./data/final_data.RData")
