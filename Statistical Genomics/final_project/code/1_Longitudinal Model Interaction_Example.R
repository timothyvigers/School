

#M.All.Long.All (NAME OF METHYLATION M VALUES DATASET)
#Pheno.All.Long  (PHENOTYPE FILE NAME)

table(Pheno.All.Long$Group)

#Data CHECK #1: is ordering correct?
  Check.1 = as.character( colnames(M.All.Long.All) )
  Check.2 = as.character(Pheno.All.Long$Array)
  #must return 0 to proceed
  sum(ifelse(Check.1==Check.2, 0, 1))
  

#Covariates that will be included in the model
Female_YN = ifelse(Pheno.All.Long$SEX=="Female", 1, 0)
Visit = Pheno.All.Long$Visit
Group = as.factor(Pheno.All.Long$Group)
CD8T	= Pheno.All.Long$CD8T
CD4T = Pheno.All.Long$CD4T
NK = Pheno.All.Long$NK
Bcell	= Pheno.All.Long$Bcell
Mono = Pheno.All.Long$Mono
Gran = Pheno.All.Long$Gran
ID = Pheno.All.Long$ID
Age = Pheno.All.Long$Age
S.W.T.All = Pheno.All.Long$S.W.T.All
Dataset = Pheno.All.Long$Dataset

#NOTE: NEEDED CUSTOM CONTRASTS FOR MY DISSERTATION, YOU CAN IGNORE

#Estimates
E1 = matrix(c(0,0,0,1,0,0,0,0,0,0,0,0,1,0), nrow =1)
E2 = matrix(c(0,0,0,1,0,0,0,0,0,0,0,0,0,1), nrow =1)
E3 = matrix(c(0,0,0,1,0,0,0,0,0,0,0,0,0,0), nrow =1)
E4 = matrix(c(0,0,0,0,1,0,0,0,0,0,0,0,0,0), nrow =1)
E5 = matrix(c(0,0,0,0,0,1,0,0,0,0,0,0,0,0), nrow =1)


#Contrasts
C1 = matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,-1,0), nrow =1)
C2 = matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,-1), nrow =1)
C = rbind(C1, C2)


library(plyr)
library(doParallel)
library(nlme)


Output.MM.Reversion.INT.Long = NULL

a = 1:nrow(M.All.Long)

MM.Int = function(a){

  M = M.All.Long[a,]
    
  Long.MM.Fit <- try(lme (M ~  Group + Visit + Group*Visit + Female_YN + Age + CD8T +	CD4T +	NK +	Bcell +	Mono + Dataset, random=~1|ID, 
                      control = lmeControl(maxIter = 1e8, msMaxIter = 1e8), method = "REML",  
                      na.action=na.omit))
  want = rep(NA, length=20)
  if("try-error" %in% class(Long.MM.Fit)){want = want}else{
    
    A1 = anova(Long.MM.Fit, L = C)
    
    numDF.1 = A1$numDF[1]
    denomDF.1 = A1$denDF[1]
    FStat.1 = A1$`F-value`[1]
    Pval.1 = pf(FStat.1, numDF.1, denomDF.1, lower.tail = FALSE)
    
    B0 = summary(Long.MM.Fit)$coefficients$fixed[1]
    B1 = summary(Long.MM.Fit)$coefficients$fixed[2]
    B2 = summary(Long.MM.Fit)$coefficients$fixed[3]
    B3 = summary(Long.MM.Fit)$coefficients$fixed[4]
    B4 = summary(Long.MM.Fit)$coefficients$fixed[5]
    B5 = summary(Long.MM.Fit)$coefficients$fixed[6]
    B6 = summary(Long.MM.Fit)$coefficients$fixed[7]
    B7 = summary(Long.MM.Fit)$coefficients$fixed[8]
    B8 = summary(Long.MM.Fit)$coefficients$fixed[9]
    B9 = summary(Long.MM.Fit)$coefficients$fixed[10]
    B10 = summary(Long.MM.Fit)$coefficients$fixed[11]
    B11 = summary(Long.MM.Fit)$coefficients$fixed[12]
    B12 = summary(Long.MM.Fit)$coefficients$fixed[13]
    B13 = summary(Long.MM.Fit)$coefficients$fixed[14]
    
    B.P.2v1 = B3 + B12
    B.R.2v1 = B3 + B13
    B.M.2v1 = B3
    
    P.P.2v1 = anova(Long.MM.Fit, L = E1)$`p-value`[1]
    P.R.2v1 = anova(Long.MM.Fit, L = E2)$`p-value`[1]
    P.M.2v1 = anova(Long.MM.Fit, L = E3)$`p-value`[1]
    
    A2 = anova(Long.MM.Fit, L = E4)
    B.Sex = B4
    numDF.Sex = A2$numDF[1]
    denomDF.Sex = A2$denDF[1]
    F.Sex = A2$'F-value'
    P.Sex = A2$`p-value`[1]
    
    A3 = anova(Long.MM.Fit, L = E5)
    B.Age = B5
    numDF.Age = A3$numDF[1]
    denomDF.Age = A3$denDF[1]
    F.Age = A3$'F-value'
    P.Age = A3$`p-value`[1]
    

    want = c(numDF.1, 
                      denomDF.1, 
                      FStat.1,
                      Pval.1,
                      B.P.2v1, 
                      P.P.2v1,
                      B.R.2v1, 
                      P.R.2v1,
                      B.M.2v1, 
                      P.M.2v1,
                      B.Sex, 
                      numDF.Sex,
                      denomDF.Sex,
                      F.Sex,
                      P.Sex,
                      B.Age, 
                      numDF.Age,
                      denomDF.Age,
                      F.Age,
                      P.Age)
  }
  
  names(want) = c("GroupxVisit.numDF", "GroupxVisit.denomDF", "GroupxVisit.FStat","GroupxVisit.Pval",
                  "Beta.P.2v1", "Pval.P.2v1", 
                  "Beta.R.2v1", "Pval.R.2v1",
                  "Beta.M.2v1", "Pval.M.2v1",
                  "B.Sex", "Sex.numDF", "Sex.denomDF", "Sex.FStat","Sex.Pval",
                  "B.Age", "Age.numDF", "Age.denomDF","Age.FStat", "Age.Pval")
  
  return(want)
}


#Start of parallel processing
#Using parallel processing to decrease run time
numCores = detectCores()-1

cl=makeCluster(numCores,type="SOCK")
clusterEvalQ(cl, library(nlme))
clusterExport(cl, c("M.All.Long", "ID", "Age", "Visit", "Group", "Female_YN", "CD8T",	"CD4T",	"NK",	
                    "Bcell",	"Mono",	"Gran", "E1", "E2", "E3", "E4", "E5", "C", "Dataset"))
Output.MM.Reversion.INT.All.0 = clusterApply(cl, a, MM.Int)
stopCluster(cl)

#Converting from list to dataframe
Output.MM.Reversion.INT.All  =  as.data.frame(t(matrix(unlist(Output.MM.Reversion.INT.All.0), nrow=length(unlist(Output.MM.Reversion.INT.All.0[1])))))


row.names(Output.MM.Reversion.INT.All) = row.names(M.All.Long)
colnames(Output.MM.Reversion.INT.All) = c("GroupxVisit.numDF", "GroupxVisit.denomDF", "GroupxVisit.FStat","GroupxVisit.Pval",
                                           "Beta.P.2v1", "Pval.P.2v1", 
                                           "Beta.R.2v1", "Pval.R.2v1",
                                           "Beta.M.2v1", "Pval.M.2v1",
                                           "B.Sex", "Sex.numDF", "Sex.denomDF", "Sex.FStat","Sex.Pval",
                                           "B.Age", "Age.numDF", "Age.denomDF","Age.FStat", "Age.Pval")









