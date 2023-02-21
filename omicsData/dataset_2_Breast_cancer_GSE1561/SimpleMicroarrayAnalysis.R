## ----librerias, message=FALSE--------------------------------------------
installifnot <- function (pckgName){
if(!(require(pckgName, character.only=TRUE))){
  BiocManager::install(pckgName)
  }
}
installifnot("hgu133a.db")
installifnot("limma")
installifnot("annotate")

## ---- preparaDirectorios-------------------------------------------------
workingDir <-getwd()
dataDir <-file.path(workingDir, "omicsData/dataset_2_Breast_cancer_GSE1561/data")
resultsDir <- file.path(workingDir,"omicsData/dataset_2_Breast_cancer_GSE1561")
setwd(workingDir)

## ----leerDatos-----------------------------------------------------------
load(file.path(dataDir, "datos.normalizados.Rda"))
Biobase::write.exprs(eset_rma, file=file.path(dataDir, "BreastCancerGSE1561.csv"),
            sep=",", dec=".")
class(eset_rma)
dim(exprs(eset_rma))
BreastCancerGSE1561 <- read.csv(file.path(dataDir, "BreastCancerGSE1561.csv"),
                                row.names=1)
targets<- read.table(file=file.path(dataDir, "targets.txt"), 
                     head=TRUE, sep="\t")

## ---- matDesign----------------------------------------------------------
design<-matrix(
            c(1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,1,1,1,1,1),
            nrow=15,byrow=F)
colnames(design)<-c("A", "B", "L")
print(design)


## ----. setContrasts------------------------------------------------------
require(limma)
cont.matrix <- makeContrasts (
      AvsB = B-A,
      AvsL = L-A,
      BvsL = L-B,
      levels=design)
print(cont.matrix)

## ---- linearmodelfit-----------------------------------------------------
require(limma)
fit<-lmFit(BreastCancerGSE1561, design)
fit.main<-contrasts.fit(fit, cont.matrix)
fit.main<-eBayes(fit.main)

## ---- topTabs1-----------------------------------------------------------
topTab_AvsB <- topTable (fit.main, number=nrow(fit.main), coef="AvsB", adjust="fdr"); head(topTab_AvsB)
topTab_AvsL <- topTable (fit.main, number=nrow(fit.main), coef="AvsL", adjust="fdr"); head(topTab_AvsL)
topTab_BvsL  <- topTable (fit.main, number=nrow(fit.main) , coef="BvsL", adjust="fdr"); head(topTab_BvsL)

## ---- volcano------------------------------------------------------------
annotation(eset_rma)
probeNames <-rownames(fit.main)
Symbols <- getSYMBOL(probeNames, annotation(eset_rma))
myNames <- paste(probeNames, Symbols, sep=".")
head(myNames)
volcanoplot(fit.main, coef="AvsB", highlight=10, names=myNames, main = "A vs B")
volcanoplot(fit.main, coef="AvsL", highlight=10, names=myNames, main = "A vs L")
volcanoplot(fit.main, coef="BvsL", highlight=10, names=myNames, main = "B vs L")

## ----saveresults---------------------------------------------------------
write.table(topTab_AvsB, file=file.path(resultsDir, "topTab_AvsB.txt"), sep="\t")
write.table(topTab_AvsL, file.path(resultsDir, "topTab_AvsL.txt"), sep="\t")
write.table(topTab_BvsL , file=file.path(resultsDir, "topTab_BvsL.txt"), sep="\t")

