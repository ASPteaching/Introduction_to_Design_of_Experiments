### R code from vignette source 'DOX-Biomedicina-All.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: workingDir
###################################################
workingDir <- getwd()


###################################################
### code chunk number 2: c1readData
###################################################
LOG_CNT<-c(7.66, 6.98, 7.80, 5.26, 5.44, 5.80, 7.41, 7.33, 7.04, 3.51, 2.91, 3.66)
meat<-rep(1:4, each=3)
dades<-data.frame (meat, LOG_CNT)
dades$meat<-as.factor(dades$meat)
levels(dades$meat)<-c("Commercial", "Vacuum", "Mixed Gas", "CO2")
dades
#attach(dades)


###################################################
### code chunk number 3: c1Model
###################################################
model<- dades$LOG_CNT ~ dades$meat


###################################################
### code chunk number 4: c1ANOVA1F
###################################################
aov1<-aov (model, data=dades)
summary(aov1)


###################################################
### code chunk number 5: c1meanTables
###################################################
model.tables(aov1)
model.tables(aov1,type="means") # Mitjanes dels grups


###################################################
### code chunk number 6: c1multcomp
###################################################
(hsd=TukeyHSD(aov1,which="dades$meat",conf=0.95))


###################################################
### code chunk number 7: c1multipleCI
###################################################
plot(hsd)


###################################################
### code chunk number 8: c1endCRD
###################################################
rm(dades)


###################################################
### code chunk number 9: c2readData
###################################################
trac <- c(rep(1,8),rep(2,8),rep(3,8))                
resp <- c(4,2,6,6,5,6,2,6,7,6,5,7,6,4,7,5,9,12,6,11,10,11,9,10)    
myData <- data.frame(trac,resp)
myData[,1]  <- as.factor(myData[,1])


###################################################
### code chunk number 10: c2comparacionesMultiples1
###################################################
aov2<-aov (resp~trac, data=myData)
(tanova<-summary(aov2))
(hsd<-TukeyHSD(aov2) )
plot(hsd)
rm(hsd)


###################################################
### code chunk number 11: c2calculosManuales
###################################################
attach(myData)
numClasses <-3
xA <-resp[1:8]
xC <-resp[17:24]
nA<-length(xA)
nC<-length(xC)
alfa <- 0.05
n<-length(resp)


###################################################
### code chunk number 12: c2Bonferroni
###################################################
S2A <- var(xA)
S2C <- var(xC)
S2.CvsA <- (((nA-1)*S2A+(nC-1)*S2C)/(nA+nC-2))
sdBONF1 <- sqrt(S2.CvsA *(1/nA+1/nC))
# Una forma alternativa de calcular S2CvsA: A partir del test de student de 2 mostres
t2<-t.test(xA,xC, var.equal=TRUE)
varBONF2<- varT2 <- ((t2$estimate[1]-t2$estimate[2])/t2$statistic)^2
sdBONF2<-sqrt(varBONF2)
# Una tercera aproximación: basar-se en el MSE
# Observem que si ens basem en el MSE multiplicarem per "sqrt(1/nA+1/nC)" pero
# si ens basem en la variància de la t de Student no cal
varBONF3 <- MSE <- tanova[[1]]["Residuals", "Mean Sq"]
sdBONF3 <- sqrt(varBONF3)*sqrt(1/nA+1/nC)
# Graus de llibertat de Bonferroni
dfBONF <- dfERR <-  tanova[[1]]["Residuals", "Df"]
# Valor crític per Bonferroni
tcritBONF<-qt(p=1-alfa/(choose(numClasses,2)*2), df=dfBONF)
Bonf <- tcritBONF * sdBONF3
Bonf2 <-tcritBONF * sdBONF2


###################################################
### code chunk number 13: c2Tukey
###################################################
#Per Tukey
dfTUK <- dfERR
tcritTUK <- qtukey(p=1-alfa, nmeans=numClasses, df=dfTUK)
tukeySD <- sqrt(MSE * (1/nA+1/nC)*(1/2)) # El terme 1/2 no apareix arreu!
Tuk <- tcritTUK * tukeySD


###################################################
### code chunk number 14: c2FisherLSD
###################################################
#Per Fisher
dfLSD <- dfERR
fishercrit <-qt(p=1-alfa/2, df=dfLSD)
fisherSD <- sqrt(MSE * (1/nA+1/nC))
Fish <- fishercrit * fisherSD


###################################################
### code chunk number 15: c2resumen
###################################################
Bonf
Tuk
Fish


###################################################
### code chunk number 16: c2agricolae
###################################################
require(agricolae)
(HSD.test(aov2,"trac", group=TRUE)) # Tukey
(LSD.test(aov2,"trac", group=TRUE)) # Fisher


###################################################
### code chunk number 17: c2ghlctExamples
###################################################
lmod <- lm(myData$resp~myData$trac)
K <- diag(length(coef(lmod)))[-1,]
rownames(K) <- names(coef(lmod))[-1]
K


###################################################
### code chunk number 18: c2hipotesisLineal
###################################################
stopifnot(require(multcomp))
hipotLG<-glht(lmod, linfct = K)


###################################################
### code chunk number 19: c2significacionDelModelo
###################################################
summary(hipotLG)
confint(hipotLG)


###################################################
### code chunk number 20: c2comparacionesMultiples
###################################################
 amod <- aov(resp~trac, data = myData)
 summary(glht(amod, linfct = mcp(trac = "Tukey")))
 summary(glht(amod, linfct = mcp(trac = "Dunnet")) )


###################################################
### code chunk number 21: c3dadesRand
###################################################
 resp <-c(98, 97, 99, 96, 91, 90, 93, 92, 96, 95, 97, 95, 95, 96, 99, 98)
 tract <-as.factor(rep(1:4, each=4))
 (dades<-data.frame(resp, tract))


###################################################
### code chunk number 22: c3aovRand
###################################################
 model <- resp~tract
 aov1<-aov(model, data=dades)
 show(s<-summary(aov1))


###################################################
### code chunk number 23: c3varcomponents
###################################################
class(s)
error.variance <- function (sum1) {sum1["Residuals","Mean Sq"]}
treat.MeanSq <- function (sum1) {(sum1[1,"Mean Sq"])}
repnumber <-function (sum1) {(sum1[1,"Df"]+1+sum1[2,"Df"])/(sum1[1,"Df"]+1)}
treat.variance <- function (sum1) {(treat.MeanSq (sum1)- error.variance(sum1))/ repnumber(sum1)}


###################################################
### code chunk number 24: c3varcompfunctions
###################################################
error.variance <- function (sum1) {sum1["Residuals","Mean Sq"]}
treat.MeanSq <- function (sum1) {(sum1[1,"Mean Sq"])}
repnumber <-function (sum1) {(sum1[1,"Df"]+1+sum1[2,"Df"])/(sum1[1,"Df"]+1)}
treat.variance <- function (sum1) {(treat.MeanSq (sum1)- error.variance(sum1))/ repnumber(sum1)}


###################################################
### code chunk number 25: c3varcompComputations
###################################################
s<-summary(aov1)[[1]]
(sigma2.E<-error.variance(s))
(n<-repnumber(s))
(sigma2.A<-treat.variance(s))


###################################################
### code chunk number 26: intraclass
###################################################
(rho=sigma2.A/(sigma2.A+sigma2.E))
alpha=0.05
ci1=s[1,"F value"]/qf(c(1-alpha/2,alpha/2),n-1,s["Residuals","Df"])-1
(ci1=ci1/(ci1+n))


###################################################
### code chunk number 27: c3mixed1
###################################################
stopifnot(require(nlme))
#tractGD =groupedData(formula=resp ~1|tract,data=dades)
dades$tract<-as.factor(dades$tract)
ResLme=lme(resp ~1,random=~1|tract,data=dades)
(s.lme<-summary(ResLme))


###################################################
### code chunk number 28: c3mixed.coefs.sig
###################################################
(coefs.mixed<-intervals(ResLme,0.95))


###################################################
### code chunk number 29: c3vars.compare
###################################################
(coefs.mixed$reStruct$tract["est."])
sqrt(sigma2.A)
(coefs.mixed$sigma["est."])
sqrt(sigma2.E)


###################################################
### code chunk number 30: c5dataRCBD
###################################################
nitrogen<-c(34.98,41.22,36.94,39.97,40.89,46.69, 46.65, 41.90, 42.07, 
  49.42, 52.68, 42.91, 37.18, 45.85, 40.23, 39.20, 37.99, 41.99, 37.61, 
  40.45, 34.89, 50.15, 44.57, 43.29)
dades<-expand.grid(block=1:4, Tx =1:6)
dades<-cbind(dades,nitrogen)
dades$block<-as.factor(dades$block)
dades$Tx<-as.factor(dades$Tx)
attach(dades)


###################################################
### code chunk number 31: c5ANOVA.1
###################################################
model.1<- nitrogen ~ block + Tx
aov.1<-aov (model.1, data=dades)
summary(aov.1)


###################################################
### code chunk number 32: c5ANOVA.0
###################################################
model.0<- nitrogen ~  Tx
aov.0<-aov (model.0, data=dades)
summary(aov.0)


###################################################
### code chunk number 33: c5anova.table
###################################################
sumCRD<-summary(aov.0)[[1]]
sumRCBD<-summary(aov.1)[[1]]
MSECRD<-sumCRD["Residuals", "Mean Sq"]
MSERCBD<-sumRCBD["Residuals", "Mean Sq"]
(ER<-MSECRD/MSERCBD)


###################################################
### code chunk number 34: c5endCRD
###################################################
detach(dades)
rm(dades)


###################################################
### code chunk number 35: c5GRBD.data
###################################################
dades<-expand.grid(respuesta=1:3,tratamiento = c("Droga 1", "Droga 2", "Droga 3"),
patologia=c("a1", "a2"))
resp<-c( 8, 4, 0 , 10, 8, 6 ,8, 6, 4,14, 10, 6 , 4, 2, 0 ,15, 12, 9)
dades$respuesta<-resp
dades$patologia <-as.factor(dades$patologia)
dades$tratamiento <-as.factor(dades$tratamiento)
attach(dades)


###################################################
### code chunk number 36: c5GRBD.ANOVA.1
###################################################
model.1<- respuesta ~ patologia + tratamiento
aov.1<-aov (model.1, data=dades)
summary(aov.1)


###################################################
### code chunk number 37: GRBD.ANOVA.2
###################################################
model.2<- respuesta ~ patologia + tratamiento + patologia:tratamiento
aov.2<-aov (model.2, data=dades)
summary(aov.2)


###################################################
### code chunk number 38: c5GRBDplot
###################################################
interaction.plot(patologia, tratamiento, respuesta)


###################################################
### code chunk number 39: c5GRBD.end
###################################################
detach(dades)
rm(dades)


###################################################
### code chunk number 40: assignaTrats
###################################################
drugs <-c(rep("D1",3), rep("D2",3), rep("D3",3))
pat1 <- sample(drugs,9)
pat2<- sample(drugs, 9)
cbind(pat1, pat2)


###################################################
### code chunk number 41: c5Petersen_122_Bloques
###################################################
dades<-expand.grid(respuesta=1:4, fosforo= c("P1", "P2", "P3"),
parcela=c("Parc_1", "Parc_2"))
resp <- c(11.5,13.6,14.3,14.5,17.1,17.6,17.6,18.1,18.2,17.6,18.2,18.9,
          11.0,11.2,12.1,12.6,8.3,10.5,9.1,12.8,15.7,16.7,16.6,17.5)
dades$respuesta<-resp
dades$parcela <-as.factor(dades$parcela)
dades$fosforo <-as.factor(dades$fosforo)
attach(dades)


###################################################
### code chunk number 42: c5GRBD.ANOVA.3
###################################################
model_Pet<- respuesta ~ parcela * fosforo
aov_Pet<-aov (model_Pet, data=dades)
summary(aov_Pet)


###################################################
### code chunk number 43: c5EfectosPrincipales
###################################################
model.tables(aov_Pet)
model.tables(aov_Pet,type="means") # Mitjanes dels grups


###################################################
### code chunk number 44: c5GRBDplot
###################################################
interaction.plot(parcela, fosforo, respuesta)


###################################################
### code chunk number 45: c5dadesQL
###################################################
load(file="doe-example.rda") 
dat=chc.Data$ch04c2
names(dat);dim(dat)
dat[1:10,]
dat$batch<-factor(dat$batch)
dat$operator<-factor(dat$operator)
dat$formulation<-factor(dat$formulation)
levels(dat$formulation)<-LETTERS[1:5]
levels(dat$assembly)<-c("alpha", "beta", "gamma", "delta", "epsilon")
dat$assembly<-factor(dat$assembly)
dat[1:10,]
attach(dat)


###################################################
### code chunk number 46: c5anovaQL
###################################################
#Anàlisi
aov1=aov(burning.rate~batch+operator+formulation)
summary(aov1)
par(mfrow=c(2,2))
plot(aov1)
par(mfrow=c(1,1))


###################################################
### code chunk number 47: c5tablasQL
###################################################
model.tables(aov1)
(hsd=TukeyHSD(aov1,which="formulation"))
plot(hsd)
#mm=model.matrix(aov1)
#t(mm)%*%mm


###################################################
### code chunk number 48: anovaQGL
###################################################
aov2=aov(burning.rate~batch+operator+assembly+formulation)
summary(aov2)


###################################################
### code chunk number 49: c5tablasQGL
###################################################
model.tables(aov2)
(hsd=TukeyHSD(aov2,which="formulation"))
plot(hsd)
#mm=model.matrix(aov2)
#t(mm)%*%mm


###################################################
### code chunk number 50: c6fact.data
###################################################
myData <- read.table(file="./dades/Asma.csv",sep=";",header=T, dec=",")
colnames(myData) <- c('Grauasma', 'Estacio', 'Farmac') 
myIndFactor <- c(2,3)
myData[,myIndFactor] <- lapply(myData[,myIndFactor],as.factor)


###################################################
### code chunk number 51: c6fact.model.fix
###################################################
data.aov <- aov(Grauasma ~ Estacio*Farmac, myData)
anova(data.aov)  
model.fix<- Grauasma ~ Estacio*Farmac
aov1=aov(model.fix, data=myData)
summary(aov1)


###################################################
### code chunk number 52: c6interactionplots
###################################################
opt<-par(mfrow=c(2,1))
interaction.plot(myData$Farmac,myData$Estacio,myData$Grauasma)
interaction.plot(myData$Estacio,myData$Farmac,myData$Grauasma)
par(opt)


###################################################
### code chunk number 53: c6fact.contrasts
###################################################
model.tables(aov1,type="means") # A:B subtables gives $\mu_{i,j}$


###################################################
### code chunk number 54: boxplots
###################################################
opt<-par(mfrow=c(2,1))
boxplot(Grauasma ~ Estacio, col=c('red','green'),myData,
        cex.axis=0.6,xlab="Estacio",ylab="Grauasma")
boxplot(Grauasma ~ Farmac, col=c('red','green','blue'),myData,
        cex.axis=0.6,xlab="Farmac",ylab="Grauasma")
par(opt)


###################################################
### code chunk number 55: c6tukey
###################################################
TukeyHSD(aov1,which="Estacio") # No need to check each factor
TukeyHSD(aov1,which="Farmac") # No need to check each factor


###################################################
### code chunk number 56: c6assignaTrats
###################################################
assignacio<-expand.grid(resp=1:4, estac=c("Primavera", "Verano", "Otonyo", "Invierno"), 
                     trat=c("A", "B", "C"))
trats <-paste (assignacio$estac, assignacio$trat)
assigna_trats <-sample(trats, length(trats))


###################################################
### code chunk number 57: c6getData
###################################################
dades<-read.table("./dades/Neter.Pearl.Coats.txt",header=T)
names(dades)=c("Value","Coats","Batch","Replic")
attach(dades)
Batch<-as.factor(dades$Batch)
Coats<-as.factor(dades$Coats)


###################################################
### code chunk number 58: c6intplot
###################################################
interaction.plot(Coats,Batch,Value)


###################################################
### code chunk number 59: c6showData
###################################################
opt<-par(mfcol=c(1,2))
boxplot(Value~Batch, main="Efecto del lote", cex.main=0.8)
boxplot(Value~Coats, main="Efecto del número de capas", cex.main=0.8)
par(opt)


###################################################
### code chunk number 60: c6anova
###################################################
ResLml<-lm(Value~Coats+Batch+Coats:Batch)
(sumry<-anova(ResLml))
Reslml.aov<-aov(Value~Coats+Batch+Coats:Batch)
summary(Reslml.aov)


###################################################
### code chunk number 61: c6calcFs
###################################################
(FCoats <- sumry["Coats","Mean Sq"]/sumry["Coats:Batch","Mean Sq"]) # MSA/MSAB
(FBatch <- sumry["Batch","F value"])       # MSB/MSE
(FErr <- sumry["Coats:Batch","F value"])   # MSAB/MSE


###################################################
### code chunk number 62: c6CriticalValues
###################################################
(F.critica.Coat<-qf(0.95,6,36)) # > FCoat
(pvalue.Coat<-1-pf (FCoats,6,36))
(F.critica.Batch<-qf(0.95,2,6))  # < FBatch
(p.value.Batch<- 1-pf(FBatch,2,6))
(F.critica.Interacc<-qf(0.95,3,36)) # > FErr
(p.value.Interacc<-1-pf(FErr,3,36))


###################################################
### code chunk number 63: c6sigmaBatch
###################################################
nCoats<-(sumry["Coats","Df"]+1)
nReplics<-max(dades$Replic)
(sigma2.Batch<- (sumry["Batch","Mean Sq"]-sumry["Residuals","Mean Sq"])/
  (nCoats*nReplics)) # MSA-MSE/na
(sigma.Batch<-sqrt(sigma2.Batch))


###################################################
### code chunk number 64: c6GroupedData
###################################################
library(nlme)
dades.mixed<-dades
names(dades.mixed)=c("Value","Coats","Batch","Replic")
dades.mixedGD=groupedData(formula=Value~Coats|Batch,data=dades.mixed)
dades.mixedGD$Batch<-factor(dades.mixedGD$Batch)
dades.mixedGD$Coats<-factor(dades.mixedGD$Coats)


###################################################
### code chunk number 65: c6nlme.analysis
###################################################
ResLme=lme(Value~Coats,random=~1|Batch,data=dades.mixedGD)
(summary(ResLme))


###################################################
### code chunk number 66: imagenNlme (eval = FALSE)
###################################################
## plot(ResLme)


###################################################
### code chunk number 67: c6ConfInt
###################################################
intervals(ResLme,0.95)


###################################################
### code chunk number 68: c6vars.compare
###################################################
(coefs.mixed<-intervals(ResLme,0.95))
(coefs.mixed$reStruct$Batch["est."])
(sigma.Batch)


###################################################
### code chunk number 69: c6noIntercept
###################################################
ResLme2=lme(Value~Coats-1,random=~1|Batch,,data=dades.mixedGD)
summary(ResLme2)
intervals(ResLme2,0.95)


###################################################
### code chunk number 70: c6fi
###################################################
detach(dades)
rm(dades,dades.mixed)


###################################################
### code chunk number 71: c7readData
###################################################
tract <- c(rep(1,9),rep(2,9))
gabia <- c(rep(1,3),rep(2,3),rep(3,3),rep(1,3),rep(2,3),rep(3,3))                
resp  <- c(0.55,0.61,0.49,0.59,0.65,0.39,0.48,0.59,0.41,0.11,0.20,0.09,0.12,0.09,0.09,0.22,0.30,0.20)   
myData <- data.frame(tract,gabia,resp)


###################################################
### code chunk number 72: c7nestedFormula
###################################################
nested1<- resp~tract + gabia %in% tract
nested2<- resp~ tract + tract/gabia


###################################################
### code chunk number 73: c7nestedAOV
###################################################
ResABinA<-aov(nested1,data=myData); 
ResABinA.2<-aov(nested2,data=myData); 


###################################################
### code chunk number 74: c7nestedAnova1
###################################################
summary(ResABinA)
summary(ResABinA.2)


###################################################
### code chunk number 75: c7gabiesAleat1
###################################################
data.aov <- aov(resp ~ tract + gabia %in% tract, myData)         # generem l'objecte data.aov, un ANOVA 2F jeràrquic
varcomp       <- summary(data.aov)[[1]]                                 # com el segon factor (niat) és aleatori 
varcomp[1,4]  <- varcomp$Mean[1]/varcomp$Mean[2]                      # calcula la F fent el quocient MSA/MSB
varcomp[1,5]  <- 1-pf(varcomp[1,4], varcomp$Df[1],varcomp$Df[2])     # calcul del p-valor
varcomp


###################################################
### code chunk number 76: c7varComp1
###################################################
varcompN  <- as.double(c(varcomp[2,1],varcomp[4,1],varcomp[5,1]))
paste('Component variància factor A',  round(varcompN[1]/sum(varcompN[1:3])*100,4),'%')
paste('Component variància factor B',  round(varcompN[2]/sum(varcompN[1:3])*100,4),'%')
paste('Component variància residual',  round(varcompN[3]/sum(varcompN[1:3])*100,4),'%')


###################################################
### code chunk number 77: c7gabiesAleat2
###################################################
library(nlme)
data.lme <- lme(resp ~ 1, random = ~ 1|tract/gabia, data = myData,method = "REML")
varcomp   <- VarCorr(data.lme)
varcomp


###################################################
### code chunk number 78: c8leedatos
###################################################
load ("doe-example.rda")
dat=chc.Data$ch14c2
dat$machine=factor(dat$machine)


###################################################
### code chunk number 79: c8ancova11
###################################################
# Test the machine effect given diameter
lm1=lm(strength~diameter+machine,data=dat)
# The ANOVA table F test for machine is valid
anova(lm1)
# However,the summary presents a valid t test for diameter
summary(lm1)


###################################################
### code chunk number 80: c8ancova2
###################################################
lm2=lm(strength~machine+diameter,data=dat)
anova(lm2)
summary(lm2)


###################################################
### code chunk number 81: c8ancova3
###################################################
cdiameter=scale(dat$diameter,scale=F)
lm3=lm(strength~cdiameter+machine,data=dat)
anova(lm3) # Identical to anova(lm1)


###################################################
### code chunk number 82: c8efectos
###################################################
dummy.coef(lm3)
dummy.coef(lm3)[[1]]+dummy.coef(lm3)[[3]]


###################################################
### code chunk number 83: c8diagnosticos
###################################################
par(mfrow=c(2,2))
plot(lm3)
par(mfrow=c(2,2))


###################################################
### code chunk number 84: neteja
###################################################
rm(dat,lm1,lm2,cdiameter,lm3)


