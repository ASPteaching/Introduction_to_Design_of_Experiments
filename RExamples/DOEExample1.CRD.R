#################################
### R CODE FOR EXAMPLE 1: CRD ###
#################################

RESP<-c(96,99,100,104,84,91,90,75,80,90,70,90,84,76,78,78,87,67,66,76)
TREAT<-rep(1:4, each=5)
dades<-data.frame (TREAT, RESP)
dades$TREAT <- as.factor(TREAT)
attach (dades)
write.csv2(dades, file="dadesCRD.csv2", row.names=TRUE)


model <- RESP ~ TREAT

aov1<-aov (model, data=dades)
summary(aov1)

model.tables(aov1)
model.tables(aov1,type="means") # Mitjanes dels grups

(hsd=TukeyHSD(aov1,which="TREAT",conf=0.95))
plot(hsd)

### Check ANOVA model assumptions

opt<- par(mfrow=c(2,2))
plot(aov1)
par(opt)

### Clean memory
detach(dades)
rm(dades)