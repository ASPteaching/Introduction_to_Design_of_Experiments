#################################
### R CODE FOR EXAMPLE 2: RBD ###
#################################

regeneration<-c(34.98,41.22,36.94,39.97,40.89,46.69, 46.65, 41.90, 42.07, 49.42, 52.68, 42.91, 37.18, 45.85, 40.23, 39.20, 37.99, 41.99, 37.61, 40.45, 34.89, 50.15, 44.57, 43.29)
dades<-expand.grid(medium=1:4, Tx =1:6)
dades<-cbind(dades,regeneration)
dades$medium<-as.factor(dades$medium)
dades$Tx<-as.factor(dades$Tx)
write.csv2(dades, "dadesRBD.csv2", row.names=TRUE)

attach(dades)


model.1<- regeneration ~ medium + Tx
aov.1<-aov (model.1, data=dades)
summary(aov.1)

### If blocking had been ignored

model.0<- regeneration ~  Tx
aov.0<-aov (model.0, data=dades)
summary(aov.0)

### Check ANOVA model assumptions

opt<- par(mfrow=c(2,2))
plot(aov1)
par(opt)

### Clean memory
detach(dades)
rm(dades)