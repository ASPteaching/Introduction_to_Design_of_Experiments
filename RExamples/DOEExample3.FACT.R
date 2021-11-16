##############################################
### R CODE FOR EXAMPLE 3: FACTOEIAL DESIGN ###
##############################################

dades<-expand.grid(replica=1:5, tratamiento = c("Control", "Trat"),cepa=c("CD1", "CBA"))
WBC<-c( 3, 1.7, 1.5,2, 2.1, 1.9, 1.9, 3.5, 1.2, 2.3, 1.9, 2.6, 1.4, 1.6, 1.1, 0.4, 0.2, 0.1, 0.4, 0.3)
dades$cepa <-as.factor(dades$cepa)
dades$tratamiento <-as.factor(dades$tratamiento)
dades <- cbind(dades, WBC)
write.csv2(dades, "dadesFACTD.csv2", row.names=F)

attach(dades)

model.1<- WBC ~ tratamiento + cepa # this model ignores interaction
aov.1<-aov (model.1, data=dades)
summary(aov.1)

model.2<- WBC ~ tratamiento + cepa + cepa:tratamiento
aov.2<-aov (model.2, data=dades)
summary(aov.2) # this shows that interaction had to be considered

interaction.plot(cepa, tratamiento, WBC)

### Check ANOVA model assumptions
opt<- par(mfrow=c(2,2)); plot(aov1); par(opt)

### Clean memory
detach(dades); rm(dades)