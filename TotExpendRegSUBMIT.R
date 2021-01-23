library(INLA)
library(fillmap)
library(rgdal)
library(spdep)
library(maptools)
library(corrplot)
library(viridis)
library(visdat)
library(car)

###data setup
data=read.csv("211_CallsByZip&NP_Supply_RemovedExtraZips.csv")
names(data)
NCZips2 <- readOGR("ZIP_Code_Tabulation_Areas.shp")
NCZips2$ZCTA5CE10
data$ï..ZIp[(which(data$ï..ZIp%in%NCZips2$ZCTA5CE10==FALSE))]
NCZips2$ZCTA5CE10[(which(NCZips2$ZCTA5CE10%in%data$ï..ZIp==FALSE))]
NCZips2red=NCZips2[-which(NCZips2$ZCTA5CE10%in%data$ï..ZIp==FALSE),]
lps=coordinates(NCZips2)
ID=cut(lps[,1], range(lps[,1]), include.lowest=TRUE)
NCoutline=unionSpatialPolygons(NCZips2,ID)
plot(NCZips2)
plot(NCoutline)
length(NCZips2red)
nc.nb = poly2nb(NCZips2)
nc.nb.red = poly2nb(NCZips2red)

#NCZips2red$ZCTA5CE10[c(277,361,404,469,534,600)]
#several islands in OBX have no neighbors
nc.nb.red[[361]]=as.integer(c(169,469))
nc.nb.red[[404]]=as.integer(c(400,600))
nc.nb.red[[469]]=as.integer(c(361,534))
nc.nb.red[[534]]=as.integer(c(469))
nc.nb.red[[277]]=as.integer(c(500, 686))
nc.nb.red[[600]]=as.integer(c(404, 656))
#nb2INLA("nczip.adj", nc.nb)
#nb2INLA("nczipred.adj", nc.nb.red)


###reorder data per shp
dataR=data[,c(112,118,113,127,131,128,50,189,32)]
for (i in 1:770){
  dataR[i,]=data[which(data$ï..ZIp==NCZips2red$ZCTA5CE10[i]),c(112,118,113,127,131,128,50,189,32)]
}
head(cbind(dataR$ï..ZIp,data$ï..ZIp,as.numeric(as.character(NCZips2red$ZCTA5CE10))))
#new vars 06/12/20 (already ordered)
dataR$totamt=read.csv("totamt.csv")[,2]
dataR$totempl.md=read.csv("zbp17tot.csv")[,3]
dataR$hlthempl.md=read.csv("zbp17HlthArt.csv")[,3]
dataR$artempl.md=read.csv("zbp17HlthArt.csv")[,6]
names(dataR)
par(mfrow=c(1,4),mar=c(5,4,1,1))
hist(dataR$Population,main="",xlab="Population")
hist(dataR$totempl.md,main="",xlab="Total Employees")
hist(dataR$hlthempl.md,main="",xlab="Health Employees")
hist(dataR$artempl.md,main="",xlab="Arts Employees")

hist(log(dataR$totamt))

###summaries...some missing
summary(dataR)
dataR2=dataR
nms=c("Tot. Nonprofit Supply","Hlth. Nonprofit Supply","Arts Nonprofit Supply",
      "Tot. NC 2-1-1 Req.","Health NC 2-1-1 Req.","Arts NC 2-1-1 Req.",
      "Population","SOVI","Blau Index","Gov't Output",
      "Tot. Forprofit Output","Hlth. Forprofit Output","Arts Forprofit Output")
colnames(dataR2)=nms
par(mar=c(5, 4, 4, 5) + 0.1)
vis_miss(dataR2[,1:10],show_perc_col=F)

####There is missing data in most vars considered here. 
####replace missing with neighboring medians (do loops twice to get the rest of the missing)
dataNeiMed=dataR
#which are missing??
ZipMissing=rep(1,length(dataR[,1]))
ZipMissing[which(complete.cases(dataR))]=0
length(ZipMissing)
table(ZipMissing)#number missing
par(mar=c(0,0,0,0))
fillmap(NCZips2,"",ZipMissing,n.col=3,bk="c",cuts=c(-5,-.5,.5,1.5),
        map.lty=0,leg.cex = 1,leg.loc="bottomleft",
        legendtxt = c("Water or Not NC","No Missing","Missing at least one"))

for (j in 1:2){
  for (i in which(is.na(dataNeiMed$TotalPublicCharities_Expenditures_LN))){
    dataNeiMed$TotalPublicCharities_Expenditures_LN[i]=median(dataNeiMed$TotalPublicCharities_Expenditures_LN[nc.nb.red[[i]]],na.rm=TRUE)
    dataNeiMed$Charities_Health_Expenditures_LN[i]=median(dataNeiMed$Charities_Health_Expenditures_LN[nc.nb.red[[i]]],na.rm=TRUE)
    dataNeiMed$Charities_Arts_Expenditures_LN[i]=median(dataNeiMed$Charities_Arts_Expenditures_LN[nc.nb.red[[i]]],na.rm=TRUE)
    dataNeiMed$TotalRequests_LN[i]=median(dataNeiMed$TotalRequests_LN[nc.nb.red[[i]]],na.rm=TRUE)
    dataNeiMed$Requests_Health_Major10_LN[i]=median(dataNeiMed$Requests_Health_Major10_LN[nc.nb.red[[i]]],na.rm=TRUE)
    dataNeiMed$Requests_Arts_Major10_LN[i]=median(dataNeiMed$Requests_Arts_Major10_LN[nc.nb.red[[i]]],na.rm=TRUE)
    dataNeiMed$Population[i]=median(dataNeiMed$Population[nc.nb.red[[i]]],na.rm=TRUE)
  }
  for (i in which(is.na(dataNeiMed$SOVI_Index_JB_EIGHT))){
    dataNeiMed$SOVI_Index_JB_EIGHT[i]=median(dataNeiMed$SOVI_Index_JB_EIGHT[nc.nb.red[[i]]],na.rm=TRUE)
  }
  for (i in which(is.na(dataNeiMed$BlauIndex_ZipAverage))){
    dataNeiMed$BlauIndex_ZipAverage[i]=median(dataNeiMed$BlauIndex_ZipAverage[nc.nb.red[[i]]],na.rm=TRUE)
  }
  for (i in which(is.na(dataNeiMed$totamt))){
    dataNeiMed$totamt[i]=median(dataNeiMed$totamt[nc.nb.red[[i]]],na.rm=TRUE)
  }
}
summary(dataNeiMed)
dataNeiMed$Population_LN=log(dataNeiMed$Population+.01)
dataNeiMed$totamt_LN=log(dataNeiMed$totamt+.01)
dataNeiMed$totempl.md_LN=log(dataNeiMed$totempl.md+.01)
dataNeiMed$hlthempl.md_LN=log(dataNeiMed$hlthempl.md+.01)
dataNeiMed$artempl.md_LN=log(dataNeiMed$artempl.md+.01)

cor(ZipMissing,dataNeiMed$TotalPublicCharities_Expenditures_LN)
cor(ZipMissing,dataNeiMed$Population)
t.test(dataNeiMed$TotalPublicCharities_Expenditures_LN[which(ZipMissing==1)],
       dataNeiMed$TotalPublicCharities_Expenditures_LN[which(ZipMissing==0)])
t.test(dataNeiMed$Charities_Health_Expenditures_LN[which(ZipMissing==1)],
       dataNeiMed$Charities_Health_Expenditures_LN[which(ZipMissing==0)])
t.test(dataNeiMed$Charities_Arts_Expenditures_LN[which(ZipMissing==1)],
       dataNeiMed$Charities_Arts_Expenditures_LN[which(ZipMissing==0)])
t.test(dataNeiMed$Population[which(ZipMissing==1)],
       dataNeiMed$Population[which(ZipMissing==0)])
fillmap2(NCZips2red,
        "Log Total Nonprofit Supply",
        dataNeiMed$TotalPublicCharities_Expenditures_LN,
        main.line=-1,
        map.lty=0,leg.cex=1,leg.rnd = 2)
fillmap2(NCZips2red,#main.line=-4,
        "Log Total NC 2-1-1 Requests",
        dataNeiMed$TotalRequests_LN,
        main.line=-1,
        map.lty=0,leg.cex=1,leg.rnd = 2)
fillmap2(NCZips2red,
         "Log Health Nonprofit Supply",
         main.line=-1,
         dataNeiMed$Charities_Health_Expenditures_LN,
         map.lty=0,leg.cex=1,leg.rnd = 2)
fillmap2(NCZips2red,#main.line=-4,
         "Log Health NC 2-1-1 Requests",
         main.line=-1,
         dataNeiMed$Requests_Health_Major10_LN,
         map.lty=0,leg.cex=1,leg.rnd = 2)
fillmap2(NCZips2red,
         "Log Arts Nonprofit Supply",
         main.line=-1,
         dataNeiMed$Charities_Arts_Expenditures_LN,
         map.lty=0,leg.cex=1,leg.rnd = 2)
fillmap2(NCZips2red,#main.line=-4,
         "Log Arts NC 2-1-1 Requests",
         main.line=-1,
         dataNeiMed$Requests_Arts_Major10_LN,
         map.lty=0,leg.cex=1,leg.rnd = 2)
fillmap2(NCZips2red,#main.line=-4,
        "Log Population",
        main.line=-1,
        dataNeiMed$Population_LN,
        map.lty=0,leg.cex=1,leg.rnd = 2)
fillmap2(NCZips2red,#main.line=-4,
        "SOVI",
        dataNeiMed$SOVI_Index_JB_EIGHT,
        main.line=-1,
        map.lty=0,leg.cex=1,leg.rnd = 2)
fillmap2(NCZips2red,#main.line=-4,
        "Blau Index",
        main.line=-1,
        dataNeiMed$BlauIndex_ZipAverage,
        map.lty=0,leg.cex=1,leg.rnd = 2)
fillmap2(NCZips2red,#main.line=-4,
        "Log Government Output",
        main.line=-1,
        dataNeiMed$totamt_LN,
        map.lty=0,leg.cex=1,leg.rnd = 2)
fillmap2(NCZips2red,#main.line=-4,
         "Log Total For-profit Output",
         main.line=-1,
         dataNeiMed$totempl.md_LN,
         map.lty=0,leg.cex=1,leg.rnd = 2)
fillmap2(NCZips2red,#main.line=-4,
         "Log Health Total For-profit Output",
         main.line=-1,
         dataNeiMed$hlthempl.md_LN,
         map.lty=0,leg.cex=1,leg.rnd = 2)
fillmap2(NCZips2red,#main.line=-4,
        "Log Arts For-profit Output",
        dataNeiMed$artempl.md_LN,
        main.line=-1,
        map.lty=0,leg.cex=1,leg.rnd = 2)


summary(dataNeiMed$TotalPublicCharities_Expenditures_LN)
summary(dataNeiMed$Charities_Health_Expenditures_LN)
summary(dataNeiMed$Charities_Arts_Expenditures_LN)
summary(exp(dataNeiMed$TotalPublicCharities_Expenditures_LN))
summary(exp(dataNeiMed$Charities_Health_Expenditures_LN))
summary(exp(dataNeiMed$Charities_Arts_Expenditures_LN))
sd(exp(dataNeiMed$TotalPublicCharities_Expenditures_LN))
sd(exp(dataNeiMed$Charities_Health_Expenditures_LN))
sd(exp(dataNeiMed$Charities_Arts_Expenditures_LN))
hist(exp(dataNeiMed$Charities_Health_Expenditures_LN))
hist((dataNeiMed$Charities_Health_Expenditures_LN))


summary(dataNeiMed$TotalRequests_LN)
summary(dataNeiMed$Population_LN)
summary(dataNeiMed$SOVI_Index_JB_EIGHT)
summary(dataNeiMed$BlauIndex_ZipAverage)
summary(dataNeiMed$totamt_LN)
write.csv(dataNeiMed,"dataNeiMed.csv")


#baseline regression models
ftot=TotalPublicCharities_Expenditures_LN~TotalRequests_LN+Population_LN+SOVI_Index_JB_EIGHT+BlauIndex_ZipAverage+totamt_LN+totempl.md_LN
fhlth=Charities_Health_Expenditures_LN~Requests_Health_Major10_LN+Population_LN+SOVI_Index_JB_EIGHT+BlauIndex_ZipAverage+totamt_LN+hlthempl.md_LN
farts=Charities_Arts_Expenditures_LN~Requests_Arts_Major10_LN +Population_LN+SOVI_Index_JB_EIGHT+BlauIndex_ZipAverage+totamt_LN+artempl.md_LN
restot=inla(ftot,data=dataNeiMed,
            quantiles=c(0.005,0.025,0.05,.95,0.975,0.995),
            control.compute = list(dic=TRUE,waic=TRUE))
summary(restot)
reshlth=inla(fhlth,data=dataNeiMed,
             quantiles=c(0.005,0.025,0.05,.95,0.975,0.995),
            control.compute = list(dic=TRUE,waic=TRUE))
summary(reshlth)
resarts=inla(farts,data=dataNeiMed,
             quantiles=c(0.005,0.025,0.05,.95,0.975,0.995),
             control.compute = list(dic=TRUE,waic=TRUE))
summary(resarts)

#pseudo r2 and vif
fittot <- restot$summary.fitted.values[1]
cor(fittot, dataNeiMed$TotalPublicCharities_Expenditures_LN)^2
fithlth <- reshlth$summary.fitted.values[1]
cor(fithlth, dataNeiMed$Charities_Health_Expenditures_LN)^2
fitart <- resarts$summary.fitted.values[1]
cor(fitart, dataNeiMed$Charities_Arts_Expenditures_LN)^2
restotlm=lm(ftot,data=dataNeiMed)
vif(restotlm)
reshlthlm=lm(fhlth,data=dataNeiMed)
vif(reshlthlm)
resartlm=lm(farts,data=dataNeiMed)
vif(resartlm)


#spatial r.e. regression models
id=1:770
ftot2=TotalPublicCharities_Expenditures_LN~TotalRequests_LN+Population_LN+SOVI_Index_JB_EIGHT+BlauIndex_ZipAverage+totamt_LN+totempl.md_LN+
  f(id,model="iid",param=c(2,1))
fhlth2=Charities_Health_Expenditures_LN ~ Requests_Health_Major10_LN + 
  Population_LN + SOVI_Index_JB_EIGHT + BlauIndex_ZipAverage + 
  totamt_LN + hlthempl.md_LN +
  f(id,model="iid",param=c(2,1))
farts2=Charities_Arts_Expenditures_LN ~ Requests_Arts_Major10_LN + 
  Population_LN + SOVI_Index_JB_EIGHT + BlauIndex_ZipAverage + 
  totamt_LN + artempl.md_LN +
  f(id,model="iid",param=c(2,1))
restot2=inla(ftot2,data=dataNeiMed,
             #quantiles=c(0.005,0.025,0.05,.95,0.975,0.995),
             quantiles=c(0.025,0.975),
             control.compute = list(dic=TRUE,waic=TRUE))
summary(restot2)
reshlth2=inla(fhlth2,data=dataNeiMed,
              #quantiles=c(0.005,0.025,0.05,.95,0.975,0.995),
              quantiles=c(0.025,0.975),
              control.compute = list(dic=TRUE,waic=TRUE))
summary(reshlth2)
resarts2=inla(farts2,data=dataNeiMed,
              #quantiles=c(0.005,0.025,0.05,.95,0.975,0.995),
              quantiles=c(0.025,0.975),
              control.compute = list(dic=TRUE,waic=TRUE))
summary(resarts2)

fillmap2(NCZips2red,"",restot2$summary.random$id$mean,
         map.lty=0,leg.cex=1,leg.rnd = 2)
fillmap2(NCZips2red,"",reshlth2$summary.random$id$mean,
         map.lty=0,leg.cex=1,leg.rnd = 2)
fillmap2(NCZips2red,"",resarts2$summary.random$id$mean,
         map.lty=0,leg.cex=1,leg.rnd = 2)
#r2
fittot2 <- restot2$summary.fitted.values[1]
cor(fittot2, dataNeiMed$TotalPublicCharities_Expenditures_LN)^2
fithlth2 <- reshlth2$summary.fitted.values[1]
cor(fithlth2, dataNeiMed$Charities_Health_Expenditures_LN)^2
fitart2 <- resarts2$summary.fitted.values[1]
cor(fitart2, dataNeiMed$Charities_Arts_Expenditures_LN)^2


#correlated r.e. regression models
ftot3=TotalPublicCharities_Expenditures_LN~TotalRequests_LN+Population_LN+SOVI_Index_JB_EIGHT+BlauIndex_ZipAverage+totamt_LN+totempl.md_LN+
  f(id,model="bym", 
    graph="nczipred.adj",
    param=c(2,1))
fhlth3=Charities_Health_Expenditures_LN ~ Requests_Health_Major10_LN + 
  Population_LN + SOVI_Index_JB_EIGHT + BlauIndex_ZipAverage + 
  totamt_LN + hlthempl.md_LN +
  f(id,model="bym", 
    graph="nczipred.adj",
    param=c(2,1))
farts3=Charities_Arts_Expenditures_LN ~ Requests_Arts_Major10_LN + 
  Population_LN + SOVI_Index_JB_EIGHT + BlauIndex_ZipAverage + 
  totamt_LN + artempl.md_LN +
  f(id,model="bym", 
    graph="nczipred.adj",
    param=c(2,1))

restot3=inla(ftot3,data=dataNeiMed,
             quantiles=c(0.005,0.025,0.05,.95,0.975,0.995),
             control.compute = list(dic=TRUE,waic=TRUE))
summary(restot3)
reshlth3=inla(fhlth3,data=dataNeiMed,
              quantiles=c(0.005,0.025,0.05,.95,0.975,0.995),
              control.compute = list(dic=TRUE,waic=TRUE))
summary(reshlth3)
resarts3=inla(farts3,data=dataNeiMed,
              quantiles=c(0.005,0.025,0.05,.95,0.975,0.995),
              control.compute = list(dic=TRUE,waic=TRUE))
summary(resarts3)
#r2
fittot3 <- restot3$summary.fitted.values[1]
cor(fittot3, dataNeiMed$TotalPublicCharities_Expenditures_LN)^2
fithlth3 <- reshlth3$summary.fitted.values[1]
cor(fithlth3, dataNeiMed$Charities_Health_Expenditures_LN)^2
fitart3 <- resarts3$summary.fitted.values[1]
cor(fitart3, dataNeiMed$Charities_Arts_Expenditures_LN)^2

fillmap2(NCZips2red,"",restot3$summary.random$id$mean[1:770],
         map.lty=0,leg.cex=1,leg.rnd = 2)
fillmap2(NCZips2red,"",reshlth3$summary.random$id$mean[1:770],
         map.lty=0,leg.cex=1,leg.rnd = 3)
fillmap2(NCZips2red,"",resarts3$summary.random$id$mean[1:770],
         map.lty=0,leg.cex=1,leg.rnd = 3)





###test spat structuring
lw <- nb2listw(nc.nb.red, style="W",zero.policy=F)
moran.test(dataNeiMed$TotalPublicCharities_Expenditures_LN,lw)
moran.test(dataNeiMed$Charities_Health_Expenditures_LN,lw)
moran.test(dataNeiMed$Charities_Arts_Expenditures_LN,lw)
moran.test(dataNeiMed$TotalRequests_LN,lw)
moran.test(dataNeiMed$Requests_Health_Major10_LN,lw)
moran.test(dataNeiMed$Requests_Arts_Major10_LN,lw)
moran.test(dataNeiMed$Population_LN,lw)
moran.test(dataNeiMed$SOVI_Index_JB_EIGHT,lw)
moran.test(dataNeiMed$BlauIndex_ZipAverage,lw)
moran.test(dataNeiMed$totamt_LN,lw)
moran.test(dataNeiMed$totempl.md_LN,lw)
moran.test(dataNeiMed$hlthempl.md_LN,lw)
moran.test(dataNeiMed$artempl.md_LN,lw)


#cor mats
mtot=cor(cbind(dataNeiMed$TotalPublicCharities_Expenditures_LN,
               dataNeiMed$SOVI_Index_JB_EIGHT,
               dataNeiMed$TotalRequests_LN,
        dataNeiMed$totempl.md_LN,
        dataNeiMed$totamt_LN,
        dataNeiMed$BlauIndex_ZipAverage,
        dataNeiMed$Population_LN))
rownames(mtot)=colnames(mtot)=nms[c(1,8,4,11,10,9,7)]
mhlth=cor(cbind(dataNeiMed$Charities_Health_Expenditures_LN,
                dataNeiMed$SOVI_Index_JB_EIGHT,
                dataNeiMed$Requests_Health_Major10_LN,
                dataNeiMed$hlthempl.md_LN,
                dataNeiMed$totamt_LN,
                dataNeiMed$BlauIndex_ZipAverage,
               dataNeiMed$Population_LN))
rownames(mhlth)=colnames(mhlth)=nms[c(2,8,5,12,10,9,7)]
mart=cor(cbind(dataNeiMed$Charities_Arts_Expenditures_LN,
               dataNeiMed$SOVI_Index_JB_EIGHT,
               dataNeiMed$Requests_Arts_Major10_LN,
               dataNeiMed$artempl.md_LN,
               dataNeiMed$totamt_LN,
               dataNeiMed$BlauIndex_ZipAverage,
               dataNeiMed$Population_LN))
rownames(mart)=colnames(mart)=nms[c(3,8,6,13,10,9,7)]
par(mfrow=c(1,3))
corrplot(mtot,method = "color",
         type = "upper", number.cex = .7,
         addCoef.col = "black",
         tl.col = "black", tl.srt = 90,
         diag = FALSE)
corrplot(mhlth,method = "color",
         type = "upper", number.cex = .7,
         addCoef.col = "black",
         tl.col = "black", tl.srt = 90,
         diag = FALSE)
corrplot(mart,method = "color",
         type = "upper", number.cex = .7,
         addCoef.col = "black",
         tl.col = "black", tl.srt = 90,
         diag = FALSE)
resmtot <- cor.mtest(mtot, conf.level = .95)
resmhlth <- cor.mtest(mhlth, conf.level = .95)
resmart <- cor.mtest(mart, conf.level = .95)
par(mfrow=c(1,3))
corrplot(mtot,p.mat=resmtot$p,method = "color",
         type = "upper", 
         sig.level = c(.001, .01, .05), 
         pch.cex = .9,
         insig = "label_sig", 
         pch.col = "black",
         tl.col = "black", tl.srt = 90,
         diag = FALSE)
corrplot(mhlth,p.mat=resmhlth$p,method = "color",
         type = "upper", 
         sig.level = c(.001, .01, .05), 
         pch.cex = .9,
         insig = "label_sig", 
         pch.col = "black",
         tl.col = "black", tl.srt = 90,
         diag = FALSE)
corrplot(mart,p.mat=resmart$p,method = "color",
         type = "upper", 
         sig.level = c(.001, .01, .05), 
         pch.cex = .9,
         insig = "label_sig", 
         pch.col = "black",
         tl.col = "black", tl.srt = 90,
         diag = FALSE)


