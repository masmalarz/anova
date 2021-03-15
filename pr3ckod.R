poker <- read.csv("anova_poker.csv", sep=";")
poker
#-----------------------------------------------------------------
#Standaryzowanie wartoœci
result_st<-scale(poker$result)
result_st
danep<-data.frame(poker, result_st)
danep[251:350,]
#-----------------------------------------------------------------
#Statystyki
tapply(poker$result,poker[,1:2],length)
tapply(poker$result,poker[,1:2],mean)
tapply(poker$result,poker[,1:2],sd)
summary(poker)
library(psych)
describeBy(poker$result, group = poker$hand, mat = TRUE, digits = 2)
describeBy(poker$result, group = poker$skill, mat = TRUE, digits = 2)
#-----------------------------------------------------------------
#Outliery
boxplot(poker$result)
library(outliers)
outliers1 <- tapply(poker$result,poker$hand,grubbs.test)
outliers1
outliers2 <- tapply(poker$result,poker$skill,grubbs.test)
outliers2
#-----------------------------------------------------------------
#Sprawdzenie za³o¿eñ ANOVA
#Testy normalnoœci
tapply(poker$result,poker$skill,shapiro.test)
tapply(poker$result,poker$hand,shapiro.test)

#aby wyœwietliæ same p-value
shapiroskill<-tapply(poker$result,poker$skill,shapiro.test) 
shapirohand<-tapply(poker$result,poker$hand,shapiro.test)
shapirointer<-tapply(poker$result,poker[,1:2],shapiro.test) 
lol<-do.call(rbind,lapply(shapiroskill,function(v){v$p.value}))
lol2<-do.call(rbind,lapply(shapirohand,function(v){v$p.value}))
lol3<-do.call(rbind,lapply(shapirointer,function(v){v$p.value}))
colnames(lol2)<-colnames(lol)<-colnames(lol3)<-c("p-value")
list<-list(skill = lol, hand=lol2,interakcja = lol3)
list

library(nortest)
sfskill<-tapply(poker$result,poker$skill,sf.test)
sfhand<-tapply(poker$result,poker$hand,sf.test)
sfinter<-tapply(poker$result,poker[,1:2],sf.test)
sfcall<-do.call(rbind,lapply(sfskill,function(v){v$p.value}))
sfcall2<-do.call(rbind,lapply(sfhand,function(v){v$p.value}))
sfcall3<-do.call(rbind,lapply(sfinter,function(v){v$p.value}))
colnames(sfcall)<-colnames(sfcall2)<-colnames(sfcall3)<-c("p-value")
listsf<-list(skill = sfcall, hand=sfcall2,interakcja=sfcall3)
listsf

adskill<-tapply(poker$result,poker$skill,ad.test)
adhand<-tapply(poker$result,poker$hand,ad.test)
adinter<-tapply(poker$result,poker[,1:2],ad.test)
adcall<-do.call(rbind,lapply(adskill,function(v){v$p.value}))
adcall2<-do.call(rbind,lapply(adhand,function(v){v$p.value}))
adcall3<-do.call(rbind,lapply(adinter,function(v){v$p.value}))
colnames(adcall)<-colnames(adcall2)<-colnames(adcall3)<-c("p-value")
listad<-list(skill = adcall, hand=adcall2,interakcja=adcall3)
listad
#-----------------------------------------------------------------
#Testy jednorodnoœci wariancji
#Levene test
library(car)
levskill<-leveneTest(result~skill, data = poker, center=mean)
levhand<-leveneTest(result~hand, data = poker, center=mean)
levinter<-leveneTest(result~hand*skill, data = poker, center=mean)
listlev<-list(skill=levskill$`Pr(>F)`[1],hand=levhand$`Pr(>F)`[1],interakcja=levinter$`Pr(>F)`[1])
listlev

#Browna-Fikogoœtam
levskill2<-leveneTest(result~skill, data = poker, center=median)
levhand2<-leveneTest(result~hand, data = poker, center=median)
levinter2<-leveneTest(result~hand*skill, data = poker, center=median)
listlev2<-list(skill=levskill2$`Pr(>F)`[1],hand=levhand2$`Pr(>F)`[1],interakcja=levinter2$`Pr(>F)`[1])
listlev2

#library(onewaytests)
#bf.test(result ~ hand, data = poker)
#bf.test(result ~ skill, data = poker)

#fligner.test(result ~ hand, data = poker)
#fligner.test(result ~ skill, data = poker)

#Test Bartletta
#bartlett.test(poker$result, poker$skill)
#bartlett.test(poker$result, poker$hand)

#-----------------------------------------------------------------
#ANOVA
poker.aov <- aov(result~hand*skill, data = poker)
summary(poker.aov)
#-----------------------------------------------------------------
#Kruskal
kruskal.test(result ~ hand, data = poker)
kruskal.test(result ~ skill, data = poker)
#-----------------------------------------------------------------
#Analiza post hoc
library(FSA)
dunnTest(result~hand, data=poker)

TukeyHSD(x=poker.aov)

install.packages("DescTools")
library (DescTools)
PostHocTest(poker.aov,method = "duncan")
PostHocTest(poker.aov,method = "scheffe")

pairwise.t.test(poker$result,poker$hand,p.adjust.method = "none")
pairwise.t.test(poker$result,poker$skill,p.adjust.method = "none")
#-----------------------------------------------------------------
#Efekty eksperymentalne
SStotal=(2647+49+219+5278)
eta.hand=(2647/(2647+5278))
eta.skill=(49/(49+5278))
eta.sk_ha=(219/(219+5278))
omega.hand=(2647-2*18.0)/(18.0+SStotal)
omega.skill=(49-1*18.0)/(18.0+SStotal)
omega.sk_ha=(219-2*18.0)/(18.0+SStotal)
ety<-c(eta.hand,eta.skill,eta.sk_ha)
ety
omegi<-c(omega.hand,omega.skill,omega.sk_ha)
omegi
efekty<-data.frame(ety,omegi)
colnames(efekty)<-c("eta-kwadrat","omega")
rownames(efekty)<-c("hand","skill","hand*skill")
efekty
