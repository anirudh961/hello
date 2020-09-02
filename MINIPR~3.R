library(readxl)
T<-read_excel("Eye.xls")
attach(T)
library(xtable)
length(names(T))
table(GENDER)
str(T) 
TF<-T[GENDER=="0",]
TF
TM<-T[GENDER=="1",]
TM
length(TF$SPHEQ)
summary(TF[,2:6])
sqrt(var(TF$SPHEQ)) #sd of SPHEQ for female
sqrt(var(TF$AL))    #sd of AL for female
sqrt(var(TF$ACD))   #sd of ACD for female
sqrt(var(TF$LT))    #sd of LT for female
sqrt(var(TF$VCD))   #sd of VCD for female
length(TM$SPHEQ)    # length of male
summary(TM[,2:6])
sqrt(var(TM$SPHEQ)) #sd of SPHEQ for Male
sqrt(var(TM$AL))    #sd of AL for Male
sqrt(var(TM$ACD))   #sd of ACD for Male

sqrt(var(TM$LT))    #sd of LT for Male
sqrt(var(TM$VCD))   #sd of VCD for Male


par(mfrow=c(2,3))
boxplot(SPHEQ~GENDER,data=T,xlab="Gender",ylab="Spherical Equivalent Refraction",names=c("Female","Male"))
boxplot(AL~GENDER,data=T,xlab="Gender",ylab="Axial Length",names=c("Female","Male"))
boxplot(ACD~GENDER,data=T,xlab="Gender",ylab="Anterior Chamber Depth",names=c("Female","Male"))
boxplot(LT~GENDER,data=T,xlab="Gender",ylab="Lens Thickness",names=c("Female","Male"))
boxplot(VCD~GENDER,data=T,xlab="Gender",ylab="Vitreous Chamber Depth",names=c("Female","Male"))
par(mfrow=c(1,1))


qqnorm(SPHEQ)
qqnorm(sqrt(abs(SPHEQ)))
qqline(sqrt(abs(SPHEQ)))
hist(SPHEQ)
qqnorm(AL)
qqline(AL)
qqnorm(ACD)
qqline(ACD)
qqnorm(LT)
qqline(LT)
qqnorm(VCD)
qqline(VCD)
bartlett.test(SPHEQ~GENDER,data=T)
bartlett.test(AL~GENDER,data=T)
bartlett.test(ACD~GENDER,data=T)
bartlett.test(LT~GENDER,data=T)
bartlett.test(VCD~GENDER,data=T)

anova(aov(SPHEQ~GENDER,data=T))
anova(aov(AL~GENDER,data=T))
anova(aov(ACD~GENDER,data=T))
anova(aov(LT~GENDER,data=T))
anova(aov(VCD~GENDER,data=T))
