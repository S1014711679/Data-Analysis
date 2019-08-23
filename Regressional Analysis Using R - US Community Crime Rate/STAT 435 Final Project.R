communcrime<-read.table("communcrime1.dat",header=T)


nrow(communcrime)
head(communcrime)
hist(communcrime$ViolentCrimesPerPop)

library("car")
library("MASS")

ViolentCrimesPerPop<-communcrime$ViolentCrimesPerPop
ViolentCrimesPerPop

#1 state
state<-communcrime$state
sort(as.factor(state))
summary(aov(ViolentCrimesPerPop~as.factor(state)))
summary(lm(ViolentCrimesPerPop~as.factor(state)))
par(mfrow=c(2,3))
plot(lm(ViolentCrimesPerPop~as.factor(state)),1:6)
plot(ViolentCrimesPerPop~as.factor(state))
#2 community name
communityname<-communcrime$communityname
communityname
sort(communityname)
summary(aov(ViolentCrimesPerPop~as.factor(communityname)))

#3 racepctblack
racepctblack<- communcrime$racepctblack
racepctblack
racepctHisp<-communcrime$racePctHisp
racepctHisp5

##############################  black   #############################
summary(lm(ViolentCrimesPerPop~racepctblack))
plot(lm(ViolentCrimesPerPop~racepctblack))

mean()
sd()
median()

mean(racepctblack)
sd(racepctblack)
median(racepctblack)
plot(racepctblack)

summary(p1<-powerTransform(ViolentCrimesPerPop~racepctblack),data=communcrime)
p1$lambda
summary(lm(basicPower(ViolentCrimesPerPop,p1$lambda)~racepctblack,data=communcrime))
summary(p2<-powerTransform(ViolentCrimesPerPop~racepctHisp),data=communcrime)
p2$lambda

##############################  spanish ##############################
summary(lm(ViolentCrimesPerPop~racepctHisp))

mean(racepctHisp)
sd(racepctHisp)
median(racepctHisp)


summary(lm(basicPower(ViolentCrimesPerPop,p2$lambda)~racepctHisp,data=communcrime))
summary(lm(ViolentCrimesPerPop~racepctHisp+racepctblack))

#############################  black + spanish ########################
summary(p3<-powerTransform(ViolentCrimesPerPop~+racepctHisp+racepctblack),data=communcrime)

summary(lm(basicPower(ViolentCrimesPerPop,p3$lambda)~racepctHisp+racepctblack),data=communcrime)

boxTidwell(ViolentCrimesPerPop~racepctHisp+racepctblack,data=communcrime)
p3$lambda


########################### age ####################################### 
#age 12-21
agePct12t21<-communcrime$agePct12t21
summary(lm(ViolentCrimesPerPop~agePct12t21))
mean(agePct12t21)
sd(agePct12t21)
median(agePct12t21)


#pctUrban
pctUrban<-communcrime$pctUrban
pctUrban
summary(lm(ViolentCrimesPerPop~pctUrban))

mean(pctUrban)
sd(pctUrban)
median(pctUrban)

p4<-powerTransform(ViolentCrimesPerPop~pctUrban)
p4$lambda

#summary(lm(ViolentCrimesPerPop^p4 ~ pctUrban,data=ViolentCrimesPerPop))


###########income ####################
#median income
medIncome<-communcrime$medIncome
pctWPubAsst<-communcrime$pctWPubAsst
PctPopUnderPov<-communcrime$PctPopUnderPov
PctUnemployed<-communcrime$PctUnemployed

mean(medIncome)
sd(medIncome)
median(medIncome)

summary(pctWPubAsst)

mean(pctWPubAsst)
sd(pctWPubAsst)
median(pctWPubAsst)

mean(PctPopUnderPov)
sd(PctPopUnderPov)
median(PctPopUnderPov)


mean(PctUnemployed)
sd(PctUnemployed)
median(PctUnemployed)



summary(lm(ViolentCrimesPerPop~medIncome))
summary(lm(ViolentCrimesPerPop~pctWPubAsst))
summary(lm(ViolentCrimesPerPop~PctPopUnderPov))
summary(lm(ViolentCrimesPerPop~PctUnemployed))

############ family status ################
TotalPctDiv<-communcrime$TotalPctDiv
PctKids2Par<-communcrime$PctKids2Par

mean(TotalPctDiv)
sd(TotalPctDiv)
median(TotalPctDiv)

mean(PctKids2Par)
sd(PctKids2Par)
median(PctKids2Par)

summary(lm(ViolentCrimesPerPop~TotalPctDiv))
summary(lm(ViolentCrimesPerPop~PctKids2Par))

############# immigration ############################
PctImmigRec5<-communcrime$PctImmigRec5
PctImmigRec10<-communcrime$PctImmigRec10
PctRecImmig5<-communcrime$PctRecImmig5
PctRecImmig10<-communcrime$PctRecImmig10
PctNotSpeakEnglWell<-communcrime$PctNotSpeakEnglWell

summary(lm(ViolentCrimesPerPop~PctImmigRec5))
summary(lm(ViolentCrimesPerPop~PctImmigRec10))
summary(lm(ViolentCrimesPerPop~PctRecImmig5))
summary(lm(ViolentCrimesPerPop~PctRecImmig10))
summary(lm(ViolentCrimesPerPop~PctNotSpeakEnglWell))



mean(PctImmigRec5)
sd(PctImmigRec5)
median(PctImmigRec5)

mean(PctImmigRec10)
sd(PctImmigRec10)
median(PctImmigRec10)

mean(PctRecImmig5)
sd(PctRecImmig5)
median(PctRecImmig5)

mean(PctRecImmig10)
sd(PctRecImmig10)
median(PctRecImmig10)

mean(PctNotSpeakEnglWell)
sd(PctNotSpeakEnglWell)
median(PctNotSpeakEnglWell)


plot(lm(ViolentCrimesPerPop~PctNotSpeakEnglWell))
immigration<-lm(ViolentCrimesPerPop~PctRecImmig5+PctRecImmig10+PctImmigRec5+PctImmigRec10+PctNotSpeakEnglWell+PctForeignBorn)
summary(immigration)

vif(immigration)


NumInShelters<-communcrime$NumInShelters
PctForeignBorn<-communcrime$PctForeignBorn
PctUsePubTrans<-communcrime$PctUsePubTrans

summary(lm(ViolentCrimesPerPop~NumInShelters))
summary(lm(ViolentCrimesPerPop~PctForeignBorn))
summary(lm(ViolentCrimesPerPop~PctUsePubTrans))

mean(NumInShelters)
sd(NumInShelters)
median(NumInShelters)

mean(PctForeignBorn)
sd(PctForeignBorn)
median(PctForeignBorn)

mean(PctUsePubTrans)
sd(PctUsePubTrans)
median(PctUsePubTrans)



############################police#############################

LemasSwornFT<-communcrime$LemasSwornFT
LemasTotalReq<-communcrime$LemasTotalReq

PolicReqPerOffic<-communcrime$PolicReqPerOffic
OfficAssgnDrugUnits<-communcrime$OfficAssgnDrugUnits
NumKindsDrugsSeiz<-communcrime$NumKindsDrugsSeiz
PolicAveOTWorked<-communcrime$PolicAveOTWorked
LemasGangUnitDeploy<-communcrime$LemasGangUnitDeploy
LemasPctOfficDrugUn<-communcrime$LemasPctOfficDrugUn

summary(lm(ViolentCrimesPerPop~LemasSwornFT))
summary(lm(ViolentCrimesPerPop~LemasTotalReq))

summary(lm(ViolentCrimesPerPop~PolicReqPerOffic))
summary(lm(ViolentCrimesPerPop~OfficAssgnDrugUnits))
summary(lm(ViolentCrimesPerPop~NumKindsDrugsSeiz))
summary(lm(ViolentCrimesPerPop~PolicAveOTWorked))
summary(lm(ViolentCrimesPerPop~LemasGangUnitDeploy))
summary(lm(ViolentCrimesPerPop~LemasPctOfficDrugUn))


mean(LemasSwornFT)
sd(LemasSwornFT)
median(LemasSwornFT)

mean(LemasTotalReq)
sd(LemasTotalReq)
median(LemasTotalReq)

mean(PolicReqPerOffic)
sd(PolicReqPerOffic)
median(PolicReqPerOffic)


mean(OfficAssgnDrugUnits)
sd(OfficAssgnDrugUnits)
median(OfficAssgnDrugUnits)


mean(NumKindsDrugsSeiz)
sd(NumKindsDrugsSeiz)
median(NumKindsDrugsSeiz)

mean(PolicAveOTWorked)
sd(PolicAveOTWorked)
median(PolicAveOTWorked)


mean(LemasGangUnitDeploy)
sd(LemasGangUnitDeploy)
median(LemasGangUnitDeploy)


mean(LemasPctOfficDrugUn)
sd(LemasPctOfficDrugUn)
median(LemasPctOfficDrugUn)


#################################the full model##############################3

full<-lm(ViolentCrimesPerPop ~ 
           racepctblack+racepctHisp
           +agePct12t21+pctUrban
           +medIncome+pctWPubAsst
           +PctPopUnderPov+PctUnemployed
           +TotalPctDiv+PctKids2Par
           +PctImmigRec5+PctImmigRec10
           +PctRecImmig5+PctRecImmig10
           +PctNotSpeakEnglWell+NumInShelters
           +PctForeignBorn+PctUsePubTrans
           +LemasSwornFT+LemasTotalReq
           +PolicReqPerOffic+OfficAssgnDrugUnits
           +NumKindsDrugsSeiz+PolicAveOTWorked
           +LemasGangUnitDeploy+LemasPctOfficDrugUn)

summary(full)
vif(full)
AIC(full)

modifiedfull<-lm(ViolentCrimesPerPop~
                  racepctblack
                 +agePct12t21
                 +PctKids2Par
                 +NumInShelters
                 +PctForeignBorn
                 +LemasTotalReq
                 +PolicReqPerOffic)

summary(modifiedfull)
vif(modifiedfull)
AIC(modifiedfull)


stepwise<-lm(formula = ViolentCrimesPerPop ~ racepctblack + racepctHisp + 
     agePct12t21 + pctWPubAsst + PctUnemployed + PctKids2Par + 
     PctRecImmig10 + NumInShelters + PctForeignBorn + LemasSwornFT + 
     LemasTotalReq + PolicReqPerOffic + OfficAssgnDrugUnits)

summary(stepwise)
vif(stepwise)

step(full,direction="both")
vif(stepwise)
AIC(stepwise)
step(lm(ViolentCrimesPerPop~
             racepctblack + racepctHisp
           +agePct12t21 + pctUrban + medIncome + pctWPubAsst
           +PctPopUnderPov + PctUnemployed
           +TotalPctDiv + PctKids2Par + PctImmigRec5
           +PctImmigRec10 + PctRecImmig5 + PctRecImmig10
           +PctNotSpeakEnglWell + NumInShelters + PctForeignBorn + PctUsePubTrans
           +LemasSwornFT + LemasTotalReq + PolicReqPerOffic
           +OfficAssgnDrugUnits + NumKindsDrugsSeiz
           +PolicAveOTWorked + LemasGangUnitDeploy + LemasPctOfficDrugUn),direction = "both")
vif(full)
step(stepwise,direction="both")

############################################# final model #######################
######### the step wise model
final1<-lm(formula = ViolentCrimesPerPop ~ 
                 racepctblack 
               #+ racepctHisp 
                + agePct12t21 
                #+ pctWPubAsst 
                + PctUnemployed 
                + PctKids2Par 
                #+ PctRecImmig10 
                #+ NumInShelters 
                + PctForeignBorn 
                #+ LemasSwornFT 
                + LemasTotalReq 
                + PolicReqPerOffic 
                #+ OfficAssgnDrugUnits
                )
summary(final1)

vif(final1)
AIC(final1)
BIC(final1)

final<-lm(formula = ViolentCrimesPerPop ~ 
             racepctblack 
           + agePct12t21 
           + PctUnemployed 
           + PctKids2Par 
           + PctForeignBorn 
           + LemasTotalReq 
           + PolicReqPerOffic 
)

summary(final)
AIC(final)
BIC(final)



summary(power<-powerTransform(final))

power$lambda

final2<-lm(ViolentCrimesPerPop^(power$lambda) ~ 
        racepctblack 
      + agePct12t21 
      + PctUnemployed 
      + PctKids2Par 
      + PctForeignBorn 
      + LemasTotalReq 
      + PolicReqPerOffic 
)

summary(final2)
AIC(final2)
BIC(final2)

par(mfrow=c(2,3))
plot(final2,1:6)

plot(final2,1:6)



