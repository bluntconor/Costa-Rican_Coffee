# Between Block Pn analysis (genotypes between Blocks Comparison AFS)
library(readxl)
MF1 <- read_excel("C:/Users/blunt/Google Drive/BREEDACFSs thesis/Photosythetic study/Simitaineous Ciras sampling/MF1.xlsx")
View(MF1)

# SET FACTORS
MF1$Block <- factor(MF1$Block)
MF1$Geno <- factor(MF1$Geno)
MF1$Time<-as.numeric(MF1$Time)
MF1$Parcel <- factor(MF1$Parcel)

#adding levels to P1MR PAR (Light Radiation)
MF1$PAR<-cut(MF1$PAR, breaks = c(0,500,1000,3000), labels=c("low IR","medium IR","high IR"))

#adding levels to P1MR Time ##### NOT WORKING #####
MF1$Time<-cut(MF1$Time, breaks = c(700,1000,1300,1700), labels=c("Morning","Midday","Afternoon"))


str(MF1)

summary(MF1)

# Creating 2 Block Dataframes 
P1B1B2<- MF1 %>% select( PN,GS,Geno,Time,Block,Ci,Parcel) %>% filter(Parcel == "1")
P2B1B2<- MF1 %>% select( PN,GS,Geno,Time,Block,Ci,Parcel) %>% filter(Parcel == "2")
P3B1B2<- MF1 %>% select( PN,GS,Geno,Time,Block,Ci,Parcel) %>% filter(Parcel == "3")
#
str(P1B1B2)
View(P1B1B2)
#-----------------------------------------------------------------------------------------------
# below Linear models comapre All PN rates of all genos together against correspondong block
# Comparison of Blocks at Parcel 1 
P1B1B2.lm = lm(PN~Geno+Block, data=P1B1B2) 
par(mfrow=c(2,2)); plot(P1B1B2.lm)
drop1(P1B1B2.lm,test="F")
P1B1B2tukey = emmeans(P1B1B2.lm,specs = "Block")
P1B1B2Pair = contrast(P1B1B2tukey, method = "pairwise")
summary(P1B1B2Pair)                                    # No Significant difference between Block PN
CLD(emmeans(P1B1B2.lm,~ Block))
CLD(emmeans(P1B1B2.lm,~ Geno))                           # # A/R-A are similar, R is different

P2B1B2.lm = lm(PN~Geno+Block, data=P2B1B2) 
par(mfrow=c(2,2)); plot(P2B1B2.lm)
drop1(P2B1B2.lm,test="F")
P2B1B2tukey = emmeans(P2B1B2.lm,specs = "Block")
P2B1B2Pair = contrast(P2B1B2tukey, method = "pairwise")
summary(P2B1B2Pair)                                    # No Significant difference between Block PN
CLD(emmeans(P2B1B2.lm,~ Time))
CLD(emmeans(P2B1B2.lm,~ Geno)) 

P3B1B2.lm = lm(PN~Geno+Block, data=P3B1B2) 
par(mfrow=c(2,2)); plot(P3B1B2.lm)
drop1(P3B1B2.lm,test="F")
P3B1B2tukey = emmeans(P3B1B2.lm,specs = "Block")
P3B1B2Pair = contrast(P3B1B2tukey, method = "pairwise")
summary(P3B1B2Pair)                                   
CLD(emmeans(P3B1B2.lm,~ Geno))
CLD(emmeans(P3B1B2.lm,~ Block))                      # significant difference in PN between blocks

#------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------
# Creating an  (A/R) PN data Frame to test Hypothesis;
#The advantageous physiological effects of grafting conilon rootstock to villa-sarchi will 
#countervail shading attributes and materialise within full sun cultivation and lower altitudes. 

#Below Dataframes also depict comparisons between same geno in two blocks

AR.PN<- MF1 %>% select( PN,Geno,Time,Block,Ci) %>% filter(Geno == "A/R") 
# lm on AR-PN
PN.lmAR = lm(PN~ Block, data=AR.PN) 
par(mfrow=c(2,2)); plot(PN.lmAR)
drop1(PN.lmAR,test="F")
AR.PNtukey = emmeans(PN.lmAR,specs = "Block")
AR.PNPair = contrast(AR.PNtukey, method = "pairwise")
summary(AR.PNPair)   
emmeans(PN.lmAR,~Block)
CLD(emmeans(PN.lmAR,~ Block))
#above model proves hypothesis-> performace directly correlates with altitude; decreasig with height.
#lm on AR-Ci

Ci.lmAR = lm(Ci~ Block, data=AR.PN) 
par(mfrow=c(2,2)); plot(Ci.lmAR)
drop1(PN.lmAR,test="F")
emmeans(Ci.lmAR,~Block)
CLD(emmeans(Ci.lmAR,~ Block))

#Creating an (Arabica) data frame to see if the hypothesis also fits this Geno. this tests if grafting helped arabica

A.PN<- MF1 %>% select( PN,Geno,Time,Block,Ci) %>% filter(Geno == "A") 
# lm on A-PN
PN.lmA = lm(PN~ Block, data=A.PN) 
par(mfrow=c(2,2)); plot(PN.lmA)
drop1(PN.lmA,test="F")
A.PNtukey = emmeans(PN.lmA,specs = "Block")
A.PNPair = contrast(A.PNtukey, method = "pairwise")
summary(A.PNPair)   
emmeans(PN.lmA,~Block)
CLD(emmeans(PN.lmA,~ Block))
#above model shows a similar result to A/R but less susecptable to altitude 


#Creating an (Robusta) data frame to see if the hypothesis also fits this Geno. this tests if grafting helped arabica

R.PN<- MF1 %>% select( PN,Geno,Time,Block,Ci) %>% filter(Geno == "R") 
# lm on A-PN
PN.lmR = lm(PN~ Block, data=R.PN) 
par(mfrow=c(2,2)); plot(PN.lmR)
drop1(PN.lmR,test="F")
R.PNtukey = emmeans(PN.lmR,specs = "Block")
R.PNPair = contrast(R.PNtukey, method = "pairwise")
summary(R.PNPair)   
emmeans(PN.lmR,~Block)
CLD(emmeans(PN.lmR,~ Block))
# 5 groups present in this test. interesting that A/R is a slight blend of A and R results 

