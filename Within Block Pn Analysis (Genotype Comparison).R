# Within Block Pn Analysis (Genotype Comparison)

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

# Creating Block Data Frames 

P1B1<- MF1 %>% select( PN,GS,Geno,Time,Block,Ci) %>% filter(Block == "P1B1") 
P1B2<- MF1 %>% select( PN,GS,Geno,Time,Block,Ci) %>% filter(Block == "P1B2") 
P2B1<- MF1 %>% select( PN,GS,Geno,Time,Block,Ci) %>% filter(Block == "P2B1") 
P2B2<- MF1 %>% select( PN,GS,Geno,Time,Block,Ci) %>% filter(Block == "P2B2")
P3B1<- MF1 %>% select( PN,GS,Geno,Time,Block,Ci) %>% filter(Block == "P3B1") 
P3B2<- MF1 %>% select( PN,GS,Geno,Time,Block,Ci) %>% filter(Block == "P3B2")

summary(P1B1) #297 records- Morning  :128 ,  Midday   :169
summary(P1B2) #294 records- Morning  :116 ,  Midday   :178
summary(P2B1) #222 records- Morning  :144 ,  Midday   : 78  
summary(P2B2) #338 records- Morning  :181  , Midday   :103  
summary(P3B1) #321 records- Morning  :184 ,  Midday   :140
summary(P3B2) #316 records- Morning  :153  , Midday   :163 
#--------------------------------------------------------------------------------
# P1B1 PN LM
P1B1.lm = lm(PN~Geno+Time, data=P1B1) 
par(mfrow=c(2,2)); plot(P1B1.lm)
drop1(P1B1.lm,test="F")
P1B1tukey = emmeans(P1B1.lm,specs = "Geno")
P1B1Pair = contrast(P1B1tukey, method = "pairwise")
summary(P1B1Pair)                                    # A/R-A are similar, R is different
CLD(emmeans(P1B1.lm,~ Time))
CLD(emmeans(P1B1.lm,~ Geno))  

#P1B2 PN LM
P1B2.lm = lm(PN~Geno+Time, data=P1B2) 
par(mfrow=c(2,2)); plot(P1B2.lm)
drop1(P1B2.lm,test="F")
P1B2tukey = emmeans(P1B2.lm,specs = "Geno")
P1B2Pair = contrast(P1B2tukey, method = "pairwise")
summary(P1B2Pair)                                   # A/R is similar to both A & R 
CLD(emmeans(P1B2.lm,~ Time))                        # Morning scores better
CLD(emmeans(P1B2.lm,~ Geno))  

#P2B1 PN LM
P2B1.lm = lm(PN~Geno+Time, data=P2B1) 
par(mfrow=c(2,2)); plot(P2B1.lm) #Q-Q plot show not normality
drop1(P2B1.lm,test="F")
P2B1tukey = emmeans(P2B1.lm,specs = "Geno")
P2B1Pair = contrast(P2B1tukey, method = "pairwise")
summary(P2B1Pair)                    
CLD(emmeans(P2B1.lm,~ Geno)) # A/R is similar to both A & R 
CLD(emmeans(P2B1.lm,~ Time)) # Mid-day scores better

#P2B2 PN LM
P2B2.lm = lm(PN~Geno+Time, data=P2B2) 
par(mfrow=c(2,2)); plot(P2B2.lm) 
drop1(P2B2.lm,test="F")
P2B2tukey = emmeans(P2B2.lm,specs = "Geno")
P2B2Pair = contrast(P2B2tukey, method = "pairwise")
summary(P2B2Pair)                    
CLD(emmeans(P2B2.lm,~ Geno)) # R-A/R are similar, A different
CLD(emmeans(P2B2.lm,~ Time)) # Morning scores better

#P3B1 PN LM
P3B1.lm = lm(PN~Geno+Time, data=P3B1) 
par(mfrow=c(2,2)); plot(P3B1.lm) 
drop1(P3B1.lm,test="F")
P3B1tukey = emmeans(P3B1.lm,specs = "Geno")
P3B1Pair = contrast(P3B1tukey, method = "pairwise")
summary(P3B1Pair)    
CLD(emmeans(P3B1.lm,~ Geno)) # R-A/R are similar , A different 
CLD(emmeans(P3B1.lm,~ Time)) # Time not significant

#P3B2 PN LM
P3B2.lm = lm(PN~Geno+Time, data=P3B2) 
par(mfrow=c(2,2)); plot(P3B2.lm) 
drop1(P3B2.lm,test="F")
P3B2tukey = emmeans(P3B2.lm,specs = "Geno")
P3B2Pair = contrast(P3B2tukey, method = "pairwise")
summary(P3B2Pair)
CLD(emmeans(P3B2.lm,~ Geno)) # Geno not significant 
CLD(emmeans(P3B2.lm,~ Time)) # Time is not significant 

## A/R appears to be more similar in performance to R than A
## PN values are higher in Sun Blocks in Morning
## PN values are highr in Shade Blocks at Mid-Day

#------------------------------------------------------------------------------------
# P1B1 Stomatal Conductance LM
P1B1.Gslm = lm(GS~Geno+Time, data=P1B1) 
par(mfrow=c(2,2)); plot(P1B1.Gslm)
drop1(P1B1.Gslm,test="F")
CLD(emmeans(P1B1.Gslm,~ Geno))  # A/R & A are similar, R different higher
CLD(emmeans(P1B1.Gslm,~ Time))  # No Time significance 
summary(P1B1.Gslm)

#P1B2 Stomatal Conductance LM
P1B2.Gslm = lm(GS~Geno+Time, data=P1B2) 
par(mfrow=c(2,2)); plot(P1B2.Gslm)
drop1(P1B2.Gslm,test="F")
CLD(emmeans(P1B2.Gslm,~ Geno)) # Geno Not significant on Gs
CLD(emmeans(P1B2.Gslm,~ Time)) # Morning has Higher Gs

#P2B1 Stomatal Conductance LM
P2B1.Gslm = lm(GS~Geno+Time, data=P2B1) 
par(mfrow=c(2,2)); plot(P2B1.Gslm) 
drop1(P2B1.Gslm,test="F")
CLD(emmeans(P2B1.Gslm,~ Geno)) # A/R similar to both A & R
CLD(emmeans(P2B1.Gslm,~ Time)) # Morning has higher Gs

#P2B2 Stomatal Conductance LM
P2B2.Gslm = lm(GS~Geno+Time, data=P2B2) 
par(mfrow=c(2,2)); plot(P2B2.Gslm) 
drop1(P2B2.Gslm,test="F")
CLD(emmeans(P2B2.Gslm,~ Geno)) # A/R similar to both A & R
CLD(emmeans(P2B2.Gslm,~ Time)) # Morning Has higher Gs

#P3B1 Stomatal Conductance LM
P3B1.Gslm = lm(GS~Geno+Time, data=P3B1) 
par(mfrow=c(2,2)); plot(P3B1.Gslm) 
drop1(P3B1.Gslm,test="F")
CLD(emmeans(P3B1.Gslm,~ Geno)) # Not significant on GS
CLD(emmeans(P3B1.Gslm,~ Time)) # Not Significant on GS

#P3B2 Stomatal Conductance LM
P3B2.Gslm = lm(GS~Geno+Time, data=P3B2) 
par(mfrow=c(2,2)); plot(P3B2.Gslm) 
drop1(P3B2.Gslm,test="F")
CLD(emmeans(P3B2.Gslm,~ Geno)) # Not significant on GS
CLD(emmeans(P3B2.Gslm,~ Time)) # Mid-day gave higher GS

#
#
#
#--------------------------------------------------------------------------------------------
# Tables binding Linear model partner results (Bock to corresponding Block)
library(arm)
c1 = coef(r2)
c_qmle=coef(r1)
s1=se.coef(r2)
s_qmle=se.coef(r1)
cbind(c1,s1,c_qmle,s_qmle)

P1B1.lm = lm(PN~Geno+Time, data=P1B1) 
par(mfrow=c(2,2)); plot(P1B1.lm)
drop1(P1B1.lm,test="F")
CLD(emmeans(P1B1.lm,~ Geno))  # A/R-A are similar, R is different
CLD(emmeans(P1B1.lm,~ Time))  # Mid-day scores better 
summary(P1B1.lm)

P1B1coef= coef(P1B1.lm)
P1B2coef= coef(P1B2.lm)
P1B1se.coef= se.coef(P1B1.lm)
P1B2se.coef= se.coef(P1B2.lm)
cbind(P1B1coef,P1B1se.coef,P1B2coef,P1B2se.coef)



#-----------------------------------------------------------------------------------------
#   PN vs GS scatter plots (I.e-  The Two Dependant Response Variables annotated with 
#   Independant variables )

ggplot(P1B1, aes(x=GS, y=PN, shape=Time, color=Geno)) +
  geom_point(size=2) +
  labs(title="P1B1 Gs vs. PN",
       x="Gs - unit", y = "PN - unit" )+ geom_smooth(method = "lm",se= FALSE)

ggplot(P1B2, aes(x=GS, y=PN, shape=Time, color=Geno)) +
  geom_point(size=2) +
  labs(title="P1B2 Gs vs. PN",
       x="Gs - unit", y = "PN - unit")

ggplot(P2B1, aes(x=GS, y=PN, shape=Time, color=Geno)) +
  geom_point(size=2) +
  labs(title="P2B1 Gs vs. PN",
       x="Gs - unit", y = "PN - unit")+ geom_smooth(method = "lm",se= FALSE)

ggplot(P2B2, aes(x=GS, y=PN, shape=Time, color=Geno)) +
  geom_point(size=2) +
  labs(title="P2B2 Gs vs. PN",
       x="Gs - unit", y = "PN - unit")

ggplot(P3B1, aes(x=GS, y=PN, shape=Time, color=Geno)) +
  geom_point(size=2) +
  labs(title="P3B1 Gs vs. PN",
       x="Gs - unit", y = "PN - unit")

ggplot(P3B2, aes(x=GS, y=PN, shape=Time, color=Geno)) +
  geom_point(size=2) +
  labs(title="P3B2 Gs vs. PN",
       x="Gs - unit", y = "PN - unit")
