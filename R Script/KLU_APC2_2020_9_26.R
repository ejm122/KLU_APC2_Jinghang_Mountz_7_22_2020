rm(list=ls())
dev.off()
pacman::p_load(pacman, rio)
library(ggplot2)


## Ellie Analysis ########################################################################################################################

# IMPORTING Data ###########################################################
data <- import("~/Desktop/GPN/KLU_APC2_Master_2020_09_18.xlsx")
# Filtering Data ##################################################################
data <- data[is.na(data$FaceNames_Exclude),] #Issues with face name data and only 1 scan/subject - 87 observations
data <- data[data$Visit_Relative == 1,] # Comment out for longitudinal studies
data <- data[!is.na(data$FaceNames_GoodCoverage),]
data$PiB_Median_Split <- NA
PiB_Median = median(data$PiB_SUVR_GTM_FS_Global, na.rm = 'True');

# PiB Median Split ###############################################################
data$PiB_Median_Split[which(data$PiB_SUVR_GTM_FS_Global > PiB_Median)] <- "high_PiB"
data$PiB_Median_Split[which(data$PiB_SUVR_GTM_FS_Global <= PiB_Median)] <- "low_PiB"

#Independent T Test ###############################################################
#Two sided = mean difference can be less than/greater than 0
#Variances are assumed to NOT be equal (false)
p <- c(0,0,0,0,0,0)
a <- t.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
p[1]<- a$p.value
t.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)

b <- t.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
p[2] <- b$p.value
t.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)

c <- t.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
p[3] <- c$p.value
t.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)

d<- t.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
p[4] <- d$p.value
t.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)

e<-t.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
p[5] <- e$p.value
t.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)

f<- t.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
p[6] <- f$p.value
t.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)

lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

#p adjustments
p_adjusted <- p.adjust(p,method = "fdr",n=6)


#Asymmetry and Sex*Age
data$Sex[data$Sex == "NaN"] <- NA
p <- c(0,0,0,0,0,0)
p[1]<- lmp(lm(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR ~ data$Sex*data$Age_CurrentVisit))
summary(lm(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR ~ data$Sex*data$Age_CurrentVisit))

p[2]<- lmp(lm(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR~ data$Sex*data$Age_CurrentVisit))
summary(lm(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR~ data$Sex*data$Age_CurrentVisit))

p[3]<- lmp(lm(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR~ data$Sex*data$Age_CurrentVisit))
summary(lm(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR~ data$Sex*data$Age_CurrentVisit))

p[4]<- lmp(lm(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR~ data$Sex*data$Age_CurrentVisit))
summary(lm(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR~ data$Sex*data$Age_CurrentVisit))

p[5]<- lmp(lm(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR~ data$Sex*data$Age_CurrentVisit))
summary(lm(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR~ data$Sex*data$Age_CurrentVisit))

p[6]<- lmp(lm(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR~ data$Sex*data$Age_CurrentVisit))
summary(lm(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR~ data$Sex*data$Age_CurrentVisit))

p.adjust(p, method = "fdr", n=length(p))

#Asymmetry and Age*Education
data$Sex[data$Sex == "NaN"] <- NA
summary(lm(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR ~ data$Education*data$Age_CurrentVisit))
summary(lm(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR ~ data$Education*data$Age_CurrentVisit))

summary(lm(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR~ data$Education*data$Age_CurrentVisit))
summary(lm(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR~ data$Education*data$Age_CurrentVisit))

summary(lm(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR~ data$Education*data$Age_CurrentVisit))
summary(lm(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR~ data$Education*data$Age_CurrentVisit))

summary(lm(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR~ data$Education*data$Age_CurrentVisit))
summary(lm(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR~ data$Education*data$Age_CurrentVisit))

summary(lm(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR~ data$Education*data$Age_CurrentVisit))
summary(lm(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR~ data$Education*data$Age_CurrentVisit))

summary(lm(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR~ data$Education*data$Age_CurrentVisit))
summary(lm(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR~ data$Education*data$Age_CurrentVisit))

#p adjustments
asy_age_p <- c(frontal_med_orb_asy_age$p.value, frontal_mid_asy_age$p.value, frontal_sup_medial_asy_age$p.value,putamen_asy_age$p.value,supp_motor_area_asy_age$p.value,thal_VPL_asy_age$p.value)
absasy_age_p <- c(frontal_med_orb_absasy_age$p.vaue, frontal_mid_absasy_age$p.value, frontal_sup_medial_absasy_age$p.value,putamen_absasy_age$p.value,supp_motor_area_absasy_age$p.value,thal_VPL_absasy_age$p.value)
p.adjust(asy_age_p,method = "fdr",n=length(asy_age_p))
p.adjust(absasy_age_p,method = "fdr",n=length(absasy_age_p))

#Visualize Data ###################################################################
boxplot(FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR ~ PiB_Median_Split, data=data)
boxplot(FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR ~ PiB_Median_Split, data=data)

boxplot(FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR ~ PiB_Median_Split, data=data)
boxplot(FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR ~ PiB_Median_Split, data=data)

boxplot(FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR ~ PiB_Median_Split, data = data)
boxplot(FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR ~ PiB_Median_Split, data = data)

boxplot(FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR ~ PiB_Median_Split, data = data)
boxplot(FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR ~ PiB_Median_Split, data = data)

boxplot(FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR ~ PiB_Median_Split, data = data)
boxplot(FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR ~ PiB_Median_Split, data = data)

boxplot(FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR ~ PiB_Median_Split, data = data)
boxplot(FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR ~ PiB_Median_Split, data = data)

#Asymmetry and Task Performance
library("irr")
data$FaceName_PostScanAccuracy[data$FaceName_PostScanAccuracy == "NA"] <- NA

plot(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
frontal_med_orb_asy_accuracy <-cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
frontal_med_orb_absasy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
frontal_mid_asy_accuracy <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
frontal_mid_absasy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
reg<-lm(as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE) ~ data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR)
coeff=coefficients(reg)
abline(reg)
frontal_sup_medial_asy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
frontal_sup_medial_absasy_accuracy<-cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
putamen_asy_accuracy <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
putamen_absasy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
supp_motor_area_asy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
supp_motor_area_absasy_accuracy<-cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
reg<-lm(as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE) ~ data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR)
coeff=coefficients(reg)
abline(reg)
thal_VPL_asy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
thal_VPL_absasy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")

#p-adjustments
asy_accuracy_p <- c(frontal_med_orb_asy_accuracy$p.value,frontal_mid_asy_accuracy$p.value,frontal_sup_medial_asy_accuracy$p.value, putamen_asy_accuracy$p.value,supp_motor_area_asy_accuracy$p.value,thal_VPL_asy_accuracy$p.value)
absasy_accuracy_p <- c(frontal_med_orb_absasy_accuracy$p.value,frontal_mid_absasy_accuracy$p.value,frontal_sup_medial_absasy_accuracy$p.value, putamen_absasy_accuracy$p.value,supp_motor_area_absasy_accuracy$p.value,thal_VPL_absasy_accuracy$p.value)
p.adjust(asy_accuracy_p,method = "fdr",n=length(asy_accuracy_p))
p.adjust(absasy_accuracy_p,method = "fdr",n=length(absasy_accuracy_p))

#Asymmetry and Age
plot(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, data$Age_CurrentVisit)
frontal_med_orb_asy_age <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, data$Age_CurrentVisit, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, data$Age_CurrentVisit)
frontal_med_orb_absasy_age <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, data$Age_CurrentVisit, use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$Age_CurrentVisit)
frontal_mid_asy_age<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$Age_CurrentVisit, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$Age_CurrentVisit)
frontal_mid_absasy_age<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$Age_CurrentVisit, use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$Age_CurrentVisit)
reg<-lm(data$Age_CurrentVisit ~ data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR)
coeff=coefficients(reg)
abline(reg)
frontal_sup_medial_asy_age<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$Age_CurrentVisit, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$Age_CurrentVisit)
frontal_sup_medial_absasy_age<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$Age_CurrentVisit, use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$Age_CurrentVisit)
putamen_asy_age<- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$Age_CurrentVisit, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$Age_CurrentVisit)
putamen_absasy_age<-cor.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$Age_CurrentVisit, use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, data$Age_CurrentVisit)
supp_motor_area_asy_age<- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, data$Age_CurrentVisit, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, data$Age_CurrentVisit)
supp_motor_area_absasy_age<- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, data$Age_CurrentVisit, use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$Age_CurrentVisit)
thal_VPL_asy_age<- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$Age_CurrentVisit, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$Age_CurrentVisit)
thal_VPL_absasy_age<- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$Age_CurrentVisit, use = "complete.obs")

#p adjustments
asy_age_p <- c(frontal_med_orb_asy_age$p.value, frontal_mid_asy_age$p.value, frontal_sup_medial_asy_age$p.value,putamen_asy_age$p.value,supp_motor_area_asy_age$p.value,thal_VPL_asy_age$p.value)
absasy_age_p <- c(frontal_med_orb_absasy_age$p.vaue, frontal_mid_absasy_age$p.value, frontal_sup_medial_absasy_age$p.value,putamen_absasy_age$p.value,supp_motor_area_absasy_age$p.value,thal_VPL_absasy_age$p.value)
p.adjust(asy_age_p,method = "fdr",n=length(asy_age_p))
p.adjust(absasy_age_p,method = "fdr",n=length(absasy_age_p))

#Asymmetry and Education
plot(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, data$Education)
frontal_med_orb_asy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, data$Education, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, data$Education)
frontal_med_orb_absasy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, data$Education, use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$Education)
frontal_mid_asy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$Education, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$Education)
frontal_mid_absasy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$Education, use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$Education)
frontal_sup_medial_asy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$Education, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$Education)
frontal_sup_medial_absasy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$Education, use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$Education)
putamen_asy_education <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$Education, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$Education)
putamen_absasy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$Education, use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, data$Education)
supp_motor_area_asy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, data$Education, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, data$Education)
reg<-lm(data$Education ~ data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR)
coeff=coefficients(reg)
abline(reg)
supp_motor_area_absasy_education <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, data$Education, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$Education)
thal_VPL_asy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$Education, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$Education)
thal_VPL_absasy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$Education, use = "complete.obs")

#p adjustments
absasy_education_p <- c(frontal_med_orb_absasy_education$p.value, frontal_mid_absasy_education$p.value, frontal_sup_medial_absasy_education$p.value,putamen_absasy_eduation$p.value, supp_motor_area_absasy_education$p.value, thal_VPL_absasy_education$p.value)
p.adjust(absasy_education_p,method = "fdr",n=length(absasy_education_p))






# Li Analysis #############################################################################################################

library("ggpubr")

# IMPORTING Data ###########################################################
data <- import("/Users/jinghangli/Desktop/Pitt Fall 2020/GPN/KLU_APC2_Master_2020_09_18.xlsx")
data <- data[is.na(data$FaceNames_Exclude), ] #Issues with face name data and only 1 scan/subject - 87 observations
data <- data[data$Visit_Relative == 1, ] # Comment out for longitudinal studies
data <- data[!is.na(data$FaceNames_GoodCoverage), ] # Comment out for longitudinal studies
data$PiBStatus_SUVR_GTM_FS_Global[data$PiBStatus_SUVR_GTM_FS_Global == "NaN"] = NA
data$Sex[data$Sex == "NaN"] = NA
PiB_m <- median(data$PiB_SUVR_GTM_FS_Global[!is.na(data$PiB_SUVR_GTM_FS_Global)])

data$PiB_status <- NA #creating a variable PiB status
data$PiB_status[(data$PiB_SUVR_GTM_FS_Global > PiB_m)] <- "Amyloid Burdened" # 1 if PiB score over threshold
data$PiB_status[(data$PiB_SUVR_GTM_FS_Global < PiB_m)] <- "non Amyloid Burdened" # 0 if PiB score under threshold

boxData <- data.frame('Putamen' = c(data$FaceNames_Pos_Novel_Control_Putamen_L, 
                                    data$FaceNames_Pos_Novel_Control_Putamen_R),
                      'PutamenCat'= c('Putamen_L'),
                      'Thalamus' = c(data$FaceNames_Pos_Novel_Control_Thal_VPL_L,
                                     data$FaceNames_Pos_Novel_Control_Thal_VPL_R),
                      'ThalamusCat'= c('Thalamus_L'),
                      'Frontal_Mid' = c(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_L,
                                        data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_R),
                      'Frontal_MidCat'= c('Frontal_Mid_L'),
                      'Frontal_Sup_Medial' = c(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_L,
                                               data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_R),
                      'Frontal_Sup_MedialCat'= c('Frontal_Sup_Medial_L'),
                      'Frontal_Med_Ord' = c(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_L,
                                            data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_R),
                      'Frontal_Med_OrdCat'= c('Frontal_Med_Ord_L'),
                      'Supp_Motor_Area' = c(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_L,
                                            data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_R),
                      'Supp_Motor_AreaCat'= c('Supp_Motor_Area_L'))

boxData$PutamenCat[71:140] <- "Putamen_R"
boxData$ThalamusCat[71:140] <- "Thalamus_R"
boxData$Frontal_MidCat[71:140] <- "Frontal_Mid_R"
boxData$Frontal_Sup_MedialCat[71:140] <- "Frontal_Sup_Medial_R"
boxData$Frontal_Med_OrdCat[71:140] <- "Frontal_Med_Ord_R"
boxData$Supp_Motor_AreaCat[71:140] <- "Supp_Motor_Area_R"

ggboxplot(boxData, x = "PutamenCat", y = "Putamen", 
          color = "PutamenCat", palette = c("#00AFBB", "#E7B800"),
          ylab = "Putamen Activation", xlab = "Left & Right regions") + geom_jitter(color="black", size=0.4, alpha=0.9)

ggboxplot(boxData, x = "ThalamusCat", y = "Thalamus", 
          color = "ThalamusCat", palette = c("#00AFBB", "#E7B800"),
          ylab = "Thalamus Activation", xlab = "Left & Right regions") + geom_jitter(color="black", size=0.4, alpha=0.9)

ggboxplot(boxData, x = "Frontal_MidCat", y = "Frontal_Mid", 
          color = "Frontal_MidCat", palette = c("#00AFBB", "#E7B800"),
          ylab = "Middle Frontal Gyrus Activation", xlab = "Left & Right regions") + geom_jitter(color="black", size=0.4, alpha=0.9)

ggboxplot(boxData, x = "Frontal_Sup_MedialCat", y = "Frontal_Sup_Medial", 
          color = "Frontal_Sup_MedialCat", palette = c("#00AFBB", "#E7B800"),
          ylab = "Superior Frontal Gyrus Activation", xlab = "Left & Right regions") + geom_jitter(color="black", size=0.4, alpha=0.9)

ggboxplot(boxData, x = "Frontal_Med_OrdCat", y = "Frontal_Med_Ord", 
          color = "Frontal_Med_OrdCat", palette = c("#00AFBB", "#E7B800"),
          ylab = "Medial Orbitofrontal Cortex Activation", xlab = "Left & Right regions") + geom_jitter(color="black", size=0.4, alpha=0.9)

ggboxplot(boxData, x = "Supp_Motor_AreaCat", y = "Supp_Motor_Area", 
          color = "Supp_Motor_AreaCat", palette = c("#00AFBB", "#E7B800"),
          ylab = "Supplement Motor Area Activation", xlab = "Left & Right regions") + geom_jitter(color="black", size=0.4, alpha=0.9)





# AI WITH SEX DIFFERENCE #######################################################
#Putamen **
ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Putamen AI", xlab = "Sex")
mdl1 <- t.test(FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR~Sex, data = data)

ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Putamen Abs AI", xlab = "Sex")
mdl11 <- t.test(FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR~Sex, data = data)

#Thalumus
ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Thalamus AI", xlab = "Sex")
mdl2 <- t.test(FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR~Sex, data = data)


ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Thalamus Abs AI", xlab = "Sex")

mdl22 <- t.test(FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR~Sex, data = data)

#Frontal_Med
ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Frontal_Med AI", xlab = "Sex")
mdl3 <- t.test(FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR~Sex, data = data)

ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Frontal_Med Abs AI", xlab = "Sex")
mdl33 <- t.test(FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR~Sex, data = data)

#Frontal_Mid **
ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Frontal Mid AI", xlab = "Sex")
mdl4 <- t.test(FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR~Sex, data = data)

ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Frontal Mid abs AI", xlab = "Sex")
mdl44 <- t.test(FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR~Sex, data = data)

#Supp_motor_Area
ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Supp_motor Area AI", xlab = "Sex")
mdl5 <- t.test(FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR~Sex, data = data)

ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Supp_motor Area abs AI", xlab = "Sex")
mdl55 <- t.test(FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR~Sex, data = data)

#Sup_Medial

ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Sup_Medial AI", xlab = "Sex")
mdl6 <- t.test(FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR~Sex, data = data)

ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Sup_Medial Area abs AI", xlab = "Sex")
mdl66 <- t.test(FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR~Sex, data = data)


#P ADJUST ######################################################################
pvalList1 <- c(mdl1$p.value, mdl2$p.value, mdl3$p.value, mdl4$p.value, mdl5$p.value, mdl6$p.value)
pvalList2 <- c(mdl11$p.value, mdl22$p.value, mdl33$p.value, mdl44$p.value, mdl55$p.value, mdl66$p.value)
adjustedP1 <- p.adjust(pvalList1, method = 'fdr')
addadjustedP2 <- p.adjust(pvalList2, method = 'fdr')

# AI WITH FDG ##################################################################
#Putamen 
mdl1 <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
mdl11 <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global)
plot(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global)

mdl2 <- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
mdl22 <- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global)
plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global)

mdl3 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
mdl33 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global)
plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global)

mdl4 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
mdl44 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
reg <- lm( FDG_SUVR_GTM_FS_Global ~  FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data = data)
plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global)
abline(reg, col = 'blue')

plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global)

mdl5 <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
mdl55 <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")

mdl6 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
mdl66 <- cor.test(data$FDG_SUVR_GTM_FS_Global, data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, method = "pearson", use = "complete.obs")

#P ADJUST ######################################################################
pvalList1 <- c(mdl1$p.value, mdl2$p.value, mdl3$p.value, mdl4$p.value, mdl5$p.value, mdl6$p.value)
pvalList2 <- c(mdl11$p.value, mdl22$p.value, mdl33$p.value, mdl44$p.value, mdl55$p.value, mdl66$p.value)
correlation1 <- c(mdl1$estimate, mdl2$estimate, mdl3$estimate, mdl4$estimate, mdl5$estimate, mdl6$estimate)
correlation2 <- c(mdl11$estimate, mdl22$estimate, mdl33$estimate, mdl44$estimate, mdl55$estimate, mdl66$estimate)
adjustedP1 <- p.adjust(pvalList1, method = 'fdr')
adjustedP2 <- p.adjust(pvalList2, method = 'fdr')

# AI with Age
mdl1 <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")
mdl11 <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$Age_CurrentVisit)
plot(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$Age_CurrentVisit)

mdl2 <- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")
mdl22 <- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$Age_CurrentVisit)
plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$Age_CurrentVisit)

mdl3 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")
mdl33 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$Age_CurrentVisit)
plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$Age_CurrentVisit)

mdl4 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")
mdl44 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$Age_CurrentVisit)
plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$Age_CurrentVisit)

mdl5 <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")
mdl55 <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")

mdl6 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")
mdl66 <- cor.test(data$Age_CurrentVisit, data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, method = "pearson", use = "complete.obs")
pvalList1 <- c(mdl1$p.value, mdl2$p.value, mdl3$p.value, mdl4$p.value, mdl5$p.value, mdl6$p.value)
pvalList2 <- c(mdl11$p.value, mdl22$p.value, mdl33$p.value, mdl44$p.value, mdl55$p.value, mdl66$p.value)
correlation1 <- c(mdl1$estimate, mdl2$estimate, mdl3$estimate, mdl4$estimate, mdl5$estimate, mdl6$estimate)
correlation2 <- c(mdl11$estimate, mdl22$estimate, mdl33$estimate, mdl44$estimate, mdl55$estimate, mdl66$estimate)

adjustedP1 <- p.adjust(pvalList1, method = 'fdr')
adjustedP2 <- p.adjust(pvalList2, method = 'fdr')


