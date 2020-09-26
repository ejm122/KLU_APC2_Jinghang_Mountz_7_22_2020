rm(list=ls())
dev.off()
pacman::p_load(pacman, rio)
library(ggplot2)
# IMPORTING Data ###########################################################
data <- import("~/Desktop/GPN/KLU_APC2_Master_2020_09_18.xlsx")
# Filtering Data ##################################################################
data <- data[is.na(data$FaceNames_Exclude),] #Issues with face name data and only 1 scan/subject - 87 observations
data <- data[data$Visit_Relative == 1,] # Comment out for longitudinal studies
data <- data[data$FaceNames_GoodCoverage == 1,]
data$PiB
data$PiB_Median_Split <- NA
PiB_Median = median(data$PiB_SUVR_GTM_FS_Global, na.rm = 'True');

# PiB Median Split ###############################################################
data$PiB_Median_Split[which(data$PiB_SUVR_GTM_FS_Global > PiB_Median)] <- "high_PiB"
data$PiB_Median_Split[which(data$PiB_SUVR_GTM_FS_Global <= PiB_Median)] <- "low_PiB"

#Independent T Test ###############################################################
#Two sided = mean difference can be less than/greater than 0
#Variances are assumed to NOT be equal (false)
t.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
t.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)

t.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
t.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)

t.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
t.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)

t.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
t.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)

t.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
t.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)

t.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
t.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)

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

#Look into these things: FDR correction, and (p.adjust(stats) == adjusts for multiple comparisons)
