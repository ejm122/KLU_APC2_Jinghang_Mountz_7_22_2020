rm(list=ls())
dev.off()
pacman::p_load(pacman, rio)
library(tibble)
library(ggplot2)
# IMPORTING Data ###########################################################
data <- import("~/Desktop/GitHub/KLU_APC2_Jinghang_Mountz_7_22_2020/Appending_to_Master/KLU_APC2_Master_2020_07_22.xlsx")
activation <- import("~/Desktop/GitHub/KLU_APC2_Jinghang_Mountz_7_22_2020/Appending_to_Master/activ_values.txt")
AI <- import("~/Desktop/GitHub/KLU_APC2_Jinghang_Mountz_7_22_2020/Appending_to_Master/AI.txt")
FWHM <- import("~/Desktop/GitHub/KLU_APC2_Jinghang_Mountz_7_22_2020/Appending_to_Master/FWHM.txt")
FWHM <- abs(FWHM)

# data <- import("/Users/jinghangli/Documents/GitHub/KLU_APC2_Jinghang_Mountz_7_22_2020/KLU_APC2_Master_2020_07_22.xlsx")
# activation <- import("/Users/jinghangli/Documents/GitHub/KLU_APC2_Jinghang_Mountz_7_22_2020/Appending_to_Master/activ_values.txt")
# AI <- import("/Users/jinghangli/Documents/GitHub/KLU_APC2_Jinghang_Mountz_7_22_2020/Appending_to_Master/AI.txt")
# FWHM <- import("/Users/jinghangli/Documents/GitHub/KLU_APC2_Jinghang_Mountz_7_22_2020/Appending_to_Master/FWHM.txt")
# FWHM <- abs(FWHM)
# Filter Data ##############################################################
data <- data[is.na(data$FaceNames_Exclude) & data$Visit_Relative == 1,] #Issues with face name data and only 1 scan/subject - 87 observations
list <- match(activation$Scan_ID,data$Vault_Scan_ID)
index <- which(list!=0,arr.ind = T)
list <- na.omit(match(activation$Scan_ID, data$Vault_Scan_ID))

#creating new variables that are going to be appended
data$Subject_ID <- NA
data$Scan_ID <- NA
data$Left_Hippocampus_Activation <- NA
data$Right_Hippocampus_Activation <- NA
data$Left_DLPFC_Activation <- NA
data$Right_DLPFC_Activation <- NA
data$Hippocampus_AI <- NA
data$DLPFC_AI <-NA
data$Left_Hippocampus_FWHM <- NA
data$Right_Hippocampus_FWHM <- NA
data$Left_DLPFC_FWHM <- NA
data$Right_DLPFC_FWHM <- NA

#appending
data$Subject_ID[list] <- activation$Subject_ID[index]
data$Scan_ID[list] <- activation$Scan_ID[index]
data$Left_Hippocampus_Activation[list] <- activation[,3][index]
data$Right_Hippocampus_Activation[list] <- activation[,4][index]
data$Left_DLPFC_Activation[list] <- activation[,5][index]
data$Right_DLPFC_Activation[list] <- activation[,6][index]
data$Hippocampus_AI[list] <- AI[,3][index]
data$DLPFC_AI[list] <- AI[,4][index]

data$Left_Hippocampus_FWHM[list] <- FWHM[,3][index]
data$Right_Hippocampus_FWHM[list] <- FWHM[,4][index]
data$Left_DLPFC_FWHM[list] <- FWHM[,5][index]
data$Right_DLPFC_FWHM[list] <- FWHM[,6][index]

# Recode Variables ##############################################################
data$Race[data$Race == "NaN"] = NA
data$Race_cat <- data$Race != 'White' #non-white = TRUE
data$Education_cat <- data$Education > 12  #higher education = True
data$Sex[data$Sex == "NaN"] = NA
data$Sex_cat <- (data$Sex == 'Male') #TRUE = male
data$PiBStatus_SUVR_GTM_FS_Global[data$PiBStatus_SUVR_GTM_FS_Global == "NaN"] = NA
data$PiB_STATUS_CODE <- (data$PiBStatus_SUVR_GTM_FS_Global == "pos") #positive = TRUE
data$APOE_CODE[data$APOE_CODE == "NaN"] = NA
data$APOE_STATUS_CODE <- data$APOE_CODE == "At Least One E4 Allele" #E4 allele = TRUE
data$FaceName_PostScanAccuracy[data$FaceName_PostScanAccuracy == "NA"] <- NA
data$FaceName_PostScanAccuracy <- as.numeric(data$FaceName_PostScanAccuracy)
data$Abs_Hippocampus_AI <- abs(data$Hippocampus_AI)
data$Abs_DLPFC_AI <- abs(data$DLPFC_AI)
data$LETTER_FLUENCY <- (data$FLUENA + data$FLUENF+ data$FLUENS) / 3
data$WREC_TOT <- (data$WREC + data$WREC2 + data$WREC3)
Pred_STRCW <- (data$STRCOL*data$STRWRD) / (data$STRCOL+data$STRWRD) #doi: 10.3389/fpsyg.2017.00557 - Stroop Interference Score
data$STRINTERFERENCE <- data$STRCW - Pred_STRCW

#identifying PiB(+) subjects
x_l_h <- data$Left_Hippocampus_Activation[data$PiB_STATUS_CODE == TRUE]
y_l_h <- data$Left_Hippocampus_FWHM[data$PiB_STATUS_CODE == TRUE]
x_r_h <- data$Right_Hippocampus_Activation[data$PiB_STATUS_CODE == TRUE]
y_r_h <- data$Right_Hippocampus_FWHM[data$PiB_STATUS_CODE == TRUE]

x_l_d <-data$Left_DLPFC_Activation[data$PiB_STATUS_CODE == TRUE]
y_l_d <- data$Left_DLPFC_FWHM[data$PiB_STATUS_CODE == TRUE]
x_r_d <-data$Right_DLPFC_Activation[data$PiB_STATUS_CODE == TRUE]
y_r_d <- data$Right_DLPFC_FWHM[data$PiB_STATUS_CODE == TRUE]
#visulize data#################################################################
plot(data$Left_Hippocampus_Activation, data$Left_Hippocampus_FWHM,col="red", pch = 1, xlab="Mean Activation", ylab="FWHM")
points(data$Right_Hippocampus_Activation, data$Right_Hippocampus_FWHM, pch = 2, col="black")
points(x_l_h,y_l_h,pch = 19, cex = 1)
points(x_r_h,y_r_h,pch = 17, cex = 1)
legend(x=-2.5,y=27,c("Left Hippocampus", "Right Hippocampus", "PiB(+) Subjects","PiB(+) Subjects"),cex=.8,col=c("red","black","black","black"),pch=c(1,2,19,17))

plot(data$Left_DLPFC_Activation, data$Left_DLPFC_FWHM,col="blue", pch = 1, xlab="Mean Activation", ylab="FWHM")
points(data$Right_DLPFC_Activation, data$Right_DLPFC_FWHM, col = "brown", pch =2)
points(x_l_d,y_l_d,pch = 19, cex = 1)
points(x_r_d,y_r_d,pch = 17, cex = 1)
legend(x=-4,y=35,c("Left DLPFC", "Right DLPFC","PiB(+) Subjects","PiB(+) Subjects"),cex=.8,col=c("blue","brown","black","black"),pch=c(1,2,19,17))

plot(data$Left_Hippocampus_Activation,data$Right_Hippocampus_Activation, col="black", pch =1, xlab = "Left Hippocampus Mean Activation", ylab = "Right Hippocampus Mean Activation",cex = 1,xlim=c(-3,4), ylim=c(-3,4))
points(x_l_h, x_r_h,pch = 19, cex = 1, col="red") #hippocampus left and right activation
data_PiB_Positive <- data[which(data$PiBStatus_SUVR_GTM_FS_Global == "pos"),]
data_PiB_Negative <- data[which(data$PiBStatus_SUVR_GTM_FS_Global == "neg"),]
mdl_right_left_activation_positive <- lm(lm(Right_Hippocampus_Activation ~ Left_Hippocampus_Activation, data = data_PiB_Positive))
summary(mdl_right_left_activation_positive)
mdl_right_left_activation_negative <- lm(lm(Right_Hippocampus_Activation ~ Left_Hippocampus_Activation, data = data_PiB_Negative))
summary(mdl_right_left_activation_negative)
abline(mdl_right_left_activation_positive, col = "red")
abline(mdl_right_left_activation_negative)
legend(-3,4, legend = c("PiB Negative", "PiB Positive"), col = c("black","red"), lty = 1, cex = 0.8)


plot(data$Left_DLPFC_Activation,data$Right_DLPFC_Activation, col="black", pch =1, xlab = "Left DLPFC Mean Activation", ylab = "Right DLPFC Mean Activation",cex = 1,xlim=c(-3,4), ylim=c(-3,4))
points(x_l_d,x_r_d,pch = 19, cex = 1, col="red") #dlpfc left and right activation
# Cognitive Domain - Z Transform ####################################################################
#Negative z value means that lower value = higher performance
# doi:10.1016/j.jalz.2017.12.003 - method of composite calculation
#doi/ 10.1136/jnnp.2004.045567 - standard deviations from normative data
CLOCKD_Z <- (data$CLOCKD - mean(data$CLOCKD, na.rm = TRUE)) / sd(data$CLOCKD, na.rm = TRUE)
BLOCKDES_Z <- (data$BLOCKDES - mean(data$BLOCKDES, na.rm = TRUE)) / sd(data$BLOCKDES, na.rm = TRUE)
BNT60TOT_Z <- (data$BNT60TOT - mean(data$BNT60TOT, na.rm = TRUE)) / sd(data$BLOCKDE, na.rm = TRUE)
REYCO_Z <- (data$REYCO - mean(data$REYCO, na.rm = TRUE)) / sd (data$REYCO, na.rm = TRUE)
REYIM_Z <- (data$REYIM-mean(data$REYIM, na.rm = TRUE)) / sd(data$REYIM, na.rm = TRUE)
REYDE_Z <- (data$REYDE - mean(data$REYDE, na.rm = TRUE)) / sd(data$REYDE, na.rm = TRUE)
FLUEN_Z <- (data$FLUEN - mean(data$FLUEN, na.rm = TRUE)) / sd(data$FLUEN, na.rm = TRUE)
LETTER_FLUENCY_Z <- (data$LETTER_FLUENCY - mean(data$LETTER_FLUENCY, na.rm = TRUE)) / sd(data$LETTER_FLUENCY, na.rm=TRUE)
WREC_TOT_Z <- (data$WREC_TOT - mean(data$WREC_TOT, na.rm = TRUE)) / sd(data$WREC_TOT, na.rm =TRUE)
WRECDE_Z <- (data$WRECDE - mean(data$WRECDE, na.rm = TRUE)) / sd(data$WRECDE, na.rm = TRUE)
SPANSF_Z <- (data$SPANSF - mean(data$SPANSF, na.rm = TRUE)) / sd(data$SPANSF, na.rm = TRUE)
SPANSB_Z <- (data$SPANSB - mean(data$SPANSB, na.rm = TRUE)) / sd(data$SPANSB, na.rm = TRUE)
TRAILAS_Z <- (data$TRAILAS - mean(data$TRAILAS, na.rm = TRUE)) / sd(data$TRAILAS, na.rm = TRUE)
TRAILBS_Z <- (data$TRAILBS - mean(data$TRAILBS, na.rm = TRUE)) / sd(data$TRAILBS, na.rm= TRUE)
LMIAIMM_Z <- (data$LMIAIMM - mean(data$LMIAIMM, na.rm = TRUE)) /sd(data$LMIAIMM, na.rm = TRUE)
LMIIADEL_Z <- (data$LMIIADEL - mean(data$LMIIADEL, na.rm = TRUE))/sd(data$LMIIADEL, na.rm = TRUE)
DIGSYMWR_Z <- (data$DIGSYMWR - mean(data$DIGSYMWR, na.rm = TRUE))/sd(data$DIGSYMWR, na.rm = TRUE)
STRINTERFERENCE_Z <- (data$STRINTERFERENCE - mean(data$STRINTERFERENCE, na.rm = TRUE))/ sd(data$STRINTERFERENCE, na.rm = TRUE)
TRAILAS_Z_INV <- -1 * TRAILAS_Z
TRAILBS_Z_INV <- -1 * TRAILBS_Z

# Domain Scores #########################################################################
#doi:10.1016/j.jalz.2017.12.003., doi:10.1080/13607860903071014. (Both Beth Snitz articles), https://www.ncbi.nlm.nih.gov/books/NBK285344/ - for SPANSB in Executive
data$memory_learning <- (LMIAIMM_Z + REYIM_Z + WREC_TOT_Z) / 3
data$memory_retrieval <- (LMIIADEL_Z + REYDE_Z + WRECDE_Z) / 3
data$visuospatial <- (BLOCKDES_Z + REYCO_Z) / 2
data$language <- (FLUEN_Z + LETTER_FLUENCY_Z + BNT60TOT_Z) / 3
data$executive_attention <- (TRAILAS_Z_INV + TRAILBS_Z_INV + CLOCKD_Z + DIGSYMWR_Z + STRINTERFERENCE_Z + SPANSF_Z + SPANSB_Z) / 7
#Violin Plots####################################
# Violin plots
library(ggplot2)
vplot_data <- data.frame("PiB" = data$PiB_STATUS_CODE, "Left Hippocampus FWHM" = data$Left_Hippocampus_FWHM,
                         "Left Hippocampus Activation" = data$Left_Hippocampus_Activation, "Right Hippocampus Activation" = data$Right_Hippocampus_Activation, 
                         "Abs Hippocampus AI" = data$Abs_Hippocampus_AI, "Executive_Attention" = data$executive_attention, "Memory_Learning" = data$memory_learning,
                         "Memory_Retrieval" = data$memory_retrieval, "Visuospatial" = data$visuospatial, "Language" = data$language)

# vplot_data <- data.frame("PiB" = na.omit(data$PiB_STATUS_CODE), "Left Hippocampus FWHM" = data$Left_Hippocampus_FWHM[(!is.na(data$PiB_STATUS_CODE))])
# vplot_data <- data.frame("PiB" = data$PiB_STATUS_CODE, "Left Hippocampus FWHM" = data$Left_Hippocampus_FWHM)
#creating violin plot
vplot_data$PiB <- as.factor(vplot_data$PiB)

L_Hippocampus_FWHM_violin <- ggplot(vplot_data, aes(x=PiB, y=Left.Hippocampus.FWHM, fill = PiB)) + geom_violin(trim=FALSE) + 
  labs(title="Left Hippocampus Full Width Half Maximum",x="Left Hippocampus FWHM", y = "PiB Status")

L_Hippocampus_activation_violin <- ggplot(vplot_data, aes(x=PiB, y=Left.Hippocampus.Activation, 
                                                          mainTitle="Left Hippocampus Activation", fill = PiB)) + geom_violin(trim=FALSE)
R_Hippocampus_activation_violin <- ggplot(vplot_data, aes(x=PiB, y=Right.Hippocampus.Activation, 
                                                          mainTitle="Right Hippocampus Activation", fill = PiB)) + geom_violin(trim=FALSE)
abs_hippocampus_AI_violin  <- ggplot(vplot_data, aes(x=PiB, y=Abs.Hippocampus.AI, 
                                                     mainTitle="Absolute Hippocampus Mean Activation Asymmetry", fill = PiB)) + geom_violin(trim=FALSE)
Executive_Attention_violin  <- ggplot(vplot_data, aes(x=PiB, y=Executive_Attention, 
                                                     mainTitle="Executive_Attention", fill = PiB)) + geom_violin(trim=FALSE)
# Function to produce summary statistics (mean and +/- sd)
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

#showing violin plots
L_Hippocampus_FWHM_violin + stat_summary(fun.data=data_summary) + scale_x_discrete(limits=c("FALSE", "TRUE")) + scale_color_brewer(palette="Dark2") + theme_classic()
L_Hippocampus_activation_violin + stat_summary(fun.data=data_summary) + scale_x_discrete(limits=c("FALSE", "TRUE")) + scale_color_brewer(palette="Dark2") 
R_Hippocampus_activation_violin + stat_summary(fun.data=data_summary) + scale_x_discrete(limits=c("FALSE", "TRUE")) + scale_color_brewer(palette="Dark2") 
abs_hippocampus_AI_violin + stat_summary(fun.data=data_summary) + scale_x_discrete(limits=c("FALSE", "TRUE")) + scale_color_brewer(palette="Dark2") 
Executive_Attention_violin + stat_summary(fun.data=data_summary) + scale_x_discrete(limits=c("FALSE", "TRUE")) + scale_color_brewer(palette="Dark2") 
#Intraclass Correlation ################################################################
# Pearson (Linear Correlation between composite and raw scores)
library("irr")
#Memory_Learning
REYIM_Pearson_Correlation <- cor(data$memory_learning, data$REYIM, use = "complete.obs")
LMIAIMM_Pearson_Correlation <- cor(data$memory_learning, data$LMIAIMM, use = "complete.obs")
WREC_TOT_Pearson_Correlation <- cor(data$memory_learning, data$WREC_TOT, use = "complete.obs")

#Memory_Retrieval
LMIIADEL_Pearson_Correlation <- cor(data$memory_learning, data$LMIIADEL, use = "complete.obs")
REYDE_Pearson_Correlation <- cor(data$memory_learning, data$REYDE, use = "complete.obs")
WRECDE_Pearson_Correlation <- cor(data$memory_learning, data$WRECDE, use = "complete.obs")

# Visuospatial
BLOCKDES_Pearson_Correlation <- cor(data$visuospatial, data$BLOCKDES, use = "complete.obs")
REYCO_Pearson_Correlation <- cor(data$visuospatial, data$REYCO, use = "complete.obs")

#Langugae
BNT60TOT_Pearson_Correlation <- cor(data$language, data$BNT60TOT, use = "complete.obs")
FLUEN_Pearson_Correlation <- cor(data$language, data$FLUEN, use = "complete.obs")
LETTER_FLUENCY_Pearson_Correlation <- cor(data$language, data$LETTER_FLUENCY, use = "complete.obs")

#Executive_Attention
TRAILBS_Combo_Pearson_Correlation <- cor(data$executive_attention, -1*data$TRAILBS, use = "complete.obs")
TRAILAS_Combo_Pearson_Correlation <- cor(data$executive_attention, -1*data$TRAILAS, use = "complete.obs")
SPANSF_Combo_Pearson_Correlation <- cor(data$executive_attention, data$SPANSF, use = "complete.obs")
DIGSYMWR_Combo_Pearson_Correlation <- cor(data$executive_attention, data$DIGSYMWR, use = "complete.obs")
SPANSB_Combo_Pearson_Correlation <- cor(data$executive_attention, data$SPANSB, use = "complete.obs")
CLOCKD_combo_Pearson_Correlation <- cor(data$executive_attention, data$CLOCKD, use = "complete.obs")

# Association with Absolute AI ####################################################################
mdl_Abs_hippocampus_AI <- lm(Abs_Hippocampus_AI ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_Abs_hippocampus_AI)

mdl_Abs_DLPFC_AI <- lm(Abs_DLPFC_AI ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_Abs_DLPFC_AI)

# Association with FWHM ####################################################################
#left hippocampus fwhm (p = 0.06307)
mdl_left_hippocampus_FWHM <- lm(Left_Hippocampus_FWHM ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_left_hippocampus_FWHM )

mdl_right_hippocampus_FWHM <- lm(Right_Hippocampus_FWHM ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_right_hippocampus_FWHM )

mdl_left_dlpfc_FWHM <- lm(Left_DLPFC_FWHM ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_left_dlpfc_FWHM )

mdl_right_dlpfc_FWHM <- lm(Right_DLPFC_FWHM ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_right_dlpfc_FWHM )
# Cognitive Factors with All Variables - Absolute AI################################################################
#Executive_attention
mdl_memory_learning_abs_AI <- lm(memory_learning ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_memory_learning_abs_AI)

mdl_memory_retrieval_abs_AI <- lm(memory_retrieval ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_memory_retrieval_abs_AI)

mdl_visuospatial_abs_AI <- lm(visuospatial ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_visuospatial_abs_AI)

mdl_language_abs_AI <- lm(language ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_language_abs_AI)

mdl_executive_attention_abs_AI <- lm(executive_attention ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_executive_attention_abs_AI)
# Cognitive Factors with All Variables - FWHM ################################################################
#executive_attention (0.001183)
mdl_memory_learning_FWHM <- lm(memory_learning ~ FaceName_PostScanAccuracy+ Left_Hippocampus_FWHM + Right_Hippocampus_FWHM + Left_Hippocampus_FWHM + Right_Hippocampus_FWHM+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_memory_learning_FWHM)

mdl_memory_retrieval_fwhm <- lm(memory_retrieval ~ FaceName_PostScanAccuracy + Left_Hippocampus_FWHM + Right_Hippocampus_FWHM +Left_DLPFC_FWHM + Right_DLPFC_FWHM+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_memory_retrieval_fwhm)

mdl_visuospatial_fwhm <- lm(visuospatial ~ FaceName_PostScanAccuracy + Left_Hippocampus_FWHM + Right_Hippocampus_FWHM +Left_DLPFC_FWHM + Right_DLPFC_FWHM+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_visuospatial_fwhm)

mdl_language_fwhm <- lm(language ~FaceName_PostScanAccuracy + Left_Hippocampus_FWHM + Right_Hippocampus_FWHM +Left_DLPFC_FWHM + Right_DLPFC_FWHM+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_language_fwhm)

mdl_executive_attention_fwhm <- lm(executive_attention ~ FaceName_PostScanAccuracy+ Left_Hippocampus_FWHM + Right_Hippocampus_FWHM +Left_DLPFC_FWHM + Right_DLPFC_FWHM+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_executive_attention_fwhm)

##########################################################################################################################
# Cohort Demographic Cacluations
age_mean <- mean(data$Age_CurrentVisit, na.rm = TRUE)
age_sd <- sd(data$Age_CurrentVisit, na.rm = TRUE)

sex_data <- as.data.frame(table(data$Sex_cat, useNA = "no"))
no_male <- sex_data[2,2]
percent_male <- no_male / (sex_data[1,2] + sex_data[2,2])

race_data <- as.data.frame(table(data$Race_cat, useNA = "no"))
no_non_white <- race_data[2,2]
percent_non_white <- no_non_white / (race_data[1,2] + race_data[2,2])

education_data <- as.data.frame(table(data$Education_cat, useNA = "no"))
no_over_12_years <- education_data[2,2]
percent_over_12_years <- no_over_12_years / (education_data[1,2] + education_data[2,2])

PiB_data <- as.data.frame(table(data$PiB_STATUS_CODE, useNA = "no"))
no_positive <- PiB_data[2,2]
percent_positive <- no_positive / (PiB_data[1,2] + PiB_data[2,2])

FDG_mean <- mean(data$FDG_SUVR_GTM_FS_Global, na.rm = TRUE)
FDG_sd <- sd(data$FDG_SUVR_GTM_FS_Global, na.rm = TRUE)

APOE_data <- as.data.frame(table(data$APOE_STATUS_CODE, useNA = "no"))
no_E4 <- APOE_data[2,2]
percent_E4 <- no_E4 / (APOE_data[1,2] + APOE_data[2,2])

task_accuracy_mean <- mean(data$FaceName_PostScanAccuracy, na.rm = TRUE)
task_accuracy_sd <- sd(data$FaceName_PostScanAccuracy, na.rm = TRUE)

LMIAIMM_mean <- mean(data$LMIAIMM, na.rm= TRUE)
LMIAIMM_sd <- sd(data$LMIAIMM, na.rm = TRUE)
REYIM_mean <- mean(data$REYIM, na.rm = TRUE)
REYIM_sd <- sd(data$REYIM, na.rm = TRUE)
WREC_TOT_mean <- mean(data$WREC_TOT, na.rm = TRUE)
WREC_TOT_sd <- sd(data$WREC_TOT, na.rm = TRUE)
LMIIADEL_mean <- mean(data$LMIIADEL, na.rm = TRUE)
LMIIADEL_sd <- sd(data$LMIIADEL, na.rm  = TRUE)
REYDE_mean <- mean(data$REYDE, na.rm = TRUE)
REYDE_sd <- sd(data$REYDE, na.rm = TRUE)
WRECDE_mean <- mean(data$WRECDE, na.rm = TRUE)
WRECDE_sd <- sd(data$WRECDE, na.rm = TRUE)
BLOCKDES_mean <- mean(data$BLOCKDES, na.rm = TRUE)
BLOCKDES_sd <- sd(data$BLOCKDES, na.rm = TRUE)
REYCO_mean <- mean(data$REYCO, na.rm = TRUE)
REYCO_sd <- sd(data$REYCO, na.rm = TRUE)
FLUEN_mean <- mean(data$FLUEN, na.rm= TRUE)
FLUEN_sd <- sd(data$FLUEN, na.rm = TRUE)
LETTER_FLUENCY_mean <- mean(data$LETTER_FLUENCY, na.rm = TRUE)
LETTER_FLUENCY_sd <- sd(data$LETTER_FLUENCY,na.rm=TRUE)
BNT60TOT_mean <- mean(data$BNT60TOT, na.rm = TRUE)
BNT60TOT_sd <- sd(data$BNT60TOT, na.rm = TRUE)
TRAILAS_mean <- mean(data$TRAILAS,na.rm= TRUE)
TRAILAS_sd <- sd(data$TRAILAS, na.rm = TRUE)
TRAILBS_mean <- mean(data$TRAILBS, na.rm= TRUE)
TRAILBS_sd <- sd(data$TRAILBS,na.rm = TRUE)
CLOCKD_mean <- mean(data$CLOCKD, na.rm = TRUE)
CLOCKD_sd <- sd(data$CLOCKD,na.rm = TRUE)
SPANSF_mean <- mean(data$SPANSF, na.rm= TRUE)
SPANSF_sd <- sd(data$SPANSF, na.rm = TRUE)
SPANSB_mean <- mean(data$SPANSB,na.rm= TRUE)
SPANSB_sd <- sd(data$SPANSB, na.rm= TRUE)
STR_INTERFERENCE_mean <- mean(data$STRINTERFERENCE, na.rm= TRUE)
STR_INTERFERENCE_sd <- sd(data$STRINTERFERENCE, na.rm= TRUE)
DIGSYMWR_mean <- mean(data$DIGSYMWR,na.rm= TRUE)
DIGSYMWR_sd <- sd(data$DIGSYMWR,na.rm = TRUE)

#Add Values for each cognitive domain to cohort table!!

# Characterization of Results #####################
# Raw AI
mdl_raw_hippocampus_AI <- lm(Hippocampus_AI ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_hippocampus_AI)

#Activation

mdl_activation_right_hippocampus <- lm(Right_Hippocampus_Activation ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_activation_right_hippocampus)

mdl_activation_left_hippocampus <- lm(Left_Hippocampus_Activation ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data)
summary(mdl_activation_left_hippocampus)
plot(data$PiB_STATUS_CODE, data$Left_Hippocampus_Activation)

# Filtering PiB with Activation
data_PiB_Positive <- data[which(data$PiBStatus_SUVR_GTM_FS_Global == "pos"),]
data_PiB_Negative <- data[which(data$PiBStatus_SUVR_GTM_FS_Global == "neg"),]

mdl_activation_right_hippocampus_positive <- lm(Right_Hippocampus_Activation ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data_PiB_Positive)
summary(mdl_activation_right_hippocampus_positive)
plot(data_PiB_Positive$PiB_SUVR_GTM_FS_Global, data_PiB_Positive$Right_Hippocampus_Activation)

mdl_activation_left_hippocampus_positive <- lm(Left_Hippocampus_Activation ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data_PiB_Positive)
summary(mdl_activation_left_hippocampus_positive)
plot(data_PiB_Positive$PiB_SUVR_GTM_FS_Global, data_PiB_Positive$Left_Hippocampus_Activation)

mdl_activation_right_hippocampus_negative <- lm(Right_Hippocampus_Activation ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data_PiB_Negative)
summary(mdl_activation_right_hippocampus_negative)
plot(data_PiB_Negative$PiB_SUVR_GTM_FS_Global, data_PiB_Negative$Right_Hippocampus_Activation)

mdl_activation_left_hippocampus_negative <- lm(Left_Hippocampus_Activation ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data_PiB_Negative)
summary(mdl_activation_left_hippocampus_negative)
plot(data_PiB_Negative$PiB_SUVR_GTM_FS_Global, data_PiB_Negative$Left_Hippocampus_Activation)

mdl_right_left_activation_positive <- lm(Left_Hippocampus_Activation ~ Right_Hippocampus_Activation, data = data_PiB_Positive)
mdl_right_left_activation_negative <- lm(Left_Hippocampus_Activation ~ Right_Hippocampus_Activation, data = data_PiB_Negative)
plot(data$Left_Hippocampus_Activation,data$Right_Hippocampus_Activation, col="black", pch =1, xlab = "Left Hippocampus Mean Activation", ylab = "Right Hippocampus Mean Activation",cex = 1, xlim=c(-3,4), ylim=c(-3,4))
points(x_l_d,x_r_d,pch = 19, cex = 1, col="red") #hippocampus left and right activation
abline(mdl_right_left_activation_positive, col = "red")
abline(mdl_right_left_activation_negative)


# FWHM and AI
summary(lm(data$Right_Hippocampus_FWHM ~ data$Abs_Hippocampus_AI))
plot(data$Abs_Hippocampus_AI, data$Right_Hippocampus_FWHM,  col="black", pch =1, xlab = "Absolute Hippocampus AI", ylab = "Right Hippocampus FWHM",cex = 1)
mdl_FWHM_AI <- lm(data$Left_Hippocampus_FWHM ~ data$Abs_Hippocampus_AI)
summary(mdl_FWHM_AI)
plot(data$Abs_Hippocampus_AI, data$Left_Hippocampus_FWHM, type = "p", col="black", pch =1, xlab = "Absolute Hippocampus AI", ylab = "Left Hippocampus FWHM",cex = 1)
abline(mdl_FWHM_AI)


# AI and FWHM with PiB
summary(lm(data$Abs_Hippocampus_AI ~ data$PiB_STATUS_CODE))
summary(lm(data$Left_Hippocampus_FWHM ~ data$PiB_STATUS_CODE))
summary(lm(data$Right_Hippocampus_FWHM ~ data$PiB_STATUS_CODE))
plot(data$PiB_STATUS_CODE, data$Abs_Hippocampus_AI)
plot(data$PiB_STATUS_CODE, data$Left_Hippocampus_FWHM)
plot(data$PiB_STATUS_CODE, data$Right_Hippocampus_FWHM)
summary(lm(PiB_STATUS_CODE ~ Abs_Hippocampus_AI + Left_Hippocampus_FWHM, data = data))
summary(lm(PiB_STATUS_CODE ~ Abs_Hippocampus_AI + Right_Hippocampus_FWHM, data = data))

summary(lm( Left_Hippocampus_FWHM ~ Abs_Hippocampus_AI + PiB_STATUS_CODE, data = data))
summary(lm( Left_Hippocampus_FWHM ~ Abs_Hippocampus_AI + PiB_STATUS_CODE + PiB_STATUS_CODE*Abs_Hippocampus_AI, data = data))
summary(lm( Abs_Hippocampus_AI ~ Left_Hippocampus_FWHM + PiB_STATUS_CODE + PiB_STATUS_CODE*Left_Hippocampus_FWHM, data = data))

# PiB and FWHM
plot(data$PiB_SUVR_GTM_FS_Global, data$Left_Hippocampus_FWHM)
plot(data$PiB_SUVR_GTM_FS_Global, data$Right_Hippocampus_FWHM)

plot(data$PiB_SUVR_GTM_FS_Global, data$Left_DLPFC_FWHM)
plot(data$PiB_SUVR_GTM_FS_Global, data$Right_DLPFC_FWHM)

# Activation Regressions - Isolated
mdl_hippocampus_left_activation <- lm(Left_Hippocampus_Activation ~ PiB_STATUS_CODE, data = data)
summary(mdl_hippocampus_left_activation)
plot(data$PiB_SUVR_GTM_FS_Global, data$Left_Hippocampus_Activation)
plot(data$PiB_STATUS_CODE, data$Left_Hippocampus_Activation)

mdl_hippocampus_right_activation <- lm(Right_Hippocampus_Activation ~ PiB_STATUS_CODE, data = data)
summary(mdl_hippocampus_right_activation)
plot(data$PiB_SUVR_GTM_FS_Global, data$Right_Hippocampus_Activation)
plot(data$PiB_STATUS_CODE, data$Right_Hippocampus_Activation)

#Activation Regressions with different PiB Classifications

mdl_hippocampus_activations <- lm(Left_Hippocampus_Activation ~ Right_Hippocampus_Activation, data = data)
summary(mdl_hippocampus_activations)
plot(data$Left_Hippocampus_Activation, data$Right_Hippocampus_Activation)

plot(data_PiB_Negative$Left_Hippocampus_Activation, data_PiB_Negative$Right_Hippocampus_Activation)
summary(lm(Right_Hippocampus_Activation ~ Left_Hippocampus_Activation, data = data_PiB_Negative))

plot(data_PiB_Positive$Left_Hippocampus_Activation, data_PiB_Positive$Right_Hippocampus_Activation)
summary(lm(Right_Hippocampus_Activation ~ Left_Hippocampus_Activation, data = data_PiB_Positive))


mdl <- lm(Abs_Hippocampus_AI ~ Left_Hippocampus_FWHM,data = data)
summary(mdl)
X <- data$Abs_Hippocampus_AI[data$PiB_STATUS_CODE == TRUE]
Y <- data$Left_Hippocampus_FWHM[data$PiB_STATUS_CODE == TRUE]
plot(data$Abs_Hippocampus_AI, data$Left_Hippocampus_FWHM)
points(points(X,Y,pch = 19, cex = 1, col="red"))
abline(mdl,col="blue")
################################################################################################################################
save.image(file = "~/Desktop/RStudio Scripts/KLU_APC2_Data_Analysis_7_22_2020") #path for spreadsheet - saves all variables


