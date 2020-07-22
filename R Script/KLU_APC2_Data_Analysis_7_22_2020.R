rm(list=ls())
dev.off()
pacman::p_load(pacman, rio) 
library(tibble)
# IMPORTING Data ###########################################################
data <- import("~/Desktop/GitHub/KLU_APC2_Jinghang_Mountz_7_22_2020/KLU_APC2_Master_2020_07_22.xlsx")
activation <- import("~/Desktop/GitHub/KLU_APC2_Jinghang_Mountz_7_22_2020/Appending_to_Master/activ_values.txt")
AI <- import("~/Desktop/GitHub/KLU_APC2_Jinghang_Mountz_7_22_2020/Appending_to_Master/AI.txt")
FWHM <- import("~/Desktop/GitHub/KLU_APC2_Jinghang_Mountz_7_22_2020/Appending_to_Master/FWHM.txt")
FWHM <- abs(FWHM)
# Filter Data ##############################################################
data <- data[-c(which(data$FaceNames_Exclude == 'Yes')),]
data <- data[data$Visit_Relative == 1,] #Issues with face name data and only 1 scan/subject - 87 observations
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
data$Race_cat <- data$Race != 'White' #not white = 1 (True)
data$Education_cat <- data$Education > 12  #higher education = 1 (True)
data$Sex_cat <- (data$Sex == 'Male') #1 = male
data$Sex_cat[data$Sex == "NaN"] = NA
data$LETTER_FLUENCY <- (data$FLUENA + data$FLUENF+ data$FLUENS) / 3
data$WREC_TOT <- (data$WREC + data$WREC2 + data$WREC3)
data$STRINTERFERENCE <- (data$STRCW - data$STRCOL) / data$STRCOL
data$PiB_STATUS_CODE <- (data$PiBStatus_SUVR_GTM_FS_Global == "pos")
data$PiB_STATUS_CODE[data$PiBStatus_SUVR_GTM_FS_Global == "NaN"] = NA
data$APOE_CODE[data$APOE_CODE == "NaN"] = NA
data$Abs_Hippocampus_AI <- abs(data$Hippocampus_AI)
data$Abs_DLPFC_AI <- abs(data$DLPFC_AI)
data$FaceName_PostScanAccuracy[data$FaceName_PostScanAccuracy == "NA"] <- NA
data$FaceName_PostScanAccuracy <- as.numeric(data$FaceName_PostScanAccuracy)

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
points(x_l_h,y_l_h,pch = 4, cex = 2)
points(x_r_h,y_r_h,pch = 4, cex = 2)
legend(x=-2.5,y=27,c("Left Hippocampus", "Right Hippocampus", "PiB(+) Subjects"),cex=.8,col=c("red","black","black"),pch=c(1,2,4))

plot(data$Left_DLPFC_Activation, data$Left_DLPFC_FWHM,col="blue", pch = 1, xlab="Mean Activation", ylab="FWHM")
points(data$Right_DLPFC_Activation, data$Right_DLPFC_FWHM, col = "brown", pch =2)
points(x_l_d,y_l_d,pch = 4, cex = 2)
points(x_r_d,y_r_d,pch = 4, cex = 2)
legend(x=-4,y=35,c("Left DLPFC", "Right DLPFC","PiB(+) Subjects"),cex=.8,col=c("blue","brown","black"),pch=c(1,2,4))

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
STRCW_Z <- (data$STRCW - mean(data$STRCW, na.rm = TRUE)) / sd(data$STRCW, na.rm = TRUE)
TRAILAS_Z_INV <- -1 * TRAILAS_Z
TRAILBS_Z_INV <- -1 * TRAILBS_Z

# Domain Scores #########################################################################
#doi:10.1016/j.jalz.2017.12.003., doi:10.1080/13607860903071014. (Both Beth Snitz articles), https://www.ncbi.nlm.nih.gov/books/NBK285344/ - for SPANSB in Executive
data$memory_learning <- (LMIAIMM_Z + REYIM_Z + WREC_TOT_Z) / 3
data$memory_retrieval <- (LMIIADEL_Z + REYDE_Z + WRECDE_Z) / 3 
data$visiospatial <- (BLOCKDES_Z + REYCO_Z) / 2
data$language <- (FLUEN_Z + LETTER_FLUENCY_Z + BNT60TOT_Z) / 3
data$executive_attention <- (TRAILAS_Z_INV + TRAILBS_Z_INV + CLOCKD_Z + DIGSYMWR_Z + STRCW_Z + SPANSF_Z + SPANSB_Z) / 7

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

# Visiospatial
BLOCKDES_Pearson_Correlation <- cor(data$visiospatial, data$BLOCKDES, use = "complete.obs")
REYCO_Pearson_Correlation <- cor(data$visiospatial, data$REYCO, use = "complete.obs")

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
mdl_Abs_hippocampus_AI <- lm(Abs_Hippocampus_AI ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_Abs_hippocampus_AI)

mdl_Abs_DLPFC_AI <- lm(Abs_DLPFC_AI ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_Abs_DLPFC_AI)

# Cognitive Factors with All Variables - Absolute AI################################################################
#Language (0.06), executive1 (0.0299), executive_attention (0.02)
mdl_memory_learning_abs_AI <- lm(memory_learning ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_memory_learning_abs_AI)

mdl_memory_retrieval_abs_AI <- lm(memory_retrieval ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_memory_retrieval_abs_AI)

mdl_visiospatial_abs_AI <- lm(visiospatial ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_visiospatial_abs_AI)

mdl_language_abs_AI <- lm(language ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_language_abs_AI)

mdl_executive_attention_abs_AI <- lm(executive_attention ~ FaceName_PostScanAccuracy+Abs_DLPFC_AI + Abs_Hippocampus_AI+ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_executive_attention_abs_AI)

###############################################################################

save.image(file = "~/Desktop/RStudio Scripts/KLU_APC2_Data_Analysis_7_22_2020") #path for spreadsheet - saves all variables




