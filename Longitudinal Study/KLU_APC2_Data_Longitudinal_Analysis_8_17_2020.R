rm(list=ls())
dev.off()
pacman::p_load(pacman, rio)
library(ggplot2)
# IMPORTING Data ###########################################################
data <- import("~/Desktop/GPN/KLU_APC2_Master_2020_07_24_AS.xlsx")
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
data <- data[is.na(data$FaceNames_Exclude),] #Issues with face name data and only 1 scan/subject - 87 observations
# Comment out for longitudinal studies #############################################################################
#data <- data[data$Visit_Relative == 1,] # Comment out for longitudinal studies
###############################################################################
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
data$PiB_Transform <- -1 / log(data$PiB_SUVR_GTM_FS_Global)

#Filter for only con_0003 > 0 in both ROIs:
# data1 <- data[which(data$Left_Hippocampus_Activation > 0 & data$Right_Hippocampus_Activation > 0),]
# data2 <- data[which(data$Left_DLPFC_Activation > 0 & data$Right_DLPFC_Activation > 0),]

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

# Longitudinal Data  #####################################################################################################3
n_occur <- data.frame(table(data$Vault_UID)) #getting the subject ID and the corresponding recurrence 
table(data$Visit_Relative == 1, useNA = "no") # 901413 = no baseline data so this value gives 87 unique partipants (TRUE = number unique participants)
table(n_occur$Freq) #Total number of subjects with each frequency, 88 total unique participants
data_multiple_visit <- data[data$Vault_UID %in% n_occur$Var1[n_occur$Freq > 1],] #getting the data with the multiple fMRI scans
length(unique(data_multiple_visit$Vault_UID)) #55 unique participants with longitudinal data > 1 visit

#Index each participant's scan info
library(data.table)
setDT(data_multiple_visit)[, Index := seq_len(.N), by = Vault_UID]

#PiB Status Tracking: #######################################################################
table(data_multiple_visit$PiB_STATUS_CODE[data_multiple_visit$Index == 1], useNA = "no")
table(data_multiple_visit$PiB_STATUS_CODE[data_multiple_visit$Index == 2], useNA = "no") #PiB(+) count increases

# Average Time of Scans
first_visit_index <- which(data_multiple_visit$Index == 1)
second_visit_index <- which(data_multiple_visit$Index == 2)
third_visit_index <- which(data_multiple_visit$Index == 3)
fourth_visit_index <- which(data_multiple_visit$Index == 4)

average_second_visit_time <- mean(data_multiple_visit$Visit_Relative[second_visit_index])
average_third_visit_time <- mean(data_multiple_visit$Visit_Relative[third_visit_index])
average_fourth_visit_time <- mean(data_multiple_visit$Visit_Relative[fourth_visit_index])

# Average time change between scans
second_visit_time_change <- data_multiple_visit$Visit_Relative[second_visit_index] - data_multiple_visit$Visit_Relative[second_visit_index - 1]
third_visit_time_change <- data_multiple_visit$Visit_Relative[third_visit_index] - data_multiple_visit$Visit_Relative[third_visit_index - 1]
fourth_visit_time_change <- data_multiple_visit$Visit_Relative[fourth_visit_index] - data_multiple_visit$Visit_Relative[fourth_visit_index - 1]

average_second_visit_time_change <- mean(second_visit_time_change) 
average_third_visit_time_change <- mean(third_visit_time_change) 
average_fourth_visit_time_change <- mean(fourth_visit_time_change) 

# Cognitive Decline Over Time 
first_visit_memory_learning <- data_multiple_visit$memory_learning[first_visit_index]
second_visit_memory_learning <- data_multiple_visit$memory_learning[second_visit_index]
third_visit_memory_learning <- data_multiple_visit$memory_learning[third_visit_index]
fourth_visit_memory_learning <- data_multiple_visit$memory_learning[fourth_visit_index]

first_visit_memory_retrieval <- data_multiple_visit$memory_retrieval[first_visit_index]
second_visit_memory_retrieval <- data_multiple_visit$memory_retrieval[second_visit_index]
third_visit_memory_retrieval <- data_multiple_visit$memory_retrieval[third_visit_index]
fourth_visit_memory_retrieval <- data_multiple_visit$memory_retrieval[fourth_visit_index]

first_visit_language <- data_multiple_visit$memory_retrieval[first_visit_index]
second_visit_language <- data_multiple_visit$memory_retrieval[second_visit_index]
third_visit_language <- data_multiple_visit$memory_retrieval[third_visit_index]
fourth_visit_language <- data_multiple_visit$memory_retrieval[fourth_visit_index]

first_visit_visuospatial <- data_multiple_visit$visuospatial[first_visit_index]
second_visit_visuospatial <- data_multiple_visit$visuospatial[second_visit_index]
third_visit_visuospatial <- data_multiple_visit$visuospatial[third_visit_index]
fourth_visit_visuospatial <- data_multiple_visit$visuospatial[fourth_visit_index]

first_visit_executive_attention <- data_multiple_visit$executive_attention[first_visit_index]
second_visit_executive_attention <- data_multiple_visit$executive_attention[second_visit_index]
third_visit_executive_attention <- data_multiple_visit$executive_attention[third_visit_index]
fourth_visit_executive_attention <- data_multiple_visit$executive_attention[fourth_visit_index]

second_visit_memory_learning_change <- data_multiple_visit$memory_learning[second_visit_index] - data_multiple_visit$memory_learning[second_visit_index - 1]
third_visit_memory_learning_change <- data_multiple_visit$memory_learning[third_visit_index] - data_multiple_visit$memory_learning[third_visit_index - 1]
fourth_visit_memory_learning_change <- data_multiple_visit$memory_learning[fourth_visit_index] - data_multiple_visit$memory_learning[fourth_visit_index - 1]

second_visit_memory_retrieval_change <- data_multiple_visit$memory_retrieval[second_visit_index] - data_multiple_visit$memory_retrieval[second_visit_index - 1]
third_visit_memory_retrieval_change <- data_multiple_visit$memory_retrieval[third_visit_index] - data_multiple_visit$memory_retrieval[third_visit_index - 1]
fourth_visit_memory_retrieval_change <- data_multiple_visit$memory_retrieval[fourth_visit_index] - data_multiple_visit$memory_retrieval[fourth_visit_index - 1]

second_visit_language_change <- data_multiple_visit$language[second_visit_index] - data_multiple_visit$language[second_visit_index - 1]
third_visit_language_change <- data_multiple_visit$language[third_visit_index] - data_multiple_visit$language[third_visit_index - 1]
fourth_visit_language_change <- data_multiple_visit$language[fourth_visit_index] - data_multiple_visit$language[fourth_visit_index - 1]

second_visit_visuospatial_change <- data_multiple_visit$visuospatial[second_visit_index] - data_multiple_visit$visuospatial[second_visit_index - 1]
third_visit_visuospatial_change <- data_multiple_visit$visuospatial[third_visit_index] - data_multiple_visit$visuospatial[third_visit_index - 1]
fourth_visit_visuospatial_change <- data_multiple_visit$visuospatial[fourth_visit_index] - data_multiple_visit$visuospatial[fourth_visit_index - 1]

second_visit_executive_attention_change <- data_multiple_visit$executive_attention[second_visit_index] - data_multiple_visit$executive_attention[second_visit_index - 1]
third_visit_executive_attention_change <- data_multiple_visit$executive_attention[third_visit_index] - data_multiple_visit$executive_attention[third_visit_index - 1]
fourth_visit_executive_attention_change <- data_multiple_visit$executive_attention[fourth_visit_index] - data_multiple_visit$executive_attention[fourth_visit_index - 1]

second_memory_learning_change_over_time <- second_visit_memory_learning_change / second_visit_time_change
data_multiple_visit$second_memory_learning_change_over_time <- NA
data_multiple_visit$second_memory_learning_change_over_time[second_visit_index] <- second_memory_learning_change_over_time
mdl_second_memory_learning_change <- lm(second_memory_learning_change_over_time ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data_multiple_visit[second_visit_index]) 
summary(mdl_second_memory_learning_change) 

second_memory_retrieval_change_over_time <- second_visit_memory_retrieval_change / second_visit_time_change
data_multiple_visit$second_memory_retrieval_change_over_time <- NA
data_multiple_visit$second_memory_retrieval_change_over_time[second_visit_index] <- second_memory_retrieval_change_over_time
mdl_second_memory_retrieval_change <- lm(second_memory_retrieval_change_over_time ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data_multiple_visit[second_visit_index]) 
summary(mdl_second_memory_retrieval_change) 

second_language_change_over_time <- second_visit_language_change / second_visit_time_change
data_multiple_visit$second_language_change_over_time <- NA
data_multiple_visit$second_language_change_over_time[second_visit_index] <- second_language_change_over_time
mdl_second_language_change <- lm(second_language_change_over_time ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data_multiple_visit[second_visit_index]) 
summary(mdl_second_language_change) 

second_visuospatial_change_over_time <- second_visit_visuospatial_change / second_visit_time_change
data_multiple_visit$second_visuospatial_change_over_time <- NA
data_multiple_visit$second_visuospatial_change_over_time[second_visit_index] <- second_visuospatial_change_over_time
mdl_second_visuospatial_change <- lm(second_visuospatial_change_over_time ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data_multiple_visit[second_visit_index]) 
summary(mdl_second_visuospatial_change) 

second_executive_attention_change_over_time <- second_visit_executive_attention_change / second_visit_time_change
data_multiple_visit$second_executive_attention_change_over_time <- NA
data_multiple_visit$second_executive_attention_change_over_time[second_visit_index] <- second_executive_attention_change_over_time
mdl_second_executive_attention_change <- lm(second_executive_attention_change_over_time ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data_multiple_visit[second_visit_index]) 
summary(mdl_second_executive_attention_change) 

# AI Change Over Visits #######################################################################
# Average AI of Scans
first_visit_Hippocampus_AI <- data_multiple_visit$Abs_Hippocampus_AI[first_visit_index]
second_visit_Hippocampus_AI <- data_multiple_visit$Abs_Hippocampus_AI[second_visit_index]
third_visit_Hippocampus_AI <- data_multiple_visit$Abs_Hippocampus_AI[third_visit_index]
fourth_visit_Hippocampus_AI <- data_multiple_visit$Abs_Hippocampus_AI[fourth_visit_index]

average_first_visit_hippocampus_AI <- mean(first_visit_Hippocampus_AI, na.rm = TRUE)
average_second_visit_hippocampus_AI <- mean(second_visit_Hippocampus_AI, na.rm =TRUE)
average_third_visit_hippocampus_AI <- mean(third_visit_Hippocampus_AI, na.rm = TRUE)
average_fourth_visit_hippocampus_AI <- mean(fourth_visit_Hippocampus_AI, na.rm = TRUE)

average_visit_Hippocampus_AI <- data.frame("Relative_Visit" = c(1,2,3,4), "Average_Hippocampus_AI" = c(average_first_visit_hippocampus_AI, average_second_visit_hippocampus_AI, average_third_visit_hippocampus_AI, average_fourth_visit_hippocampus_AI))
plot(average_visit_Hippocampus_AI$Relative_Visit, average_visit_Hippocampus_AI$Average_Hippocampus_AI)

average_first_visit_DLPFC_AI <- mean(data_multiple_visit$Abs_DLPFC_AI[first_visit_index], na.rm = TRUE)
average_second_visit_DLPFC_AI <- mean(data_multiple_visit$Abs_DLPFC_AI[second_visit_index], na.rm =TRUE)
average_third_visit_DLPFC_AI <- mean(data_multiple_visit$Abs_DLPFC_AI[third_visit_index], na.rm = TRUE)
average_fourth_visit_DLPFC_AI <- mean(data_multiple_visit$Abs_DLPFC_AI[fourth_visit_index], na.rm = TRUE)

average_visit_DLPFC_AI <- data.frame("Relative_Visit" = c(1,2,3,4), "Average_DLPFC_AI" = c(average_first_visit_DLPFC_AI, average_second_visit_DLPFC_AI, average_third_visit_DLPFC_AI, average_fourth_visit_DLPFC_AI))
plot(average_visit_DLPFC_AI$Relative_Visit, average_visit_DLPFC_AI$Average_DLPFC_AI)

# Average AI change between scans
second_visit_Hippocampus_AI_change <- data_multiple_visit$Hippocampus_AI[second_visit_index] - data_multiple_visit$Hippocampus_AI[second_visit_index - 1]
third_visit_Hippocampus_AI_change <- data_multiple_visit$Hippocampus_AI[third_visit_index] - data_multiple_visit$Hippocampus_AI[third_visit_index - 1]
fourth_visit_Hippocampus_AI_change <- data_multiple_visit$Hippocampus_AI[fourth_visit_index] - data_multiple_visit$Hippocampus_AI[fourth_visit_index - 1]

average_second_visit_Hippocampus_AI_change <- mean(second_visit_Hippocampus_AI_change, na.rm = TRUE) 
average_third_visit_Hippocampus_AI_change <- mean(third_visit_Hippocampus_AI_change, na.rm = TRUE) 
average_fourth_visit_Hippocampus_AI_change <- mean(fourth_visit_Hippocampus_AI_change, na.rm = TRUE) 

average_visit_Hippocampus_AI_change <- data.frame("Relative_Visit" = c(2,3,4), "Average_Hippocampus_AI_change" = c(average_second_visit_Hippocampus_AI_change, average_third_visit_Hippocampus_AI_change, average_fourth_visit_Hippocampus_AI_change))
plot(average_visit_Hippocampus_AI_change$Relative_Visit, average_visit_Hippocampus_AI_change$Average_Hippocampus_AI_change)
#Rate of change does seem to be increasing (on average)

second_visit_DLPFC_AI_change <- data_multiple_visit$DLPFC_AI[second_visit_index] - data_multiple_visit$DLPFC_AI[second_visit_index - 1]
third_visit_DLPFC_AI_change <- data_multiple_visit$DLPFC_AI[third_visit_index] - data_multiple_visit$DLPFC_AI[third_visit_index - 1]
fourth_visit_DLPFC_AI_change <- data_multiple_visit$DLPFC_AI[fourth_visit_index] - data_multiple_visit$DLPFC_AI[fourth_visit_index - 1]

average_second_visit_DLPFC_AI_change <- mean(second_visit_DLPFC_AI_change, na.rm= TRUE) 
average_third_visit_DLPFC_AI_change <- mean(third_visit_DLPFC_AI_change, na.rm = TRUE) 
average_fourth_visit_DLPFC_AI_change <- mean(fourth_visit_DLPFC_AI_change, na.rm = TRUE) 

average_visit_DLPFC_AI_change <- data.frame("Relative_Visit" = c(2,3,4), "Average_DLPFC_AI_change" = c(average_second_visit_DLPFC_AI_change, average_third_visit_DLPFC_AI_change, average_fourth_visit_DLPFC_AI_change))
plot(average_visit_DLPFC_AI_change$Relative_Visit, average_visit_DLPFC_AI_change$Average_DLPFC_AI_change)
#No obvious pattern

# AI Change over Time ########################################################################################
second_Hippocampus_AI_change_over_time <- second_visit_Hippocampus_AI_change / second_visit_time_change
data_multiple_visit$second_Hippocampus_AI_change_over_time <- NA
data_multiple_visit$second_Hippocampus_AI_change_over_time[second_visit_index] <- second_Hippocampus_AI_change_over_time
average_second_Hippocampus_AI_change_over_time <- mean(second_Hippocampus_AI_change_over_time, na.rm = TRUE)

third_Hippocampus_AI_change_over_time <- third_visit_Hippocampus_AI_change / third_visit_time_change
data_multiple_visit$third_Hippocampus_AI_change_over_time <- NA
data_multiple_visit$third_Hippocampus_AI_change_over_time[third_visit_index] <- third_Hippocampus_AI_change_over_time
average_third_Hippocampus_AI_change_over_time <- mean(third_Hippocampus_AI_change_over_time, na.rm = TRUE)

first_to_third_visit_Hippocampus_AI_change <- data_multiple_visit$Hippocampus_AI[third_visit_index] - data_multiple_visit$Hippocampus_AI[third_visit_index - 2]
first_to_third_visit_time_change <- data_multiple_visit$Visit_Relative[third_visit_index] - data_multiple_visit$Visit_Relative[third_visit_index - 2]
first_to_third_Hippocampus_AI_change_over_time <-first_to_third_visit_Hippocampus_AI_change / first_to_third_visit_time_change
data_multiple_visit$first_to_third_Hippocampus_AI_change_over_time <- NA
data_multiple_visit$first_to_third_Hippocampus_AI_change_over_time[third_visit_index] <- first_to_third_visit_Hippocampus_AI_change
average_first_to_third_Hippocampus_AI_change_over_time <- mean(first_to_third_Hippocampus_AI_change_over_time, na.rm = TRUE)

fourth_Hippocampus_AI_change_over_time <- fourth_visit_Hippocampus_AI_change / fourth_visit_time_change
average_fourth_Hippocampus_AI_change_over_time <- mean(fourth_Hippocampus_AI_change_over_time, na.rm = TRUE)

subject_AI_over_time <- data.frame("Relative_Visit" = c(1,2,3), "AI" = c(first_visit_Hippocampus_AI[third_visit_index-2], second_visit_Hippocampus_AI[third_visit_index-1], third_visit_Hippocampus_AI[third_visit_index]))
plot(subject_AI_over_time$Relative_Visit, subject_AI_over_time$AI)

mdl_second_change_AI <- lm(second_Hippocampus_AI_change_over_time ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data_multiple_visit[second_visit_index]) 
summary(mdl_second_change_AI) 

mdl_third_change_AI <- lm(third_Hippocampus_AI_change_over_time ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data_multiple_visit[third_visit_index]) 
summary(mdl_third_change_AI)

mdl_first_to_third_change_AI <- lm(first_to_third_Hippocampus_AI_change_over_time ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data_multiple_visit[third_visit_index]) 
summary(mdl_first_to_third_change_AI)

# Global Despression Change: 

# Second derivatives
data_multiple_visit$AI_second_derivative <- NA
AI_second_derivative <- data_multiple_visit$third_Hippocampus_AI_change_over_time[third_visit_index] - data_multiple_visit$second_Hippocampus_AI_change_over_time[third_visit_index - 1]
data_multiple_visit$AI_second_derivative[third_visit_index] <- AI_second_derivative

mdl_second_derivative_AI <- lm(AI_second_derivative ~ Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE, data = data_multiple_visit[third_visit_index]) 
summary(mdl_second_derivative_AI)

#FWHM Change Over Time

