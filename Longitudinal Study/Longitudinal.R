rm(list=ls())
dev.off()
pacman::p_load(pacman, rio)
# IMPORTING Data ###########################################################
data <- import("/Users/jinghangli/Desktop/Pitt Summer 2020/KLU_APC2_Master_2020_07_24_AS.xlsx")
activation <- import("/Users/jinghangli/Documents/GitHub/KLU_APC2_Jinghang_Mountz_7_22_2020/Appending_to_Master/activ_values.txt")
AI <- import("/Users/jinghangli/Documents/GitHub/KLU_APC2_Jinghang_Mountz_7_22_2020/Appending_to_Master/AI.txt")
FWHM <- import("/Users/jinghangli/Documents/GitHub/KLU_APC2_Jinghang_Mountz_7_22_2020/Appending_to_Master/FWHM.txt")
FWHM <- abs(FWHM)
# Filter Data ##############################################################
n_occur <- data.frame(table(data$Vault_UID)) #getting the subject ID and the corresponding recurrence 
data_multiple_visit <- data[data$Vault_UID %in% n_occur$Var1[n_occur$Freq > 1],] #getting the data with the multiple fMRI scans

list <- match(activation$Scan_ID,data_multiple_visit$Vault_Scan_ID)
index <- which(list!=0,arr.ind = T)
list <- na.omit(match(activation$Scan_ID, data_multiple_visit$Vault_Scan_ID))

#creating new variables that are going to be appended
data_multiple_visit$Subject_ID <- NA
data_multiple_visit$Scan_ID <- NA
data_multiple_visit$Left_Hippocampus_Activation <- NA
data_multiple_visit$Right_Hippocampus_Activation <- NA
data_multiple_visit$Left_DLPFC_Activation <- NA
data_multiple_visit$Right_DLPFC_Activation <- NA
data_multiple_visit$Hippocampus_AI <- NA
data_multiple_visit$DLPFC_AI <-NA
data_multiple_visit$Left_Hippocampus_FWHM <- NA
data_multiple_visit$Right_Hippocampus_FWHM <- NA
data_multiple_visit$Left_DLPFC_FWHM <- NA
data_multiple_visit$Right_DLPFC_FWHM <- NA

#appending
data_multiple_visit$Subject_ID[list] <- activation$Subject_ID[index]
data_multiple_visit$Scan_ID[list] <- activation$Scan_ID[index]
data_multiple_visit$Left_Hippocampus_Activation[list] <- activation[,3][index]
data_multiple_visit$Right_Hippocampus_Activation[list] <- activation[,4][index]
data_multiple_visit$Left_DLPFC_Activation[list] <- activation[,5][index]
data_multiple_visit$Right_DLPFC_Activation[list] <- activation[,6][index]
data_multiple_visit$Hippocampus_AI[list] <- AI[,3][index]
data_multiple_visit$DLPFC_AI[list] <- AI[,4][index]

data_multiple_visit$Left_Hippocampus_FWHM[list] <- FWHM[,3][index]
data_multiple_visit$Right_Hippocampus_FWHM[list] <- FWHM[,4][index]
data_multiple_visit$Left_DLPFC_FWHM[list] <- FWHM[,5][index]
data_multiple_visit$Right_DLPFC_FWHM[list] <- FWHM[,6][index]
# Recode Variables ##############################################################
data_multiple_visit$Race[data_multiple_visit$Race == "NaN"] = NA
data_multiple_visit$Race_cat <- data_multiple_visit$Race != 'White' #non-white = TRUE
data_multiple_visit$Education_cat <- data_multiple_visit$Education > 12  #higher education = True
data_multiple_visit$Sex[data_multiple_visit$Sex == "NaN"] = NA
data_multiple_visit$Sex_cat <- (data_multiple_visit$Sex == 'Male') #TRUE = male
data_multiple_visit$PiBStatus_SUVR_GTM_FS_Global[data_multiple_visit$PiBStatus_SUVR_GTM_FS_Global == "NaN"] = NA
data_multiple_visit$PiB_STATUS_CODE <- (data_multiple_visit$PiBStatus_SUVR_GTM_FS_Global == "pos") #positive = TRUE
data_multiple_visit$APOE_CODE[data_multiple_visit$APOE_CODE == "NaN"] = NA
data_multiple_visit$APOE_STATUS_CODE <- data_multiple_visit$APOE_CODE == "At Least One E4 Allele" #E4 allele = TRUE
data_multiple_visit$FaceName_PostScanAccuracy[data_multiple_visit$FaceName_PostScanAccuracy == "NA"] <- NA
data_multiple_visit$FaceName_PostScanAccuracy <- as.numeric(data_multiple_visit$FaceName_PostScanAccuracy)
data_multiple_visit$Abs_Hippocampus_AI <- abs(data_multiple_visit$Hippocampus_AI)
data_multiple_visit$Abs_DLPFC_AI <- abs(data_multiple_visit$DLPFC_AI)
data_multiple_visit$LETTER_FLUENCY <- (data_multiple_visit$FLUENA + data_multiple_visit$FLUENF+ data_multiple_visit$FLUENS) / 3
data_multiple_visit$WREC_TOT <- (data_multiple_visit$WREC + data_multiple_visit$WREC2 + data_multiple_visit$WREC3)
Pred_STRCW <- (data_multiple_visit$STRCOL*data_multiple_visit$STRWRD) / (data_multiple_visit$STRCOL+data_multiple_visit$STRWRD) #doi: 10.3389/fpsyg.2017.00557 - Stroop Interference Score
data_multiple_visit$STRINTERFERENCE <- data_multiple_visit$STRCW - Pred_STRCW

# Cognitive Domain - Z Transform ####################################################################
#Negative z value means that lower value = higher performance
# doi:10.1016/j.jalz.2017.12.003 - method of composite calculation
#doi/ 10.1136/jnnp.2004.045567 - standard deviations from normative data
CLOCKD_Z <- (data_multiple_visit$CLOCKD - mean(data_multiple_visit$CLOCKD, na.rm = TRUE)) / sd(data_multiple_visit$CLOCKD, na.rm = TRUE)
BLOCKDES_Z <- (data_multiple_visit$BLOCKDES - mean(data_multiple_visit$BLOCKDES, na.rm = TRUE)) / sd(data_multiple_visit$BLOCKDES, na.rm = TRUE)
BNT60TOT_Z <- (data_multiple_visit$BNT60TOT - mean(data_multiple_visit$BNT60TOT, na.rm = TRUE)) / sd(data_multiple_visit$BLOCKDE, na.rm = TRUE)
REYCO_Z <- (data_multiple_visit$REYCO - mean(data_multiple_visit$REYCO, na.rm = TRUE)) / sd (data_multiple_visit$REYCO, na.rm = TRUE)
REYIM_Z <- (data_multiple_visit$REYIM-mean(data_multiple_visit$REYIM, na.rm = TRUE)) / sd(data_multiple_visit$REYIM, na.rm = TRUE)
REYDE_Z <- (data_multiple_visit$REYDE - mean(data_multiple_visit$REYDE, na.rm = TRUE)) / sd(data_multiple_visit$REYDE, na.rm = TRUE)
FLUEN_Z <- (data_multiple_visit$FLUEN - mean(data_multiple_visit$FLUEN, na.rm = TRUE)) / sd(data_multiple_visit$FLUEN, na.rm = TRUE)
LETTER_FLUENCY_Z <- (data_multiple_visit$LETTER_FLUENCY - mean(data_multiple_visit$LETTER_FLUENCY, na.rm = TRUE)) / sd(data_multiple_visit$LETTER_FLUENCY, na.rm=TRUE)
WREC_TOT_Z <- (data_multiple_visit$WREC_TOT - mean(data_multiple_visit$WREC_TOT, na.rm = TRUE)) / sd(data_multiple_visit$WREC_TOT, na.rm =TRUE)
WRECDE_Z <- (data_multiple_visit$WRECDE - mean(data_multiple_visit$WRECDE, na.rm = TRUE)) / sd(data_multiple_visit$WRECDE, na.rm = TRUE)
SPANSF_Z <- (data_multiple_visit$SPANSF - mean(data_multiple_visit$SPANSF, na.rm = TRUE)) / sd(data_multiple_visit$SPANSF, na.rm = TRUE)
SPANSB_Z <- (data_multiple_visit$SPANSB - mean(data_multiple_visit$SPANSB, na.rm = TRUE)) / sd(data_multiple_visit$SPANSB, na.rm = TRUE)
TRAILAS_Z <- (data_multiple_visit$TRAILAS - mean(data_multiple_visit$TRAILAS, na.rm = TRUE)) / sd(data_multiple_visit$TRAILAS, na.rm = TRUE)
TRAILBS_Z <- (data_multiple_visit$TRAILBS - mean(data_multiple_visit$TRAILBS, na.rm = TRUE)) / sd(data_multiple_visit$TRAILBS, na.rm= TRUE)
LMIAIMM_Z <- (data_multiple_visit$LMIAIMM - mean(data_multiple_visit$LMIAIMM, na.rm = TRUE)) /sd(data_multiple_visit$LMIAIMM, na.rm = TRUE)
LMIIADEL_Z <- (data_multiple_visit$LMIIADEL - mean(data_multiple_visit$LMIIADEL, na.rm = TRUE))/sd(data_multiple_visit$LMIIADEL, na.rm = TRUE)
DIGSYMWR_Z <- (data_multiple_visit$DIGSYMWR - mean(data_multiple_visit$DIGSYMWR, na.rm = TRUE))/sd(data_multiple_visit$DIGSYMWR, na.rm = TRUE)
STRINTERFERENCE_Z <- (data_multiple_visit$STRINTERFERENCE - mean(data_multiple_visit$STRINTERFERENCE, na.rm = TRUE))/ sd(data_multiple_visit$STRINTERFERENCE, na.rm = TRUE)
TRAILAS_Z_INV <- -1 * TRAILAS_Z
TRAILBS_Z_INV <- -1 * TRAILBS_Z
# Domain Scores #########################################################################
#doi:10.1016/j.jalz.2017.12.003., doi:10.1080/13607860903071014. (Both Beth Snitz articles), https://www.ncbi.nlm.nih.gov/books/NBK285344/ - for SPANSB in Executive
data_multiple_visit$memory_learning <- (LMIAIMM_Z + REYIM_Z + WREC_TOT_Z) / 3
data_multiple_visit$memory_retrieval <- (LMIIADEL_Z + REYDE_Z + WRECDE_Z) / 3
data_multiple_visit$visuospatial <- (BLOCKDES_Z + REYCO_Z) / 2
data_multiple_visit$language <- (FLUEN_Z + LETTER_FLUENCY_Z + BNT60TOT_Z) / 3
data_multiple_visit$executive_attention <- (TRAILAS_Z_INV + TRAILBS_Z_INV + CLOCKD_Z + DIGSYMWR_Z + STRINTERFERENCE_Z + SPANSF_Z + SPANSB_Z) / 7
# Time series data visualization (cognitive domains)
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$executive_attention)
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$language)
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$visuospatial)
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$memory_retrieval)
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$memory_learning)
# Time series data visulization
bi <- data_multiple_visit$Visit_Relative == 1
pre_ind <- which (bi == TRUE)
las_ind <- pre_ind -1
las_ind <- c(tail(las_ind,-1),length(data_multiple_visit$Visit_Relative))
PiB_slope <- vector("numeric", 100)
tracefunction <- function(val) {
  # Time series with activation
  plot(data_multiple_visit$Visit_Relative, val)
  points(data_multiple_visit$Visit_Relative[data_multiple_visit$PiB_STATUS_CODE == TRUE],
         val[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")
  title(main = "", ylab = "", xlab = "")
  bi <- data_multiple_visit$Visit_Relative == 1
  pre_ind <- which (bi == TRUE)
  las_ind <- pre_ind -1
  las_ind <- c(tail(las_ind,-1),length(data_multiple_visit$Visit_Relative))
  for (i in 1:sum(n_occur$Freq >1)){
    # a <- pre_ind[i]
    # b <- las_ind[i]
    # print(c(a,b))
    a <- pre_ind[i]
    b <- las_ind[i]
    x <- c(data_multiple_visit[a:b,]$Visit_Relative)
    y <- c(val[a:b])
    # lines(x,y)
    if( is.na(y[1]) == TRUE){
      y[1] <- 0
    }
    if(sum(y[1] == na.omit(val[data_multiple_visit$PiB_STATUS_CODE == TRUE])) == 1){
      lines(x,y,col = 'red', lwd = 2)
    }
    else{
      lines(x,y,col = 'gray', lwd = 2)
    }
  }
}
tracefunction(data_multiple_visit$Left_Hippocampus_FWHM)
title(main = "Left Hippocampus FWHM trajectory")
tracefunction(data_multiple_visit$Right_Hippocampus_FWHM)
title(main = "Right Hippocampus FWHM trajectory")
tracefunction(data_multiple_visit$Left_DLPFC_FWHM)
title(main = "Left DLPFC FWHM trajectory")
tracefunction(data_multiple_visit$Right_DLPFC_FWHM)
title(main = "Right DLPFC FWHM trajectory")
tracefunction(data_multiple_visit$Left_Hippocampus_Activation)
title(main = "Left Hippocampus Activation trajectory")
tracefunction(data_multiple_visit$Right_Hippocampus_Activation)
title(main = "Right Hippocampus Activation trajectory")
tracefunction(data_multiple_visit$Left_DLPFC_Activation)
title(main = "Left DLPFC Activation trajectory")
tracefunction(data_multiple_visit$Right_DLPFC_Activation)
title(main = "Right DLPFC Activation trajectory")
tracefunction(data_multiple_visit$Abs_DLPFC_AI)
title(main = "DLPFC Absolute Asymmetry Index trajectory")
tracefunction(data_multiple_visit$Abs_Hippocampus_AI)
title(main = "Hippocampus Absolute Asymmetry Index trajectory")
tracefunction(data_multiple_visit$DLPFC_AI)
tracefunction(data_multiple_visit$Hippocampus_AI)
# Cognition
tracefunction(data_multiple_visit$memory_learning)
title(main = "Memory learning score trajectory")
tracefunction(data_multiple_visit$memory_retrieval)
title(main = "Memory retrieval score trajectory")
tracefunction(data_multiple_visit$language)
title(main = "Language score trajectory")
tracefunction(data_multiple_visit$executive_attention)
title(main = "Executive attention score trajectory")
tracefunction(data_multiple_visit$visuospatial)
title(main = "Visuosaptial score trajectory")








# FWHM ############################
# Left Hippocampus 
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$Left_Hippocampus_FWHM)
pib_mdl <- lm(data_multiple_visit$Left_Hippocampus_FWHM[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$Left_Hippocampus_FWHM[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Visit_Relative[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$Left_Hippocampus_FWHM[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")

# Right** Hippocampus
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$Right_Hippocampus_FWHM)
pib_mdl <- lm(data_multiple_visit$Right_Hippocampus_FWHM[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$Right_Hippocampus_FWHM[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Visit_Relative[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$Right_Hippocampus_FWHM[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")

# Right DLPFC
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$Right_DLPFC_FWHM)
pib_mdl <- lm(data_multiple_visit$Right_DLPFC_FWHM[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$Right_DLPFC_FWHM[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Visit_Relative[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$Right_DLPFC_FWHM[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")

# Left DLPFC
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$Left_DLPFC_FWHM)
pib_mdl <- lm(data_multiple_visit$Left_DLPFC_FWHM[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$Left_DLPFC_FWHM[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Visit_Relative[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$Left_DLPFC_FWHM[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")
##############################################

# Activation ############################
# Left Hippocampus 
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$Left_Hippocampus_Activation)
pib_mdl <- lm(data_multiple_visit$Left_Hippocampus_Activation[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$Left_Hippocampus_Activation[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Visit_Relative[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$Left_Hippocampus_Activation[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")

# Right Hippocampus
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$Right_Hippocampus_Activation)
pib_mdl <- lm(data_multiple_visit$Right_Hippocampus_Activation[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$Right_Hippocampus_Activation[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Visit_Relative[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$Right_Hippocampus_Activation[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")

#** Right DLPFC
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$Right_DLPFC_Activation)
pib_mdl <- lm(data_multiple_visit$Right_DLPFC_Activation[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$Right_DLPFC_Activation[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Visit_Relative[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$Right_DLPFC_Activation[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")
title(main = "Right DLPFC Activation Trajectory")


# Left DLPFC
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$Left_DLPFC_Activation)
pib_mdl <- lm(data_multiple_visit$Left_DLPFC_Activation[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$Left_DLPFC_Activation[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Visit_Relative[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$Left_DLPFC_Activation[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")
##############################################

# Cognition ############################
# memory_learning
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$memory_learning)
pib_mdl <- lm(data_multiple_visit$memory_learning[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$memory_learning[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Visit_Relative[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$memory_learning[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")

# memory_retrieval
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$memory_retrieval)
pib_mdl <- lm(data_multiple_visit$memory_retrieval[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$memory_retrieval[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Visit_Relative[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$memory_retrieval[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")

# visuospatial
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$visuospatial)
pib_mdl <- lm(data_multiple_visit$visuospatial[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$visuospatial[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Visit_Relative[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$visuospatial[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")

#** language
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$language)
pib_mdl <- lm(data_multiple_visit$language[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$language[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Visit_Relative[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$language[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")
title(main = "Language Domain Decline Trajectory")

#** executive_attention
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$executive_attention)
pib_mdl <- lm(data_multiple_visit$executive_attention[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$executive_attention[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Visit_Relative[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$executive_attention[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")
title(main = "Executive_Attention Domain Decline Trajectory")
##############################################

#Abs Hippocampus AI#############################################
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$Abs_Hippocampus_AI)
pib_mdl <- lm(data_multiple_visit$Abs_Hippocampus_AI[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$Abs_Hippocampus_AI[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Visit_Relative[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$Abs_Hippocampus_AI[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")

#Abs DLPFC AI#############################################
plot(data_multiple_visit$Visit_Relative, data_multiple_visit$Abs_DLPFC_AI)
pib_mdl <- lm(data_multiple_visit$Abs_DLPFC_AI[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$Abs_DLPFC_AI[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Visit_Relative[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Visit_Relative[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$Abs_DLPFC_AI[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")
##############################################


#** executive_attention vs Right Hippocampus FWHM
plot(data_multiple_visit$Right_Hippocampus_FWHM, data_multiple_visit$executive_attention)
pib_mdl <- lm(data_multiple_visit$executive_attention[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Right_Hippocampus_FWHM[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$executive_attention[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Right_Hippocampus_FWHM[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Right_Hippocampus_FWHM[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$executive_attention[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")
title(main = "Executive_Attention vs Right Hippocampus FWHM")

#** executive_attention vs Right dlpfc activation
plot(data_multiple_visit$Right_DLPFC_Activation, data_multiple_visit$executive_attention)
pib_mdl <- lm(data_multiple_visit$executive_attention[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Right_DLPFC_Activation[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$executive_attention[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Right_DLPFC_Activation[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Right_DLPFC_Activation[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$executive_attention[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")
title(main = "Executive_Attention vs Right DLPFC Activation")

#** language vs Left dlpfc activation
plot(data_multiple_visit$Left_DLPFC_Activation, data_multiple_visit$language)
pib_mdl <- lm(data_multiple_visit$language[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Left_DLPFC_Activation[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$language[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Left_DLPFC_Activation[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Left_DLPFC_Activation[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$language[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")
title(main = "Language vs Left DLPFC Activation")

#** language vs Left hippocampus fwhm
plot(data_multiple_visit$Left_Hippocampus_FWHM, data_multiple_visit$language)
pib_mdl <- lm(data_multiple_visit$language[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Left_Hippocampus_FWHM[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$language[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Left_Hippocampus_FWHM[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Left_Hippocampus_FWHM[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$language[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")
title(main = "Language vs Left Hippocampus FWHM")

#Executive Attention vs abs hippocampus AI
plot(data_multiple_visit$Abs_Hippocampus_AI,data_multiple_visit$executive_attention)
pib_mdl <- lm(data_multiple_visit$executive_attention[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Abs_Hippocampus_AI[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$executive_attention[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Abs_Hippocampus_AI[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Abs_Hippocampus_AI[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$executive_attention[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")
title(main = "Executive Attention vs hippocampus AI")

#** Executive Attention vs abs dlpfc AI
plot(data_multiple_visit$Abs_DLPFC_AI,data_multiple_visit$executive_attention)
pib_mdl <- lm(data_multiple_visit$executive_attention[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Abs_DLPFC_AI[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$executive_attention[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Abs_DLPFC_AI[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Abs_DLPFC_AI[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$executive_attention[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")
title(main = "Executive Attention vs DLPFC AI")

#** Language vs abs dlpfc AI
plot(data_multiple_visit$Abs_DLPFC_AI,data_multiple_visit$language)
pib_mdl <- lm(data_multiple_visit$language[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Abs_DLPFC_AI[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$language[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Abs_DLPFC_AI[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Abs_DLPFC_AI[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$language[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")
title(main = "language vs dlpfc AI")

#** Language vs abs Hippocampus AI
plot(data_multiple_visit$Abs_Hippocampus_AI,data_multiple_visit$language)
pib_mdl <- lm(data_multiple_visit$language[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)] ~ 
                data_multiple_visit$Abs_Hippocampus_AI[which(data_multiple_visit$PiB_STATUS_CODE == TRUE)], data = data_multiple_visit)
abline(pib_mdl, col='red')
mdl <- lm(data_multiple_visit$language[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)] ~ 
            data_multiple_visit$Abs_Hippocampus_AI[which(data_multiple_visit$PiB_STATUS_CODE == FALSE)], data = data_multiple_visit)
abline(mdl, col='black')
points(data_multiple_visit$Abs_Hippocampus_AI[data_multiple_visit$PiB_STATUS_CODE == TRUE],
       data_multiple_visit$language[data_multiple_visit$PiB_STATUS_CODE == TRUE], pch = 19, cex =1, col = "red")
title(main = "language vs Hippocampus AI")



rate_of_change <- function(val2, val1){
  rate <- (val2 - val1)/data_multiple_visit$Visit_Relative[las_ind]
  return(rate)
}
#rate of change left hippocampus fwhm
val2_l_hippo_fwhm <- data_multiple_visit$Left_Hippocampus_FWHM[las_ind]
val1_l_hippo_fwhm <- data_multiple_visit$Left_Hippocampus_FWHM[pre_ind]

val2_abs_hippo_AI <- data_multiple_visit$Abs_Hippocampus_AI[las_ind]
val1_abs_hippo_AI <- data_multiple_visit$Abs_Hippocampus_AI[pre_ind]

rate_l_hippocampus_fhwm <- rate_of_change(val2_l_hippo_fwhm,val1_l_hippo_fwhm)
rate_abs_hippocampus_AI <- rate_of_change(val2_abs_hippo_AI,val1_abs_hippo_AI)

data_multiple_visit$rate_l_hippocampus_fwhm <- NA
data_multiple_visit$rate_abs_hippocampus_AI <- NA
data_multiple_visit$rate_l_hippocampus_fwhm[pre_ind] <-rate_l_hippocampus_fhwm
data_multiple_visit$rate_abs_hippocampus_AI[pre_ind] <-rate_abs_hippocampus_AI

data1 <- data_multiple_visit[which(data_multiple_visit$Visit_Relative == 1),]
#Regression FWHM#######################################################################
mdl_left_hippocampus_FWHM <- lm(Left_Hippocampus_FWHM ~ Visit_Relative + Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE
                                +Visit_Relative*FDG_SUVR_GTM_FS_Global+Visit_Relative*PiB_STATUS_CODE+Visit_Relative*APOE_STATUS_CODE, data = data_multiple_visit)
summary(mdl_left_hippocampus_FWHM)

mdl_right_hippocampus_FWHM <- lm(Right_Hippocampus_FWHM ~ Visit_Relative + Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE
                                +Visit_Relative*FDG_SUVR_GTM_FS_Global+Visit_Relative*PiB_STATUS_CODE+Visit_Relative*APOE_STATUS_CODE, data = data_multiple_visit)
summary(mdl_right_hippocampus_FWHM)

mdl_left_dlpfc_FWHM <- lm(Left_DLPFC_FWHM ~ Visit_Relative + Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE
                                +Visit_Relative*FDG_SUVR_GTM_FS_Global+Visit_Relative*PiB_STATUS_CODE+Visit_Relative*APOE_STATUS_CODE, data = data_multiple_visit)
summary(mdl_left_dlpfc_FWHM)

mdl_right_dlpfc_FWHM <- lm(Right_DLPFC_FWHM ~ Visit_Relative + Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE
                                 +Visit_Relative*FDG_SUVR_GTM_FS_Global+Visit_Relative*PiB_STATUS_CODE+Visit_Relative*APOE_STATUS_CODE, data = data_multiple_visit)
summary(mdl_right_dlpfc_FWHM)
#Regression Activation #######################################################################
mdl_left_hippocampus_activation <- lm(Left_Hippocampus_Activation ~ Visit_Relative + Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE
                                +Visit_Relative*FDG_SUVR_GTM_FS_Global+Visit_Relative*PiB_STATUS_CODE+Visit_Relative*APOE_STATUS_CODE, data = data_multiple_visit)
summary(mdl_left_hippocampus_activation)

mdl_right_hippocampus_activation <- lm(Right_Hippocampus_Activation ~ Visit_Relative + Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE
                                 +Visit_Relative*FDG_SUVR_GTM_FS_Global+Visit_Relative*PiB_STATUS_CODE+Visit_Relative*APOE_STATUS_CODE, data = data_multiple_visit)
summary(mdl_right_hippocampus_activation)

mdl_left_dlpfc_activation <- lm(Left_DLPFC_Activation ~ Visit_Relative + Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE
                          +Visit_Relative*FDG_SUVR_GTM_FS_Global+Visit_Relative*PiB_STATUS_CODE+Visit_Relative*APOE_STATUS_CODE, data = data_multiple_visit)
summary(mdl_left_dlpfc_activation)

mdl_right_dlpfc_activation <- lm(Right_DLPFC_Activation ~ Visit_Relative + Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE
                           +Visit_Relative*FDG_SUVR_GTM_FS_Global+Visit_Relative*PiB_STATUS_CODE+Visit_Relative*APOE_STATUS_CODE, data = data_multiple_visit)
summary(mdl_right_dlpfc_activation)

#Regression abs AI #######################################################################
mdl_abs_hippocampus_AI <- lm(Abs_Hippocampus_AI ~ Visit_Relative + Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE
                                      +Visit_Relative*FDG_SUVR_GTM_FS_Global+Visit_Relative*PiB_STATUS_CODE+Visit_Relative*APOE_STATUS_CODE, data = data_multiple_visit)
summary(mdl_abs_hippocampus_AI)

mdl_abs_dlpfc_AI <- lm(Abs_DLPFC_AI ~ Visit_Relative + Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE
                             +Visit_Relative*FDG_SUVR_GTM_FS_Global+Visit_Relative*PiB_STATUS_CODE+Visit_Relative*APOE_STATUS_CODE, data = data_multiple_visit)
summary(mdl_abs_dlpfc_AI)

#Regression Cognition #######################################################################
mdl_memory_learning <- lm(memory_learning ~ Visit_Relative + Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE
                             +Visit_Relative*FDG_SUVR_GTM_FS_Global+Visit_Relative*PiB_STATUS_CODE+Visit_Relative*APOE_STATUS_CODE, data = data_multiple_visit)
summary(mdl_memory_learning)

mdl_memory_retrieval <- lm(memory_retrieval ~ Visit_Relative + Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE
                       +Visit_Relative*FDG_SUVR_GTM_FS_Global+Visit_Relative*PiB_STATUS_CODE+Visit_Relative*APOE_STATUS_CODE, data = data_multiple_visit)
summary(mdl_memory_retrieval)
#7.738e-05**
mdl_visuospatial <- lm(visuospatial ~ Visit_Relative + Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE
                           +Visit_Relative*FDG_SUVR_GTM_FS_Global+Visit_Relative*PiB_STATUS_CODE+Visit_Relative*APOE_STATUS_CODE, data = data_multiple_visit)
summary(mdl_visuospatial)

mdl_language <- lm(language ~ Visit_Relative + Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE
                       +Visit_Relative*FDG_SUVR_GTM_FS_Global+Visit_Relative*PiB_STATUS_CODE+Visit_Relative*APOE_STATUS_CODE, data = data_multiple_visit)
summary(mdl_language)
#8.212e-06**
mdl_executive_attention<- lm(executive_attention ~ Visit_Relative + Age_CurrentVisit+Sex_cat+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_STATUS_CODE
                   +Visit_Relative*FDG_SUVR_GTM_FS_Global+Visit_Relative*PiB_STATUS_CODE+Visit_Relative*APOE_STATUS_CODE, data = data_multiple_visit)
summary(mdl_executive_attention)


# list <- data.frame(data_multiple_visit$Vault_UID,data_multiple_visit$Vault_Scan_ID) #getting the subject and scan ID of the participants that have multiple fMRI scans
# write.table(list, "list.txt",sep="/",row.names = FALSE,col.names = FALSE) #exporting the list to txt file
