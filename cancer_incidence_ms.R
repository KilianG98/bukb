rm(list=ls())

library(tidyverse)
library(data.table)

# load data ---------------------------------------------------------------

setwd("C:/Users/kiliang98/ucloud/Shared/projects_KR/Bilirubin/data/cancer_ms")
cancer_df <- read.csv("cancer_data_raw.csv")


# Cancer incidence --------------------------------------------------------

# Data cleaning - Outcome
# updated Version with new cancer data from Scotland
# Authors: Andrea Weber and Anja Sedlmeier
# Adjustements: Michael Stein

# Adjust
head(cancer_df)
names(cancer_df) <- gsub("^X", "f.", names(cancer_df)) # change to old UKB standard to keep code working (counts number of elements in colnames)

cancer_df$f.53.0.0 <- as.Date(cancer_df$f.53.0.0)
cancer_df$f.191.0.0 <- as.Date(cancer_df$f.191.0.0)
cancer_df$f.40000.0.0 <- as.Date(cancer_df$f.40000.0.0)
cancer_df$f.40005.0.0 <- as.Date(cancer_df$f.40005.0.0)
cancer_df$f.40005.1.0 <- as.Date(cancer_df$f.40005.1.0)
cancer_df$f.40005.2.0 <- as.Date(cancer_df$f.40005.2.0)
cancer_df$f.40005.3.0 <- as.Date(cancer_df$f.40005.3.0)
cancer_df$f.40005.4.0 <- as.Date(cancer_df$f.40005.4.0)
cancer_df$f.40005.5.0 <- as.Date(cancer_df$f.40005.5.0)
cancer_df$f.40005.6.0 <- as.Date(cancer_df$f.40005.6.0)
cancer_df$f.40005.7.0 <- as.Date(cancer_df$f.40005.7.0)
cancer_df$f.40005.8.0 <- as.Date(cancer_df$f.40005.8.0)
cancer_df$f.40005.9.0 <- as.Date(cancer_df$f.40005.9.0)
cancer_df$f.40005.10.0 <- as.Date(cancer_df$f.40005.10.0)
cancer_df$f.40005.11.0 <- as.Date(cancer_df$f.40005.11.0)
cancer_df$f.40005.12.0 <- as.Date(cancer_df$f.40005.12.0)
cancer_df$f.40005.13.0 <- as.Date(cancer_df$f.40005.13.0)
cancer_df$f.40005.14.0 <- as.Date(cancer_df$f.40005.14.0)
cancer_df$f.40005.15.0 <- as.Date(cancer_df$f.40005.15.0)
cancer_df$f.40005.16.0 <- as.Date(cancer_df$f.40005.16.0)
cancer_df$f.40005.17.0 <- as.Date(cancer_df$f.40005.17.0)
cancer_df$f.40005.18.0 <- as.Date(cancer_df$f.40005.18.0)
cancer_df$f.40005.19.0 <- as.Date(cancer_df$f.40005.19.0)
cancer_df$f.40005.20.0 <- as.Date(cancer_df$f.40005.20.0)
cancer_df$f.40005.21.0 <- as.Date(cancer_df$f.40005.21.0)

# Subsets for data preparation --------------------------------------------
# Date of Cancer Diagnosis: f.40005.0.0 - "f.40005.21.0" (for every occurence one column)
ca_date <- cancer_df[ , c("eid","f.40005.0.0","f.40005.1.0","f.40005.2.0","f.40005.3.0", 
                          "f.40005.4.0","f.40005.5.0","f.40005.6.0","f.40005.7.0","f.40005.8.0","f.40005.9.0","f.40005.10.0",
                          "f.40005.11.0", "f.40005.12.0", "f.40005.13.0", "f.40005.14.0", "f.40005.15.0", "f.40005.16.0",
                          "f.40005.17.0", "f.40005.18.0", "f.40005.19.0", "f.40005.20.0", "f.40005.21.0"
)]

# Type of Cancer ICD9: f.40013.0.0 - f.40013.13.0
ca_icd9 <- cancer_df[ , c("eid","f.40013.0.0", "f.40013.1.0", "f.40013.2.0","f.40013.3.0",
                          "f.40013.4.0", "f.40013.5.0", "f.40013.6.0", "f.40013.7.0", "f.40013.8.0", "f.40013.9.0",
                          "f.40013.10.0", "f.40013.11.0", "f.40013.12.0", "f.40013.13.0")]

# Type of Cancer ICD10: f.40006.0.0 - f.40006.21.0
ca_icd10 <- cancer_df[ , c("eid", "f.40006.0.0", "f.40006.1.0", "f.40006.2.0", "f.40006.3.0",
                           "f.40006.4.0", "f.40006.5.0", "f.40006.6.0", "f.40006.7.0", "f.40006.8.0", "f.40006.9.0",
                           "f.40006.10.0", "f.40006.11.0", "f.40006.12.0", "f.40006.13.0", "f.40006.14.0", "f.40006.15.0",
                           "f.40006.16.0","f.40006.17.0","f.40006.18.0","f.40006.19.0","f.40006.20.0","f.40006.21.0"
)]

# Behaviour of Cancer Tumour: f.40012.0.0 - f.40012.21.0
ca_bhv <- cancer_df[ , c("eid",  "f.40012.0.0", "f.40012.1.0", "f.40012.2.0", "f.40012.3.0",
                         "f.40012.4.0" , "f.40012.5.0" , "f.40012.6.0", "f.40012.7.0", "f.40012.8.0", "f.40012.9.0", 
                         "f.40012.10.0", "f.40012.11.0", "f.40012.12.0", "f.40012.13.0", "f.40012.14.0","f.40012.15.0",
                         "f.40012.16.0","f.40012.17.0","f.40012.18.0","f.40012.19.0","f.40012.20.0","f.40012.21.0")]

# Histology of Cancer Tumour: f.40011.0.0 - f.40011.21.0
ca_hist <- cancer_df[ , c("eid",
                          "f.40011.0.0",  "f.40011.1.0", "f.40011.2.0", "f.40011.3.0", "f.40011.4.0",
                          "f.40011.5.0",  "f.40011.6.0", "f.40011.7.0", "f.40011.8.0", "f.40011.9.0","f.40011.10.0", "f.40011.11.0",
                          "f.40011.12.0", "f.40011.13.0","f.40011.14.0","f.40011.15.0","f.40011.16.0",
                          "f.40011.17.0", "f.40011.18.0","f.40011.19.0","f.40011.20.0","f.40011.21.0"
)]

# Age at Cancer Diagnosis: f.40008.0.0 - f.40008.21.0
ca_age <- cancer_df[ , c("eid", "f.40008.0.0", "f.40008.1.0", "f.40008.2.0", "f.40008.3.0",
                         "f.40008.4.0", "f.40008.5.0", "f.40008.6.0", "f.40008.7.0", "f.40008.8.0", "f.40008.9.0",
                         "f.40008.10.0", "f.40008.11.0", "f.40008.12.0", "f.40008.13.0", "f.40008.14.0", "f.40008.15.0",
                         "f.40008.16.0","f.40008.17.0", "f.40008.18.0", "f.40008.19.0", "f.40008.20.0", "f.40008.21.0"
)]

# Transpose Date of Diagnosis ---------------------------------------------

#one column for every cancer occurrence --> transpose into one row per cancer occurrence
ca_date_long <- melt(as.data.table(ca_date), id=c("eid"))
ca_date_long <- ca_date_long[!is.na(ca_date_long$value) , ]

#number of occurrences 
ca_date_long$no_occ <- substring(ca_date_long$variable, 9) #count number of occurrences
ca_date_long$no_occ <- sub("\\..*", "", ca_date_long$no_occ) 
ca_date_long$no_occ <- as.numeric(ca_date_long$no_occ)
colnames(ca_date_long)[3] <- "date_diagnose"
ca_date_long$variable <- NULL

# Transpose ICD9 ----------------------------------------------------------
#different classes of variables! --> transform into integer
for (i in 2:ncol(ca_icd9)){
  ca_icd9[ , i] <- as.integer(as.character(ca_icd9[ , i]))
}

#one column for every ICD9 occurrence -> transpose into one row per ICD9 occurrence
ca_icd9_long <- melt(as.data.table(ca_icd9), id=c("eid"))
ca_icd9_long <- ca_icd9_long[!is.na(ca_icd9_long$value) , ]
ca_icd9_long$no_occ <- substring(ca_icd9_long$variable, 9) 
ca_icd9_long$no_occ <- sub("\\..*", "", ca_icd9_long$no_occ)
ca_icd9_long$no_occ <- as.numeric(ca_icd9_long$no_occ)

#ICD9 coding: 173.3 --> transform values without dot in this format e.g. 1733 -> 173.3
ca_icd9_long$ICD9_3 <- substring(ca_icd9_long$value, 1,3)
ca_icd9_long$ICD9_3 <- as.numeric(ca_icd9_long$ICD9_3)

colnames(ca_icd9_long)[3] <- "ICD9"
ca_icd9_long$variable <- NULL

# Transpose ICD10 ---------------------------------------------------------

#one column for every ICD10 occurence -> transpose into one row per ICD10 occurrence
ca_icd10_long <- melt(as.data.table(ca_icd10), id=c("eid"))
ca_icd10_long$value[which(ca_icd10_long$value=="")] <- NA # RAP maked it empty instead of NA
ca_icd10_long <- ca_icd10_long[!is.na(ca_icd10_long$value) , ] 

ca_icd10_long$no_occ <- substring(ca_icd10_long$variable, 9)
ca_icd10_long$no_occ <- sub("\\..*", "", ca_icd10_long$no_occ)
ca_icd10_long$no_occ <- as.numeric(ca_icd10_long$no_occ)

#separate letter from number (e.g. C502 --> C)
ca_icd10_long$ICD10_letter <- as.factor(substring(ca_icd10_long$value, 1,1))

#separate first two letters (e.g. C502 --> 50)
ca_icd10_long$ICD10_code <- substring(ca_icd10_long$value, 2,3)
ca_icd10_long$ICD10_code <- as.numeric(ca_icd10_long$ICD10_code)    #00, 01, 02 etc was transformed to 0,1,2 because of transformation to numeric

colnames(ca_icd10_long)[3] <- "ICD10"
ca_icd10_long$variable <- NULL

# Transpose Behaviour ------------------------------------
#one column for every cancer occurrence -> transpose into one row per behavior occurrence
ca_bhv_long <- melt(as.data.table(ca_bhv), id=c("eid"))
ca_bhv_long <- ca_bhv_long[!is.na(ca_bhv_long$value) , ]

#number of occurrences 
ca_bhv_long$no_occ <- substring(ca_bhv_long$variable, 9)
ca_bhv_long$no_occ <- sub("\\..*", "", ca_bhv_long$no_occ)
ca_bhv_long$no_occ <- as.numeric(ca_bhv_long$no_occ)

colnames(ca_bhv_long)[3] <- "behaviour"
ca_bhv_long$variable <- NULL

# Transpose histology -----------------------------------------------------
#one column for every cancer occurrence -> transpose into one row per histology occurrence
ca_hist_long <- melt(as.data.table(ca_hist), id=c("eid"))
ca_hist_long <- ca_hist_long[!is.na(ca_hist_long$value) , ]

#number of occurrences 
ca_hist_long$no_occ <- substring(ca_hist_long$variable, 9)
ca_hist_long$no_occ <- sub("\\..*", "", ca_hist_long$no_occ)
ca_hist_long$no_occ <- as.numeric(ca_hist_long$no_occ)

colnames(ca_hist_long)[3] <- "histology"
ca_hist_long$variable <- NULL

# Merge -------------------------------------------------------------------
merge1 <- merge(ca_date_long, ca_icd9_long, by=c("eid", "no_occ"), all=T)
merge2 <- merge(merge1, ca_icd10_long, by=c("eid", "no_occ"), all=T)
merge3 <- merge(merge2, ca_bhv_long, by=c("eid", "no_occ"), all=T)
merge4 <- merge(merge3, ca_hist_long, by=c("eid", "no_occ"), all=T)

#adding assessment centre and attending assessment center
toadd <- cancer_df[ ,c("eid", "f.54.0.0", "f.53.0.0","f.2724.0.0")] # added meno
merge4 <- left_join(merge4, toadd, by="eid")
colnames(merge4)[11] <- "assess_center"
colnames(merge4)[12] <- "date_baseline" 
merge4$date_baseline <- as.Date(merge4$date_baseline)

merge4$date_diagnose <- as.Date(merge4$date_diagnose)
outcome1 <- merge4

# Inclusion criteria ------------------------------------------------------
# Inclusion of all participants without prevalent cancer diagnoses (except benign or non-melanoma skin cancer (C44))
# determine for all cancer occurrences per participant: do all cancer occurrences allow the participant to be included?
# (no cancer row must be a prevalent malignant cancer)

# Question 1: Is date_diagnose > f.53.0.0 ?  (yes:1/ no:0)
# Question 2: Is behavior benign? (yes:1/ no:0)
# Question 3: Is ICD9_3 == 173 OR is ICD10_letter == "C" AND ICD10_code == 44 (yes:1/ no:0)

#(1=inclusion; 0=exclusion)

# Question 1
outcome1$inclusion1 <- 0
outcome1$inclusion1[outcome1$date_diagnose > outcome1$date_baseline & !is.na(outcome1$date_baseline)] <- 1

# Question 2
outcome1$inclusion2 <- 0
outcome1$inclusion2[outcome1$behaviour == 0 & !is.na(outcome1$behaviour)] <- 1

# Question 3
outcome1$inclusion3 <- 0
outcome1$inclusion3[outcome1$ICD9_3 == 173 | (outcome1$ICD10_letter == "C" & outcome1$ICD10_code == 44)] <- 1

#merging together
# INCLUSION (0=no; 1=yes)

outcome1$inclusionTotal <- 0
#diagnosis is after baseline examination
outcome1$inclusionTotal[outcome1$inclusion1 == 1] <- 1 
#diagnosis is prior to baseline but benign
outcome1$inclusionTotal[outcome1$inclusion1 == 0 & outcome1$inclusion2 == 1] <- 1 
#diagnosis is prior to baseline but is a non-melanoma cancer
outcome1$inclusionTotal[outcome1$inclusion1 == 0 & outcome1$inclusion3 == 1] <- 1 

# all participants with at least one row inclusionTotal == 0 must be excluded (all lines of this participant)
exclude <- subset(outcome1, inclusionTotal == 0)
outcome2 <- anti_join(outcome1, exclude, by="eid") 

# save(exclude, file = "cancer_censoring/prevalent_cancer_to_exclude.RData")
# participants with prevalent malignant cancer (except NMSC)

# Relevant cancers --------------------------------------------------------
# Identification and Classification of primary malignant cancer outcomes

# does cancer row classify as outcome or is it irrelevant?

#date of cancer diagnosis is before date of attending assessment center (only benign or NMSC)
#--> prevalent cancer --> exclusion of participants
outcome2$irrelevant <- 0
outcome2$irrelevant[outcome2$date_diagnose <= outcome2$date_baseline | is.na(outcome2$date_diagnose)] <- 1

# 0 (benign)  
# 1 (uncertain benign/malignant)  
# 2 (in situ)   
# 3 (malignant primary)   
# 5 (malignant microinvasive)   
# 6 (malignant metastatic) 
# 9 (malignant uncertain primary/metastatic) 

# benign, uncertain benign/malignant, in situ, metastatic, uncertain primary/metastatic is irrelevant for outcome
outcome2$irrelevant[outcome2$behaviour %in% c(0, 1, 2,6,9)] <- 1

# behaviour = NA -> irrelevant
outcome2$irrelevant[is.na(outcome2$behaviour)] <- 1

# secondary and unspecified malignant neoplasms (C77-C79) irrelevant
outcome2$irrelevant[outcome2$ICD10_code >= 77 & outcome2$ICD10_code <= 79] <-1

 #Coding cancers
# WE NOW REMOVE ALL D AND O CODES TO NOT MISTAKENLY SELECT NON MALIGNANT CANCER
# Except benign meningioma

rows_do <- which(outcome2$ICD10_letter %in% c("D","O") ) # & !outcome2$ICD10 %in% c("D320","D321","D329") # IF BENIGN MENIN IS IMPORTANT
outcome2$ICD10[rows_do] <- NA
outcome2$ICD10_code[rows_do] <- NA

outcome2$irrelevant[rows_do] <- 1 # now all other with D need be set to irrelevant==1 !

# Remain benign morphologies
# outcome2$ICD10[which(outcome2$ICD10 %in% c("D320","D321","D329") &
#                        !outcome2$histology %in% c(9530:9539))] <- NA
# outcome2$ICD10_code[which(outcome2$ICD10 %in% c("D320","D321","D329") &
#                        !outcome2$histology %in% c(9530:9539))] <- NA
# 
# outcome2$irrelevant[which(outcome2$ICD10 %in% c("D320","D321","D329"))] <- 0 # those are not irrelevant!
# outcome2$irrelevant[rows_do] <- 1 # now all other with D need be set to irrelevant==1 !

outcome2$ICD10[which(outcome2$irrelevant==0)] %>% unique() %>% sort()

outcome2 <- left_join(outcome2, cancer_df[,c("eid","f.31.0.0","f.21022.0.0")]) # merge SEX for male breast

# ICD codes ----------------------------------------------------------------------------------------------------------------------

outcome2$cancer_type <- NA
outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(0:14,32) &
                                 outcome2$ICD10_letter=="C", # need to specific its C32 not meningioma
                               "oral_pharynx_larynx", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code == 15 &
                                 outcome2$histology %in% c(8140,8141, 8143:8145, 8190:8231, 8260:8263, 8310, 8401, 8480:8490, 8550:8551, 8570:8574, 8576), # adeno
                               "esophageal_adeno", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code == 15 &
                                 outcome2$histology %in% c(8050:8078, 8083, 8084), # any remainder?
                               "esophageal_scc", outcome2$cancer_type)
# length(which(outcome2$ICD10_code==15 & !outcome2$histology %in% c(8050:8078, 8083, 8084, 8140,8141, 8143:8145, 8190:8231, 8260:8263, 8310, 8401, 8480:8490, 8550:8551, 8570:8574, 8576))) # 75

outcome2$cancer_type <- ifelse(outcome2$ICD10 == "C160" &
                                 outcome2$histology %in% c(8140:8145, 8147, 8210, 8211, 8214, 8220, 8221, 8230, 8231, 8255, 8260:8263, 8310, 8480, 8481, 8490, 8510, 8560, 8562, 8570:8576), 
                               "stomach_cardia", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10 %in% c("C161","C162","C163","C164","C165","C166") &
                                 outcome2$histology %in% c(8140:8145, 8147, 8210, 8211, 8214, 8220, 8221, 8230, 8231, 8255, 8260:8263, 8310, 8480, 8481, 8490, 8510, 8560, 8562, 8570:8576), 
                               "stomach_non_cardia", outcome2$cancer_type)
# length(which((outcome2$ICD10 %in% c("C160","C161","C162","C163","C164","C165","C166") & !outcome2$histology %in% c(8140:8145, 8147, 8210, 8211, 8214, 8220, 8221, 8230, 8231, 8255, 8260:8263, 8310, 8480, 8481, 8490, 8510, 8560, 8562, 8570:8576))))

outcome2$cancer_type <- ifelse(outcome2$ICD10 %in% c("C168","C169"),
                               "stomach_other", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code == 17,
                               "small_intestine", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(18,19,20),
                               "colorectum", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(21),
                               "anus", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10 == "C220",
                               "hcc", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10 == "C221",       # may be classified as gallbladder in analyses
                               "ibdc", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(23) | # ,24
                                 outcome2$ICD10 %in% c("C241","C242","C243","C244","C245","C246","C247"),
                               "gallbladder", outcome2$cancer_type)
outcome2$cancer_type <- ifelse(outcome2$ICD10 %in% c("C240","C248","C249"),
                               "extraheptic_biliary_tract", outcome2$cancer_type) 

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(25),
                               "pancreas", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(30,31,33,37,38), # 30 is nasal, 31 is sinuses, 33 trachea,37 is thymus, 38 is heart
                               "head_neck", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(34), 
                               "lung", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(40,41,42), 
                               "bone_cartilage", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(43),
                               "melanoma", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(45:49), 
                               "soft_tissue", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(50) &
                                 (outcome2$f.31.0.0=="0" & outcome2$f.2724.0.0 == 1), # had meno pause
                               "breast_post", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(50) &
                                 (outcome2$f.31.0.0=="0" & outcome2$f.2724.0.0 == 0), # had not menopause
                               "breast_pre", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(50) &
                                 (outcome2$f.31.0.0=="0" & (is.na(outcome2$f.2724.0.0) | outcome2$f.2724.0.0 %in% c(-3,2,3))), # meno not known
                               "breast_unknown", outcome2$cancer_type)
outcome2$cancer_type <- ifelse(outcome2$cancer_type == "breast_unknown" &
                                 outcome2$f.21022.0.0 >= 55, 
                               "breast_post", ifelse(outcome2$cancer_type == "breast_unknown" &
                                                       outcome2$f.21022.0.0 < 55, 
                                                     "breast_pre", outcome2$cancer_type))

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(50) &           
                                 outcome2$f.31.0.0 == "1", # among men
                               "breast_men", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(51,52,57),
                               "female_genital", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(53),
                               "cervix", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10 %in% c("C540","C541","C542", "C543", "C549","C55"),
                               "corpus_uteri", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(56) &
                                 !outcome2$histology %in% c(8441,8460,8461),
                               "ovary", outcome2$cancer_type)                  
outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(56) &
                                   outcome2$histology %in% c(8441,8460,8461),   
                               "ovary_serous", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(60,63), 
                               "male_genital", outcome2$cancer_type) 

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(62), 
                               "testicles", outcome2$cancer_type) 

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(61),
                               "prostate", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(64) &
                                 outcome2$histology %in% c(8050, 8140, 8260, 8270, 8280:8312, 8316:8320, 8340:8344),
                               "kidney_rcc", outcome2$cancer_type)
outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(64) &
                                 !outcome2$histology %in% c(8050, 8140, 8260, 8270, 8280:8312, 8316:8320, 8340:8344), 
                               "kidney_other", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(65,66), 
                               "renal_ureter", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(67),
                               "bladder", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(69),
                               "eye", outcome2$cancer_type)

# outcome2$cancer_type <- ifelse(outcome2$ICD10 %in% c("D320","D321","D329"),
#                                "meningioma", outcome2$cancer_type) 

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(71,72) &
                                 outcome2$histology %in% c(9380:9384, 9391:9460),  
                               "glioma", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(73),
                               "thyroid", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(81),
                               "hodgkins", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(82:85) & 
                                 !outcome2$ICD10 %in% c("C833"), # not diffuse large lymphoma
                               "non_hodgkins_lymphoma", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10 %in% c("C833"),
                               "diffuse_large_lymphoma", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(90),
                               "multiple_myeloma", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(91),
                               "lymphoid_leukemia", outcome2$cancer_type)

outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(92),
                               "myeloid_leukemia", outcome2$cancer_type)

# other cancers
outcome2$cancer_type <- ifelse(outcome2$ICD10_code %in% c(26,39, 58, 68, 74, 75, 76,80,86,88,93,94,95,96,97) | # 58 is other female gen but doesnt exist
                                 (outcome2$ICD10_code==15 & !outcome2$histology %in% c(8050:8078, 8083, 8084, 8140,8141, 8143:8145, 8190:8231, 8260:8263, 8310, 8401, 8480:8490, 8550:8551, 8570:8574, 8576)) | # esophageal
                                 outcome2$ICD10 %in% c("C223","C224","C227","C229") |
                                 (outcome2$ICD10 %in% c("C160","C161","C162","C163","C164","C165","C166") & !outcome2$histology %in% c(8140:8145, 8147, 8210, 8211, 8214, 8220, 8221, 8230, 8231, 8255, 8260:8263, 8310, 8480, 8481, 8490, 8510, 8560, 8562, 8570:8576)) |
                                 outcome2$ICD10_code %in% c(70) | # malignant meningioma
                                 (outcome2$ICD10_code %in% c(71,72) & !outcome2$histology %in% c(9380:9384, 9391:9460)) , # other brain and CNS
                               "other", outcome2$cancer_type)

# lancet cancer definintion, group some cancer to other
outcome2$cancer_type <- ifelse(outcome2$cancer_type %in%
                                 c("kidney_other","female_genital","stomach_other","bone_cartilage","soft_tissue","male_genital","eye"),
                               "other", outcome2$cancer_type)

table(outcome2$ICD10_code[which(is.na(outcome2$cancer_type) & outcome2$irrelevant==0)])

# # if yet a relevant cancer but not a pa cancer = irrelvant
# outcome2$irrelevant[which(outcome2$irrelevant == 0 & is.na(outcome2$cancer_type))] <- 1

# Date of diagnosis ---------------------------------------------------------------------------

#Identification of relevant cancers, first date of diagnosis
# ignore NMSC diagnosis and count following diagnosis
# https://biobank.ndph.ox.ac.uk/~bbdatan/CancerNumbersReport.html

relevant <- subset(outcome2, irrelevant==0)
relevant <- subset(relevant, !(ICD10 %in% c("C440","C441", "C442" , "C443",  "C444",  "C445",  "C446" , "C447" , "C448" , "C449")))
relevant <- relevant[order(relevant$eid, relevant$date_diagnose), ]

 # HOW MANY SAME DAY DIAGNOSES
same_day <- relevant %>%
  group_by(eid) %>%
  filter(date_diagnose == min(date_diagnose)) %>%  # Keep only the earliest date per eid
  ungroup() %>%
  group_by(eid, date_diagnose) %>%
  filter(n() > 1) %>%  # Retain only those with multiple diagnoses on that date
  ungroup()
# dim(same_day) -> 1524 x 21
# unique(same_day$eid) %>% length() -> 757

relevant <- relevant[!duplicated(relevant$eid),]  # # only keep first diagnosis 
relevant$ICD10 <- as.factor(relevant$ICD10)

relevant$inclusionTotal <- NULL
relevant$inclusion1 <- NULL
relevant$inclusion2 <- NULL
relevant$inclusion3 <- NULL
relevant$irrelevant <- NULL

#drop unused levels 
relevant$ICD10_letter <- droplevels(relevant$ICD10_letter)
relevant$ICD10 <- droplevels(relevant$ICD10)

# all_cancers == obesity related cancers
relevant$all_cancers <- 0
relevant$all_cancers[relevant$ICD10_code >= 00 & relevant$ICD10_code <= 97 & relevant$ICD10_code != 44] <- 1

# relevant$all_cancers <- ifelse(!is.na(relevant$cancer_type), 1,0)
# table(relevant$cancer_type) %>% sum()
table(relevant$all_cancers)

# Censoring date --------

#censoring date for cancer data: http://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=Data_providers_and_dates 
#England and Wales censoring date = 29th February 2020; Scotland = 31 January 2021

dates <- cancer_df[ ,c("eid", "f.31.0.0", "f.34.0.0", "f.21003.0.0", "f.21022.0.0", "f.53.0.0", "f.54.0.0", 
                       "f.191.0.0", "f.40000.0.0", "f.40007.0.0")]

colnames(dates)[2] <- "sex"
colnames(dates)[3] <- "year_of_birth"
colnames(dates)[4] <- "age_at_assessment_center"
colnames(dates)[5] <- "age_at_recruitment"
colnames(dates)[6] <- "date_baseline" 
colnames(dates)[7] <- "center"
colnames(dates)[8] <- "lost_to_fu_date"
colnames(dates)[9] <- "death_date"
colnames(dates)[10] <- "death_age"

# grouping regions to countries
# WATCH OUT: WALES IS NO LONGER FOLLOW UP; CENSORING UNTIL 2016 ONLY
dates$country <- ifelse(dates$center %in% c(11003, 11022, 11023), "Wales", 
                        ifelse (dates$center %in% c(11004, 11005), "Scotland",
                                "England"))

#country-specific end of follow-up for cancer data:
#https://biobank.ndph.ox.ac.uk/showcase/exinfo.cgi?src=Data_providers_and_dates
dates$end_fu <- as.Date(NA)
dates$end_fu[dates$country == "Wales"] <- "2016-12-31" # 31 December 2016*
dates$end_fu[dates$country == "Scotland"] <- "2021-11-30" # 30 November 2021
dates$end_fu[dates$country == "England"] <- "2020-12-31" # 31 December 2020

# joining relevant cancer diagnosis to complete UKB baseline sample
toadd <- relevant[ ,c("eid", "date_diagnose", "ICD10", "all_cancers")] #include "cancer_type"
dates <- left_join(dates, toadd, by="eid")

# exclude participants with prevalent malignant cancers (see inclusion critera)
n <- nrow(dates) # 502134
dates <- anti_join(dates, exclude, by="eid")  
nrow(dates)-n # 36929 excluded

dates$date_diagnose <- as.Date(dates$date_diagnose)
dates$death_date <- as.Date(dates$death_date)
dates$lost_to_fu_date <- as.Date(dates$lost_to_fu_date)
dates$end_fu <- as.Date(dates$end_fu)

whicheverfirst <- gather(dates, "date_diagnose", "death_date", "lost_to_fu_date", "end_fu", key="whicheverfirst", value="date_exit_first")
whicheverfirst1 <- whicheverfirst[!is.na(whicheverfirst$date_exit_first), ]
whicheverfirst1$date_exit_first_num <- as.numeric(whicheverfirst1$date_exit_first)
whicheverfirst2 <- whicheverfirst1[order(whicheverfirst1$eid, whicheverfirst1$date_exit_first_num), ]  # only keep first row, earliest date
whicheverfirst3 <- whicheverfirst2[!duplicated(whicheverfirst2$eid),]

names(whicheverfirst3)[names(whicheverfirst3) == "whicheverfirst"] <- "reason_exit_first" 

# cancer cases have 1
whicheverfirst3$all_cancers[is.na(whicheverfirst3$all_cancers)] <- 0
whicheverfirst3$date_exit_first_num <- NULL


# cancer diagnoses after complete date of follow up must be set to 0
whicheverfirst3$all_cancers_cens <- whicheverfirst3$all_cancers # 47598 * 1
whicheverfirst3$ICD10_cens <- whicheverfirst3$ICD10   # 47598

whicheverfirst3[whicheverfirst3$all_cancers == 1 & whicheverfirst3$reason_exit_first == "end_fu", "all_cancers_cens"] <- 0
whicheverfirst3[whicheverfirst3$all_cancers == 1 & whicheverfirst3$reason_exit_first == "end_fu", "ICD10_cens"] <- NA

# Age at diagnosis/censoring/follow-up/death ---------------------------------------------------------
#days between baseline examination and date of exit (ca diagnosis, death, loss to FU, censoring date according to study center)
whicheverfirst3$FUtime <- NA
whicheverfirst3$FUtime <- whicheverfirst3$date_exit_first - whicheverfirst3$date_baseline
whicheverfirst3$FUtime <- as.numeric(as.character(whicheverfirst3$FUtime))

# Age at diagnosis/Censoring/Death: age at Baseline/Recruitment + Follow up time
whicheverfirst3$age_exit <- NA
whicheverfirst3$age_exit <- round(whicheverfirst3$age_at_recruitment + whicheverfirst3$FUtime/365.25, 2)
whicheverfirst3$age_exit <- as.numeric(whicheverfirst3$age_exit)

# Cancer endpoints --------------------------------------------------------
outcome <- whicheverfirst3
colnames(outcome)
outcome$ICD10 <- NULL             # including diagnoses after complete follow up
outcome$all_cancers <- NULL       # including diagnoses after complete follow up

outcome$ICD10_code <- substring(outcome$ICD10_cens, 2,3)
outcome$ICD10_code <- as.numeric(outcome$ICD10_code)
head(outcome)

# clean and save data set############################# 
outcome$ICD10_code <- NULL
outcome$year_of_birth <- NULL
outcome$age_at_assessment_center <- NULL

head(outcome)

# Final clean and save ------------------------------------------------------------------------
outcome <- outcome[,c("eid","reason_exit_first","date_exit_first","ICD10_cens","age_exit")]
cancer_data <- outcome

# sort according to ID
cancer_data <- cancer_data[order(cancer_data$eid),]
rownames(cancer_data) <- NULL # reset rownames
head(cancer_data)

# add date cancer again for MM framework
cancer_data <- left_join(cancer_data, dates[,c("eid","date_diagnose")])

colnames(cancer_data) <- c("eid","reason_exit_first_cancer","date_exit_first_cancer","cancer_code","age_exit_cancer","cancer_date")

# merge cancer type
cancer_data <- left_join(cancer_data, relevant[,c("eid","cancer_type")])

t <- table(cancer_data$cancer_type[which(cancer_data$reason_exit_first_cancer=="date_diagnose")]) %>% sort(decreasing = T) %>% as.data.frame()
write.csv(t, file = "cancer_cases.csv", row.names = F)


library(data.table)
library(dplyr)

# Ensure cancer_data is a data.table
cancer_data <- as.data.table(cancer_data)

# Create unique identifiers for multiple occurrences of cancer per patient
cancer_data[, occurrence := seq_len(.N), by = eid]



# # Store this file to RAP --------------------------------------------------
# system("dx upload cancer_cases.csv --destination /KilianG/bili_nazli/cancer_cases.csv") 
# 
# save(cancer_data, file = "cancer_data.RData")
# system("dx upload cancer_data.RData --destination /KilianG/bili_nazli/cancer_data.RData") 
# 
# system("dx upload cancer_incidence.R --destination /KilianG/bili_nazli/cancer_incidence.R")
