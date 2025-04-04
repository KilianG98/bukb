rm(list = ls())

library(dplyr)
library(ggplot2)
library(table1)
library(broom)
library(openxlsx)
library(survival)

setwd("C:/Users/kiliang98/ucloud/Shared/projects_KR/Bilirubin/data")



#read in data, column names and cancer codes
df <- read.csv("bili_data.csv")
cnames <- read.delim("new_vars_names", FALSE)
cancer_codes <- read.table("cancer_codes.txt", header = FALSE, stringsAsFactors = FALSE)
colnames(cancer_codes) <- c("ICD10", "CancerType")

#set colnames excetpt icd columns
for (i in seq_along(cnames[,1])) {
  df <- setNames(df, replace(names(df), names(df) == cnames[i, 1], cnames[i, 2]))
}

#set colnames for icd-columns
colnames(df) <- gsub("^p40006_", "icd10_", names(df))

#initalize column for every cancer type
unique_cancers <- unique(cancer_codes$CancerType)
for (cancer in unique_cancers) {
  df[[cancer]] <- 0
}

#transform column witch icd10 codes to type character
icd_cols <- paste0("icd10_i", 0:21)
df[icd_cols] <- lapply(df[icd_cols], as.character)

#loop through cancer codes and names
#for each code find all rows in the dataframe that contain a match in one of the icd10 columns
#assign corresponding cacner column as 1 or 0
for (i in seq_len(nrow(cancer_codes))) {
  icd_code <- cancer_codes$ICD10[i]
  cancer_type <- cancer_codes$CancerType[i]
  
  df[[cancer_type]] <- as.logical(rowSums(sapply(df[icd_cols], function(col) startsWith(col, icd_code)), na.rm = TRUE) > 0)
}

#assign presence of obesity related cancer for each participant (yes/no)
df$obesity_cancer <- as.logical(apply(df[, unique_cancers], 1, function(x) any( x == TRUE, na.rm = TRUE)))

myvars<-c("age_rec","tbili", "weight_m_0", "height", "bmi_m", "waist_c", "alc_stat", "smoke_stat", "pa_mod_dur", "ever_hrt", "college_degree" )
fwf_vars<- c( "pack_years", "calcium_blood", "processed_meat",  "red_meat")
all_vars <- c(myvars, fwf_vars)
#energy intake not feasible, would drop 50 percent of participants
#calcium intake simialar, but can use blood level calcuim


levels(as.factor(df$processed_meat))
cats<-c("alc_stat", "smoke_stat")



#drop missings, transform empty to na for categorical vars
df[cats] <- lapply(df[cats], function(x) ifelse(x == "", NA, x))
df[cats] <- lapply(df[cats], function(x) ifelse(x == "Prefer not to answer", NA, x))
df[cats] <- lapply(df[cats], function(x) ifelse(x == "Do not know", NA, x))
df$college_degree <- as.logical(ifelse(grepl("College or University degree", df$qualifications), 1, 0))


df$energy_intake

for (var in all_vars) {
  initial_n <- nrow(df)
  print(var)
  df <- df[complete.cases(df[var]), ]
  removed_n <- initial_n - nrow(df)
  cat("Removed", removed_n, "participants due to missing", var, "\n")
}


# Check remaining participants (465264), 36870 excuded
nrow(df)

#1=normal weights, non GS, 2= normal weight GS, 3 = overweight non GS, 4 = overwight GS, 9 = underweigth or obese
df <- df %>%
  mutate(bmi_bil_cat = case_when(
    bmi_m >= 18.5 & bmi_m < 25 & tbili < 10 ~ 1,
    bmi_m >= 18.5 & bmi_m < 25 & tbili >= 10 ~ 2,
    bmi_m >= 25 & bmi_m < 30 & tbili >= 10 ~ 3,
    bmi_m >= 25 & bmi_m < 30 & tbili < 10 ~ 4,
    TRUE ~ 9
  ))

df$bmi_bil_cat = factor(df$bmi_bil_cat, 
                    levels = c(1, 2, 3, 4, 9), 
                    labels = c("Normal weights non-GS", "Normal weight GS", "Overweight non-GS", "Overweight GS", "Underweight or Obese"))

df <- df %>%
  mutate(bmi_cat = case_when(
    bmi_m < 18.5 ~ 1,
    bmi_m >= 18.5 & bmi_m < 25 ~ 2,
    bmi_m >= 25 & bmi_m < 30  ~ 3,
    bmi_m >= 30 ~ 4,
    TRUE ~ 9
  ))

df$bmi_cat = factor(df$bmi_cat, 
                 levels = c(1, 2, 3, 4, 9), 
                 labels = c("Underweight", "Normal weight", "Overweight", "Obese", "Unknown"))

#115646 are not categorised, vast majority is obese. can these not be included?
sum(df$bmi_bil_cat == 4 )
sum(df$bmi_m >= 30)

label(df$age_rec) <- "Age (years)"
label(df$tbili) <- "Total Bilirubin (uM/L)"
label(df$weight_m_0) <- "Weight (kg="
label(df$height) <- "Height (cm)"
label(df$bmi_m) <- "Body Mass Index"
label(df$waist_c) <- "Waist Circumference (cm)"
label(df$alc_stat) <- "Alcohol Consumption Status (n)"
label(df$smoke_stat) <- "Smoking Status(n)"
label(df$pa_mod_dur) <- "Moderate Physical Activity Duration (min)"
label(df$ever_hrt) <- "Ever Used Hormone Replacement Therapy (n)"
label(df$oesophagus) <- "Oesophageal cancer (n)"
label(df$crc) <- "Colorectal cancer (n)"
label(df$pancreas) <- "Pancreatic cancer (n)"
label(df$gallbladder) <- "Gallbladder cancer (n)"
label(df$hepatocellular) <- "Hepatocellular carcinoma (n)"
label(df$intrahep_bile_duct) <- "Intrahepatic bile duct cancer (n)"
label(df$breast) <- "Breast cancer (n)"
label(df$ovary) <- "Ovarian cancer (n)"
label(df$endometrial) <- "Endometrial cancer (n)"
label(df$kidney) <- "Kidney cancer (n)"
label(df$thyroid) <- "Thyroid cancer (n)"
label(df$multiple_myeloma) <- "Multiple myeloma (n)"
label(df$prostate) <- "Prostate cancer (n)"
label(df$obesity_cancer) <- "Obesity related cancer (n)"



cancer_vars_f <- paste(unique_cancers[unique_cancers != "prostate"], collapse = " + ")
cancer_vars_m <- paste(unique_cancers[!unique_cancers %in% c("breast", "endometrial", "ovary") ], collapse = " + ")

# construct the formula
formula_m <- as.formula(paste("~ age_rec + tbili + weight_m_0 + height + bmi_m + waist_c + alc_stat + smoke_stat + college_degree | as.factor(bmi_cat)"))
formulac_m <- as.formula(paste("~ obesity_cancer + ", cancer_vars_m , "| as.factor(bmi_cat)"))
formula_f <- as.formula(paste("~ age_rec + tbili + weight_m_0 + height + bmi_m + waist_c + alc_stat + smoke_stat + college_degree | as.factor(bmi_cat)"))
formulac_f <- as.formula(paste("~ obesity_cancer + ", cancer_vars_f , "| as.factor(bmi_cat)"))
formula1_m <- as.formula(paste("~ age_rec + tbili + weight_m_0 + height + bmi_m + waist_c + alc_stat + smoke_stat + college_degree | as.factor(bmi_bil_cat)"))
formula1c_m <- as.formula(paste("~ obesity_cancer + ", cancer_vars_m , "| as.factor(bmi_bil_cat)"))
formula1_f <- as.formula(paste("~ age_rec + tbili + weight_m_0 + height + bmi_m + waist_c + alc_stat + smoke_stat + college_degree | as.factor(bmi_bil_cat)"))
formula1c_f <- as.formula(paste("~ obesity_cancer + ", cancer_vars_f , "| as.factor(bmi_bil_cat)"))

#render functions
# my.render.cont <- function(x) {
#   with(stats.apply.rounding(stats.default(x), digits=2), c("",
#                                                            "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
# }
# my.render.cat <- function(x) {
#   c("", sapply(stats.default(x), function(y) with(y,
#                                                   sprintf("%d (%0.0f %%)", FREQ))))
# }

# Generate Table 1

rndr <- function(x, ...) {
  y <- render.default(x, ...)
  if (is.logical(x)) y[2] else y
}

table_one_bmi_m <- table1(
  formula_m, 
  data = df %>% filter(sex == "Male"), 
  caption = "Table 1A. Population characteristics by body mass index (BMI) category at enrolment among men", 
  render.continuous = "Mean (SD)",
  render = rndr
)
table_one_bmi_mc <- table1(
  formulac_m, 
  data = df %>% filter(sex == "Male"), 
  caption = "Table 1A. Population characteristics by body mass index (BMI) category at enrolment among men", 
  render=rndr
)
table_one_bmi_f <- table1(
  formula_f, 
  data = df %>% filter(sex == "Female"), 
  caption = "Table 1B. Population characteristics by body mass index (BMI) category at enrolment among women", 
  render.continuous = "Mean (SD)"
)
table_one_bmi_fc <- table1(
  formulac_m, 
  data = df %>% filter(sex == "Female"), 
  caption = "Table 1A. Population characteristics by body mass index (BMI) category at enrolment among men", 
  render.cat = "FREQ"
)
table_one_bmigs_m <- table1(
  formula1_m, 
  data = df %>% filter(sex == "Male"), 
  caption = "Table 1C. Population characteristics by body mass index (BMI) and Gilbert syndrome (GS) category at enrolment among men", 
  render.continuous = "Mean (SD)"
)
table_one_bmigs_mc <- table1(
  formula1c_m, 
  data = df %>% filter(sex == "Male"), 
  caption = "Table 1A. Population characteristics by body mass index (BMI) category at enrolment among men", 
  render.cat = "FREQ"
)
table_one_bmigs_f <- table1(
  formula1_f, 
  data = df %>% filter(sex == "Female"), 
  caption = "Table 1D. Population characteristics by body mass index (BMI) and Gilbert syndrome (GS) category at enrolment among women",
  render.continuous = "Mean (SD)"
)
table_one_bmigs_fc <- table1(
  formula1c_f, 
  data = df %>% filter(sex == "Female"), 
  caption = "Table 1A. Population characteristics by body mass index (BMI) category at enrolment among men", 
  render.cat = "FREQ"
)


df_one_bmi_m <- rbind(as.data.frame(table_one_bmi_m), as.data.frame(table_one_bmi_mc))
df_one_bmi_f <- rbind(as.data.frame(table_one_bmi_f), as.data.frame(table_one_bmi_fc))
df_one_bmigs_m <- rbind(as.data.frame(table_one_bmigs_m), as.data.frame(table_one_bmigs_mc))
df_one_bmigs_f <- rbind(as.data.frame(table_one_bmigs_f), as.data.frame(table_one_bmigs_fc))


write.xlsx(df_one_bmi_m, file = "../results/table_one_bmi_m.xlsx",rowNames = TRUE)
write.xlsx(df_one_bmi_f, file = "../results/table_one_bmi_f.xlsx",rowNames = TRUE)
write.xlsx(df_one_bmigs_m, file = "../results/table_one_bmigs_m.xlsx",rowNames = TRUE)
write.xlsx(df_one_bmigs_f, file = "../results/table_one_bmigs_f.xlsx",rowNames = TRUE)





# #########################################################################################
# # investigate t bili, random stuff 
# tbili1 <- read.csv("tbili1.csv")
# cbili <- read.csv("cbili.csv")
# 
# tbili1 <- merge(tbili1, data_raw[, c("eid", "tbili")], by= "eid")
# 
# colnames(tbili1) <- c("eid", "t0", "t1")
# colnames(cbili) <- c("eid", "t0", "t1")
# 
# cnames <- read.delim("new_vars_names", FALSE)
# cnames <- cnames[cnames[,2] != "weight_m", ]
# 
# colnames(data_raw) <- cnames[,2]
# colnames(data_val) <- cnames[,2]
# 
# data_raw <- merge(data_raw, tbili1, by= "eid")
# tbili1 <- merge(tbili1, data_raw[, c("eid", "tbili")], by= "eid")
# 
# bil_dif <- data_raw$tbili - data_raw$tbili1
# bil_dif <- na.omit(bil_dif)
# summary(bil_dif)
# hist(bil_dif, breaks=30)
# sd(bil_dif)
# 
# 
# cor.test(data_raw$tbili, data_raw$tbili1)
# tbili1 <- na.omit(tbili1)# Define range of w values
# 
# w_values <- seq(5, 15, by = 0.1)
# probabilities <- numeric(length(w_values))  # Empty vector to store probabilities
# 
# # Loop through w values and calculate probabilities
# for (i in seq_along(w_values)) {
#   w <- w_values[i]
#   subset_data <- tbili1[tbili1$t0 >= w, ]
#   probabilities[i] <- sum(subset_data$t1 >= 12, na.rm = TRUE) / nrow(subset_data)
# }
# 
# # Combine results into a dataframe
# prob_data <- data.frame(w = w_values, probability = probabilities)
# 
# # Plot probabilities
# ggplot(prob_data, aes(x = w, y = probability)) +
#   geom_line(color = "blue") +
#   geom_point(color = "red") +
#   labs(title = "Probability of t1 >= 12 for different values of t0",
#        x = "t0",
#        y = "Probability") +
#   theme_minimal()
# 
# data_raw$bili_bmi_cat <- 
# 
# 
# hist(data_raw$tbili, breaks=1000, xlim=c(0,30))
# 
# 
# #investigate c bili
# 
# allbili <- merge(cbili, data_raw[, c("eid", "tbili", "tbili1")], by= "eid")
# allbili$ratio <- allbili$t0 / allbili$tbili
# allbili$ratio1 <- allbili$t1 / allbili$tbili1
# hist(allbili$ratio1, breaks = 50, xlim =c(0,1))
# summary(cbili)
# summary(allbili)
# 
# w_values <- seq(0, 0.2, by = 0.02)
# probabilities <- numeric(length(w_values)) 
# for (i in seq_along(w_values)) {
#   w <- w_values[i]
#   subset_data <- allbili[allbili$ratio >= w, ]
#   subset_data<- na.omit(subset_data)
#   probabilities[i] <- sum(subset_data$ratio1 <= 0.2, na.rm = TRUE) / nrow(subset_data)
# }
# prob_data <- data.frame(w = w_values, probability = probabilities)
# ggplot(prob_data, aes(x = w, y = probability)) +
#   geom_line(color = "blue") +
#   geom_point(color = "red") +
#   labs(title = "",
#        x = "t0",
#        y = "Probability") +
#   theme_minimal()
# 
