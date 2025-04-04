rm(list = ls())
setwd("C:/Users/kiliang98/ucloud/Shared/projects_KR/Bilirubin/data")

source("../scripts/functions_ct.r")
libs()

#read data and colnames
df <- read.csv("data_w_cancer.csv")
cnames<- read.table("new_vars_names")

cancer_vars <- c("obesity_cancer>", "all_cancer", "breast_post", "crc", "esophagus", "pancreas", "endometrial", "ovary", "kidney",
																	"hepatocellular", "thyroid","gallbladder", "intrahep_bile_duct","multiple_myeloma",
																	 "gastric_cardia", "meningioma")
#write colnames to df
colnames(df) <- coalesce(cnames[match(names(df), cnames[, 1]), 2], names(df))

#name vars of interest, specify categorical
all_vars <- c("tbili", "bmi_m", "waist_c", "height", "age_rec", "alc_stat", "smoke_stat", 
										"pa_min_per_week_MET", "college_degree", "score_diet", "ever_hrt")
cats <- c( "alc_stat", "smoke_stat", "qualifications")

df$college_degree <- ifelse(grepl("College or University degree", df$qualifications), 1, 0)

df$inclusion_cancer[is.na(df$inclusion_cancer)] <- 1
#mark non-answer to cats as missing
for (cat in cats){
	df[[cat]][df[[cat]] == "Prefer not to answer" ] <- NA
	df[[cat]][df[[cat]] == "" ] <- NA
	df[[cat]][df[[cat]] == "Do not know" ] <- NA
}

for (var in all_vars) {
	cat("Initial n:", nrow(df), "\n")
	i_rows <- nrow(df)
	
	if (!var %in% names(df)) {
		cat("Variable", var, "not found in dataframe. Skipping.\n")
		next
	}
	# Remove missing values for current variable
	df <- df[complete.cases(df[[var]]), ]
	removed_n <- i_rows - nrow(df)
	cat("Removed", removed_n, "participants due to missing", var, "\n")
	
	# Special condition for 'waist_c'
	if (var == "waist_c") {
		i_rows <- nrow(df)
		df <- df[df$inclusion_cancer != 0, ]
		removed_n <- i_rows - nrow(df)
		cat("Removed", removed_n, "participants due to cancer\n")
	}
}
i_rows <- nrow(df)
df<- df[complete.cases(df$qualifications), ]
removed_n <- i_rows - nrow(df)
cat("Removed", removed_n, "participants due to missing quali\n")

i_rows <- nrow(df)
df <- df %>%
	filter((sex == "Female" & ever_hrt != "") | sex == "Male")
removed_n <- i_rows - nrow(df)
cat("Removed", removed_n, "participants due to missing hrt\n")

df <- get_cat_vars(df)
df <- get_cancer_vars(df)

df<- get_labels(df)

table_specs <- list(
	list(name = "table_one_bmi_m", sex = "Male", stratify_var = "bmi_cat"),
	list(name = "table_one_bmi_f", sex = "Female", stratify_var = "bmi_cat"),
	list(name = "table_one_bmigs_m", sex = "Male", stratify_var = "bmi_bil_cat"),
	list(name = "table_one_bmigs_f", sex = "Female", stratify_var = "bmi_bil_cat"),
	list(name = "table_one_bmigs1_m", sex = "Male", stratify_var = "bmi_bil_cat2"),
	list(name = "table_one_bmigs1_f", sex = "Female", stratify_var = "bmi_bil_cat2")
)

# Create and store tables dynamically
tables_list <- list()
for (spec in table_specs) {
	tables_list[[spec$name]] <- generate_table(df, spec$sex, spec$stratify_var, all_vars, cancer_vars)
}





saveRDS(df, "working_file.rds")
saveRDS(df1, "working_file_missing.rds")


# old code ------------------------------------------------------------------------------------

# # Create deciles of tbili
# df$tbili_decile <- cut(df$tbili, breaks = quantile(df$tbili, probs = seq(0, 1, 0.1), na.rm = TRUE), 
#                        include.lowest = TRUE, labels = FALSE)
# 
# # Summarize counts
# df_summary <- df %>%
#   group_by(tbili_decile, obesity_cancer) %>%
#   summarise(count = n(), .groups = "drop")
# 
# # Plot
# ggplot(df_summary, aes(x = factor(tbili_decile), y = count, fill = factor(obesity_cancer))) +
#   geom_bar(stat = "identity", position = "fill") +
#   scale_y_continuous(labels = scales::percent_format()) +
#   labs(x = "Tbili Decile", y = "Proportion", fill = "Obesity Cancer",
#        title = "Distribution of Obesity Cancer Status Across Tbili Deciles") +
#   theme_minimal()


# # Initialize cancer type columns
# unique_cancers <- unique(cancer_codes$CancerType)
# df[unique_cancers] <- 0
# 
# # Convert ICD columns to character type
# icd_cols <- paste0("icd10_i", 0:21)
# df[icd_cols] <- lapply(df[icd_cols], as.character)
# 
# # Assign cancer presence based on ICD codes
# df <- get_cancer_types_w_dates(df, cancer_codes, icd_cols)

# Create obesity-related cancer indicator and dates
# df$obesity_cancer <- apply(df[unique_cancers], 1, function(x) any(x, na.rm = TRUE))
# cancer_date_cols <- paste0(unique_cancers, "_date")
# df[cancer_date_cols] <- lapply(df[cancer_date_cols], function(x) as.Date(x, format = "%Y-%m-%d"))
# df$obesity_date <- do.call(pmin, c(df[cancer_date_cols], na.rm = TRUE))



# rm(list = ls())
# 
# library(dplyr)
# library(ggplot2)
# library(table1)
# library(broom)
# library(openxlsx)
# library(survival)
# 
# setwd("C:/Users/kiliang98/ucloud/Shared/projects_KR/Bilirubin/data")
# 
# # Read in data
# df <- read.csv("bili_data.csv")
# cnames <- read.delim("new_vars_names", FALSE)
# cancer_codes <- read.table("cancer_codes.txt", header = FALSE, stringsAsFactors = FALSE)
# colnames(cancer_codes) <- c("ICD10", "CancerType")
# 
# # Rename columns based on 'new_vars_names'
# colnames(df) <- replace(names(df), names(df) %in% cnames[, 1], cnames[match(names(df), cnames[, 1]), 2])
# colnames(df) <- gsub("^p40006_", "icd10_", names(df))  # Standardize ICD column names
# 
# # Initialize cancer type columns
# unique_cancers <- unique(cancer_codes$CancerType)
# df[unique_cancers] <- 0
# 
# # Convert ICD columns to character type
# icd_cols <- paste0("icd10_i", 0:21)
# df[icd_cols] <- lapply(df[icd_cols], as.character)
# 
# # Assign cancer presence based on ICD codes
# for (i in seq_len(nrow(cancer_codes))) {
# 	icd_code <- cancer_codes$ICD10[i]
# 	cancer_type <- cancer_codes$CancerType[i]
# 	
# 	df[[cancer_type]] <- rowSums(sapply(df[icd_cols], function(col) startsWith(col, icd_code)), na.rm = TRUE) > 0
# }
# 
# # Create obesity-related cancer indicator
# df$obesity_cancer <- apply(df[unique_cancers], 1, function(x) any(x, na.rm = TRUE))
# 
# # Define variables
# myvars <- c("age_rec", "tbili", "weight_m_0", "height", "bmi_m", "waist_c", 
# 												"alc_stat", "smoke_stat", "pa_mod_dur", "ever_hrt", "college_degree")
# 
# cats <- c("alc_stat", "smoke_stat")
# 
# # Clean categorical variables
# df[cats] <- lapply(df[cats], function(x) ifelse(x %in% c("", "Prefer not to answer", "Do not know"), NA, x))
# df$college_degree <- grepl("College or University degree", df$qualifications)
# 
# # Drop missing values
# df <- df[complete.cases(df[myvars]), ]
# print(paste("Remaining participants:", nrow(df)))
# 
# # Categorize BMI and bilirubin
# df <- df %>%
# 	mutate(
# 		bmi_bil_cat = factor(
# 			case_when(
# 				bmi_m >= 18.5 & bmi_m < 25 & tbili < 10 ~ 1,
# 				bmi_m >= 18.5 & bmi_m < 25 & tbili >= 10 ~ 2,
# 				bmi_m >= 25 & bmi_m < 30 & tbili >= 10 ~ 3,
# 				bmi_m >= 25 & bmi_m < 30 & tbili < 10 ~ 4,
# 				TRUE ~ 9
# 			),
# 			levels = c(1, 2, 3, 4, 9),
# 			labels = c("Normal weights non-GS", "Normal weight GS", 
# 														"Overweight non-GS", "Overweight GS", "Underweight or Obese")
# 		),
# 		bmi_cat = factor(
# 			case_when(
# 				bmi_m < 18.5 ~ 1,
# 				bmi_m >= 18.5 & bmi_m < 25 ~ 2,
# 				bmi_m >= 25 & bmi_m < 30 ~ 3,
# 				bmi_m >= 30 ~ 4,
# 				TRUE ~ 9
# 			),
# 			levels = c(1, 2, 3, 4, 9),
# 			labels = c("Underweight", "Normal weight", "Overweight", "Obese", "Unknown")
# 		)
# 	)
# 
# # Define labels
# label_map <- list(
# 	age_rec = "Age (years)", tbili = "Total Bilirubin (uM/L)",
# 	weight_m_0 = "Weight (kg)", height = "Height (cm)",
# 	bmi_m = "Body Mass Index", waist_c = "Waist Circumference (cm)",
# 	alc_stat = "Alcohol Consumption Status (n)",
# 	smoke_stat = "Smoking Status (n)", pa_mod_dur = "Moderate Physical Activity Duration (min)",
# 	ever_hrt = "Ever Used Hormone Replacement Therapy (n)",
# 	obesity_cancer = "Obesity related cancer (n)"
# )
# for (var in names(label_map)) {
# 	label(df[[var]]) <- label_map[[var]]
# }
# 
# # Apply labels to cancer types
# for (cancer in unique_cancers) {
# 	label(df[[cancer]]) <- paste(cancer, "(n)")
# }
# 
# # Define table formulas
# cancer_vars_f <- paste(unique_cancers[unique_cancers != "prostate"], collapse = " + ")
# cancer_vars_m <- paste(unique_cancers[!unique_cancers %in% c("breast", "endometrial", "ovary")], collapse = " + ")
# 
# # Define table formulas
# table_formulas <- list(
# 	formula_m  = as.formula("~ age_rec + tbili + weight_m_0 + height + bmi_m + waist_c + alc_stat + smoke_stat + college_degree | as.factor(bmi_cat)"),
# 	formula_f  = as.formula("~ age_rec + tbili + weight_m_0 + height + bmi_m + waist_c + alc_stat + smoke_stat + college_degree | as.factor(bmi_cat)"),
# 	formula1_m = as.formula("~ age_rec + tbili + weight_m_0 + height + bmi_m + waist_c + alc_stat + smoke_stat + college_degree | as.factor(bmi_bil_cat)"),
# 	formula1_f = as.formula("~ age_rec + tbili + weight_m_0 + height + bmi_m + waist_c + alc_stat + smoke_stat + college_degree | as.factor(bmi_bil_cat)"),
# 	formulac_m = as.formula(paste("~ obesity_cancer +", cancer_vars_m, "| as.factor(bmi_cat)")),
# 	formulac_f = as.formula(paste("~ obesity_cancer +", cancer_vars_f, "| as.factor(bmi_cat)")),
# 	formula1c_m = as.formula(paste("~ obesity_cancer +", cancer_vars_m, "| as.factor(bmi_bil_cat)")),
# 	formula1c_f = as.formula(paste("~ obesity_cancer +", cancer_vars_f, "| as.factor(bmi_bil_cat)"))
# )
# 
# # Define render function
# rndr <- function(x, ...) {
# 	y <- render.default(x, ...)
# 	if (is.logical(x)) y[2] else y
# }
# 
# # Function to create tables
# create_table <- function(formula, data, caption) {
# 	table1(formula, data = data, caption = caption, render = rndr, render.continuous = "Mean (SD)")
# }
# 
# # Generate and save tables
# tables <- list(
# 	table_one_bmi_m  = create_table(table_formulas$formula_m, df %>% filter(sex == "Male"), "Table 1A. Men BMI categories"),
# 	table_one_bmi_f  = create_table(table_formulas$formula_f, df %>% filter(sex == "Female"), "Table 1B. Women BMI categories"),
# 	table_one_bmigs_m = create_table(table_formulas$formula1_m, df %>% filter(sex == "Male"), "Table 1C. Men BMI and GS"),
# 	table_one_bmigs_f = create_table(table_formulas$formula1_f, df %>% filter(sex == "Female"), "Table 1D. Women BMI and GS")
# )
# 
# # Save tables to Excel
# lapply(names(tables), function(name) {
# 	write.xlsx(as.data.frame(tables[[name]]), file = paste0("../results/", name, ".xlsx"), rowNames = TRUE)
# })



