libs <- function(){
  
library(dplyr)
library(ggplot2)
library(table1)
library(broom)
library(openxlsx)
library(survival)
}

get_cancer_vars <- function(df){
	
	#all
	df$esophagus <- 0
	df$crc <- 0
	df$gallbladder <- 0
	df$intrahep_bile_duct <- 0
	df$hepatocellular <- 0
	df$kidney <- 0
	df$thyroid <- 0
	df$multiple_myeloma <- 0
	df$pancreas <- 0
	df$stomach_cardia <- 0
	df$meningioma <- 0
	#men
	df$prostate <- 0
	#women
	df$breast_pre <- 0
	df$breast_post <- 0
	df$endometrial <- 0
	df$ovary <- 0
	#sum
	df$homrmonal <-0
	df$obesity_non_hormonal <- 0
	df$obesity_cancer <- 0
	df$all_cancer <- 0

	df$crc[df$cancer_type == "colorectum"] <- 1
	df$esophagus[df$cancer_type == "esophageal_adeno"] <- 1
	df$gallbladder[df$cancer_type == "gallbladder"] <- 1
	df$pancreas[df$cancer_type == "pancreas"] <- 1
	df$intrahep_bile_duct[df$cancer_type == "ibdc"] <- 1
	df$hepatocellular[df$cancer_type == "hcc"] <- 1
	df$kidney[df$cancer_type == "kidney_rcc"] <- 1
	df$thyroid[df$cancer_type == "thyroid"] <- 1
	df$multiple_myeloma[df$cancer_type == "multiple_myeloma"] <- 1	
	df$prostate[df$cancer_type == "prostate"] <- 1	
	df$breast_pre[df$cancer_type == "breast_pre"] <- 1
	df$breast_post[df$cancer_type == "breast_post"] <- 1
	df$endometrial[df$cancer_type == "corpus_uteri" & df$cancer_code == "C541"] <- 1
	df$ovary[df$cancer_type == "ovary" | df$cancer_type == "ovary_serous"] <- 1
	df$gastric_cardia[df$cancer_type == "stomach_cardia"] <- 1
	df$meningioma[df$cancer_type == "other" & grepl("^C70", df$cancer_code)] <- 1
	
	df$obesity_cancer <- ifelse(rowSums(df[, c("crc", "esophagus", "gallbladder", "intrahep_bile_duct", "pancreas",
																																												"hepatocellular", "kidney", "thyroid", "multiple_myeloma",
																																												"breast_post", "endometrial", "ovary", "gastric_cardia", "meningioma")], # "breast_pre", "prostate"
																																					na.rm = TRUE) > 0, 1, 0)
	
	df$hormonal <- ifelse(rowSums(df[, c("breast_post", "endometrial", "ovary")], 
																															na.rm = TRUE) > 0, 1, 0)
	
	df$obesity_non_hormonal[df$obesity_cancer == 1 & df$hormonal == 0] <- 1
	
	df$all_cancer[(!is.na(df$cancer_type))] <- 1
	
	return(df)
}

create_missing_indicators <- function(df, all_vars) {
	for (var in all_vars) {
		# Print current number of participants
		initial_n <- nrow(df)
		print(initial_n)
		
		# Calculate number of missing cases for the variable
		missing_n <- sum(is.na(df[[var]]))
		cat("Found", missing_n, "missing values for", var, "\n")
		
		# Create a missing data indicator column for the variable
		missing_col_name <- paste0(var, "_missing")
		df[[missing_col_name]] <- ifelse(is.na(df[[var]]), 1, 0)
		
		if (var == "waist_c") {
			initial_n <- nrow(df)
			df <- subset(df, inclusion_cancer == 1)
			removed_n <- initial_n - nrow(df)
			cat("Removed", removed_n, "participants due to cancer\n")
		}
	}
	return(df)
}


get_cat_vars <- function (df){
	df <- df %>%
		mutate(
			bmi_bil_cat = factor(
				case_when(
					bmi_m >= 16 & bmi_m < 25 ~ 1,
					bmi_m >= 25 & bmi_m < 30 & tbili <= 10 ~ 2,
					bmi_m >= 25 & bmi_m < 30 & tbili > 10 ~ 3,
					TRUE ~ 9
				),
				levels = c(1, 2, 3, 9),
				labels = c("Normal weights", 
															"Overweight non-GS", "Overweight GS", "Underweight or Obese")
			),
			bmi_bil_cat2 = factor(
				case_when(
					bmi_m >= 16 & bmi_m < 25  ~ 1,
					bmi_m >= 25 & tbili <= 10 ~ 2,
					bmi_m >= 25 & tbili > 10 ~ 3,
					TRUE ~ 9
				),
				levels = c(1, 2, 3, 9),
				labels = c("Normal weight", 
															"Overweight non-GS", "Overweight GS", "Underweight")
			),
			bmi_cat = factor(
				case_when(
					bmi_m >= 16 & bmi_m < 25 ~ 1,
					bmi_m >= 25 & bmi_m < 30 ~ 2,
					bmi_m >= 30 ~ 3,
					TRUE ~ 9
				),
				levels = c( 1, 2, 3, 9),
				labels = c("Normal weight", "Overweight", "Obese", "Unknown")
			),
			bil_bin_cat = factor(
				case_when(
					tbili < 10 ~ 1,
					tbili >= 10 ~2,
					TRUE ~ 9
				),
				levels = c(1, 2, 9),
				labels = c("bil <10", "bil >= 10", "")
			),
			bil_tert_cat= factor(
				ntile(tbili, 3), 
				levels = c(1, 2, 3),
				labels = c("lower tertile", "middle", "upper")
			)
		)
	return(df)
}
#assign level and variable labels
get_labels <- function(df){
	
	#assign levels, so that they appear in corect order in the table
	
	df$alc_stat <- factor(
		df$alc_stat,
		levels = c(
			"Current",
			"Previous",
			"Never"
		)
	)
	
	df$smoke_stat <- factor(
		df$smoke_stat,
		levels = c(
			"Current",
			"Previous",
			"Never"
		)
	)
	
	#assign labels to variables, so that they appear labelled in the table
  label_map <- list(
    age_rec = "Age (years)", tbili = "Total Bilirubin (uM/L)",
    weight_m_0 = "Weight (kg)", height = "Height (cm)",
    bmi_m = "Body Mass Index", waist_c = "Waist Circumference (cm)",
    alc_stat = "Alcohol Consumption Status (n)",smoke_stat = "Smoking Status (n)", 
    pa_mod_dur = "Moderate Physical Activity Duration (min)", college_degree = "College Degree (n)",
    pa_min_per_week_MET = "Physical Activity - MET score (min/week)",
    ever_hrt = "Ever Used Hormone Replacement Therapy (n)",
    score_diet = "Diet Score (0-7)", all_cancer = "Total cancer (n)",
    obesity_cancer = "Obesity related cancer (n)", prostate = "Prostate cancer (n)",
    esophagus = "Esophageal Adenocarcinoma (n)", crc = "Colorectal cancer (n)",
    pancreas = "Pancreatic cancer (n)",  gallbladder = "Gallbladder cancer (n)",
    hepatocellular = "Hepatocellular carcinoma (n)",
    intrahep_bile_duct = "Intrahepatic bile duct cancer (n)",
    breast_pre = "Breast cancer pre menopause (n)", 
    breast_post = "Breast cancer post menopause (n)", ovary = "Ovarian cancer (n)",
    endometrial = "Endometrial cancer (n)", kidney = "Renal Cell Carcinoma (n)",
    thyroid = "Thyroid cancer (n)",multiple_myeloma = "Multiple myeloma (n)", 
    gastric_cardia = "Gastric Cardia (n)", meningioma = "Malignant meningioma (n)"
  )
  
  for (var in names(label_map)) {
   label(df[[var]]) <- label_map[[var]]
  }
  return(df)
}


generate_table <- function(data, sex, stratify_var, row_vars,cancer_vars) {

  # Define cancer variables based on sex
  cancer_vars <- if (sex == "Male") {
    paste(cancer_vars[!cancer_vars %in% c("breast_pre", "breast_post", "endometrial", "ovary")], collapse = " + ")
  } else {
    paste(cancer_vars[cancer_vars != "prostate"], collapse = " + ")
  }
  
  # Define table formulas dynamically
  formula_base <- as.formula(paste("~", paste(row_vars, collapse = " + "), "| as.factor(", stratify_var, ")"))
  formula_cancer <- as.formula(paste("~ obesity_cancer +", cancer_vars, "| as.factor(", stratify_var, ")"))
  
  # Define render function
  rndr <- function(x, ...) {
  	if (is.logical(x)) {
  		y <- render.default(x, ...)
  		y[2]  # Keep the second element for logicals
  	} else if (is.numeric(x)) {
  		# Ensure Mean (SD) is calculated and rounded properly
  		mean_x <- mean(x, na.rm = TRUE)
  		sd_x <- sd(x, na.rm = TRUE)
  		sprintf("%.1f (%.1f)", mean_x, sd_x)  # Round both mean and SD to 1 decimal
  	} else {
  		render.default(x, ...)
  	}
  }
  
  # Define render function
  rndr.cancer <- function(x, ...) {
  	if (is.logical(x)) {
  		y <- render.default(x, ...)
  		sub(" .*", "", y[2])  # Keep the second element for logicals
  	} else if (is.numeric(x)) {
  		# Ensure Mean (SD) is calculated and rounded properly
  		mean_x <- mean(x, na.rm = TRUE)
  		sd_x <- sd(x, na.rm = TRUE)
  		sprintf("%.1f (%.1f)", mean_x, sd_x)  # Round both mean and SD to 1 decimal
  	} else {
  		render.default(x, ...)
  	}
  }
  
  
  # Function to create tables
  create_table <- function(formula, data, caption) {
    table1(formula, data = data, caption = caption, render = rndr, render.continuous = "Mean (SD)", overall=NULL, digits.pct=1)
  }
  create_tablec <- function(formula, data, caption) {
  	table1(formula, data = data, caption = caption, render = rndr.cancer, overall=NULL)
  }
  
  table_base = create_table(formula_base, data %>% filter(sex == !!sex), paste("Table: ", sex, stratify_var))
  table_cancer = create_tablec(formula_cancer, data %>% filter(sex == !!sex), paste("Table: ", sex, stratify_var, "and Cancer"))
  
  
  return(rbind(as.data.frame(table_base), as.data.frame(table_cancer)[-1, ]))
}





#initial cancer processing, no longer needed----------------------------------------------

# drop_cancer_at_rec <- function(df) {
#   initial_n <- nrow(df)  # Store initial number of rows
#   
#   df_filtered <- df %>%
#     filter(date_recruit <= date_cancer_i0 | date_cancer_i0 =="")  # Filter rows
#   
#   removed_n <- initial_n - nrow(df_filtered)  # Calculate number of removed rows
#   final_n <- nrow(df_filtered)  # Store final count
#   
#   # Print summary
#   cat("Initial participants:", initial_n, "\n")
#   cat("Participants removed:", removed_n, "\n")
#   cat("Final participants:", final_n, "\n")
#   
#   return(df_filtered)  #Return cleaned dataframe
# }
# 
# #not needed anymore
# get_cancer_types_w_dates <- function(df, cancer_codes, icd_cols) {
#   for (i in seq_len(nrow(cancer_codes))) {
#     icd_code <- cancer_codes$ICD10[i]
#     cancer_type <- cancer_codes$CancerType[i]
#     
#     df[[cancer_type]] <- FALSE
#     df[[paste0(cancer_type, "_date")]] <- as.Date(NA)
#     
#     # Loop over each ICD column and its corresponding date column
#     for (i in seq_along(icd_cols)) {
#       current_icd_col <- icd_cols[i]
#       current_date_col <- paste0("date_cancer_i", as.numeric(i -1))
#       
#       # Check which rows have the current ICD code at the start of the string
#       matches <- startsWith(as.character(df[[current_icd_col]]), icd_code)
#       
#       # Set the cancer indicator to TRUE for matching rows
#       df[[cancer_type]][matches] <- TRUE
#       
#       # For matching rows that do not already have a recorded date,
#       # assign the date from the corresponding date column.
#       not_assigned <- is.na(df[[paste0(cancer_type, "_date")]])
#       assign_date <- matches & not_assigned
#       
#       df[[paste0(cancer_type, "_date")]][assign_date] <- df[[current_date_col]][assign_date]
#     }
#   }
#   return(df)
# }


# calculate_red_meat_intake <- function(df) {
# 	# Define mapping from categorical responses to numerical values (weekly intake)
# 	intake_map <- c(
# 		"Never" = 0,
# 		"Less than once a week" = 0.5,
# 		"Once a week" = 1,
# 		"2-4 times a week" = 3,
# 		"5-6 times a week" = 5.5,
# 		"Once or more daily" = 7,
# 		"Do not know" = NA,
# 		"Prefer not to answer" = NA
# 	)
# 	
# 	# Reverse mapping to convert numeric values back to categorical levels
# 	reverse_map <- function(intake) {
# 		case_when(
# 			intake == 0 ~ "Never",
# 			intake > 0 & intake < 1 ~ "Less than once a week",
# 			intake == 1 ~ "Once a week",
# 			intake > 1 & intake <= 4 ~ "2-4 times a week",
# 			intake > 4 & intake <= 6 ~ "5-6 times a week",
# 			intake > 6 ~ "Once or more daily",
# 			TRUE ~ NA
# 		)
# 	}
# 	
# 	# Convert categorical variables to numeric
# 	df <- df %>%
# 		mutate(
# 			meat_beef = intake_map[meat_beef],
# 			meat_pork = intake_map[meat_pork],
# 			meat_lamb = intake_map[meat_lamb]
# 		)
# 	
# 	# Compute total red meat intake per week
# 	df <- df %>%
# 		mutate(
# 			red_meat_numeric = rowSums(select(., meat_beef, meat_pork, meat_lamb), na.rm = TRUE),
# 			red_meat= reverse_map(red_meat_numeric)  # Convert back to categories
# 		)
# 	
# 	return(df)
# }
