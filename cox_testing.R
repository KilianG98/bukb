# Clear workspace and load required libraries
rm(list = ls())

library(survival)
library(dplyr)
library(forestplot)
library(grid)
library(rms)
library(ggplot2)

setwd("C:/Users/kiliang98/ucloud/Shared/projects_KR/Bilirubin/data")


####functions------------------------------------------------------------------------------------------------------------------------------------

#Function to load and prepare the data -----------------------------------------
prepare_data <- function(data_file) {
	df <- readRDS(data_file)
	
	# Define cancer outcomes for females and males
	cancers_f <- c("obesity_cancer", "all_cancer", "breast_post", "endometrial", "crc", "pancreas" ,
																"esophagus", "gallbladder", "intrahep_bile_duct", "hepatocellular", 
																"kidney", "thyroid", "multiple_myeloma", "ovary" )
	cancers_m <- c("obesity_cancer", "all_cancer", "crc", "esophagus", "gallbladder", "pancreas",
																"intrahep_bile_duct", "hepatocellular", "kidney", "thyroid", 
																"multiple_myeloma")
	
	# Store cancer outcome lists in a list so that they can be selected later by sex
	cancers <- list(Female = cancers_f, Male = cancers_m)
	
	# Format physical activity and dates
	df <- df %>%
		group_by(sex) %>%
		mutate(pa_min_per_week_MET_sd = pa_min_per_week_MET / sd(pa_min_per_week_MET, na.rm = TRUE)) %>%
		ungroup()
	
	df$date_recruit <- as.Date(as.character(df$date_recruit), format = "%Y-%m-%d")
	df$date_exit_first_cancer <- as.Date(as.character(df$date_exit_first_cancer), format = "%Y-%m-%d")
	df$days_followup <- as.numeric(df$date_exit_first_cancer - df$date_recruit)
	
	#create bmi_bil_cat for obese only
	df <- df %>%
		mutate(
			bmi_bil_cat_obese = factor(
				case_when(
					bmi_m >= 16 & bmi_m < 25 ~ 1,
					bmi_m >= 30 & tbili < 10 ~ 2,
					bmi_m >= 30 & tbili >= 10 ~ 3,
					TRUE ~ 9
				),
				levels = c(1, 2, 3, 9),
				labels = c("Normal weights", "Obese non-GS", "Obese GS", "overweight")
			)
		)
	
	
	#create age categoies
	df <- df %>%
		mutate(age_cat = cut(
			age_rec, 
			breaks = c(-Inf, 44.9, 49.9, 54.9, 59.9, 64.9, Inf), 
			labels = c("<44.9", "45-49.9", "50-54.9", "55-59.9", "60-64.9", "65+"),
			right = TRUE
		))
	
	# Remove Underweight and keep records where exit age is greater than recruitment age
	df <- subset(df, bmi_bil_cat2 != "Underweight" & age_exit_cancer > age_rec)
	
	#drop unused levels
	df <- droplevels(df)
	
	# Create separate datasets for females and males
	df_f <- subset(df, sex == "Female")
	df_m <- subset(df, sex == "Male")
	
	df_f <- df_f %>%
		mutate(
			bmi_wc_bil_cat_oo = factor(
				case_when(
					bmi_m >= 16 & waist_c < 88 & bmi_m < 25 ~ 1,
					bmi_m >= 25 & waist_c >= 88 & tbili < 10 ~ 2,
					bmi_m >= 25 & waist_c >= 88 & tbili >= 10 ~ 3,
					TRUE ~ 9
				),
				levels = c(1, 2, 3, 9),
				labels = c("Normal weights", "Overweight non-GS", "Overweight GS", "Overweight small waist")
			)
		)
	df_m <- df_m %>%
		mutate(
			bmi_wc_bil_cat_oo = factor(
				case_when(
					bmi_m >= 16 & waist_c < 102 & bmi_m < 25 ~ 1,
					bmi_m >= 25 & waist_c >= 102 & tbili < 10 ~ 2,
					bmi_m >= 25 & waist_c >= 102 & tbili >= 10 ~ 3,
					TRUE ~ 9
				),
				levels = c(1, 2, 3, 9),
				labels = c("Normal weights", "Overweight non-GS", "Overweight GS", "Overweight small waist")
			)
		)
	
	
	data_list <- list(Female = df_f, Male = df_m)
	
	
	
	return(list(data = data_list, cancers = cancers))
}

#functions for analysis----------------------------------------------------------

# Function to run Cox models for each sex cancer outcome.
run_cox_models <- function(data_list, cancers, exposure_variable, covariates, stratify) {
	results <- list()
	for (sex in names(data_list)) {
		data <- data_list[[sex]]
		cancer_list <- cancers[[sex]]
		covars <- covariates[[sex]]
		
		for (canc in cancer_list) {
			# Create the formulas for raw and adjusted models
			formula_raw <- as.formula(
				paste("Surv(age_rec, age_exit_cancer,", canc, ") ~", exposure_variable, 
										paste("+ strata(", paste(stratify, collapse = ", "), ")", sep = "")
				)
			)
			
			
			formula_adj <- as.formula(
				paste("Surv(age_rec, age_exit_cancer,", canc, ") ~", exposure_variable, "+", 
										paste(covars, collapse = " + "), 
										paste("+ strata(", paste(stratify, collapse = ", "), ")", sep = "")
				))
			
			
			# Fit the Cox models
			cox_raw <- coxph(formula_raw, data = data)
			cox_adj <- coxph(formula_adj, data = data)
			
			# Dynamically name the results and store them
			raw_name <- paste0(sex, "_", canc, "_raw")
			adj_name <- paste0(sex, "_", canc, "_adj")
			results[[raw_name]] <- cox_raw
			results[[adj_name]] <- cox_adj
		}
	}
	return(results)
}


# Function to extract summary statistics from a Cox model
extract_summary <- function(model, exposure_variable, level_gs, level_non_gs) {
	
	#construct row names for extraction
	rowname_gs <- paste0(exposure_variable, level_gs)
	rowname_non_gs <- paste0(exposure_variable, level_non_gs)
	
	suma <- summary(model)
	
	print(suma)
	print(rowname_gs)
	
	
	# Extract estimates and confidence intervals
	coef_summary <- suma$coefficients
	conf_int <- suma$conf.int
	
	print(coef_summary)
	
	coef_gs <- coef_summary[rowname_gs, c("exp(coef)", "Pr(>|z|)")]
	coef_non_gs <- coef_summary[rowname_non_gs, c("exp(coef)", "Pr(>|z|)")]
	conf_gs <- conf_int[rowname_gs, c("lower .95", "upper .95")]
	conf_non_gs <- conf_int[rowname_non_gs, c("lower .95", "upper .95")]
	conc <- summary(model)$concordance[1]  # C-statistic
	cases <- summary(model)$nevent
	
	return(data.frame(
		Category   = c("GS", "non-GS"),
		exp.coef   = c(coef_gs["exp(coef)"], coef_non_gs["exp(coef)"]),
		p.value    = c(coef_gs["Pr(>|z|)"], coef_non_gs["Pr(>|z|)"]),
		lower.95   = c(conf_gs["lower .95"], conf_non_gs["lower .95"]),
		upper.95   = c(conf_gs["upper .95"], conf_non_gs["upper .95"]),
		concordance = rep(conc, 2),
		cases = rep(cases,2)
	))
}

# Function to apply summary extraction to all models and combine into one data frame
summarize_models <- function(results, exposure_variable, level_gs, level_non_gs) {
	
	
	summary_list <- lapply(results, extract_summary, exposure_variable = exposure_variable,
																								level_gs = level_gs, level_non_gs = level_non_gs)
	summary_df <- do.call(rbind, summary_list)
	#make model column based on rownames
	summary_df$Model <- row.names(summary_df)
	summary_df$Model <- substr(summary_df$Model, 1, nchar(summary_df$Model) - 2)
	summary_df$Sex <- sub("_.*", "", summary_df$Model)
	summary_df$Adjustment <- sub("^.*_", "", summary_df$Model)
	summary_df$Cancer <- sub("^[^_]*_(.*)_.*$", "\\1", summary_df$Model)
	# 
	round_cols <- c("exp.coef", "lower.95", "upper.95", "concordance")
	summary_df[round_cols] <- round(summary_df[round_cols], 2)
	summary_df$p.value <- round(summary_df$p.value, 3)
	summary_df$p.value[summary_df$p.value == 0] <- "< 0.001"
	
	return(summary_df)
}

# Function to plot a forest plot for models with at least one significant result
plot_significant_forest <- function(summary_df) {
	# Filter to include only adjusted models with at least one significant result
	plot_df <- summary_df %>%
		filter(grepl("adj$", Model)) %>%
		group_by(Model) %>%
		filter(any(p.value <= 0.05)) %>%
		ungroup() %>%
		arrange(Model)
	
	tabletext <- cbind(
		c("Model", substr(as.character(plot_df$Model), 1, nchar(as.character(plot_df$Model)) - 4)),
		c("Category", as.character(plot_df$Category)),
		c("HR", sprintf("%.2f", plot_df$exp.coef)),
		c("95% CI", sprintf("%.2f - %.2f", plot_df$lower.95, plot_df$upper.95))
	)
	
	mean  <- c(NA, plot_df$exp.coef)
	lower <- c(NA, plot_df$lower.95)
	upper <- c(NA, plot_df$upper.95)
	
	# Set colors based on category
	colors <- ifelse(plot_df$Category == "GS", "blue", "grey")
	box_styles <- lapply(colors, function(col) gpar(fill = col, col = "#555555"))
	full_box_styles <- c(list(gpar()), box_styles)
	
	forestplot(labeltext = tabletext,
												mean      = mean,
												lower     = lower,
												upper     = upper,
												zero      = 1,
												xlab      = "Hazard Ratio",
												boxsize   = 0.25,
												line.margin = 0.1
	) |>
		fp_set_style(box = full_box_styles, default = gpar(vertices = TRUE))
}

get_lr_interaction <- function(data,canc, covars, stratify){
	
	formula_no_int <- as.formula(
		paste("Surv(age_rec, age_exit_cancer,", canc, ") ~ bmi_cat + tbili +", 
								paste(covars, collapse = " + "), 
								paste("+ strata(", paste(stratify, collapse = ", "), ")", sep = "")
		))
	
	formula_int <- as.formula(
		paste("Surv(age_rec, age_exit_cancer,", canc, ") ~ bmi_cat * tbili +", 
								paste(covars, collapse = " + "), 
								paste("+ strata(", paste(stratify, collapse = ", "), ")", sep = "")
		))
	
	cox_no_int <- coxph(formula_no_int, data = data)
	cox_int <- coxph(formula_int, data = data)
	
	res <- anova(cox_int, cox_no_int, test= "LRT")
	
	return(res)
}


### main---------------------------------------------------------------------------------------------------------------------------------


data_and_cancers_list <- prepare_data("working_file.rds") # contains 2 datasets (male and female) as well as the list of cancers for both sexes

covariates_m <- c("height", "alc_stat", "smoke_stat", "pa_min_per_week_MET_sd", "college_degree", "score_diet")
covariates_f <- c("height", "alc_stat", "smoke_stat", "pa_min_per_week_MET_sd", "college_degree", "score_diet", "ever_hrt")

covariates <- list(Female = covariates_f, Male = covariates_m)

stratify <- c("age_cat", "centre")

exposure_variable = "bmi_wc_bil_cat_oo"
level_gs = "Overweight GS"
level_non_gs = "Overweight non-GS"


data_list <- data_and_cancers_list$data
cancers <- data_and_cancers_list$cancers

print("running cox models")
# Run the Cox models
results <- run_cox_models(data_list, cancers, exposure_variable, covariates, stratify)

model<-results$Female_all_cancer_adj
suma <- summary(results$Female_obesity_cancer_raw)
suma$coefficients[rowname_gs, c("exp(coef)", "Pr(>|z|)")]

print("summarizing results")
# Extract summary statistics from all models
summary_df <- summarize_models(results, exposure_variable, level_gs, level_non_gs)

summary_df <- summary_df[order(summary_df$Cancer,
																															summary_df$Sex,
																															summary_df$Adjustment, 
																															summary_df$Category), ]
