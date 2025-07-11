#Script to run cox models using different exposure variables on all obesity related cancer outcomes.
#several functons are defined on top, can be called below to obtain a summary of results

#Clear workspace and load required libraries
rm(list = ls())

library(survival)
library(dplyr)
library(forestplot)
library(grid)
library(rms)
library(ggplot2)


setwd("C:/Users/kiliang98/ucloud/Shared/projects_KR/Bilirubin/data")

################################################################################
####functions
################################################################################

#Function to load and prepare the data------------------------------------------
#receives location of data, and boolean to indicate whether participants with abnoirmal liver values should be excluded.
#returns a list of datasets and cancers for male and female
prepare_data <- function(data_file, liver=F) {
	df <- readRDS(data_file)
	
	# Define cancer outcomes for females and males
	cancers_f <- c("obesity_cancer", "all_cancer", "breast_post", "endometrial", "ovary", "crc", "pancreas" ,
																"hormonal", "obesity_non_hormonal",
															"esophagus",	"kidney","hepatocellular",  "thyroid","gallbladder", "intrahep_bile_duct",  "multiple_myeloma")
	cancers_m <- c("obesity_cancer", "all_cancer", "crc",  "pancreas", "kidney","hepatocellular",  "thyroid","gallbladder", "intrahep_bile_duct",  "multiple_myeloma", "esophagus")
	
	for (canc in cancers_f){
		df[[canc]] <- as.numeric(df[[canc]])
	}
	# Store cancer outcome lists in a list so that they can be selected later by sex
	cancers <- list(Female = cancers_f, Male = cancers_m)
	
	df$tbili_10 <- df$tbili >= 10
	df$bmi_25 <- df$bmi_m >= 25
	
	# Format physical activity and dates
	df <- df %>%
		group_by(sex) %>%
		mutate(pa_min_per_week_MET_sd = pa_min_per_week_MET / sd(pa_min_per_week_MET, na.rm = TRUE)) %>%
		ungroup()
	
	if(liver == T){
	print(nrow(df))
	df <- df[df$ala_trans >= 7 & df$ala_trans <= 55 & df$asp_trans >= 8 & df$asp_trans <= 48 & df$gglut_trans >= 8 & df$gglut_trans <= 61, ]
	print(nrow(df))
	}
	
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
	
	#generate releveled vars to get significance btw levels
	df$bmi_bil_cat2_rl <- relevel(df$bmi_bil_cat2, ref= "Overweight non-GS")
	df$bmi_bil_cat_obese_rl <- relevel(df$bmi_bil_cat_obese, ref= "Obese non-GS")
	df$bmi_bil_cat_rl <- relevel(df$bmi_bil_cat, ref= "Overweight non-GS")
	df$bmi_bil_cat4_rl <- relevel(df$bmi_bil_cat4, ref= "Overweight non-GS")
	df$bmi_bil_cat_obese4_rl <- relevel(df$bmi_bil_cat_obese4, ref= "Obese non-GS")
	
	#drop unused levels
	df <- droplevels(df)
	
	df$smoke_stat <- relevel(df$smoke_stat, ref= "Never")
	df$alc_stat <- relevel(df$alc_stat, ref= "Never")
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
	
	
	
	tbili_tertile_m <- quantile(df_m$tbili, probs = c(1/3, 2/3), na.rm = TRUE)
	tbili_tertile_f <- quantile(df_f$tbili, probs = c(1/3, 2/3), na.rm = TRUE)
	
	cat("tertile:", tbili_tertile_f, tbili_tertile_m)
	
	df_m <- df_m %>%
		mutate(
			bmi_bil_cat_tert = factor(
				case_when(
					bmi_m >= 16 & bmi_m < 25 ~ 1,
					bmi_m >=25 & tbili < tbili_tertile_m[2] ~ 2,
					bmi_m >= 25 & tbili >= tbili_tertile_m[2] & tbili < 25 ~ 3,
					TRUE ~ 9
				),
				levels = c(1, 2, 3, 9),
				labels = c("Normal weights", "Overweight non-GS", "Overweight GS", "????")
			)
		)
	
	df_f <- df_f %>%
		mutate(
			bmi_bil_cat_tert = factor(
				case_when(
					bmi_m >= 16 & bmi_m < 25 ~ 1,
					bmi_m >=25 & tbili < tbili_tertile_f[2] ~ 2,
					bmi_m >= 25 & tbili >= tbili_tertile_f[2] & tbili < 25 ~ 3,
					TRUE ~ 9
				),
				levels = c(1, 2, 3, 9),
				labels = c("Normal weights", "Overweight non-GS", "Overweight GS", "????")
			)
		)
	
	df_f <- df_f %>%
		mutate(
			bmi_bil_cat_alt = factor(
				case_when(
					bmi_m >= 16 & bmi_m < 25 & ala_trans < 35 ~ 1 ,
					bmi_m >=25 & tbili < 10 & ala_trans < 35 ~ 2 ,
					bmi_m >= 25 & tbili >= 10 & ala_trans < 35 ~ 3,
					TRUE ~ 9
				),
				levels = c(1, 2, 3, 9),
				labels = c("Normal weights", "Overweight non-GS", "Overweight GS", "????")
			)
		)
	
	df_m <- df_m %>%
		mutate(
			bmi_bil_cat_alt = factor(
				case_when(
					bmi_m >= 16 & bmi_m < 25 & ala_trans < 50 ~ 1,
					bmi_m >=25 & tbili < 10 & ala_trans < 50 ~ 2,
					bmi_m >= 25 & tbili >= 10 & ala_trans < 50 ~ 3,
					TRUE ~ 9
				),
				levels = c(1, 2, 3, 9),
				labels = c("Normal weights", "Overweight non-GS", "Overweight GS", "????")
			)
		)
	
	
	
	df_f$bmi_wc_bil_cat_oo_rl <- relevel(df_f$bmi_wc_bil_cat_oo, ref= "Overweight non-GS")
	df_m$bmi_wc_bil_cat_oo_rl <- relevel(df_m$bmi_wc_bil_cat_oo, ref= "Overweight non-GS")
	
	df_f$bmi_bil_cat_tert_rl <- relevel(df_f$bmi_bil_cat_tert, ref= "Overweight non-GS")
	df_m$bmi_bil_cat_tert_rl <- relevel(df_m$bmi_bil_cat_tert, ref= "Overweight non-GS")
	
	df_f$bmi_bil_cat_alt_rl <- relevel(df_f$bmi_bil_cat_alt, ref= "Overweight non-GS")
	df_m$bmi_bil_cat_alt_rl <- relevel(df_m$bmi_bil_cat_alt, ref= "Overweight non-GS")
	
	df_f <- droplevels(df_f)
	df_m <- droplevels(df_m)
	
	df_f$pa_min_per_week_MET_quint <- ntile(df_f$pa_min_per_week_MET, 5)
	df_m$pa_min_per_week_MET_quint <- ntile(df_m$pa_min_per_week_MET, 5)
	
	data_list <- list(Female = df_f, Male = df_m)
	
	
	
	return(list(data = data_list, cancers = cancers))
}

#functions for analysis----------------------------------------------------------

# Function to run Cox models for each sex cancer outcome.
#receives list of data,cancer, covariates, stratification variable for male and femal as well as name of the exposure
#returns list of modelling results for all sex and cancer
run_cox_models <- function(data_list, cancers, exposure_variable, covariates, stratify_list) {
	
	results <- list()
	for (sex in names(data_list)) {
		data <- data_list[[sex]]
		
		cancer_list <- cancers[[sex]]
		covars <- covariates[[sex]]
		stratify <- stratify_list[[sex]]
		
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
			
			#cat(sex, canc, "\n")
			#print(cox.zph(cox_adj))
			
			# name the results and store them
			raw_name <- paste0(sex, "_", canc, "_raw")
			adj_name <- paste0(sex, "_", canc, "_adj")
			results[[raw_name]] <- cox_raw
			results[[adj_name]] <- cox_adj
		}
	}
	return(results)
}

# Function to extract summary statistics from a Cox model
#receives the normal and relevelled models, exposure with levels of interest, boolena to indicate if 4 levels are present (i. e Normal weight GS)
#returns dataframe with HR, lower/upper,no cases,...
extract_summary <- function(model, model_rl, exposure_variable, level_gs, level_non_gs, lev4=F) {
	
	#construct row names for extraction
	rowname_gs <- paste0(exposure_variable, level_gs)
	rowname_non_gs <- paste0(exposure_variable, level_non_gs)
	rowname_gs_rl <- paste0(exposure_variable, "_rl", level_gs)
	
		
		
	suma <- summary(model)
	suma1 <- summary(model_rl)
	
	# Extract estimates and confidence intervals
	coef_summary <- suma$coefficients
	conf_int <- suma$conf.int
	
	#extract p.dif value + HR
	coef_summary_dif <- suma1$coefficients
	conf_int_dif <- suma1$conf.int
	exp_coef_dif <- coef_summary_dif[rowname_gs_rl,"exp(coef)"]
	pdif <- coef_summary_dif[rowname_gs_rl, "Pr(>|z|)"]
	conf_gs_dif <- conf_int_dif[rowname_gs_rl, c("lower .95", "upper .95")]
	
	
	#extract overweight GS and non-GS values
	coef_gs <- coef_summary[rowname_gs, c("exp(coef)", "Pr(>|z|)")]
	coef_non_gs <- coef_summary[rowname_non_gs, c("exp(coef)", "Pr(>|z|)")]
	conf_gs <- conf_int[rowname_gs, c("lower .95", "upper .95")]
	conf_non_gs <- conf_int[rowname_non_gs, c("lower .95", "upper .95")]
	conc <- summary(model)$concordance[1]  # C-statistic
	cases <- summary(model)$nevent
	
	#in case of 4 level var extract Normal weight GS and return
	if(lev4 == T){
		rowname_normgs <- paste0(exposure_variable, "Normal weight GS")
		coef_normgs <- coef_summary[rowname_normgs, c("exp(coef)", "Pr(>|z|)")]
		conf_normgs <- conf_int[rowname_normgs, c("lower .95", "upper .95")]
		
		return(data.frame(
			Category   = c( "Normal weight GS","GS", "non-GS"),
			exp.coef   = c(coef_normgs["exp(coef)"], coef_gs["exp(coef)"], coef_non_gs["exp(coef)"]),
			lower.95   = c(conf_normgs["lower .95"], conf_gs["lower .95"], conf_non_gs["lower .95"]),
			upper.95   = c(conf_normgs["upper .95"] ,conf_gs["upper .95"], conf_non_gs["upper .95"]),
			p.value   = c(coef_normgs["Pr(>|z|)"], coef_gs["Pr(>|z|)"], coef_non_gs["Pr(>|z|)"]),
			exp.coef.dif= c(NA, exp_coef_dif, NA),
			lower.95.dif    = c(NA, conf_gs_dif["lower .95"], NA),
			upper.95.dif    = c(NA, conf_gs_dif["upper .95"], NA),
			p.difference    = c(NA, as.numeric(pdif), NA),
			concordance = rep(conc, 3),
			cases = rep(cases,3)
			)
		)
		
	}
	
	return(data.frame(
		Category   = c("GS", "non-GS"),
		exp.coef   = c(coef_gs["exp(coef)"], coef_non_gs["exp(coef)"]),
		lower.95   = c(conf_gs["lower .95"], conf_non_gs["lower .95"]),
		upper.95   = c(conf_gs["upper .95"], conf_non_gs["upper .95"]),
		p.value   = c(coef_gs["Pr(>|z|)"], coef_non_gs["Pr(>|z|)"]),
		exp.coef.dif= c(exp_coef_dif, NA),
		lower.95.dif    = c(conf_gs_dif["lower .95"], NA),
		upper.95.dif    = c(conf_gs_dif["upper .95"], NA),
		p.difference    = c(as.numeric(pdif), ""),
		concordance = rep(conc, 2),
		cases = rep(cases,2)
	))
}

# Function to apply summary extraction to all models and combine into one data frame
#receives lists of all normal and relevelled models, ...
#calls extract_summary for each pair of models and returns summary_df over all models
summarize_models <- function(results, results_rl, exposure_variable, level_gs, level_non_gs, lev4=F) {


	summary_list <- mapply(extract_summary, results, results_rl, 
																								MoreArgs = list(exposure_variable = exposure_variable,
																																								level_gs = level_gs, 
																																								level_non_gs = level_non_gs,
																																								lev4 = lev4),
																								SIMPLIFY = FALSE)

	
	# Add model name column to each
	for (i in seq_along(summary_list)) {
		summary_list[[i]]$Model <- names(summary_list)[i]
	}
	
	# Bind to df
	summary_df <- do.call(rbind, summary_list)
	rownames(summary_df) <- NULL
	
	
	#make sex, adj, cancer columns based on modelname
	summary_df$Sex <- sub("_.*", "", summary_df$Model)
	summary_df$Adjustment <- sub("^.*_", "", summary_df$Model)
	summary_df$Cancer <- sub("^[^_]*_(.*)_.*$", "\\1", summary_df$Model)
	
	round_cols <- c("exp.coef", "lower.95", "upper.95", "concordance", "exp.coef.dif", "lower.95.dif", "upper.95.dif")
	summary_df[round_cols] <- format(round(summary_df[round_cols], 2), nsmall =2)
	
	summary_df$p.value <- format(round(summary_df$p.value, 3), nsmall =3)
	summary_df$p.value[summary_df$p.value == 0] <- "< 0.001"
	
	
	summary_df$p.difference[summary_df$p.difference <= 0.001] <- "< 0.001"

	summary_df$p.difference <- sapply(summary_df$p.difference, function(x) {
		if (is.numeric(as.numeric(x))) {
			return(round(as.numeric(x), 3))
		} else {
			return(x)
		}
	})
	

	return(summary_df)
}

# Function to test likelihood ratio of model vs moel with interaction
#receives model parameters, variable to use for interaction with main exposure
# returns result of LRT
get_lr_interaction <- function(data,exposure, canc, covars, inter, stratify){
	
		covars_no_inter <- covars[covars != inter]
		
		# Create the right-hand side of the model with interaction
		rhs_interaction <- paste0(inter, "*", exposure)
		rhs_covars <- paste(covars_no_inter, collapse = " + ")
		rhs_strata <- paste("strata(", paste(stratify, collapse = ", "), ")", sep = "")
		
		formula_int <- as.formula(
			paste("Surv(age_rec, age_exit_cancer,", canc, ") ~",
									paste(c(rhs_interaction, rhs_covars, rhs_strata), collapse = " + "))
		)
		
		# No interaction formula
		formula_no_int <- as.formula(
			paste("Surv(age_rec, age_exit_cancer,", canc, ") ~",
									paste(c(exposure, covars, rhs_strata), collapse = " + "))
		)

		cox_no_int <- coxph(formula_no_int, data = data)
		cox_int <- coxph(formula_int, data = data)

		res <- anova(cox_int, cox_no_int, test= "LRT")

		return(res)
	}

#function to add teh cases in each exposure category to the summary df
add_cases_to_summary <-function(summary_df, data_list, exposure, level_GS, level_non_GS, lev4=F){
	
	for(row in 1:nrow(summary_df)){
		sex <- summary_df[row, "Sex"]
		cancer <- summary_df[row, "Cancer"]
		df <- data_list[[sex]]
		cases_gs <- sum(df[[cancer]][df[[exposure]] == level_GS])
		cases_ngs <- sum(df[[cancer]][df[[exposure]] == level_non_GS])
	
		
		if(lev4 ==T){
			cases_normgs <- sum(df[[cancer]][df[[exposure]] == "Normal weight GS"])
			summary_df[row, "cases"] <- paste (summary_df[row, "cases"], cases_normgs, cases_gs, cases_ngs, sep = " ")
		}else{summary_df[row, "cases"] <- paste (summary_df[row, "cases"], cases_gs, cases_ngs, sep = " ")}
	}
	return(summary_df)
}

# function to run analysis with lag time
#runs models and creates summary with lag time included 
run_with_lag <- function(data_and_cancers_list, exposure_variable, level_gs, level_non_gs,
																									covariates, stratify_list){
	
	data_list <- data_and_cancers_list$data
	cancers <- data_and_cancers_list$cancers
	exposure_rl <- paste0(exposure_variable, "_rl")
	
	#add lag time, exclude cancers
	for (sex in names(data_list)) {
		print(nrow(data_list[[sex]]))
		data_list[[sex]]$age_lag <- data_list[[sex]]$age_rec + 2
		data_list[[sex]] <- data_list[[sex]][data_list[[sex]]$age_exit_cancer > data_list[[sex]]$age_lag, ]
		print(nrow(data_list[[sex]]))
	}
	
	print("running cox models")
	# Run the Cox models
	results <- list()
	for (sex in names(data_list)) {
		data <- data_list[[sex]]
		
		cancer_list <- cancers[[sex]]
		covars <- covariates[[sex]]
		stratify <- stratify_list[[sex]]
		
		cancer_list <- cancers[[sex]]
		covars <- covariates[[sex]]
		stratify <- stratify_list[[sex]]
		
		for (canc in cancer_list) {
			
			
			# Create the formulas for raw and adjusted models
			formula_raw <- as.formula(
				paste("Surv(age_lag, age_exit_cancer,", canc, ") ~", exposure_variable, 
										paste("+ strata(", paste(stratify, collapse = ", "), ")", sep = "")
				)
			)
			
			
			formula_adj <- as.formula(
				paste("Surv(age_lag, age_exit_cancer,", canc, ") ~", exposure_variable, "+", 
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
	
	print("running cox models releveled")
	results_rl <- list()
	for (sex in names(data_list)) {
		data <- data_list[[sex]]
		
		cancer_list <- cancers[[sex]]
		covars <- covariates[[sex]]
		stratify <- stratify_list[[sex]]
		
		cancer_list <- cancers[[sex]]
		covars <- covariates[[sex]]
		stratify <- stratify_list[[sex]]
		
		for (canc in cancer_list) {
			
			
			# Create the formulas for raw and adjusted models
			formula_raw <- as.formula(
				paste("Surv(age_lag, age_exit_cancer,", canc, ") ~", exposure_rl, 
										paste("+ strata(", paste(stratify, collapse = ", "), ")", sep = "")
				)
			)
			
			
			formula_adj <- as.formula(
				paste("Surv(age_lag, age_exit_cancer,", canc, ") ~", exposure_rl, "+", 
										paste(covars, collapse = " + "), 
										paste("+ strata(", paste(stratify, collapse = ", "), ")", sep = "")
				))
			
			
			# Fit the Cox models
			cox_raw <- coxph(formula_raw, data = data)
			cox_adj <- coxph(formula_adj, data = data)
			
			# Dynamically name the results and store them
			raw_name <- paste0(sex, "_", canc, "_raw")
			adj_name <- paste0(sex, "_", canc, "_adj")
			results_rl[[raw_name]] <- cox_raw
			results_rl[[adj_name]] <- cox_adj
		}
	}
	
	
	print("summarizing results")
	# Extract summary statistics from all models
	summary_df <- summarize_models(results, results_rl, exposure_variable, level_gs, level_non_gs)
	
	summary_df <- add_cases_to_summary(summary_df, data_list, exposure_variable, level_gs, level_non_gs)
	
	summary_df <- summary_df[order(summary_df$Cancer,
																																summary_df$Sex,
																																summary_df$Adjustment, 
																																summary_df$Category), ]
	
	
	
	
	# Return all results as a list
	return(list(
		models = results,
		summary = summary_df
	))
	
	
}

#function to run the  analysis--------------------------------------------------
#calls above functions to run all models and summarize the results. 
run_analysis <- function(data_and_cancers_list, exposure_variable, level_gs, level_non_gs,
																									covariates, stratify, lev4 = F) {
	
	
	data_list <- data_and_cancers_list$data
	cancers <- data_and_cancers_list$cancers
	
	#remove unneceessary level
	if (exposure_variable == "bmi_bil_cat_obese"){
		data_list$Female <- data_list$Female[data_list$Female$bmi_bil_cat_obese != "overweight", ]
		data_list$Male <- data_list$Male[data_list$Male$bmi_bil_cat_obese != "overweight", ]
	}
	
	exposure_rl <- paste0(exposure_variable, "_rl")
	print("running cox models")
	# Run the Cox models
	results <- run_cox_models(data_list, cancers, exposure_variable, covariates, stratify)
	
	print("running cox models releveled")
	results_rl <- run_cox_models(data_list, cancers, exposure_rl, covariates, stratify)
	
	print("summarizing results")
	# Extract summary statistics from all models
	summary_df <- summarize_models(results, results_rl, exposure_variable, level_gs, level_non_gs, lev4)
	
	summary_df <- add_cases_to_summary(summary_df, data_list, exposure_variable, level_gs, level_non_gs, lev4)

	summary_df <- summary_df[order(summary_df$Cancer,
																																summary_df$Sex,
																																summary_df$Adjustment, 
																																summary_df$Category), ]
	
	

	
	# Return all results as a list
	return(list(
		models = results,
		summary = summary_df
	))
}



#################################################################################
### main------------------------------------------------------------------------
################################################################################

#prep data and variables
data_and_cancers_list <- prepare_data("working_file.rds") # contains 2 datasets (male and female) as well as the list of cancers for both sexes

covariates_m <- c( "height", "alc_stat", "smoke_stat", "pa_min_per_week_MET_quint", "score_diet", "college_degree")
covariates_f <- c( "height", "alc_stat", "smoke_stat", "pa_min_per_week_MET_quint", "score_diet", "college_degree")

covariates <- list(Female = covariates_f, Male = covariates_m)

stratify_f <- c("age_cat", "centre" , "ever_hrt")
stratify_m <- c("age_cat", "centre")

stratify <- list(Female = stratify_f, Male = stratify_m)

##covariates for missing indicator mehtod (sensitivity)
# covariates_m <- c("height", "height_missing", "alc_stat", "smoke_stat", "pa_min_per_week_MET_sd", "pa_min_per_week_MET_missing","score_diet", "college_degree")
# covariates_f <- c("height", "height_missing","alc_stat", "smoke_stat", "pa_min_per_week_MET_sd", "pa_min_per_week_MET_missing", "score_diet", "college_degree", "ever_hrt")
# 
# covariates <- list(Female = covariates_f, Male = covariates_m)


#run different analyses

results_bil_cat2 <- run_analysis(data_and_cancers_list, exposure_variable = "bmi_bil_cat2", level_gs = "Overweight GS", level_non_gs = "Overweight non-GS", covariates=covariates, stratify = stratify)
results_bil_cat3 <- run_analysis(data_and_cancers_list, exposure_variable = "bmi_bil_cat_obese", level_gs= "Obese GS", level_non_gs = "Obese non-GS", covariates=covariates, stratify = stratify)
#results_bil_cat1 <- run_analysis(data_and_cancers_list, "bmi_bil_cat", level_gs= "Overweight GS", level_non_gs = "Overweight non-GS", covariates=covariates, stratify = stratify)
#results_bil_cat4 <- run_analysis(data_and_cancers_list, exposure_variable = "bmi_bil_cat4", level_gs = "Overweight GS", level_non_gs = "Overweight non-GS", covariates=covariates, stratify = stratify, lev4 = T)
#results_bil_catob4 <- run_analysis(data_and_cancers_list, exposure_variable = "bmi_bil_cat_obese4", level_gs = "Obese GS", level_non_gs = "Obese non-GS", covariates=covariates, stratify = stratify, lev4 = T)
#results_bil_catwc <- run_analysis(data_and_cancers_list, exposure_variable = "bmi_wc_bil_cat_oo", level_gs = "Overweight GS", level_non_gs = "Overweight non-GS", covariates=covariates, stratify = stratify)
#results_bil_cat_tert<- run_analysis(data_and_cancers_list, exposure_variable = "bmi_bil_cat_tert", level_gs = "Overweight GS", level_non_gs = "Overweight non-GS", covariates=covariates, stratify = stratify)
#results_bil_cat_lag2 <- run_with_lag(data_and_cancers_list, exposure_variable = "bmi_bil_cat_obese", level_gs = "Obese GS", level_non_gs = "Obese non-GS", covariates=covariates, stratify = stratify)


#extract summary_df, reorder, write to file
summary_df <- results_bil_cat2$summary

summary_df$HR_CI <- paste0(summary_df$exp.coef, " (", summary_df$lower.95, "-", summary_df$upper.95, ")")
summary_df$HR_CI_dif <- paste0(summary_df$exp.coef.dif, " (", summary_df$lower.95.dif, "-", summary_df$upper.95.dif, ")")
summary_df <- summary_df[order(
	summary_df$Cancer,
	summary_df$Sex,
	summary_df$Adjustment), ]
summary_df <- summary_df %>%
	select(Cancer, Sex, cases, Adjustment, everything())


################################################################################
###plottting
################################################################################




# All cancer sites, Male and Female GS, nGS vs normal ------------------------------------

# cancer sites def
plot_sites<- c( "breast_post", "endometrial", "ovary", "crc", "pancreas" ,	
																"esophagus",	"kidney",  "thyroid", "gallbladder",
																"intrahep_bile_duct","multiple_myeloma", "hepatocellular")

#filter df for cancer sites
plot_sites_df <- summary_df %>%
	filter(grepl("adj$", Model)) %>%
	filter(grepl(paste(plot_sites, collapse = "|"), Model)) %>%
	arrange(Model)

#label cancers
plot_sites_df$Cancer <- factor(
	plot_sites_df$Cancer,
	levels = c( "hepatocellular","kidney", "multiple_myeloma","intrahep_bile_duct","crc","pancreas",	"gallbladder",   "esophagus","thyroid",  
													"endometrial", "ovary",  "breast_post" ),
	
	labels = c( "Hepatocellular Carcinoma","Kidney","Multiple Myeloma", "Intrahepatic Bile Duct","Colorectal","Pancreas", "Gallbladder", "Esophageal", "Thyroid", 
													"Endometrial", "Ovary", "Breast (post menopause)" )
)
#old order of plotting
plot_sites_df <- plot_sites_df[order(plot_sites_df$Cancer), ]

#plot by HR lowest to highest, keep cancers together
cancer_order <- plot_sites_df %>%
	group_by(Cancer) %>%
	summarise(mean_exp_coef = mean(as.numeric(exp.coef), na.rm = TRUE)) %>%
	arrange(mean_exp_coef) %>%
	pull(Cancer)
plot_sites_df <- plot_sites_df %>%
	mutate(Cancer = factor(Cancer, levels = cancer_order)) %>%
	arrange(Cancer)

#extract total cases from cases (first element)
split_cases <- strsplit(as.character(plot_sites_df$cases), " ")
first_elements <- sapply(split_cases, function(x) x[1])

#make df for plotting
plot_data <- data.frame(
	cancer = c(as.character(plot_sites_df$Cancer)),
	Category = c(as.character(plot_sites_df$Category)),
	Cases= c (as.character(first_elements)),
	mean = c(plot_sites_df$exp.coef),
	lower = c(plot_sites_df$lower.95),
	upper = c(plot_sites_df$upper.95),
	sex = c(as.character(plot_sites_df$Sex)),
	hr = c( as.character(plot_sites_df$HR_CI))
)

#plot_data$cancer[seq(2, nrow(plot_data), by = 2)] <- NA
plot_data$Cases[seq(2, nrow(plot_data), by = 2)] <- NA

# Split by sex
fp_f_df <- plot_data |> filter(sex == "Female")
fp_m_df <- plot_data |> filter(sex == "Male")


fp_f_df <- fp_f_df |> mutate(
	mean = as.numeric(mean),
	lower = as.numeric(lower),
	upper = as.numeric(upper)
)

fp_m_df <- fp_m_df |> mutate(
	mean = as.numeric(mean),
	lower = as.numeric(lower),
	upper = as.numeric(upper)
)

cancer_order <- fp_f_df %>%
	group_by(cancer) %>%
	summarise(mean_exp_coef = mean(as.numeric(mean), na.rm = TRUE)) %>%
	arrange(mean_exp_coef) %>%
	pull(cancer)
fp_f_df <- fp_f_df %>%
	mutate(cancer = factor(cancer, levels = cancer_order)) %>%
	arrange(cancer)

fp_f_df$cancer[seq(2, nrow(fp_f_df), by = 2)] <- NA

cancer_order <- fp_m_df %>%
	group_by(cancer) %>%
	summarise(mean_exp_coef = mean(as.numeric(mean), na.rm = TRUE)) %>%
	arrange(mean_exp_coef) %>%
	pull(cancer)
fp_m_df <- fp_m_df %>%
	mutate(cancer = factor(cancer, levels = cancer_order)) %>%
	arrange(cancer)

fp_m_df$cancer[seq(2, nrow(fp_m_df), by = 2)] <- NA

maxi <- round(max(as.numeric(plot_data$mean), na.rm =T), 0)

fp_f <- fp_f_df |>
	group_by(Category) |>
	forestplot( 
		labeltext = c(cancer, Cases, hr),
		mean = mean,
		lower=lower,
		upper=upper, 
		zero =1, 
		fn.ci_norm = c(fpDrawNormalCI, fpDrawNormalCI),
		ci.vertices = TRUE,
		ci.vertices.height = 0.05,
		boxsize = .3,
		clip = c(0.5, maxi),
		col =fpColors(box = c(rgb(0, 0, 1),rgb(0.5, 0.5, 0.5)), lines = c("#CCCCCC", "#CCCCCC")),
		title = "Women",
		legend = c("GS", "non-GS"),
		legend_args = fpLegend(pos = list(x = 1, y = 0.91),
																									gp = gpar(col = "#CCCCCC", fill = "#F9F9F9")),
		)|>
	fp_add_header(cancer = "Cancer", Cases ="Cases",  hr = "HR")|>
	fp_decorate_graph(
		graph.pos = 3)
													
fp_m <- fp_m_df |>
	group_by(Category) |>
	forestplot( 
		labeltext = c(cancer, Cases, hr),
		mean = mean,
		lower=lower,
		upper=upper, 
		zero =1, 
		fn.ci_norm = c(fpDrawCircleCI, fpDrawCircleCI),
		ci.vertices = TRUE,
		ci.vertices.height = 0.05,
		boxsize = .3,
		clip = c(0.5, maxi),
		col =fpColors(box = c(rgb(0, 0, 1),rgb(0.5, 0.5, 0.5)), lines = c("#CCCCCC", "#CCCCCC")),
		title = "Men",
		legend = c("GS", "non-GS"),
		legend_args = fpLegend(pos = list(x = 1, y = 0.89),
																									gp = gpar(col = "#CCCCCC", fill = "#F9F9F9")),
	)|>
	fp_add_header(cancer = "Cancer", Cases ="Cases",  hr = "HR")|>
	fp_decorate_graph(
		graph.pos = 3)

grid.newpage()
borderWidth <- unit(1, "pt")
width <- unit(convertX(unit(1, "npc") - borderWidth, unitTo = "npc", valueOnly = TRUE)/2, "npc")
pushViewport(viewport(layout = grid.layout(nrow = 1,
																																											ncol = 3,
																																											widths = unit.c(width,
																																																											borderWidth,
																																																											width))
)
)
pushViewport(viewport(layout.pos.row = 1,
																						layout.pos.col = 1))
fp_f
upViewport()
pushViewport(viewport(layout.pos.row = 1,
																						layout.pos.col = 2))
grid.rect(gp = gpar(fill = "#dddddd", col = "#eeeeee"))
upViewport()
pushViewport(viewport(layout.pos.row = 1,
																						layout.pos.col = 3))
fp_m
upViewport(2)


# All cancer sites, Male and Female GS vs nGS ----------------------------------

plot_sites_df2 <- plot_sites_df [ plot_sites_df$Category == "GS", ]

#extract total cases from cases (first element)
split_cases <- strsplit(as.character(plot_sites_df2$cases), " ")
first_elements <- sapply(split_cases, function(x) x[1])

#make df for plotting


cancer_order <- plot_sites_df2 %>%
	group_by(Cancer) %>%
	summarise(mean_exp_coef = mean(as.numeric(exp.coef.dif), na.rm = TRUE)) %>%
	arrange(mean_exp_coef) %>%
	pull(Cancer)
plot_sites_df2 <- plot_sites_df2 %>%
	mutate(Cancer = factor(Cancer, levels = cancer_order)) %>%
	arrange(Cancer)

plot_data <- data.frame(
	cancer = c(as.character(plot_sites_df2$Cancer)),
	Category = c( as.character(plot_sites_df2$Category)),
	Cases= c( as.character(first_elements)),
	mean = c( plot_sites_df2$exp.coef.dif),
	lower = c( plot_sites_df2$lower.95.dif),
	upper = c(plot_sites_df2$upper.95.dif),
	sex = c(as.character(plot_sites_df2$Sex)),
	hr = c( as.character(plot_sites_df2$HR_CI_dif))
)
# Split by sex
fp_f_df <- plot_data |> filter(sex == "Female")
fp_m_df <- plot_data |> filter(sex == "Male")


fp_f_df <- fp_f_df |> mutate(
	mean = as.numeric(mean),
	lower = as.numeric(lower),
	upper = as.numeric(upper)
)

fp_f_df <- fp_f_df[order(fp_f_df$mean),]

fp_m_df <- fp_m_df |> mutate(
	mean = as.numeric(mean),
	lower = as.numeric(lower),
	upper = as.numeric(upper)
)
fp_m_df <- fp_m_df[order(fp_m_df$mean),]



maxi <- round(max(as.numeric(plot_data$mean), na.rm =T), 0)

if (maxi <= max(as.numeric(plot_data$mean))) {
	maxi <- maxi + 0.5
}

fp_f <- fp_f_df |>
	forestplot( 
		labeltext = c(cancer, Cases, hr),
		mean = mean,
		lower=lower,
		upper=upper, 
		zero =1, 
		fn.ci_norm = fpDrawNormalCI,
		ci.vertices = TRUE,
		ci.vertices.height = 0.05,
		boxsize = .2,
		clip = c(0.5, maxi),
		col =fpColors(box = rgb(0, 0, 1), lines ="#CCCCCC"),
		title = "Women"
	)|>
	fp_add_header(cancer = "Cancer", Cases ="Cases",  hr = "HR")|>
	fp_decorate_graph(
		graph.pos = 3)

fp_m <- fp_m_df |>
	forestplot( 
		labeltext = c(cancer, Cases, hr),
		mean = mean,
		lower=lower,
		upper=upper, 
		zero =1, 
		fn.ci_norm = fpDrawCircleCI,
		ci.vertices = TRUE,
		ci.vertices.height = 0.05,
		boxsize = .2,
		clip = c(0.5, maxi),
		col =fpColors(box = rgb(0, 0, 1), lines = "#CCCCCC"),
		title = "Men"
	)|>
	fp_add_header(cancer = "Cancer", Cases ="Cases",  hr = "HR")|>
	fp_decorate_graph(
		graph.pos = 3)

grid.newpage()
borderWidth <- unit(1, "pt")
width <- unit(convertX(unit(1, "npc") - borderWidth, unitTo = "npc", valueOnly = TRUE)/2, "npc")
pushViewport(viewport(layout = grid.layout(nrow = 1,
																																											ncol = 3,
																																											widths = unit.c(width,
																																																											borderWidth,
																																																											width))
)
)
pushViewport(viewport(layout.pos.row = 1,
																						layout.pos.col = 1))
fp_f
upViewport()
pushViewport(viewport(layout.pos.row = 1,
																						layout.pos.col = 2))
grid.rect(gp = gpar(fill = "#dddddd", col = "#eeeeee"))
upViewport()
pushViewport(viewport(layout.pos.row = 1,
																						layout.pos.col = 3))
fp_m
upViewport(2)




################################################################################
##########################stuff#################################################
################################################################################

#run model manualy, test assumptions--------------------------------------------

#obesity cancer
cox <- coxph(formula = Surv(age_rec, age_exit_cancer, obesity_cancer) ~ bmi_bil_cat2 + height + alc_stat + smoke_stat + pa_min_per_week_MET_quint+ score_diet + college_degree + strata (age_cat, centre), data= data_and_cancers_list$data$Male)
ph_test<- cox.zph(cox)
plot(ph_test, var = "height")

cox <- coxph(formula = Surv(age_rec, age_exit_cancer, obesity_cancer) ~ bmi_bil_cat2 + height + alc_stat + smoke_stat + pa_min_per_week_MET_quint+ score_diet + college_degree + ever_hrt+ strata (age_cat, centre), data= data_and_cancers_list$data$Female)
ph_test<- cox.zph(cox)
ph_testplot(ph_test, var = "ever_hrt")

#total cancer
cox <- coxph(formula = Surv(age_rec, age_exit_cancer, all_cancer) ~ bmi_bil_cat2 + height + alc_stat + smoke_stat + pa_min_per_week_MET_quint+ score_diet + college_degree + strata (age_cat, centre), data= data_and_cancers_list$data$Male)
ph_test<- cox.zph(cox)
plot(ph_test, var = "pa_min_per_week_MET_quint")

cox <- coxph(formula = Surv(age_rec, age_exit_cancer, all_cancer) ~ bmi_bil_cat2 + height + alc_stat + smoke_stat + pa_min_per_week_MET_quint+ score_diet + college_degree + ever_hrt+ strata (age_cat, centre), data= data_and_cancers_list$data$Female)
ph_test<- cox.zph(cox)
plot(ph_test, var = "smoke_stat")

#hormonal cancer
cox <- coxph(formula = Surv(age_rec, age_exit_cancer, hormonal) ~ bmi_bil_cat2 + height + alc_stat + smoke_stat + pa_min_per_week_MET_quint+ score_diet + college_degree + ever_hrt+ strata (age_cat, centre), data= data_and_cancers_list$data$Female)
ph_test<- cox.zph(cox)

#nonhormonal cancer
cox <- coxph(formula = Surv(age_rec, age_exit_cancer, obesity_non_hormonal) ~ bmi_bil_cat2 + height + alc_stat + smoke_stat + pa_min_per_week_MET_quint+ score_diet + college_degree + ever_hrt+ strata (age_cat, centre), data= data_and_cancers_list$data$Female)
ph_test<- cox.zph(cox)
plot(ph_test, var = "ever_hrt")

#coxph(formula = Surv(age_rec, age_exit_cancer, obesity_cancer) ~ bmi_bil_cat2 + height + alc_stat + smoke_stat + pa_min_per_week_MET_quint+ score_diet + college_degree + stratify (age_cat, centre), data= data_and_cancers_list$data$Male)

#write.csv(summary_df, file = "../results/summary_cox_obese_4l_correct.csv", row.names=F)

# # All cancer sites, Male and Female GS, nGS vs normal ------------------------------------
# 
# # cancer sites def
# plot_sites<- c( "hepatocellular","breast_post", "endometrial", "ovary", "crc", "pancreas" ,	
# 																"esophagus",	"kidney",  "thyroid", "gallbladder",
# 																"intrahep_bile_duct","multiple_myeloma")
# 
# #filter df for cancer sites
# plot_sites_df <- summary_df %>%
# 	filter(grepl("adj$", Model)) %>%
# 	filter(grepl(paste(plot_sites, collapse = "|"), Model)) %>%
# 	arrange(Model)
# 
# #label cancers
# plot_sites_df$Cancer <- factor(
# 	plot_sites_df$Cancer,
# 	levels = c( "hepatocellular","kidney", "multiple_myeloma","intrahep_bile_duct","crc","pancreas",	"gallbladder",   "esophagus","thyroid",  
# 													 "endometrial", "ovary",  "breast_post" ),
# 	
# 	labels = c( "Hepatocellular Carcinoma","Kidney","Multiple Myeloma", "Intrahepatic Bile duct","Colorectal","Pancreas", "Gallbladder", "Esophageal", "Thyroid", 
# 												  "Endometrial", "Ovary", "Breast (post menopause)" )
# )
# #old order of plotting
# plot_sites_df <- plot_sites_df[order(plot_sites_df$Cancer), ]
# 
# #plot by HR lowest to highest, keep cancers together
# cancer_order <- plot_sites_df %>%
# 	group_by(Cancer) %>%
# 	summarise(mean_exp_coef = mean(as.numeric(exp.coef), na.rm = TRUE)) %>%
# 	arrange(mean_exp_coef) %>%
# 	pull(Cancer)
# plot_sites_df <- plot_sites_df %>%
# 	mutate(Cancer = factor(Cancer, levels = cancer_order)) %>%
# 	arrange(Cancer)
# 
# #extract total cases from cases (first element)
# split_cases <- strsplit(as.character(plot_sites_df$cases), " ")
# first_elements <- sapply(split_cases, function(x) x[1])
# 
# #make df for plotting
# plot_data <- data.frame(
# 	cancer = c(as.character(plot_sites_df$Cancer)),
# 	Category = c( as.character(plot_sites_df$Category)),
# 	Cases= c(as.character(first_elements)),
# 	mean = c(plot_sites_df$exp.coef),
# 	lower = c(plot_sites_df$lower.95),
# 	upper = c(plot_sites_df$upper.95),
# 	sex = c(as.character(plot_sites_df$Sex)),
# 	hr = c(as.character(plot_sites_df$HR_CI))
# )
# 
# plot_data$cancer[seq(3, nrow(plot_data), by = 2)] <- NA
# plot_data$Cases[seq(3, nrow(plot_data), by = 2)] <- NA
# 
# # Split by sex
# fp_f_df <- plot_data |> filter(sex == "Female")
# fp_m_df <- plot_data |> filter(sex == "Male")
# 
# #make empty rows for men of hormonal cancers
# # Identify cancers present in females but missing in males
# missing_cancers <- setdiff(fp_f_df$cancer, fp_m_df$cancer)
# 
# for (c in missing_cancers) {
# 	new_row <- fp_m_df[1, ]  # copy structure
# 	new_row[] <- NA          # set all to NA
# 	new_row$cancer <- c      # assign the cancer value
# 	fp_m_df <- bind_rows(fp_m_df, new_row)
# 	fp_m_df <- bind_rows(fp_m_df, new_row)
# }
# 
# colors <- ifelse(fp_f_df$Category == "GS", rgb(0, 0, 1, alpha = 0.7), rgb(0.5, 0.5, 0.5, alpha = 0.3))
# box_styles <- lapply(colors, function(col) gpar(fill = col, col = "#555555"))
# full_box_styles <- c(list(gpar()), box_styles)
# 
# maxi <- round(max(as.numeric(plot_data$mean), na.rm =T), 0)
# # Start new graphics page and layout
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 2, widths = unit(c(0.57, 0.43), "npc"))))
# 
# # --- Female plot
# pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
# 
# forestplot(
# 	labeltext = list( c(fp_txt_bold("Cancer"), fp_f_df$cancer), c(fp_txt_bold("Cases"),fp_f_df$Cases ), c( fp_txt_bold("HR"),fp_f_df$hr )),
# 	mean = c(NA, fp_f_df$mean),
# 	lower = c(NA, fp_f_df$lower),
# 	upper = c(NA, fp_f_df$upper),
# 	fn.ci_norm = fpDrawNormalCI,
# 	lwd.ci = 1,
# 	zero = 1,
# 	xlab = "Hazard Ratio",
# 	boxsize = 0.3,
# 	line.margin = 0.3,
# 	clip = c(0.2, 3),
# 	xticks = seq(0.5,3, by = 0.5),
# 	ci.vertices = TRUE,
# 	ci.vertices.height = 0.05,
# 	hrzl_lines = TRUE,
# 	title = "Women",
# 	new_page = FALSE
# )  |>
# 	 # fp_add_lines(h_2 = gpar(lwd = 1, columns = 1:3, col = "#000044"), 
# 	 # 													h_20 = gpar(lty = 2))|>
# 	fp_set_style(box = full_box_styles, default = gpar(vertices = TRUE))|>
# 	fp_decorate_graph(
# 		graph.pos = 3)
# 
# upViewport()
# 
# # --- Male plot
# pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
# 
# forestplot(
# 	labeltext = list( c(fp_txt_bold("Cases"),fp_m_df$Cases ), c( fp_txt_bold("HR"),fp_m_df$hr )),
# 	mean = c(NA, fp_m_df$mean),
# 	lower = c(NA, fp_m_df$lower),
# 	upper = c(NA, fp_m_df$upper),
# 	fn.ci_norm = fpDrawCircleCI,
# 	zero = 1,
# 	xlab = "Hazard Ratio",
# 	boxsize = 0.1,
# 	line.margin = 0.02,
# 	clip = c(0.2, 3),
# 	xticks = seq(0.5, 3, by = 0.5),
# 	ci.vertices = TRUE,
# 	ci.vertices.height = 0.05,
# 	hrzl_lines = TRUE,
# 	title = "Men",
# 	new_page = FALSE
# ) |>
# 	#fp_add_lines(h_2 = gpar(lwd = 1, columns = 1:2, col = "#000044"))|>
# 	fp_set_style(box = full_box_styles, default = gpar(vertices = TRUE))|>
# 	fp_decorate_graph(
# 		graph.pos = 2)
# 
# upViewport(2)
# 
# # All cancer sites, Male and Female GS vs nGS ----------------------------------
# 
# plot_sites_df2 <- plot_sites_df [ plot_sites_df$Category == "GS", ]
# 
# #extract total cases from cases (first element)
# split_cases <- strsplit(as.character(plot_sites_df2$cases), " ")
# first_elements <- sapply(split_cases, function(x) x[1])
# 
# #make df for plotting
# plot_data <- data.frame(
# 	cancer = c("Cancer",as.character(plot_sites_df2$Cancer)),
# 	Category = c("Category", as.character(plot_sites_df2$Category)),
# 	Cases= c("Cases", as.character(first_elements)),
# 	mean = c(NA, plot_sites_df2$exp.coef.dif),
# 	lower = c(NA, plot_sites_df2$lower.95.dif),
# 	upper = c(NA,plot_sites_df2$upper.95.dif),
# 	sex = c("Sex",as.character(plot_sites_df2$Sex)),
# 	hr = c("HR (95% CI", as.character(plot_sites_df2$HR_CI_dif))
# )
# 
# # Split by sex
# fp_f_df <- plot_data |> filter(sex == "Female")
# fp_m_df <- plot_data |> filter(sex == "Male")
# 
# #make empty rows for men of hormonal cancers
# # Identify cancers present in females but missing in males
# missing_cancers <- setdiff(fp_f_df$cancer, fp_m_df$cancer)
# 
# for (c in missing_cancers) {
# 	new_row <- fp_m_df[1, ]  # copy structure
# 	new_row[] <- NA          # set all to NA
# 	new_row$cancer <- c      # assign the cancer value
# 	fp_m_df <- bind_rows(fp_m_df, new_row)
# }
# 
# colors <- ifelse(fp_f_df$Category == "GS", rgb(0, 0, 1, alpha = 0.7), rgb(0.5, 0.5, 0.5, alpha = 0.3))
# box_styles <- lapply(colors, function(col) gpar(fill = col, col = "#555555"))
# full_box_styles <- c(list(gpar()), box_styles)
# 
# 
# # Start new graphics page and layout
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 2, widths = unit(c(0.57, 0.43), "npc"))))
# 
# # --- Female plot
# pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
# 
# forestplot(
# 	labeltext = list( c(fp_txt_bold("Cancer"), fp_f_df$cancer),  c(fp_txt_bold("Cases"),fp_f_df$Cases ), c( fp_txt_bold("HR"),fp_f_df$hr )),
# 	mean = c(NA, fp_f_df$mean),
# 	lower = c(NA, fp_f_df$lower),
# 	upper = c(NA, fp_f_df$upper),
# 	fn.ci_norm = fpDrawNormalCI,
# 	zero = 1,
# 	xlab = "Hazard Ratio",
# 	boxsize = 0.25,
# 	line.margin = 1,
# 	clip = c(0.5, 5),
# 	xticks = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
# 	ci.vertices = TRUE,
# 	ci.vertices.height = 0.05,
# 	title = "Women",
# 	#hrzl_lines = TRUE,
# 	new_page = FALSE
# )|>
# 	fp_set_style(box = full_box_styles, default = gpar(vertices = TRUE))|>
# 	fp_decorate_graph(
# 		graph.pos = 3)
# 
# upViewport()
# 
# # --- Male plot
# pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
# 
# forestplot(
# 	labeltext = list( c(fp_txt_bold("Cases"),fp_m_df$Cases ), c( fp_txt_bold("HR"),fp_m_df$hr )),
# 	mean = c(NA, fp_m_df$mean),
# 	lower = c(NA, fp_m_df$lower),
# 	upper = c(NA, fp_m_df$upper),
# 	fn.ci_norm = fpDrawCircleCI,
# 	zero = 1,
# 	xlab = "Hazard Ratio",
# 	boxsize = 0.3,
# 	line.margin = 0.1,
# 	clip = c(0.5, 5),
# 	xticks = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
# 	ci.vertices = TRUE,
# 	ci.vertices.height = 0.1,
# 	title = "Men",
# 	new_page = FALSE
# )|>
# 	fp_set_style(box = full_box_styles, default = gpar(vertices = TRUE))|>
# 	fp_decorate_graph(
# 		graph.pos = 2)
# 
# upViewport(2)
# 
# 
# # all cancer, F----------------------------------------------------------------
# 
# # cancer sites def
# plot_sites<- c( "breast_post", "endometrial", "ovary", "crc", "pancreas" ,	
# 																"esophagus",	"kidney","hepatocellular",  "thyroid", "gallbladder",
# 																"intrahep_bile_duct","multiple_myeloma", "obesity_non_hormonal", "hormonal")
# 
# #filter df for cancer sites
# plot_sites_df <- summary_df %>%
# 	filter(grepl("adj$", Model) & Sex == "Female") %>%
# 	filter(grepl(paste(plot_sites, collapse = "|"), Model)) %>%
# 	arrange(Model)
# 
# #label cancers
# plot_sites_df$Cancer <- factor(
# 	plot_sites_df$Cancer,
# 	levels = c( "hepatocellular","kidney", "multiple_myeloma","intrahep_bile_duct","crc","pancreas",	"gallbladder", "esophagus","thyroid", "obesity_non_hormonal",
# 													"endometrial", "ovary",  "breast_post", "hormonal" ),
# 	
# 	labels = c("Hepatocellular Carcinoma", "Kidney","Multiple Myeloma", "Intrahepatic Bile duct","Colorectal","Pancreas", "Gallbladder", "Esophageal", "Thyroid", "Total non-hormonal",
# 												"Endometrial", "Ovary", "Breast (post menopause)", "Total hormonal" )
# )
# 
# plot_sites_df <- plot_sites_df[order(plot_sites_df$Cancer), ]
# 
# #extract total cases from cases (first element)
# split_cases <- strsplit(as.character(plot_sites_df$cases), " ")
# first_elements <- sapply(split_cases, function(x) x[1])
# 
# #make df for plotting
# plot_data <- data.frame(
# 	cancer = c("Cancer",as.character(plot_sites_df$Cancer)),
# 	Category = c("Category", as.character(plot_sites_df$Category)),
# 	Cases= c("Cases", as.character(first_elements)),
# 	mean = c(NA, plot_sites_df$exp.coef.dif),
# 	lower = c(NA, plot_sites_df$lower.95.dif),
# 	upper = c(NA,plot_sites_df$upper.95.dif),
# 	sex = c("Sex",as.character(plot_sites_df$Sex)),
# 	hr = c("HR (95% CI", as.character(plot_sites_df$HR_CI_dif))
# )
# plot_data$sum_bool <- grepl("hormonal", plot_data$cancer)
# 
# 
# plot_data$cancer[seq(3, nrow(plot_data), by = 2)] <- NA
# plot_data$Cases[seq(3, nrow(plot_data), by = 2)] <- NA
# 
# # Split by sex
# fp_f_df <- plot_data |> filter(sex == "Female" & Category == "GS")
# 
# #make empty rows for men of hormonal cancers
# # Identify cancers present in females but missing in males
# missing_cancers <- setdiff(fp_f_df$cancer, fp_m_df$cancer)
# 
# for (c in missing_cancers) {
# 	new_row <- fp_m_df[1, ]  # copy structure
# 	new_row[] <- NA          # set all to NA
# 	new_row$cancer <- c      # assign the cancer value
# 	fp_m_df <- bind_rows(fp_m_df, new_row)
# 	fp_m_df <- bind_rows(fp_m_df, new_row)
# }
# 
# colors <- ifelse(fp_f_df$sum_bool == F, rgb(0, 0, 1, alpha = 0.7), rgb(1,0.5,0.5, alpha = 0.9))
# box_styles <- lapply(colors, function(col) gpar(fill = col, col = "#555555"))
# full_box_styles <- c(list(gpar()), box_styles)
# 
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 1)))
# 
# forestplot(
# 	labeltext = list( c(fp_txt_bold("Cancer"), fp_f_df$cancer), c(fp_txt_bold("Cases"),fp_f_df$Cases ), c( fp_txt_bold("HR"),fp_f_df$hr )),
# 	mean = c(NA, fp_f_df$mean),
# 	lower = c(NA, fp_f_df$lower),
# 	upper = c(NA, fp_f_df$upper),
# 	fn.ci_norm = fpDrawNormalCI,
# 	zero = 1,
# 	xlab = "Hazard Ratio",
# 	boxsize = 0.3,
# 	line.margin = 0.1,
# 	clip = c(0.5, 2.5),
# 	xticks = c(0.5, 1, 1.5, 2, 2.5),
# 	ci.vertices = TRUE,
# 	ci.vertices.height = 0.05,
# 	title = "Women",
# 	new_page = FALSE
# )  |>
# 	fp_add_lines(#h_2 = gpar(lwd = 1, columns = 1:3, col = "#000044"),
# 														h_12 = gpar(lty = 2))|>
# 	fp_set_style(box = full_box_styles, default = gpar(vertices = TRUE))|>
# 	fp_decorate_graph(
# 		graph.pos = 3)
# 
# 
# 
# 
# 
# 
# 

# # GS + non-GS vs normalo--------------
# #female only
# plot_df <- plot_df[ plot_df$Sex == "Female", ]
# 
# #plot GS + non-GS vs normal weight as ref
# cancer_vec <- c(fp_txt_bold("Cancer"), as.character(plot_df$Cancer))
# 
# # Replace every *second* data element (indices 3, 5, 7, ...)
# cancer_vec[seq(3, length(cancer_vec), by = 2)] <- NA
# 
# tabletext <- list(
# 	cancer_vec,
# 	c(fp_txt_bold("Phenotype"), as.character(plot_df$Category)),
# 	c("HR", as.character(plot_df$HR_CI))
# )
# 
# mean  <- c(NA, plot_df$exp.coef)
# lower <- c(NA, plot_df$lower.95)
# upper <- c(NA, plot_df$upper.95)
# 
# # Set colors based on category
# colors <- ifelse(plot_df$Category == "GS", rgb(0, 0, 1, alpha = 0.3), rgb(0.5, 0.5, 0.5, alpha = 0.3))
# box_styles <- lapply(colors, function(col) gpar(fill = col, col = "#555555"))
# full_box_styles <- c(list(gpar()), box_styles)
# 
# forestplot(labeltext = tabletext,
# 											mean      = mean,
# 											lower     = lower,
# 											upper     = upper,
# 											zero      = 1,
# 											xlab      = "Hazard Ratio",
# 											boxsize   = 0.2,
# 											line.margin = 0.1,
# 											clip         = c(0.5, 3),           
# 											xticks       = c(0.5, 1, 2, 3, 4),
# 											ci.vertices = T,
# 											ci.vertices.height = 0.05,
# 											title="Risk of Hormonal and Non-Hormonal Obesity-Related Cancers in Women:\nObese GS/non-GS vs normal weight "
# ) |>
# 	fp_set_style(box = full_box_styles, default = gpar(vertices = TRUE))
# 
# 
# 
# #plot GS with non_GS as ref-----------------
# 
# split_cases <- strsplit(as.character(plot_df1$cases), " ")
# # Extract the first element of each split vector
# first_elements <- sapply(split_cases, function(x) x[1])
# 
# tabletext <- list(
# 	
# 	c("Cancer", as.character(plot_df1$Cancer)), 
# 	c("Cases", as.character(first_elements)),
# 	c("HR", as.character(plot_df1$HR_CI))
# 	
# 	
# )
# 
# 
# 
# mean  <- c(NA, plot_df1$exp.coef.dif)
# lower <- c(NA, plot_df1$lower.95.dif)
# upper <- c(NA, plot_df1$upper.95.dif)
# 
# # Transparent blue fill for boxes
# colors <- rep(rgb(0, 0, 1, alpha = 0.3), nrow(plot_df1))
# box_styles <- lapply(colors, function(col) gpar(fill = col, col = "#555555"))
# full_box_styles <- c(list(gpar()), box_styles)
# 
# forestplot(
# 	labeltext    = tabletext,
# 	mean         = mean,
# 	lower        = lower,
# 	upper        = upper, 
# 	zero         = 1,
# 	xlab         = "Hazard Ratio",
# 	boxsize      = 0.1,
# 	line.margin  = 0.1,
# 	clip         = c(0.5, 1.5),              # <-- Actual axis range
# 	xticks       = c(0.5, 1, 1.5),      # <-- Tick marks
# 	ci.vertices = T,
# 	ci.vertices.height = 0.05,
# 	title="Risk of Hormonal and Non-Hormonal Obesity-Related Cancers in Women:\nObese GS vs. Obese Non-GS"
# )  |>
# 	fp_set_style(
# 		box = full_box_styles,
# 		default = gpar(
# 			vertices      = TRUE
# 		)
# 	)
# 
# 
# 
# 
# 
# # GS + non-GS vs normalo MF------------------
# 
# split_cases <- strsplit(as.character(plot_df_over$cases), " ")
# # Extract the first element of each split vector
# first_elements <- sapply(split_cases, function(x) x[1])
# 
# #forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR)
# 
# 
# # Build the final tibble
# dfHR <- data.frame(
# 	cancer = c("Cancer",as.character(plot_df_over$Cancer)),
# 	Category = c("Category", as.character(plot_df_over$Category)),
# 	Cases= c("Cases", as.character(first_elements)),
# 	meanf = c(NA, ifelse(plot_df_over$Sex == "Female",plot_df_over$exp.coef, NA)),
# 	meanm = c(NA, ifelse(plot_df_over$Sex == "Male",plot_df_over$exp.coef, NA)),
# 	lowerf = c(NA, ifelse(plot_df_over$Sex == "Female",plot_df_over$lower.95, NA)),
# 	lowerm = c(NA, ifelse(plot_df_over$Sex == "Male",plot_df_over$lower.95, NA)),
# 	upperf = c(NA, ifelse(plot_df_over$Sex == "Female",plot_df_over$upper.95, NA)),
# 	upperm = c(NA, ifelse(plot_df_over$Sex == "Male",plot_df_over$upper.95, NA)),
# 	sex = c("Sex",as.character(plot_df_over$Sex)),
# 	hr = c("HR (95% CI", as.character(plot_df_over$HR_CI))
# )
# 
# row_colours <- ifelse(
# 	dfHR$Category == "GS",
# 	rgb(0, 0, 1, alpha = 0.3),      # blueish for GS
# 	rgb(0.5, 0.5, 0.5, alpha = 0.3) # grey for non‑GS
# )
# 
# # 2. Expand to a 2‑column matrix so each sex (column) gets the same row colour
# box_colour_list <- list(
# 	row_colours,  # female column
# 	row_colours   # male column
# )
# 
# 
# forestplot(
# 	labeltext = list(dfHR$cancer, dfHR$sex, dfHR$Category),
# 	mean = cbind(dfHR$meanf, dfHR$meanm),
# 	lower = cbind(dfHR$lowerf, dfHR$lowerm), 
# 	upper = cbind(dfHR$upperf, dfHR$upperm),
# 	fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
# 	zero = 1,
# 	xlab = "Hazard Ratio",
# 	boxsize = 0.2,
# 	line.margin = 0.1,
# 	clip = c(0.8, 1.5),
# 	xticks = c(0.8, 1, 1.5),
# 	ci.vertices = TRUE,
# 	ci.vertices.height = 0.05,
# 	col          = fpColors(box = row_colours),
# 	title = "Risk of Hormonal and Non-Hormonal Obesity-Related Cancers in Men and Women:\nOverweight GS/non-GS vs Normal Weight"
# )
# 
# 
# 
# 
# 
# 
# 

## lr test for interactions of all covars------------------
# results <- list()
# 
# # Counter for results list indexing
# i <- 1
# 
# for (sex in c("Male", "Female")) {
# 	data <- data_and_cancers_list$data[[sex]]
# 	cancers <- data_and_cancers_list$cancers[[sex]]
# 	covars <- covariates[[sex]]
# 	strat <- stratify[[sex]]
# 
# 	for (canc in cancers) {
# 		for (inter in covars) {
# 			lr <- get_lr_interaction(
# 				data = data,
# 				covars = covars,
# 				inter = inter,
# 				canc = canc,
# 				exposure = "bmi_bil_cat2",
# 				stratify = strat
# 			)
# 
# 			if (!is.null(lr)) {
# 				pval <- format(lr$`Pr(>|Chi|)`[2], scientific = FALSE, digits = 4)
# 				results[[i]] <- data.frame(
# 					sex = sex,
# 					cancer = canc,
# 					interaction = inter,
# 					p_value = pval,
# 					stringsAsFactors = FALSE
# 				)
# 				i <- i + 1
# 			}
# 		}
# 	}
# }
# 
# # Combine all rows into a single data frame
# results_df <- do.call(rbind, results)
# 
# # Optional: sort by p-value
# results_df <- results_df[order(results_df$sex, results_df$cancer, results_df$p_value), ]
# 
# # View result
# print(results_df[results_df$p_value <= 0.05,])


##cox model with interaction
# covars <- c( "height", "alc_stat", "smoke_stat", "pa_min_per_week_MET_sd", "score_diet", "college_degree", "ever_hrt")
# formula<- as.formula(
# 	paste("Surv(age_rec, age_exit_cancer,", "obesity_cancer", ") ~ bmi_bil_cat2 +", 
# 							paste(covars, collapse = " + "), 
# 							" + bmi_bil_cat2:ever_hrt",
# 							paste("+ strata(", paste(stratify$Female, collapse = ", "), ")", sep = "")
# 	))
# 
# 
# # Fit the Cox model
# cox <- coxph(formula, data = data_and_cancers_list$data$Female)

