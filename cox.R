rm(list = ls())

setwd("C:/Users/kiliang98/ucloud/Shared/projects_KR/Bilirubin/data")

library(survival)
library(dplyr)

df <- read.csv("working_file_v2.csv")

cancers_f <- c("obesity_cancer", "all_cancer", "breast_post", "endometrial","crc", 
													"esophagus", "gallbladder", "intrahep_bile_duct", "hepatocellular", 
													"kidney", "thyroid", "multiple_myeloma", "ovary", "gastric_cardia")

cancers_m <- c("obesity_cancer", "all_cancer", "crc", "esophagus", "gallbladder", 
															"intrahep_bile_duct", "hepatocellular", "kidney", "thyroid", 
															"multiple_myeloma", "gastric_cardia")

exposures<- c("bmi_bil_cat", "bmi_bil_cat2")
covariates <- c("height", "alc_stat", "smoke_stat", "pa_min_per_week_MET_sd", "college_degree" )

#formatting variables
#calculate pa / sd, otherwise 1 unit increase is too little change to be meaningful.
#use different standard deviations by sex, since the physical activity distributions differ by sex.
df <- df %>%
	group_by(sex) %>%
	mutate(pa_min_per_week_MET_sd = pa_min_per_week_MET / sd(pa_min_per_week_MET, na.rm = TRUE)) %>%
	ungroup()

df$date_recruit <- as.Date(as.character(df$date_recruit), format = "%Y-%m-%d")
df$date_exit_first_cancer <- as.Date(as.character(df$date_exit_first_cancer), format = "%Y-%m-%d")

df$days_followup <-as.numeric(df$date_exit_first_cancer - df$date_recruit)

#alternative phenotype definitons
df <- df %>%
	mutate(
		bmi_bil_cat3 = factor(
			case_when(
				bmi_m >= 16 & bmi_m < 25 ~ 1,
				bmi_m >= 30 & tbili < 10 ~ 2,
				bmi_m >= 30 & tbili >= 10 ~ 3,
				TRUE ~ 9
			),
			levels = c(1, 2, 3, 9),
			labels = c("Normal weights", "Obese non-GS", "Obese GS", "Underweight or overweight")
		)
	)

df1 <- df[df$bmi_bil_cat2 != "Underweight",]
df1 <-  subset(df1, df1$age_exit_cancer >df1$age_rec)


#create subsets
df_f <- subset(df1, df1$sex == "Female")
df_m <- subset(df1, df1$sex == "Male")

dfs <- list(Female = df_f, Male = df_m)

results <- list() 
#create cox modles for each outcome -> always one raw, one adjusted.
for (sex in names(dfs)) {
	
	data <- dfs[[sex]]  # Extract data frame
	
	if (sex == "Female") {
		cancer_list <- cancers_f
		} else {
		cancer_list <- cancers_m
		}
	
	# Cox model loop
	for (canc in cancer_list) {
		formula_raw <- as.formula(paste("Surv(age_rec, age_exit_cancer,", canc, ") ~ bmi_bil_cat2"))
		formula_adj <- as.formula(paste("Surv(age_rec, age_exit_cancer,", canc, ") ~ bmi_bil_cat2 +", paste(covariates, collapse = " + ")))
		
		cox_raw <- coxph(formula_raw, data = data)
		cox_adj <- coxph(formula_adj, data = data)
		
		results[[paste0(sex, "_", canc, "_raw")]] <- cox_raw
		results[[paste0(sex, "_", canc, "_adj")]] <- cox_adj
	}
}

# Clear the file before writing
writeLines("", "outfile.txt")

# Redirect output to the file
sink("outfile.txt")

for (c in names(results)) {
	cat("\n", strrep("=", 50), "\n")  # Add a separator for readability
	cat("Model for:", c, "\n")
	cat(strrep("-", 50), "\n")
	
	print(summary(results[[c]]))
}

sink()  # Stop redirecting output



extract_summary <- function(model) {
	# Extract coefficient summary
	coef_summary <- summary(model)$coefficients
	
	# Extract values for GS and non-GS
	coef_gs <- coef_summary["bmi_bil_cat2Overweight GS", c("exp(coef)", "Pr(>|z|)")]
	coef_non_gs <- coef_summary["bmi_bil_cat2Overweight non-GS", c("exp(coef)", "Pr(>|z|)")]
	
	# Print coefficient summaries for debugging
	print(coef_summary)
	
	# Extract confidence intervals
	conf_int <- summary(model)$conf.int
	conf_gs <- conf_int["bmi_bil_cat2Overweight GS", c("lower .95", "upper .95")]
	conf_non_gs <- conf_int["bmi_bil_cat2Overweight non-GS", c("lower .95", "upper .95")]
	
	# Print confidence intervals for debugging
	print(conf_gs)
	print(conf_non_gs)
	
	# Extract concordance index (C-statistic)
	conc <- summary(model)$concordance[1]  # Get the C-statistic value
	
	# Return the results as a data frame with two rows
	return(data.frame(
		Category = c("GS", "non-GS"), 
		exp.coef = c(coef_gs["exp(coef)"], coef_non_gs["exp(coef)"]),
		p.value = c(coef_gs["Pr(>|z|)"], coef_non_gs["Pr(>|z|)"]),
		lower.95 = c(conf_gs["lower .95"], conf_non_gs["lower .95"]),
		upper.95 = c(conf_gs["upper .95"], conf_non_gs["upper .95"]),
		concordance = rep(conc, 2)  # Same concordance value for both rows
	))
}

# Apply to all models and store results in a list
summary_results <- lapply(results, extract_summary)

# Convert to a single data frame
summary_df <- do.call(rbind, summary_results)
summary_df$Model <- row.names(summary_df)
summary_df$Model <- substr(summary_df$Model, 1, nchar(summary_df$Model) - 2)
print(summary_df)


plot_significant_forest <- function(summary_df){
# Load necessary libraries
library(forestplot)
library(dplyr)
library(grid)  # for gpar()

# Filter the data for models ending with "adj" and where either GS or non-GS has p.value <= 0.05
plot_df <- summary_df %>%
	filter(grepl("adj$", Model)) %>%
	group_by(Model) %>%
	filter(any(p.value <= 0.05)) %>%
	ungroup() %>%
	arrange(Model)

# Create a table of text labels with a header row,
# and truncate the last 4 characters from Model names.
tabletext <- cbind(
	c("Model", substr(as.character(plot_df$Model), 1, nchar(as.character(plot_df$Model)) - 4)),
	c("Category", as.character(plot_df$Category)),
	c("HR", sprintf("%.2f", plot_df$exp.coef)),
	c("95% CI", sprintf("%.2f - %.2f", plot_df$lower.95, plot_df$upper.95))
)

# Create numeric vectors for the hazard ratios and confidence intervals,
# with NA for the header row.
mean  <- c(NA, plot_df$exp.coef)
lower <- c(NA, plot_df$lower.95)
upper <- c(NA, plot_df$upper.95)

# Create a vector of colors based on Category (blue for GS, grey for non-GS)
colors <- ifelse(plot_df$Category == "GS", "blue", "grey")

# Create a list of graphical parameters (gpar) for each box based on the colors.
# This list currently matches the number of data rows.
box_styles <- lapply(colors, function(col) gpar(fill = col, col = "#555555"))

# Prepend a dummy style for the header row so the style vector has the correct length.
full_box_styles <- c(list(gpar()), box_styles)

# Create the forest plot and then set the custom style for each box.
forestplot(labeltext = tabletext,
											mean      = mean,
											lower     = lower,
											upper     = upper,
											zero      = 1,          # reference line at HR = 1
											xlab      = "Hazard Ratio",
											boxsize   = 0.25,       # adjust the box size as desired
											line.margin = 0.1     # avoid crowding of lines
) |>
	fp_set_style(box = full_box_styles, default = gpar(vertices = TRUE))

}

plot_significant_forest(summary_df)
# summary(coxph(as.formula(paste("Surv(age_rec, age_exit_cancer, pancreas ) ~ bmi_bil_cat2 +", paste(covariates, collapse = " + "))), 
#       data = subset(df, df$sex == "Female")))
# 
# 
# summary(coxph(as.formula(paste("Surv(age_rec, age_exit_cancer, pancreas ) ~ tbili +", paste(covariates, collapse = " + "))), 
# 														data = subset(df, df$sex == "Female" & df$bmi_m >= 30 & df$tbili >= 10)))
# 
# 
# 
# cox_model <- coxph(as.formula(paste("Surv(age_rec, age_exit_cancer, breast_post ) ~ tbili")), 
# 																	data = subset(df, df$sex == "Female" ))


