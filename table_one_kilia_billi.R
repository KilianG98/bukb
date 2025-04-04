install.packages("table1")
install.packages("broom")
install.packages("openxlsx")

setwd("C:/Users/reyna/ucloud/Start Kilian Gandolf/Bilirubin/scripts")
################################################descriptive table one#####################################################

#Note: in the UK biobank unit of energy intake is in KJ- I generated a kcal energy variable

# # Create a new variable for energy intake in kcal
# UKB_MM_workmiss$energy_kcal <- UKB_MM_workmiss$energy_intake_overall/ 4.184
# 
# UKB_MM_workmiss[10:20, c('energy_kcal', 'energy_intake_overall' )]

#generating table one (descriptive table) - start with your exposure as continous variable then all your (co-) varaibles and after the " | " you put your categorial variable to stratify through it: in your case: bmi cat and then the bmi_gilbert_cat 
# both for men and women separate
# for specifing amon men or woman you use: data = subset(df, sex == "male") # check the labels- maybe  it is written with a capital letter. 

table_one_billi <- table1( ~  rs_totalhpdi_score_fm  + age_recru +  sex +alcohol_intake_overall + bmi_m_0_0 + smoke_stat +dpr_index + pa_index + energy_kcal +  menopause + use_hrt
                                 | rs_totalhpdi_score_sex, data = UKB_MM_workmiss, caption = "Table 1 hPDI", render.continuous = "Mean (SD)", render.categorical= "PCT")


#also read about the specific render options for continous and categorical 


# save the table as dataset
table_one_billi_df <- as.data.frame(table_one_billi) 

# export it to excel
write.xlsx(table_one_billi_df, file = "table_one_billi_df.xlsx",rowNames = TRUE)
#####################################################################################################################################################
