
rm(list = ls())
setwd("C:/Users/kiliang98/ucloud/Shared/projects_KR/Bilirubin/data")

df <- read.csv("data_w_cancer.csv")
df1 <- read.csv("bili_data_raw_ds.csv")
df <- merge(df, df1[,c("eid", "score_diet")], by = "eid")
names(df)

write.csv(df, "data_w_cancer.csv", row.names = FALSE)
# icd <- read.csv("icd10_raw.csv")
# 
# df1 <- df[,!grepl('^p40006_',colnames(df))]
# 
# df1 <- merge(df1, icd, by= "eid")
# 
# write.csv(df1, "bili_data.csv",row.names = FALSE)




# cncr <- read.csv("cancer_ms/cancer_processed.csv")
# excl <- read.csv("cancer_ms/cancer_excl.csv")
# 
# 
# df <- left_join(df, cncr, by="eid" )
# df <- left_join(df, excl[, c("eid", "inclusionTotal")], by = "eid")
# 
# names(df)[names(df) == 'inclusionTotal'] <- 'inclusion_cancer'
# 
# write.csv(df, "data_w_cancer.csv",row.names = FALSE)
