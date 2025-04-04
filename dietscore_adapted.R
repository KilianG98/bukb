rm(list = ls())

library(dplyr)

setwd("C:/Users/kiliang98/ucloud/Shared/projects_KR/Bilirubin/data")

# Read in data
dataset <- read.csv("bili_data_raw.csv")
cnames <- read.delim("new_vars_names", FALSE)

# # Rename columns
colnames(dataset) <- coalesce(cnames[match(names(dataset), cnames[, 1]), 2], names(dataset))

#vegetables
vegetables1 <-  sapply(dataset$veg_cooked ,as.character)
vegetables1 <- ifelse(vegetables1 =="-3",NA,vegetables1)
vegetables1 <- ifelse(vegetables1 =="-1",NA,vegetables1)

vegetables2 <-  sapply(dataset$veg_saladraw, as.character)
vegetables2 <- ifelse(vegetables2 =="-3",NA,vegetables2)
vegetables2 <- ifelse(vegetables2 =="-1",NA,vegetables2)

vegetables1 = as.numeric(vegetables1)
vegetables2 = as.numeric(vegetables2)
vegetables1<- cut(vegetables1 ,c(-12, 0.1, 1.1, 2.1, 3.1, 4.1, 50), labels=c("0","1","2","3","4","5"), na.rm=TRUE)
vegetables2 <- cut(vegetables2 ,c(-11, 0.1, 1.1, 2.1, 3.1, 4.1, 50), labels=c("0","1","2","3","4","5"), na.rm=TRUE)

vegetables1 = as.numeric(vegetables1)

vegetables2 = as.numeric(vegetables2)

vegetables =rowSums(cbind(vegetables1, vegetables2), na.rm = T )

table(vegetables)

#fruits
fruits1 <-  sapply(dataset$fruit_fresh ,as.character)
fruits1 <- ifelse(fruits1 =="-3",NA,fruits1)
fruits1 <- ifelse(fruits1 =="-1",NA,fruits1)

fruits2 <-  sapply(dataset$dried_fruit_intake ,as.character) #####neeed
fruits2 <- ifelse(fruits2 =="-3",NA,fruits2)
fruits2 <- ifelse(fruits2 =="-1",NA,fruits2)

fruits1 = as.numeric(fruits1)
fruits2 = as.numeric(fruits2)
fruits1 <- cut(fruits1 ,c(-11, 0.1, 1.1, 2.1, 3.1, 4.1, 50), labels=c("0","1","2","3","4","5"), na.rm=TRUE)
fruits2 <- cut(fruits2 ,c(-11,0.1, 1.1, 2.1, 3.1, 4.1, 50), labels=c("0","1","2","3","4","5"), na.rm=TRUE)


fruits =rowSums(cbind(fruits1, fruits2), na.rm = T )

table(fruits)

#fish

oilfish <-  sapply(dataset$fish_oil  ,as.character)
oilfish <- ifelse(oilfish =="-3",NA,oilfish)
oilfish <- ifelse(oilfish =="-1",NA,oilfish)

nonoilfish <-  sapply(dataset$fish_nonoil ,as.character)
nonoilfish <- ifelse(nonoilfish =="-3",NA, nonoilfish)
nonoilfish <- ifelse(nonoilfish =="-1",NA, nonoilfish)

oilfish = as.numeric(oilfish)
nonoilfish = as.numeric(nonoilfish)

fish =rowSums(cbind(oilfish, nonoilfish), na.rm = T )

#0	Never
#1	Less than once a week
#2	Once a week
#3	2-4 times a week
#4	5-6 times a week
#5	Once or more daily
#-1	Do not know
#-3	Prefer not to answer et j'ai fait la somme des deux types de poissons 

#Processed meat 

processedmeat <-  sapply(dataset$processed_meat ,as.character)
table(processedmeat )
processedmeat<- ifelse(processedmeat=="-3",NA, processedmeat)
processedmeat<- ifelse(processedmeat=="-1",NA, processedmeat)
processedmeat= as.numeric(processedmeat)
processedmeat= as.numeric(processedmeat)

#0	Never
#1	Less than once a week
#2	Once a week
#3	2-4 times a week
#4	5-6 times a week
#5	Once or more daily
#-1	Do not know
#-3	Prefer not to answer et j'ai fait la somme des deux types de poissons 

# Red meat 
#beef, pork, lamb

beef <-  sapply( dataset$meat_beef ,as.numeric)
beef <- ifelse( beef =="-3",NA, beef)
beef <- ifelse( beef =="-1",NA, beef)
summary( beef)

lamp <-  sapply( dataset$meat_lamb ,as.character)
lamp <- ifelse( lamp =="-3",NA, lamp)
lamp <- ifelse( lamp =="-1",NA, lamp)

pork <-  sapply( dataset$meat_pork ,as.character)
pork <- ifelse( pork =="-3",NA, pork)
pork <- ifelse( pork =="-1",NA, pork)

beef = as.numeric( beef)
lamp = as.numeric( lamp)
pork = as.numeric( pork)

redmeat =rowSums(cbind(beef, lamp, pork), na.rm = T )

# Whole grains definitions -  Whole grains: ≥ 3servings/day & Refined grains: ≤1.5servings/day
# I did not consider brown bread, should I ? Ask H 
###BREAD

#perweek
bread_intake <-  sapply( dataset$bread_intake ,as.numeric)
bread_intake <- ifelse( bread_intake =="-3",NA, bread_intake)
bread_intake <- ifelse( bread_intake =="-1",NA, bread_intake)
bread_intake <- ifelse( bread_intake =="-10","0", bread_intake)
bread_intake = as.numeric( bread_intake)
#serving per day
bread_intake =  bread_intake/7

wholebread = ifelse( dataset$bread_type  =="3","1","0")
whitebread = ifelse( dataset$bread_type  =="1","1","0")
wholebread = as.numeric( wholebread)
whitebread = as.numeric( whitebread)

### CEREAL 
cereal_intake <-  sapply( dataset$cereal_intake ,as.numeric)
cereal_intake <- ifelse( cereal_intake =="-3",NA, cereal_intake)
cereal_intake <- ifelse( cereal_intake =="-1",NA, cereal_intake)
cereal_intake <- ifelse( cereal_intake =="-10","0", cereal_intake)
cereal_intake = as.numeric( cereal_intake)
#serving per day
cereal_intake =  cereal_intake/7

wholecereals = ifelse( dataset$cereal_type =="2"|  dataset$cereal_type =="3"|  dataset$cereal_type=="4","1","0")
whitecereals = ifelse(  dataset$cereal_type =="1"|  dataset$cereal_type =="5","1","0")
wholecereals = as.numeric( wholecereals)
whitecereals = as.numeric( whitecereals)

grainsintaketotal =rowSums(cbind(cereal_intake, bread_intake), na.rm = T )


# TOTAL SCORE 
vegetablesscore = ifelse( vegetables >=3,1,0)
fruitsscore = ifelse( fruits >=3,1,0)
fishscore = ifelse ( fish>=3,1,0)
processedmeatscore = ifelse( processedmeat<3,1,0)
redmeatscore = ifelse( redmeat<3,1,0)

wholegrains = ifelse( wholebread==1 &  grainsintaketotal >=3 |  wholecereals==1 &  grainsintaketotal>=3,1,0 )
whitegrains = ifelse( whitebread==1 &  grainsintaketotal <2 |  whitecereals==1 &  grainsintaketotal<2,1,0 )

dataset$score_diet =rowSums(cbind(vegetablesscore, fruitsscore, fishscore,processedmeatscore,redmeatscore,wholegrains,whitegrains ), na.rm = T )

levels(as.factor(dataset$score_diet))



# Define a matrix of the original input variables that contribute to the diet score
# (using the preprocessed numeric variables)
original_vars <- cbind(vegetables1, vegetables2, fruits1, fruits2, oilfish, nonoilfish, 
																							processedmeat, beef, lamp, pork, bread_intake, cereal_intake)

# For each row, check if every value is either NA or (if not NA) negative
flag_all_negative_or_NA <- apply(original_vars, 1, function(x) {
	# Remove NAs; if nothing is left then all were NA
	non_na <- x[!is.na(x)]
	if(length(non_na) == 0) {
		return(TRUE)
	} else {
		return(all(non_na < 0))
	}
})

# Assign NA to diet score for rows where all original values were NA or negative
dataset$score_diet[flag_all_negative_or_NA] <- NA
sum(is.na(dataset$score_diet))


write.csv(dataset, "bili_data_raw_ds.csv", row.names = FALSE)
