library(dplyr)
library(readxl)
library(tools)

####Load data####
data <- read_excel("WHR20_DataForFigure2.1.xls")
data<-as.data.frame(data)

####Inspection####
head(data)
str(data)
colnames(data)

####pick up & rename columns####
data <- data[,c(1:3,14:19)]
data <- rename(data, "Region"="Regional indicator",
               "happiness"="Ladder score",
               "gdp"="Explained by: Log GDP per capita",
               "social"="Explained by: Social support", 
               "life_expectancy"="Explained by: Healthy life expectancy",
               "freedom"="Explained by: Freedom to make life choices",
               "generosity"="Explained by: Generosity",
               "perceptions"="Explained by: Perceptions of corruption",
)
summary(data)

####create a new categorical variable####
Score_cat <- c()
Score_cat[data$happiness < quantile(data$happiness, 1/3)] <- "Low"
Score_cat[data$happiness >= quantile(data$happiness, 1/3) & data$happiness
          < quantile(data$happiness, 2/3)] <- "Middle"
Score_cat[data$happiness >= quantile(data$happiness, 2/3)] <- "High"

data <- cbind(data, Score_cat)
remove(Score_cat)


####Merge Quality of Life by country####
data_ql_co <- read.csv("Quality of  Life by Country.csv")
data_ql_co <- data_ql_co[,-c(1,10)]
colnames(data_ql_co) <- paste(colnames(data_ql_co), "co", sep = "_")

#check_merge result
check_merge <- left_join(data,data_ql_co, by =c("Country name"="Country_co"))
summary(check_merge)
check_merge$`Country name`[is.na(check_merge$Popularity_co)]
remove(check_merge)

#Fix names of Country_co
data_ql_co$Country_co[data_ql_co$Country_co=="Czechia"] <-"Czech Republic"
data_ql_co$Country_co[data_ql_co$Country_co=="Hong Kong"] <-"Hong Kong S.A.R. of China"
data_ql_co$Country_co[data_ql_co$Country_co=="Congo (Dem. Republic)"] <-"Congo (Brazzaville)"
data_ql_co$Country_co[data_ql_co$Country_co=="Congo"] <-"Congo (Kinshasa)" 
data_ql_co$Country_co[data_ql_co$Country_co=="Eswatini"] <-"Swaziland"
data_ql_co$Country_co[data_ql_co$Country_co=="Burma"] <-"Myanmar" 
data_ql_co$Country_co[data_ql_co$Country_co=="Central Africa"] <-"Central African Republic" 

#merge
data <- left_join(data,data_ql_co, by =c("Country name"="Country_co"))
summary(data)
remove(data_ql_co)

#delete 6 rows from data
data <- data[complete.cases(data),]
summary(data)


#### Merge Quality of Life by city ####
# open the file
data_qul_city <- read.csv("uaScoresDataFrame.csv", header=T)
newcountry <- read.csv("newcountryname.csv", header=T)
colnames(data_qul_city)
colnames(newcountry)
summary(data_qul_city)
head(data_qul_city)

# change the column name
names(newcountry)[names(newcountry) == "?..UA_newcountry"] <- "UA_newcountry"
colnames(newcountry)


# Combine new country name with the Quality of life by city report ####
# because the country's name in the data_qul_city is not match with other reports 
# as they contain space, state name(instead of country)

data_qul_city2 <- data.frame(data_qul_city,newcountry)
colnames(data_qul_city2)
remove(newcountry)

# remove column 1,3 (X rage and UA_Country)
data_qul_city2 <- data_qul_city2[,-c(1,3)] 
colnames(data_qul_city2)

# change the column name
data_qul_city2 <- rename(data_qul_city2, "City"="UA_Name",
                         "Cost"="Cost.of.Living",
                         "Travel"="Travel.Connectivity",
                         "Leisure.Culture"="Leisure...Culture", 
                         "Internet"="Internet.Access",
                         "Environment"="Environmental.Quality")

# add "city" at the end of the column in the Quality of life by city
colnames(data_qul_city2) <- paste(colnames(data_qul_city2), "city", sep = "_")

# reorder columns
data_qul_city2 <- data_qul_city2[, c(1, 2, 20, 3, 4,
                                     5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
colnames(data_qul_city2)

# average the amount in the quality of life by city
data_qul_city3 <- data_qul_city2
data_qul_city3 <- aggregate(cbind(Housing_city,Cost_city,Startups_city,
                                  Venture.Capital_city,Travel_city,Commute_city,
                                  Business.Freedom_city,Safety_city,
                                  Healthcare_city,Education_city,Environment_city,
                                  Economy_city,Taxation_city,Internet_city,
                                  Leisure.Culture_city,Tolerance_city,
                                  Outdoors_city)~ UA_newcountry_city, 
                            data=data_qul_city2,FUN = mean)

colnames(data_qul_city3)
summary(data_qul_city3)
remove(data_qul_city)
remove(data_qul_city2)

# check merge result
testdata <- data
colnames(testdata)

testdata <- left_join(testdata,data_qul_city3, 
                      by=c("Country name"="UA_newcountry_city"))
colnames(testdata)
summary(testdata)
remove(testdata)

# merge 'Quality by city' to 'Data'
data <- left_join(data,data_qul_city3, 
                  by=c("Country name"="UA_newcountry_city"))
summary(data)
sort(data$`Country name`[is.na(data$Outdoors_city)])  #there are 58 NA observations
remove(data_qul_city3)

#delete NA observations (147-58 = 89 observations remain)
data <- data[complete.cases(data),]
summary(data)

# check the output excel
write.csv(data,file="testdata1.csv", row.names = F)


#### Merge Suicide ####
# open the file & change column name
data_sui <-  read.csv("Suicide rates.csv", header=T)
head(data_sui)
data_sui <- data_sui[,c(1,3)]
names(data_sui) =c("country", "suicide")

#Convert country To Title Case
data_sui$country <- toTitleCase(data_sui$country)

#check_merge result
check_merge <- left_join(data,data_sui, by =c("Country name"="country"))
summary(check_merge)
check_merge$`Country name`[is.na(check_merge$Popularity_co)]
remove(check_merge)

#merge 'Suicide' to 'Data'
data <- left_join(data,data_sui, by =c("Country name"="country"))
summary(data)
remove(data_sui)

####changing data types####
summary(data)
data$Region <- factor(data$Region)
data$Score_cat <- factor(data$Score_cat, levels= c("Low", "Middle", "High")) 

#### check correlation####
cor(data[,c(3:9,11:35)])