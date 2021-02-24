#Happiness-Quality of life Group Project (Winter 2021)

# Subject:
# 1.Cleaning data and Merge 4 files
# 2.Summary data
# 3.Correlation Matrix
# 4.Multivariate Regression

library(dplyr)
library(readxl)
library(tools)
library(corrplot)

##### 1) Cleaning data #####
# There are 4 files merging; 1.Happiness score          2.Quality of life(by country), 
#                            3.Quality of life(by city) 4.Suicide rate

### 1.1 Clean happiness score data ###
data <- read_excel("WHR20_DataForFigure2.1.xls")      # import happiness score data
data<-as.data.frame(data)

# inspection
head(data)
str(data)
colnames(data)

# pick up the important column & rename columns #
data <- data[,c(1:3,14:19)]
data <- rename(data, "Region"="Regional indicator",
               "happiness"="Ladder score",
               "gdp"="Explained by: Log GDP per capita",
               "social"="Explained by: Social support", 
               "life_expectancy"="Explained by: Healthy life expectancy",
               "freedom"="Explained by: Freedom to make life choices",
               "generosity"="Explained by: Generosity",
               "perceptions"="Explained by: Perceptions of corruption")
summary(data)



### 1.2 create a new categorical variable ###
Score_cat <- c()
Score_cat[data$happiness < quantile(data$happiness, 1/3)] <- "Low"
Score_cat[data$happiness >= quantile(data$happiness, 1/3) & data$happiness
          < quantile(data$happiness, 2/3)] <- "Middle"
Score_cat[data$happiness >= quantile(data$happiness, 2/3)] <- "High"

data <- cbind(data, Score_cat)
remove(Score_cat)



### 1.3 Clean the Quality of life by country ###
data_ql_co <- read.csv("Quality of  Life by Country.csv")    # import the Quality of life by country
data_ql_co <- data_ql_co[,-c(1,10)]                         # delete insignificant variables

# add  "_co" to identify columns after merging Quality of life by country to Happiness data
colnames(data_ql_co) <- paste(colnames(data_ql_co), "co", sep = "_")



### 1.4 Merge Quality of Life by country with happiness together ###
# check_merge result
check_merge <- left_join(data,data_ql_co, by =c("Country name"="Country_co"))
summary(check_merge)

# check if any NA (means not matching with Happiness)
check_merge$`Country name`[is.na(check_merge$Popularity_co)]
remove(check_merge)

# change names of Quality of life by country (to match with Happiness data)
data_ql_co$Country_co[data_ql_co$Country_co=="Czechia"] <-"Czech Republic"
data_ql_co$Country_co[data_ql_co$Country_co=="Hong Kong"] <-"Hong Kong S.A.R. of China"
data_ql_co$Country_co[data_ql_co$Country_co=="Congo (Dem. Republic)"] <-"Congo (Brazzaville)"
data_ql_co$Country_co[data_ql_co$Country_co=="Congo"] <-"Congo (Kinshasa)" 
data_ql_co$Country_co[data_ql_co$Country_co=="Eswatini"] <-"Swaziland"
data_ql_co$Country_co[data_ql_co$Country_co=="Burma"] <-"Myanmar" 
data_ql_co$Country_co[data_ql_co$Country_co=="Central Africa"] <-"Central African Republic" 

# merge Quality of life by country to Happiness score data
data <- left_join(data,data_ql_co, by =c("Country name"="Country_co"))
summary(data)
remove(data_ql_co)

# delete 6 rows from data:
# countries in data that we deleted are 1.Taiwan Province of China/ 2.Kosovo/ 3.North Cyprus/
#                                       4.Liberia/ 5.Palestinian Territories/ 6.Swaziland
data <- data[complete.cases(data),]
summary(data)

# the data from Quality of life by country seem to have the different minimum
# and maximum rate at 0-100 but Happiness score has the minimum and maximum rate at 0-10
data$Stability_co <- (data$Stability_co/10)
data$Rights_co    <- (data$Rights_co/10)
data$Health_co <- (data$Health_co/10)
data$Safety_co <- (data$Safety_co/10)
data$Climate_co <- (data$Climate_co/10)
data$Costs_co <- (data$Costs_co/10)
data$Popularity_co <- (data$Popularity_co/10)
summary(data)



### 1.5 Clean the QUality of life by city ####

data_qul_city <- read.csv("uaScoresDataFrame.csv", header=T) # import the QUality of life by city
newcountry <- read.csv("newcountryname.csv", header=T)
colnames(data_qul_city)
colnames(newcountry)
summary(data_qul_city)
head(data_qul_city)

# change the column name
names(newcountry)[names(newcountry) == "ï..UA_newcountry"] <- "UA_newcountry"
colnames(newcountry)


# combine New country name with the Quality of life by city report ####
# because the country's name in the data_qul_city is not match with other reports 
# as they contain; space, state names(instead of country)
data_qul_city2 <- data.frame(data_qul_city,newcountry)
colnames(data_qul_city2)
remove(newcountry)

# remove column 1,3 (X range and UA_Country)
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

# reorder columns (move country to left column)
data_qul_city2 <- data_qul_city2[, c(1, 2, 20, 3, 4,
                                     5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
colnames(data_qul_city2)

# sum up quality of city and convert to be country by using average amount
# for example: if CA housing=15 and NY housing=5 average therefore, US housing=10  
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



### 1.6 Merge Quality of Life by city ###
# check merge result
testdata <- data    #clone the data to testdata
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

# check if any NA
sort(data$`Country name`[is.na(data$Outdoors_city)])  #there are 58 NA observations
remove(data_qul_city3)

#delete NA observations (147-58 = 89 observations remain)
data <- data[complete.cases(data),]
summary(data)

# check the output excel
write.csv(data,file="testdata1.csv", row.names = F)



### 1.7 Clean Suicide rate data ###
data_sui <-  read.csv("Suicide rates.csv", header=T)   # import the suicide rate data
head(data_sui)
data_sui <- data_sui[,c(1,3)]                          # select the important columns
names(data_sui) =c("country", "suicide")               # change column names

# convert country To Title Case
data_sui$country <- toTitleCase(data_sui$country)



### 1.8 Merge Suicide rate data ###
# check_merge result
check_merge <- left_join(data,data_sui, by =c("Country name"="country"))
summary(check_merge)
check_merge$`Country name`[is.na(check_merge$Popularity_co)]
remove(check_merge)

# merge Suicide rate data to 'Data'
data <- left_join(data,data_sui, by =c("Country name"="country"))
summary(data)
remove(data_sui)



### 1.9 Clean the region name ### 
# create new column for new region
data$newregion <- data$Region
data$newregion <- as.character(data$newregion)

# group and change the region name (make it shorter)
data$newregion[data$newregion=="Western Europe"] <-"Europe"
data$newregion[data$newregion=="Central and Eastern Europe"] <-"Europe"
data$newregion[data$newregion=="Latin America and Caribbean"] <-"Caribbean"
data$newregion[data$newregion=="North America and ANZ"] <- "North America"
data$newregion[data$`Country name`=="Australia"] <- "Oceania"
data$newregion[data$`Country name`=="New Zealand"] <- "Oceania"
data$newregion[data$newregion=="Sub-Saharan Africa"] <- "Africa"
data$newregion[data$newregion=="Middle East and North Africa"] <- "Africa"
data$newregion[data$newregion=="Southeast Asia"] <- "Asia"
data$newregion[data$newregion=="East Asia"] <- "Asia"
data$newregion[data$newregion=="South Asia"] <- "Asia"
data$newregion[data$newregion=="Commonwealth of Independent States"] <- "CIS"
data$newregion <- as.factor(data$newregion)
data$Region <- data$newregion
summary(data)
data <- data[,-c(36)]                 # remove newregion variable



#### 1.10 Change data types ###
summary(data)
data$Region <- factor(data$Region)
data$Score_cat <- factor(data$Score_cat, levels= c("Low", "Middle", "High")) 





##### 2) summary data #####

str(data)
summary(data)
install.packages("RColorBrewer")
library(RColorBrewer)

### 2.1 Box plot ###

# box plot: Overall (all variables)

# box plot: Happiness by region
par(mar=c(5, 4, 2, 2))
boxplot(happiness~Region, data=data, ylim=c(0,10), main="Happiness score by region", 
        xlab="", ylab="Happiness score",las=2, col = brewer.pal(n = 7, name = "RdBu"), 
        cex.axis=0.6)


### 2.2 Bar plot ###

# bar plot: Level of happiness score
score <- table(data$Score_cat)
xx <- barplot(score, ylab = "Count", col = brewer.pal(n = 7, name = "Dark2"), 
              main = "Level of happiness",xlab="", space=1, ylim=c(0,50)) 
text(x = xx, y = score, label = score, pos = 3, 
     cex = 0.8, col = "#293352")

# bar plot: count region
Region <- table(data$Region)
xy <- barplot(Region, ylab = "Count", col = brewer.pal(n = 7, name = "RdBu"), 
              main = "Region",xlab="", space=1, ylim=c(0,40)) 
text(x = xy, y = Region, label = Region, pos = 3, 
     cex = 0.8, col = "#293352")



#### check correlation####
cor(data[,c(3:9,11:35)])