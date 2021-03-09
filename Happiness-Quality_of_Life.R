#Happiness-Quality of life Group Project (Winter 2021)

# Subject:
# 1.Cleaning data and Merging 4 files
# 2.Summary data
# 3.Multivariate Regression
# 4.Correlation Matrix

library(dplyr)
library(readxl)
library(tools)
library(corrplot)
library(RColorBrewer)
library(rcompanion)
library(reshape2)
library(tmap)
library(spData)

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



### 1.5 Clean the Quality of life by city ####

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
data_qul_city2 <- data_qul_city2[, c(1, 2, 20, 3:19)]
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
str(data)

colnames(data_qul_city3)
summary(data_qul_city3)
remove(data_qul_city)
remove(data_qul_city2)
remove(newcountry)



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

### 2.1 Box plot ###

# box plot: Overall (all variables)
data_box_plot <- data                        # clone data to data_box_plot
data_box_plot <- data_box_plot[,-c(1,2)]     # delete column Country name, Region to keep only numeric variables.
head(data_box_plot)

data_melt <- melt(data_box_plot)             # melt data to illustrate overall data
head(data_melt)
remove(data_box_plot)

par(mar=c(7, 4, 2, 2))
boxplot(value~variable, data=data_melt, ylim=c(0,32), xlab="",ylab="score",las=2,
        main="All variables in Happiness-Quality of life", 
        col = brewer.pal(n = 32, name = "Set3"), 
        cex.axis=0.7)

# add vertical line to separate report
abline(v=1.6, col="chocolate4", lty=2)
abline(v=7.4, col="chocolate4", lty=2)
abline(v=14.5, col="chocolate4", lty=2)
abline(v=31.5, col="chocolate4", lty=2)


# box plot: Level of happiness
boxplot(happiness~Score_cat, data=data, ylim=c(0,10), main="Happiness score by region", 
        xlab="", ylab="Level", col=c(rgb(0,0,1,0.15),rgb(0,1,0,0.15),rgb(1,0,0,0.15)), 
        cex.axis=1)

# box plot: Happiness by region
par(mar=c(7, 4, 2, 2))
boxplot(happiness~Region, data=data, ylim=c(0,10), main="Happiness score", 
        xlab="", ylab="Happiness score",las=2, col = brewer.pal(n = 7, name = "RdBu"), 
        cex.axis=1)

# box plot: Suicide rate by region
boxplot(suicide~Region, data=data, ylim=c(0,32), main="Suicide by region", 
        xlab="", ylab="per 100k", col = brewer.pal(n = 7, name = "RdBu"), 
        cex.axis=1, las=2)

remove(data_melt)

### 2.2 Bar plot ###

# bar plot: count region (separated by level)
region_Hap <- table(data$Score_cat, data$Region)
par(mar=c(7, 4, 2, 2))
xy = barplot(region_Hap, ylab = "Count", main = "Count by region and level of happiness", xlab="",
             col = c("mediumpurple1","darkseagreen2","pink"), ylim=c(0,60), beside=T, las=2)
legend("topright", c("High 44","Middle 32","Low 13"), 
       text.col=c("pink","darkseagreen2","mediumpurple1"))
text(x = xy, y = region_Hap, label = region_Hap, pos = 3, 
     cex = 0.8, col = "#293352")

remove(xy)
remove(region_Hap)

### 2.3 Density plot ###
den1 <- density(data$happiness[data$Score_cat == "High"])
den2 <- density(data$happiness[data$Score_cat == "Middle"])
den3 <- density(data$happiness[data$Score_cat == "Low"])

hist(data$happiness[data$Score_cat == "High"],
     main = "Level of Happiness scores",
     xlim=c(0,10), breaks = seq(0,10,1),col=rgb(1,0,0,0.15),
     cex.axis = 0.8, freq = F, xlab = "Happiness", 
     ylim=c(0,1.2))
hist(data$happiness[data$Score_cat == "Middle"],
     xlim=c(0,10), breaks = seq(0,10,1),col=rgb(0,1,0,0.15),
     cex.axis = 0.8, freq = F, add=T)
hist(data$happiness[data$Score_cat == "Low"],
     xlim=c(0,10), breaks = seq(0,10,1),col=rgb(0,0,1,0.15),
     cex.axis = 0.8, freq = F, add=T)

legend("topright", c("High", "Middle", "Low"), 
       col=c(rgb(1,0,0,0.15),rgb(0,1,0,0.15),rgb(0,0,1,0.15)), lwd=10)

lines(den1, lwd = 2, col = "lightpink4", lty = 1)
lines(den2, lwd = 2, col = "darkseagreen4", lty = 1)
lines(den3, lwd = 2, col = "blue", lty = 1)

remove(den1)
remove(den2)
remove(den3)

### 2.4 Maps ###
#Check merging data
world1 <- left_join(data, world, by = c("Country name" = "name_long"))
world1$`Country name`[is.na(world1$continent)]
# [1] "Malta"       "Singapore"   "South Korea" "Russia"   

data_tempo <- data
sort(world$name_long)
data_tempo$`Country name`[data_tempo$`Country name`=="South Korea"] <- "Republic of Korea"
data_tempo$`Country name`[data_tempo$`Country name`=="Russia"] <- "Russian Federation"

#Marge dataset for map
world1 <- left_join(world,data_tempo, by = c("name_long" = "Country name"))
world1$name_long[!is.na(world1$happiness)]
#remove Malta & Singapore due to not existing
tm_shape(world1) + tm_fill("happiness") + 
        tm_layout(main.title="                                     Happiness score map") 


### 2.5 Scatter plots ###
#Single regression model with the highest correlation variable (social) 
m_single <- lm(happiness ~ social, data=data)
summary(m_single)
# RSE 0.6064 Multiple R-squared:  0.5943,	Adjusted R-squared:  0.5896 
plot(m_single)

#scatter plot
plot(data$social[data$Region=="Africa"], 
     data$happines[data$Region=="Africa"], ylim=c(3,8),xlim=c(0.6,1.6), pch=16,
     xlab="Social (point)",ylab="Happiness Score (point)",main="Happiness vs Social by Region",col="red")

points(data$social[data$Region=="Asia"],
       data$happines[data$Region=="Asia"],col="orange",pch=16)
points(data$social[data$Region=="Caribbean"],
       data$happines[data$Region=="Caribbean"],col="darksalmon",pch=16)
points(data$social[data$Region=="CIS"], 
       data$happines[data$Region=="CIS"],col="gainsboro",pch=16)
points(data$social[data$Region=="Europe"], 
       data$happines[data$Region=="Europe"],col="skyblue",pch=16)
points(data$social[data$Region=="North America"], 
       data$happines[data$Region=="North America"],col="dodgerblue",pch=16)
points(data$social[data$Region=="Oceania"], 
       data$happines[data$Region=="Oceania"],col="dodgerblue4",pch=16)

legend("topleft", c("Africa","Asia","Caribbean","CIS","Europe North America","Oceania"), 
       col = c("red", "orange","darksalmon","gainsboro","skyblue","dodgerblue", "dodgerblue4"),lwd=10)
abline(m_single, lwd = 2, lty = 2)


### 2.6 correlation Matrix ###
str(data)
correlation_matrix <- cor(data[,c(3:9,11:35)])
write.csv(correlation_matrix,file="Matrix1.csv") # to check the value over 0.70

##### 3) Multivariate Regression #####
### 3.1 We first manually test the models using different features ###
names(data)

# With all the variables without Score_cat
model1<-lm(happiness ~ gdp + social + life_expectancy + freedom + generosity + 
                   perceptions + Stability_co + Rights_co + Health_co + 
                   Safety_co + Climate_co + Costs_co + Popularity_co + Housing_city +
                   Cost_city + Startups_city + Venture.Capital_city + Travel_city + 
                   Commute_city + Business.Freedom_city + Safety_city + Healthcare_city + 
                   Education_city + Environment_city + Economy_city + Taxation_city +
                   Internet_city + Leisure.Culture_city + Tolerance_city + Outdoors_city + suicide,
           data = data)
summary(model1)
# RSE 0.3953 Multiple R-squared:  0.887,	Adjusted R-squared:  0.8256 


# with variables gdp,life_expectancy, commute_city, safety_city, environment_city, Taxation,
#  tolerance, suicide 
model2<-lm(happiness ~ gdp + life_expectancy + Commute_city + Safety_city +  Environment_city + 
                   Taxation_city + Tolerance_city + suicide, data = data)
summary(model2)
# RSE 0.4926 Multiple R-squared:  0.7538,	Adjusted R-squared:  0.7292 


# without Rights_co, Health_city, Score_cat due to the correlation between Healthcare_city
# and Heath_co & life_expectancy over 0.9. Also Rights_co has many correlations with other
# features 
model3<-lm(happiness ~ gdp + social + life_expectancy + freedom + generosity + 
                   perceptions + Stability_co + Health_co + 
                   Safety_co + Climate_co + Costs_co + Popularity_co + Housing_city +
                   Cost_city + Startups_city + Venture.Capital_city + Travel_city + 
                   Commute_city + Business.Freedom_city + Safety_city + 
                   Education_city + Environment_city + Economy_city + Taxation_city +
                   Internet_city + Leisure.Culture_city + Tolerance_city + Outdoors_city + suicide,
           data = data)
summary(model3)
# RSE 0.3942 Multiple R-squared:  0.8837,	Adjusted R-squared:  0.8266 


# Without Rights_co, Healthcare_city, Score_cat
# Without Health_co, Startups_city, Venture.Capital_city since their p > 0.85
model4<-lm(happiness ~ gdp + social + life_expectancy + freedom + generosity + 
                   perceptions + Stability_co + 
                   Safety_co + Climate_co + Costs_co + Popularity_co + Housing_city +
                   Cost_city + Travel_city + 
                   Commute_city + Business.Freedom_city + Safety_city + 
                   Education_city + Environment_city + Economy_city + Taxation_city +
                   Internet_city + Leisure.Culture_city + Tolerance_city + Outdoors_city + suicide,
           data = data)
summary(model4)
# RSE 0.3846 Multiple R-squared:  0.8837,	Adjusted R-squared:  0.8349 


# Without Rights_co, Healthcare_city, Score_cat
# Without Health_co, Startups_city, Venture.Capital_city since their p > 0.85
# Without Economy_city since p > 0.85
model5<-lm(happiness ~ gdp + social + life_expectancy + freedom + generosity + 
                   perceptions + Stability_co + 
                   Safety_co + Climate_co + Costs_co + Popularity_co + Housing_city +
                   Cost_city + Travel_city + 
                   Commute_city + Business.Freedom_city + Safety_city + 
                   Education_city + Environment_city + Taxation_city +
                   Internet_city + Leisure.Culture_city + Tolerance_city + Outdoors_city + suicide,
           data = data)
summary(model5)
# RSE 0.3817 Multiple R-squared:  0.8836,	Adjusted R-squared:  0.8374 


# Without Rights_co, Healthcare_city, Score_cat
# Without Health_co, Startups_city, Venture.Capital_city since their p > 0.85
# Without Economy_city since p > 0.85
# Without Taxation since p >0.75
model6<-lm(happiness ~ gdp + social + life_expectancy + freedom + generosity + 
                   perceptions + Stability_co + Safety_co + Climate_co + Costs_co + 
                   Popularity_co + Housing_city + Cost_city + Travel_city + Commute_city + 
                   Business.Freedom_city + Safety_city + Education_city + Environment_city +
                   Internet_city + Leisure.Culture_city + Tolerance_city + Outdoors_city + suicide,
           data = data)
summary(model6)
# RSE 0.3789 Multiple R-squared:  0.8835,	Adjusted R-squared:  0.8398 


# Without Rights_co, Healthcare_city, Score_cat
# Without Health_co, Startups_city, Venture.Capital_city since their p > 0.85
# Without Economy_city since p > 0.85
# Without Taxation since p > 0.75
# Without Commute_city since p > 0.75
model7<-lm(happiness ~ gdp + social + life_expectancy + freedom + generosity + 
                   perceptions + Stability_co + Safety_co + Climate_co + Costs_co + 
                   Popularity_co + Housing_city + Cost_city + Travel_city + 
                   Business.Freedom_city + Safety_city + Education_city + Environment_city +
                   Internet_city + Leisure.Culture_city + Tolerance_city + Outdoors_city + suicide,
           data = data)
summary(model7)
# RSE 0.3762 Multiple R-squared:  0.8833,	Adjusted R-squared:  0.842 


# Without Rights_co, Healthcare_city, Score_cat
# Without Health_co, Startups_city, Venture.Capital_city since their p > 0.85
# Without Economy_city since p > 0.85
# Without Taxation since p > 0.75
# Without Commute_city since p > 0.75
# Without Popularity_co since p > 0.68
model8<-lm(happiness ~ gdp + social + life_expectancy + freedom + generosity + 
                   perceptions + Stability_co + Safety_co + Climate_co + Costs_co + 
                   Housing_city + Cost_city + Travel_city + Business.Freedom_city + 
                   Safety_city + Education_city + Environment_city + Internet_city + 
                   Leisure.Culture_city + Tolerance_city + Outdoors_city + suicide,
           data = data)
summary(model8)
# RSE 0.3739 Multiple R-squared:  0.883,	Adjusted R-squared:  0.844 


# Without Rights_co, Healthcare_city, Score_cat
# Without Health_co, Startups_city, Venture.Capital_city since their p > 0.85
# Without Economy_city since p > 0.85
# Without Taxation since p > 0.75
# Without Commute_city since p > 0.75
# Without Popularity_co since p > 0.68
# Without Environment_city since p > 0.65
model9<-lm(happiness ~ gdp + social + life_expectancy + freedom + generosity + 
                   perceptions + Stability_co + Safety_co + Climate_co + Costs_co + 
                   Housing_city + Cost_city + Travel_city + Business.Freedom_city + 
                   Safety_city + Education_city + Internet_city + 
                   Leisure.Culture_city + Tolerance_city + Outdoors_city + suicide,
           data = data)
summary(model9)
# RSE 0.3716 Multiple R-squared:  0.8827,	Adjusted R-squared:  0.8459 


# Without Rights_co, Healthcare_city, Score_cat
# Without Health_co, Startups_city, Venture.Capital_city since their p > 0.85
# Without Economy_city since p > 0.85
# Without Taxation since p > 0.75
# Without Commute_city since p > 0.75
# Without Popularity_co since p > 0.68
# Without Environment_city since p > 0.65
# Without Costs_co since p > 0.65
model10<-lm(happiness ~ gdp + social + life_expectancy + freedom + generosity + 
                    perceptions + Stability_co + Safety_co + Climate_co + Costs_co + 
                    Housing_city + Travel_city + Business.Freedom_city + Safety_city + 
                    Education_city + Internet_city + Leisure.Culture_city + Tolerance_city +
                    Outdoors_city + suicide, data = data)
summary(model10)
# RSE 0.3765 Multiple R-squared:  0.8778,	Adjusted R-squared:  0.8418 


### 3.2 Finding the best model using step function ###
model.null = lm(happiness ~ 1, data=data)
step(model.null, scope = list(upper=model1), direction="both", data=data) 
#fit the model 
model.step<-lm(happiness ~ social + freedom + life_expectancy + Safety_co + 
                       generosity + gdp + Tolerance_city + Safety_city + Cost_city + 
                       Climate_co + Business.Freedom_city, data = data)
summary(model.step)
# RSE 0.3639 Multiple R-squared:  0.8707,	Adjusted R-squared:  0.8522 


### 3.3 Investigate model and features ###

#adding Region to model.step improve the RSE and Ajusted R^2 by 1%, but most region 
# other than Asia have p-value >0.05, not significant. drop region. 
model.stepx1<-lm(happiness ~ social + freedom + life_expectancy + Safety_co + 
                         generosity + gdp + Tolerance_city + Safety_city + Cost_city + 
                         Climate_co + Business.Freedom_city + Region, data = data)
summary(model.stepx1)
# RSE 0.3552 Multiple R-squared:  0.8864,	Adjusted R-squared:  0.8592 

#adding suicide improve the RSE and Ajusted R^2 by a little 
model.stepx2<-lm(happiness ~ social + freedom + life_expectancy + Safety_co + 
                         generosity + gdp + Tolerance_city + Safety_city + Cost_city + 
                         Climate_co + Business.Freedom_city + suicide, data = data)
summary(model.stepx2)
# RSE 0.363 Multiple R-squared:  0.873,	Adjusted R-squared:  0.8529 

#but adding Region and suicide isn't better than adding only region
model.stepx3<-lm(happiness ~ social + freedom + life_expectancy + Safety_co + 
                         generosity + gdp + Tolerance_city + Safety_city + Cost_city + 
                         Climate_co + Business.Freedom_city +Region + suicide, data = data)
summary(model.stepx3)
# RSE 0.3576 Multiple R-squared:  0.8865,	Adjusted R-squared:  0.8573 

#subtracting Business.Freedom_city from model.stepx1 improve the RSE and Ajusted R^2
#this is our final model 
model.stepx4<-lm(happiness ~ social + freedom + life_expectancy + Safety_co + 
                         generosity + gdp + Tolerance_city + Safety_city + Cost_city + 
                         Climate_co, data = data)
summary(model.stepx4)
# RSE 0.3545 Multiple R-squared:  0.8853,	Adjusted R-squared:  0.8598


### 3.4 Comparing model performance ###
compareLM(model1, model2, model3, model4, model5, model6,
          model7, model8, model9, model10, model.step, 
          model.stepx1, model.stepx2, model.stepx3, model.stepx4)
model.final<-model.stepx4
remove(model1, model2, model3, model4, model5, model6,
       model7, model8, model9, model10, model.step, model.stepx, model.stepx1,
       model.stepx2, model.stepx3, model.stepx4, model.null)

#dropping either Safety_co or Safety_city will increase RSE, we will combine both 
# variables later in future step 
model_sa_co<-lm(happiness ~ social + freedom + life_expectancy + Safety_co + 
                        generosity + gdp + Tolerance_city + Cost_city + 
                        Climate_co + Region, data = data)
summary(model_sa_co)
# RSE 0.3595 Multiple R-squared:  0.8804,	Adjusted R-squared:  0.8558 (better)

model_sa_ci<-lm(happiness ~ social + freedom + life_expectancy + 
                        generosity + gdp + Tolerance_city + Safety_city + Cost_city + 
                        Climate_co + Region, data = data)
summary(model_sa_ci)
# RSE 0.3597 Multiple R-squared:  0.8802,	Adjusted R-squared:  0.8556

#switching to cost_co from cost_city increases RSE 
model_less<-lm(happiness ~ social + freedom + life_expectancy + Safety_co + 
                       generosity + gdp + Tolerance_city + Costs_co + Climate_co + 
                       Region, data = data)
summary(model_less)
# RSE 0.3574 Multiple R-squared:  0.8817,	Adjusted R-squared:  0.8574

remove(model_sa_co, model_sa_ci)




##### 4) Correlation Matrix #####

### 4.1 check final model
# happiness ~ social + freedom + life_expectancy + Safety_co + generosity 
# + gdp + Tolerance_city + Safety_city + Cost_city + Climate_co + Region

names(data)
cor(data[,c(3,5,7,6,14,8,4,33,25,19,15)])
#                  happiness     social     freedom life_expectancy  Safety_co  generosity
# happiness        1.0000000  0.7709149  0.51577690      0.71976790  0.3955936  0.27568968
# social           0.7709149  1.0000000  0.30397570      0.58236183  0.2526772  0.13545723
# freedom          0.5157769  0.3039757  1.00000000      0.17884169  0.2368033  0.40558318
# life_expectancy  0.7197679  0.5823618  0.17884169      1.00000000  0.5984291  0.05184827
# Safety_co        0.3955936  0.2526772  0.23680334      0.59842915  1.0000000  0.29371040
# generosity       0.2756897  0.1354572  0.40558318      0.05184827  0.2937104  1.00000000
# gdp              0.7255450  0.6265144  0.18964420      0.75984496  0.5862569  0.09068623
# Tolerance_city   0.5825596  0.5186065  0.26323251      0.56427186  0.3467037  0.00100773
# Safety_city      0.2370228  0.1893991  0.08922301      0.40979211  0.6019437  0.05609378
# Cost_city       -0.6205714 -0.4746822 -0.43458475     -0.51178899 -0.3557078 -0.40345245
# Climate_co      -0.4766846 -0.4799675 -0.25178090     -0.31480341 -0.3856727 -0.16951964
#                         gdp Tolerance_city Safety_city  Cost_city Climate_co
# happiness        0.72554502     0.58255958  0.23702276 -0.6205714 -0.4766846
# social           0.62651440     0.51860650  0.18939907 -0.4746822 -0.4799675
# freedom          0.18964420     0.26323251  0.08922301 -0.4345848 -0.2517809
# life_expectancy  0.75984496     0.56427186  0.40979211 -0.5117890 -0.3148034
# Safety_co        0.58625691     0.34670373  0.60194370 -0.3557078 -0.3856727
# generosity       0.09068623     0.00100773  0.05609378 -0.4034524 -0.1695196
# gdp              1.00000000     0.34272947  0.42016598 -0.4427836 -0.4485482
# Tolerance_city   0.34272947     1.00000000  0.40443172 -0.3096572 -0.3235666
# Safety_city      0.42016598     0.40443172  1.00000000 -0.2859966 -0.4350400
# Cost_city       -0.44278363    -0.30965725 -0.28599657  1.0000000  0.2302806
# Climate_co      -0.44854825    -0.32356660 -0.43504003  0.2302806  1.0000000

# more than 0.60 
# social vs gdp (0.63) / life_expectancy vs gdp (0.76) / Safety_co vs Safety_city (0.60)


### 4.2 check less variables model
# happiness ~ social + freedom + life_expectancy + Safety_co + 
# generosity + gdp + Tolerance_city + Costs_co + Climate_co + Region

cor(data[,c(3,5,7,6,14,8,4,33,16,15)])
#                  happiness     social    freedom life_expectancy  Safety_co
# happiness        1.0000000  0.7709149  0.5157769      0.71976790  0.3955936
# social           0.7709149  1.0000000  0.3039757      0.58236183  0.2526772
# freedom          0.5157769  0.3039757  1.0000000      0.17884169  0.2368033
# life_expectancy  0.7197679  0.5823618  0.1788417      1.00000000  0.5984291
# Safety_co        0.3955936  0.2526772  0.2368033      0.59842915  1.0000000
# generosity       0.2756897  0.1354572  0.4055832      0.05184827  0.2937104
# gdp              0.7255450  0.6265144  0.1896442      0.75984496  0.5862569
# Tolerance_city   0.5825596  0.5186065  0.2632325      0.56427186  0.3467037
# Costs_co        -0.5252890 -0.3070025 -0.2771449     -0.55510548 -0.2536313
# Climate_co      -0.4766846 -0.4799675 -0.2517809     -0.31480341 -0.3856727
#                  generosity         gdp Tolerance_city   Costs_co Climate_co
# happiness        0.27568968  0.72554502     0.58255958 -0.5252890 -0.4766846
# social           0.13545723  0.62651440     0.51860650 -0.3070025 -0.4799675
# freedom          0.40558318  0.18964420     0.26323251 -0.2771449 -0.2517809
# life_expectancy  0.05184827  0.75984496     0.56427186 -0.5551055 -0.3148034
# Safety_co        0.29371040  0.58625691     0.34670373 -0.2536313 -0.3856727
# generosity       1.00000000  0.09068623     0.00100773 -0.2209632 -0.1695196
# gdp              0.09068623  1.00000000     0.34272947 -0.4109084 -0.4485482
# Tolerance_city   0.00100773  0.34272947     1.00000000 -0.2237474 -0.3235666
# Costs_co        -0.22096319 -0.41090839    -0.22374742  1.0000000  0.1393038
# Climate_co      -0.16951964 -0.44854825    -0.32356660  0.1393038  1.0000000

# more than 0.60 
# social vs gdp (0.63) / life_expectancy vs gdp (0.76)

#########           Multivariate regression        ###################
#Compare models 

# Step function generates our base model 
model.step<-lm(happiness ~ social + freedom + life_expectancy + Safety_co + 
                 generosity + gdp + Tolerance_city + Safety_city + Cost_city + 
                 Climate_co + Business.Freedom_city, data = data)
summary(model.step)

# Dropping business freedom 
model.dropb<-lm(happiness ~ social + freedom + life_expectancy + Safety_co + 
                   generosity + gdp + Tolerance_city + Safety_city + Cost_city + 
                   Climate_co, data = data)
summary(model.dropb)
par(mfrow = c(1,1))
plot(model.dropb)

# Adding both Region and suicide rate 
model.addrr<-lm(happiness ~ social + freedom + life_expectancy + Safety_co + 
                   generosity + gdp + Tolerance_city + Safety_city + Cost_city + 
                   Climate_co + Region + suicide, data = data)
summary(model.addrr)

# Compare models 
compareLM(model.step, model.dropb, model.addrr)

### Check the distribution of suicide rate 
################ Scatterplot for suicide vs happiness score by region  ###############################

# fit the regression line with x as the suicide and y as the happiness score  
regline <- lm(happiness~suicide, data=data)
# output the coefficients of the regression equation
regline$coefficients

# use the plot for category: continent = Africa as the base of the scatterplot 
plot(data$suicide[data$Region == "Africa"], data$happiness[data$Region == "Africa"],
     main = "Suicide Rate vs. Happiness Score by Country", 
     xlab = "Suicide Rate per 100k ppl", ylab = "Happiness Score", pch = 15, col = "firebrick2",
     xlim = c(0,34), ylim = c(0,10))

# add the scatter points by category: Region, differentiated by color and type 
points(data$suicide[data$Region == "Asia"], data$happiness[data$Region == "Asia"],
       pch = 16, col = "yellow")
points(data$suicide[data$Region == "Europe"], data$happiness[data$Region == "Europe"],pch = 17, col = "yellowgreen")
points(data$suicide[data$Region == "North America"], data$happiness[data$Region == "North America"],pch = 18, col = "purple")
points(data$suicide[data$Region == "Oceania"], data$happiness[data$Region == "Oceania"],	pch = 7, col = "black")
points(data$suicide[data$Region == "CIS"], data$happiness[data$Region == "CIS"],pch = 19, col = "deepskyblue1")
points(data$suicide[data$Region == "Caribbean"], data$happiness[data$Region == "Caribbean"],pch = 11, col = "tan4")

# add legend to the plot, adjust the size by cex =, used color map to pick the colors 
#  that could help differentiate the data 
legend("topright", c("Africa", "Asia", "Europe", "North America", "Oceania", "CIS", "Caribbean"), 
       col = c("firebrick2", "yellow", "yellowgreen", "purple", "black", "deepskyblue1", "tan4"),
       pch = c(15:18,7,19,11),cex = 0.7)

# add linear regression line to the plot as required by item 3 
abline(regline, col = "red", lty = 1, lwd = 1)
abline(h=6, col = "black", lty = 1, lwd = 1)

################################# End of scatterplot suicide ######################



### Check the distribution of gdp  
################ Scatterplot for gdp vs happiness score by region  ###############################

# fit the regression line with x as the suicide and y as the happiness score  
regline <- lm(happiness~gdp, data=data)
# output the coefficients of the regression equation
regline$coefficients
summary(regline)

# use the plot for category: continent = Africa as the base of the scatterplot 
plot(data$gdp[data$Region == "Africa"], data$happiness[data$Region == "Africa"],
     main = "GDP per Capita vs. Happiness Score by Country", 
     xlab = "GDP per capita", ylab = "Happiness Score", pch = 15, col = "firebrick2",
     xlim = c(0, 1.8), ylim = c(0,10))

# add the scatter points by category: Region, differentiated by color and type 
points(data$gdp[data$Region == "Asia"], data$happiness[data$Region == "Asia"],
       pch = 16, col = "yellow")
points(data$gdp[data$Region == "Europe"], data$happiness[data$Region == "Europe"],pch = 17, col = "yellowgreen")
points(data$gdp[data$Region == "North America"], data$happiness[data$Region == "North America"],pch = 18, col = "purple")
points(data$gdp[data$Region == "Oceania"], data$happiness[data$Region == "Oceania"],	pch = 7, col = "black")
points(data$gdp[data$Region == "CIS"], data$happiness[data$Region == "CIS"],pch = 19, col = "deepskyblue1")
points(data$gdp[data$Region == "Caribbean"], data$happiness[data$Region == "Caribbean"],pch = 11, col = "tan4")

# add legend to the plot, adjust the size by cex =, used color map to pick the colors 
#  that could help differentiate the data 
legend("topright", c("Africa", "Asia", "Europe", "North America", "Oceania", "CIS", "Caribbean"), 
       col = c("firebrick2", "yellow", "yellowgreen", "purple", "black", "deepskyblue1", "tan4"),
       pch = c(15:18,7,19,11),cex = 0.7)

# add linear regression line to the plot as required by item 3 
abline(regline, col = "red", lty = 1, lwd = 1)
text(0.3, 3, "Y = 3.877 * X + 4.033", cex = .8)

################################# End of scatterplot gdp ######################

### Check the distribution of life_expectancy  
################ Scatterplot for life_expectancy vs happiness score by region  ###############################

# fit the regression line with x as the suicide and y as the happiness score  
regline <- lm(happiness~life_expectancy, data=data)
# output the coefficients of the regression equation
regline$coefficients
summary(regline)

# use the plot for category: continent = Africa as the base of the scatterplot 
plot(data$life_expectancy[data$Region == "Africa"], data$happiness[data$Region == "Africa"],
     main = "Life Expectancy vs. Happiness Score by Country", 
     xlab = "Life Expenctancy", ylab = "Happiness Score", pch = 15, col = "firebrick2",
     xlim = c(0, 1.2), ylim = c(0,10))

# add the scatter points by category: Region, differentiated by color and type 
points(data$life_expectancy[data$Region == "Asia"], data$happiness[data$Region == "Asia"],
       pch = 16, col = "yellow")
points(data$life_expectancy[data$Region == "Europe"], data$happiness[data$Region == "Europe"],pch = 17, col = "yellowgreen")
points(data$life_expectancy[data$Region == "North America"], data$happiness[data$Region == "North America"],pch = 18, col = "purple")
points(data$life_expectancy[data$Region == "Oceania"], data$happiness[data$Region == "Oceania"],	pch = 7, col = "black")
points(data$life_expectancy[data$Region == "CIS"], data$happiness[data$Region == "CIS"],pch = 19, col = "deepskyblue1")
points(data$life_expectancy[data$Region == "Caribbean"], data$happiness[data$Region == "Caribbean"],pch = 11, col = "tan4")

# add legend to the plot, adjust the size by cex =, used color map to pick the colors 
#  that could help differentiate the data 
legend("topleft", c("Africa", "Asia", "Europe", "North America", "Oceania", "CIS", "Caribbean"), 
       col = c("firebrick2", "yellow", "yellowgreen", "purple", "black", "deepskyblue1", "tan4"),
       pch = c(15:18,7,19,11),cex = 0.7)

# add linear regression line to the plot as required by item 3 
abline(regline, col = "red", lty = 1, lwd = 1)
text(0.2, 2, "Y =  4.185 * X + 2.5336", cex = .8)

################################# End of scatterplot life_expectancy ######################


###########           Conclusion            ######################################

##### our final model 
model.dropb<-lm(happiness ~ social + freedom + life_expectancy + Safety_co + 
                        generosity + gdp + Tolerance_city + Safety_city + Cost_city + 
                        Climate_co, data = data)
summary(model.dropb)
# Adjusted R-squared:  0.8489  p-value: < 2.2e-16

model.drop<-lm(happiness ~  freedom + Safety_city + Tolerance_city + gdp  + social + 
                       Cost_city + Climate_co + generosity + Safety_co, data = data)
summary(model.drop)

compareLM(model.dropb, model.drop)

# drop Climate_co 
# Adjusted R-squared:  0.8427 p-value: < 2.2e-16

# drop Cost_city
# Adjusted R-squared:  0.8413

# drop Safety_city
# Adjusted R-squared:  0.8386 

# drop Tolerance_city
# Adjusted R-squared:  0.824 

# drop gdp
# Adjusted R-squared:  0.8076  

# drop generosity 
# Adjusted R-squared:  0.8435

# drop Safety_co
# Adjusted R-squared:  0.8406 

# drop life_expectancy
# Adjusted R-squared:  0.8367

# drop freedom 
# Adjusted R-squared:  0.8138

# drop social 
# Adjusted R-squared:  0.8403


####### Future steps 
# 1 Explore Safety_co and Safety_city
# Explore if I can combine Safety_co and Safety_city since both are significant 
#   in building the model but a little bit correlated since they're the same 
#   indicator for Safety scores of countries but just from two different dataset 
datacopy <- data 

# Scale the new column Safety
datacopy$Cost <- (datacopy$Cost_city + datacopy$Costs_co)/2 # it can help 
datacopy$Safety <- (datacopy$Safety_city + datacopy$Safety_co)/2 # it can help 

summary(datacopy$Cost)

########### Exam if the combination would influence the model 

#the old model with both variables Safety_co and Safety_city 
final<-lm(happiness ~ social + freedom + life_expectancy + Safety_co + 
                 generosity + gdp + Tolerance_city + Safety_city + Cost_city + 
                 Climate_co, data = datacopy)

#the new model with only one variable Safety 
new.final<-lm(happiness ~ social + freedom + life_expectancy +
                   generosity + gdp + Tolerance_city + Safety + Cost + 
                   Climate_co, data = datacopy)

### model performance ###
compareLM(final, new.final)
#$Fit.criteria
#Rank Df.res   AIC  AICc   BIC R.squared Adj.R.sq   p.value Shapiro.W Shapiro.p
#1   11     78 86.88 90.98 116.7    0.8661   0.8489 6.289e-30    0.9896    0.7056
#2   10     79 85.20 88.63 112.6    0.8656   0.8503 9.300e-31    0.9894    0.6951
