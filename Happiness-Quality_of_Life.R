library(dplyr)
library(readxl)

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

#delete 6 rows from data
data <- data[complete.cases(data),]
summary(data)


####Merge Quality of Life by city###




####changing data types####
summary(data)
data$Region <- factor(data$Region)
data$Score_cat <- factor(data$Score_cat, levels= c("Low", "Middle", "High")) 

#### check correlation####
cor(data[,c(3:9,11:17)])
