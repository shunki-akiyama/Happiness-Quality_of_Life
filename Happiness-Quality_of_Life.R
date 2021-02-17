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
data <- data[,c(1:3,14:20)]
data <- rename(data, "Region"="Regional indicator",
               "happiness"="Ladder score",
               "gdp"="Explained by: Log GDP per capita",
               "social"="Explained by: Social support", 
               "life_expectancy"="Explained by: Healthy life expectancy",
               "freedom"="Explained by: Freedom to make life choices",
               "generosity"="Explained by: Generosity",
               "perceptions"="Explained by: Perceptions of corruption",
               "dystopia_residual"="Dystopia + residual"
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

###changing data types####
summary(data)
data$Region <- factor(data$Region)
data$Score_cat <- factor(data$Score_cat, levels= c("Low", "Middle", "High")) 
