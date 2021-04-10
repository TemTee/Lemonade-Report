library(dslabs)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(lubridate)
library(psych)

#          *******            Predict lemonade sales using linear regression model        ***********


lemonade <- readxl::read_xlsx("lemonade edited.xlsx", 
                              sheet = "Lemonade Comma Separated Sheet")
lemonade <- lemonade %>% mutate(Month = month(as.Date(Date), label = TRUE),
                                Day = wday(as.Date(Date), label = TRUE),
                                Price = Price + 2,
                                Sales = round((Sales*100) + 0.5*Sales),
                                Temperature_in_Cel = (Temperature -32) * (100/180)) %>% 
  select(Date, Month,Day, Temperature_in_Cel, everything(), -Temperature)

str(lemonade)
head(lemonade)

#####                   *******     Exploring the data          **********

######################   unorder the data

set.seed(42, sample.kind = "Rounding")
row <- sample(nrow(lemonade))    
lemonade <- lemonade[row, ]
str(lemonade)
head(lemonade)

lemonade %>% 
  summary(
    c("Date", "Temperature_in_Cel", "Rainfall", "Price", "Sales"))# summary of distribution of the variables
plot(lemonade)
hist(lemonade$Sales)####### Visualize distribution of sales

round(cor(subset(lemonade, select = c(-Date, -Month, -Day)), method = "pearson"), digits = 2)# check for multivariate correlation

round(sapply(data.frame(subset(lemonade, select = c(-Date, -Month, -Day))),
       cor, y = lemonade$Sales, method = "pearson"), digits = 2) # check correlation between sales and other variables

# lemonade_z <- as.data.frame(scale(lemonade[4:8]))    # to  standardize the data using z score

# lemonade_z <- data.frame(lemonade[c(1,2,3)], lemonade_z) # adds other missing variables from lemonade

pairs.panels(lemonade[c(-1,-3)], scale = TRUE)  #  visualiz the correlation and distribution



#                   ******************          Build a model             **************


##################### split the data

library(caret)
set.seed(1, sample.kind = "Rounding")
index <- with(lemonade, createDataPartition(y= lemonade$Sales, times = 1, p = 0.3, list = FALSE))
train_set <- lemonade [-index,]
test_set <- lemonade [index, ]
str(train_set)
str(test_set)


#####################  train the model

fit <- lm(Sales ~ Temperature_in_Cel + Rainfall +
            Flyers + Temperature_in_Cel*Flyers + 
            Rainfall*Flyers + Month, data = train_set)
prediction <- predict(fit, test_set)

summary(fit)

74.2 / mean(train_set$Sales) # calculate the error rate of the model residual standard error is 74.2
RMSE(prediction, test_set$Sales)
R2(prediction, test_set$Sales)