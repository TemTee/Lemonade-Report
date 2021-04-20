#          *******            Predict lemonade sales using linear regression model        ***********


library(dslabs)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(lubridate)
library(psych)

#          *******            Load and manipulate the data        ***********


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

######################   unorder the data

set.seed(42, sample.kind = "Rounding")
row <- sample(nrow(lemonade))    
lemonade <- lemonade[row, ]
str(lemonade)
head(lemonade)


#####                   *******     Exploring the data          **********

plot(lemonade$Month, lemonade$Sales)
plot(lemonade$Day, lemonade$Sales)

####### Visualize distribution of sales
plot(lemonade)
hist(lemonade$Sales)

# summary of distribution of the variables

lemonade %>% 
  summary(
    c("Date", "Temperature_in_Cel", "Rainfall", "Price", "Sales"))

describe(lemonade)

# check for multivariate correlation in the dataset

round(cor(subset(lemonade, select = c(-Date, -Month, -Day)), method = "pearson"), digits = 2)

# check correlation between sales and other variables

round(sapply(data.frame(subset(lemonade, select = c(-Date, -Month, -Day))),
       cor, y = lemonade$Sales, method = "pearson"), digits = 2)

# to  standardize the data using z score

# lemonade_z <- as.data.frame(scale(lemonade[4:8])) 

# add other missing variables from lemonade

# lemonade_z <- data.frame(lemonade[c(1,2,3)], lemonade_z) 


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

#     Evaluate the model on the test set

prediction <- predict(fit, test_set)

summary(fit)

# calculate the error rate of the model using the residual standard error 74.2

74.2 / mean(train_set$Sales) 

#  our rmse using the psych package appears ok for a 0 to over four thousand range

RMSE(prediction, test_set$Sales)
RMSE(fit, train_set$Sales)

# our r ssquared is also at %
R2(prediction, test_set$Sales)

hist(residuals(fit))