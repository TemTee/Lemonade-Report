library(dslabs)
library(tidyverse)
library(dplyr)
library(gridExtra)
options(digits = 5)
lemonade <- readxl::read_xlsx("lemonade edited.xlsx", 
                              sheet = "Lemonade Comma Separated Sheet")
lemonade <- lemonade %>% 
  mutate(Month = months.Date(Date),Temperature = (Temperature -32) * (100/180)) %>% 
  select(Date, Month, everything())
lemonade %>% 
  summary(c("Date", "Temperature", "Rainfall", "Price", "Sales"))

plot(lemonade$Temperature,lemonade$Rainfall) # to confirm the relationship between rainfall
# and temperature is negative


sales_by_temp <- lemonade  %>% 
  ggplot(aes(Sales,Temperature, color = Month, label = Month)) +
  geom_point(show.legend = FALSE)+
  geom_text(show.legend = FALSE, nudge_y = 1)+
  geom_jitter()+
  labs(title = "There is a positive relationship between sales and temperature", 
       subtitle = "Correlation btw Sales and Temp ~ 99%",
       xlab = "Sales", ylab = "Temperature")+
  coord_flip()

temp_vs_flyers <- lemonade %>%
  ggplot(aes(Temperature,Flyers, color = Month, label = Month)) +
  geom_text(show.legend=FALSE,nudge_y = 1)+
  geom_point(show.legend=FALSE)+
  labs(title = "Temperature also relates positively with number of flyers distributed per time", 
       subtitle = "Correlation btw Sales and Temp ~ 80%",
       xlab = "Temperature", ylab = "Flyers")

sale_by_flyers <- lemonade %>%
  ggplot(aes(Sales,Flyers, color = Month, label = Month)) +
  geom_text(show.legend=FALSE,nudge_y = 1)+
  geom_point(show.legend=FALSE)+
  xlab("Sales")

sales_by_mnth <- lemonade  %>%
  ggplot(aes(x=fct_reorder(Month, Sales),y=Sales)) +
  geom_col()+
  xlab("Month")+
  labs(title = "Highest Sales was made in the Month of July followed by June, August,
       May and September while December, January, February and November had lowest Sales",
       subtitle = "Months with low sales can be improved by making more awareness or
       come up with product that will be suitable for the temperature/weatherw` ",
       xlab = "Month", ylab = "Total Sales")+
  coord_flip()

(res_Sales_Temp <- cor.test(lemonade$Sales,lemonade$Temperature, method = "pearson"))
(res_Sales_Price <- cor.test(lemonade$Sales,lemonade$Price, method = "pearson"))
(res_Sales_Flyers <- cor.test(lemonade$Sales,lemonade$Flyers, method = "pearson"))
(res_Temp_Rainfall <- cor.test(lemonade$Temperature,lemonade$Rainfall, method = "pearson"))
(res_Sales_Rainfall <- cor.test(lemonade$Sales,lemonade$Rainfall, method = "pearson"))
(res_flyer_Temp <- cor.test(lemonade$Flyers,lemonade$Temperature, method = "pearson"))

Sale_To_Temp <- cor(lemonade$Sales,lemonade$Temperature)
Sale_To_Price <- cor(lemonade$Sales,lemonade$Price)
Sale_To_Flyers <- cor(lemonade$Sales,lemonade$Flyers)
Temp_To_Rainfal <- cor(lemonade$Temperature,lemonade$Rainfall)
Sales_To_Rainfal <- cor(lemonade$Sales,lemonade$Rainfall)
flyers_To_Temp <- cor(lemonade$Flyers,lemonade$Temperature)

corr_btw_var <- as.data.frame(table(Sale_To_Temp,
                                    Sale_To_Flyers,
                                    flyers_To_Temp,
                                    Sale_To_Price,
                                    Temp_To_Rainfal,
                                    Sales_To_Rainfal,
                                    flyers_To_Temp))
corr_btw_var
grid.arrange(temp_vs_flyers,sales_by_temp,sale_by_flyers,sales_by_mnth, ncol = 2,nrow = 2)