library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(GGally)
library(here)
library(skimr)
library(janitor)
library(broom)
library(huxtable)
library(lubridate)
library(ggfortify)

# Explore the relationship between bikes hired and a bunch of explanatory variables

#read the CSV file
bike <- read_csv(here::here("data", "london_bikes.csv"))

typeof(bike$year)
# fix dates using lubridate, and generate new variables for year, month, month_name, day, and day_of _week
bike <- bike %>%   
  mutate(
    year=year(date),
    month = month(date),
    month_name=month(date, label = TRUE),
    day_of_week = wday(date, label = TRUE)) 

# generate new variable season_name to turn seasons from numbers to Winter, Spring, etc
bike <- bike %>%  
  mutate(
    season_name = case_when(
      month_name %in%  c("Dec", "Jan", "Feb")  ~ "Winter",
      month_name %in%  c("Mar", "Apr", "May")  ~ "Spring",
      month_name %in%  c("Jun", "Jul", "Aug")  ~ "Summer",
      month_name %in%  c("Sep", "Oct", "Nov")  ~ "Autumn",
    ),
    season_name = factor(season_name, 
                         levels = c("Winter", "Spring", "Summer", "Autumn"))
  )

# skimr::skim() gives us variable types, summary statistics, missing values
# anything that seems off
skim(bike)

# model building

# start the naive model where you just use the average
favstats(~bikes_hired, data = bike)

# can you create a confidence interval for mean bikes_hired? What is the SE?

t_critical = qt(0.975, count(bike)[[1]]-1) #count(bike) will return a tibble, [[1]] will return just the value
se_delta = sd(bike$bikes_hired)/sqrt(count(bike)[[1]])
radius = t_critical * se_delta
upper_bound = mean(bike$bikes_hired) + radius
lower_bound = mean(bike$bikes_hired) - radius

with(bike, t.test(~bikes_hired))

model1 <- lm(bikes_hired ~ 1, data= bike) #the ~1 runs the model for mean y
msummary(model1)

# What is the regression's residual standard error? 
#The Residual standard error is 9851 (how well mean fits data). 
#This number should decrease as we do a regression 

# What is the intercept standard error? 
#Intercept standard error is 148.8 while the intercept is 26746. The slope is zero because we run the
#model for the mean

# build a number of linear regression models where you try to explain
# bikes_hired with (some of) the explanatory variables included in the file.
all_variables_vs_bikes <-lm(bikes_hired ~ ., data= bike)
msummary(all_variables_vs_bikes)

season_name_vs_bikes <- lm(bikes_hired ~ season_name, data= bike)
msummary(season_name_vs_bikes)

mean_temp_vs_bikes <- lm(bikes_hired ~ mean_temp, data= bike) #Tells us y-intercept, slope, 
#t-value & r squared (how much variance of bikes_hired is explained by mean_temp)
msummary(mean_temp_vs_bikes)

library(moderndive)
bike%>%
  get_correlation(formula = bikes_hired ~ mean_temp) 
#gives us one value for correlation (0.635) but doesn't say if there is cause and effect

ggplot(bike, aes(mean_temp, bikes_hired))+
  geom_point()+
  geom_smooth(method = "lm")

rain_vs_bikes<- lm(bikes_hired ~ precipitation, data= bike)
msummary(rain_vs_bikes)

performance::check_model(rain_vs_bikes)

rainplustemp_vs_bikes<- lm(bikes_hired ~ precipitation + mean_temp, data= bike)
msummary(rainplustemp_vs_bikes)

# produce summary table comparing models using huxtable::huxreg()
# have a look at https://mam2023.netlify.app/example/modelling_side_by_side_tables/
library(palmerpenguins)


huxreg(season_name_vs_bikes, mean_temp_vs_bikes, rain_vs_bikes)
huxreg(list("Season" = season_name_vs_bikes, 
            "Temperature" = mean_temp_vs_bikes,
            "Rain" = rain_vs_bikes))

