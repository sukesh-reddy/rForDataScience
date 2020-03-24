setwd('D:/dataScience/greatLearning/rForDataScience/practiceAssesments/cars')

# Reading the data
library(readr)
df.cars <- read_csv('Car.csv')

#Lokking at the data
df.cars
glimpse(df.cars)

#1 You  have to create derived variables  and focus on two variables -AGE  & RED_CA
#filtering the data
library(dplyr)
df.filterd <- df.cars %>% select(AGE,RED_CAR)

#2 You  have to segregate the age groups into 3 intervals -???Less than 35,35-48,49+
df.filterd$age_grouped <- cut(df.filterd$AGE,breaks =c(0,34,48,Inf) ,labels = c('less than 35','35-48','49+'))

#3
df.filterd$RED_CAR <- as.factor(df.filterd$RED_CAR)
 df.filterd %>% group_by(age_grouped) %>%
   summarise(total.Cars = n(),
             red.Cars = sum(RED_CAR %in% ('yes'),rm.na=TRUE),
             per.of.red.cars = (red.Cars/total.Cars)*100)
