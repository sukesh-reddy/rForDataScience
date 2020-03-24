#1 Set the working directory for your work
setwd('D:/dataScience/greatLearning/rForDataScience/practiceAssesments')

#2 Import the dataset in the console in the .xls(x) format
library(readxl)
df.food <- read_excel('Food Nutrition-1 (1).xlsx')

#3 View the top 10 rows of the data
head(df.food,10)

#4 View the last 20 rows of the data
tail(df.food,20)

#5 summary of the data
summary(df.food)

#6 Create a vector "test" using the top 10 values of variable Protein_(mg)
test <- head(df.food$`Protein_(g)`,10)

#7 Select the top 5 rows of initial 5 variables in a matrix format
df.food[1:5,1:5]

#8 What is the class of the Sodium_(mg) variable
class(df.food$`Sodium_(mg)`)

#9 Create a new variable "EPW" by dividing Energ_Kcal with the Water; what is the dimension of the new dataset?
df.food$EPW <- df.food$Energ_Kcal/df.food$`Water_(g)`
dim(df.food)

#10 Create a subset of the dataset, where the Energ_Kcal is less than 500, what is the dimension of this new dataset?
filterd.df <- df.food[df.food$Energ_Kcal<500,]
dim(filterd.df)

#11 Create a plot between Enrg_Kcal and Water using the new subset created
library(ggplot2)
ggplot(data = filterd.df) +
  geom_point(aes(x=`Water_(g)`,y=Energ_Kcal))

#12 Plot a histogram of Sugar_tot variable using the new subset
library(ggplot2)
ggplot(data = filterd.df) +
  geom_histogram(aes(x=`Sugar_Tot_(g)`))

#13 Find the top 10 products based on following
  #1 Higher the Energy_Kcal, higher the ranking
top.10.products <- df.food[order(-df.food$Energ_Kcal),'Shrt_Desc'][1:10,]

  #2 Lower the water content, higher the ranking
lower.10.products <- df.food[order(df.food$`Water_(g)`),'Shrt_Desc'][1:10,]

#14 Create a subset of the data where product_desc contains "CHEESE" and list down the summary statistics of the subset
library(dplyr)
df.food %>% filter(grepl("CHEESE",Shrt_Desc)) %>%
  summary()
