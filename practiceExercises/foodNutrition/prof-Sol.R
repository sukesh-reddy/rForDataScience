#1-Set up working directory
setwd("C:/Users/admin/Documents")

#2-2.	Import the dataset in the console in the .xls(x) format
library(readxl)
Food_Nutrition <- read_excel("C:/Users/admin/Desktop/Work/BABI-Intl/Basics of R-2/Food Nutrition.xlsx")

#3-View the top 10 rows of the data
head(Food_Nutrition,10)

#4-View the last 20 rows of the data
head(Food_Nutrition,20)

#5-Show the summary of the data
summary(Food_Nutrition)

#6-Create a vector "test" using the top 10 values of variable Protein_(mg)
test<-head(Food_Nutrition$`Protein_(g)`,10)
test

#7-Select the top 5 rows of initial 5 variables in a matrix format
Food_Nutrition[1:5,1:5]

#8-What is the class of the Sodium_(mg) variable
class(Food_Nutrition$`Sodium_(mg)`)

#9-Create a new variable "EPW" by dividing Energ_Kcal with the Water; what is the 
# dimension of the new dataset?
Food_Nutrition$EPW<-Food_Nutrition$Energ_Kcal/Food_Nutrition$`Water_(g)`
head(Food_Nutrition$EPW)
dim(Food_Nutrition)

#10.	Create a subset of the dataset, where the Energ_Kcal is less than 500, what 
# is the dimension of this new dataset?
food_new<-Food_Nutrition[Food_Nutrition$Energ_Kcal<500,]
dim(food_new)

#11-Plot a Bar Plot between Enrg_Kcal and Water using the new subset created
plot(food_new$Energ_Kcal,food_new$`Water_(g)`, main = "Plot between Energ_KCal vs Water")

#12-Plot a histogram of Sugar_tot variable using the new subset
hist(food_new$`Sugar_Tot_(g)`)

#13-	Find the top 10 products based on following:
#13.1-	Higher the Energy_Kcal, higher the ranking
x<-Food_Nutrition
y<-x[order(-x$Energ_Kcal),]
head(y$Shrt_Desc,10)
#13.2-	Lower the water content, higher the ranking
z<-x[order(x$`Water_(g)`),]
head(z$Shrt_Desc,10)

#14-Create a subset of the data where product_desc contains "CHEESE" and 
# list down the summary statistics of the subset
library(dplyr)
m<-filter(x,grepl("CHEESE",x$Shrt_Desc))
head(m$Shrt_Desc)
summary(m)

#15.	Using the cut function on water variable divide the whole data into 6 bins, list 
# down the summary statistics of all the 6 bins
l<-Food_Nutrition
l$bins<-cut(x$`Water_(g)`, 6, labels=c("A", "B", "C","E","F","G"))
View(l)
bin1<-filter(l,l$bins=="A")
View(bin1)
summary(bin1)
