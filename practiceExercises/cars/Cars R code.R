
library(tidyverse)
library(ggplot2)
library(forcats)

## Set the working directory
setwd("D:/learning/BABI Online")

# Read data from CSV file
claim_data <- read.csv("car.csv")

## Look at the first few rows.
head(claim_data)


## Choose 5 features only to work on.
work_data <- claim_data[,c("AGE", "MSTATUS", "SEX", "EDUCATION", "RED_CAR")]

## Look at the structure
str(work_data)


## Look at the attributes of work_data$MSTATUS
attributes(work_data$MSTATUS)

## Correct the levels of MSTATUS
levels(work_data$MSTATUS) <- c("Yes", "No")

## Correct the levels using function from forcats package
work_data$SEX <- fct_collapse(work_data$SEX, F = c("z_F"))

## Now the structure of data is correct.
## Lets look at the summary statistics
summary(work_data)

## Lets find out how many people with age less than 18 are filing for claims
work_data[ which(work_data$AGE < 18), ]

## Lets do some more analysis
## Group the age into buckets 
## Add a new variable agegroup with these buckets
work_data$agegroup <- cut(work_data$AGE, breaks = c(0,35,50, 100), labels = c("less than 35", "35 to 50", "more than 50"))

## Now see the summary data with new field added
summary(work_data)

## Generate the frequency tables of RED_CAR and MSTATUS for agegroup
red_car_stats <- table(work_data$agegroup, work_data$RED_CAR)
mstatus_stats <- table(work_data$agegroup, work_data$MSTATUS)
total_cars <- table(work_data$agegroup)

## Combine the RED_CAR and MSTATUS into a dataframe
output <- cbind(total_cars, red_car_stats[,2], mstatus_stats[,1])
output <- data.frame(output)

## Check the attribute of the output variable
attributes(output)

## Update the column names of the features
colnames(output) <- c("Total_Cars", "Red_Cars", "Marital_Status")

## Print the output
output

output$red_car_percent <- (output$Red_Cars/output$Total_Cars * 100)
output$red_car_percent <- round(output$red_car_percent, 2)

## Print the output
output


## We can save the output as CSV
write.csv(output, "output.csv")


## boxplot for one variable (work_data$AGE)
## It specifies the outliers
boxplot(work_data$AGE) 

## Plot more than one variables
plot(work_data$SEX, work_data$AGE)
plot(work_data$EDUCATION, work_data$AGE)


## Example of ggplot
ggplot(data = work_data, mapping = aes(x = SEX, y = AGE)) + geom_boxplot( aes(colour = EDUCATION), outlier.colour = "red")
