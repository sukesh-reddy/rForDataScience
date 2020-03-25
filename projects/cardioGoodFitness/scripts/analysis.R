###########################
# Cardio Good Fitness EDA
###########################


##### Step 1(Envoirnment Setup and Data Import) ###############

# 1.1 Installing the necessary Packages  and Invoke libraries
#install.packages("DataExplorer")
# install.packages("rpivotTable")
library(DataExplorer)
library(rpivotTable)
library(readr)

# 1.2 Setting the working directory
setwd('D:/dataScience/greatLearning/rForDataScience/projects/cardioGoodFitness/data')

# 1.3 Import and read the data Set
df.cardio <- read.csv("CardioGoodFitness.csv")

######## Step 2(Variable identification) ###############

# 2.1 Variable Identification
head(df.cardio) # View of the dataset

tail(df.cardio)

# names(df.cardio)  - if you have large number of columns

str(df.cardio) #structure of the dta set

plot_str(df.cardio)

summary(df.cardio) #High Level Overview of the data

plot_intro(df.cardio)

plot_missing(df.cardio)
# 2.2 Variable Inference

############ Step 3(Univarient Analysis) #############
# categorical Variable PLot
cat.plot <- function(df){
  
  d.types <- sapply(df, class)
  col.names <- names(d.types[d.types %in% c('factor','character')])
 #print(col.names)
  for (col in col.names){
    a <- ggplot(data=df) +
      geom_bar(aes_string(x=col,)) +
      #geom_text(aes(label=..count..), vjust=1.6, color="white", size=3.5) + 
      #geom_text(stat='count', aes(label=..count..), vjust=-1)
    print(a)
    Sys.sleep(1)
  }
}
cat.plot(df.cardio)

attach(df.cardio)

par(mfrow = c(1,3));
text(x = barplot(table(Product),main = "Product",ylab = "Frequency"), cex=1,pos=3);
text(x = barplot(table(Gender),main = "Gender",ylab = "Frequency"), y = 0,
     table(Gender), cex=1,pos=3);
text(x = barplot(table(MaritalStatus),main = "Marital Status",ylab = "Frequency"), y = 0, table(MaritalStatus), cex=1,pos=3);

par(mfrow = c(3,2));
text(x= barplot(table(Education),main = "Education",ylab = "Frequency"), y= 0, table(Education), cex=1,pos=3);
boxplot(Education, horizontal = TRUE, main = "Education");
text(x= barplot(table(Usage),main = "Usage",ylab = "Frequency"), y = 0, table(Usage), cex=1,pos=3);
boxplot(Usage, horizontal = TRUE, main = "Usage");
text(x= barplot(table(Fitness),main = "Fitness",ylab = "Frequency"), y = 0, table(Fitness), cex=1,pos=3);
boxplot(Fitness, horizontal = TRUE, main = "Fitness");

hist(Age, main = "Age", ylim = c(0,75), xlab = "", labels = TRUE, col = "dark grey");
boxplot(Age, horizontal = TRUE, main = "Age");
text(fivenum(Age), labels =fivenum(Age), y=1.25)
rug(Age);

hist(Income, main = "Income", ylim = c(0,60), xlab = "", labels = TRUE, co
     l = "dark grey");
boxplot(Income, horizontal = TRUE, main = "Income");
text(fivenum(Income), labels =fivenum(Income), y=1.25)
rug(Income);

hist(Miles, main = "Miles", ylim = c(0,105), xlab = "",labels = TRUE, col
     = "dark grey");
boxplot(Miles, horizontal = TRUE, main = "Miles");
text(fivenum(Miles), labels =fivenum(Miles), y=1.25)
rug(Miles);

########## STEP 4(Bi-Varient Analysis) ##############
boxplot(Age~Product, horizontal = TRUE, main = "Age Distribution by Produc
t");
rug(Age);

boxplot(Education~Product, horizontal = TRUE, main = "Education Distributi
on by Product");
rug(Education);

boxplot(Usage~Product, horizontal = TRUE, main = "Usage Distribution by Pr
oduct");
rug(Usage);

boxplot(Fitness~Product, horizontal = TRUE, main = "Fitness Distribution b
y Product");
rug(Fitness);

boxplot(Income~Product, horizontal = TRUE, main = "Income Distribution by
Product");
rug(Income);

boxplot(data$Miles~data$Product, horizontal = TRUE, main = "Miles Distribu
tion by Product");
rug(data$Miles);

boxplot(data$MilesPerUsage~Product, horizontal = TRUE, main = "Miles Per U
sage Distribution by Product");
rug(data$MilesPerUsage);

data1 = table(Gender, Product);
barplot(data1, main="Product Buyers by Gender", xlab="Product Name", col=c
        ("pink","blue"), legend = rownames(data1), beside=TRUE)

data2 = table(MaritalStatus, Product);
barplot(data2, main="Product Buyers by Marital Status", xlab="Product Name
", col=c("orange","light green"), legend = rownames(data2), beside=TRUE)

plot_correlation(data)

######### Step 5(Missing Value Treatment) ###########

######### Step 6(Outlier Treatment) ##############

############ Step 7(Variable Trandformation/Feature Creation) #################

# 7.1 Creating Miles Per Usage Variable
# df.cardio$milesPerUsage <- df.cardio$Miles/df.cardio$Usage

########### Step 8(Feature Exploration) #####################


