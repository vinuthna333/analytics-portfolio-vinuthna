#EDA

library(ggplot2) 
library(ggrepel)
 
#Import the data

df <- read.csv('C:/New folder/bike_buyers.csv', header = T, na.strings = "")
head(df)

#Check the data

str(df)
summary(df)
colSums(is.na(df))

#Replacing the missing values in Character columns with "unknown"

df$Gender[is.na(df$Gender)] <- "Unknown"
df$Marital.Status[is.na(df$Marital.Status)] <- "Unknown"
df$Home.Owner[is.na(df$Home.Owner)] <- "Unknown"

colSums(is.na(df))
str(df)

#Converting Charcter variables to Factor variables(not all but checking and converting all those that can be converted)

df$Marital.Status <- as.factor(df$Marital.Status)
df$Gender <- as.factor(df$Gender)
df$Home.Owner <- as.factor(df$Home.Owner)
df$Purchased.Bike <- as.factor(df$Purchased.Bike)

str(df)

#Handling other columns missing values

#Income
hist(df$Income) #Right skewed so replace it with Median

df$Income[is.na(df$Income)] <- median(df$Income, na.rm = T)

colSums(is.na(df))

#Age
hist(df$Age) #Right skewed so replace it with Median

df$Age[is.na(df$Age)] <- median(df$Age, na.rm = T)

colSums(is.na(df))

#Children
hist(df$Children)  #not a continuous variable, cannot be defined in distribution so replace it with Mode
#Mode is most repeating value
library(modeest)   #library to calculate mode

df$Children[is.na(df$Children)] <- mfv(df$Children, na_rm = T) #mfv is function from library modeest

colSums(is.na(df))

#Cars
hist(df$Cars) #not a continuous variable, cannot be defined in distribution so replace it with Mode

df$Cars[is.na(df$Cars)] <- mfv(df$Cars, na_rm = T) #mfv is function from library modeest

colSums(is.na(df))

#Check the Outliers
#Identify for which variables outliers will affect the calculation and deal with those
#Here it is Income and Age
#We need to check the outliers and decide if we want to remove the outlier or not,
#If we decide not to remove the outlier, we can replace it with Median

boxplot.stats(df$Income)
boxplot(df$Income) #not removing outliers, can be replaced with Median

boxplot.stats(df$Age)
boxplot(df$Age)

#Removing the Age 89 outlier
df1 <- df[df$Age != 89,] #taking all the values in df except for age 89 and assigning it to df1

boxplot.stats(df1$Age) 
boxplot(df1$Age)

#Saving the files in csv format

#1.With outliers (df)
write.csv(df, file = 'bikers_cleaned_withoutliers.csv', row.names = FALSE)

#2.Without outliers (df1)
write.csv(df1, file = 'bikers_cleaned_outlierremoved.csv', row.names = FALSE)

head(df1)

#QUESTIONS

#1.How many purchased bike?

bike <- xtabs(~Purchased.Bike, data = df1)
bike

#2.How many females and males bought the bike?

gp <- xtabs(~Purchased.Bike + Gender, data = df1)
gp

barplot(gp, xlab = "Gender", ylab= "Count", main = "Bikes Vs.Gender")

#3.How many bikes purchased in region?

dfp = df1[df1$Purchased.Bike == "Yes",]
region <- xtabs(~ Region, data = dfp)
region

rt <- table(dfp$Region) #no idea why this was done here
rt

barplot(region, xlab = "Region", ylab = "Count", main = "Bikes purchased in each region")

# Correlation between Income and Age

cor(df1$Income, df1$Age)

#Scatter plot of Income by Age

plot(df1$Age, df1$Income, xlab = "Age", ylab = "Income", 
     main = "Scatter Plot of Income by Age", col = "blue", pch = 16)

