##################################
# ----- This is an example -------
# Created by: Xianjie Guan
# Date: 02/03/2021
# Edited:
# HW : HW 1
##################################

setwd('C:/Users/xianj/Desktop/data maining 2/HW1')

library('ISLR')
head(College)
colnames(College)
# a) Use the function summary() to produce a numerical summary of the variables in the dataset.
summary(College)

# b) Use Pairs() to produce a scatterplot of the continuous variables in the data set. 
pairs(College[ ,2:18])

savepdf('scatterplots')

# c) Create a new qualitative variable called "Elite", by "binning" the variable
# "Top10perc". We are going to divide universities into two groups based on
# whether or not the proportion of students coming from the two 10% of their
# high school exceeds 50%. Add this variable to your dataset.



College$Top10perc
high_50 <- which(College$Top10perc > 50)
Elite <- College$Top10perc
Elite[high_50] <- 1
Elite[-high_50] <- 0
clean_College=data.frame(College ,Elite)
head(clean_College)
# d) Use the table function to figure out how many Elite schools there are.

table(clean_College$Elite)

# e) Use the table function to figure out how many of the Elite schools are private

table(clean_College$Elite, clean_College$Private)

# f) Do elite schools tend to have higher graduation rates?
table(clean_College$Elite, clean_College$Grad.Rate)

# the answer is no, elite schools won't raise graduation rates.
# However, the lower 50% of Top10perc students has graduation rates between 45% to 84% 




# Q2)
head(Auto)
colnames(Auto)
data(Auto)
# a) Remove missing values from the data.
summary(is.na(Auto))

# b) What variables are numerical (continuous) or factors (categorical)?
sapply(Auto, class)#before change origin is numerical
Auto$origin <- factor(Auto$origin, levels=1:3, labels=c("American", "European", "Japanese"))
sapply(Auto, class)#after change origin is factor

# c) Report the mean and standard deviation for each continuous variable in the data.

sapply(Auto[ ,c('mpg','displacement','horsepower','weight','acceleration', 'year' )], mean)
sapply(Auto[ ,c('mpg','displacement','horsepower','weight','acceleration', 'year' )], sd)

# d) Remove the 5th through 55th observation. What is the range, mean and
# standard deviation? 
Auto_new <- Auto[-c(5:55), ]
dim(Auto_new)
range_Auto_new_range <- data.frame(sapply(Auto_new[ ,c('mpg','displacement','horsepower','weight','acceleration', 'year' )], range))
rownames(range_Auto_new_range) <- c("min:", "max:")

range_Auto_new_mean <- sapply(range_Auto_new_range[ ,c('mpg','displacement','horsepower','weight','acceleration', 'year' )], mean)

range_Auto_new_sd <- sapply(range_Auto_new_range[ ,c('mpg','displacement','horsepower','weight','acceleration', 'year' )], sd)

Auto_datasets <- list()
Auto_datasets[[1]] <- range_Auto_new_range
Auto_datasets[[2]] <- range_Auto_new_mean
Auto_datasets[[3]] <- range_Auto_new_sd

names(Auto_datasets) <- c("range", "mean",'sd')
names(Auto_datasets)
Auto_datasets

#e) In the full Auto dataset, are there any variables you would consider removing,
# or representing differently? Why? 
# the column name "name" can be consider removing since it contain 392 individual vehicle  model names
# and it is not going to help us to predict any other numerical of column names. 

#f) In the full Auto dataset, graphically explore the relationships between the
# variables in the data set. 
pairs(Auto[ ,1:7])
savepdf('Auto_relationship_plots')

#g) In the full Auto dataset, consider the variable mpg. You are going to create a
# new categorical variable for MPG, which has the categories: {low, med,
#  high}. Call this variable "my_mpg", and create a new_Auto dataset, which
# contains all of the Auto variables, and your new variable "my_mpg". Save
# the dataset as an *.RData file and submit it with your assignment.

Auto_new2 <- Auto
summary(Auto_new2$mpg)
boxplot(Auto_new2$mpg,horizontal = T)

low <- which(Auto_new2$mpg <= 17)
med <- which(Auto_new2$mpg > 17 & Auto_new2$mpg <= 29)
high <- which(Auto_new2$mpg >29)
colnames(Auto_new2)[1] <- "my_mpg"
MPG<- Auto_new2$my_mpg
MPG[low] <- 'Low'
MPG[med] <- 'Med'
MPG[high] <- 'High'


as.factor(MPG)
as.numeric(Auto_new2$MPG)
head(Auto_new2)
Auto_new2[ ,1] <- MPG
head(Auto_new2)

# save data in an R object
save(Auto_new2, file = "myAuto.RData")
load("myAuto.RData")



