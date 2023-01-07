# MSBX 5415
#BigCityHealth Final R Code

rm(list = ls())
setwd("/Users/bates.crowther/Documents/College/Graduate School - MSBA/Fall 2022/Advanced Data Analytics - MSBX 5415 /Project/Big City Health")
bigCityHealth <- read.csv("Big_Cities_Health_Data_Inventory.csv", stringsAsFactors = TRUE)
head(bigCityHealth)

#---------------------------------------------------------------------------------------------------------------------------

# Summary

str(bigCityHealth)

nrow(bigCityHealth)

nrow(is.na(bigCityHealth))
colSums(is.na(bigCityHealth))

bigCityHealth <- na.omit(bigCityHealth)

tapply(bigCityHealth$Value, bigCityHealth$Indicator.Category, mean)

#---------------------------------------------------------------------------------------------------------------------------

# Sub-setting the Data

top_years = subset(bigCityHealth, bigCityHealth$Year == "2013"| bigCityHealth$Year == "2012" | bigCityHealth$Year == "2011")
top_years = top_years[,-c(8:11)]
str(top_years)

#---------------------------------------------------------------------------------------------------------------------------

# Average Life Expectancy at Birth (Years) by City

LE.topyears <- subset(top_years, top_years$Indicator == "Life Expectancy at Birth (Years)")

LE.topyears

lm.LEplace.topyears <- lm(LE.topyears$Value ~ LE.topyears$Place, data = LE.topyears)
summary(lm.LEplace.topyears)

tapply(LE.topyears$Value, LE.topyears$Place, mean)

meanTY_LE = tapply(LE.topyears$Value, LE.topyears$Place, mean)

sort(meanTY_LE, decreasing = FALSE, na.last = NA)

#---------------------------------------------------------------------------------------------------------------------------

# Linear Regression/Time Series of Life Expectancy at Birth

lm.TS.topyears <- lm(LE.topyears$Value ~ LE.topyears$Year, data = LE.topyears)
summary(lm.LEplace.topyears)

plot(lm.TS.topyears)

#---------------------------------------------------------------------------------------------------------------------------

# Average All-Cause Mortality Rate by City

MR.topyears <- subset(top_years, top_years$Indicator == "All-Cause Mortality Rate (Age-Adjusted; Per 100,000 people)")

MR.topyears

lm.MRplace.topyears <- lm(MR.topyears$Value ~ MR.topyears$Place, data = MR.topyears)
summary(lm.LEplace.topyears)

tapply(MR.topyears$Value, MR.topyears$Place, mean)

meanTY_MR = tapply(MR.topyears$Value, MR.topyears$Place, mean)

sort(meanTY_MR, decreasing = FALSE, na.last = NA)

404.1611/100000

#---------------------------------------------------------------------------------------------------------------------------

# Average All-Type Cancer Mortality Rates 

C_MR.topyears <- subset(top_years, top_years$Indicator == "All Types of Cancer Mortality Rate (Age-Adjusted; Per 100,000 people)")

lm.C_MRplace.topyears <- lm(C_MR.topyears$Value ~ C_MR.topyears$Place, data = C_MR.topyears)
summary(lm.C_MRplace.topyears)

tapply(C_MR.topyears$Value, C_MR.topyears$Place, mean)

meanTY_C_MR = tapply(C_MR.topyears$Value, C_MR.topyears$Place, mean)

sort(meanTY_C_MR, decreasing = FALSE, na.last = NA)

0.0007219000*100000

#---------------------------------------------------------------------------------------------------------------------------

# Average Heart Disease Mortality Rates 

HD_MR.topyears <- subset(top_years, top_years$Indicator == "Heart Disease Mortality Rate (Age-Adjusted; Per 100,000 people)")

lm.HD_MRplace.topyears <- lm(HD_MR.topyears$Value ~ HD_MR.topyears$Place, data = HD_MR.topyears)
summary(lm.C_MRplace.topyears)

tapply(HD_MR.topyears$Value, HD_MR.topyears$Place, mean)

meanTY_HD_MR = tapply(HD_MR.topyears$Value, HD_MR.topyears$Place, mean)

sort(meanTY_HD_MR, decreasing = FALSE, na.last = NA)

#---------------------------------------------------------------------------------------------------------------------------

# Average Lung Cancer Mortality Rates 

LC_MR.topyears <- subset(top_years, top_years$Indicator == "Lung Cancer Mortality Rate (Age-Adjusted; Per 100,000 people)")

lm.LC_MRplace.topyears <- lm(LC_MR.topyears$Value ~ LC_MR.topyears$Place, data = LC_MR.topyears)
summary(lm.C_MRplace.topyears)

tapply(LC_MR.topyears$Value, LC_MR.topyears$Place, mean)

meanTY_LC_MR = tapply(LC_MR.topyears$Value, LC_MR.topyears$Place, mean)

sort(meanTY_LC_MR, decreasing = FALSE, na.last = NA)

#---------------------------------------------------------------------------------------------------------------------------

# Average Pneumonia and Influenza Mortality Rates 

PI_MR.topyears <- subset(top_years, top_years$Indicator == "Pneumonia and Influenza Mortality Rate (Age-Adjusted; Per 100,000 people)")

lm.PI_MRplace.topyears <- lm(PI_MR.topyears$Value ~ PI_MR.topyears$Place, data = PI_MR.topyears)
summary(lm.PI_MRplace.topyears)

tapply(PI_MR.topyears$Value, PI_MR.topyears$Place, mean)

meanTY_PI_MR = tapply(PI_MR.topyears$Value, PI_MR.topyears$Place, mean)

sort(meanTY_PI_MR, decreasing = FALSE, na.last = NA)

#---------------------------------------------------------------------------------------------------------------------------

# Average Diabetes Mortality Rates 

D_MR.topyears <- subset(top_years, top_years$Indicator == "Diabetes Mortality Rate (Age-Adjusted; Per 100,000 people)")

lm.D_MRplace.topyears <- lm(D_MR.topyears$Value ~ D_MR.topyears$Place, data = D_MR.topyears)
summary(lm.D_MRplace.topyears)

tapply(D_MR.topyears$Value, D_MR.topyears$Place, mean)

meanTY_D_MR = tapply(D_MR.topyears$Value, D_MR.topyears$Place, mean)

sort(meanTY_D_MR, decreasing = FALSE, na.last = NA)

#---------------------------------------------------------------------------------------------------------------------------

# start of decision tree
library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)


sample_data = sample.split(LE.topyears, SplitRatio = 0.6)
train_data <- subset(LE.topyears, sample_data == TRUE)
test_data <- subset(LE.topyears, sample_data == FALSE)

unique(test_data$Indicator)


# temp code
# train_data1 = train_data[!is.na(train_data$Value), ]

library(tree)
re_cols = names(test_data)[sapply(test_data[, sapply(test_data, is.factor)], nlevels) > 20]
test_data1  = test_data[, !(names(test_data) %in% re_cols)]
test_data2 = cbind(test_data[, c("Value", "Indicator")], test_data1)
test_data2$Notes = NULL
str(test_data2)


test_data3 = test_data[, c("Value", "Indicator", "Year", "Gender", 
                           "Race..Ethnicity", "Place")]

test_data3$Indicator = factor(test_data3$Indicator)
model<- tree(Value ~ ., test_data3)
plot(model)
text(model)

install.packages("rpart")
library(rpart)

#---------------------------------------------------------------------------------------------------------------------------

# random forest

library(randomForest)

set.seed(888)
df.rf <- randomForest(Value ~ ., data= test_data3, importance=TRUE,
                      proximity=TRUE)
print(df.rf)

plot.randomForest(df.rf)

?plot.randomForest



#---------------------------------------------------------------------------------------------------------------------------
install.packages('Rserve')
library(Rserve)


#---------------------------------------------------------------------------------------------------------------------------
# KNN Regression model 

library(caret)
set.seed(300)
train <- sample(nrow(top1), nrow(top1) * 0.6)
vk <- seq(1, 11)
MSE <- vk
for (i in 1:length(vk)) {
  knn.fit <- knnreg(Value ~ ., data=LE.topyears, subset = train, k = vk[i])
  # knn.fit <- knnreg(Value ~ .-Indicator.Category - Source - Methods - Notes, data=top_years, subset = train, k = vk[i])
  MSE[i] <- mean((LE.topyears$Value - predict(knn.fit, LE.topyears))[-train]^2)
}
plot(vk, MSE, xlab = "k", ylab = "MSE", col = "blue", main = 'Big Cities Health - KNN Analysis', sub = 'Examining Elbow Method')

