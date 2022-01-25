#Author: Suryateja Chalapati

#Importing Libraries
library(readxl)
library(car)
library(lmtest)
library(stargazer)

#Setting the Working Directory and Importing the Dataset
setwd("C:/Users/surya/Downloads")

bms <- read_excel("BigMartSales.xlsx", sheet = 'Data')
names(bms) <- tolower(colnames(bms))

#NA Values Column-Wise
sapply(bms, function(x) sum(is.na(x)))
str(bms)

#Data Visualizations
hist(bms$item_sales, col = 'lightcoral', main = "Histogram of Item Sales", xlab = 'Item_Sales', ylab = 'Frequency')
hist(log(bms$item_sales), col = 'lightgreen', main = "Histogram of log(Item Sales)", xlab = 'log(Item_Sales)', ylab = 'Frequency')
hist(bms$item_visibility)
hist(log(bms$item_visibility))
hist(bms$item_mrp)
hist(log(bms$item_mrp))

library(lattice)
histogram(~item_sales, data = bms)
densityplot(~item_sales, data = bms)
densityplot(~item_sales | outlet_type, data = bms)
densityplot(~item_sales | city_type, data = bms)

bwplot(item_sales ~ outlet_type, data = bms)
bwplot(item_sales ~ city_type, data = bms)
bwplot(item_sales ~ city_type | outlet_type, data = bms)

xyplot(item_sales ~ item_visibility | outlet_type, data = bms)
xyplot(item_sales ~ item_visibility | city_type, data = bms)
xyplot(item_sales ~ item_visibility | outlet_type*city_type, data = bms)

#Feature Engineering/Pre-processing
bms$item_sales <- round(bms$item_sales)
bms$item_fat_content <- tolower(bms$item_fat_content)
bms$item_fat_content <- as.factor(bms$item_fat_content)
bms$item_type <- as.factor(bms$item_type)
bms$outlet_type <- as.factor(bms$outlet_type)
bms$outlet_id <- as.factor(bms$outlet_id)
bms$city_type <- as.factor(bms$city_type)
bms$outlet_age <- 2013 - bms$outlet_year
bms$outlet_year <- NULL
bms$outlet_size <- NULL
bms$item_id <- NULL
str(bms)

#Checking for Correlations
temp <- data.frame(bms$item_sales)
temp$item_weight <- bms$item_weight
temp$item_visibility <- bms$item_visibility
temp$item_mrp <- bms$item_mrp
temp <- temp[complete.cases(temp), ]
corpl <- cor(cbind(temp))

library(corrplot)
corrplot(corpl, method = "number", number.cex = 0.7)

#Multi-Level Regression Analysis
library(lme4)
library(arm)

m1 <- lmer(item_sales ~ city_type + outlet_type + (1 | outlet_id), data = bms)

m1.a <- lmer(item_sales ~ outlet_type + (1 | outlet_id), data = bms)

m1.b <- lmer(item_sales ~ city_type + (1 | outlet_id), data = bms)

m2 <- lmer(item_sales ~ item_type + item_visibility + item_mrp + outlet_type + 
             outlet_age + (1 | outlet_id), data = bms)

m3 <- lmer(item_sales ~ item_type + item_visibility + item_mrp + outlet_type + 
             outlet_age + (1 | outlet_type/outlet_id), data = bms)

#Final Models [3]
m4 <- lmer(item_sales ~ item_type + item_visibility + item_mrp + city_type + 
             outlet_type + outlet_age + (1 | city_type/outlet_id), data = bms)

#m5 <- lmer(item_sales ~ item_fat_content + item_visibility + item_mrp + city_type + outlet_type + outlet_age + (1 | city_type/outlet_id), data = bms)

m5 <- lmer(log(item_sales) ~ item_type + item_fat_content + item_visibility + item_mrp + city_type + 
             outlet_type + outlet_age + (1 | city_type/outlet_id), data = bms)

m6 <- lmer(item_sales ~ item_type + item_fat_content + item_visibility + item_mrp + city_type + 
             outlet_type + outlet_age + (1 | city_type/outlet_id), data = bms)
confint(m6)
fixef(m6)
ranef(m6)
coef(m6)

stargazer(m1, m1.a, m1.b, type='text', single.row = TRUE)
stargazer(m2, m3, m4, m5, type='text', single.row = TRUE)

#Final Model Comparisons
stargazer(m4, m5, m6, type='text', single.row = TRUE)

#Assumptions
#Multicollinearity
vif(m6)

#Autocorrelation (Independence)
#Durbin-Watson Test
dwtest(m6)
