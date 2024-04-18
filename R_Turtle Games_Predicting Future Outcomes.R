## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

# Assignment 3
# Student: Aleksandra Witek

###############################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use..

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library(tidyverse)

# Import the data set.
sales <- read.csv('turtle_sales.csv', header=T)
# sales <- read.csv(file.choose(), header=T)


# Print the data frame.
print(sales)
View(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales2 <- sales[c('Product', 'Platform', 
                  'NA_Sales', 'EU_Sales', 
                  'Global_Sales')]

# View the data frame.
sales2

# View the descriptive statistics.
summary(sales2)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
library(gridExtra)

# Sales by Product

chart1 <- ggplot(sales2, aes(Product,Global_Sales, color = Product)) + 
  geom_point() + 
  geom_smooth() + 
  geom_jitter() +
  labs(title = "Global Sales by Product") +
  theme(plot.title = element_text(size = 10),
        legend.position = "none") 

chart2 <- ggplot(sales2, aes(Product, EU_Sales, color = Product)) + 
  geom_point() + 
  geom_smooth() + 
  geom_jitter() +
  labs(title = "European Union Sales by Product") +
  theme(plot.title = element_text(size = 10),
        legend.position = "none") 

chart3 <- ggplot(sales2, aes(Product, NA_Sales, color = Product))+ 
  geom_point() + 
  geom_smooth() + 
  geom_jitter() +
  labs(title = "North America Sales by Product") +
  theme(plot.title = element_text(size = 10),
        legend.position = "none") 

grid.arrange(chart1, chart2, chart3, nrow = 3)

# Sales by Platform

chart4 <- ggplot(sales2, aes(Global_Sales, Platform, color = Platform)) + 
  geom_point() + 
  geom_jitter() + 
  labs(title = "Global Sales by Platform") +
  theme(plot.title = element_text(size = 10),
        legend.position = "none")

chart5 <- ggplot(sales2, aes(EU_Sales, Platform, color = Platform)) + 
  geom_point() + 
  geom_jitter() + 
  labs(title = "European Union Sales by Platform") +
  theme(plot.title = element_text(size = 10),
        legend.position = "none")

chart6 <- ggplot(sales2, aes(NA_Sales, Platform, color = Platform)) + 
  geom_point() + 
  geom_jitter() + 
  labs(title = "North America Sales by Platform") +
  theme(plot.title = element_text(size = 10),
        legend.position = "none")

grid.arrange(chart4, chart5, chart6, ncol = 3)


## 2b) Boxplots
# Create boxplots.

chart7 <- ggplot(sales2, aes(Global_Sales)) + 
  geom_boxplot(colour=I('darkred')) + 
  labs(title = "Global Sales Boxplot") +
  theme(plot.title = element_text(size = 10))

chart8 <- ggplot(sales2, aes(EU_Sales)) + 
  geom_boxplot(colour=I('blue')) + 
  labs(title = "European Union Sales Boxplot") +
  theme(plot.title = element_text(size = 10))

chart9 <- ggplot(sales2, aes(NA_Sales)) + 
  geom_boxplot(colour=I('darkgreen')) + 
  labs(title = "North America Sales Boxplot")+
  theme(plot.title = element_text(size = 10))

grid.arrange(chart7, chart8, chart9, nrow = 3)

# Remove outliers that you can see on the boxplots

# Global Sales
gs <- sales$Global_Sales
Q1_gs <- quantile(gs, 0.25)
Q3_gs <- quantile(gs, 0.75)
IQR_gs <- Q3_gs - Q1_gs
filtered_gs <- subset(gs, gs >= Q1_gs - 1.5*IQR_gs & gs <= Q3_gs + 1.5*IQR_gs)
filtered_gs <- data.frame(filtered_gs)
filtered_gs

# EU Sales
eu <- sales$EU_Sales
Q1_eu <- quantile(eu, 0.25)
Q3_eu <- quantile(eu, 0.75)
IQR_eu <- Q3_eu - Q1_eu
filtered_eu <- subset(eu, eu >= Q1_eu - 1.5*IQR_eu & eu <= Q3_eu + 1.5*IQR_eu)
filtered_eu <- data.frame(filtered_eu)
filtered_eu

# NA Sales
na <- sales$NA_Sales
Q1_na <- quantile(na, 0.25)
Q3_na <- quantile(na, 0.75)
IQR_na <- Q3_na - Q1_na
filtered_na <- subset(na, na >= Q1_na - 1.5*IQR_na & na <= Q3_na + 1.5*IQR_na)
filtered_na <- data.frame(filtered_na)
filtered_na

chart10 <- ggplot(filtered_gs, aes(filtered_gs)) + 
  geom_boxplot(colour=I('darkred')) + 
  labs(title = "Global Sales Boxplot") +
  theme(plot.title = element_text(size = 10))

chart11 <- ggplot(filtered_eu, aes(filtered_eu)) + 
  geom_boxplot(colour=I('blue')) + 
  labs(title = "EU Boxplot") +
  theme(plot.title = element_text(size = 10))

chart12 <- ggplot(filtered_na, aes(filtered_na)) + 
  geom_boxplot(colour=I('darkgreen')) + 
  labs(title = "NA Boxplot") +
  theme(plot.title = element_text(size = 10))

grid.arrange(chart10, chart11, chart12, nrow = 3)


## 2c) Histograms
# Create histograms.

chart13 <- ggplot(sales2, aes(Global_Sales)) + 
  geom_histogram(bins=20, fill='darkred') +
  labs(title = "Histogram of Global Sales", 
       x = "X values", y = "Frequency") +
  theme(plot.title = element_text(size = 10))

chart14 <- ggplot(sales2, aes(EU_Sales)) + 
  geom_histogram(bins=20, fill='blue') +
  labs(title = "Histogram of European Union Sales", 
       x = "X values", y = "Frequency") +
  theme(plot.title = element_text(size = 10))

chart15 <- ggplot(sales2, aes(NA_Sales)) + 
  geom_histogram(bins=20, fill='darkgreen') +
  labs(title = "Histogram of North America Sales", 
       x = "X values", y = "Frequency") +
  theme(plot.title = element_text(size = 10))

grid.arrange(chart13, chart14, chart15, nrow = 3)

# For the purpose of comment
length(unique(sales$Product)) #175
length(unique(sales$Platform)) #22

## 2d) Sales by Platform

sales_by_product <- sales %>%
  group_by(Product, Platform, Genre) %>%
  summarise(Sum_NA_Sales = sum(NA_Sales),
            Sum_EU_Sales = sum(EU_Sales),
            Sum_Global_Sales = sum(Global_Sales))

head(sales_by_product)

# Explore the data frame.
dim(sales_by_product)

# Create bar plot for global sales per platform.
ggplot(sales_by_product, aes(x = Platform, y = Sum_Global_Sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Global Sales by Platform",
       x = "Platform",
       y = "Global Sales") +
  theme_minimal()


# Create bar plot for global sales per genre.
ggplot(sales_by_product, aes(x = Genre, y = Sum_Global_Sales)) +
  geom_bar(stat = "identity", fill = "seagreen3") +
  labs(title = "Global Sales by Genre",
       x = "Genre",
       y = "Global Sales") +
  theme_minimal()


# Create a box plot with Platform and Sum_Global_Sales
ggplot(sales_by_product, aes(x = Platform, y = Sum_Global_Sales)) +
  geom_boxplot(fill = "springgreen3", alpha = 0.8) +
  labs(title = "Total Global Sales by Platform",
       x = "Platform", y = "Total Global Sales (in millions)") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.position = "none")



# Create barplot with Global_Sales by genre and platforms
ggplot(sales_by_product, aes(x = Genre, y = Sum_Global_Sales, fill = Platform)) +
  geom_col() +
  labs(title = "Global Sales by Genre and Platform",
       x = "Genre",
       y = "Global Sales",
       fill = "Platform") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))



###############################################################################

# 3. Observations and insights

# Turtle games offers a wide variety of games (175) on a number of consoles (22).
# The global sales consist mostly of sales in European Union and North America.
# Most of the games are sold in lower quantities, meaning the company produces
# a high number of games but only a few gain an extreme popularity.
# The average sales globally are higher than in EU or NA. This means that other
# location on average buy a higher number of games.
# The most popular games are the one with a low number of unique code. I would
# like to further explore if nomenclature of product unique code is based on
# any rule.




###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
View(sales)

# Calculate mix, max, mean.
# Global Sales
min_GS <- sales %>%
  summarise(min_GS = min(Global_Sales))

mean_GS <- sales %>%
  summarise(mean_GS = mean(Global_Sales))

max_GS <- sales %>%
  summarise(max_GS = max(Global_Sales))

min_GS
mean_GS
max_GS

# EU Sales
min_EU <- sales %>%
  summarise(min_EU = min(EU_Sales))

mean_EU <- sales %>%
  summarise(mean_EU = mean(EU_Sales))

max_EU <- sales %>%
  summarise(max_EU = max(EU_Sales))

min_EU
mean_EU
max_EU

# NA Sales
min_NA <- sales %>%
  summarise(min_NA = min(NA_Sales))

mean_NA <- sales %>%
  summarise(mean_NA = mean(NA_Sales))

max_NA <- sales %>%
  summarise(max_NA = max(NA_Sales))

min_NA
mean_NA
max_NA

all <- data.frame(min = c(min_GS, min_EU, min_NA),
                 mean = c(mean_GS, mean_EU, mean_NA),
                 max = c(max_GS, max_EU, max_NA))

all

# View the descriptive statistics.
summary(sales)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sales_GS <- aggregate(Global_Sales~Product, sales, sum)
sales_EU <- aggregate(EU_Sales~Product, sales, sum)
sales_NA <- aggregate(NA_Sales~Product, sales, sum)

# View the data frame.
View(sales_GS)
View(sales_EU)
View(sales_NA)

# Explore the data frame.
summary(sales_GS)
summary(sales_EU)
summary(sales_NA)

dim(sales_GS)
dim(sales_EU)
dim(sales_NA)


###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(sales$Global_Sales)
qqline(sales$Global_Sales, col='red', lwd=2)

qqnorm(sales$EU_Sales)
qqline(sales$EU_Sales, col='red', lwd=2)

qqnorm(sales$NA_Sales)
qqline(sales$NA_Sales, col='red', lwd=2)


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test((sales$Global_Sales))
# Our p-value is <0.05,so the data is not normally distributed.

shapiro.test((sales$EU_Sales))
# Our p-value is <0.05,so the data is not normally distributed.

shapiro.test((sales$NA_Sales))
# Our p-value is <0.05,so the data is not normally distributed.


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness((sales$Global_Sales))
skewness((sales$EU_Sales))
skewness((sales$NA_Sales))
# The output suggest a positive skewness.

kurtosis((sales$Global_Sales))
kurtosis((sales$EU_Sales))
kurtosis((sales$NA_Sales))
# Kurtosis is over 3 for all the analysed data.


## 3d) Determine correlation
# Determine correlation.
cor(sales$Global_Sales, sales$EU_Sales)
cor(sales$Global_Sales, sales$NA_Sales)
cor(sales$EU_Sales, sales$NA_Sales)
# The correlation is highly positive between all the variables.

###############################################################################

# 4. Observations and insights
# Based on the Shapiro-Wilk test, the data is normally distributed.
# The data is positively skewed (right skewness).
# Kurtosis is well over 3 which shows that the data has heavier tails than 
# a normal distribution.
# All the sales data is highly positively correlated with each other.



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.



###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
View(sales)

# Determine a summary of the data frame.
summary(sales)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
cor(sales$EU_Sales, sales$NA_Sales)

model1 <- lm(EU_Sales~NA_Sales,
             data=sales)

model1

summary(model1)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(sales$EU_Sales, sales$NA_Sales)
coefficients(model1)
abline(coefficients(model1), col = 'red')

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
sales_num <- sales[c('Global_Sales', 'EU_Sales', 'NA_Sales')]

cor(sales_num)

library(psych)
corPlot(sales_num, cex=2)

# Multiple linear regression model.

model2 = lm(Global_Sales~EU_Sales+NA_Sales, data=sales_num)
summary(model2)

###############################################################################

# 4. Predictions based on given values

# y = 0.22175 + 1.34197 * EU_Sales + 1.15543 * NA_Sales

##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
y_pred1 = 0.22175 + 1.34197 * 23.80 + 1.15543 * 34.02
y_pred1

##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
y_pred2 = 0.22175 + 1.34197 * 1.56 + 1.15543 * 3.93
y_pred2

##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
y_pred3 = 0.22175 + 1.34197 * 0.65 + 1.15543 * 2.73
y_pred3

##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
y_pred4 = 0.22175 + 1.34197 * 0.97 + 1.15543 * 2.26
y_pred4

##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52
y_pred5 = 0.22175 + 1.34197 * 0.52 + 1.15543 * 22.08
y_pred5

# Compare with observed values for a number of records.
y1 <- subset(sales_num, NA_Sales == 34.02 
             & EU_Sales == 23.8)
y1

y2 <- subset(sales_num, NA_Sales == 3.93 
             & EU_Sales == 1.56) 
y2

y3 <- subset(sales_num, NA_Sales == 2.73 
             & EU_Sales == 0.65) 
y3

y4 <- subset(sales_num, NA_Sales == 2.26 
             & EU_Sales == 0.97) 
y4

y5 <- subset(sales_num, NA_Sales == 22.08 
             & EU_Sales == 0.52) 
y5

# Case 1
comp1 <- data.frame(Observed_values=y1$Global_Sales, 
                    Predicted_values=y_pred1)
print(comp1)

# Case 2
comp2 <- data.frame(Observed_values=y2$Global_Sales, 
                    Predicted_values=y_pred2)
print(comp2)

# Case 3
comp3 <- data.frame(Observed_values=y3$Global_Sales, 
                    Predicted_values=y_pred3)
print(comp3)

# Case 4
comp4 <- data.frame(Observed_values=y4$Global_Sales, 
                    Predicted_values=y_pred4)
print(comp4)

# Case 5
comp5 <- data.frame(Observed_values=y5$Global_Sales, 
                    Predicted_values=y_pred5)
print(comp5)

###############################################################################

# 5. Observations and insights
# Simple linear regression model studying the relationship between EU and NA 
# sales has a R-squared of 50%. Half of the variables are explained by the model.
# All coefficients are significant. 

# Multiple linear regression model explains globel sales based on eu and na sales.
# It has a R-squared of 97%. This model is much better than simple linear 
# regression applied for eu and na sales. It explains most of the variables 
# in the data set. All coefficients are significant.
# P-value in f-statistic is lower than 0.05. 


###############################################################################
###############################################################################