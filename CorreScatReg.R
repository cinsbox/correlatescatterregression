# If .txt tab file, use this
# my_data <- read.delim(file.choose())
# Or, if .csv file, use this
# Data from https://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/frames/frame.html
# Use mlr11.csv
# Data is Using Technology: U.S. Economy Case Study
# 7 variables: X1 to X7
my_data <- read.csv(file.choose()) # use mlr11.csv 

# Print the first 5 rows
head(my_data, 5)

# Calculate overall summary
summary(my_data, digits = 1)

# Install pastecs package for descriptive or summary statistics
install.packages("pastecs")

# Load library
library(pastecs)

# Calculate descriptive statistics
descrip <- stat.desc(my_data)
round(descrip, 2)

# Install package
install.packages("ggpubr") # ggpubr contains easy functions for publication ready plots

# Laad library
library(ggpubr)

# Create density plot that shows distribution of numeric variable
ggdensity(my_data$CRUDE, 
          main = "Density Plot", # Title
          xlab = "CRUDE") # Variable on X-axis

ggdensity(my_data$INTEREST, 
          main = "Density Plot",
          xlab = "INTEREST")

ggdensity(my_data$FOREIGN, 
          main = "Density Plot",
          xlab = "FOREIGN")

ggdensity(my_data$DJIA, 
          main = "Density Plot",
          xlab = "DJIA")

ggdensity(my_data$GNP, 
          main = "Density Plot",
          xlab = "GNP")

ggdensity(my_data$PURCHASE, 
          main = "Density Plot",
          xlab = "PURCHASE")

ggdensity(my_data$CONSUMER, 
          main = "Density Plot",
          xlab = "CONSUMER")

# Create QQ plots to see if variables are normally distributed 
# Normal distributed data looks roughly like a straight line
# my_data is name of the data set 
# After "$" is the name of the variable
# main is title of the plot
# xlab is the x-axis label
ggqqplot(my_data$CRUDE, main="QQ Plot", xlab="CRUDE") 
ggqqplot(my_data$INTEREST, main="QQ Plot", xlab="INTEREST")
ggqqplot(my_data$FOREIGN, main="QQ Plot", xlab="FOREIGN")
ggqqplot(my_data$DJIA, main="QQ Plot", xlab="DJIA")
ggqqplot(my_data$GNP, main="QQ Plot", xlab="GNP")
ggqqplot(my_data$PURCHASE, main="QQ Plot", xlab="PURCHASE")
ggqqplot(my_data$CONSUMER, main="QQ Plot", xlab="CONSUMER")

# Normality test: Shapiro-Wilks test
# p-value > 0.05 implying that the distribution of the data 
# are not significantly different from normal distribution. 
# In other words, we can assume the normality.

# For single variable 
shapiro.test(my_data$CRUDE) # Single variable ok if few variables 
shapiro.test(my_data$INTEREST) 

# What if have multiple variables? 
# For all of the variables, use sapply to run the entire data frame
# sapply: take list, vector or data frame as input and gives output in vector or matrix
sapply(my_data, shapiro.test)

# Pearson's correlation: relationship between two variables
my_data_correl <- cor(my_data) # cor: correlation
round(my_data_correl, 2) # round data to 2 decimal points

# Install Package
install.packages("ggplot2") # ggplot2 is used for data visualization

# Import library
library(ggplot2)

# Basic scatter plot (ggplot)
# aes: aesthetic mapping
# Use CRUDE as X variable and INTEREST as Y variable
# geom_point (geom is for graphical representation) is used for scatter plot
ggplot(my_data, aes(x=CRUDE, y=INTEREST)) + geom_point()

# Change the point size and shape
# size refers to size of the marker 
# shape refers to the shape of the marker 
# http://www.sthda.com/english/wiki/ggplot2-point-shapes
ggplot(my_data, aes(x=CRUDE, y=INTEREST)) +
  geom_point(size=2, shape=19) # geom_point(size, color, shape) 

# Add the regression line
ggplot(my_data, aes(x=CRUDE, y=INTEREST)) + 
  geom_point()+
  geom_smooth(method=lm)

# Simple linear regression
# predict a quantitative outcome y on the basis of one single predictor
# variable x

# Install packages
install.packages("tidyverse") # 
install.packages("ggpubr") # if have not installed

# Load library
library(tidyverse) # Package for data analysis
library(ggpubr) # if haven not installed
theme_set(theme_pubr()) # Create publication ready theme 

# Regression computation
# INTEREST = y and CRUDE = x 
model <- lm(INTEREST ~ CRUDE, data = my_data)
model # # Simple regression: INTEREST = 0.253(CRUDE) + 4.526

# Simple regression model summary
summary(model)

# Multiple regression
# CONSUMER = y, FOREIGN = x1, DJIA = x2 and GNP = x3
model <- lm(CONSUMER ~ FOREIGN + DJIA + GNP, data = my_data)
model
summary(model)
# Multiple linear regression: 
# CONSUMER = -2(FOREIGN) + 0.155(DJIA) + 0.250(GNP) - 289
