
rm(list = ls())
#Importing Data

#install/load libraries
library(ggplot2)
library(GGally)
library(ggcorrplot)
library(tidyverse)
library(cowplot)
library(psych)
library(lattice)
library(xtable)
library(plyr)
library(dplyr)
library(gridExtra)
library(WVPlots)

life_expectancy_data <- read.csv("Life_Expectancy_Data.csv")
head(life_expectancy_data)


#Dimensions : Gives numbers of rows and columns
dim(life_expectancy_data)

# Structure of dataset
str(life_expectancy_data)

#statistical summary of the variables
summary(life_expectancy_data)

#Check for missing values
colSums(is.na(life_expectancy_data))

# Select numeric variables for calculating mean
life_expectancy_data_num <- select(life_expectancy_data,-c(1,2,3))

#Calculate means of all the numeric variables
colMeans(life_expectancy_data_num, na.rm = TRUE)

# Impute missing values in numeric variables with mean
for(i in 4:ncol(life_expectancy_data)) {
  life_expectancy_data[ , i][is.na(life_expectancy_data[ , i])] <- mean(life_expectancy_data[ , i], na.rm=TRUE)
}
summary(life_expectancy_data) 

# We can see that now the data set has no missing values
colSums(is.na(life_expectancy_data))

dim(life_expectancy_data)

#Plotting box plots of life expectancy to understand outliers
boxplot(life_expectancy_data$life_expectancy, xlab="Life Expectancy")

#removing outliers
outliers <- boxplot(life_expectancy_data$life_expectancy, plot=FALSE)$out

life_expectancy_data<- life_expectancy_data[-which(life_expectancy_data$life_expectancy %in% outliers),]

dim(life_expectancy_data)

#correlation between percentage expenditure and life expectancy
life_expectancy_vs_percentage_expenditure <-  ggplot(life_expectancy_data, aes(percentage_expenditure, life_expectancy)) + 
  geom_jitter(color = "red", alpha = 0.5) + theme_light()

life_expectancy_vs_Total_expenditure  <- ggplot(life_expectancy_data, aes(Total_expenditure, life_expectancy)) +
  geom_jitter(color = "Light blue", alpha = 0.5) + theme_light()

p <- plot_grid(life_expectancy_vs_percentage_expenditure, life_expectancy_vs_Total_expenditure) 
title <- ggdraw() + draw_label("Correlation between Health expenditure and life expectancy", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

#correlation between immunizations and life expectancy
life_expectancy_vs_Hepatitis_B <- ggplot(life_expectancy_data, aes(Hepatitis_B, life_expectancy)) + 
  geom_jitter(color = "yellow", alpha = 0.5) + theme_light()

life_expectancy_vs_Diphtheria  <- ggplot(life_expectancy_data, aes(Diphtheria, life_expectancy)) +
  geom_jitter(color = "violet", alpha = 0.5) + theme_light()

life_expectancy_vs_Polio  <- ggplot(life_expectancy_data, aes(Polio, life_expectancy)) + geom_jitter(color = "green", alpha = 0.5) + theme_grey()

p <- plot_grid(life_expectancy_vs_Hepatitis_B, life_expectancy_vs_Diphtheria, life_expectancy_vs_Polio ) 
title <- ggdraw() + draw_label("Correlation between Immunizations and life expectancy", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))


#correlation between measles and life expectancy
life_expectancy_vs_Measles  <- ggplot(life_expectancy_data, aes(Measles, life_expectancy)) +
  geom_jitter(color = "dark green", alpha = 0.5) + theme_light()

p <- plot_grid(life_expectancy_vs_Measles) 
title <- ggdraw() + draw_label("Correlation between Measles vs Life expectancy", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

#correlation between GDP and lif expectancy
life_expectancy_vs_GDP  <- ggplot(life_expectancy_data, aes(GDP, life_expectancy)) +
  geom_jitter(color = "dark green", alpha = 0.5) + theme_light()

p <- plot_grid(life_expectancy_vs_GDP) 
title <- ggdraw() + draw_label("Correlation between GDP vs Life expectancy", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

#correlation between BMI and life expectancy
life_expectancy_vs_BMI  <- ggplot(life_expectancy_data, aes(BMI, life_expectancy)) +
  geom_jitter(color = "dark green", alpha = 0.5) + theme_light()

p <- plot_grid(life_expectancy_vs_BMI) 
title <- ggdraw() + draw_label("Correlation between BMI vs Life expectancy", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))


#correlation between Alcohol and life expectancy
life_expectancy_vs_Alcohol  <- ggplot(life_expectancy_data, aes(Alcohol, life_expectancy)) +
  geom_jitter(color = "dark green", alpha = 0.5) + theme_light()

p <- plot_grid(life_expectancy_vs_Alcohol) 
title <- ggdraw() + draw_label("Correlation between Alcohol vs Life expectancy", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))


#correlation between Under 5 Deaths and life expectancy
life_expectancy_vs_under_five_deaths  <- ggplot(life_expectancy_data, aes(under_five_deaths , life_expectancy)) +
  geom_jitter(color = "dark green", alpha = 0.5) + theme_light()

p <- plot_grid(life_expectancy_vs_under_five_deaths) 
title <- ggdraw() + draw_label("Correlation between Under 5 Deaths vs Life expectancy", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))


#correlation between thinness and life expectancy
life_expectancy_vs_thinness_1_19_years  <- ggplot(life_expectancy_data, aes(thinness_1_19_years, life_expectancy)) +
  geom_jitter(color = "green", alpha = 0.5) + theme_light()

life_expectancy_vs_thinness_5_9_years  <- ggplot(life_expectancy_data, aes(thinness_5_9_years, life_expectancy)) +
  geom_jitter(color = "dark green", alpha = 0.5) + theme_light()

p <- plot_grid(life_expectancy_vs_thinness_1_19_years, life_expectancy_vs_thinness_5_9_years) 
title <- ggdraw() + draw_label("Correlation between Thinness vs Life expectancy", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))


#correlation between Income Composition of Resources and life expectancy
life_expectancy_vs_Income_composition_of_resources  <- ggplot(life_expectancy_data, aes(Income_composition_of_resources , life_expectancy)) +
  geom_jitter(color = "dark green", alpha = 0.5) + theme_light()

p <- plot_grid(life_expectancy_vs_Income_composition_of_resources) 
title <- ggdraw() + draw_label("Correlation between Income Composition of Resources vs Life expectancy", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

##

ggcorr(life_expectancy_data, label = T, label_size = 2, label_round =2)


#we will split the data into train and test for model building
split_train <- round(0.8 * nrow(life_expectancy_data))
train_indices <- sample(1:nrow(life_expectancy_data), split_train)
train_data <- life_expectancy_data[train_indices, ]
test_data <- life_expectancy_data[-train_indices, ]



#first model 
regformula1 <- as.formula("life_expectancy ~ Alcohol + percentage_expenditure + Hepatitis_B + Measles +  BMI + under_five_deaths + Polio + Total_expenditure + Diphtheria  + thinness_1_19_years + thinness_5_9_years + GDP + Income_composition_of_resources")
model1 <- lm(regformula1, data = train_data)
summary(model1)


r_sq1 <- summary(model1)$r.squared
prediction1 <- predict(model1, newdata = test_data)
residuals1 <- test_data$Life.expectancy - prediction1
rmse1 <- sqrt(mean(residuals1^2, na.rm=TRUE))

summary(model1)$coefficient

#second model
#dropping insignificant variables like measles, percentage.expenditure, Hepatitis.B, Under.five.deaths, Thinness.5.9.years
regformula2 <- as.formula("life_expectancy ~  Alcohol +  Diphtheria  +  BMI +  Polio + Total_expenditure + thinness_1_19_years +  Income_composition_of_resources")
model2 <- lm(regformula2, data = train_data)
summary(model2)

r_sq2 <- summary(model2)$r.squared
prediction2 <- predict(model2, newdata = test_data)
residuals2 <- test_data$Life.expectancy - prediction2
rmse2 <- sqrt(mean(residuals2^2, na.rm=TRUE))

print(paste0("R-squared for first model:", round(r_sq1, 4)))

print(paste0("R-squared for second model: ", round(r_sq2, 4)))

print(paste0("RMSE for first model: ", round(rmse1, 2)))

print(paste0("RMSE for second model: ", round(rmse2, 2)))

confint(model2, level=0.95)

#prediction
test_data$prediction <- predict(model2, newdata = test_data)
ggplot(test_data, aes(x = prediction, y = life_expectancy)) + 
  geom_point(color = "dark green", alpha = 0.7) + 
  geom_abline(color = "yellow") +
  ggtitle("Prediction vs. Real values")

#residuals vs linear model prediction
test_data$residuals <- test_data$life_expectancy - test_data$prediction
ggplot(data = test_data, aes(x = prediction, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals), color = "dark green", alpha = 0.7) + geom_hline(yintercept = 0, linetype = 4, color = "red") +
  ggtitle("Residuals vs. Linear model prediction")

#histogram for residuals
ggplot(test_data, aes(x = residuals)) + 
  geom_histogram(bins = 15, fill = "light blue") +
  ggtitle("Histogram of residuals")

GainCurvePlot(test_data, "prediction", "life_expectancy", "Model2")
sigma(model2)/mean(life_expectancy_data$life_expectancy)


#test1
Kumar <- data.frame(  Country = "India",
                     Alcohol = 5.28,
                     Diphtheria = 86,
                     BMI = 38.9,
                     Polio = 98,
                     Total_expenditure = 11.14,
                     thinness_1_19_years = 2.1,
                     Income_composition_of_resources = 0.741)
print(paste0("Life expectancy for Kumar: ", round(predict(model2, Kumar), 2)))
