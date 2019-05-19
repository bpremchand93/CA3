# Using statistical methods to examine
# the relationships between variables (i.e. Rent and Income data of dublin and donegal)
income_data <- read.csv("Income.csv")
rent_data <- read.csv("Rent.csv")

# Modifying the variable name and selecting the appropriate fields for testing
income_data$Year <- income_data$Year.
income_data <- subset(income_data, select = c(1,4,3))
income_rent_data <- merge(income_data, rent_data)

str(income_rent_data)
head(income_rent_data)
# The income and rent dataset contains data of average rent and income for every quarter for
# different counties like dublin, donegal and galway.
# Used the transform function to change the county to a factor
transformed_income_rent_data <- transform(income_rent_data,County = factor(County, labels = c("Donegal", "Dublin","Galway")))

# Removed the ',' in the income and rent data
transformed_income_rent_data$Income <- gsub(',','',transformed_1$Income)
transformed_income_rent_data$Rent <- gsub(',','',transformed_1$Rent)
head(transformed_income_rent_data)

# Selecting the appropriate test
# and to check whether the data is normally distributed or not below test has been done

library("lattice")
histogram(~Rent | County, data = transformed_income_rent_data)
histogram(~Income | County, data = transformed_income_rent_data)


# Using a QQ plot to check for normality
# qqnorm function used to plots the samples
# against a normal distribution

# Quantile-quantile plot used to confirm whethere the data 
# is distributed normally and to compare rent and income for each county

 
# Adding normality line 
# to the plot and to evaluate normality
# for income of different counties

with(transformed_income_rent_data, {
  qqnorm(Income)
  qqline(Income)
  main = "Income of different counties"
})


# Adding normality line 
# to the plot to evaluate normality
# for rent of different counties

with(transformed_income_rent_data, {
  qqnorm(Rent)
  qqline(Rent)
  main = "Rent of different counties"
})


# Formal test of normality
# provided through widely used Shapiro-Wilks test
normality_test <- shapiro.test(transformed_income_rent_data$Income)
normality_test$p.value

normality_test <- shapiro.test(transformed_income_rent_data$Rent)
normality_test$p.value

# Finding the relationship between income and rent of counties with the help of spearman test 
# Pearson correlation coefficient test does not require normally distributed data. 

corr <- cor.test(x=transformed_income_rent_data$Rent, y=transformed_income_rent_data$Income, method = "spearman",
                 exact = T)
corr$p.value


# Selecting the appropriate test
# We need to check whether the data is normally distributed or not

# Power analysis allows to determine the sample size required to detect an effect of a given size
# With a given degree of confidence.

# pwr package implements power analysis  
library(pwr)
library(dplyr)

# Cohen describes the effective size as "the degree to which the null hypothesis is false."
# Cohen value suggests that r value of 0.1, 0.3, 0.5 represents small, 
# medium and large effect size respectively.
effective_size <- cohen.ES(test = "r", size = "large")
effective_size

# To examine the relation between the residential property and average income and rent of Ireland
# For correlation coefficient used pwr.r.test where r is the correlation.
# Sample size refers to number of observations in each condition/group of the experiment design
sample_size <- pwr.r.test(r = effective_size$effect.size, sig.level = 0.05, power = 0.80, alternative = "two.sided")
sample_size

plot(sample_size)

sample_data <- sample_n(transformed_income_rent_data, 29)
head(sample_data)
nrow(sample_data)

cor_1 <- cor.test(sample_data$Rent, sample_data$Income, method = 'spearman')
cor_1


# cor.test(sample_data$Rent, sample_data$Income)