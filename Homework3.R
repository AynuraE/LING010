#== Homework 3 =====================================
# LING10 Statistics for Linguistics
# Dartmouth College. March 1, 2026
# Aynura Erejepbaeva

#load data
library(ggplot2)
library(lmtest)
library(dplyr)
library(lsmeans)
library(car)
library(asbio)
library(vcd)
library(ggmosaic)

#=============================================.
# Part 1. Exercise 1: Warlpiri Ergatives                   ====

#For each of the chi-squared exercises, you need to provide:
#(a) A one sentence summary of whether the data set meets the assumptions of chi-squared or not.
#(b) A chart (using ggplot) of the relationship between the two variables.
#(c) A report of the results, written in formal language. You can use the examples studied in class.
#(d) An explanation of the results in one or two sentences, without using any statistical words or numbers,
#as if you were explaining them to someone who knows nothing about statistics.
#=============================================.

#load file
warlpiri <- read.csv(file = "warlpiri.csv", header = TRUE, sep = ",")

# Display the loaded dataframe and
# convert it into a matrix of counts. 
tab <- table(warlpiri$AgeGroup, warlpiri$CaseMarking)
tab

# Get the proportion table 
prop.table(tab,1)

#ggplot
ggplot(warlpiri, aes(x = AgeGroup, fill = CaseMarking)) +
  geom_bar(position = "dodge") +
  labs(title = "Ergative Case Marking by Age Group",
       x = "Age Group",
       y = "Count",
       fill = "Case Marking")

#Chi-squared test
chi_res <- chisq.test(tab)
chisq.test(tab)

# Expected values
# Are any of the expected values less than 5?
# If so, then we need the Fisher Exact Test.
# If none of the values is less than 5, we can continue.
chisq.test(tab)$expected 

# Effect size
assocstats(tab)

#=============================================.
# Part 1. Exercise 2: Panamanian -s.                   ====

#For each of the chi-squared exercises, you need to provide:
#(a) A one sentence summary of whether the data set meets the assumptions of chi-squared or not.
#(b) A chart (using ggplot) of the relationship between the two variables.
#(c) A report of the results, written in formal language. You can use the examples studied in class.
#(d) An explanation of the results in one or two sentences, without using any statistical words or numbers,
#as if you were explaining them to someone who knows nothing about statistics.
#=============================================.

#load file
panama <- read.csv(file = "panama.csv", header = TRUE, sep = ",")

# Convert to factors
panama$deletion <- as.factor(panama$deletion)
panama$class <- as.factor(panama$class)

# Display the loaded dataframe and
# convert it into a matrix of counts. 
tab_panama <- table(panama$class, panama$deletion)
tab_panama

# Get the proportion table 
prop.table(tab_panama,1)

# Bar chart of deletion by social class
ggplot(panama, aes(x = class, fill = deletion)) +
  geom_bar(position = "dodge") +
  labs(title = "/s/ Deletion by Social Class in Panama",
       x = "Social Class",
       y = "Count of Words",
       fill = "Deletion (1 = yes, 0 = no)")

# Chi-squared test
chi_panama <- chisq.test(tab_panama)
chi_panama

# Expected values
# Are any of the expected values less than 5?
# If so, then we need the Fisher Exact Test.
# If none of the values is less than 5, we can continue.
chi_panama$expected

# Effect size
assocstats(tab_panama)

#=============================================.
# Part 1. Exercise 3: Tapping in North America.                   ====

#For each of the chi-squared exercises, you need to provide:
#(a) A one sentence summary of whether the data set meets the assumptions of chi-squared or not.
#(b) A chart (using ggplot) of the relationship between the two variables.
#(c) A report of the results, written in formal language. You can use the examples studied in class.
#(d) An explanation of the results in one or two sentences, without using any statistical words or numbers,
#as if you were explaining them to someone who knows nothing about statistics.
#=============================================.

#load file
englishTap <- read.csv(file = "englishTap.csv", header = TRUE, sep = ",")

#Convert to factors
englishTap$speechrate <- as.factor(englishTap$speechrate)
englishTap$tapped <- as.factor(englishTap$tapped)

# Display the loaded dataframe and
# convert it into a matrix of counts. 
tab <- table(englishTap$speechrate, englishTap$tapped)
tab

# Get the proportion table 
prop.table(tab,1)

#ggplot
ggplot(englishTap, aes(x = speechrate, fill = tapped)) +
  geom_bar(position = "dodge") +
  labs(title = "Tapping by Speech Rate",
       x = "Speech Rate",
       y = "Count",
       fill = "Tapped")

#Chi-squared
chisq.test(tab)

# Expected values
# Are any of the expected values less than 5?
# If so, then we need the Fisher Exact Test.
# If none of the values is less than 5, we can continue.
chisq.test(tab)$expected

# Effect size
assocstats(tab)

#=============================================.
# Part 2. Exercise 4: Icelandic Aspiration and Vowel Length.                   ====

#For each of the linear regression exercises, you need to provide:
#(a) At least one chart that describes the relationship between the variables.
#(b) A short discussion on whether the data meets the assumptions of a linear model.
#(c) A formal report of the linear model (written as if you were submitting it to a conference or on a scientific paper).
#(d) The equation that predicts the y-variable, including the coefficients, the intercept and the error.
#(e) An explanation of the results in one or two sentences, without using any statistical words or numbers,
#as if you were explaining them to someone who knows nothing about statistics.
#=============================================.

#Read file
icelandic <- read.csv(file = "icelandic-200.csv", header = TRUE, sep = ",")

icelandic$cons1 <- as.factor(icelandic$cons1)

#scatterplot
ggplot(icelandic, aes(x = cons1, y = vowel.dur)) +
  geom_boxplot() +
  labs(title = "Vowel Duration by Consonant Aspiration",
       x = "Consonant Type (cons1)",
       y = "Vowel Duration")

#linear regression
model <- lm(vowel.dur ~ cons1, data = icelandic)
summary(model)
model

# R-squared
summary(model)$r.squared
summary(model)$adj.r.squared
sqrt(summary(model)$r.squared)

# Shapiro-Wilk Test of normality
# If the test is significant, then the distribution is
# significantly DIFFERENT from a normal distribution
shapiro.test(model$residuals)

# Assumption: Variance
par(mfrow=c(1,2))
plot(model$res~model$fit,xlab="Fitted value",ylab="Residuals")
abline(h=0,col="gray")
qqnorm(model$res,main="")
qqline(model$res)
par(mfrow=c(1,1))

# Assumption: Levene's test for homoscedasticity (similar variances)
ncvTest(model)

# Assumption: Autocorrelation
durbinWatsonTest(model)

#=============================================.
# Part 2. Exercise 5: Tongue positions that are difficult to measure.                   ====

#For each of the linear regression exercises, you need to provide:
#(a) At least one chart that describes the relationship between the variables.
#(b) A short discussion on whether the data meets the assumptions of a linear model.
#(c) A formal report of the linear model (written as if you were submitting it to a conference or on a scientific paper).
#(d) The equation that predicts the y-variable, including the coefficients, the intercept and the error.
#(e) An explanation of the results in one or two sentences, without using any statistical words or numbers,
#as if you were explaining them to someone who knows nothing about statistics.
#=============================================.

#Read file
chaindata <- read.csv(file = "chaindata.csv", header = TRUE, sep = ",")

#scatterplot
ggplot(chaindata, aes(x = y2, y = y15)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between y2 and y15",
       x = "y2 (Front pellet position)",
       y = "y15 (Back pellet position)")

#linear regression
model <- lm(y15 ~ y2 + y6 + y5 + x6 + x5, data = chaindata)
summary(model)
model

# R-squared
summary(model)$r.squared
summary(model)$adj.r.squared
sqrt(summary(model)$r.squared)

# Shapiro-Wilk Test of normality
# If the test is significant, then the distribution is
# significantly DIFFERENT from a normal distribution
shapiro.test(model$residuals)

# Assumption: Variance
par(mfrow=c(1,2))
plot(model$res~model$fit,xlab="Fitted value",ylab="Residuals")
abline(h=0,col="gray")
qqnorm(model$res,main="")
qqline(model$res)
par(mfrow=c(1,1))

# Assumption: Levene's test for homoscedasticity (similar variances)
ncvTest(model)

# Assumption: Autocorrelation
durbinWatsonTest(model)

summary(model)

#=============================================.
# Part 2. Exercise 6: Frequency of Dutch words.                   ====

#For each of the linear regression exercises, you need to provide:
#(a) At least one chart that describes the relationship between the variables.
#(b) A short discussion on whether the data meets the assumptions of a linear model.
#(c) A formal report of the linear model (written as if you were submitting it to a conference or on a scientific paper).
#(d) The equation that predicts the y-variable, including the coefficients, the intercept and the error.
#(e) An explanation of the results in one or two sentences, without using any statistical words or numbers,
#as if you were explaining them to someone who knows nothing about statistics.
#=============================================.

#Read file
dutchmult <- read.csv(file = "dutch-mult-200.csv", header = TRUE, sep = ",")

dutchmult$Auxiliary <- as.factor(dutchmult$Auxiliary)
dutchmult$Regularity <- as.factor(dutchmult$Regularity)

#scatterplot
ggplot(dutchmult, aes(x = FamilySize, y = WrittenFrequency)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Written Frequency vs Family Size",
       x = "Family Size",
       y = "Written Frequency")

#linear regression
model <- lm(WrittenFrequency ~ FamilySize + LengthInLetters + Auxiliary + Regularity,
            data = dutchmult)

summary(model)
model

# R-squared
summary(model)$r.squared
summary(model)$adj.r.squared
sqrt(summary(model)$r.squared)

# Shapiro-Wilk Test of normality
# If the test is significant, then the distribution is
# significantly DIFFERENT from a normal distribution
shapiro.test(model$residuals)

# Assumption: Variance
par(mfrow=c(1,2))
plot(model$res~model$fit,xlab="Fitted value",ylab="Residuals")
abline(h=0,col="gray")
qqnorm(model$res,main="")
qqline(model$res)
par(mfrow=c(1,1))

# Assumption: Levene's test for homoscedasticity (similar variances)
ncvTest(model)

# Assumption: Autocorrelation
durbinWatsonTest(model)

summary(model)

