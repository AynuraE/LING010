#== Project: Initial Statistical Model =====================================
# LING10 Statistics for Linguistics
# Dartmouth College. March 17, 2026
# Name: Aynura Erejepbaeva
# Email: aynura.m.erejepbaeva.28@dartmouth.edu
# Brief description: This program is for performing a multilinear model on my project on analyzing sentiment analysis and effects of sexual assault cases in the Uzbek language.
#========================================================================.

# Load data
library(ggplot2)
library(car)
library(effects)
library(effectsize)

#=============================================.
# Part 1                  ====
# Construct a statistical model where you predict a y-variable using a linear model (LM), a linear
#mixed-effects model (LMER) or a generalized linear model (GLM). You need to include more than
#one predictor variable. (You should conduct a careful analysis of why two variables should or shouldn’t interact).
#=============================================.

# Load files
sentiment <- read.csv("LINGS10 - Sentiment Data.csv", header = TRUE)
frequent <- read.csv("LINGS10 - Frequent Words.csv", header = TRUE)

# Convert variables to factors
sentiment$Source <- as.factor(sentiment$Source)
sentiment$GenderAuthor <- as.factor(sentiment$GenderAuthor)

# Data exploration
aggregate(sentiment$Sentiment, list(sentiment$Source, sentiment$GenderAuthor), mean)
aggregate(sentiment$Sentiment, list(sentiment$Source, sentiment$GenderAuthor), sd)

# Boxplot chart
ggplot(sentiment, aes(x = Source, y = Sentiment, fill = GenderAuthor)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(title = "Sentiment Score Per Source",
       x = "Source Type",
       y = "Sentiment Score",
       fill = "Gender") +
  theme_minimal()

# Multilinear model with interaction
modelSentiment <- lm(Sentiment ~ Source * GenderAuthor, data = sentiment)

# Model summary
summary(modelSentiment)

# Equation coefficients
coef(modelSentiment)

# R-squared
summary(modelSentiment)$r.squared
summary(modelSentiment)$adj.r.squared

# Diagnostic plots
par(mfrow = c(1,2))
plot(modelSentiment$res ~ modelSentiment$fit,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "gray")

qqnorm(modelSentiment$res)
qqline(modelSentiment$res)
par(mfrow = c(1,1))

# Assumption tests
shapiro.test(modelSentiment$residuals)       # Normality

ncvTest(modelSentiment)                      # Homoscedasticity
durbinWatsonTest(modelSentiment)             # Autocorrelation

# Post-hoc analysis
anova(modelSentiment)
plot(allEffects(modelSentiment), multiline = TRUE, ci.style = "bars")


