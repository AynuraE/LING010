#== Project: Initial Statistical Model =====================================
# LING10 Statistics for Linguistics
# Dartmouth College. March 6, 2026
# Name: Aynura Erejepbaeva
# Email: aynura.m.erejepbaeva.28@dartmouth.edu
# Brief description: This program is for performing ANOVA, post-hoc and chart tests on my project on analyzing sentiment analysis of sexual assault cases in the Uzbek language.
#========================================================================.

#load data
library(ggplot2)
library(car)
library(effects)
library(effectsize)

#=============================================.
# Part 3                  ====
# You need to perform a simple test of your data. 
# ANOVA and post-hoc test
# One chart
#=============================================.

## Load files
frequentwords <- read.csv("LINGS10 - Frequent Words.csv", header = TRUE)

sentiment <- read.csv("LINGS10 - Sentiment Data.csv", header = TRUE)

# Data exploration
sentiment$Source <- as.factor(sentiment$Source)
sentiment$GenderAuthor <- as.factor(sentiment$GenderAuthor)

aggregate(sentiment$Sentiment, list(sentiment$Source), mean)
aggregate(sentiment$Sentiment, list(sentiment$Source), sd)

#chart
ggplot(sentiment, aes(x = Source, 
                      y = Sentiment, 
                      fill = GenderAuthor)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(title = "Sentiment Score Per Source",
       x = "Source Type",
       y = "Sentiment Score",
       fill = "Gender") +
  theme_minimal()

newspaperF <- subset(sentiment, Source == "newspaper" & GenderAuthor == "F")
articleF <- subset(sentiment, Source == "article" & GenderAuthor == "F")
newspaperM <- subset(sentiment, Source == "newspaper" & GenderAuthor == "M")
articleM <- subset(sentiment, Source == "article" & GenderAuthor == "M")

nrow(newspaperF)
nrow(articleF)
nrow(newspaperM)
nrow(articleM)

# Assumption tests: Normality (is the data distributed normally?)
newspaperF = subset(sentiment, Source == "newspaper" & GenderAuthor == "F")
newspaperM = subset(sentiment, Source == "newspaper" & GenderAuthor == "M")
articleF = subset(sentiment, Source == "article" & GenderAuthor == "F")
articleM = subset(sentiment, Source == "article" & GenderAuthor == "M")

shapiro.test(newspaperF$Sentiment)
shapiro.test(articleF$Sentiment)
shapiro.test(newspaperM$Sentiment)
shapiro.test(articleM$Sentiment)

aggregate(Sentiment ~ Source, data=sentiment, function(x) shapiro.test(x)$p.value)

# Assumption tests: Homoscedasticity (do the groups have the same variance?)
leveneTest(Sentiment ~ Source * GenderAuthor, data = sentiment)
fligner.test(Sentiment ~ interaction(Source, GenderAuthor), data = sentiment)

#Two way ANOVA
anovaSentiment <- aov(Sentiment ~ Source * GenderAuthor, data = sentiment)
summary(anovaSentiment)

# Post-hoc Analysis
TukeyHSD(anovaSentiment)
plot(TukeyHSD(anovaSentiment), las=1)

model.tables(anovaSentiment, type="means")
plot(allEffects(anovaSentiment), multiline=TRUE, ci.style="bars")

# Effect size
eta_squared(anovaSentiment)




