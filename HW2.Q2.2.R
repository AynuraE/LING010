#== Homework 2 =====================================
# LING10 Statistics for Linguistics
# Dartmouth College. February 7, 2026
# Aynura Erejepbaeva
#========================================================================.

#load data
library(ggplot2)
install.packages("car")

#=============================================.
# Exercise 2.1.                             ====
# Dutch onset
#=============================================.

#assign the name of the file
dutch <- read.csv("dutch-ont.csv")

str(dutch)

dutch$PlosivePresent <- as.factor(dutch$PlosivePresent)
dutch$NumberOfSegmentsOnset <- as.factor(dutch$NumberOfSegmentsOnset)

#(a) Explore the data: Draw a chart that describes the phenomenon you’re studying.
ggplot(dutch,
       aes(x = NumberOfSegmentsOnset,
           y = DurationOfPrefix,
           fill = PlosivePresent)) +
  geom_boxplot(position = "dodge") +
  labs(
    x = "Number of segments in root onset",
    y = "Duration of ont- prefix",
    fill = "Plosive present",
    title = "Duration of ont- by plosive presence and onset complexity"
  )

#Standard deviation
by(dutch$DurationOfPrefix, dutch$PlosivePresent,sd)

by(dutch$DurationOfPrefix, dutch$NumberOfSegmentsOnset, sd)

#Choose an appropriate statistical test (one-way ANOVA, two-way ANOVA, repeated measures
#ANOVA). Think of which variables are between-subjects and which variables are within-subjects.

#This is two-way ANOVA

#Provide evidence that the datasets comply with the assumptions of the test you choose. The
#evidence should include Shapiro-Wilks tests and Levene tests. You can also use density charts,
#QQ-Plots and histograms if you desire.
anova_model <- aov(DurationOfPrefix ~
                     PlosivePresent * NumberOfSegmentsOnset,
                   data = dutch)

res <- residuals(anova_model)

shapiro.test(res)

hist(res, main="Histogram of residuals")
qqnorm(res); qqline(res, col="blue")

# Levene's test for equal variances
install.packages("car")
library(car)

leveneTest(DurationOfPrefix ~
             PlosivePresent * NumberOfSegmentsOnset,
           data = dutch)

#Conduct your test and report your results using the scientific format we used in class. This will
#include the results of the test, the effect size (if your test allows for this), the results of 
#the post hoc tests (if your test allows for this) and a description of interactions if there are any.
summary(anova_model)

anova_table <- summary(anova_model)[[1]]
eta_sq <- anova_table[,"Sum Sq"] / sum(anova_table[,"Sum Sq"])
eta_sq
