#== Homework 2 =====================================
# LING10 Statistics for Linguistics
# Dartmouth College. February 7, 2026
# Aynura Erejepbaeva
#========================================================================.

#load data
library(ggplot2)
library(car)

#=============================================.
# Exercise 2.1.                             ====
# Spavot learning
#=============================================.

#assign the name of the file
spavot <- read.csv("spavot-learning.csv")

str(spavot)

spavot$student  <- as.factor(spavot$student)
spavot$timeTest <- as.factor(spavot$timeTest)
spavot$year     <- as.factor(spavot$year)

#(a) Explore the data: Draw a chart that describes the phenomenon you’re studying.
ggplot(spavot, aes(x = timeTest, y = vot, group = student)) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ year) +
  labs(title = "VOT change from beginning to end, by year group",
       x = "Test time", y = "VOT (ms)")

#Standard deviation
by(spavot$vot, spavot$year, sd)

#P value for the third year
t.test(spavot$vot[spavot$year == "3rd"], mu = 50)$p.value

with(spavot,
     tapply(vot,
            list(year, timeTest),
            mean))

#Choose an appropriate statistical test (one-way ANOVA, two-way ANOVA, repeated measures
#ANOVA). Think of which variables are between-subjects and which variables are within-subjects.

#This is mixed ANOVA

#Provide evidence that the datasets comply with the assumptions of the test you choose. The
#evidence should include Shapiro-Wilks tests and Levene tests. You can also use density charts,
#QQ-Plots and histograms if you desire.
anova_model <- aov(vot ~ timeTest * year + Error(student / timeTest), data = spavot)

res <- residuals(anova_model)

by(
  spavot$vot,
  list(spavot$year, spavot$timeTest),
  shapiro.test
)

# Levene's test for equal variances
leveneTest(vot ~ year, data = spavot)

#Conduct your test and report your results using the scientific format we used in class. This will
#include the results of the test, the effect size (if your test allows for this), the results of 
#the post hoc tests (if your test allows for this) and a description of interactions if there are any.
summary(anova_model)

anova_tab <- summary(anova_model)[[2]][[1]]
eta_sq <- anova_tab[,"Sum Sq"] /
  (anova_tab[,"Sum Sq"] + anova_tab["Residuals","Sum Sq"])
eta_sq
