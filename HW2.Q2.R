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
# German modals
#=============================================.

#assign the name of the file
modals <- read.csv("german-modals-20.csv")

str(modals)
table(modals$country)

modals$country <- as.factor(modals$country)

#(a) Explore the data: Draw a chart that describes the phenomenon you’re studying.
ggplot(modals, aes(x = country, y = perc)) +
  geom_boxplot() +
  labs(x = "Country", y = "% modal-first order", title = "Modal-first order by country")

#Choose an appropriate statistical test (one-way ANOVA, two-way ANOVA, repeated measures
#ANOVA). Think of which variables are between-subjects and which variables are within-subjects.

#This is one-way ANOVA.

#Provide evidence that the datasets comply with the assumptions of the test you choose. The
#evidence should include Shapiro-Wilks tests and Levene tests. You can also use density charts,
#QQ-Plots and histograms if you desire.

# country is a factor
modals$country <- as.factor(modals$country)

# Shapiro-Wilk normality test within each country group
by(modals$perc, modals$country, shapiro.test)

#Standard deviation
by(modals$perc, modals$country, sd)

#Mean
by(modals$perc, modals$country, mean)

#P-values
by(modals$perc, modals$country, function(x)
  t.test(x, mu = 0)$p.value
)

# Levene's test for equal variances
library(car)
leveneTest(perc ~ country, data = modals)

#Conduct your test and report your results using the scientific format we used in class. This will
#include the results of the test, the effect size (if your test allows for this), the results of 
#the post hoc tests (if your test allows for this) and a description of interactions if there are any.
anova_res <- aov(perc ~ country, data = modals)
summary(anova_res)

TukeyHSD(anova_res)
