#== Homework 2 =====================================
# LING10 Statistics for Linguistics
# Dartmouth College. February 7, 2026
# Aynura Erejepbaeva
#========================================================================.

#load data
library('ggplot2')

#=============================================.
# Exercise 1. 2.                             ====
# Taiwan subject  
#=============================================.

#assign the name of the file
taiwan <- read.csv("taiwan-subject-test.csv", header = TRUE)

#(a) A chart that shows the relationship that we're testing.
ggplot(taiwan, aes(x = beginningScore, y = endScore)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Beginning score", y = "End score",
       title = "Change in scores over time")

#(b) The mean and confidence interval for each of the two groups (at a confidence of 95%)
#Mean
mean_begin <- mean(taiwan$beginningScore)
mean_end   <- mean(taiwan$endScore)

#Confidence interval
t.test(taiwan$beginningScore, conf.level = 0.95)$conf.int
t.test(taiwan$endScore, conf.level = 0.95)$conf.int

#Standard deviation 
sd(taiwan$beginningScore)
sd(taiwan$endScore)

#(c) Verification of whether the data meets the assumptions of 
#the appropriate test or not (either t-test or Wilcoxon).
diff_scores <- taiwan$endScore - taiwan$beginningScore

hist(diff_scores, main = "Histogram of score differences",
     xlab = "End − Beginning")

qqnorm(diff_scores)
qqline(diff_scores, col = "blue")

shapiro.test(diff_scores)

#(d) A significance test using the appropriate test.
t_res <- t.test(taiwan$beginningScore,
                taiwan$endScore,
                paired = TRUE,
                alternative = "two.sided")

t_res
