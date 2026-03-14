#== Homework 2 =====================================
# LING10 Statistics for Linguistics
# Dartmouth College. February 7, 2026
# Aynura Erejepbaeva
#========================================================================.

#load data
library('ggplot2')

#=============================================.
# Exercise 1.                             ====
# Animal ratings  
#=============================================.

#assign the name of the file
animalRatings <- read.csv(file = "animal-ratings copy.csv", header = TRUE, sep = ",")

# (a) A chart that shows the relationship that we're testing.
plot_a <- ggplot(animalRatings, aes(x = Class, y = meanFamiliarity)) +
  geom_boxplot() +
  labs(x = "Class type", y = "meanFamiliarity",
       title = "Familiarity of plants and animals")
plot_a
ratingAnimal = subset(animalRatings, Class == "animal")
ratingPlant = subset(animalRatings, Class == "plant")

#(b) The mean and confidence interval for each of the two groups (at a confidence of 95%)
aggregate(animalRatings$meanFamiliarity, list(func = animalRatings$Class), mean)

# Mean and 95% CI for animals
t_animal <- t.test(ratingAnimal$meanFamiliarity, conf.level = 0.95)
t_animal$estimate     # mean
t_animal$conf.int     # 95% CI

# Mean and 95% CI for plants
t_plant <- t.test(ratingPlant$meanFamiliarity, conf.level = 0.95)
t_plant$estimate
t_plant$conf.int

#Standard deviation
sd(animalRatings$meanFamiliarity)
sd(ratingPlant$meanFamiliarity)

#(c) Verification of whether the data meets the assumptions of 
#the appropriate test or not (either t-test or Wilcoxon).

# Shapiro-Wilk tests
shapiro.test(ratingAnimal$meanFamiliarity)
shapiro.test(ratingPlant$meanFamiliarity)

par(mfrow=c(2,2))

hist(ratingAnimal$meanFamiliarity, main="Animal familiarity")
qqnorm(ratingAnimal$meanFamiliarity); qqline(ratingAnimal$meanFamiliarity)

hist(ratingPlant$meanFamiliarity, main="Plant familiarity")
qqnorm(ratingPlant$meanFamiliarity); qqline(ratingPlant$meanFamiliarity)

par(mfrow=c(1,1))

#(d) A significance test using the appropriate test.
t_res <- t.test(meanFamiliarity ~ Class,
                data = animalRatings,
                alternative = "two.sided",
                var.equal = FALSE)

t_res
