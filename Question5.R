#== Homework 1 =====================================
# LING10 Statistics for Linguistics
# Dartmouth College. January 25, 2026
# Aynura Erejepbaeva

#Library package
library(dplyr)

#Assigning animal-ratings.csv file to a variable
animals <- read.csv("animal-ratings.csv", header = TRUE)
str(animals)

#Histogram
hist(animals$meanFamiliarity,
     main = "Histogram of mean familiarity",
     xlab = "Mean familiarity",
     col = "lightgray", #color
     border = "black") #borders of the histogram

#Q-Q plot
qqnorm(animals$meanFamiliarity)
qqline(animals$meanFamiliarity, col = "blue") #color

#Shapiro test
shapiro.test(animals$meanFamiliarity)

#To get the mean
mean(animals$meanFamiliarity)

#Part b
#Before applying log transformation

#Histogram
hist(animals$FreqSingular,
     main = "Histogram of singular frequency",
     xlab = "Frequency (singular)",
     col = "lightgray", #color
     border = "black") #borders of the histogram

#Q-Q plot
qqnorm(animals$FreqSingular)
qqline(animals$FreqSingular, col = "blue") #color that crosses across the plot

#Shapiro test
shapiro.test(animals$FreqSingular)

#log transformation
animals$logFreqSingular <- log(animals$FreqSingular + 1)

hist(animals$logFreqSingular,
     main = "Histogram of log-transformed singular frequency",
     xlab = "log(FreqSingular)",
     col = "lightgray",
     border = "black")

qqnorm(animals$logFreqSingular)
qqline(animals$logFreqSingular, col = "blue")

qqnorm(animals$logFreqSingular, pch = 1, frame = FALSE)
qqline(animals$logFreqSingular, col = "blue", lwd = 2)

shapiro.test(animals$logFreqSingular)

#Part c
#Before applying log transformation

#Histogram
hist(animals$meanWeightRating,
     main = "Histogram of mean weight rating",
     xlab = "Mean weight rating",
     col = "lightgray", #color
     border = "black") #borders of the histogram

#Q-Q plot
qqnorm(animals$meanWeightRating)
qqline(animals$meanWeightRating, col = "blue") #color is blue

#Shapiro test
shapiro.test(animals$meanWeightRating)

#log transformation
animals$logWeight <- log(animals$meanWeightRating)

#Histogram
hist(animals$logWeight,
     main = "Histogram of log-transformed weight ratings",
     xlab = "log(meanWeightRating)",
     col = "lightgray",
     border = "black")

#Q-Q plot
qqnorm(animals$logWeight)
qqline(animals$logWeight, col = "blue")

#Shapiro test
shapiro.test(animals$logWeight)
