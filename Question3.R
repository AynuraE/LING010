#== Homework 1 =====================================
# LING10 Statistics for Linguistics
# Dartmouth College. January 25, 2026
# Aynura Erejepbaeva

#Library package
library(dplyr)

#Assigning emotion-words.csv file to a variable
emotionFile = "emotion-words.csv"
emotion <- read.csv(file = emotionFile, header = TRUE, sep = ",")

set.seed(123) 
emotion_sample <- sample_n(emotion, 20)

#sample mean
sample_mean <- mean(emotion_sample$V.Mean.Sum)
sample_mean

#population mean
population_mean <- mean(emotion$V.Mean.Sum)
population_mean

sample_sd <- sd(emotion_sample$V.Mean.Sum)
sample_se <- sample_sd / sqrt(20)

#confidence interval
z_95 <- 1.96
margin_error <- z_95 * sample_se

lower_ci <- sample_mean - margin_error
upper_ci <- sample_mean + margin_error

#lower confidence interval
lower_ci
#upper confidence interval
upper_ci

#Histogram
hist(emotion_sample$V.Mean.Sum,
     main = "Histogram of emotional valence (sample of 20)",
     xlab = "V.Mean.Sum")

#Q-Q plot
qqnorm(emotion_sample$V.Mean.Sum,
       pch = 1,
       frame = FALSE,
       main = "Q-Q plot of emotional valence (sample of 20)")
qqline(emotion_sample$V.Mean.Sum, col = "steelblue", lwd = 1)

#Shapiro test
shapiro.test(emotion_sample$V.Mean.Sum)

val_mean <- mean(emotion$V.Mean.Sum, na.rm = TRUE)
val_sd   <- sd(emotion$V.Mean.Sum, na.rm = TRUE)

# z-score the variable
emotion$zValence <- (emotion$V.Mean.Sum - val_mean) / val_sd

hist(emotion$zValence,
     main = "Histogram of z-scored emotional valence",
     xlab = "z(V.Mean.Sum)",
     col = "lightgray",
     border = "black")

# mean
mean(emotion$V.Mean.Sum)

#Histogram, Q-Q
par(mfrow=c(1,2))
hist(emotion$V.Mean.Sum, main="Histogram of emotional valence", xlab="V.Mean.Sum")
qqnorm(emotion$V.Mean.Sum, pch = 1, frame = FALSE, main="Q-Q plot of emotional valence")
qqline(emotion$V.Mean.Sum, col = "steelblue", lwd = 1)
par(mfrow=c(1,1))

#Shapiro test
shapiro.test(emotion$V.Mean.Sum)
