#== Homework 1 =====================================
# LING10 Statistics for Linguistics
# Dartmouth College. January 25, 2026
# Aynura Erejepbaeva

#I've reused code from X-hour Lab 1 and 2. 

frenchFile = "french_reduction.csv"

#Assigning french_reduction.csv file to a variable
french <- read.csv(file = frenchFile, header = TRUE, sep = ",")

#string
str(french)
unique(french$func)
summary(french$syldur)

#categorizing variables
french$func <- as.factor(french$func)
french$c1   <- as.factor(french$c1) 

french$syldur
str(french$syldur)

french$func
str(french$func)

french$c1
str(french$c1)

french$speechrate
str(french$speechrate)

mean(french$syldur)
median(french$syldur)
sd(french$syldur)

#(a)func -> syldur
aggregate(french$syldur, list(func = french$func), mean)
aggregate(french$syldur, list(func = french$func), median)

#(b)func + c1 -> syldur
aggregate(french$syldur, list(func = french$func, c1 = french$c1), mean)
aggregate(french$syldur, list(func = french$func, c1 = french$c1), median)

#install.packages("ggplot2") we don't have to install the package over and over again.
library('ggplot2')

#Plots
#Plot a
plot_a <- ggplot(french, aes(x = func, y = syldur)) +
  geom_boxplot() +
  labs(x = "Word type (func)", y = "Syllable duration (ms)",
       title = "Syllable duration by word type")
plot_a

#Plot b
plot_b <- ggplot(french, aes(x = c1, y = syldur, fill = func)) +
  geom_boxplot(position = "dodge") +
  labs(x = "Onset consonant (c1)", y = "Syllable duration (ms)", fill = "Word type",
       title = "Syllable duration by onset consonant and word type")
plot_b

#Plot c
plot_c <- ggplot(french, aes(x = speechrate, y = syldur, color = func)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Speech rate (syll/sec)", y = "Syllable duration (ms)", color = "Word type",
       title = "Syllable duration vs speech rate by word type")
plot_c

