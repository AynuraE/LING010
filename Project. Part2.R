#== Project: Part 2 =====================================
# LING10 Statistics for Linguistics
# Dartmouth College. February 15, 2026
# Aynura Erejepbaeva
#========================================================================.

#load data
library('ggplot2')

#=============================================.
# Exercise 4, 5, 7, 10.                   ====
# Calculating the mean, median, standard deviation and confidence interval of numerical variables. 
#=============================================.

sentiment <- read.csv("LINGS10 - Sentiment Data.csv", header = TRUE)

sentiment <- c(
  -0.88, -0.72, -0.93, -0.91, -0.89,
  -0.80, -0.95, -0.97, -0.96, -0.94,
  -0.90, -0.86, -0.20, -0.97, -0.60
)


mean(sentiment)
median(sentiment)
sd(sentiment)

n <- length(sentiment)          # sample size
mean_val <- mean(sentiment)     # mean
sd_val <- sd(sentiment)         # standard deviation
se <- sd_val / sqrt(n)          # standard error

t.test(sentiment)$conf.int

# Frequency table for gender
table(sentiment$GenderAuthor)

# Frequency table for sources
table(sentiment$Source)

ggplot(sentiment, aes(x = GenderAuthor)) +
  geom_bar() +
  labs(title = "Distribution of Gender",
       x = "GenderAuthor",
       y = "Count")

ggplot(sentiment, aes(x = GenderAuthor, y = Sentiment, fill=Source)) +
  geom_boxplot()+
  labs(title="Gender of Authors", y="Sentiment Score", x="GAuthor", fill="Source")

ggplot(sentiment, aes(x = GenderAuthor)) +
  geom_bar() +
  labs(title = "Distribution of Gender",
       x = "Gender",
       y = "Count")

ggplot(sentiment, aes(x = Source)) +
  geom_bar() +
  labs(title = "Distribution of Sources",
       x = "Source",
       y = "Count")

# Find the most frequent category (mode)
gender_mode <- names(which.max(table(sentiment$GenderAuthor)))
gender_mode

source_mode <- names(which.max(table(sentiment$Source)))
source_mode

#Histogram
hist(sentiment$Sentiment,
     main = "Histogram of Sentiment Scores",
     xlab = "Sentiment Score",
     col = "lightblue",
     border = "black")

#Q-Q plot
qqnorm(sentiment$Sentiment)
qqline(sentiment$Sentiment, col = "blue")

#Shapiro Test
shapiro.test(sentiment$Sentiment)

frequency <- read.csv("LINGS10 - Frequent Words.csv", header = TRUE)

# Chart: FrequentAyol by Gender
ggplot(frequency, aes(x = FrequentAyol, fill = GenderAuthor)) +
  geom_bar(position = "dodge") +
  labs(title = "FrequentAyol by Gender",
       x = "FrequentAyol",
       y = "Count",
       fill = "Gender")

# Chart: FrequentErkak by Gender
ggplot(frequency, aes(x = FrequentErkak, fill = GenderAuthor)) +
  geom_bar(position = "dodge") +
  labs(title = "FrequentErkak by Gender",
       x = "FrequentErkak",
       y = "Count",
       fill = "Gender")


