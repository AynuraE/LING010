#== Homework 1 =====================================
# LING10 Statistics for Linguistics
# Dartmouth College. January 25, 2026
# Aynura Erejepbaeva

#Assigning dutch-freq.csv file to a variable
dutchFile = "dutch_freq.csv"
dutch <- read.csv("dutch_freq.csv", header = TRUE, sep = ",")

#string and tables
str(dutch)
table(dutch$Regularity, useNA = "ifany")
table(dutch$Auxiliary, useNA = "ifany")

# 3) categorical
dutch$Regularity <- as.factor(dutch$Regularity)
dutch$Auxiliary  <- as.factor(dutch$Auxiliary)

# 4) Core relationship
tab <- table(dutch$Regularity, dutch$Auxiliary)
tab

# 5) within each Regularity, what % are each Auxiliary?
prop_by_reg <- prop.table(tab, margin = 1)
round(prop_by_reg, 3)

# 6) within each Auxiliary, what % are regular/irregular?
prop_by_aux <- prop.table(tab, margin = 2)
round(prop_by_aux, 3)

# 7) Chi-square test of association (typical for two categorical variables)
chisq.test(tab)

#install.packages("ggplot2")  #We don't have to run over and over again
library(ggplot2)

#ggplot
ggplot(dutch, aes(x = Regularity, fill = Auxiliary)) +
  geom_bar(position = "fill") +  # "fill" makes it proportions (0 to 1)
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Regularity",
    y = "Proportion of verbs",
    fill = "Auxiliary",
    title = "Auxiliary choice by verb regularity (Dutch)"
  )
