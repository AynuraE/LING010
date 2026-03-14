#========================================================================.
# LING10 Stats for Ling, 2022W                                       ====
# Rolando Coto. rolando.a.coto.solano@dartmouth.edu
# Week 4 class notes
#========================================================================.

library('ggplot2')
library('car')
library('effectsize')
library('effects')
library('overlapping')

#========================================================================.
# Example 1: NSL Manner and Path                                      ====
#
# Source: Levshina (2015) and Senghas et al. (2004)
# https://benjamins.com/sites/z.195/
#
# This dataset includes 3 generations of children learning Nicaraguan
# Sign Language. MannerPath is the percentage of expressions where
# the children use separate words for the manner and the path of
# motion. E.g.: ROLL [manner] DOWN [path]
#
# Is the percentage of manner-path expressions different for
# different cohorts of children?
#
# Senghas, A., Kita, S., & Ozyurek, A. (2004). Children creating core
# properties of language: Evidence from an emerging Sign Language in
# Nicaragua. Science, 305(5691), 1779-1782.
#========================================================================.


# Open file
nsl = read.csv("nsl.csv", header=T, sep=",")


# Data exploration

nsl$Cohort = as.factor(nsl$Cohort)

aggregate(nsl$MannerPath, list(nsl$Cohort), mean)
aggregate(nsl$MannerPath, list(nsl$Cohort), sd)

ggplot(nsl, aes(x = Cohort, y = MannerPath)) +
  geom_boxplot()+
  scale_y_continuous(name = "Proportion of separate expressions")+
  scale_x_discrete(name = "Cohort") +
  ggtitle("Manner and Path in NSL")


nsl1 = subset(nsl, Cohort==1)
nsl2 = subset(nsl, Cohort==2)
nsl3 = subset(nsl, Cohort==3)
nNSL1 = nrow(nsl1)
nNSL2 = nrow(nsl2)
nNSL3 = nrow(nsl3)
nNSL1
nNSL2
nNSL3


x <- list(X1=nsl1$MannerPath, X2=nsl2$MannerPath, X3=nsl3$MannerPath)
out <- overlap(x, plot=TRUE)


# Assumption tests: Normality (is the data distributed normally?)

shapiro.test(nsl1$MannerPath)
shapiro.test(nsl2$MannerPath)
shapiro.test(nsl3$MannerPath)

aggregate(MannerPath ~ Cohort, data=nsl, function(x) shapiro.test(x)$p.value)


# Assumption tests: Homoscedasticity (do the groups have the same variance?)

leveneTest(MannerPath ~ Cohort, data=nsl)
fligner.test(MannerPath ~ Cohort, data=nsl)


# ANOVA

colnames(nsl) = c("person","MannerPath","years")
anovaNSL = aov(MannerPath ~ years, data=nsl)
summary(anovaNSL)


# Post-hoc Analysis

TukeyHSD(anovaNSL)
plot(TukeyHSD(anovaNSL), las=1)

model.tables(anovaNSL, type = "means")
plot(allEffects(anovaNSL), multiline=TRUE, ci.style="bars")


# Effect size
eta_squared(anovaNSL)



#========================================================================.
# Exercise 1: POS and RT                                              ====
#
# (Simulated data based on)
# Part-of-speech and Reaction Time in English
# Dataset by Balota et al. (2007); appears in Levshina (2015)
# Length: word length
# SUBTLWF: normalized word frequency
# POS: part-of-speech
# Mean_RT: Mean reaction time
#
# Research question: Does POS affect Mean_RT?
#
#========================================================================.


# Read the file
filename = "elp-balota.csv"
engRT <- read.csv(file=filename, header=TRUE, sep=",")
engRT$POS = as.factor(engRT$POS)


# Create a new column with the logarithm transformation
engRT$logRT = log(engRT$Mean_RT)


# Data exploration

# How many samples does each group have?
# They all have more than 30, so we can treat them as normal
table(engRT$POS)

aggregate(Mean_RT ~ POS, data=engRT, FUN=mean)
aggregate(Mean_RT ~ POS, data=engRT, FUN=sd)

# Boxplot
ggplot(engRT, aes(x = POS, y = logRT)) +
  geom_boxplot()+
  scale_y_continuous(name = "log(RT)")+
  scale_x_discrete(name = "POS") +
  ggtitle("Reaction time by part of speech")


# Boxplot with the actual data points
ggplot(engRT, aes(x = POS, y = logRT)) +
  geom_boxplot()+
  scale_y_continuous(name = "log(RT)")+
  scale_x_discrete(name = "POS") +
  ggtitle("Reaction time by part of speech")+
  geom_jitter(position=position_jitter(0.2))


# Assumptions

#Q-Q plots
ggplot(engRT, aes(sample=logRT))+stat_qq()+facet_grid(. ~ POS) + stat_qq_line()


# Test assumptions: Shapiro and Levene
# The samples all have the same variance
# But they don't have theoretically normal distributions.
# There are so many datapoints though (JJ:159, NN:532, VB:189),
# and their distributions are so close to normal, that we will
# assume that they are normal.
aggregate(logRT ~ POS, data=engRT, function(x) shapiro.test(x)$p.value)
leveneTest(logRT ~ POS, data=engRT)


# Conduct ANOVA test
englishAnova = aov(logRT ~ POS, data=engRT)
summary(englishAnova)


# Conduct Post-Hoc tests
TukeyHSD(englishAnova)
plot(TukeyHSD(englishAnova))


# Calculate eta-squared
eta_squared(englishAnova)
remainingVariance = 1 - sum(eta_squared(englishAnova)$Eta2)
remainingVariance


# Optional: Effects chart with confidence interval bars
plot(allEffects(englishAnova), multiline=TRUE, ci.style="bars")


# Optional: Fancy overlap charts
e.jj = subset(engRT, POS=="JJ")
e.nn = subset(engRT, POS=="NN")
e.vb = subset(engRT, POS=="VB")

x <- list(X1=e.jj$logRT, X2=e.nn$logRT, X3=e.vb$logRT)
out <- overlap(x, plot=TRUE)


#========================================================================.
# Example 2: NSL Deictics                                             ====
#
# Source: Levshina (2015) and Senghas et al. (2004)
# https://benjamins.com/sites/z.195/
#
# This dataset includes 3 generations of children learning Nicaraguan
# Sign Language. It also includes the age when they joined the school.
# The variable "mod" refers to spatial modulation: signs that are produced
# in a non-central position. This indicates deictic and locative information.
# The variable contains the percentage of space-modulated signs per verb.
#
# Is the number of space-modulated signs per verb different for 
# different cohorts, and dependent on how old they were when they
# came into the school?
#
# Senghas, A., Kita, S., & Ozyurek, A. (2004). Children creating core
# properties of language: Evidence from an emerging Sign Language in
# Nicaragua. Science, 305(5691), 1779-1782.
#========================================================================.


# Open file
sr = read.csv("nsl-sharedref.csv", header=T, sep=",")

sr$cohort = as.factor(sr$cohort)
sr$age = factor(sr$age, levels = c("early", "middle", "late"))

# Data exploration

aggregate(mod ~ age + cohort, data=sr, FUN=mean)
aggregate(mod ~ age + cohort, data=sr, FUN=sd)

ggplot(sr, aes(x = age, y = mod, fill=cohort)) +
  geom_boxplot()+
  labs(title="Shared Reference in NSL", y="Average of spatial modulation per verb", x="Age", fill="Cohort")

interaction.plot(sr$age, sr$cohort, sr$mod)


# Overlaps

sr1e= subset(sr, age=="early" & cohort == "1")
sr1m= subset(sr, age=="middle" & cohort == "1")
sr1l= subset(sr, age=="late" & cohort == "1")
x <- list(X1=sr1e$mod, X2=sr1m$mod, X3=sr1l$mod)
out <- overlap(x, plot=TRUE)

sr2e= subset(sr, age=="early" & cohort == "2")
sr2m= subset(sr, age=="middle" & cohort == "2")
sr2l= subset(sr, age=="late" & cohort == "2")
x <- list(X1=sr2e$mod, X2=sr2m$mod, X3=sr2l$mod)
out <- overlap(x, plot=TRUE)


# Assumption checks

aggregate(mod ~ age + cohort, data = sr, function(x) shapiro.test(x)$p.value)

leveneTest(mod ~ age * cohort, data=sr)

fligner.test(mod ~ interaction(age,cohort), data=sr)


# ANOVA

sraov = aov(mod ~ age*cohort, data=sr)
summary(sraov)


# Post-hoc Analysis

TukeyHSD(sraov)
plot(TukeyHSD(sraov), las=1, cex.axis=.5)


# Effect size
eta_squared(sraov, partial=FALSE)
# The "partial eta" is the proportion of variance explained by a
# given variable of the total variance remaining after accounting
# for variance explained by other variables in the model
eta_squared(sraov)


#========================================================================.
# Exercise 2: French Consonants                                      ====
#
# Independent variables:
# Type of words = {function, content}
# Consonants    = {t,f,s}
#
# Dependent variable:
# Reaction time (in milliseconds)
#
# Question: Do function words have shorter syllable
# durations than content words? Do specific consonants
# in the onset cause a longer or shorter syllable durations?
#========================================================================.


# Read the file
frenchCons  = "frenchCons-example.csv"
frenchCons <- read.csv(file=frenchCons, header=TRUE, sep=",")


# Data exploration

table(frenchCons$type, frenchCons$cons)

aggregate(syldur ~ type*cons, data = frenchCons, mean)
aggregate(syldur ~ type*cons, data = frenchCons, sd)

ggplot(frenchCons, aes(x = type, y = syldur, fill=cons)) +
  geom_boxplot()+
  scale_y_continuous(name = "Syllable duration")+
  scale_x_discrete(name = "Type of word") +
  ggtitle("Type of word versus duration of syllables\ndepending on the first consonant")


interaction.plot(frenchCons$type, frenchCons$cons, frenchCons$syldur)


# Assumptions

ggplot(frenchCons, aes(sample=syldur))+stat_qq()+facet_grid(type ~ cons) + stat_qq_line()

aggregate(syldur ~ type*cons, data = frenchCons, function(x) shapiro.test(x)$p.value)
leveneTest(syldur ~ type*cons, data=frenchCons)


# ANOVA
frenchAnova = aov(syldur ~ type*cons, data=frenchCons)
summary(frenchAnova)


# Conduct Post-Hoc tests
TukeyHSD(frenchAnova)
plot(TukeyHSD(frenchAnova), las=1, cex.axis=.4)


# Calculate eta-squared
eta_squared(frenchAnova, partial=FALSE)
remainingVariance = 1 - sum(eta_squared(frenchAnova,partial=FALSE)$Eta2)
remainingVariance


# Optional: Interactions and confidence interval bars
plot(allEffects(frenchAnova), multiline=TRUE, ci.style="bars")


# Optional: Fancy overlap charts
c.k = subset(frenchCons, type=="content" & cons == "k")
c.s = subset(frenchCons, type=="content" & cons == "s")
c.t = subset(frenchCons, type=="content" & cons == "t")
f.k = subset(frenchCons, type=="function" & cons == "k")
f.s = subset(frenchCons, type=="function" & cons == "s")
f.t = subset(frenchCons, type=="function" & cons == "t")

x <- list(X1=c.k$syldur, X2=c.s$syldur, X3=c.t$syldur)
out <- overlap(x, plot=TRUE)

x <- list(X1=f.k$syldur, X2=f.s$syldur, X3=f.t$syldur)
out <- overlap(x, plot=TRUE)


#========================================================================.
# Example 3: Russian Speech Rate                                     ====
#
# syllsPerSec: Syllables per second when speaking
# type: Type of speech (general prose, narration, verse)
#
# Does the speech rate vary according to type of speech act?
#========================================================================.


# Read the file
filename = "readingRate.csv"
readingRate <- read.csv(file=filename, header=TRUE, sep=",")
readingRate$type = as.factor(readingRate$type)

# Data exploration

aggregate(syllsPerSec ~ type, data=readingRate, mean)
aggregate(syllsPerSec ~ type, data=readingRate, sd)

ggplot(readingRate, aes(x = type, y = syllsPerSec)) +
  geom_violin()+
  geom_boxplot(width=0.1)+
  scale_y_continuous(name = "Syllables per second")+
  scale_x_discrete(name = "Type of reading") +
  ggtitle("Speech rate in 3 types of reading")


# Assumptions

aggregate(syllsPerSec ~ type, data=readingRate, function(x) shapiro.test(x)$p.value)
leveneTest(syllsPerSec ~ type, data=readingRate)
fligner.test(syllsPerSec ~ type, data=readingRate)


# ANOVA

rr.aov = aov(syllsPerSec ~ type + Error(student/type), data=readingRate)
summary(rr.aov)


# Effect size
eta_squared(rr.aov, partial=FALSE)


#========================================================================.
# Example 4: Russian Speech Rate by University and Genre             ====
#
# syllsPerSec: Syllables per second when speaking
# type: Type of speech (general prose, narration, verse)
# university: University the students were enrolled in (MGU, HSE)
#
# Does the speech rate vary according to type of speech act and to
# the university that the student was enrolled in?
#========================================================================.


# Read the file
filename = "readingRate-mx.csv"
readingRateMx <- read.csv(file=filename, header=TRUE, sep=",")
readingRateMx$type = as.factor(readingRateMx$type)


# Data exploration

aggregate(syllsPerSec ~ type*university, data=readingRateMx, mean)
aggregate(syllsPerSec ~ type*university, data=readingRateMx, sd)

aggregate(syllsPerSec ~ type, data=readingRateMx, mean)
aggregate(syllsPerSec ~ type, data=readingRateMx, sd)

aggregate(syllsPerSec ~ university, data=readingRateMx, mean)
aggregate(syllsPerSec ~ university, data=readingRateMx, sd)


ggplot(readingRateMx, aes(x = type, y = syllsPerSec, fill=university)) +
  geom_boxplot()+
  labs(title="Speech Rate by genre and university", x="Age", y="Speech rate (sylls/second)", fill="University")


# Assumptions

aggregate(syllsPerSec ~ type*university, data=readingRateMx, function(x) shapiro.test(x)$p.value)
leveneTest(syllsPerSec ~ type*university, data=readingRateMx)


# ANOVA

rrMx.aov = aov(syllsPerSec ~ type*university + Error(student/type), data=readingRateMx)
summary(rrMx.aov)


# Effect size
eta_squared(rrMx.aov, partial=FALSE)


#========================================================================.
# Exercise 3: Icelandic Vowel Length                                 ====
#
# The variable cons1 constains different types of
# consonant onsets in Icelandic. One of these types is 
# "asp" (aspirated stops) and another one is
# "nasp" (unaspirated stops).
#
# There is another variable, vowel.dur, which
# measures the duration of the vowel after a
# consonant.
#
# Make an ANOVA of vowel.dur versus cons1 to
# answer the question: Is the vowel length
# different for aspirated and unaspirated
# consonants?
#
# This is data from the Masters' Thesis:
# Coretta, Stefano (2017). Vowel duration and
# aspiration effects in Icelandic. University
# of York. URL: https://osf.io/6au2k/
#
# The data is in icelandic-stops.csv
# Assume that the data meets the assumptions of ANOVA
#========================================================================.

# Read file
icelandicFile = "icelandic-stops.csv"
icelandic <- read.csv(file=icelandicFile, header=TRUE, sep=",")


#========================================================================.
# Exercise 4: Dutch -lijk                                            ====
#
# This dataset documents variation in the use of the 80 most frequent
# words ending in the suffix -lijk in written Dutch.
#
# Country:  a factor with levels Flanders and Netherlands
# Register: a factor with levels National, Quality and Regional
#           coding the type of newspaper.
# Count:    A numeric vector with token counts in the CONDIV corpus.
#
# Is the log(Count) of words related to the 
# interaction of Country and Register?
#
# https://rdrr.io/cran/languageR/man/writtenVariationLijk.html
# Keune, K., Ernestus, M., Van Hout, R. and Baayen, R.H. (2005) Social,
# geographical, and register variation in Dutch: From written 'mogelijk'
# to spoken 'mok', Corpus Linguistics and Linguistic Theory, 1, 183-223.
#========================================================================.

# Read file
filename = "writtenVariationLijk.csv"
w <- read.csv(file=filename, header=TRUE, sep=",")
