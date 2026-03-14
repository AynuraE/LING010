#========================================================================.
# LING10 Stats for Ling, 2026W                                       ====
# Rolando Coto. rolando.a.coto.solano@dartmouth.edu
# Week 7 Code and Exercises (LM Interactions)
#========================================================================.


#== Load me =============================================================
library(ggplot2)
library(car)
library(asbio)
library(lsmeans)
library(dplyr)
library(plotly)
library(lmtest)
library(MASS)


#========================================================================.
# Example 1: NSL Deictics                                            ====
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
# The data is real: Senghas, A., Kita, S., & Ozyurek, A. (2004).
# Children creating core properties of language: Evidence from
# an emerging Sign Language in Nicaragua. Science, 305(5691), 1779-1782.
#========================================================================.


# Read file
nsl = read.csv(file="nsl2.csv", header=TRUE, sep=",")
nsl$cohort = as.factor(nsl$cohort)
nsl$age = as.factor(nsl$age)
nsl$age = factor(nsl$age,levels(nsl$age)[c(1,3,2)])


# Model 
mNSL = lm(mod ~ age*cohort, data = nsl)
summary(mNSL)


# Post-hoc analysis
lsmeans(mNSL, pairwise ~ age*cohort, adjust="tukey")


# Q-Q Plots of the regression residuals
par(mfrow=c(1,2))
plot(mNSL$res~mNSL$fit,xlab="Fitted value",ylab="Residuals")
abline(h=0,col="gray")
qqnorm(mNSL$res,main="")
qqline(mNSL$res)
par(mfrow=c(1,1))


# Shapiro-Wilk Test of normality
shapiro.test(mNSL$residuals)


# variances
ncvTest(mNSL)


# Autocorrelation
durbinWatsonTest(mNSL)


# Collinearity in chart
pairs(~ age+cohort, data=nsl, cex.labels=2.5, cex=1.5, lower.panel=panel.lm, upper.panel=panel.cor.res)


# Collinearity
vif(mNSL)


# Chart
ggplot(nsl, aes(x = age, y = mod, fill=cohort)) +
  geom_boxplot()+
  scale_y_continuous(name = "Average of spatial modulation per verb")+
  scale_x_discrete(name = "Age") +
  ggtitle("Shared Reference in NSL")


# Interaction chart
interaction.plot(nsl$age, nsl$cohort, nsl$mod)


#========================================================================.
# Exercise 1A: Dutch -ont                                             ====
#
# Dutch has a prefix ont- (e.g. onthuld 'revealed', ontheven
# 'relieved'). The ont- can be followed by a plosive sound
# (e.g. ontploft 'exploded'), or by other sounds. (e.g. ontaarden
# 'to degenerate'). This information is in the column PlosivePresent.
#
# There is another column, NumberOfSegmentsOnset, which tells you
# the number of segments in the onset of the root syllable. (For
# example, e.g. ontmoet 'met' has 1 segment in the root onset,
# and ontsnappen 'relax' has two).
#
# Do these two variables (whether a plosive is present or not,
# and the number of segments in the onset) influence the
# DurationOfPrefix?
#
# The data is real: Pluymaekers, M., Ernestus, M. and Baayen,
# R. H. (2005) Frequency and acoustic length: the case of
# derivational affixes in Dutch, Journal of the Acoustical
# Society of America, 118, 2561-2569.
# https://rdrr.io/cran/languageR/man/durationsOnt.html
#========================================================================.


# Read file
ont.f = read.csv(file="dutch-ont-interact.csv", header=TRUE, sep=",")
ont.f$NumberOfSegmentsOnset = as.factor(ont.f$NumberOfSegmentsOnset)


# Data exploration
aggregate(ont.f$DurationOfPrefix, list(ont.f$PlosivePresent, ont.f$NumberOfSegmentsOnset), mean)
aggregate(ont.f$DurationOfPrefix, list(ont.f$PlosivePresent, ont.f$NumberOfSegmentsOnset), sd)


# model
mOnt.f = lm(DurationOfPrefix ~ PlosivePresent * NumberOfSegmentsOnset, data = ont.f)
summary(mOnt.f)


# another type of chart: boxplot
ont.f$plosiveGGVar[ont.f$PlosivePresent=="No"] = "No plosive in the root"
ont.f$plosiveGGVar[ont.f$PlosivePresent=="Yes"] = "Plosive present in the root"
ont.f$plosiveGGVar = as.factor(ont.f$plosiveGGVar)
ont.f$plosiveGGVar <- relevel(ont.f$plosiveGGVar, ref="Plosive present in the root")
ggplot(ont.f, aes(x = plosiveGGVar, y = DurationOfPrefix)) +
  facet_wrap(NumberOfSegmentsOnset ~ .)+
  geom_boxplot()+
  scale_y_continuous(name = "Duration of prefix (z-scored seconds)")+
  scale_x_discrete(name = "Number of segments in the onset") +
  ggtitle("Duration of Dutch ont- by whether\nthe verb root has a plosive or not")


# R-squared
summary(mOnt.f)$r.squared
summary(mOnt.f)$adj.r.squared


# Q-Q Plots for the variance of the residuals
par(mfrow=c(1,2))
plot(mOnt.f$res~mOnt.f$fit,xlab="Fitted value",ylab="Residuals")
abline(h=0,col="gray")
qqnorm(mOnt.f$res,main="")
qqline(mOnt.f$res)
par(mfrow=c(1,1))


# Shapiro-Wilk Test of normality
shapiro.test(mOnt.f$residuals)


# Homoscedasticity of variances
ncvTest(mOnt.f)


# Autocorrelation
durbinWatsonTest(mOnt.f)


# Collinearity
vif(mOnt.f)


#========================================================================.
# Example 1B: Pitch of Turkish Vowels                                ====
#
# The dataset contains 4347 measurements of F0 (f0, in semitones)
# from the first 50 msec of the vowel in CV-initial, utterance
# initial words (1265 types), from the GlobalPhone Turkish corpus,
# of read sentences from 98 speakers.
#
# Is the pitch of a vowel dependent on the vowel itself, and
# on the voicing of the preceding consonant? (Assume the data
# is not autocorrelated)
#
# Pg. 428, Sonderegger's Textbook (https://osf.io/pnumg/)
# Original dataset: https://osf.io/vga8k/
#
# The data is real: Sonderegger, Morgan, M McAuliffe, and H Bang.
# 2017. Segmental influences on f0: A large-scale study of
# cross-linguistic and interspeaker variability. Talk delivered
# at Fourth Workshop on Sound Change.
# Slides at http://people.linguistics.mcgill.ca/~morgan/wsc2017_slides.pdf
#========================================================================.


# Load file
turkish = read.csv("turkish_if0_rmld.csv", header=TRUE, sep=",", encoding="utf8")


# Average the F0 of all instances of the vowels over each word
# E.g. Find all of the instances of the word "biz" "we". Get
# all of their "i" vowels (all of which are preceded by the
# voiced consonant "b"), and average them.
turkish = turkish %>%
  group_by(word, Voicing, base_vowel) %>%
  summarise(f0_m = mean(f0))


# Data exploration
aggregate(turkish$f0_m, list(turkish$Voicing, turkish$base_vowel), mean)
aggregate(turkish$f0_m, list(turkish$Voicing, turkish$base_vowel), sd)


# Bar plot with vowel in the x-axis
ggplot(turkish, aes(x=base_vowel, y=f0_m, fill=Voicing)) +
  geom_boxplot()+
  labs(y = "Vowel F0 (st)", title = "F0 of Turkish Vowels by the\nvoicing of the preceding consonant", x = "Vowel", color="Voicing")


# Bar plot with vowel in the fill
ggplot(turkish, aes(x=Voicing, y=f0_m, fill=base_vowel)) +
  geom_boxplot()+
  labs(y = "Vowel F0 (st)", title = "F0 of Turkish Vowels by the\nvoicing of the preceding consonant", x = "Voicing", fill="Vowel")


# A slightly different plot; might be useful to see the variation
ggplot(turkish, aes(x = Voicing, y = f0_m, color = base_vowel)) +
  stat_summary(position = position_dodge(width = 0.2)) +
  stat_summary(aes(group = base_vowel), geom = "line", position = position_dodge(width = 0.2)) +
  labs(y = "Vowel F0 (st)", x="Voicing", fill = "Vowel", title = "F0 of Turkish Vowels by the\nvoicing of the preceding consonant", color="Vowel")


# Interaction chart
interaction.plot(turkish$Voicing, turkish$base_vowel, turkish$f0_m)


# Model 
mTurkish = lm(f0_m ~ base_vowel*Voicing, data = turkish)
summary(mTurkish)


# Post-hoc analysis
lsmeans(mTurkish, pairwise ~ base_vowel*Voicing, adjust="tukey")


# Histogram of the y-variable (to determine normality of y)
hist(turkish$f0_m)


# Histogram of the residuals (to determine their normality)
hist(mTurkish$residuals)


# Q-Q Plots of the regression residuals
par(mfrow=c(1,2))
plot(mTurkish$res~mTurkish$fit,xlab="Fitted value",ylab="Residuals")
abline(h=0,col="gray")
qqnorm(mTurkish$res,main="")
qqline(mTurkish$res)
par(mfrow=c(1,1))


# Shapiro-Wilk Test of normality
# Should we assume that it is normal?
shapiro.test(mTurkish$residuals)


# Homoscedasticity of variances
ncvTest(mTurkish)


# Autocorrelation
durbinWatsonTest(mTurkish)


# Collinearity
vif(mTurkish)


#========================================================================.
# Example 2: Cavs and Happiness                                      ====
#
# In order to better understand numerical x categorical interactions, we
# will use a non-linguistic example. It comes from here:
# https://stats.stackexchange.com/questions/284798/interaction-vs-association
#
# This is a fictional dataset with three variables:
# happiness:      How happy a person is
# like_warriors:  How much a person likes the Golden State Warriors
# winner:         Who won the game (categorical: {cavs, warriors})
#
# Individually, the variables are not significantly related to happiness.
# For example, how happy you are does not depend on how much you like the
# Warriors
# Also, who wins the game {cavs,warrior} does not correlate to whether
# people are happy or not
#
# But, for people who like the Warriors a LOT, if the Warriors win,
# this makes them very happy.
# And, for people who like the Cavs a LOT, if the Warriors win,
# this makes them very sad.
# This is an interaction.
#========================================================================.


# Read file
cavsData = read.csv(file="cavs.csv", header=TRUE, sep=",")


# Main effect: like_warrior
ggplot(cavsData, aes(x=like_warriors, y=happiness)) +
  geom_point() + geom_smooth(method="lm", se=FALSE) + labs(title = "Happiness by how much people like the Golden State Warriors")


# Simple regression: Happiness by like_warrior
mCavs = lm(happiness ~ like_warriors, cavsData)
summary(mCavs)


# Main effect: Happiness by who won the game
ggplot(cavsData, aes(x=winner, y=happiness)) + geom_boxplot() + labs(title = "Happiness by who won the game")


# Single regression: Happiness by who won the game
m = lm(happiness ~ winner, cavsData)
summary(m)


# Interaction of like_warrior and winner
ggplot(cavsData, aes(x=like_warriors, y=happiness, group=winner, color=winner)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)+
  labs(title = "Happiness by how much people like\nthe Golden State Warriors and who won the game", color = "Who won")


# Interaction of like_warrior and winner
cavsData$ggWinner = ""
cavsData$ggWinner[cavsData$winner == "cavs"] = "Winner: Cavaliers"
cavsData$ggWinner[cavsData$winner == "warriors"] = "Winner: Golden State Warriors"
ggplot(cavsData, aes(x=like_warriors, y=happiness)) +
  facet_wrap(~ ggWinner)+
  geom_point() +
  geom_smooth(method="lm", se=FALSE)+
  labs(title = "Happiness by how much people like\nthe Golden State Warriors and who won the game", x = "How much people like the Golden State Warriors", y = "Happiness")


# Linear regression with interaction
m = lm(happiness ~ winner*like_warriors, cavsData)
summary(m)


# This model is the same as: winner*like_Warriors
m = lm(happiness ~ winner + like_warriors + winner:like_warriors, cavsData)
summary(m)


#========================================================================.
# Exercise 2: Duration and POS of most prominent word, and its       ====
#             effect on the intensity of a syllable in English          
#
# http://people.linguistics.mcgill.ca/~morgan/qmld-book/datasets-appendix.html#alternatives
# This data is from a speech production experiment, reported in Wagner (2016),
# examining how information structure affects which words in a sentence are
# pronounced with more emphasis (“prominence”)
#
# When people change the duration of a syllable in English, does
# that change its intensity (volume)? Is this effect different 
# for different parts of speech? {Adj, Noun} (Assume that the data
# is not autocorrelated)
#
# Assume that the residuals have a normal distribution
# and that the the residuals are not autocorrelated.
#========================================================================.


# Load files
alt = read.csv("alternativesMcGillLing620.csv", header=TRUE, sep=",")
alt = subset(alt, !is.na(prominence))


# Make a nice/clearer string for the prominence variable
alt$prominenceString = ""
alt$prominenceString[alt$prominence == "Adjective"] = "Prominent word: Adjective"
alt$prominenceString[alt$prominence == "Noun"] = "Prominent word: Noun"
alt$prominenceString = as.factor(alt$prominenceString)


# model
altCNM = lm(rintensity ~ rduration * prominence, data = alt)
summary(altCNM)


# Chart with facets
ggplot(alt, aes(x=rduration, y=rintensity)) +
  facet_wrap(~prominenceString)+
  geom_point() +
  geom_smooth(method="lm", se=FALSE)+
  labs(title = "Intensity by duration and prominent POS", x = "Duration (z-scored seconds)", y = "Intensity (z-scored decibels)")


# Two lines in one chart
ggplot(alt, aes(x=rduration, y=rintensity, group=prominence, color=prominence)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)+
  labs(title = "Intensity by duration and prominent POS", x = "Duration (z-scored ms)", color="Prominent\nword", y = "Intensity (z-scored decibels)")


# Q-Q Plots for the residuals
par(mfrow=c(1,2))
plot(altCNM$res~altCNM$fit,xlab="Fitted value",ylab="Residuals")
abline(h=0,col="gray")
qqnorm(altCNM$res,main="")
qqline(altCNM$res)
par(mfrow=c(1,1))


# Shapiro-Wilk Test of normality
# Should we assume that they are normal?
shapiro.test(altCNM$residuals)


# Homoscedasticity of variances
ncvTest(altCNM)


# Autocorrelation
durbinWatsonTest(altCNM)


# Collinearity
vif(altCNM)


#========================================================================.
# Example 3: Duration and pitch on the intensity of an English word  ====
#
# # When saying words in English, what is the effect of duration
# and pitch on the intensity (volume) of a syllable?
#
# Assume that the residuals are not correlated.
#========================================================================.


# Read file
alt = read.csv("alternativesMcGillLing620.csv", header=TRUE, sep=",")
alt = subset(alt, !is.na(alt$rduration) & !is.na(alt$rintensity) & !is.na(alt$rpitch))


# model
altM = lm(rintensity ~ rduration * rpitch, data = alt)
summary(altM)


# We could do a 3-D chart, but this is almost
# impossible to interpret.
alt.f = subset(alt, !is.na(alt$rintensity) & !is.na(alt$rpitch) & !is.na(alt$rduration))
xE = alt.f$rpitch
yE = alt.f$rintensity
zE = alt.f$rduration
sd(alt.f$rduration)
temp <- rnorm(524, mean=-0.34, sd=0.64)
plot_ly(x=alt.f$rpitch, y=alt.f$rintensity, z=alt.f$rduration,
        type="scatter3d", mode="markers", color=temp)


# Instead of trying to figure out the effect in 3-D
# let's create a new variable: catDur5. This is
# the duration, divided in five regions (quintiles).
# We will do this so we can analyze the NumxNum 
# interaction more easily.
summary(alt$rduration)
quantile(alt$rduration)
quantile(alt$rduration, prob = seq(0, 1, length = 6))
alt$catDur5[alt$rduration < -0.871] = "Dur00%-20%"
alt$catDur5[alt$rduration >= -0.871 & alt$rduration < -0.541] = "Dur20%-40%"
alt$catDur5[alt$rduration >= -0.541 & alt$rduration < -0.232] = "Dur40%-60%"
alt$catDur5[alt$rduration >= -0.232 & alt$rduration <  0.145] = "Dur60%-80%"
alt$catDur5[alt$rduration >= 0.145] = "Dur80%-100%"


# Two lines in one chart
#
# I think this is the clearest option in this case
# This is the chart of pitch x intensity, and then
# individual lines for 5 types of duration:
#  very low duration (lowest quintile, 00-20)
#       low duration (2nd quintile, 20-40)
#    middle duration (3rd quintile, 40-60)
#      high duration (4th quintile, 60-80)
# very high duration (5th quintile, 80-100)
# 
# It looks like, for words with low duration,
# pitch makes a lot of difference in intensity.
#
# In words with higher duration, does not
# make a big difference in intensity.
ggplot(alt, aes(x=rpitch, y=rintensity, group=catDur5, color=catDur5)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)+
  scale_y_continuous(name = "Intensity")+
  scale_x_continuous(name = "Pitch") +
  ggtitle("Intensity by pitch and duration")


# Another type of chart: scatterplot
# This is good, but I don't think it's the clearest option here
ggplot(alt, aes(x=rpitch, y=rintensity)) +
  facet_wrap(~catDur5)+
  geom_point() +
  geom_smooth(method="lm", se=FALSE)+
  scale_y_continuous(name = "Intensity")+
  scale_x_continuous(name = "Pitch") +
  ggtitle("Duration of Dutch ont- by whether\nthe verb root has a plosive or not")


# By the way, if we made a model with the new
# categorical variable, there is a significant
# difference in duration between words of
# low and high duration.
altM = lm(rintensity ~ rpitch * catDur5, data = alt)
summary(altM)


# Q-Q Plots of the residuals
par(mfrow=c(1,2))
plot(altM$res~altM$fit,xlab="Fitted value",ylab="Residuals")
abline(h=0,col="gray")
qqnorm(altM$res,main="")
qqline(altM$res)
par(mfrow=c(1,1))


# Shapiro-Wilk Test of normality
shapiro.test(altM$residuals)


# Homoscedasticity of variances
ncvTest(altM)


# Autocorrelation
durbinWatsonTest(altM)


# Collinearity
vif(altM)
