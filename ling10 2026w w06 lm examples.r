#========================================================================.
# LING10 Stats for Ling, 2026W                                       ====
# Rolando Coto. rolando.a.coto.solano@dartmouth.edu
# Week 6 Code and Exercises (Linear models)
#========================================================================.


#== Load me =============================================================
library(ggplot2)
library(lmtest)
library(dplyr)
library(lsmeans)
library(car)
library(asbio)


#========================================================================.
# Example 1: English Frequency and Reaction Time                     ====
#
# Is reaction time correlated to word frequency in English?
#
# The data is real: Dataset by Balota et al. (2007);
# appears in Levshina (2015)
#========================================================================.


# Read file
elp = read.csv(file="elp_200.csv", header=TRUE, sep=",")


# Example words

subset(elp, Word == "few")
exp(12.486)
log(264606.7)

subset(elp, Word == "rundown")
exp(6.55)

subset(elp, Word == "fossilize")
exp(2.944)


# Scatterplot

ggplot(elp, aes(x=Log_Freq_HAL, y=I_Mean_RT))+
  geom_point()+
  geom_smooth(method=lm) +
  labs(title="Reaction time versus frequency",
       x ="log(Freq) (words per million)", y = "Reaction time (ms)")


# Linear regression
elpFreqModel = lm(I_Mean_RT  ~ Log_Freq_HAL, data = elp)
summary(elpFreqModel)
elpFreqModel


# R-squared
summary(elpFreqModel)$r.squared
summary(elpFreqModel)$adj.r.squared
sqrt(summary(elpFreqModel)$r.squared)


# Shapiro-Wilk Test of normality
# If the test is significant, then the distribution is
# significantly DIFFERENT from a normal distribution
shapiro.test(elpFreqModel$residuals)


# Assumption: Variance
par(mfrow=c(1,2))
plot(elpFreqModel$res~elpFreqModel$fit,xlab="Fitted value",ylab="Residuals")
abline(h=0,col="gray")
qqnorm(elpFreqModel$res,main="")
qqline(elpFreqModel$res)
par(mfrow=c(1,1))


# Assumption: Levene's test for homoscedasticity (similar variances)
ncvTest(elpFreqModel)


# Assumption: Autocorrelation
durbinWatsonTest(elpFreqModel)


# Equation and predictions
elpFreqModel
elp$predicted = predict(elpFreqModel)
elp$residuals = residuals(elpFreqModel)
subset(elp, Word == "few")
subset(elp, Word == "rundown")
subset(elp, Word == "fossilize")



# Fancy chart with the residuals, in case you ever need it
ggplot(elp, aes(x = Log_Freq_HAL, y = I_Mean_RT)) +
  geom_smooth(method = "lm", se = FALSE) +  # Plot regression slope
  geom_segment(aes(xend = Log_Freq_HAL, yend = predicted), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = predicted), shape = 1)+
  labs(title="Reaction time versus frequency",
       x ="log(Freq) (words per million)", y = "Reaction time (ms)")




#========================================================================.
# Exercise 1A: English frequency by familiarity for animal words     ====
#
# In theory, familiarity is a proxy for frequency. These two values
# should be correlated. Are familiarity and frequency correlated for
# the animal words in the English animals dataset?
#
# Source: Jen Hay 2004: https://rdrr.io/cran/languageR/man/ratings.html
#========================================================================.


# Read the file
animal = read.csv(file="animal-ratings.csv", header=TRUE, sep=",")


# Make a scatterplot
ggplot(animal, aes(x=meanFamiliarity, y=Frequency))+
  geom_point()+
  geom_smooth(method=lm) +
  labs(title="Familiarity by Frequency for\nanimal words in English",
       y ="Frequency", x = "Familiarity")


# Linear regression model
anModel = lm(Frequency  ~ meanFamiliarity, data = animal)
summary(anModel)


# R-squared: Effect Size
summary(anModel)$r.squared
summary(anModel)$adj.r.squared


# Visual verification of residuals
par(mfrow=c(1,2))
plot(anModel$res~anModel$fit,xlab="Fitted value",ylab="Residuals")
abline(h=0,col="gray")
qqnorm(anModel$res,main="")
qqline(anModel$res)
par(mfrow=c(1,1))


# Shapiro-Wilk Test of normality of residuals
shapiro.test(anModel$residuals)


# Test of constant variance of residuals
ncvTest(anModel)


# Test of autocorrelation
durbinWatsonTest(anModel)


# Fancy chart with the residuals, in case you ever need it
animal$predicted = predict(anModel)
animal$residuals = residuals(anModel)
ggplot(animal, aes(x = Frequency, y = meanFamiliarity)) +
  geom_smooth(method = "lm", se = FALSE) +  # Plot regression slope
  geom_segment(aes(xend = Frequency, yend = predicted), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = predicted), shape = 1)+
  labs(title="Familiarity by Frequency for\nanimal words in English",
       y ="Familiarity", x = "Frequency")



#========================================================================.
# Exercise 1B: Valence and Dominance                                 ====
#
# In line with Osgood, Suci, and Tannenbaum's (1957) theory
# of emotions:
#
# Valence: the pleasantness of a stimulus, the emotions invoked
# by a word, going from unhappy to happy.
#
# Dominance: the degree of control exerted by a stimulus, the
# extent to which the word denotes something that is
# weak/submissive or strong/dominant
#
# The dataset valence-200.csv has information on the affective
# ratings of English words. Is there a correlation between
# valence (xVar = V.Mean.Sum) and dominance (yVar = D.Mean.Sum)?
#
# This is real data: Warriner, A.B., Kuperman, V., & Brysbaert, M.
# (2013). Norms of valence, arousal, and dominance for 13,915
# English lemmas. Behavior Research Methods, 45, 1191-1207.
# http://crr.ugent.be/archives/1003
# https://link.springer.com/article/10.3758/s13428-012-0314-x
#========================================================================.


# Read file
val = read.csv(file="valence-200.csv", header=TRUE, sep=",")


# Exploring some data points
tempVal = head(data.frame(val$Word, val$V.Mean.Sum, val$D.Mean.Sum),15)
tempVal


# Scatterplot
ggplot(val, aes(x=V.Mean.Sum, y=D.Mean.Sum))+
  geom_point(shape=1)+
  geom_smooth(method=lm) +
  labs(title = "Dominancy by Valence in English", x = "Valence", y= "Dominance")


# Linear model
valModel = lm(D.Mean.Sum  ~ V.Mean.Sum, data = val)
summary(valModel)
valModel


# R-squared
summary(valModel)$r.squared
summary(valModel)$adj.r.squared


# Shapiro-Wilk Test of normality
hist(valModel$residuals)
shapiro.test(valModel$residuals)


# Assumption: Normality of the residuals
par(mfrow=c(1,2))
plot(valModel$res~valModel$fit,xlab="Fitted value",ylab="Residuals")
abline(h=0,col="gray")
qqnorm(valModel$res,main="")
qqline(valModel$res)
par(mfrow=c(1,1))


# Assumption: Homoscedasticity of Variances
ncvTest(valModel)


# Autocorrelation
durbinWatsonTest(valModel)


#========================================================================.
# Example 2: POS and RT                                              ====
#
# In English, is the reaction time different for different 
# parts of speech? (This data is simulated).
#========================================================================.


# Read the file
fpos = read.csv(file="fpos.csv", header=TRUE, sep=",")
fpos$pos = as.factor(fpos$pos)


# Data exploration
aggregate(fpos$rt, list(fpos$pos), mean)
aggregate(fpos$rt, list(fpos$pos), sd)


# Boxplot
ggplot(fpos, aes(x = pos, y = rt)) +
  geom_boxplot()+
  scale_y_continuous(name = "Reaction time (ms)")+
  scale_x_discrete(name = "Part of Speech") +
  ggtitle("Part of speech and reaction time\n(simulated data)")


# Model with "noun" as reference level
fpos$pos <- relevel(fpos$pos, ref="noun")
fposM = lm(rt  ~ pos, data = fpos)
summary(fposM)


# Model with "verb" as reference level
fpos$pos <- relevel(fpos$pos, ref="verb")
fposM = lm(rt  ~ pos, data = fpos)
summary(fposM)


# Shapiro-Wilk Test of normality
shapiro.test(fposM$residuals)


# Assumption: Homoscedasticity (similar variances)
par(mfrow=c(1,2))
plot(fposM$res~fposM$fit,xlab="Fitted value",ylab="Residuals")
abline(h=0,col="gray")
qqnorm(fposM$res,main="")
qqline(fposM$res)
par(mfrow=c(1,1))


# Assumption: Homoscedasticity (similar variances)
ncvTest(fposM)


# Assumption: Autocorrelation
durbinWatsonTest(fposM)


# R-squared
summary(fposM)$r.squared
summary(fposM)$adj.r.squared


#========================================================================.
# Exercise 2: Germanic syntax                                        ====
#
# Western Germanic languages have a dialect continuum where local
# villages change progressively between German and Dutch. The data
# in german-modals-20.csv has 3 villages that share the same dialect,
# but one is in Germany, one in the Netherlands, and one in Dutch-speaking
# Belgium.  In both the Netherlands and Belgium the standard language is
# Dutch (different varieties), but in Germany it's German.  The order of
# modal verb and main verb in subordinate clauses differs between
# Standard Dutch and Standard German:  
#
# Dutch: 	Ik denk  dat  ik kan gaan.        Modal first
#         I  think that I  can  go.
# German: Ich denke da?  ich gehen kann.    Modal second
#         I   think that I   go    can.
#
# Based on historical data, it is known that the local dialect, before
# the standard languages began to have any influence in the area, was
# variable on this point.  It allowed either order but favored the order
# Dutch uses. We have data from 20 speakers in each village and counted
# the percentage of times each speaker used the modal-before-main-verb
# word order (hereafter, "modal-first order").  Therefore, the dependent
# variable is % modal-first order used, and the independent variable is 
# country (what country the village is located in). There are three hypotheses:
# 
# (i)	  The percentage of modal-first order will differ across dialects.
# (ii)  Speakers in the Netherlands will use modal-first order more often
#       than speakers in Belgium because the standard language has a
#       stronger influence in the Netherlands than in Belgium.
# (iii)	Speakers in the German village will use modal-first order less
#       often than speakers in the other two villages because of the
#       influence of German.
#
# What does the data say about these hypotheses?
# 
# THIS DATA IS SIMULATED, but the variation is real. It is based on:
# Gerritsen, Marinel (1999). Divergence of Dialects in a Linguistic
# Laboratory near the Belgian-Dutch-German Border: Similar Dialects
# under the Influence of Different Standard Languages.  Language
# Variation and Change, 11:43-65.
#========================================================================.


# Load data
ge = read.csv(file="german-modals-20.csv", header=TRUE, sep=",")
ge$country = as.factor(ge$country)
ge$country <- relevel(ge$country, ref="nl")

# Plot the data
ggplot(ge, aes(x=country, y=perc))+
  geom_boxplot()+
  scale_y_continuous(name = "Percent of Modal-Verb Constructions")+
  scale_x_discrete(name = "Country") +
  ggtitle("Percent of Modal-Verb constructions in Germanic Languages in 3 countries")


# Make linear model
geModel = lm(perc  ~ country, data = ge)
summary(geModel)


# R-squared
summary(geModel)$r.squared
summary(geModel)$adj.r.squared


# If you have more than two variables, you can
# perform a post-hoc test. Is there a significant
# difference between the village in Germany
# and the village in the Netherlands?
lsmeans(geModel, pairwise ~ country, adjust="tukey")


# Shapiro-Wilk Test of normality
shapiro.test(geModel$residuals)
ge$perc


geModel$fit

# Check the residuals assumptions
par(mfrow=c(1,2))
plot(geModel$res~geModel$fit,xlab="Fitted value",ylab="Residuals")
abline(h=0,col="gray")
qqnorm(geModel$res,main="")
qqline(geModel$res)
par(mfrow=c(1,1))


# variances
ncvTest(geModel)


# Autocorrelation
durbinWatsonTest(geModel)


#========================================================================.
# Example 3: ELP                                                     ====
#
# Are frequency and length correlated to the reaction time
# of a word in English?
#
# The data is real: Dataset by Balota et al. (2007);
# appears in Levshina (2015)
#========================================================================.


# Load file
elpM = read.csv(file="elp_200.csv", header=TRUE, sep=",")


# Chart 1 (RT-Freq)
ggplot(elpM, aes(x=Log_Freq_HAL, y=I_Mean_RT))+
  geom_point()+
  geom_smooth(method = "lm") +
  labs(title="Reaction time versus frequency in English",
       x ="log(Freq) (words per million)", y = "Reaction time (ms)")


# Chart 2 (RT-Len)
ggplot(elpM, aes(x=Length, y=I_Mean_RT))+
  geom_point()+
  geom_smooth(method = "lm") +
  labs(title="Reaction time versus word length in English",
       x ="Word length (letters)", y = "Reaction time (ms)")


# Chart 3 (3D: RT - Freq+Len)
library(plotly)
xE = elpM$Log_Freq_HAL
yE = as.numeric(elpM$Length)
zE = elpM$I_Mean_RT
temp <- rnorm(200, mean=30, sd=5)
plot_ly(x=elpM$Log_Freq_HAL, y=elpM$Length, z=elpM$I_Mean_RT,
        type="scatter3d", mode="markers", 
        trendline="ols", color=temp)


# Linear model
elpMultModel = lm(I_Mean_RT  ~ Log_Freq_HAL + Length, data = elpM)
summary(elpMultModel)
elpMultModel


# R-squared
summary(elpMultModel)$r.squared
summary(elpMultModel)$adj.r.squared
sqrt(summary(elpMultModel)$r.squared)


# Checking variance assumptions
par(mfrow=c(1,2))
plot(elpMultModel$res~elpMultModel$fit,xlab="Fitted value",ylab="Residuals")
abline(h=0,col="gray")
qqnorm(elpMultModel$res,main="")
qqline(elpMultModel$res)
par(mfrow=c(1,1))


# Shapiro-Wilk Test of normality for the residuals
shapiro.test(elpMultModel$residuals)


# Non-constant variance test
ncvTest(elpMultModel)


# Autocorrelation
durbinWatsonTest(elpMultModel)


# Collinearity plot
ggplot(elpM, aes(x=Log_Freq_HAL, y=Length))+
  geom_point()+
  geom_smooth(method = "lm") +
  labs(title="Two predictors",
       x ="log(Freq) (words per million)", y = "Word length")


# Collinearity visualizations
pairs(~ Log_Freq_HAL + Length, data=elpM, cex.labels=2.5, cex=1.5,
      lower.panel=panel.lm, upper.panel=panel.cor.res)


# Collinearity test
vif(elpMultModel)


#========================================================================.
# Exercise 3: RT in English                                          ====
#
# elpBig$SUBTLWF # Frequency in a movie subtitle corpus
# elpBig$Length  # Word Length
# elpBig$POS     # Part of speech
#
# Is the reaction time correlated with the word frequency,
# the word length and the parts of speech?
#
# The data is real: Dataset by Balota et al. (2007);
# appears in Levshina (2015)
#========================================================================.


# Read file
elpBig = read.csv(file="elp.csv", header=TRUE, sep=",")


# model
elpBigModel = lm(Mean_RT ~ Length + log(SUBTLWF) + POS, data = elpBig)
summary(elpBigModel)


# Chart 1 (RT-Freq)
ggplot(elpBig, aes(x=log(SUBTLWF), y=Mean_RT))+
  geom_point()+
  geom_smooth(method = "lm") +
  labs(title="Reaction time versus frequency in English Movie Subtitles",
       x ="log(Freq) (words per million)", y = "Reaction time (ms)")


# Chart 2 (RT-Length)
ggplot(elpBig, aes(x=Length, y=Mean_RT))+
  geom_point()+
  geom_smooth(method = "lm") +
  labs(title="Reaction time versus frequency in English Movie Subtitles",
       x ="Word length (letters)", y = "Reaction time (ms)")


# Chart 3 (RT-POS+)
# JJ=Adjective, NN=Noun, VB=Verb
ggplot(elpBig, aes(x = POS, y = Mean_RT)) +
  geom_boxplot()+
  scale_y_continuous(name = "Reaction time (ms)")+
  scale_x_discrete(name = "Part of speech") +
  ggtitle("Reaction time versus frequency in English Movie Subtitles")


# R-squared
summary(elpBigModel)$r.squared
summary(elpBigModel)$adj.r.squared
sqrt(summary(elpBigModel)$r.squared)


# Variance assumptions (linearity and homoscedasticity)
par(mfrow=c(1,2))
plot(elpBigModel$res~elpBigModel$fit,xlab="Fitted value",ylab="Residuals")
abline(h=0,col="gray")
qqnorm(elpBigModel$res,main="")
qqline(elpBigModel$res)
par(mfrow=c(1,1))


# Shapiro-Wilk Test of normality
shapiro.test(elpBigModel$residuals)


# Non-constant variance 
ncvTest(elpBigModel)


# Autocorrelation
durbinWatsonTest(elpBigModel)


# Collinearity in chart
pairs(~ Length + log(SUBTLWF) + as.factor(POS), data=elpBig, cex.labels=2.5, cex=1.5,
      lower.panel=panel.lm, upper.panel=panel.cor.res)


# Collinearity
vif(elpBigModel)

