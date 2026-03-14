#========================================================================.
# LING10 Stats for Ling, 2026W                                       ====
# Rolando Coto. rolando.a.coto.solano@dartmouth.edu
# Week 8 Code and Exercises (Model Selection, Random Variables)
#========================================================================.


#== Load me =============================================================
library(ggplot2)
library(car)
library(MASS)
library(ggeffects)   # https://cran.r-project.org/web/packages/ggeffects/vignettes/introduction_randomeffects.html
library(lattice)

library(asbio)
library(lsmeans)
library(dplyr)
library(plotly)
library(lmtest)
library(lmerTest)
library(lme4)



#========================================================================.
# Example 1: Incomplete Neutralization in German                     ====
#
# In German, voiced stops become voiceless at the end of  a word. 
#	Rat /ra:t/ > [ra:t] 'council'   Räte  	[re:ta] 'councils'
# Rad /ra:d/ > [ra:t] 'wheel'     Räder 	[re:da] 'wheels'
#
# Because of this change, the word Rad 'wheel' [ra:t] sounds the
# same as the word Rat 'council' [ra:t]. The 't' and 'd' are no
# longer different. This process is called neutralization.
#
# People used to think that the 't' in Rat was identical to the
# 't' in Rad. However, recent research has shown that they are
# different. One of their differences is that the vowels that
# precede them have different durations.
#
# How can we predict the duration of a vowel that precedes
# a neutralized stop in German?
#
# The data is real: Roettger T.B. et al. (2014). Assessing
# incomplete neutralization of final devoicing in German.
# Journal of Phonetics 43(2014): 11-25.
# https://bodowinter.com/pdfs/roettger_2014_final_devoicing_incomplete_neutralization.pdf
# https://osf.io/2j9cx/
#========================================================================.


# Read file and filter so we only use "usable" stimuli
neutral = read.csv("Roettger_et_al_2014_E1_production.csv", header=TRUE, sep=",")
neutral = subset(neutral, usable=="usable")


# Data exploration
aggregate(neutral$item_vowel_dur, list(neutral$voicing), mean)
aggregate(neutral$item_vowel_dur, list(neutral$voicing), sd)


# Boxplot
ggplot(neutral, aes(x = voicing, y = item_vowel_dur)) +
  geom_boxplot()+
  labs(title = "Duration of a vowel that precedes\na neutralized stop in German", x="Coda consonant", y="Duration of the vowel (ms)")


# A linear model with one variable: voicing
# AIC    = 6557
# AdjR2  = 0.22
oneVarNeutral = lm(item_vowel_dur ~ voicing, data = neutral)
summary(oneVarNeutral)
AIC(oneVarNeutral)
summary(oneVarNeutral)$r.squared
summary(oneVarNeutral)$adj.r.squared


# A linear model with six variables: voicing, utterancelength, accent_type, prosodic_boundary, place, subject
# AIC    = 6534
# AdjR2  = 0.25
maxNeutral = lm(item_vowel_dur ~ voicing + utterancelength + accent_type + prosodic_boundary + place + subject, data = neutral)
summary(maxNeutral)
AIC(maxNeutral)
vif(maxNeutral)
summary(maxNeutral)$r.squared
summary(maxNeutral)$adj.r.squared


# Data exploration with five of the six variables below variables
aggregate(neutral$item_vowel_dur, list(neutral$voicing, neutral$accent_type, neutral$prosodic_boundary, neutral$place, neutral$subject), mean)
aggregate(neutral$item_vowel_dur, list(neutral$voicing, neutral$accent_type, neutral$prosodic_boundary, neutral$place, neutral$subject), sd)


# Search for model with minimum AIC
# Forward direction
# Selected model: item_vowel_dur ~ voicing + place
stepAIC(lm(neutral$item_vowel_dur~1),
        neutral$item_vowel_dur ~ neutral$voicing + neutral$utterancelength + neutral$accent_type + neutral$prosodic_boundary + neutral$place + neutral$subject,
        direction="forward")


# Search for model with minimum AIC
# "Both" direction
# Selected model: item_vowel_dur ~ voicing + place
stepAIC(lm(neutral$item_vowel_dur~1),
        neutral$item_vowel_dur ~ neutral$voicing + neutral$utterancelength + neutral$accent_type + neutral$prosodic_boundary + neutral$place + neutral$subject,
        direction="both")


# Search for model with minimum AIC
# Backward direction
# Selected model: item_vowel_dur ~ voicing + place
stepAIC(lm(neutral$item_vowel_dur ~ neutral$voicing + neutral$utterancelength + neutral$accent_type + neutral$prosodic_boundary + neutral$place + neutral$subject),
        neutral$item_vowel_dur~1,
        direction="backward")


# A linear model with two variables: voicing, place
# All of the variables are equally significant, but
# the model has become more parsimonious without 
# losing explanatory power.
# AIC    = 6525
# AdjR2  = 0.25
minNeutral = lm(item_vowel_dur ~ voicing + place, data = neutral)
summary(minNeutral)
AIC(minNeutral)
vif(minNeutral)
summary(maxNeutral)$r.squared
summary(maxNeutral)$adj.r.squared


# Data exploration
aggregate(neutral$item_vowel_dur, list(neutral$place, neutral$voicing), mean)
aggregate(neutral$item_vowel_dur, list(neutral$place, neutral$voicing), sd)


# Boxplot with the two chosen variables
ggplot(neutral, aes(x = place, y = item_vowel_dur, fill=voicing)) +
  geom_boxplot()+
  labs(title = "Duration of a vowel that precedes\na neutralized stop in German", fill="Voicing of\ncoda consonant", y="Duration of the vowel (ms)", x="Place of articulation of the coda consonant")


# Finally, let's do a model with an
# interaction to see if it is significant
minNeutralInterax = lm(item_vowel_dur ~ voicing * place, data = neutral)
summary(minNeutralInterax)
lsmeans(minNeutralInterax, pairwise ~ voicing*place, adjust="tukey")


#========================================================================.
# Exercise 1: English Frequency and variable selection               ====
#
# In English, the reaction time to a word can be determined by its
# word frequency, its length (in letters), and the part of speech
# (adjectives, nouns, verbs).
#
# But, do we really need all three predictors? Run the example and
# tell us what variables we actually need.
#
# SUBTLWF: logarithmical transformation of English Subtitle frequency per million
#
# This exercise is in Levshina pg. 149-153
#========================================================================.


# Load file
ELP = read.csv(file="elp-100.csv", header=TRUE, sep=",")


# Make a categorical variable so we can visualize the data
ELP$LengthRange = "Length:LessThan8"
ELP$LengthRange[ELP$Length == 8 | ELP$Length == 9] = "Length:8or9"
ELP$LengthRange[ELP$Length > 9] = "Length:10OrMore"


# Data exploration with the temporary categories we created
# But the actual test will be with the continuous variable "Range",
# not with LengthRange. LengthRange was made only to make it easier
# to show the variation in the tables.
aggregate(ELP$Mean_RT, list(ELP$POS, ELP$LengthRange), mean)
aggregate(ELP$Mean_RT, list(ELP$POS, ELP$LengthRange), sd)


table(ELP$POS)

# Scatterplot with facets
ggplot(ELP, aes(x = log(SUBTLWF), y = Mean_RT)) +
  facet_wrap(POS ~ LengthRange)+
  geom_point() + geom_smooth(method="lm") +
  labs(title = "Reaction time by frequency,\nword length and part of speech", x ="log(Frequency)", y = "RT (ms)")


# Another type of scatterplot
ggplot(ELP, aes(x = log(SUBTLWF), y = Mean_RT, fill=LengthRange)) +
  facet_wrap(. ~ POS)+
  geom_point() + geom_smooth(method="lm") +
  labs(title = "Reaction time by frequency,\nword length and part of speech", x ="log(Frequency)", y = "RT (ms)")



# A linear model with three variables: length, log(SUBTLWF), POS
# AIC   = 1216
# AdjR2 = 0.49
maxELP = lm(Mean_RT ~ Length + log(SUBTLWF) + POS, data = ELP)
summary(maxELP)
AIC(maxELP)
summary(maxELP)$r.squared
summary(maxELP)$adj.r.squared


# Search for model with minimum AIC
# Forward direction
# Selected model: Mean_RT ~ Length, SUBTLWF
stepAIC(lm(ELP$Mean_RT~1),
        ELP$Mean_RT ~ ELP$Length + log(ELP$SUBTLWF) + ELP$POS,
        direction="forward")


# Search for model with minimum AIC
# "Both" direction
# Selected model: Mean_RT ~ Length, SUBTLWF
stepAIC(lm(ELP$Mean_RT~1),
        ELP$Mean_RT ~ ELP$Length + log(ELP$SUBTLWF) + ELP$POS,
        direction="both")


# Search for model with minimum AIC
# Backward direction
# Selected model: item_vowel_dur ~ Length, SUBTLWF
stepAIC(lm(ELP$Mean_RT ~ ELP$Length + log(ELP$SUBTLWF) + ELP$POS),
        ELP$Mean_RT~1,
        direction="backward")


# A linear model with two variables: length, log(SUBTLWF)
# AIC   = 1214
# AdjR2 = 0.49
minELP = lm(Mean_RT  ~ Length + log(SUBTLWF), data = ELP)
summary(minELP)
AIC(minELP)
summary(minELP)$r.squared
summary(minELP)$adj.r.squared


# Variance assumptions
# We will assume that the set has constant variance
par(mfrow=c(1,2))
plot(minELP$res~minELP$fit,xlab="Fitted value",ylab="Residuals")
abline(h=0,col="gray")
qqnorm(minELP$res,main="")
qqline(minELP$res)
par(mfrow=c(1,1))


# Shapiro-Wilk Test of normality
shapiro.test(minELP$residuals)


# Non-constant variance 
ncvTest(minELP)


# Autocorrelation
durbinWatsonTest(minELP)


# Collinearity
vif(minELP)


# Scatterplot with only the variables in the reduced model
ggplot(ELP, aes(x = log(SUBTLWF), y = Mean_RT, fill=LengthRange)) +
  #facet_wrap(. ~ POS)+
  geom_point() + geom_smooth(method="lm") +
  labs(title = "Reaction time by frequency and word length", x ="log(Frequency)", y = "RT (ms)")


# Don't report this regression. But, this is a question for you
# to think about. Here we have a model with an interaction, and
# the results are very different. Why?
minELPInterax = lm(Mean_RT  ~ Length * log(SUBTLWF), data = ELP)
summary(minELPInterax)


#========================================================================.
# Example 2: Reaction Times in English                               ====
#
# What variables can we use to predict the
# reaction times of nouns in English?
#
# The data is real: Jen Hay (2004). Lexical decision latencies
# elicited from 21 subjects for 79 English concrete nouns,
# with variables linked to subject or word.
# https://cran.r-project.org/web/packages/languageR/
#========================================================================.


# Read file
lex = read.csv(file="lexdec.csv", header=TRUE, sep=",")


# Chart relationship between Frequency and RT
ggplot(lex, aes(x=Frequency, y=RT)) +
  geom_point() +
  geom_smooth(method = lm)+
  labs(title="Reaction Time by Frequency", x ="Log Frequency (per million)", y = "Reaction Time (log ms)")


# Chart relationship between Word length and RT
ggplot(lex, aes(x=Length, y=RT)) +
  geom_point() +
  geom_smooth(method = lm)+
  labs(title="Reaction Time by Word length", x ="Word length (characters)", y = "Reaction Time (log ms)")


# Chart relationship between Frequency, Word Length and RT
# These have the frequency in the x-axis
lex$lenCats = ""
lex$lenCats[lex$Length < 6] = "Length:1-5"
lex$lenCats[lex$Length >= 6 & lex$Length < 8] = "Length:6-7"
lex$lenCats[lex$Length >= 8] = "Length:8-10"
# Chart with three lines
ggplot(lex, aes(x=Frequency, y=RT, color=lenCats)) +
  geom_point() +
  geom_smooth(method = lm)+
  labs(title="Reaction Time by Frequency and Word Length", x ="Log Frequency (per million)", y = "Reaction Time (log ms)", color="Word Length")
# Chart with facets
ggplot(lex, aes(x=Frequency, y=RT)) +
  facet_wrap(lenCats ~ .)+
  geom_point() +
  geom_smooth(method = lm)+
  labs(title="Reaction Time by Frequency and Word Length", x ="Word length (in characters)", y = "Reaction Time (log ms)")


# Another set of charts for Frequency, Word Length and RT
# These have the word length at the x-axis
lex$freqCats = ""
lex$freqCats[lex$Frequency < 3.48] = "LogFreq<3.48"
lex$freqCats[lex$Frequency >= 3.48 & lex$Frequency < 4.75] = "LogFreq<4.75"
lex$freqCats[lex$Frequency >= 4.75] = "LogFreq>=4.75"
# Chart with three lines
ggplot(lex, aes(x=Length, y=RT, color=freqCats)) +
  geom_point() +
  geom_smooth(method = lm)+
  labs(title="Reaction Time by Word Length and Frequency", x ="Word length (Characters)", y = "Reaction Time (log ms)", color="Log Frequency\n(per million)")
# Chart with facets
ggplot(lex, aes(x=Length, y=RT)) +
  facet_wrap(freqCats ~ .)+
  geom_point() +
  geom_smooth(method = lm)+
  labs(title="Reaction Time by Word Length and Frequency", x ="Word length (Characters)", y = "Reaction Time (log ms)")



# Linear model with independent variables
mLex1 <- lm(RT ~ Length + Frequency, lex)
summary(mLex1)
# The coefficients are just the intercept and the slope for Frequency
coefsMLex1 = coef(mLex1)
coefsMLex1


# Predicting values "when other variables are held constant"
predictionLex1 = ggpredict(mLex1, terms=c("Length", "Frequency"), type="random")
predictionLex1
plot(predictionLex1)


# Linear mixed-effects model with independent variables and random effects
mLex2 <- lmer(RT ~ Length + Frequency + (1+Frequency|Word) + (1+Frequency|Subject), lex)
summary(mLex2)


# Coefficients. Each type of Subject and Word (item)
# has its own intercept and slope. They all have the
# same sign, so the direction should be roughly the
# same for all of them.
coefsMLex2 = coef(mLex2)
coefsMLex2$Word
coefsMLex2$Subject


# Visualizing the random effects. This produces two charts,
# one for each random effect.
dotplot(ranef(mLex2,condVar=TRUE))
predictionLex2 = ggpredict(mLex2, terms=c("Length", "Frequency", "Subject"), type="random")
predictionLex2
plot(predictionLex2)


# Chart showing slopes for different subjects
# They are all descending, so the regression
# should be okay.
ggplot(lex, aes(x=Frequency, y=RT, colour = Subject)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm)+
  labs(title="Reaction Time by Frequency and subjects", x ="Log Frequency (per million)", y = "Reaction Time (log ms)")


# Chart showing slopes for different subjects
# They are all descending, so the regression
# should be okay.
ggplot(lex, aes(x=Length, y=RT, colour = Subject)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm)+
  labs(title="Reaction Time by Word Length and subjects", x ="Word Length", y = "Reaction Time (log ms)")


# Bonus code: This regression uses a "maximal" random effects structure.
# Notice how the significance relationships change. Also, notice
# how adding "Length" to the random effects structure doesn't help
# explain a lot of the variation.
mLex3 <- lmer(RT ~ Length + Frequency + (1+Length+Frequency|Word) + (1+Length+Frequency|Subject), lex)
summary(mLex3)


#========================================================================.
# Example 3: Distance from Cape Town                                 ====
#
# Does the phoneme inventory of a language decrease due to
# distance from Cape Town in South Africa? (If so, this 
# would be evidence of a "founder effect" that language
# originates from South Africa).
#
# The data is real: The tones and the geographic coordinates for 
# the languages come from the World Atlas of Linguistic Structures
# WALS (https://wals.info/feature/13A#2/19.3/153.1). I used the
# geographical coordinates to calculate the distance between the
# nucleus of the language and Cape Town, South Africa.
#
# The hypothesis is real: Atkinson (2011) Phonemic Diversity
# supports a serial founder effect model of language expansion
# from Africa. Science 332(6027).
# https://www.science.org/doi/10.1126/science.1199295
#
# Here's some real media coverage of the hypothesis:
# New York Times: Phonetic Clues Hint Language Is Africa-Born
# https://www.nytimes.com/2011/04/15/science/15language.html
#========================================================================.


# Read file
cape = read.csv(file="wals-happy.csv", header=TRUE, sep=",")


# Chart for relationship between phonemic inventory (how many
# phonemes a language has) and the distance between the point
# where the language is spoken and Cape Town in South Africa.
ggplot(cape, aes(x=distanceFromCapeTown, y=inventory)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm)+
  labs(title="Phoneme inventory by distance from Cape Town", x ="Distance (km)", y = "Phonemic Inventory")


# Linear model with distance from Cape Town as
# independent variable and inventory as a
# dependent variable.
mDist1 = lm(inventory ~ distanceFromCapeTown, data=cape)
summary(mDist1)


# Linear mixed-effects model with distance from Cape Town as
# independent variable, inventory as a dependent variable,
# and linguistic family and geographical area as random
# effects, with random intercepts and random slopes for
# distance from Cape Town.
mDist2 = lmer(inventory ~ distanceFromCapeTown + (1+distanceFromCapeTown|family) + (1+distanceFromCapeTown|macroarea), cape, REML="FALSE")
summary(mDist2)


# Predictions for each Macroarea
predictionDist2 = ggpredict(mDist2, terms=c("distanceFromCapeTown","macroarea"), type="random")
predictionDist2
plot(predictionDist2)


# Coefficients for macroarea and family
coefsmDist2 = coef(mDist2)
coefsmDist2$family
coefsmDist2$macroarea


# Chart for the relationship between phonemic inventory and
# the distance to Cape Town with slopes and intercepts for
# each geographical area.
ggplot(cape, aes(x=distanceFromCapeTown, y=inventory, colour=macroarea)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm)+
  labs(title="Phoneme inventory by distance from Cape Town", x ="Distance (km)", y = "Phonemic Inventory")


#========================================================================.
# Example 4: Storks                                                  ====
#
# Countries with more storks have more babies.
# 
# The data is real: Matthews, R. (2000). Storks deliver babies
# (p= 0.008). Teaching Statistics, 22(2), 36-38.
# https://onlinelibrary.wiley.com/doi/abs/10.1111/1467-9639.00013
#========================================================================.

# Read file
storks <- read.csv(file="storks.csv", header=TRUE, sep=",")

# Linear model of birthrate (in thousands of children per year) versus storks (pairs)
mS = lm(birthrate ~ storks, data=storks)
summary(mS)

# Linear model of storks (pairs) versus area (km2)
mS = lm(storks ~ area, data=storks)
summary(mS)

# Linear model of birthrate (in thousands of children per year) versus area (km2)
mS = lm(birthrate ~ area, data=storks)
summary(mS)

# Linear model of birthrate (in thousands of children per year) versus population (in millions of people)
mS = lm(birthrate ~ humans, data=storks)
summary(mS)


#========================================================================.
# Example 5: Tones and rainfall                                      ====
# 
# Are tonal language more likely to exist
# in places with heavy rainfall?
#
# The data is real: The tones and the geographic coordinates for 
# the languages come from the World Atlas of Linguistic Structures
# WALS (https://wals.info/feature/13A#2/19.3/153.1). The data for
# rainfall (mm per year) comes from the World Bank
# (https://data.worldbank.org/indicator/AG.LND.PRCP.MM).
#========================================================================.


# Read file and filter out any empty rows
wals = read.csv(file="happy-tones.csv", header=TRUE, sep=",")
tempW = subset(wals,  tones != "")
tempW = subset(tempW, !is.na(rainfall))
tempW = subset(tempW, name != "Waimaha")  # I'll get rid of just one datapoint. What can possibly go wrong?


# Chart for the relationship between the rainfall in
# a country and the number of tones its languages.
ggplot(tempW, aes(x=rainfall, y=tones)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm)+
  labs(title="Tones by rainfall where they are spoken", x ="Rainfall (mm per year)", y = "Tones in the language")


# Linear model with number of tones in a language as the 
# independent variable and freedom of the press as the 
# dependent variable.
mRain1 = lm(tones ~ rainfall, data=tempW)
summary(mRain1)


# Predictions for tones and freedom of the press
predictionRain1 = ggpredict(mRain1, terms=c("rainfall"), type="random")
predictionRain1
plot(predictionRain1)


# Linear mixed-effects model with number of tones in
# a language as the independent variable, freedom of
# the press as the dependent variable, and a random
# intercept for the random effects of language
# family and geographical area.
mRain2 = lmer(tones ~ rainfall + (1|family) + (1|macroarea), tempW, REML="FALSE")
summary(mRain2)


# Coefficients for macroarea and family
coefsmRain2 = coef(mRain2)
coefsmRain2$family
coefsmRain2$macroarea


# Chart for the relationship between the rainfall in
# a country and the number of tones its languages,
# taking the macroarea into account
ggplot(tempW, aes(x=rainfall, y=tones, color=macroarea)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm)+
  labs(title="Tones by rainfall where they are\nspoken separated by macroarea", x ="Rainfall (mm per year)", y = "Tones in the language")


# Also, wait... did this even meet the assumptions of LMs?
fittedRain2 = fitted(mRain2)
residsRain2 = resid(mRain2)

# Shapiro-Wilk Test of normality
hist(wals$tones)
shapiro.test(wals$tones)
hist(residsRain2)
shapiro.test(residsRain2)

# Assumptions: Normality and homoscedasticity
par(mfrow=c(1,2))
plot(residsRain2~fittedRain2,xlab="Fitted value",ylab="Residuals")
abline(h=0,col="gray")
qqnorm(residsRain2,main="")
qqline(residsRain2)
par(mfrow=c(1,1))



#========================================================================.
# Example 6: Tones and forest coverage                               ====
#
# Does the percentage of forest coverage in a country
# depend on how many tones there are in the languages
# of the country?
#
# The data is real: The tones and the geographic coordinates for 
# the languages come from the World Atlas of Linguistic Structures
# WALS (https://wals.info/feature/13A#2/19.3/153.1). The data for
# the percentage of forest cover comes from the World Bank
# (https://data.worldbank.org/indicator/AG.LND.FRST.ZS?view=chart).
#========================================================================.


# Read file
wals = read.csv(file="happy-tones.csv", header=TRUE, sep=",")
tempW = subset(wals, tones != "")


# Chart for relationship between how many tones a language has
# and percentage of forest in the country where the language is spoken
ggplot(tempW, aes(x=tones, y=percentForest)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm)+
  labs(title="Percentage of forest in the country\nby tones in its languages", x ="Tones", y = "% Forest in the country")


# Linear model with number of tones as independent variable
# and percentage of forest in the country where the language
# is spoken as dependent variable.
mForest1 = lm(percentForest ~ tones, data=tempW)
summary(mForest1)


# Predictions of %forest coverage by number of tones
predictionForest1 = ggpredict(mForest1, terms=c("tones"), type="random")
predictionForest1
plot(predictionForest1)


# Linear-mixed effects model with number of tones as
# independent variable, percentage of forest in the
# country where the language is spoken as dependent
# variable, and language family and geographical
# area as random effects (with a random intercept).
mForest2 = lmer(percentForest ~ tones + (1|family) + (1|macroarea), data=tempW, REML="FALSE")
summary(mForest2)
dotplot(ranef(mForest2,condVar=TRUE))


# Predictions for %forest coverage,
# taking into account the macroarea.
predictionForest2 = ggpredict(mForest2, terms=c("tones", "macroarea"), type="random")
predictionForest2
plot(predictionForest2)


# Coefficients for macroarea and family
coefsmForest2 = coef(mForest2)
coefsmForest2$family
coefsmForest2$macroarea


# Linear-mixed effects model with number of tones as
# independent variable, percentage of forest in the
# country where the language is spoken as dependent
# variable, and language family and geographical
# area as random effects (with a random intercept
# and a random slope for tones).
mForest3 = lmer(percentForest ~ tones + (1+tones|family) + (1+tones|macroarea), data=tempW, REML="FALSE")
summary(mForest3)
dotplot(ranef(mForest3,condVar=TRUE))


# Predictions of %forest coverage, taking
# into account the macroarea and the
# tones in each family
predictionForest3 = ggpredict(mForest3, terms=c("tones", "macroarea"), type="random")
predictionForest3
plot(predictionForest3)


# Coefficients for macroarea and family
coefsmForest3 = coef(mForest3)
coefsmForest3$family
coefsmForest3$macroarea


# Coefficients for language family and geographical
# area. Check whether all of the random coefficients
# have the same sign. If they are different, then
# languages are going in two different directions.
ct = coef(mForest3)
ct$family
ct$macroarea
u = as.data.frame(ct$family)


# Chart with tone, percent of forest, and
# individual slopes for geographical area.
ggplot(tempW, aes(x=tones, y=percentForest, colour = macroarea)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm)+
  labs(title="Percentage of forest in the country by tones\nin its languages and area of the world", x ="Tones", y = "% Forest in the country")


#========================================================================.
# Example 7: Tones and freedom of the press                          ====
#
# Does the freedom of the press in a country depend on
# how many tones there are in the languages of the country?
#
# The data is real: The tones and the geographic coordinates for 
# the languages come from the World Atlas of Linguistic Structures
# WALS (https://wals.info/feature/13A#2/19.3/153.1). The data for
# freedom of the press comes from Reporters Without Borders
# (smaller values are better) (https://rsf.org/en/ranking_table). 
#========================================================================.

# Read file and filter out any empty rows
wals = read.csv(file="happy-tones.csv", header=TRUE, sep=",")
tempW = subset(wals,  tones != "")
tempW = subset(tempW, !is.na(freedomPress))


# Chart for the relationship between the number of tones in a language and
# the freedom of press in the country where the language is spoken.
ggplot(tempW, aes(x=tones, y=freedomPress)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm)+
  labs(title="Freedom of the press in a country\nby tones in its languages", x ="Tones", y = "Score of freedom of the press")


# Linear model with number of tones in a language as the 
# independent variable and freedom of the press as the 
# dependent variable.
mFP1 = lm(freedomPress ~ tones, data=wals)
summary(mFP1)


# Predictions for tones and freedom of the press
predictionFP1 = ggpredict(mFP1, terms=c("tones"), type="random")
predictionFP1
plot(predictionFP1)


# Linear mixed-effects model with number of tones in
# a language as the independent variable, freedom of
# the press as the dependent variable, and a random
# intercept for the random effects of language
# family and geographical area.
mFP2 = lmer(freedomPress ~ tones + (1|family) + (1|macroarea), wals, REML="FALSE")
summary(mFP2)


# Predictions for tones and freedom of the press,
# taking macroarea and family into account
dotplot(ranef(mFP2,condVar=TRUE))
predictionFP2 = ggpredict(mFP2, terms=c("tones","macroarea"), type="random")
predictionFP2
plot(predictionFP2)


# Coefficients for macroarea and family
coefsmFP2 = coef(mFP2)
coefsmFP2$family
coefsmFP2$macroarea


# Linear mixed-effects model with number of tones in
# a language as the independent variable, freedom of
# the press as the dependent variable, and a random
# intercept and slope (for tone) for the random effects
# of language family and geographical area.
mFP3 = lmer(freedomPress ~ tones + (1+tones|family) + (1+tones|macroarea), wals, REML="FALSE")
summary(mFP3)


# Predictions for tones and freedom of the press,
# taking into account the effect of tone in
# macroarea and family
dotplot(ranef(mFP3,condVar=TRUE))
predictionFP3 = ggpredict(mFP3, terms=c("tones","macroarea"), type="random")
predictionFP3
plot(predictionFP3)


# Coefficients for macroarea and family
coefsmFP3 = coef(mFP3)
coefsmFP3$family
coefsmFP3$macroarea


# Chart for the relationship between tones and 
# freedom of the press for each geographical area.
ggplot(tempW, aes(x=tones, y=freedomPress, colour = macroarea)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm)+
  labs(title="Freedom of the press in a country by tones\nin its languages and area of the world", x ="Tones", y = "Score of freedom of the press")
