#========================================================================.
# LING10 Stats for Ling, 2026W                                       ====
# Rolando Coto. rolando.a.coto.solano@dartmouth.edu
# Week 9 Code and Exercises (Logistic Regression)
#========================================================================.


#== Load me =============================================================
library(ggplot2)
library(survival)
library(emmeans)


#== Example A1: English Tapping ====================================
# Source: Kilbourn-Ceron, O., Wagner, M., and Clayards, M. (2017).
# The effect of production planning locality on external sandhi:
# A study in /t/. In Proceedings of the 52nd Annual Meeting of the
# Chicago Linguistic Society, pages 313–326.
# http://people.linguistics.mcgill.ca/~morgan/qmld-book/datasets-appendix.html#north-american-english-tapping
#
# In North American English, the sounds [t] and [d] can sometimes be
# optionally pronounced as a tap if followed by a vowel:
#    For those of you who'd like to ea[t] early, lunch will be served.
#    For those of you who'd like to ea[r] early, lunch will be served.
#
# The dataset in englishTap.csv is from Kilbourn-Ceron et al. (2017), who
# performed an experiment to test whether speech rate affected tapping.
# 23 students from McGill University Participants were asked to produce
# sentences like these:
#    If you plit Alice, John will be mad.
#
# The sentences non-words like plit ending in [t] or [d], followed by a vowel
# initial word (such as "Alice"). The speakers were asked to speak at two
# different rates: normal versus fast. This is in the variable speechrate.
#
# Are the speaking rates different in how much people tap?
#========================================================================.


# Load file
tap <- read.csv(file="englishTap.csv", header=TRUE, sep=",")
tap$tapped = as.factor(tap$tapped)
tap$speechrate = as.factor(tap$speechrate)


# Chart
ggplot(tap, aes(x=speechrate,fill=tapped))+
  geom_bar(position='dodge')+
  ggtitle("Bar chart for tapped /t/ by speech rate")+
  geom_text(stat='count', aes(label=..count..), vjust=1.4, color="black",
            position = position_dodge(0.9), size=3.5)


# Levels
#------------------------------------------.
#              |  Reference  Non-reference
#------------------------------------------.
# Tapped       |  NotTapped  YesTapped
# Speech Rate  |  Fast       Slow
#------------------------------------------.
levels(tap$tapped)
levels(tap$speechrate)


# Logit model
mTap = glm(tapped ~ speechrate, data=tap, family=binomial)
summary(mTap)
concordance(mTap)


# Predictions for the probability of
# tapping according to speech rate

newdata = rbind( c("fast"),
                 c("slow") )
newdata = as.data.frame(newdata)
colnames(newdata) = c("speechrate")
newdata$speechrate = as.factor(newdata$speechrate)

probsTapped = predict(mTap, newdata, type="response", SE.fit="TRUE")
probsTapped
probsNonTapped = 1-probsTapped
probsNonTapped


#== Example A2: Warlpiri Ergative =====================================
#
# The dataset warlpiri.csv documents the use of ergative case marking
# in the narratives of native speakers of Lajamanu Warlpiri describing
# events in picture books. There are 8 children and 13 adults in the set
# (AgeGroup).
#
# The variable CaseMarking contains data on how often the speaker
# produced the ergative case versus other cases. Are the two
# generations different in how they use the ergative case? Is the
# use of the ergative marker related to the animacy of the subject?
#
# The data is real: O'Shannessy, C. (2006) Language contact and child
# bilingual acquisition: Learning a mixed language and Warlpiri in
# Northern Australia, PhD Thesis, University of Sydney, Australia.
# https://www.rdocumentation.org/packages/languageR/versions/1.5.0/topics/warlpiri
#
# This data set documents the use of ergative case marking in the narratives of native speakers of
# Lajamanu Warlpiri (8 children, 13 adults) describing events in picture books.
#========================================================================.


# Load files
w <- read.csv(file="warlpiri.csv", header=TRUE, sep=",")
w$CaseMarking = as.factor(w$CaseMarking)
w$AgeGroup = as.factor(w$AgeGroup)
w$AnimacyOfSubject = as.factor(w$AnimacyOfSubject)


# Reference levels
levels(w$CaseMarking)
levels(w$AgeGroup)
levels(w$AnimacyOfSubject)


# Barchart plot
ggplot(w, aes(x=AnimacyOfSubject,fill=CaseMarking))+
  facet_wrap(AgeGroup ~ .)+
  geom_bar(position='dodge')+
  ggtitle("Case marking by animacy of subject in Warlpiri")+
  geom_text(stat='count', aes(label=..count..), vjust=1.4, color="white",
            position = position_dodge(0.9), size=3.5)


# Generalized Linear model
mW = glm(CaseMarking ~ AgeGroup * AnimacyOfSubject, data=w, family=binomial)
summary(mW)
concordance(mW)



# How many items are there in each combination
t = table( w$AgeGroup, w$AnimacyOfSubject)
t
t = table(w$AnimacyOfSubject, w$AgeGroup, w$CaseMarking)
t


# =========================================================.
# Probabilities
#
# Adults-AnimateSubj OtherCase:  8.5%
# Adults-AnimateSubj Ergative:   91.5%
#
# Adult-InanimateSubj OtherCase: 26.5%
# Adult-InanimateSubj Ergative:  73.5%
#
# Child-AnimateSubj OtherCase:   20%
# Child-AnimateSubj Ergative:    80%
#
# Child-InanimateSubj OtherCase: 23%
# Child-InanimateSubj Ergative:  77%
# =========================================================.


newdata = rbind( c("adult","animate"),    # First combination
                 c("adult","inanimate"),  # Second combination
                 c("child","animate"),    # Third combination
                 c("child","inanimate") ) # Fourth combination
newdata = as.data.frame(newdata)
colnames(newdata) = c("AgeGroup", "AnimacyOfSubject")

probOtherCases = predict(mW, newdata, type="response")
probOtherCases
probErgative = 1-probOtherCases
probErgative


# Post-hoc analysis
pairsWarlpiri = emmeans(mW, ~ AgeGroup + AnimacyOfSubject)
pairs(pairsWarlpiri)


#== Exercise A3: Dutch Causatives ====================================
# Dutch has two different verbs to make causative constructions:
#
# Hij DEED me denken aan mijn vader.
# He  did  me think  at  my   father
# "He reminded me of my father."
#
# Ik LIET hem mijn  huis schilderen.
# I  let  him paint my   house.
# "I had him paint my house."
#
# Dit  DOET denken aan de  onzalige tijden van de  junta van de  partijvoorzitters.
# This does think  at  the wretched times  of  the junta of  the party-chairmen
# "This reminds of the wretched times of the junta of party chairmen"
#
# In the Flemish-speaking regions of Belgium these two verbs are both
# very frequent, but in the Netherlands laten has become the more
# frequent verb.
#
# Is there a significant difference in the use of these verbs between
# the two countries?
#
# The data is real: Levshina, Natasha (2011). Doe wat je niet laten
# kan: A usage-based analysis of Dutch causative constructions. Ph.D.
# Dissertation, Katholieke Universiteit Leuven.
# https://lirias.kuleuven.be/1821217?limo=0
# The data is in your textbook (Levshina pg. 254)
#========================================================================.


# Read the files
doenLaten = read.csv(file="doenLaten.csv", header=TRUE, sep=",")
doenLaten$Aux = as.factor(doenLaten$Aux)
doenLaten$Country = as.factor(doenLaten$Country)


# The reference level for the variable Aux is "doen"
# because it's the first one alphabetically
levels(doenLaten$Aux)


# The reference level for the variable Country is "Belgium"
# because it's the first one alphabetically
levels(doenLaten$Country)


# Table
t1 = table(doenLaten$Aux, doenLaten$Country)
prop.table(t1, 2)


# Chart
ggplot(doenLaten, aes(x=Country,fill=Aux))+
  geom_bar(position='dodge')+
  labs(title = "Causative auxiliary in Dutch/Flemish", fill="Auxiliary", y="Count")+
  geom_text(stat='count', aes(label=..count..), vjust=1.4, color="black",
            position = position_dodge(0.9), size=3.5)


# Logistic Model
mDL = glm(Aux ~ Country, data=doenLaten, family=binomial)
summary(mDL)
concordance(mDL)


# Getting the predictions, the easy way

newdata = rbind( c("BE"),
                 c("NL") )
newdata = as.data.frame(newdata)
colnames(newdata) = c("Country")

percLaten = predict(mDL, newdata, type="response")
percDoen = 1-percLaten
percLaten
percDoen



#== Example B1: English Tapping ====================================
# Source: Kilbourn-Ceron, O., Wagner, M., and Clayards, M. (2017).
# The effect of production planning locality on external sandhi:
# A study in /t/. In Proceedings of the 52nd Annual Meeting of the
# Chicago Linguistic Society, pages 313–326.
# http://people.linguistics.mcgill.ca/~morgan/qmld-book/datasets-appendix.html#north-american-english-tapping
#
# In North American English, the sounds [t] and [d] can sometimes be
# optionally pronounced as a tap if followed by a vowel:
#    For those of you who'd like to ea[t] early, lunch will be served.
#    For those of you who'd like to ea[r] early, lunch will be served.
#
# The dataset in englishTap.csv is from Kilbourn-Ceron et al. (2017), who
# performed an experiment to test whether speech rate affected tapping.
# 23 students from McGill University Participants were asked to produce
# sentences like these:
#    If you plit Alice, John will be mad.
#
# The sentences non-words like plit ending in [t] or [d], followed by a vowel
# initial word (such as "Alice"). The speakers were asked to speak at two
# different rates: normal versus fast. This is in the variable speechrate.
#
# Are the speaking rates different in how much people tap?
#========================================================================.


# Load file
tap <- read.csv(file="englishTap.csv", header=TRUE, sep=",")
tap$tapped = as.factor(tap$tapped)
tap$speechrate = as.factor(tap$speechrate)


# Chart
ggplot(tap, aes(x=speechrate,fill=tapped))+
  geom_bar(position='dodge')+
  ggtitle("Bar chart for tapped /t/ by speech rate")+
  geom_text(stat='count', aes(label=..count..), vjust=1.4, color="black",
            position = position_dodge(0.9), size=3.5)


# Levels
#------------------------------------------.
#              |  Reference  Non-reference
#------------------------------------------.
# Tapped       |  NotTapped  YesTapped
# Speech Rate  |  Fast       Slow
#------------------------------------------.
levels(tap$tapped)
levels(tap$speechrate)


# Logit model
mTap = glm(tapped ~ speechrate, data=tap, family=binomial)
summary(mTap)
concordance(mTap)


# Predictions for the probability of
# tapping according to speech rate

newdata = rbind( c("fast"),
                 c("slow") )
newdata = as.data.frame(newdata)
colnames(newdata) = c("speechrate")
newdata$speechrate = as.factor(newdata$speechrate)

probsTapped = predict(mTap, newdata, type="response", SE.fit="TRUE")
probsTapped
probsNonTapped = 1-probsTapped
probsNonTapped


# Coefficients from the GLM model
coeffIntercept  = as.numeric(mTap$coefficients[1]) 
coeffRateSlow   = as.numeric(mTap$coefficients[2]) 
coeffIntercept
coeffRateSlow


# Linear Predictor Speech Rate Fast
lpRateFast = coeffIntercept + 0*coeffRateSlow
lpRateFast
# Linear Predictor Speech Rate Slow
lpRateSlow = coeffIntercept + 1*coeffRateSlow
lpRateSlow


# Probability of {nonTapped,tapped} in SpeechRateFast
# Results of the linear predictor, run through
# the logit function.
pFastTapped    = exp(lpRateFast) / (1 + exp(lpRateFast))
pFastNonTapped = 1 - pFastTapped
pFastTapped
pFastNonTapped
# Probability of {nonTapped,tapped} in the SpeechRateSlow
pSlowTapped = exp(lpRateSlow) / (1 + exp(lpRateSlow))
pSlowNonTapped  = 1 - pSlowTapped
pSlowTapped
pSlowNonTapped


# Log-Ratio
logOddsFast = exp(coeffIntercept + 0*coeffRateSlow)
logOddsFast
logOddsSlow = exp(coeffIntercept + 1*coeffRateSlow)
logOddsSlow

logRatio = logOddsSlow/logOddsFast
logRatioFromBeta = exp(coeffRateSlow)

logRatio
logRatioFromBeta



#== Exercise B3: Dutch Causatives ====================================
# Dutch has two different verbs to make causative constructions:
#
# Hij DEED me denken aan mijn vader.
# He  did  me think  at  my   father
# "He reminded me of my father."
#
# Ik LIET hem mijn  huis schilderen.
# I  let  him paint my   house.
# "I had him paint my house."
#
# Dit  DOET denken aan de  onzalige tijden van de  junta van de  partijvoorzitters.
# This does think  at  the wretched times  of  the junta of  the party-chairmen
# "This reminds of the wretched times of the junta of party chairmen"
#
# In the Flemish-speaking regions of Belgium these two verbs are both
# very frequent, but in the Netherlands laten has become the more
# frequent verb.
#
# Is there a significant difference in the use of these verbs between
# the two countries?
#
# The data is real: Levshina, Natasha (2011). Doe wat je niet laten
# kan: A usage-based analysis of Dutch causative constructions. Ph.D.
# Dissertation, Katholieke Universiteit Leuven.
# https://lirias.kuleuven.be/1821217?limo=0
# The data is described in your textbook (Levshina pg. 254)
#========================================================================.


# Read the files
doenLaten = read.csv(file="doenLaten.csv", header=TRUE, sep=",")
doenLaten$Aux = as.factor(doenLaten$Aux)
doenLaten$Country = as.factor(doenLaten$Country)


# The reference level for the variable Aux is "doen"
# because it's the first one alphabetically
levels(doenLaten$Aux)


# The reference level for the variable Country is "Belgium"
# because it's the first one alphabetically
levels(doenLaten$Country)


# Chart
ggplot(doenLaten, aes(x=Country,fill=Aux))+
  geom_bar(position='dodge')+
  labs(title = "Causative auxiliary in Dutch/Flemish", fill="Auxiliary", y="Count")+
  geom_text(stat='count', aes(label=..count..), vjust=1.4, color="black",
            position = position_dodge(0.9), size=3.5)


# Logistic Model
mDL = glm(Aux ~ Country, data=doenLaten, family=binomial)
summary(mDL)
concordance(mDL)


# Getting the predictions, the easy way

newdata = rbind( c("BE"),
                 c("NL") )
newdata = as.data.frame(newdata)
colnames(newdata) = c("Country")

percLaten = predict(mDL, newdata, type="response")
percDoen = 1-percLaten
percLaten
percDoen


# Coefficients from the GLM model
coeffIntercept = as.numeric(mDL$coefficients[1]) 
coeffCountryNL = as.numeric(mDL$coefficients[2]) 
coeffIntercept
coeffCountryNL


# Linear Predictor Belgium
lpBE = coeffIntercept + 0*coeffCountryNL
lpBE
# Linear Predictor Netherlands
lpNL = coeffIntercept + 1*coeffCountryNL
lpNL


# The probability of LATEN in BELGIUM is
# 51.8%. The probability of DOEN in BELGIUM
# is 48.2%.
pBELaten = exp(lpBE) / (1 + exp(lpBE))
pBEDoen  = 1 - pBELaten
pBELaten
pBEDoen
# The probability of LATEN in the NETHERLANDS is
# 69.5%. The probability of DOEN in the NETHERLANDS
# is 30.5%.
pNLLaten = exp(lpNL) / (1 + exp(lpNL))
pNLDoen  = 1 - pNLLaten
pNLLaten
pNLDoen


# Log-Odds: Belgium
# The intercept is 0.0721
# If we want to talk about Belgium, the countryNL=0
# The exp(0.0721 + 0*0.7528) = exp(0.0721) = 1.0747
# This means that in BELGIUM, the odds that a sentence
# will use laten are 1.07 times larger than the odds
# that a sentence will use doen.
logOddsBE = exp(coeffIntercept + 0*coeffCountryNL)
logOddsBE
# The probability of getting pDoen in BELGIUM is
# 1.07 times larger than the probability of getting laten.
pBELaten / pBEDoen


# Log-Odds: Netherlands
# The intercept is 0.0721
# If we want to talk about the NETHERLANDS, the countryNL=1
# The exp(0.0721 + 1*0.7528) = exp(0.0721) = 2.28
# This means that in the NETHERLANDS, the odds that a sentence
# will use laten are 2.28 times larger than the odds
# that a sentence will use doen.
logOddsNL = exp(coeffIntercept + 1*coeffCountryNL)
logOddsNL
# The probability of getting pDoen in the Netherlands is
# 2.28 times larger than the probability of getting laten.
pNLLaten/ pNLDoen


# Log Ratios
# What do the coefficients mean? They are the difference 
# in odds for each level.
# When the country is Netherlands, LATEN is 2.28 times larger than DOES
# When the country is Belgium, LATEN is 1.074763 times larger than DOES
# 2.281653 / 1.1.074763 = 2.122936
#
# The coefficient for CountryNL is 0.7528
# exp(0.7528) = 2.122936
# The odds of laten/doen in Netherlands are 2.12 times larger
# than the odds of laten/doen in Belgium

logRatio = logOddsNL/logOddsBE
logRatioFromBeta = exp(coeffCountryNL)

logRatio
logRatioFromBeta


# Log-Ratios, from the chart

beDoen  = 115
beLaten = 107
nlDoen  = 71
nlLaten = 162

ratioBE = 107/115
ratioBE

ratioNL = 71/162
ratioNL

logRatioFromChartNums = ratioBE/ratioNL

logRatio
logRatioFromBeta
logRatioFromChartNums

