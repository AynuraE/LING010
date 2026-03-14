#== Metadata ==================================================================
# LING10 Statistics for Linguistics
# Dartmouth College. January 2026
# X-Hour 2. Samples and distributions


#== Load me! ==================================================================
# When you load datasets, please remember to, either (1) Change the path so it
# matches the path in your computer, or (2) set the working directory to where
# you are storing your files, and then open the files without an explicit path.


# Library dplyr. We'll use a function from here (sample_n)
library(dplyr)


# File from WALS with information about tonal languages
# File source: https://wals.info/feature/13A#2/19.3/152.8
# Four variables:
# X (int): The index of each example (you don't need this column)
# language (Factor): The name of each language in the sample
# tonal (Factor): Whether the language is tonal or not {Two factors: yes,no}
# tonalBinary (int): Whether the language is tonal or not, but expressed as
#                    either a zero or a one. We have this redundant column
#                    so that we can perform arithmetic and get averages.
tonesFile  = "tone.csv"
tones <- read.csv(file=tonesFile, header=TRUE, sep=",")


# Cherokee VOT
# File source: https://www.wiley.com/en-us/Quantitative+Methods+In+Linguistics-p-9781405144247
# Three variables:
# VOT (int): Voice onset time (in milliseconds)
# year (int): Year when the recordings were made
# Consonant (factor): Type of consonant where the VOT was measured {k,t}
votFile = "cherokeeVOT.csv"
vot = read.csv(file=votFile, header=TRUE, sep=",")


#== Part 1. Sampling and means ================================================
#
# How many tonal languages are there in the world? In reality, no one has
# counted the 7000+ languages to actually check, so we don't know. The usually
# cited number of 40%. This comes from the World Atlas of Linguistic Structures
# (WALS) http://wals.info/, which has information on thousands of languages.
#
# WALS has a sample of 527 languages where they tried to answer the question
# of whether the language was tonal or not. We will treat this as the 
# "population". In this sample, there are 41.7% of languages (0.417) that
# have tone. We will treat this as the true population mean. We will then
# get subsamples and try to see how things could have been different with
# a different group of languages.


#== Let's get the population means.


# First, let's find out how many languages in the population 
# are tonal and how many aren't. We can use a table for this.
tableTotalTones = table(tones$tonal)
tableTotalTones


# Next, we'll find out how many languages there are in the
# population. The function nrow tells you how many rows
# there are in a dataframe.
totalLangsInWalsTone= nrow(tones)
totalLangsInWalsTone


# Next, we will find out the percentage. tableTotalTones is
# a vector, and its second element is the number of languages
# that have "Yes" as their value for tones$tonal. We will
# get this second element and divide it by the total of
# languages in the WALS file.
percTonalLangs = tableTotalTones[2] / totalLangsInWalsTone
percTonalLangs


#== Now, let's pick a confidence level.
# The variables below have the value of "z", the function that
# determines the level of confidence. You can find more information
# about this number on pages 98-101 of the textbook. For now,
# notice that line 89 has zFor95Confidence as the chosenConfidenceZ.
# For now, let's leave it like this. (But later, you will change
# line #93 so that we can use a difference confidence interval. It
# will say, for example:
# chosenConfidenceZ = zFor50Confidence

zFor95Confidence = 1.96
zFor99Confidence = 2.58
zFor50Confidence = 0.674
chosenConfidenceZ = zFor95Confidence


#== Now, let's pick some random samples!

# First, we choose the size of the random sample. 20 should be a small
# but interesting size to start with.
randomSampleSize = 20

# Next, we use the function sample_n to get a random sample (of size
# randomSampleSize) from the dataframe tones. We store this new 
# dataframe in sampleTone.
sampleTone = sample_n(tones,randomSampleSize)

# Here, we get the percentage of languages that are tonal in our 
# random sample. We will use the tonalBinary column, because it
# allows for very easy calculations.
# (For example, if I have two languages that are tonalBinary=1 and
# tonalBinary=0, their mean would be 0.5. This means that "half" of
# the languages in the sample are tonal).
percTonalInSample = mean(sampleTone$tonalBinary)


# We will use the formulas on page 99 of the textbook to calculate
# the margin of error. Notice that (1) we first get the standard
# deviation. (2) Then, we get the standard error by using the 
# standard deviation AND the size of the random sample. (2) Then,
# we use this and the chosenConfidenceZ to calculate the actual
# margin of error. We selected the chosenConfidenceZ on line 93)
sdSampleTone = sd(sampleTone$tonalBinary)
seSampleTone = sdSampleTone / sqrt(randomSampleSize)
marginErrorSampleTone = chosenConfidenceZ * seSampleTone
marginErrorSampleTone


#== Confidence interval for our sample
# Here, we will print the true mean (for all of the tones dataframe),
# the sample mean (obtained from our random sample), and we will get
# the lower and upper boundaries of the confidence interval.

print("true mean: ")
percTonalLangs
print("percent of tonal languages in sample:")
percTonalInSample
print("lower boundary of confidence interval of sample:")
percTonalInSample - marginErrorSampleTone
print("higher boundary of confidence interval of sample:")
percTonalInSample + marginErrorSampleTone


# Now, let's try to make the following changes:
#  
# (1) Try changing the confidence level (on line 93) and set it to 50% of confidence.
#     After you make this change, try running lines 100-140 again. Do this several times.
#     How does that change your sample means and your confidence intervals?
#
# (2) Set the confidence interval back to 95% (on line 93) and try changing the size of
#     the random sample (on line 100) to make it larger. After you do this, run lines
#     100-140 again. Do this several times. How does that change your sample means and
#     your confidence intervals?



#== Part 2. Normality =========================================================
# Checking for normal distributions will be important as we move forward. So,
# is the Cherokee data normal? The following functions can help you determine
# that.


# First let's make a histogram. The histogram doesn't look very normal.
hist(vot$VOT)


# Next, let's try a method called the Q-Q Plot. (More info on pages 50-57
# of your book).If the distribution is normal, the dots will fall along
# the diagonal line. It looks... ish? It's not great. It's oscillating,
# but more importantly, there's a couple of points at the right edge that
# are far from the line. (This is a "long tail", values that are much
# larger than expected).
qqnorm(vot$VOT, pch = 1, frame = FALSE)
qqline(vot$VOT, col = "steelblue", lwd = 1)


# By the way, let me introduce a nice function that works with some
# plots. The par(mfrow=c(ROWS,COLUMNS)) makes it so that R draws your
# charts in a nice arrangement. (It doesn't work for ggplot, but
# it works for histogram and qqplot charts). Notice that, I am first
# going to set it to 1Column-2Rows, and then, after I'm done with
# the drawing, I am going to reset it to its default 1Column-1Row.
par(mfrow=c(1,2))
hist(vot$VOT)
qqnorm(vot$VOT, pch = 1, frame = FALSE)
qqline(vot$VOT, col = "steelblue", lwd = 1)
par(mfrow=c(1,1))


# Let's go back to normality testing. Finally, let's try a test
# called the "Shapiro-Wilk Test". If the p-value is less than
# 0.05, then the distribution is NOT normal (i.e. it is significantly
# different from the normal). 
shapiro.test(vot$VOT)


# Just for the math fans: Here's the Kolmogorov-Smirnov test.
# We usually don't use this test in Linguistics because it's
# too strong (and we depend a lot on the Central Limit Theorem,
# to be honest).
ks.test(vot$VOT, "pnorm")


# All of our tests confirm that this distribution does not look very normal.
# Let's look separately at the two components we are studying, the data from
# 1971 and the data from 2001. We will use the function "subset" to separate
# the two parts and put them in two different dataframes.
vot1971 = subset(vot, year==1971) 
vot2001 = subset(vot, year==2001) 


# == Let's test the 1971 data.
# It looks like it is normal! The Shapiro-Wilk test is not significant, and
# there are not maaajor deviations in the Q-Q plot.

par(mfrow=c(1,2))
hist(vot1971$VOT)
qqnorm(vot1971$VOT, pch = 1, frame = FALSE)
qqline(vot1971$VOT, col = "steelblue", lwd = 1)
par(mfrow=c(1,1))

shapiro.test(vot1971$VOT)


# == Let's test the 2001 data.
# It looks like it is normal! Here's where the outliers live. The Shapiro-Wilk test
# is less than p=0.05, so that tells us that this is different from the normal. The
# histogram and the q-q plot show some points that are way out there. They are values
# that are much larger than the rest of the sample.

par(mfrow=c(1,2))
hist(vot2001$VOT)
qqnorm(vot2001$VOT, pch = 1, frame = FALSE)
qqline(vot2001$VOT, col = "steelblue", lwd = 1)
par(mfrow=c(1,1))

shapiro.test(vot2001$VOT)



# == Part 3: Transformations ==================================================
# Can the problem in normality be solved through a transformation? Usually
# transformations are used to change the shape of a distribution into something
# more normal. They will not help a lot with outliers. But let's give it a try.

# First, let's calculate two more columns for the dataframe vot2001. We
# will add the natural logarithm value of VOT to a new column called logVOT.
# We will also add the square root of VOT to a new column called sqrtVOT.
vot2001$logVOT = log(vot2001$VOT)       # Yes, "log" is the natural logarithm. Base 10 logarithms are log10.
vot2001$expVOT = exp(vot2001$VOT)
vot2001$sqrtVOT = sqrt(vot2001$VOT)


# Next, let's look try our normality tests on the log-transformed VOT.
# The logarithm helped a little, but this distribution is still not normal.
par(mfrow=c(1,2))
hist(vot2001$logVOT)
qqnorm(vot2001$logVOT, pch = 1, frame = FALSE)
qqline(vot2001$logVOT, col = "steelblue", lwd = 1)
par(mfrow=c(1,1))
shapiro.test(vot2001$logVOT)



# Let's try the square root transformation. It pulls it slightly towards
# the center, but the conversion was not enough to make it normal.
par(mfrow=c(1,2))
hist(vot2001$sqrtVOT)
qqnorm(vot2001$sqrtVOT, pch = 1, frame = FALSE)
qqline(vot2001$sqrtVOT, col = "steelblue", lwd = 1)
par(mfrow=c(1,1))
shapiro.test(vot2001$sqrtVOT)


# Finally, let's try the exponential transformation. It pulled the distribution
# even more towards the left, so it was totally ineffective.
par(mfrow=c(1,2))
hist(vot2001$expVOT)
qqnorm(vot2001$expVOT, pch = 1, frame = FALSE)
qqline(vot2001$expVOT, col = "steelblue", lwd = 1)
par(mfrow=c(1,1))
shapiro.test(vot2001$expVOT)


# == Part 4: Z-Scores (Normalization) =========================================
# Let's look at a different topic. Let's say you want to compare across 
# speakers. Not everyone has the same pitch, so a "low" pitch could be
# 100Hz for some speakers, and 150Hz for others. If we want to compare
# across speakers, we should get the z-scores of VOT so that we "normalize"
# the data.
#
# The z-score does two things: (1) It "centers" the data, so that the mean
# becomes the new zero. (2) It changes the unit. Now, a change of 1 unit
# will represent a change of 1 standard-deviation. You can find more 
# information about the z-score on pages 59-62 of your textbook.


# First, let's calculate the z-score. We need the mean and the standard
# deviation. Then we use the formula on page 59 of your book.

votMean = mean(vot$VOT)
votSD   = sd(vot$VOT)
vot$zVOT = (vot$VOT - votMean)/votSD

# Let's look at what happened. We'll draw the histograms for the original
# VOT and for the new z-score VOT. 

par(mfrow=c(2,2))
hist(vot$VOT)
qqnorm(vot$VOT, pch = 1, frame = FALSE)
qqline(vot$VOT, col = "steelblue", lwd = 1)
hist(vot$zVOT)
qqnorm(vot$zVOT, pch = 1, frame = FALSE)
qqline(vot$zVOT, col = "steelblue", lwd = 1)
par(mfrow=c(1,1))


# As you can see, the shapes of the histograms didn't change. The Q-Q plots
# didn't change either. What changed is the scale of the histogram. Now
# the mean is at zero, and we can easily see that most data is within one
# standard deviation of the mean.
#
# Using z-scores is very common when doing phonetics, for example, because
# we need to compare across speakers. Vowel formants and fundamental
# frequencies (F0) should be z-scored.
# and other measurements should be z-scored.