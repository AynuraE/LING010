#========================================================================.
# LING10 Stats for Ling, 2026W                                       ====
# Rolando Coto. rolando.a.coto.solano@dartmouth.edu
# Week 3 class notes
#========================================================================.

library('plyr')
library('dplyr')
library('overlapping')
library('ggplot2')

#========================================================================.
# QQ-Plot example                                                    ====
#========================================================================.

testV = c(20,30,35,40,42,45,47,48,50,52,53,55,58,60,65,70,80)
v = data.frame(testV)

mean(testV)

ggplot(v, aes(y=testV)) + 
  geom_boxplot()+
  theme(axis.text.x=element_blank())+
  scale_y_continuous(name = "Units of measurement (unit)")+
  scale_x_discrete(name = "Variable") +
  ggtitle("Boxplot")

sd(testV)
mean(testV) + 1.96*(sd(testV))
mean(testV) - 1.96*(sd(testV))
length(testV)

# Even if the dots don't align perfectly with the QQ-Plot
# diagonal, the Shapiro-Wilk test says that this distribution
# is similar enough to a normal (i.e. it is "not not" normal).
# If p < 0.05:  The distribution is significantly different from normal
# If p >= 0.05: The distribution is not significantly different from normal

par(mfrow=c(1,2))
hist(testV)
shapiro.test(testV)
qqnorm(testV, pch = 1, frame = FALSE)
qqline(testV, col = "steelblue", lwd = 1)
par(mfrow=c(1,1))


#========================================================================.
# T-test: Freq and word associations                                 ====
# Source: Levshina pg 89
#========================================================================.

# The dataset pym is based on pym_high and pim_low, referred to on page 89 of the Levshina textbook.
# Pym contains both high and low frequency words, selected at random. The dataset contains information
# on the number of words associated to a word (e.g. time > watch, clock, spend, waste, minute, second...),
# on how easy it is to imagine a word, and how "concrete" a word is.
# Are there more words associated to high frequency words?

pym <- read.csv("pym.csv", header=T, sep=",")

pym_high = subset(pym, type == "High")
pym_low = subset(pym, type == "Low")

# Are they normal?

par(mfrow=c(1,2))
hist(pym_high$assoc)
shapiro.test(pym_high$assoc)
qqnorm(pym_high$assoc, pch = 1, frame = FALSE)
qqline(pym_high$assoc, col = "steelblue", lwd = 1)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
hist(pym_low$assoc)
shapiro.test(pym_low$assoc)
qqnorm(pym_low$assoc, pch = 1, frame = FALSE)
qqline(pym_low$assoc, col = "steelblue", lwd = 1)
par(mfrow=c(1,1))

# Let's calculate some statistics

nH = nrow(pym_high)
nL = nrow(pym_low)
nH  # Total of "high" freq items
nL  # Total of "low" freq items

mean(pym_high$assoc)
mean(pym_low$assoc)

sd(pym_high$assoc)
sd(pym_low$assoc)

seAssocHigh = sd(pym_high$assoc) / sqrt(nH)
seAssocLow = sd(pym_low$assoc) / sqrt(nL)

# confidence intervals

mean(pym_high$assoc) + 1.96*seAssocHigh
mean(pym_high$assoc) - 1.96*seAssocHigh

mean(pym_low$assoc) + 1.96*seAssocLow
mean(pym_low$assoc) - 1.96*seAssocLow

# mean plus/minus standard deviation
mean(pym_high$assoc) + sd(pym_high$assoc)
mean(pym_high$assoc) - sd(pym_high$assoc)
mean(pym_low$assoc) + sd(pym_low$assoc)
mean(pym_low$assoc) - sd(pym_low$assoc)

# Boxplot

ggplot(pym, aes(x = type, y = assoc)) +
  geom_boxplot()+
  scale_y_continuous(name = "Number of associations")+
  scale_x_discrete(name = "Frequency of words") +
  ggtitle("Number of associations depending on frequency")

# Overlap plot

mu <- ddply(pym, "type", summarise, grp.mean=mean(assoc))

ggplot(pym, aes(x = assoc, fill = type)) + geom_density(alpha = 0.5) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=type),
             linetype="dashed", size=2)+
  scale_x_continuous(name = "Number of associations") 


x <- list(X1=pym_high$assoc, X2=pym_low$assoc)
out <- overlap(x, plot=TRUE)
out$OV

# t-test
t.test(pym_high$assoc, pym_low$assoc)


#========================================================================.
# T-test: Freq and number of letters                                 ====
# Source: Levshina pg 89
#========================================================================.

# Count of items for each category
nH = nrow(pym_high)
nL = nrow(pym_low)
nH
nL


# Boxplot
ggplot(pym, aes(x = type, y = let)) +
  geom_boxplot()+
  scale_y_continuous(name = "Number of letters")+
  scale_x_discrete(name = "Frequency of words") +
  ggtitle("Number of letters depending on frequency")


# Histograms and normality tests

par(mfrow=c(1,2))
hist(pym_high$let)
shapiro.test(pym_high$let)
qqnorm(pym_high$let, pch = 1, frame = FALSE)
qqline(pym_high$let, col = "steelblue", lwd = 1)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
hist(pym_low$let)
shapiro.test(pym_low$let)
qqnorm(pym_low$let, pch = 1, frame = FALSE)
qqline(pym_low$let, col = "steelblue", lwd = 1)
par(mfrow=c(1,1))


#  Overlap

x <- list(X1=pym_high$let, X2=pym_low$let)
out <- overlap(x, plot=TRUE)
out$OV

mu <- ddply(pym, "type", summarise, grp.mean=mean(let))

ggplot(pym, aes(x = let, fill = type)) + geom_density(alpha = 0.5) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=type),
             linetype="dashed", size=2)+
  scale_x_continuous(name = "Number of letters in the word") 


# Statistics (mean, SD, SE) and confidence intervals

mean(pym_high$let)
mean(pym_low$let)
mean(pym_high$let) - mean(pym_low$let) #diff means

mean(pym_high$let) + sd(pym_high$let) # SD1 high
mean(pym_high$let) - sd(pym_high$let) # SD1 low

seLetHigh = sd(pym_high$let) / sqrt(nH) # SE1
mean(pym_high$let) + 1.96*seLetHigh # CI1 high
mean(pym_high$let) - 1.96*seLetHigh # CI1 low

mean(pym_low$let) + sd(pym_low$let) # SD2 high
mean(pym_low$let) - sd(pym_low$let) # SD2 low

seLetLow = sd(pym_low$let) / sqrt(nL)   # SE2
mean(pym_low$let) + 1.96*seLetLow # C2 high
mean(pym_low$let) - 1.96*seLetLow # C2 low


# T-tests

t.test(pym_high$let,pym_low$let)
t.test(pym_high$let,pym_low$let, alternative = "less")



#==============================================================================================================.
# T-test: VOT of Cherokee Consonants                                                                       ====
# Source: Keith Johnson 2008 https://www.wiley.com/en-us/Quantitative+Methods+In+Linguistics-p-9781405144247
#==============================================================================================================.

votFile = "cherokeeVOT.csv"
vot = read.csv(file=votFile, header=TRUE, sep=",")

# Split dataset into two sets
vot1971 = subset(vot, year=="1971")
vot2001 = subset(vot, year=="2001")

# Statistics
nrow(vot1971)
nrow(vot2001)
mean(vot1971$VOT)
mean(vot2001$VOT)
sd(vot1971$VOT)
sd(vot2001$VOT)

# Distribution and normality tests
hist(vot1971$VOT)
hist(vot2001$VOT)
shapiro.test(vot1971$VOT)
shapiro.test(vot2001$VOT)

# T-Test
t.test(vot1971$VOT, vot2001$VOT)

# Overlap
x <- list(X1=vot1971$VOT, X2=vot2001$VOT)
out <- overlap(x, plot=TRUE)
out$OV

# Chart
vot$year = as.factor(vot$year)
ggplot(vot, aes(x = year, y = VOT)) +
  geom_boxplot()+
  scale_y_continuous(name = "VOT (ms)")+
  scale_x_discrete(name = "Year") +
  ggtitle("Cherokee VOT per year")

# Power tests

power.t.test(power=0.80, sig.level=0.05, delta=29, sd=36, type="two.sample", alternative="two.sided")

power.t.test(power=0.60, sig.level=0.05, delta=29, sd=36, type="two.sample", alternative="two.sided")

power.t.test(power=0.95, sig.level=0.05, delta=29, sd=36, type="two.sample", alternative="two.sided")

power.t.test(power=0.99, sig.level=0.05, delta=29, sd=36, type="two.sample", alternative="two.sided")


#========================================================================================================.
# Simulated set: VOT+1                                                                               ====
# All I did was add a one to every number in the 1971 column. I did this so that we can 
# study how you would need a larger amount of measurements to study such a small difference.
#========================================================================================================.

vot1971 = subset(vot, year=="1971")
vot1971Plus1 = subset(vot, year=="1971")

# Add one and bind together with the previous dataset
vot1971Plus1$VOT = vot1971Plus1$VOT+1
vot1971Plus1$year = "1971VOT+1"
vot1971Prime = rbind(vot1971,vot1971Plus1)

# T-test
t.test(vot1971$VOT, vot1971Plus1$VOT)

# Chart
ggplot(vot1971Prime, aes(x = year, y = VOT)) +
  geom_boxplot()+
  scale_y_continuous(name = "VOT (ms)")+
  scale_x_discrete(name = "Year") +
  ggtitle("Cherokee VOT per year")

# Power test
power.t.test(power=0.8, sig.level=0.05, delta=1, sd=36, type="two.sample", alternative="two.sided")


#========================================================================================================.
# Paired T-test                                                                                      ====
# Factors which determine the speed of turn-taking in conversation
# Source: http://people.linguistics.mcgill.ca/~morgan/qmld-book/datasets-appendix.html#transitionsdata 
#========================================================================================================.

# The dataframe has a list of statements for each person, and for each statement it has:
# dur: "duration (ms) of the floor transition between turn A (the turn preceding the floor transition)
#       and turn B (the turn following the transition)"
# time: The moment in the conversation (ms) when the transition happened.
tFile = "transitions.csv"
t = read.csv(file=tFile, header=TRUE, sep=",")


# This takes all the transitions that happen before/after the first minute and calculates
# their average duration for each speaker (file).
tLessThanAMinute = subset(t, time <  60000)
tMoreThanAMinute = subset(t, time >= 60000)
durFilesLessMinute = aggregate(tLessThanAMinute$dur, list(tLessThanAMinute$file), mean)
durFilesMoreMinute = aggregate(tMoreThanAMinute$dur, list(tMoreThanAMinute$file), mean)


# Create a "type" variable which tells us what kind of transition this is
durFilesLessMinute$type = "During First Minute"
durFilesMoreMinute$type = "Second Minute and Afterwards"


# Set the names of the columns
colnames(durFilesLessMinute) = c("file","meanDur","type")
colnames(durFilesMoreMinute) = c("file","meanDur","type")


# Removing speakers ("file") that have NaN values
durFilesLessMinute = na.omit(durFilesLessMinute)
nrow(durFilesLessMinute)
durFilesMoreMinute = na.omit(durFilesMoreMinute)
nrow(durFilesMoreMinute)


# Removing files that are only in one of the two subsets
# (We need to have the same speakers in both groups)
durFilesMoreMinute = subset(durFilesMoreMinute, file %in% durFilesLessMinute$file)
  

# Join the two dataframes into one
d = rbind(durFilesLessMinute,durFilesMoreMinute)


# Statistics

length(durFilesLessMinute$meanDur)
length(durFilesMoreMinute$meanDur)

mean(durFilesLessMinute$meanDur, na.rm=TRUE)
mean(durFilesMoreMinute$meanDur, na.rm=TRUE)
sd(durFilesLessMinute$meanDur, na.rm=TRUE)
sd(durFilesMoreMinute$meanDur, na.rm=TRUE)

hist(durFilesLessMinute$meanDur)
hist(durFilesMoreMinute$meanDur)


# Overlap
x <- list(X1=durFilesLessMinute$meanDur, X2=durFilesMoreMinute$meanDur)
out <- overlap(x, plot=TRUE)
out$OV


# Chart
ggplot(d, aes(x = type, y = meanDur)) +
  geom_boxplot()+
  scale_y_continuous(name = "Wait between speaking turns (ms)")+
  scale_x_discrete(name = "") +
  ggtitle("Time between turns of speaking,\nbefore or after one minute of conversation")


# T-test
t.test(durFilesLessMinute$meanDur,durFilesMoreMinute$meanDur, paired = TRUE)


# Power test
power.t.test(power=0.99929, sig.level=0.05, delta=63, sd=195, type="paired", alternative="two.sided")


#==============================================================================================================.
# Non-parametric test                                                                                      ====
# More info: http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/BS704_Nonparametric/BS704_Nonparametric_print.html
#==============================================================================================================.

# Values for each "novel"
np1 = c(7,5,6,4,12)
np2 = c(3,6,4,2,1)

# Statistics
median(np1)
median(np2)

# Put together the dataframe
np1.d = data.frame(np1)
colnames(np1.d) = c("val")
np1.d$type= "Type1"
np2.d = data.frame(np2)
colnames(np2.d) = c("val")
np2.d$type= "Type2"
npb = rbind(np1.d,np2.d)

# Modify the column names
hath = npb
colnames(hath) = c("numberHath", "novel")

# Wilcox test
wilcox.test(numberHath ~ novel, data=hath, alternative = c("greater")) 

# Overlap
x <- list(X1=np1, X2=np2)
out <- overlap(x, plot=TRUE)
out$OV

