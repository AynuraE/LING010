#========================================================================.
# LING10 Stats for Ling, 2022W                                       ====
# Rolando Coto. rolando.a.coto.solano@dartmouth.edu
# Week 5 Code and Exercises
#========================================================================.

library(ggplot2)
library(vcd)
library(ggmosaic)

#========================================================================.
# Example 1: Latin Tenses in Gospel Translators                      ====
# Source: https://www.lancaster.ac.uk/fss/courses/ling/corpus/Corpus3/3SIG.HTM
#
# This is a classic example to explain chi-squared. In the Latin
# translations of the gospels of Matthew and John, Latin verbs
# can appear in two different tenses:
#
# Present tense:	dicit "(he/she) says"
# Perfect tense:	dixit "(he/she) said"
#
# Is there a different between the two documents in their use of
# the present and perfect tense?
#========================================================================.

# Load the files
latinFile  = "dixit.csv"
latin <- read.csv(file=latinFile, header=TRUE, sep=",")


# Display the loaded dataframe and
# convert it into a matrix of counts. 
latin
matrixLatin = xtabs(count ~ Author+Verb, data=latin)
matrixLatin


# Get the proportion table 
# (percent of each verb for each author)
prop.table(matrixLatin,1)


# Make a barchart of the counts for
# each author and type of verb
ggplot(latin, aes(x=Author,y=count, fill=Verb))+
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("Type of 3rd person\nverb per author")+
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)


# Make a barchart of the percentage
# for each author and type of verb.
ggplot(latin, aes(x=Author,y=perc, fill=Verb))+
  geom_bar(stat="identity", position=position_dodge())+
  labs(title="Type of 3rd person\nverb per author", 
       x="Author", y = "Percent")+
  geom_text(aes(label=perc), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)


# Chi-squared test
chisq.test(matrixLatin)


# Expected values
# Are any of the expected values less than 5?
# If so, then we need the Fisher Exact Test.
# If none of the values is less than 5, we can continue.
chisq.test(matrixLatin)$expected


# Effect size
assocstats(matrixLatin)


# Optional: Residuals
chisq.test(matrixLatin)$residuals


# Optional: oddsRatios
oddsJohn         = 118/119
oddsMatthew      = 46/107
oddsRatioAuthors = oddsJohn / oddsMatthew
oddsRatioAuthors   # Matthew uses 1 dicit for every 2.3 dixit
# John uses 1 dicit for every 1 dixit.
# The ratio for John is 2.3 (oddsRatio) higher than for Matthew


#========================================================================.
# Exercise 1: Shtr in Ohio                                           ====
# Source: https://daviddurian.wordpress.com/publications/
#         https://t-nagano.com/research/R/
#
# Data from 120 native-English speakers in Columbus, OH. It was elicited
# through the Rapid Anomalous Survey Technique. Researchers elicited the
# data by asking for directions to a known street. Their spontaneous
# pronunciation of [str]eet and [str]aight is the data without emphasis.
#
# After getting directions to the street, the researchers asked "do you
# mean X street?". People?fs replies had emphasis on words like [str]eet
# and [str]aight. This is the data with emphasis. (This study is similar
# to the fou[r]th floo[r] studies in NYC).
#
# When do people say [str]eet and when do they say [shtr]eet?
#========================================================================.

# Load file
columbusFile  = "DDRASSTR.csv"
columbus <- read.csv(file=columbusFile, header=TRUE, sep=",")


# Get tables with counts and proportions
t = table(columbus$emphatic,columbus$onset)
t
# Proportions of 
pt.byEmphasis = prop.table(t, 1)
pt.byEmphasis
# Proportions of 
pt.byOnset = prop.table(t, 2)
pt.byOnset


# Chart with counts per Emphasis and Onset
ggplot(columbus, aes(x=emphatic,fill=onset))+
  geom_bar(position='dodge')+
  labs(title="Pronunciation of str/shtr in\nOhio depending on emphasis", 
       x="Emphasis", y = "Count", fill = "Onset")+
  geom_text(stat='count', aes(label=..count..), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)


# Here I leave you an example of a chart with percentages
# First, I converted the proportion table to a dataframe.
# Then, I rename the columns in the dataframe.
# After that, I multiply all the values in the column "perc"
# by 100 and then round it to zero decimals.
# Finally, we plot the chart. The instruction geom_text
# places the numbers on the bars.
dfpt = as.data.frame(pt.byEmphasis)
colnames(dfpt) = c("Onset","Emphasis","perc")
dfpt
dfpt$perc = round(dfpt$perc * 100,0)
ggplot(dfpt, aes(x=Onset,y=perc, fill=Emphasis))+
  geom_bar(stat="identity", position=position_dodge())+
  labs(title="Pronunciation of str/shtr in\nOhio depending on emphasis", 
       x="Emphasis", y = "Percent", fill="Onset")+
  geom_text(aes(label=perc), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)


# Chi-squared test
chisq.test(columbus$onset, columbus$emphatic)


# Effect size
assocstats(t)


# Expected values
# Are any of the expected values less than 5?
# If so, then we need the Fisher Exact Test.
# If none of the values is less than 5, we can continue.
chisq.test(t)$expected


# Optional: oddsRatios
oddsMore          = 43/77
oddsLess          = 14/106
oddsRatioEmphasis = oddsMore / oddsLess
oddsRatioEmphasis 


#========================================================================.
# Example 2: Raise versus Do-Supp in Shakespeare                     ====
#
# This is an example of an Exact Fisher Test. The data is simulated,
# but it's inspired on research I carried out in 2018:
# https://www.degruyter.com/document/doi/10.1515/jhsl-2018-0022/html  
#
# The data has three columns:
#     Character: Four different characters from a literary work in the 1600s
#     verbType:  Type of sentence:
#                Raise   {TP thou like Shakespeare?} -> Likest thou Shakespeare?
#                DoSupp  {TP you  like Shakespeare?} -> Do you like Shakespeare?
#     count:     The quantity of each verbType occurence for each character
#========================================================================.


# Read the files
raise  = "exact-fisher-characters.csv"
raise <- read.csv(file=raise, header=TRUE, sep=",")


# First, let's look at the data
ggplot(raise, aes(x=character,y=count, fill=verbType))+
  geom_bar(stat="identity", position=position_dodge())+
  labs(title="Types of Verb per character", 
       x="Character", y = "Count")+
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)



# Convert the columns into a matrix
t = xtabs(count ~ character+verbType, data=raise)
t


# Conduct a t-test. It will give you a warning that
# the approximation might be incorrect. We will
# verify this by looking at the expected numbers for
# each of the cells in the matrix.
chisq.test(t)
chisq.test(t)$expected


# Indeed, this data does not meet the assumptions
# of chi-squared because some of the cells have
# expected values of less than 5.
# We carry out an Exact Fisher test instead.
fisher.test(t)


#========================================================================.
# Exercise 2: Lady Tasting Tea                                       ====
#
# In a summer tea-part in Cambridge, England, a lady
# claimed to be able to discern, by taste alone, whether
# a cup of tea with milk had the tea poured first or the
# milk poured first. An experiment was performed by R.A.
# Fisher, then and there, to see if her claim was valid.
# Eight cups of tea are prepared and presented to her in
# random order. Four had the milk poured first, and four
# had the tea poured first. The lady tasted each one and
# rendered her opinion. The results are summarized in the
# file lady-tasting.tea.csv.
#
# If you want to read more about this test, and what
# exactly is the mechanism behind Fisher's exact test,
# you can go here:
# https://online.stat.psu.edu/stat504/book/export/html/708
#========================================================================.

# Read the files
tea  = "lady-tasting-tea.csv"
tea <- read.csv(file=tea, header=TRUE, sep=",")


# Chart with counts per Onset and Emphasis
ggplot(tea, aes(x=pouredFirst,fill=ladySaid))+
  geom_bar(position='dodge')+
  labs(title="Lady Tasting Tea", 
       x="Which one was poured first?", y = "Count", fill = "Lady\nsaid")+
  geom_text(stat='count', aes(label=..count..), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)


# Conduct a t-test. It will give you a warning that
# the approximation might be incorrect. We will
# verify this by looking at the expected numbers for
# each of the cells in the matrix.
# Go look at the dataset. There is no "counts" 
# columns. The "counts" are just the combinations
# of the values in the columns pouredFirst and
# ladySaid. That's why we have these two
# columns here, instead of a matrix.
chisq.test(tea$pouredFirst, tea$ladySaid)
chisq.test(tea$pouredFirst, tea$ladySaid)$expected


# Indeed, this data does not meet the assumptions
# of chi-squared because some of the cells have
# expected values of less than 5.
# We carry out an Exact Fisher test instead.
fisher.test(tea$pouredFirst, tea$ladySaid)


#========================================================================.
# Example 3: Etymology and Regularity                                ====
#
# Estimated etymological age for regular and irregular
# monomorphemic Dutch verbs, together with other distributional
# predictors of regularity.
#
# Source: Baayen, R. H. and Moscoso del Prado Martin, F. (2005)
# Semantic density and past-tense formation in three Germanic
# languages, Language, 81, 666-698.
# https://cran.r-project.org/web/packages/languageR
#
# What is the relationship between verb regularity and 
# etymological age in Dutch?
#========================================================================.


# Read file
etymFile  = "dutch-etymology.csv"
etym <- read.csv(file=etymFile, header=TRUE, sep=",")


# Contingency table
matrixEtym = table(etym$EtymAge, etym$Regularity)
matrixEtym


# Reorder variables to match time
etym$EtymAge <- factor(etym$EtymAge, levels = c("Dutch", "DutchGerman", "WestGermanic", "Germanic", "IndoEuropean"))


# Make the contingency table with the right temporal order
matrixEtym = table(etym$EtymAge, etym$Regularity)
matrixEtym


# Barchart
ggplot(etym, aes(x=EtymAge,fill=Regularity))+
  geom_bar(position='dodge')+
  labs(title = "Verb regularity at different historical stages of Dutch", fill = "Verb\nconjugation", x = "Historical stage", y = "Count")+
  geom_text(stat='count', aes(label=..count..), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)


# Barchart with percentages
percEtym = prop.table(matrixEtym, 1)
percEtym
dfe = as.data.frame(percEtym)
colnames(dfe) = c("Stage","Regularity","perc")
dfe
dfe$perc = round(dfe$perc * 100,0)
ggplot(dfe, aes(x=Stage,y=perc, fill=Regularity))+
  geom_bar(stat="identity", position=position_dodge())+
  labs(title="Verb regularity at different historical stages of Dutch", 
       x="Historical stage", y = "Percent", fill="Verb\nConjugation")+
  geom_text(aes(label=perc), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)



# Chi-squared test and expected values
chisq.test(etym$EtymAge, etym$Regularity)
chisq.test(matrixEtym)$expected


# Fisher Test
fisher.test(matrixEtym)


# Effect size
assocstats(matrixEtym)


#========================================================================.
# Example 4: Datives                                                 ====
#
# Data describing the realization of the dative as NP or PP
# in the Switchboard corpus and the Treebank Wall Street Journal
# collection.
# https://cran.r-project.org/web/packages/languageR
#
# Source: Bresnan, J., Cueni, A., Nikitina, T. and Baayen, R. H.
# (2007) Predicting the dative alternation, in Bouma, G. and
# Kraemer, I. and Zwarts, J. (eds.), Cognitive Foundations of
# Interpretation, Royal Netherlands Academy of Sciences.
#========================================================================.


# Read file
datFile  = "eng-dative.csv"
dat <- read.csv(file=datFile, header=TRUE, sep=",")


# Contingency table
matrixDat = table(dat$RealizationOfRec, dat$AnimacyOfRec)
matrixDat


# Chi-Squared and expected values
chisq.test(dat$RealizationOfRec, dat$AnimacyOfRec)
chisq.test(dat$RealizationOfRec, dat$AnimacyOfRec)$expected


# Barchart
ggplot(dat, aes(x=AnimacyOfRec,fill=RealizationOfRec))+
  geom_bar(position='dodge')+
  labs(title = "Dative phrase by the\nanimacy of the dative noun", fill = "Dative\nphrase", x = "Animacy of dative noun", y = "Count")+
  geom_text(stat='count', aes(label=..count..), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)


# Barchart with percentages
percDat = prop.table(matrixDat, 1)
percDat
dfDat = as.data.frame(percDat)
colnames(dfDat) = c("Phrase","Noun","perc")
dfDat
dfDat$perc = round(dfDat$perc * 100,0)
ggplot(dfDat, aes(x=Noun,y=perc, fill=Phrase))+
  geom_bar(stat="identity", position=position_dodge())+
  labs(title="Dative phrase by the\nanimacy of the dative noun", 
       x="Animacy of dative noun", y = "Percent", fill="Dative\nphrase")+
  geom_text(aes(label=perc), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)


# Effect size
assocstats(matrixDat)

