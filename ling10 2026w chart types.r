#== Metadata =====================================
# LING10 Statistics for Linguistics
# Dartmouth College. January 2026
# Chart Examples

library(ggplot2)

#== Example files ================================
#
# (1) Cherokee VOT Data (Keith Johnson's "Quantitative
#     Methods in Linguistics, pgs 2-3):
#     VOT (voice onset time) measurements, which show the
#     duration of aspiration in voiceless stops in
#     Cherokee. Measurements from recordings
#     of one speaker, the Cherokee linguist Durbin Feeling,
#     that were made in 1971 and 2001. There are three
#     variables:
#
#     (i)   VOT: voice onset time, in milliseconds
#     (ii)  year: year when the recordings were made {1971,2001}
#     (iii) Consonant: Consonants tested {t,k}
#
#
# (2) English Lexicon Project (Levshina's book, pgs 41-42):
#     The dataset contains 100 randomly selected words
#     from the English Lexicon Project data (Balota et
#     al. 2007). The ELP provides behavioral and 
#     descriptive data from hundreds of native speakers
#     for over forty thousand words of American English,
#     as well as for non-words. The dataset contains
#     three quantitative variables:
#
#     (i)   Length: word length in letters
#     (ii)  Freq: word frequency according to the Hyperspace
#           Analog to Language frequency norms (Lund &
#           Burgess 1996), based on the HAL corpus, which
#           consists of about 131 million words gathered
#           from Usenet newsgroups, also known as Google
#           groups.
#     (iii) Mean_RT: average reaction times in a lexical
#           decision task, in milliseconds. A lexical
#           decision task is an experiment where subjects
#           are asked to classify stimuli as words or 
#           non-words.


votFile = "cherokeeVOT.csv"
vot = read.csv(file=votFile, header=TRUE, sep=",")

ldtFile = "ldt.csv"
ldt = read.csv(file=ldtFile, header=TRUE, sep=",")



#== Univariate: Numerical ========================
# Charts for just one variable, where the variable
# is numerical continuous


# Histogram
votHist = ggplot(vot, aes(x=VOT)) + 
          geom_histogram()+
          ggtitle("Histogram of the variable VOT")
votHist


# Density plot
votDensity <- ggplot(vot, aes(x=VOT)) + 
              geom_density()+
              ggtitle("Density of variable VOT")
votDensity


# Both histogram and density
votHistDens = ggplot(vot, aes(x=VOT)) + 
  geom_histogram(aes(y=..density..))+
  geom_density()+
  ggtitle("Chart with both the histogram and the density")
votHistDens


# Boxplot
votBox = ggplot(vot, aes(y=VOT)) + 
  geom_boxplot()+
  theme(axis.text.x=element_blank())+
  ggtitle("Boxplot for the variable VOT")
votBox



#== Univariate: Categorical ======================
# Charts for just one variable, where the variable
# is categorical

consBarChart = ggplot(vot, aes(x=Consonant))+
               geom_bar()+
               ggtitle("Bar chart for number of each consonant")
consBarChart


# Bar chart with percents
consBarPerc = ggplot(vot, aes(x=Consonant))+
              geom_bar(aes(y = (..count..)/sum(..count..)))+
              geom_text(aes( label = round((..count..)/sum(..count..),2),
                 y= (..count..)/sum(..count..) ), stat= "count", vjust = -.35)+
              ggtitle("Bar chart for percentage of each consonant")

consBarPerc




#== 2Vars: Cat x Num =============================
# Categorical x numerical
# Charts for when your x-variable is categorical
# (e.g. type of consonant) and your y-variable is
# numerical (e.g. vOT)

# Boxplot
votChart = ggplot(vot, aes(x = Consonant, y = VOT)) +
  geom_boxplot()+
  scale_y_continuous(name = "VOT (ms)")+
  scale_x_discrete(name = "Consonant") +
  ggtitle("Cherokee VOT per consonant type")

votChart



#== 2Vars: Cat x Cat =============================
# Categorical x categorical
# The most elementary way we have to display
# these relationships is a contingency table.

# First, we will create a new column, where we
# will store the year not as a number, but as
# a factor.
vot$yearAsFactor = as.factor(vot$year)

# Now we can make our contingency table
tableYearCons = table(vot$year, vot$Consonant)
tableYearCons

# You can get it as percentages
prop.table(tableYearCons)     # As absolute percentages
prop.table(tableYearCons,1)   # As percentages per row
prop.table(tableYearCons,2)   # As percentages per column


# Or you could do a more complex barchart
consYearBarChart = ggplot(vot, aes(x=Consonant,fill=yearAsFactor))+
  geom_bar(position='dodge')+
  ggtitle("Bar chart for quantity of each\nconsonant on each year")
consYearBarChart



#== 2Vars: Num x Num =================
# Numerical x numerical
# We will use a kind of plot called a 
# scatterplot.

# Scatterplot
scatterFreqMean1 = ggplot(ldt, aes(x=Freq, y=Mean_RT)) + 
                   geom_point() +
                   ggtitle("Reaction time by frequency")
scatterFreqMean1


# A scatterplot, but with different variables
scatterFreqMean2 = ggplot(ldt, aes(x=Length, y=Mean_RT)) + 
                   geom_point()+
                   ggtitle("Reaction time by word length")+
                   labs(y="Reaction time (milliseconds)", x="Length (in letters)")
scatterFreqMean2




#== 3Vars: CatxCatXNum ===========================
# Categorical x categorical x numerical
# We will use a more complex boxplot

votBoxConsYear = ggplot(vot, aes(x = Consonant, y = VOT, fill=yearAsFactor)) +
                 geom_boxplot()+
                 scale_y_continuous(name = "VOT (ms)")+
                 scale_x_discrete(name = "Consonant") +
                 ggtitle("Cherokee VOT per consonant type and year")

votBoxConsYear




#== 3Vars: CatxNumXNum ===========================
# Categorical x numerical x numerical
# You can make a scatterplot and add something
# called a "facet", so that the charts are
# displayed side by side.


# Just for the sake of the exercise, I will make a new 
# variable in ldt. I will call some words "frequent" if
# they have occur more than 100 times. I will call all
# other words "lessFrequent".

# First, make the new variable "long" for all of the data points
ldt$frequentOrNot = "lessFrequent"   
# Then, we tell it to put the value "Short" in the new column 
# wordsize for all datapoints where the Length is less than 6
ldt$frequentOrNot[ldt$Freq > 100] = "frequent"
# Finally, we turn this new variable into a factor
ldt$frequentOrNot = as.factor(ldt$frequentOrNot)


scatterFreqMean2 = ggplot(ldt, aes(x=Length, y=Mean_RT)) + 
  facet_grid(. ~ frequentOrNot)+
  geom_point()+
  ggtitle("Reaction time by word length and frequency")
scatterFreqMean2
