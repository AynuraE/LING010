#== Metadata =====================================
# LING10 Statistics for Linguistics
# Dartmouth College. January 2025
# X-Hour 20250106 - Intro to R


#== Intro =====================================
# This is a comment. It's a way to clarify code
# (so that five years from now, when you read
# your own code again, you'll know what it's about).
# You make a comment with a pound sign at the
# beginning of the line. Whatever is in a comment
# will not be executed as an instruction.
#
# We use R for several reasons: 
# (1) It is the standard in data science these days,
#     and will look amazing in your CV.
# (2) It makes your code "reproducible", so that you
#     or other researchers can retrace your steps.
# (3) It has a large community of people supporting
#     and developing tools, so there's plenty of ways
#     to learn more.
# (4) It's free!
#
# Below we will look at some basic operations: 
# Basic arithmetic, file opening and saving, 
# dataframe management.


#== Basic instructions ==========================

# You are typing in a "script" (a file with the .R
# extension). In order to execute an instruction, go
# to its line and press CTRL+ENTER. The instruction
# "print" prints text onto the console screen (below).

print("Hello everyone!")

# Arithmetic instructions use predictable symbols
# +, -, *, /, ^

2+2
2-2
2*2
2/2
2^2

# Variables are a way to store your results. Notice
# how, as you create a variable, it appears in the
# "environment" window to the right. It changes
# every time you change it. The following variables
# are all numbers, so they are type "numeric"

x = 2+2
y = 2-2
z = 2*3

# If you print a variable onto the console, it will
# show the value it contains. 

x
y
z

# "Functions" are instructions that take a variable 
# as input and then give you some output. "Print" is
# a function we used above. It takes the input "Hello
# world" or any other phrase you give it, and it will
# print it in the console. "Print" also works with
# functions.

print(x)

# If you use the function "str" (structure), you will
# see the type of variable you have and its value. If
# you ever need immediate help with a function, you
# can use the function "help".

str(x)
help(str)

# You can operate on variables to create other variables

sumXY = x + y
sumXY

sumXYZ = sumXY + z
sumXYZ


# == Logical operations =========================
# Programming languages use TRUE or FALSE to
# process instructions. (These are equivalent to
# zero and one which frequently pop up in everyday
# descriptions of computers).
#
# Some logical operations are
# > Greater than
# < Less than
# == Equal (with two equal signs, not just one)

2 > 2   # FALSE
2 > 3   # FALSE
3 > 2   # TRUE

2 < 2
2 < 3
3 < 2

2 == 2
2 == 3
3 == 2

# == Vectors =====================================
# You can create groups of values in a single
# variable. we call these "vectors". This is
# going to create a NUM vector, that goes 
# from [1:5] because it starts at one and has
# five values.
# (The 'c' means 'concatenate').

x = c(2,4,6,8,9)

# You can access individual values by using
# square brackets:

x[1]
x[5]

# You can use these values in the same way
# you use variables.

x[1] + x[5]  # 2+9=11

# You can access groups of values by giving the
# first one, then a colon, then the final one

x[1:3]

# You can perform operations on entire vectors.

x
x = x*2 + 5
x

# And yes, R is unique in that the first value
# of a vector is the value "one". Most programming
# languages start vectors at zero.


# == Types of vectors ============================

# The vector above is a numeric vector
str(x)

# But there are other types of vectors. The
# following instruction will create a character
# vector. It works the same as other vectors.
y = c('Hanover', 'Lebanon', 'WestLebanon', 'Hanover')
y
str(y)
y[1]


# There is a third type of vector, a "factor".
# This will be very important. It stores the values
# but also knows which are the different values
# stored in the vector. You can turn a character
# vector into a factor vector using the function
# as.factor()

z = as.factor(y)
z
str(z)
z[1]


# == Reading files ===============================
# You can bring in data from previously existing
# files. Here, we will open the Cherokee VOT
# dataset (from Keith Johnson's "Quantitative
# Methods in Linguistics"). the first variable,
# votFile, has the path of the file in my 
# computer. Change it so that it is the path
# to the file in your computer. 
#
# File source: https://www.wiley.com/en-us/Quantitative+Methods+In+Linguistics-p-9781405144247
# 
# VOT is the "voice onset time". It's the 
# duration of the small puff of air you have
# when you produce the letter 't' in "telephone".
# Put a piece of paper in front of your mouth,
# holding it from the top so that it hangs in
# front of your lips. Say "top". You'll see that
# the paper moves forward because the 't' is
# accompanied by a small explosion of air. The
# duration of this explosion is the "VOT".
# By the way, now say the word "stop". The paper
# will not move! The duratio of the English VOT
# varies according to the consonants around
# the 't'.

#votFile = "C:\\Users\\dartuser\\Desktop\\cherokeeVOT.csv"
votFile = "cherokeeVOT.csv"
# macPath = "//Users//publicuser//Desktop//cherokeeVOT.csv"

# This second instruction opens the file. It says:
# (1) The file that should be opened is in the path
#     votFile.
# (2) The first row of the file is the "headers" 
#     of the columns.
# (3) The columns are separated by commas.
# (4) The data should be stored in the variable VOT.

vot <- read.csv(file=votFile, header=TRUE, sep=",")


# == Dataframes ==================================
# Dataframes are the equivalent of spreadsheets
# in R. The previous instruction creates a 
# dataframe. You can look at it on the right.
# Click on the small blue button to look at the
# different columns in the dataframe.
# When you are in the script, you can get to the
# columns by using the dollar sign after the
# dataframe's name.

vot$VOT
str(vot$VOT)   # int is an "integer number" It behaves
               # like numeric vectors.

vot$year
str(vot$year)

vot$Consonant  
str(vot$Consonant)  # This factor vector has two levels:
                    # The consonants 't' and 'k'


# == Basic stats =================================
# "mean" is the function to get the average value
# from a numeric vector. "sd" gets you the standard
# deviation.

mean(vot$VOT)
median(vot$VOT)
sd(vot$VOT)

# If you want to get the mean separated by different
# factor (e.g. if you want the average VOT for the
# consonant 'k' *and* separately for the consonant 't'),
# use the instruction "aggregate". The first 
# argument of the function is the number whose mean
# you want to get (in this case, "VOT"). The second
# argument of the function is the list of variables
# that you want to use to separate the average (in 
# this case, vot$Consonant"). The third argument is
# the operation you want to perform (first the "mean"
# and then the "median").

aggregate(vot$VOT, list(vot$Consonant), mean)
aggregate(vot$VOT, list(vot$Consonant), median)

# You can use more than one variable in the second
# argument:

aggregate(vot$VOT, list(vot$Consonant,vot$year), mean)
aggregate(vot$VOT, list(vot$Consonant, vot$year), median)


# == Your first chart ============================
# We will install the library "ggplot2" and make
# our first chart, a basic boxplot. After installing
# it, you need to load it into the workspace.

install.packages("ggplot2")
library('ggplot2')

# The following instruction makes a chart. The first
# line states that:
# (1) The dataframe that contains the data is "vot"
# (2) The x-axis of the chart is vot$Consonant
# (3) The y-axis of the chart is vot$VOT
# (4) The plot should be stored in the variable votChart
#
# The second line says that the plot should be a boxplot.
# The third line says that the title of the y-axis should
# be "VOT (ms)"
# The fourth line says that the title of the x-axis
# should be "Consonant"

votChart = ggplot(vot, aes(x = Consonant, y = VOT)) +
           geom_boxplot()+
           scale_y_continuous(name = "VOT (ms)")+
           scale_x_discrete(name = "Consonant") +
           ggtitle("Cherokee VOT per consonant type")

# The chart is stored in the variable votChart. Now
# you need to "print" the chart. You can then go to
# "Export" to copy the chart or to save it as a JPG.

votChart


# == Exercise 1: Error  ===========================
# Find the errors: Each one of these lines has an
# error. Try to spot what it is.

xValues = c(1, 2 3, 4, 5, 6, 7, 8, 9)
mean_x = mean(Xvalues)

x = c(1, 2, 3, '4')
mean(x)


# == Exercise 2: Stats and chart =================
# Change the VOT chart so that the x-axis is the
# year, not the consonant. Be mindful of what 
# should be numeric and what should be a factor.


# == Exercise 3: Three variables in one chart ====
# Change the VOT chart so that you have two x-variables:
# year and Consonant. Try Googling for something like
# "ggplot boxplot two factors" and see how it goes.
# The instruction you need is "fill=year", but,
# where does it go?


votChart = ggplot(vot, aes(x = Consonant, y = VOT, fill = interaction(Consonant, year))) +
  geom_boxplot()+
  scale_y_continuous(name = "VOT (ms)")+
  scale_x_discrete(name = "Consonant") +
  ggtitle("Cherokee VOT per Consonant")
votChart

vot$year = as.factor(vot$year)
votChart = ggplot(vot, aes(x = Consonant, y = VOT, fill = year)) +
  geom_boxplot()+
  scale_y_continuous(name = "VOT (ms)")+
  scale_x_discrete(name = "Consonant") +
  labs(title = 'Cherokee VOT per Consonant', x="Consonant", y="VOT (ms)", fill="Year")
votChart


# == Exercise 4: Swirl Tutorial ==================
# Swirl is a self-guided tutorial of R. In the
# following instructions you will install it and
# use it in the console.

# By the way, Swirl will tell you that, instead
# of equal signs, you must write your instructions
# like this:
# x <- 5+7
# This is completely equivalent to:
# x = 5+7
# (In case you want to know, the "<-" is an 
# "assignment operator and "=" is a "named
# argument passing" operator.)

install.packages('swirl')
library('swirl')
swirl()
