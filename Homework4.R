#== Homework 4 =====================================
# LING10 Statistics for Linguistics
# Dartmouth College. March 10, 2026
# Aynura Erejepbaeva

#load data
library(ggplot2)
library(car)
library(asbio)
library(lsmeans)
library(dplyr)
library(plotly)
library(lmtest)
library(MASS)
library(lmerTest)
library(ggeffects)
library(lme4)
library(survival)
library(emmeans)

#=============================================.
# Exercise 1: Iconicity, sensory experience and parts-of-speech in English    ====

#(a) Does this model meet the assumptions of a linear model? (Assume the residuals are normal and
#that variables are not collinear).
#(b) Write the equation of the model for the linear regression Iconicity ~ SER*POS
#(c) Write a short but formal report for the linear regression result, as if you were writing a conference
#abstract. What does the interaction mean? Include a chart with your explanation. You can use the
#following as the first line of your ggplot chart:
  #ggplot(ico2, aes(x=SER, y=Iconicity, group=POS, color=POS))
#=============================================.

# Load dataset
ico <- read.csv("pos-iconicity-100.csv", header = TRUE)

# Convert POS to categorical
ico$POS <- as.factor(ico$POS)

#Data exploration
aggregate(ico$Iconicity, list(ico$POS), mean)
aggregate(ico$Iconicity, list(ico$POS), sd)

#chart
ggplot(ico, aes(x=SER, y=Iconicity, group=POS, color=POS)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Iconicity by sensory experience and part of speech",
       x="Sensory Experience Rating (SER)",
       y="Iconicity")

# Interaction chart
model <- lm(Iconicity ~ SER * POS, data=ico)
summary(model)

#Model
coef(model)

# Q-Q Plots of the regression residuals
par(mfrow=c(1,2))
plot(model$res ~ model$fit, xlab="Fitted values", ylab="Residuals")
abline(h=0,col="gray")
qqnorm(model$res)
qqline(model$res)
par(mfrow=c(1,1))

# Shapiro-Wilk Test of normality
# Should we assume that it is normal?
shapiro.test(model$residuals)

# Homoscedasticity of variances
ncvTest(model)

# Autocorrelation
durbinWatsonTest(model)

# R-squared
summary(model)$r.squared
summary(model)$adj.r.squared

# Collinearity
vif(model)

#=============================================.
# Exercise 2: Iconicity, sensory experience and semantic neighborhoods ====

#(a) Does the model meet the assumptions of a linear model? (Assume that the residuals are normally
#distributed and that the variables are not collinear).
#(b) Write the equation for the model in the linear regression Iconicity ~ SER*ARC
#(c) Write a short but formal report for the linear regression result. What does the interaction mean?
#Include a chart with your explanation. I recommend that you split the ARC variable in half, using
#the code below:
#summary(ico$ARC)
#ico$highLowARC = "LowARC: SparseNeighborhood"
#ico$highLowARC[ico$ARC > 0.6] = "HighARC: DenseNeighborhood"
#ggplot(ico, aes(x=SER, y=Iconicity)) +
  #facet_wrap(~highLowARC)+
  #geom_point() +
  #geom_smooth(method="lm")+
  #scale_y_continuous(name = "Iconicity")+
  #scale_x_continuous(name = "SER") +
  #ggtitle("Iconocity by SER and ARC")
#=============================================.

# Load file
ico <- read.csv("iconocity_50.csv", header = TRUE, sep = ",")

# Data exploration
aggregate(ico$Iconicity, list(ico$ARC), mean)
aggregate(ico$Iconicity, list(ico$ARC), sd)

# Split ARC into two groups 
summary(ico$ARC)
ico$highLowARC = "LowARC: SparseNeighborhood"
ico$highLowARC[ico$ARC > 0.6] = "HighARC: DenseNeighborhood"
ico$highLowARC = as.factor(ico$highLowARC)

# Chart
ggplot(ico, aes(x=SER, y=Iconicity)) +
  facet_wrap(~highLowARC) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_y_continuous(name = "Iconicity") +
  scale_x_continuous(name = "SER") +
  ggtitle("Iconicity by SER and ARC")

# Linear model with interaction
mIco <- lm(Iconicity ~ SER * ARC, data = ico)
summary(mIco)

# Coefficients
coef(mIco)

# R-squared
summary(mIco)$r.squared
summary(mIco)$adj.r.squared

# Assumptions
par(mfrow=c(1,2))
plot(mIco$res ~ mIco$fit, xlab="Fitted value", ylab="Residuals")
abline(h=0, col="gray")
qqnorm(mIco$res, main="")
qqline(mIco$res)
par(mfrow=c(1,1))

# Assumption, normality of residuals
shapiro.test(mIco$residuals)

# Assumption, homoscedasticity
ncvTest(mIco)

# Assumption: independence
durbinWatsonTest(mIco)

#=============================================.
# Exercise 3: Tongue positions that are difficult to measure      ====

#(a) Run diagnostics on the initial model (normality of residuals, non-variance of residuals, collinearity,
#autocorrelation). Does it show any problems? (Assume that the autocorrelation is not a problem).
#(b) Try to run the stepAIC instruction in three directions: forward, backward and “both”. What are the
#simplest models that can describe y15? Do the results of all three directions match?
  #(c) Use the simplest suggestion out of all of the stepAIC suggestions. Does this reduce the AIC?
  #(Calculate the AIC of this simpler model with the AIC(modelName) instruction. Don’t use the
  #AIC number from the stepAIC directly). What is the adjusted R2 of this simpler model, compared to
#the adjusted R2 of the initial model?
  #(d) Let’s think about the differences between the initial model and the simplest model. Which
#variables are significant in the initial model? Which ones are significant in the simplest model? How
#is the simplest model different from the first one?
#=============================================.

# Load file
chain <- read.csv("chaindata.csv", header = TRUE, sep = ",")

mChain1 <- lm(y15 ~ x1 + y1 + x2 + y2 + x4 + x5 + y5 + x6 + y6, data = chain)

summary(mChain1)

# AIC and R-squared values of initial model
AIC(mChain1)
summary(mChain1)$r.squared
summary(mChain1)$adj.r.squared

# Residual plots
par(mfrow = c(1,2))
plot(mChain1$res ~ mChain1$fit,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "gray")

qqnorm(mChain1$res, main = "")
qqline(mChain1$res)
par(mfrow = c(1,1))

# Normality of residuals
shapiro.test(mChain1$residuals)

# Non-constant variance
ncvTest(mChain1)

# Collinearity
vif(mChain1)

# Autocorrelation
durbinWatsonTest(mChain1)

# Forward selection
step_forward <- stepAIC(
  lm(y15 ~ 1, data = chain),
  scope = y15 ~ x1 + y1 + x2 + y2 + x4 + x5 + y5 + x6 + y6,
  direction = "forward",
  trace = TRUE
)

# Backward selection
step_backward <- stepAIC(
  lm(y15 ~ x1 + y1 + x2 + y2 + x4 + x5 + y5 + x6 + y6, data = chain),
  direction = "backward",
  trace = TRUE
)

# Both directions
step_both <- stepAIC(
  lm(y15 ~ x1 + y1 + x2 + y2 + x4 + x5 + y5 + x6 + y6, data = chain),
  direction = "both",
  trace = TRUE
)

# three selected models
step_forward
step_backward
step_both

# Selected models
formula(step_forward)
formula(step_backward)
formula(step_both)

mChainSimple <- lm(y15 ~ y2 + y5 + y6, data = chain)

# Simplest model
summary(mChainSimple)

# AIC of simplest model
AIC(mChainSimple)

# R-squared and adjusted R-squared
summary(mChainSimple)$r.squared
summary(mChainSimple)$adj.r.squared
-
AIC(mChain1)
AIC(mChainSimple)

summary(mChain1)$adj.r.squared
summary(mChainSimple)$adj.r.squared

# Initial model coefficients
summary(mChain1)

# Simplest model coefficients
summary(mChainSimple)

coef(mChain1)
coef(mChainSimple)

# Scatterplot of observed vs fitted for simplest model
ggplot(chain, aes(x = fitted(mChainSimple), y = y15)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Observed y15 by fitted values",
       x = "Fitted values",
       y = "Observed y15")

ggplot(chain, aes(x = y5, y = y15)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "y15 by y5",
       x = "y5",
       y = "y15")

#=============================================.
# Exercise 4: Logistic Regression, Back to Panama     ====

#What is the relationship between social class and /s/ deletion? Your answer should have four parts:
#(1) The equation for your generalized linear model. Remember to include your error term and the link function.
#(2) A formal report of a generalized Linear Model with class as an independent variable and
#deletion as the dependent variable. Remember that the number 1 in class is the highest class, not the lowest.
#(3) Calculate the concordance index of this model. Do you think this provides sufficient discrimination? Should we add more variables?
#(4) Use the predict instruction to get the predicted percentage of deleted /s/ for each social group.
#=============================================.

# Load file
panama <- read.csv("big-panama.csv", header = TRUE, sep = ",")

# Convert variables to factors
panama$deletion <- as.factor(panama$deletion)
panama$class <- as.factor(panama$class)

# Check reference levels
levels(panama$deletion)
levels(panama$class)

# Bar chart
ggplot(panama, aes(x = class, fill = deletion)) +
  geom_bar(position = "dodge") +
  labs(title = "/s/ deletion by social class in Panama",
       x = "Social Class",
       y = "Count",
       fill = "Deletion") +
  geom_text(stat = "count",
            aes(label = ..count..),
            vjust = 1.4,
            color = "black",
            position = position_dodge(0.9),
            size = 3.5)

# Proportion table
tab <- table(panama$class, panama$deletion)
tab
prop.table(tab, 1)

# Logistic model
mPanama <- glm(deletion ~ class, data = panama, family = binomial)
summary(mPanama)

# Concordance index
concordance(mPanama)

# Predicted probabilities for each class
newdata <- rbind(c("1"),
                 c("2"),
                 c("3"),
                 c("4"))
newdata <- as.data.frame(newdata)
colnames(newdata) <- c("class")
newdata$class <- as.factor(newdata$class)

# Predicted probability for deletion
predDelete <- predict(mPanama, newdata, type = "response")
predDelete

# Predicted probability of non-deletion 
predNonDelete <- 1 - predDelete
predNonDelete

# coefficients 
coef(mPanama)

# Linear predictors
coeffIntercept <- as.numeric(mPanama$coefficients[1])
coeffClass2 <- as.numeric(mPanama$coefficients[2])
coeffClass3 <- as.numeric(mPanama$coefficients[3])
coeffClass4 <- as.numeric(mPanama$coefficients[4])

coeffIntercept
coeffClass2
coeffClass3
coeffClass4

#=============================================.
# Exercise 5: Random Effects: Tones make you sad     ====

#(1) Make a chart for the relationship between tones (x-axis) and happyPlanetIndex (y-axis).
#(2) Run a linear mixed-effects model, with tone as the independent variable,
#happyPlanetIndex as the dependent variable, and family and macroarea as random
#effects with a random intercept (e.g. (1|family) + (1|macroarea)). Write a formal
#report and a 1~2 sentence summary of the results without using any numbers.
#(3) Run a linear mixed-effects model, with tone as the independent variable,
#happyPlanetIndex as the dependent variable, and family and macroarea as random
#effects with a random intercept and a random slope for tone (e.g. (1+tone|family) + (1+tone|macroarea)). Write a formal report and a 1~2 sentence summary of the results
#without using any numbers.
#(4) Make a chart for the relationship between tones (x-axis) and happyPlanetIndex (y-axis),
#but using different slopes for macroarea. Using the information on this chart and the two
#models, write a simple 3~5 sentence response to the following question: What are the slopes
#telling you? What is the difference between the first and the second model?
#=============================================.

# Load file
happy <- read.csv("happy-tones.csv", header = TRUE, sep = ",")

# Make factors for random effects
happy$family <- as.factor(happy$family)
happy$macroarea <- as.factor(happy$macroarea)

happy$tones <- as.numeric(happy$tones)
happy$happyPlanetIndex <- as.numeric(happy$happyPlanetIndex)

# Chart
ggplot(happy, aes(x = tones, y = happyPlanetIndex)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = lm) +
  labs(title = "Happy Planet Index by Tones",
       x = "Number of tones",
       y = "Happy Planet Index")

# Random intercept model
mHappy1 <- lmer(happyPlanetIndex ~ tones + (1 | family) + (1 | macroarea),
                data = happy, REML = FALSE)
summary(mHappy1)

predHappy1 <- ggpredict(mHappy1, terms = c("tones"), type = "random")
plot(predHappy1)

# Random intercept, random slope model
mHappy2 <- lmer(happyPlanetIndex ~ tones + (1 + tones | family) + (1 + tones | macroarea),
                data = happy, REML = FALSE)
summary(mHappy2)

predHappy2 <- ggpredict(mHappy2, terms = c("tones", "macroarea"), type = "random")
plot(predHappy2)

#Chart with different slopes by macroarea
ggplot(happy, aes(x = tones, y = happyPlanetIndex, colour = macroarea)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Happy Planet Index by Tones and Macroarea",
       x = "Number of tones",
       y = "Happy Planet Index")
