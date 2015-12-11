library(ggplot2)
library(dplyr)
library(MASS)
library(aod)

#This is a new sheet to do some really rigorous data analysis and visualization

study <- read.csv('/Users/Owner/Documents/Work_transfer/User Study/Chris L Data/User Study Factor Model.csv')
#After the cleaning we're left with 9 columns to work with
#To determine the types of people who use the software

#The model is going to be an ordered logistic regression

factorcheck <- function(x) {
  for (i in x) {
    print ("Factor")
    print (is.factor(i))
    print ("Order")
    print (is.ordered(i))
  }
}

factorcheck(study)
#Exporting the new dataset, we lose all the ordering in the variables
#Which is annoying AF
#So I'm gonna have to redo the ordering on this notebook
#Blessed Copy+Paste
study$Used <- factor(study$Used)
study$Found.Info <- factor(study$Found.Info)
study$Frequency <- factor(study$Frequency, ordered = TRUE)
study$Age <- ordered(study$Age, levels = c("24 years and under", "25 - 29 years", "30 - 34 years", "35 - 39 years", "40 - 44 years", "45 - 49 years", "50 - 54 years", "55 - 59 years", "60 years and over"))
study$Years.Served <- ordered(study$Years.Served, levels = c("Less than 3 years", "3 to 10 years", "11 to 20 years", "More than 20 years"))

logitmodel <- glm(Used ~ Found.Info + Age + Years.Served + Gender + Area.of.Work, family = "binomial", data = study)

summary(logitmodel)

#The significant things to take from this model is that people who use it are the people are
#People who find it useful..... But this is also controlling for Gender and Area of Work
#The next part is to determine the factors that are influencing usefulness

study.info.model <- study[study$Found.Info != "Don't Know / Not Sure",] #Removing the not sure part
study.info.model$Found.Info <- factor(study.info.model$Found.Info) #Resetting the Factor

logitmodelinfo <- glm(Found.Info ~ Age + Years.Served + Gender + Area.of.Work, family = "binomial",
                      data = study.info.model)

summary(logitmodelinfo)


infoaow <- glm(Found.Info ~ Area.of.Work, family = "binomial", data=study.info.model)
summary(infoaow)
#Unsurprisingly, age and years served don't have an impact on usage.
#Surprisingly, a male is more likely to find GCconnex useful than a female (but not by much)
#There were not many insights that we could pull out from the regressions as they stand so far..
#What we can extrapolate however, is that the most amount of significant factors arise from Area of Work!

#When we separate all the other factors, things get even less significant... This is weird.
#There are so many shortcomings with the data and the survey, especially because we aren't able to link
#The survey data to actual GCconnex usage. If we did that, we could get so much better information
#And even some chances to VERIFY assumptions about usage.


#Now let's try removing the Area of Work Variable. #Doing so has no real effect on the sigificance
#of the remaining variables.

logitmodelnoaow <- glm(Found.Info ~ Age + Years.Served + Gender, family = "binomial",
                       data = study.info.model)

summary(logitmodelnoaow)


usednoaow <- glm(Used ~ Age + Years.Served + Gender, family = "binomial",
                 data = study)

summary(usednoaow)
