#Using R for the User Study because I think that R will let me do more than Pandas with this complex
#Dataset.

  #Also, the Dataset is pretty cleaned up as it is
  require(foreign)
  require(ggplot2)
  require(MASS)
  require(Hmisc)
  require(reshape2)
  require(dplyr)
  library(ordinal)
  study <- read.csv('/Users/Owner/Documents/Work_transfer/User Study/User Study 2014.csv')
  
  #I'm going to be working on this User Study with the spreadsheet in an Excel Document
  #It's just so large
  
  #I'm going to divide this dataset into separate datasets for each question
  
  q1 <- select(study, Respondant, starts_with('q1_'), starts_with('q1.'))
  q2 <- select(study, Respondant, starts_with('q2_'), starts_with('q2.'))
  q3 <- select(study, Respondant, starts_with('q3_'), starts_with('q3.'))
  q4 <- select(study, Respondant, starts_with('q4_'), starts_with('q4.'))
  q5 <- select(study, Respondant, starts_with('q5_'), starts_with('q5.'))
  q6 <- select(study, Respondant, starts_with('q6_'), starts_with('q6.'))
  q7 <- select(study, Respondant, starts_with('q7_'), starts_with('q7.'))
  q8 <- select(study, Respondant, starts_with('q8_'), starts_with('q8.'))
  q9 <- select(study, Respondant, starts_with('q9_'), starts_with('q9.'))
  q10 <- select(study, Respondant, starts_with('q10_'), starts_with('q10.'))
  q11 <- select(study, Respondant, starts_with('q11_'), starts_with('q11.'))
  q12 <- select(study, Respondant, starts_with('q12_'), starts_with('q12.'))
  q13 <- select(study, Respondant, starts_with('q13_'), starts_with('q13.'))
  q14 <- select(study, Respondant, starts_with('q14_'), starts_with('q14.'))
  q15 <- select(study, Respondant, starts_with('q15_'), starts_with('q15.'))
  q16 <- select(study, Respondant, starts_with('q16_'), starts_with('q16.'))
  q17 <- select(study, Respondant, starts_with('q17_'), starts_with('q17.'))
  q18 <- select(study, Respondant, starts_with('q18_'), starts_with('q18.'))
  q19 <- select(study, Respondant, starts_with('q19_'), starts_with('q19.'))
  q20 <- select(study, Respondant, starts_with('q20_'), starts_with('q20.'))
  q21 <- select(study, Respondant, starts_with('q21_'), starts_with('q21.'))
  q22 <- select(study, Respondant, starts_with('q22_'), starts_with('q22.'))
  q23 <- select(study, Respondant, starts_with('q23_'), starts_with('q23.'))
  q24 <- select(study, Respondant, starts_with('q24_'), starts_with('q24.'))
  q25 <- select(study, Respondant, starts_with('q25_'), starts_with('q25.'))
  q26 <- select(study, Respondant, starts_with('q26_'), starts_with('q26.'))
  q27 <- select(study, Respondant, starts_with('q27_'), starts_with('q27.'))
  q28 <- select(study, Respondant, starts_with('q28_'), starts_with('q28.'))
  q29 <- select(study, Respondant, starts_with('q29_'), starts_with('q29.'))
  q30 <- select(study, Respondant, starts_with('q30_'), starts_with('q30.'))
  
  #Doing this gives a much better way of organizing things
  
  #For the purposes of applying this data to the Tech Acceptance Model, I'm going to
  #To use a make a custom set out of all the questions
  
  #Preliminarily, I want to look at:
  # Awareness of GCconnex Q1
  # GCconnex Usage Q2 
  # Frequency of Usages Q5
  # Whether it improved work Q9
  # Area of Work Q24
  # Age Q25
  # Gender Q26
  # Years Served Q30
  
  #This will save me quite a bit of time now by limiting what I want to look at
  
  
  gcconnex <- function(x) {
    select(x, Respondant, contains("gcconnex"))
  } #A function that goes through the questions and only selects the Gcconnex part, and the ID
  
  #mxqy = model x, question y
  m1q1 <- gcconnex(q1)
  m1q2 <- gcconnex(q2)
  m1q5 <- gcconnex(q5)
  m1q9 <- gcconnex(q9)
  
  m1q24 <- select(q24, Respondant, contains("area"))
  m1q25 <- q25
  m1q26 <- q26
  m1q30 <- q30
  
  #So now I have my initial variables of interest, I still have some work to clean the data some more
  #Firstly, renaming columns
  
  m1q1 <- rename(m1q1, Aware = q1.1_aware_gCconnex)
  m1q2 <- rename(m1q2, Used = q2.1_used_gcconnex)
  m1q5 <- rename(m1q5, Frequency = q5.1_frequency_requency_gcconnex)
  m1q9 <- rename(m1q9, Found.Info = q9.1_found.info_gcconnex)
  m1q24 <- rename(m1q24, Area.of.Work = q24_area.of.work)
  m1q25 <- rename(m1q25, Age = q25_age.group)
  m1q26 <- rename(m1q26, Gender = q26_gender)
  m1q30 <- rename(m1q30, Years.Served = q30_years.of.service)
  
  #Cool, now let's throw them all in a dataframe together
  framelist <- list(m1q1, m1q2, m1q5, m1q9, m1q24, m1q25, m1q26, m1q30)
  st <- Reduce(full_join, list(m1q1, m1q2, m1q5, m1q9, m1q24, m1q25, m1q26, m1q30))
  stbackup <- st
  #Now we have all the variables that I wanted to play with in one dataframe. Dope
  
  summary(studytable)
  
  #Now that we have everything in a table, it's time to start playing with the variables
  #In a way that we can start doing some econometric work with it
  
  #Due to the nature of the survey, almost all of the variables are categorical
  
  #This function checks if the variables in the set are already factored
  factorcheck <- function(x) {
    for(i in names(x)) {
     print(i)
      print ("Factored:")
      print (is.factor(x[[i]]))
      print ("Ordered:")
      print (is.ordered(x))
      print ("Levels:")
      print (levels(x[[i]]))
    }
  }
  
  
  factorcheck(st)
  
  #Recalling the summary of st, there are some issues with the table
  #The "Used" column has 714 0's, which are meaningless
  #Frequency has 2162 " " for an answer
  #Found info helpful has 2162 0's
  #Area of Work has 2 other variables
  #All of these variables are to be removed
  
  
  st <- st[(st$Used != 0),]
  st <- st[st$Found.Info != 0,]
  st <- st[st$Frequency != "",]
  
  #Now to refactor these variables
  st$Used <- factor(st$Used)
  st$Found.Info <- factor(st$Found.Info)
  st$Frequency <- factor(st$Frequency, ordered = TRUE)
  st$Age <- ordered(st$Age, levels = c("24 years and under", "25 - 29 years", "30 - 34 years", "35 - 39 years", "40 - 44 years", "45 - 49 years", "50 - 54 years", "55 - 59 years", "60 years and over"))
  st$Years.Served <- ordered(st$Years.Served, levels = c("Less than 3 years", "3 to 10 years", "11 to 20 years", "More than 20 years"))
   
  #So we went from 7200 to 5000 observations, but the data is all meaningful now (more or less)
  
  #ORDERED LOGISTIC REGRESSION TIME!
  
  m1 <- polr(Frequency ~ Found.Info + Age + Years.Served + Area.of.Work + Gender, data = st, Hess = TRUE)
  summary(m1) #There are miscoded variables
  
  
  st1 <- st[st$Found.Info != "Don't Know / Not Sure", ]
  ctable <- coef(summary(m1))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2
  ctable <- cbind(ctable, "p value" = p)
  confint(m1)

#Check out the outputs for this, and you'll be sad. Most of the stuff is not statistically significant

freq.clm <- clm(Frequency ~ Found.Info + Age + Years.Served + Area.of.Work + Gender, data=st)
summary(freq.clm)
confint(freq.clm)

#A different more effective model shows alternative results
write.csv(st, file='/Users/Owner/Documents/Work_transfer/User Study/User Study Factor Model.csv')

#Let's look at question 4 so we can analyze exactly what people would LIKE to use the tools for
colSums(q4)

#Look at why people don
stuff <- table(summary(st))

summarytable <- function(x) {
  for (i in (x)) {
    print (summary(i))
  }
}


summarytable(st)
