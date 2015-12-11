library(ggplot2)
require(dplyr)

study <- read.csv('/Users/Owner/Documents/Work_transfer/User Study/Chris L Data/User Study Factor Model.csv')


study.dontknow <- study[study$Found.Info == "Don't Know / Not Sure",]
study.dontknow.fi <- study.dontknow$Area.of.Work

fhmy <- ggplot(study.dontknow, aes(study.dontknow.fi, fill=study.dontknow.fi)) + geom_bar() +
  labs(title = "AOW for Those Who Don't Know", x = "Area of Work", y = "Count") + 
  scale_x_discrete(breaks = NULL) + scale_fill_discrete(name = "Area of Work")
fhmy

normalizer <- table(study$Area.of.Work)
yesaow <- study[study$Found.Info == "Yes",]
yesaow <- table(yesaow$Area.of.Work)
noaow <- study[study$Found.Info == "No",]
noaow <- table(noaow$Area.of.Work)
dkaow <- study[study$Found.Info == "Don't Know / Not Sure",]
dkaow <- table(dkaow$Area.of.Work)

df <- data.frame(normalizer, yesaow, noaow, dkaow)
df <- df[,c("Var1", "Freq", "Freq.1", "Freq.2", "Freq.3")]
df <- rename(df, Area = Var1, Total = Freq, FI.yes = Freq.1, FI.no = Freq.2, FI.dk = Freq.3)

dfyn <- df$FI.yes/df$Total*100
dfnn <- df$FI.no/df$Total*100
dfdkn <- df$FI.dk/df$Total*100

dfn <- data.frame(df$Area, dfyn, dfnn, dfdkn)
dfn <- rename(dfn, Area = df.Area, Yes.Norm = dfyn, No.Norm = dfnn, Dontknow.Norm = dfdkn)


write.csv(dfn, '/Users/Owner/Documents/Work_transfer/User Study/Chris L Data/Department Found Useful.csv')

#Huge Study

largestudy <- read.csv('/Users/Owner/Documents/Work_transfer/User Study/User Study 2014.csv')

q16 <- select(largestudy, Respondant, starts_with('q16_'), starts_with('q16.'))
q16summary <- rowSums(q16)
q16summary

gcconnex <- function(x) {
  select(x, Respondant, contains("gcconnex"))
} #A function that goes through the questions and only selects the Gcconnex part, and the ID

q16 <- gcconnex(q16)
q16table <- data.frame(colSums(q16))

q7 <- select(largestudy, Respondant, starts_with('q7_'), starts_with('q7.'))
q7 <- gcconnex(q7)
q7table <- data.frame(colSums(q7))

write.csv(q16table, '/Users/Owner/Documents/Work_transfer/User Study/Chris L Data/Why Peeps Use GCconnex.csv')
write.csv(q7table, '/Users/Owner/Documents/Work_transfer/User Study/Chris L Data/Why Peeps DONT Use GCconnex.csv')


