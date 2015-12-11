
#For plotting useful factors in the study
library(ggplot2)
study <- read.csv('/Users/Owner/Documents/Work_transfer/User Study/Chris L Data/User Study Factor Model.csv')

fi <- factor(study$Found.Info)

fip <- ggplot(study, aes(fi, fill=fi)) + geom_bar() + xlab("Found Info Useful") + ylab("Count") + 
  ggtitle("Whether Participants Found GCconnex Useful as a Tool") + guides(color = "none") + 
  scale_fill_discrete(name="Answer")

fip
                                                                      
#Now let's take the people who've found it useful and see in which area they work

study.useful <- study[study$Found.Info == "Yes",]

aowy <- factor(study.useful$Area.of.Work)

fiyw <- ggplot(study.useful, aes(aowy, fill=aowy)) + geom_bar() + 
  labs(title = "Area of Work of Those who find the tool useful",
       x = "Area of Work", y = "Count of Users") + 
  scale_fill_discrete(name = "Area of Work") + scale_x_discrete(breaks =NULL)

fiyw

#This tells us the metadata about people who find it useful because why not
summary(study.useful) 

#Let's look at Frequency
#First, for overall
freq <- factor(study$Frequency, levels = c("Not at all", "Very infrequently (less than once a month)",
                                           "Somewhat infrequently (once a month)", "Occasionally (a few times a month)",
                                           "Somewhat frequently (weekly)", "Very frequently (daily)"))

freqplot <- ggplot(study, aes(freq, fill=freq)) + geom_bar() + 
  labs(title = "Frequency of Use", x = "Frequency", y = "Count") + scale_fill_discrete(name = "Frequency") +
  scale_x_discrete(breaks = NULL)

freqplot
 

#Now the same graph, except only for people who find it useful
frequse <- factor(study.useful$Frequency, levels = c("Not at all", "Very infrequently (less than once a month)",
                                              "Somewhat infrequently (once a month)", "Occasionally (a few times a month)",
                                              "Somewhat frequently (weekly)", "Very frequently (daily)"))

frequseplot <- ggplot(study.useful, aes(frequse, fill=frequse)) + geom_bar() + 
  labs(title = "Frequency of Use for Those who Found GCconnex Useful", 
       x = "Frequency", y = "Count") + scale_fill_discrete(name = "Frequency") +
  scale_x_discrete(breaks = NULL)

frequseplot

#One last Frequency plot for those who have actually use the tools
study.used <- study[study$Used == "Yes",]

frequsedyes <- factor(study.used$Frequency, levels = c("Not at all", "Very infrequently (less than once a month)",
                                                                "Somewhat infrequently (once a month)", "Occasionally (a few times a month)",
                                                                "Somewhat frequently (weekly)", "Very frequently (daily)"))

freq.used.yes.plot <- ggplot(study.used, aes(frequsedyes, fill=frequsedyes)) + geom_bar() +
  labs(title = "Frequency of Use for People who Use the Tools", x = "Frequency", y = "Count") + 
  scale_x_discrete(breaks = NULL) + scale_fill_discrete(name = "Frequency Used")

freq.used.yes.plot

used.useful <- factor(study.used$Found.Info, levels = c("Don't Know / Not Sure", "No", "Yes" ))
used.useful.plot <- ggplot(study.used, aes(used.useful, fill=used.useful)) + geom_bar() + 
  labs(title = "Whether Those Who Use the Tool Find it Useful", x = "Found Info Useful",
       y = "Count") + scale_fill_discrete(name = "Answer")

used.useful.plot