
library(ggplot2)
library(rgl)
library(MASS)
path <- '/Users/Owner/Documents/Work_transfer/Data/Report Card/'
df <- read.csv('/Users/Owner/Documents/Work_transfer/Data/Report Card/Individual Statistics.csv')

colleague_thresh <- df$Colleague_thresh
df$Profile <- 0

df$Profile[df$Avatar == 1 & df$About_me == 1 & df$Skill == 1] <- 1
summary(df$Profile)

Profile_df  <- df[df$Profile == 1,]
df$Profile_no_skills <- 0
df$Profile_no_skills[df$Avatar == 1 & df$About_me == 1] <- 1
summary(df$Profile_no_skills)

Profile <- df$Profile
Colleagues <- df$Colleague_count
Aboutme <- df$About_me
Avatar <- df$Avatar
Groups <- df$Groups.Joined
Comments <- df$Comments
Profile_no_skills <- df$Profile_no_skills

p_model <- lm(Profile ~ Colleagues + Groups + Comments, data=df)
p_nos_model <- lm(Profile_no_skills ~ Colleagues + Groups + Comments, data=df)
c_model <- lm(Comments ~ Colleagues + Groups + Profile, data=df)
c_nos_model <- lm(Comments ~ Colleagues + Groups + Profile_no_skills, data=df)

summary(p_model)
summary(p_nos_model)
summary(c_model)
summary(c_nos_model)


qplot(x=Groups, y=Comments, data=df)

gcomplot_table <- df[Groups >5 & Comments > 5,]

plotgroups <- gcomplot_table$Groups.Joined
plotcomments <- gcomplot_table$Comments
plotcolleagues <- gcomplot_table$Colleague_count

plot(x=plotgroups, y=plotcomments, data=gcomplot_table)
abline(lm(Comments~Groups), col='blue')

scatterplot3d(Groups, Comments, Colleagues, main='GCconnex User Activity')

plot3d(Comments, Colleagues, Groups, col = 'blue', main='GCconnex User Activity')

#Let's filter out some of the nonsense by only account for people who have used activities
plot3d(plotgroups, plotcomments, plotcolleagues, type = 'h', col = 'blue', main='GCconnex Heavy User Activity')
scatterplot3d(plotgroups, plotcomments, plotcolleagues, main='GCconnex Heavy User Activity')

plot3d(plotcomments, xlab = "Comments", ylab = "Groups", zlab = "Colleagues",
       type = ('wire'))



#An interesting plot would be the 3D plot of people who have filled their Profile out

profile_plot <- df[df$Profile == 1,]

pplot_colleagues <- profile_plot$Colleague_count
pplot_groups <- profile_plot$Groups.Joined
pplot_comments <- profile_plot$Comments

plot3d(pplot_colleagues, pplot_groups, pplot_comments, xlab = "Colleagues", ylab= "Groups", zlab = "Comments",
       col = 'blue')

summary(profile_plot)

x <- profile_plot[,c("Comments", "Colleague_count", "Groups.Joined")]
plot3d(x, xlab = "Comments", ylab = "Colleagues", zlab = "Groups", type='wire')


