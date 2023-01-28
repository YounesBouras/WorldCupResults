install.packages("tree")
install.packages("ISLR")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("lifecycle")
install.packages("dplyr")
install.packages("ggplot2")
library(tree)
#library(ISLR)
library(rpart)
library(rpart.plot)
library(readr)
library(dplyr)
library(ggplot2)


award_winners <- read.csv("award_winners.csv")
goals <- read.csv("goals.csv")
bookings <- read.csv("bookings.csv")
tournament_standings <- read.csv("tournament_standings.csv")
host_countries <- read.csv("host_countries.csv")
manager_appointments <- read.csv("manager_appointments.csv")
player_appearances <- read.csv("player_appearances.csv")
qualified_teams <- read.csv("qualified_teams.csv")
stadiums <- read.csv("stadiums.csv")
teams <- read.csv("teams.csv")
tournaments <- read.csv("tournaments.csv")
players <- read.csv("players.csv")
team_appearances <- read.csv("team_appearances.csv")
matches <- read.csv("matches.csv")
team_appearances <- read.csv("team_appearances.csv")
group_standings <- read.csv("group_standings.csv")


#--------------------------------------------------------------------------------------
### First Table : Group_standings
head(group_standings)

## Training Data
nrow(group_standings)
train=sample(1:nrow(group_standings), floor(nrow(group_standings)*0.7))
train

## Building the Model
tree.group=tree(advanced~wins+draws+losses+goals_for+goals_against+points,
                group_standings,subset=train)
#?tree
#summary(tree.group)

plot(tree.group)
text(tree.group,pretty=0)


# Test Data
group_test=group_standings[-train,]
group_test
nrow(group_test)

# prediction part
tree.pred=predict(tree.group,group_test)
tree.pred
# the output is the probability if the team will advance or not
predtest <- ifelse(tree.pred>=0.5, 1, 0)
#length(tree.pred)
table(actual_outcome = group_test$advanced, prediction = predtest)

#acurracy of the prediction
(57+73)/(57+73+2+6)
