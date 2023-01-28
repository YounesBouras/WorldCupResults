### Importing the Tables into R 
library(readr)

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


# Installing the Packages
install.packages("lifecycle")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("neuralnet")
install.packages("NeuralNetTools")

library(dplyr)
library(ggplot2)
library(NeuralNetTools)
library(nnet)
library(neuralnet)


#-------------------------------------------------------------------
######## First Example

### 1st Method (neuralnet)

head(group_standings)
nrow(group_standings)

nr_training_rows <- floor(nrow(group_standings) * 0.7)
nr_training_rows

# indixes for the Trainings data (randomly chosen)
train_idx <- sample(1:nrow(group_standings), nr_training_rows)
train_idx

#Training/Test Data
train_data <- group_standings[train_idx, ]
test_data <- group_standings[-train_idx, ]
head(train_data)
head(test_data)

### Building the Neuonales Netzwerk Model
set.seed(1)
neuralnet <- neuralnet(advanced ~ wins+draws+losses+goals_for+goals_against+points, data = train_data,
                       hidden= 4)

plot(neuralnet)
# matrix with the weights
neuralnet$weights

#predicting
pred <- predict(neuralnet, newdata=test_data)
pred <- ifelse(pred>=0.5, 1, 0)
table(actual_outcome = test_data$advanced,
      prediction = pred)

# how exact is our PREDICTION:
1 - mean(test_data$advanced != pred)


## The probability here will be counted, that a team will make it to the next round with the stats that are in the matrix 
new.output <- compute(neuralnet, covariate = matrix(c(2,0,1,9,5,6),
                                                    byrow=TRUE, ncol=6))
new.output$net.result


### 2nd Method (nnet)

df_nnet <- nnet(advanced ~ wins+draws+losses+goals_for+goals_against+points, data = train_data, size = 4)

pred <- predict(df_nnet, newdata=train_data)
predtest <- ifelse(pred>=0.5, 1, 0)
table(tatsächtlich = train_data$advanced,
      Vorhergesagt = predtest)

# how exact is our PREDICTION:
1 - mean(train_data$advanced != predtest)



#---------------------------------------------------------------------------

#### Bonus example (only 1 input layer)

# check the df
head(team_appearances)
nrow(team_appearances)

### Training and Test Data (0.8 Stichprobe)

# How many cases for the training data
nrow(team_appearances)
nr_training_rows <- floor(nrow(team_appearances) * 0.8)
nr_training_rows


# indizes für die Trainings- bzw. Testdata (Zufall)
train_idx <- sample(1:nrow(team_appearances), nr_training_rows)

head(team_appearances)

#Training/Test Data
train_data <- team_appearances[train_idx, ]
test_data <- team_appearances[-train_idx, ]

### Nnet Model aufbauen
set.seed(123)

df_nnet <- neuralnet(win ~ goals_for, data = train_data, hidden = 4, linear.output=FALSE)


pred <- predict(df_nnet, newdata=train_data)
predtest <- ifelse(pred>=0.5, 1, 0)
table(tatsächtlich = train_data$win,
      Vorhergesagt = predtest)

# how exact is our PREDICTION:
1 - mean(train_data$win != predtest)
## The accuracy here is lower because we used less input layers 