### Importing the Tables into R 
#setwd("C:/Users/Lenovo/Desktop/World Cup Projekt")
install.packages("readr")
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


# Install and load the dplyr package
install.packages("lifecycle")
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

#-----------------------------------------------------------------------------------

### Checking which player scored the most goals in the history of the WC:

head(goals)
head(players)
# Group the data by the player_id 
goals_grouped <- group_by(goals, player_id)

# printing out for each player how many goals he scored (by first transforming the group_by func into a table then a new df to join with another df)
goals_scored_by_player <- table(goals_grouped$player_id)
head(goals_scored_by_player)

df_goals_scored <- setNames(as.data.frame(goals_scored_by_player), c("player_id", "goals_scored"))
head(df_goals_scored)

# sorting (or arranging) the df by how many goals_scored by each player
#head(df_goals_scored %>% arrange(desc(goals_scored)))

# Joining (or merging) two tables with each other
players_merged <- merge(players, df_goals_scored, by = "player_id")
head(players_merged,5)

# Sorting the data frame in a descending form
players_merged <- players_merged %>% arrange(desc(goals_scored))
head(players_merged)
#players_merged <- players_merged[!duplicated(players_merged), ]

# extracting only the top 6 goal scorers
top_6_scorers <- head(players_merged,6)
top_6_scorers
# -> Klose scored the most amount of goals (16)

# plotting the top 6 goal scorers
ggplot(top_6_scorers, aes(x = family_name, y = goals_scored)) + geom_col()


#---------------------------------------------------------------------------------

### Checking which player had the most red cards
head(bookings)

red_card_by_player <- bookings %>% select(player_id, family_name, given_name,yellow_card, red_card)
head(red_card_by_player)
red_cards <- red_card_by_player %>% filter(red_card > 0)
head(red_cards)
# how many red cards in the wc
nrow(red_cards) 

#amount_of_red_cards_per_player
n_red_cards <- red_cards %>% count(player_id)
n_red_cards <- n_red_cards %>% arrange(desc(n))
n_red_cards <- setNames(n_red_cards, c("player_id", "number_of_red_cards_given"))
head(n_red_cards)

#merging the two tables together
red_cards_merged <- merge(red_cards, n_red_cards, by = "player_id")
head(red_cards_merged)

red_cards_by_name <- red_cards_merged %>% select(family_name, given_name, number_of_red_cards_given)
head(red_cards_by_name)

red_cards_by_name <- head(red_cards_by_name %>% arrange(desc(number_of_red_cards_given)))
red_cards_by_name

#removing duplicates
red_cards_by_name[!duplicated(red_cards_by_name), ]
#-> Zidane and Rigobert have received the most red Cards in WC history (2)



#---------------------------------------------------------------------------------

### Checking which team had the most WCs and which team made it the most to top 4

# Checking the df
head(tournament_standings)

# Extracting only the champions from each wc from the df

champions_df <- tournament_standings %>% filter(position == 1)
head(champions_df)
n_wc_won <- champions_df %>% count(team_name)
n_wc_won <- n_wc_won %>% arrange(desc(n))
n_wc_won <- setNames(n_wc_won, c("team_name", "number_of_wc_won"))
n_wc_won

# Here we have to change "west germany" to "germany"
n_wc_won <- champions_df %>% mutate(team_name = replace(team_name, team_name == "West Germany", "Germany"))
n_wc_won
n_wc_won <- n_wc_won %>% count(team_name)
n_wc_won <- n_wc_won %>% arrange(desc(n))
n_wc_won <- setNames(n_wc_won, c("team_name", "number_of_wc_won"))
head(n_wc_won)
# plotting the teams that won the wc
ggplot(n_wc_won, aes(x = team_name, y = number_of_wc_won)) + geom_col()


## Checking the team that made it the most to the top 4 (semi final)
head(tournament_standings)

# Replacing west germany to germany
top_4_teams <- tournament_standings %>% mutate(team_name = replace(team_name, team_name == "West Germany", "Germany"))
top_4_teams <- top_4_teams %>% count(team_name)
top_4_teams <- top_4_teams %>% arrange(desc(n))
top_4_teams <- setNames(top_4_teams, c("team_name", "semi_finals"))
# Plotting the top 4 teams that made it the most to the semi finals
top_4_teams <- head(top_4_teams, 4)
top_4_teams

ggplot(top_4_teams, aes(x = team_name, y = semi_finals)) + geom_col()



# ----------------------------------------------------------------------------------

### The Player with the most appearances 
head(player_appearances)
players <- player_appearances %>% select(player_id, family_name, given_name)
head(players)

players_games <- players %>% count(player_id)
players_games <- setNames(players_games, c("player_id", "number_of_games"))
head(players_games)

players_games_merged <- merge(players, players_games, by = "player_id")
players_games_merged <- players_games_merged %>% arrange(desc(number_of_games))

players_games_merged <- players_games_merged[!duplicated(players_games_merged), ]
head(players_games_merged)

# -> Matthäus Lothar played the most games (25 games)

# -----------------------------------------------------------------------------------------

### checking the biggest stadiums from the wc (capacity wise)
head(stadiums)

stadium <- stadiums %>% select(stadium_id, stadium_name, country_name, stadium_capacity)
head(stadium)
stadium <- stadium %>% arrange(desc(stadium_capacity))
head(stadium)
# -> it used to have 200000 capacity, but due to renovations it has been reduced

# ----------------------------------------------------------------------------------------

### Checking out the data about the Host winning the World Cup
head(tournaments)
nrow(tournaments)
tournament <- tournaments %>% select(tournament_name, winner, host_country, host_won)
head(tournament)
# how many times did the host win the wc
sum_of_host_won = sum(tournament$host_won)
# probability of a host winning the world cup
sum_of_host_won/nrow(tournament)


# ----------------------------------------------------------------------------------------

### Checking how many teams from represented its continent's  confederation/association
head(teams)
team <- teams %>% select(team_name, confederation_name)
head(team)

# grouping the table by the different confederations

confederation <- group_by(team, confederation_name)
region <- table(confederation$confederation_name)
df_regions <- setNames(as.data.frame(region), c("team_name", "n"))
head(df_regions)
# sorting (or arranging) the df by how many teams represented the different confederations in the wc
head(df_regions %>% arrange(desc(n))) #absteigende Reihenfolge
# plotting
ggplot(df_regions, aes(x = team_name, y = n)) + geom_col()


