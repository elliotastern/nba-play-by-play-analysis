#############################################################
## NBA Team performance with player not playing (Title: Miss Me?)
## Elliot Stern stern.elliot@gmail.com - July 2021
#############################################################

##############################
# 0 - LOAD PACKAGE   
##############################
library('flexdashboard') #0.5.2
library('tidyverse') #1.2.1
library('magrittr') #2.0.1
library('data.table') #1.12.6
library('plotly') #4.9.4

color_pallette <- c("#069E2D", "#DB162F")

############################## 
# 1 - LOAD DATA
##############################
nba_player_box_score_df <- read.csv("2020_games_details.csv",
                                    stringsAsFactors = FALSE)

nba_team_box_score_df <- read.csv("2020_box_scores.csv",
                                  stringsAsFactors = FALSE)

player_details_df <- read.csv("nba_2020_player_details.csv",
                              stringsAsFactors = FALSE)

player_name_translator <- read.csv("translator_player_names.csv",
                                   stringsAsFactors = FALSE)


############################## 
# 2 - DATA CLEANING
############################## 
## CLEAN DATAFRAMES
# make the date columns the same in both dataframes
nba_team_box_score_df$GAME_DATE_EST <- as.Date(nba_team_box_score_df$GAME_DATE_EST, format = "%m/%d/%y")
nba_player_box_score_df$Date <- as.Date(nba_player_box_score_df$Date, format = "%m/%d/%y")

# remove players who did not play
nba_player_box_score_df <- nba_player_box_score_df[!is.na(nba_player_box_score_df$FG3M),]
# create data team id to group by so we can create lineups for each game
nba_player_box_score_df$DATE_TEAM_ID <- paste(nba_player_box_score_df$Date, nba_player_box_score_df$TEAM_ABBREVIATION)
# get the lineup by for each team and date
nba_player_box_score_df %<>%
  group_by(DATE_TEAM_ID) %>%
  mutate(lineup = list(PLAYER_NAME))
# create a dataframe of playing lineup for each team on each date
lineup_df <- nba_player_box_score_df[, c("Date", "TEAM_ABBREVIATION", "lineup", "DATE_TEAM_ID")]
lineup_df <- distinct(lineup_df, DATE_TEAM_ID, .keep_all = TRUE)


## CREATE DATAFRAME of players who played
# create a dataframe of all nba players and their teams so we can connect the data
nba_player_list <- nba_player_box_score_df[, c("PLAYER_NAME", "TEAM_ABBREVIATION")] 
nba_player_list <- distinct(nba_player_list, PLAYER_NAME, TEAM_ABBREVIATION)
nba_player_list$PLAYER_NAME_underlined <- gsub(" ", "_", nba_player_list$PLAYER_NAME)
# View(nba_player_list)



# clean the team box scores so each team has self and opponent stats in each row.
nba_team_box_score_df_away_teams <- nba_team_box_score_df 
nba_team_box_score_df <- nba_team_box_score_df[, c("GAME_DATE_EST", "REAL_GAME_ID",
                                                   "Initials","nickname", "Initials_away",
                                                   "PTS_home", "FG_PCT_home", 
                                                   "FG3_PCT_home", "AST_home", 
                                                   "REB_home", "PTS_away", 
                                                   "FG_PCT_away", "FG3_PCT_away", 
                                                   "AST_away", "REB_away")]