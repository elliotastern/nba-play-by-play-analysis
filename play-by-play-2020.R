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
nba_player_box_score_df <- read.csv("/Users/elliotstern/OneDrive/Sports/nba/NBA_Play_by_Play/2021_games_details.csv",
                                    stringsAsFactors = FALSE)

nba_team_box_score_df <- read.csv("/Users/elliotstern/OneDrive/Sports/nba/NBA_Play_by_Play/2021_box_scores.csv",
                                  stringsAsFactors = FALSE)

player_details_df <- read.csv("/Users/elliotstern/OneDrive/Sports/nba/nba_2021_player_details.csv",
                              stringsAsFactors = FALSE)

player_name_translator <- read.csv("/Users/elliotstern/OneDrive/Sports/nba/NBA_Details/translator_player_names.csv",
                                   stringsAsFactors = FALSE)