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



# clean the team box scores so each team has self and opponent stats in each row.
nba_team_box_score_df_away_teams <- nba_team_box_score_df 
nba_team_box_score_df <- nba_team_box_score_df[, c("GAME_DATE_EST", "REAL_GAME_ID",
                                                   "Initials","nickname", "Initials_away",
                                                   "PTS_home", "FG_PCT_home", 
                                                   "FG3_PCT_home", "AST_home", 
                                                   "REB_home", "PTS_away", 
                                                   "FG_PCT_away", "FG3_PCT_away", 
                                                   "AST_away", "REB_away")]

nba_team_box_score_df %<>% 
  rename(
    Initials = Initials,
    Opponent_Initials = Initials_away,
    Points_Scored = PTS_home,
    Field_Goal_Percentage = FG_PCT_home,
    Three_Point_Percentage = FG3_PCT_home,
    Assists = AST_home,
    Rebounds = REB_home,
    Opp_Points_Scored = PTS_away,
    Opp_Field_Goal_Percentage = FG_PCT_away,
    Opp_Three_Point_Percentage = FG3_PCT_away,
    Opp_Assists = AST_away,
    Opp_Rebounds = REB_away
  )

nba_team_box_score_df_away_teams <- nba_team_box_score_df_away_teams[, c("GAME_DATE_EST", "REAL_GAME_ID",
                                                                         "Initials_away", "Initials", "nickname_away",
                                                                         "PTS_away", "FG_PCT_away", 
                                                                         "FG3_PCT_away", "AST_away", 
                                                                         "REB_away", "PTS_home", 
                                                                         "FG_PCT_home", "FG3_PCT_home", 
                                                                         "AST_home", "REB_home")]
nba_team_box_score_df_away_teams %<>% 
  rename(
    Opponent_Initials = Initials,
    Initials = Initials_away,
    nickname = nickname_away,
    Points_Scored = PTS_away,
    Field_Goal_Percentage = FG_PCT_away,
    Three_Point_Percentage = FG3_PCT_away,
    Assists = AST_away,
    Rebounds = REB_away,
    Opp_Points_Scored = PTS_home,
    Opp_Field_Goal_Percentage = FG_PCT_home,
    Opp_Three_Point_Percentage = FG3_PCT_home,
    Opp_Assists = AST_home,
    Opp_Rebounds = REB_home,
  )


# Join the tables into one
nba_team_box_score_df <- bind_rows(nba_team_box_score_df, 
                                   nba_team_box_score_df_away_teams)

remove(nba_team_box_score_df_away_teams)

nba_team_box_score_df$DATE_TEAM_ID <- paste(nba_team_box_score_df$GAME_DATE_EST, nba_team_box_score_df$Initials)

all_nba_data <- left_join(nba_team_box_score_df, lineup_df, 
                          by = "DATE_TEAM_ID")

nba_team_box_score_df$DATE_TEAM_ID

# Add player details to player list dataframe

# Adjust Names that do not match with a translator 
player_details_df_2 <- left_join(player_details_df, player_name_translator, by = c("PLAYER" = "Source.1"))
player_details_df_2 <- player_details_df_2[!is.na(player_details_df_2$Source.2), ]
player_details_df_2$PLAYER <- player_details_df_2$Source.2
player_details_df_2$Source.2 <- NULL

player_details_df <- bind_rows(player_details_df, player_details_df_2)

nba_player_list <- left_join(nba_player_list, player_details_df, by = c("PLAYER_NAME" = "PLAYER"))

nba_player_list_name_error <- nba_player_list[nba_player_list$POS == "", ]

# remove duplicates and players with low minutes
nba_player_list <- nba_player_list[!nba_player_list$POS == "", ]


############################## 
# 3 - CREATE THE DATAFRAME FOR DATA VIZ
############################## 

# Remove duplicate rows
all_nba_data <- unique(all_nba_data)


for (z in 1:nrow(nba_player_list)){
  player_name <- unlist(nba_player_list[z, 1])
  teams <- unlist(nba_player_list[nba_player_list$PLAYER_NAME == player_name, "TEAM_ABBREVIATION"])
  player_underlined <- unlist(nba_player_list[z,3])
  all_nba_data[player_underlined] <- "Not On Team"
  all_nba_data[all_nba_data$Initials %in% teams, player_underlined] <- "Did Not Play"
  
  for (i in 1:nrow(all_nba_data)){
    if(player_name %in% unlist(all_nba_data$lineup[i]) == TRUE){
      all_nba_data[i, player_underlined] <- "Played"
    }
  }
}

# Add statistics
all_nba_data$Point_Differential <- all_nba_data$Points_Scored - all_nba_data$Opp_Points_Scored
all_nba_data$Rebounds_Differential <- all_nba_data$Rebounds - all_nba_data$Opp_Rebounds
all_nba_data$Assists_Differential <- all_nba_data$Assists - all_nba_data$Opp_Assists

all_nba_data$lineup <- vapply(all_nba_data$lineup, paste, collapse = ", ", character(1L))


# Add player team list
player_position <- nba_player_list[nba_player_list$PLAYER_NAME == player_name_space, "POS"]
all_nba_data$GAME_DATE_EST <- as.Date(all_nba_data$GAME_DATE_EST, format = "%Y-%m-%d")

the_teams_list <- c()
player_team_list <- function(the_player_name = "James_Harden"){
  all_nba_data_filtered <- all_nba_data[!all_nba_data[the_player_name] == "Not On Team", ]
  player_name_df <- all_nba_data_filtered %>% count(nickname)
  teams <- list(player_name_df$nickname)
  the_teams_list <- c(the_teams_list, teams)
  
  the_teams_list <<- the_teams_list
}


sapply(nba_player_list$PLAYER_NAME_underlined, player_team_list)
nba_player_list$teams_list <- the_teams_list

nba_player_list <- unnest(nba_player_list, cols = c(teams_list))
nba_player_list <- nba_player_list[!duplicated(nba_player_list[, c("PLAYER_NAME", "teams_list")]), ]


############################## 
# 3.1 Get player means
############################## 
player_mean_df <- function(player_name = "CJ_McCollum", player_team = "Trail Blazers", player_pos = "SG"){
  
  all_nba_data_filtered <- all_nba_data[all_nba_data["nickname"] == player_team, ]
  
  # remove rows were player was not on the ream
  all_nba_data_filtered <- all_nba_data[!all_nba_data[player_name] == "Not On Team", ]
  all_nba_data_filtered <- all_nba_data[all_nba_data["nickname"] == player_team, ]
  
  # split into played and not played
  play_mean <- all_nba_data_filtered[all_nba_data_filtered[player_name] == "Played", ]
  not_playing_mean <- all_nba_data_filtered[all_nba_data_filtered[player_name] == "Did Not Play", ]
  
  # show games played
  games_played <- nrow(play_mean)
  games_out <- nrow(not_playing_mean)
  
  
  # get the means
  play_means <- play_mean %>%
    summarise_at(c("Point_Differential", "Points_Scored", "Opp_Points_Scored",
                   "Assists_Differential", "Assists", "Opp_Assists",
                   "Rebounds_Differential", "Rebounds", "Opp_Rebounds",
                   "Field_Goal_Percentage", "Opp_Field_Goal_Percentage",
                   "Three_Point_Percentage", "Opp_Three_Point_Percentage"), mean, na.rm = TRUE)
  
  not_playing_means <- not_playing_mean %>%
    summarise_at(c("Point_Differential", "Points_Scored", "Opp_Points_Scored",
                   "Assists_Differential", "Assists", "Opp_Assists",
                   "Rebounds_Differential", "Rebounds", "Opp_Rebounds",
                   "Field_Goal_Percentage", "Opp_Field_Goal_Percentage",
                   "Three_Point_Percentage", "Opp_Three_Point_Percentage"), mean, na.rm = TRUE)  
  
  all_means <- bind_rows(play_means, not_playing_means)
  
  # Clean column for display
  colnames(all_means) <-  gsub("_", " ", colnames(all_means)) 
  # percentage to % to save space
  colnames(all_means) <-  gsub("Percentage", "%", colnames(all_means)) 
  
  all_means[, grepl("%", colnames(all_means))] <- all_means[, grepl("%", colnames(all_means))] * 100
  
  all_means_dt <- all_means
  
  all_means <- t(all_means)
  
  
  all_means_clean <- data.frame(stat = row.names(all_means),
                                number = all_means[, 1],
                                played = "Played")
  all_means_clean_2 <- data.frame(stat = row.names(all_means),
                                  number = all_means[, 2],
                                  played = "Did Not Play")
  
  all_means_clean_dif <- left_join(all_means_clean, all_means_clean_2, by = "stat")
  all_means_clean_dif$difference <- all_means_clean_dif$number.x - all_means_clean_dif$number.y
  all_means_clean_dif <- all_means_clean_dif[, c("stat", "difference")]
  
  all_means_clean <- bind_rows(all_means_clean,
                               all_means_clean_2)
  
  
  
  all_means_clean <- left_join(all_means_clean, all_means_clean_dif)
  
  
  # CREATE DIFFERENCE TABLE
  # get the difference
  all_means_dt[3, ] <- all_means_dt[1, ] - all_means_dt[2, ]
  
  # round the values
  all_means_dt_rounded <- data.frame(sapply(all_means_dt, round, 2))
  
  colnames(all_means_dt_rounded) <-  colnames(all_means_dt)
  
  all_means_dt_rounded$type <- c("Played", "Did Not Play", "Difference")
  
  all_means_dt_rounded$output_file = paste0("elliotastern.com/NBA/", player_name, "_", player_team, "_Miss_Him-2020.html")
  all_means_dt_rounded$PLAYER_NAME <- gsub("_", " ", player_name)
  all_means_dt_rounded$Position <- player_pos
  
  
  #add games played
  all_means_dt_rounded$games_played <- games_played
  all_means_dt_rounded$games_out <- games_out
  all_means_dt_rounded$Team <- player_team
  
  mean_df <- bind_rows(mean_df, all_means_dt_rounded)
  
  mean_df <<- mean_df
}

mean_df <- data.frame()

mapply(player_mean_df, nba_player_list$PLAYER_NAME_underlined, nba_player_list$teams_list, nba_player_list$POS)

# filter out small sample sizes
mean_df$small_sample <- mean_df$games_played < 6 | mean_df$games_out < 6


# difference dataframe analysis
difference_df <- mean_df[mean_df$type == "Difference", ]

############################## 
# 3 - DATA VIZ
############################## 


#Stephen Curry
all_nba_data_filtered <- all_nba_data[!all_nba_data[unlist(nba_player_list[1, 3])] == "Not On Team", ]

# all_nba_data_filtered <- all_nba_data[!all_nba_data[unlist(nba_player_list[100, 1])] == "Not On Team", ]



# final score difference bar chart colored by player playing
all_nba_data_filtered$Points_Scored
all_nba_data_filtered$Opp_Points_Scored
all_nba_data_filtered$Point_Differential
all_nba_data_filtered$GAME_DATE_EST
all_nba_data_filtered$Assists
all_nba_data_filtered$Rebounds


played <- all_nba_data_filtered[all_nba_data_filtered$`Stephen_Curry` == "Played", ]
did_not_play <- all_nba_data_filtered[all_nba_data_filtered$`Stephen_Curry` == "Did Not Play", ]


ggplot(all_nba_data_filtered, aes(GAME_DATE_EST, Point_Differential, color = `Stephen_Curry`)) +
  geom_point()

ggplot(data = all_nba_data_filtered, aes(x = GAME_DATE_EST, y = Point_Differential, color = `Stephen Curry`)) +
  geom_bar(stat = "identity")

ggplot(data = all_nba_data_filtered, aes(x = GAME_DATE_EST, y = Point_Differential, color = `Stephen Curry`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = REAL_GAME_ID), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  coord_flip()

mean(all_nba_data_filtered$Point_Differential[all_nba_data_filtered$`Stephen Curry` == "Played"])
mean(all_nba_data_filtered$Point_Differential[all_nba_data_filtered$`Stephen Curry` == "Did Not Play"])


ggplot(data = all_nba_data_filtered, aes(x = GAME_DATE_EST, y = Point_Differential, fill = `Stephen Curry`)) +
  geom_bar(stat = "identity") + 
  annotate(geom = 'text', size = 3, label = paste(round(mean(did_not_play$Point_Differential), 2), " When Playing"), x = max(all_nba_data_filtered$GAME_DATE_EST) + 10, y = Inf, hjust = 0, vjust = 1) +
  annotate(geom = 'text', size = 3, label = paste(round(mean(played$Point_Differential), 2), " When Not Playing"), x = max(all_nba_data_filtered$GAME_DATE_EST) + 10, y = Inf, hjust = 0, vjust = 3) +
  coord_cartesian(xlim = c(min(all_nba_data_filtered$GAME_DATE_EST), max(all_nba_data_filtered$GAME_DATE_EST)), # This focuses the x-axis on the range of interest
                  clip = 'off') +
  geom_hline(yintercept = mean(all_nba_data_filtered$Point_Differential[all_nba_data_filtered$`Stephen Curry` == "Played"]), linetype="dashed", 
             color = color_pallette[2], size= .5) +
  geom_hline(yintercept = mean(all_nba_data_filtered$Point_Differential[all_nba_data_filtered$`Stephen Curry` == "Did Not Play"]), linetype="dashed", 
             color = color_pallette[1], size= .5) +
  theme(text = element_text(family="Roboto")) +
  labs(title = paste("Stephen Curry:", "Point Differential"), 
       subtitle = "Miss Me?",
       y = "Point Differential",
       x = "Date") +
  scale_fill_manual(values=c(color_pallette[1], color_pallette[2])) +
  theme(plot.background = element_rect(colour = 'black', fill = 'black'),
        line = element_line(colour = "white", size = 0.5, linetype = 1, 
                            lineend = "butt"), 
        rect = element_rect(fill = "white", 
                            colour = "white", size = 0.5, linetype = 1),
        panel.border = element_rect(fill = NA, colour = "white") 
  )


ggplot(data = all_nba_data_filtered, aes(x = GAME_DATE_EST, y = Point_Differential, fill = `Stephen Curry`)) +
  geom_bar(stat = "identity") + 
  annotate(geom = 'text', size = 3, label = paste(round(mean(did_not_play$Point_Differential), 2), " When Playing"), x = max(all_nba_data_filtered$GAME_DATE_EST) + 10, y = Inf, hjust = 0, vjust = 1) +
  annotate(geom = 'text', size = 3, label = paste(round(mean(played$Point_Differential), 2), " When Not Playing"), x = max(all_nba_data_filtered$GAME_DATE_EST) + 10, y = Inf, hjust = 0, vjust = 3) +
  coord_cartesian(xlim = c(min(all_nba_data_filtered$GAME_DATE_EST), max(all_nba_data_filtered$GAME_DATE_EST)), # This focuses the x-axis on the range of interest
                  clip = 'off') +
  geom_hline(yintercept = mean(all_nba_data_filtered$Point_Differential[all_nba_data_filtered$`Stephen Curry` == "Played"]), linetype="dashed", 
             color = color_pallette[2], size= .5) +
  geom_hline(yintercept = mean(all_nba_data_filtered$Point_Differential[all_nba_data_filtered$`Stephen Curry` == "Did Not Play"]), linetype="dashed", 
             color = color_pallette[1], size= .5) +
  dark_theme_gray() +
  theme(text = element_text(family="Roboto"),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2)) +
  labs(title = paste("Stephen Curry:", "Point Differential"), 
       subtitle = "Miss Me?",
       y = "Point Differential",
       x = "Date") +
  scale_fill_manual(values=c(color_pallette[1], color_pallette[2])) 



all_nba_data_filtered$`Stephen Curry` <- factor(all_nba_data_filtered$`Stephen Curry`, levels=c("Played", "Did Not Play"))

p <- ggplot(data = all_nba_data_filtered, aes(x = GAME_DATE_EST, y = Point_Differential, fill = `Stephen Curry`, Opponent_Initials = Opponent_Initials, Points_Scored = Points_Scored, Opp_Points_Scored = Opp_Points_Scored)) +
  geom_bar(stat = "identity", width = .8) + 
  annotate(geom = 'text', size = 4, label = paste(round(mean(did_not_play$Point_Differential), 2), "Point Differential", "When Not Playing"), x = mean(all_nba_data_filtered$GAME_DATE_EST), y = min(all_nba_data_filtered$Point_Differential), hjust = 0, vjust = 0) +
  annotate(geom = 'text', size = 4, label = paste(round(mean(played$Point_Differential), 2), "Point Differential", "When Playing"), x = mean(all_nba_data_filtered$GAME_DATE_EST), y = min(all_nba_data_filtered$Point_Differential) + 3, hjust = 0, vjust = 0) +
  coord_cartesian(xlim = c(min(all_nba_data_filtered$GAME_DATE_EST), max(all_nba_data_filtered$GAME_DATE_EST)), # This focuses the x-axis on the range of interest
                  clip = 'off') +
  geom_hline(yintercept = mean(all_nba_data_filtered$Point_Differential[all_nba_data_filtered$`Stephen Curry` == "Played"]), linetype="dashed", 
             color = color_pallette[2], size= .5) +
  geom_hline(yintercept = mean(all_nba_data_filtered$Point_Differential[all_nba_data_filtered$`Stephen Curry` == "Did Not Play"]), linetype="dashed", 
             color = color_pallette[1], size= .5) +
  dark_theme_gray() +
  theme(text = element_text(family="Roboto"),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2)) +
  labs(title = paste("Stephen Curry:", "Point Differential"), 
       subtitle = "Miss Me?",
       y = "Point Differential",
       x = "Date",
       caption = ) +
  scale_fill_manual(values=c(color_pallette[1], color_pallette[2])) +
  guides(colour = guide_legend(reverse = T))

ggplotly(p, tooltip = c("Opponent_Initials", "Points_Scored", "Opp_Points_Scored"))






filtered_gg <- function(the_stat = Point_Differential, tester = Stephen_Curry){
  tester <- enquo(tester)
  the_stat <- enquo(the_stat)
  
  title_name <- gsub("_", " ", quo_name(tester))
  title_stat <- gsub("_", " ", quo_name(the_stat))
  
  #filter to only games of teams the player was on this season
  all_nba_data_filtered <- all_nba_data[!all_nba_data[quo_name(tester)] == "Not On Team", ]
  
  # change to factor to reorder legend
  all_nba_data_filtered[quo_name(tester)] <- factor(all_nba_data_filtered[[quo_name(tester)]], levels=c("Played", "Did Not Play"))
  
  play_mean <- all_nba_data_filtered[all_nba_data_filtered[quo_name(tester)] == "Played", quo_name(the_stat)]
  not_playing_mean <- all_nba_data_filtered[all_nba_data_filtered[quo_name(tester)] == "Did Not Play", quo_name(the_stat)]
  
  
  p <- ggplot(data = all_nba_data_filtered, aes(x = GAME_DATE_EST, y = !! the_stat, fill = !! tester, Opponent_Initials = Opponent_Initials, Points_Scored = Points_Scored, Opp_Points_Scored = Opp_Points_Scored)) +
    geom_bar(stat = "identity", width = .8) +
    # Change to dark theme
    dark_theme_gray() +
    theme(text = element_text(family="Roboto"),
          panel.grid.major = element_line(color = "grey30", size = 0.2),
          panel.grid.minor = element_line(color = "grey30", size = 0.2)) +
    # Change titles
    labs(title = paste("Miss Me?", title_name, ":", title_stat),
         y = quo_name(the_stat),
         x = "Date",
         fill = title_name) +
    # Change colors
    scale_fill_manual(values=c(color_pallette[1], color_pallette[2])) +
    # Remove Excess X-Axis 
    coord_cartesian(xlim = c(min(all_nba_data_filtered$GAME_DATE_EST), max(all_nba_data_filtered$GAME_DATE_EST)), # This focuses the x-axis on the range of interest
                    clip = 'off') +
    geom_hline(yintercept = mean(unlist(play_mean)), linetype="dashed", 
               color = color_pallette[2], size= .5) +
    geom_hline(yintercept = mean(unlist(not_playing_mean)), linetype="dashed", 
               color = color_pallette[1], size= .5) +
    annotate(geom = 'text', size = 4, label = paste(round(mean(unlist(play_mean)), 2), title_stat, "When Playing"), x = mean(all_nba_data_filtered$GAME_DATE_EST), y = max(all_nba_data_filtered[[quo_name(the_stat)]]) * 1.06, hjust = 0, vjust = 0) +
    annotate(geom = 'text', size = 4, label = paste(round(mean(unlist(not_playing_mean)), 2), title_stat, "When Not Playing"), x = mean(all_nba_data_filtered$GAME_DATE_EST), y = max(all_nba_data_filtered[[quo_name(the_stat)]]) * 1.04 - 2, hjust = 0, vjust = 0) 
  
  
  
  ggplotly(p, tooltip = c("Opponent_Initials", "Points_Scored", "Opp_Points_Scored"))
  
}

filtered_gg(the_stat = Point_Differential)
filtered_gg(the_stat = Points_Scored)
filtered_gg(the_stat = Opp_Points_Scored)
filtered_gg(the_stat = Assists_Differential)
filtered_gg(the_stat = Assists)
filtered_gg(the_stat = Opp_Assists)
filtered_gg(the_stat = Rebounds_Differential)
filtered_gg(the_stat = Rebounds)
filtered_gg(the_stat = Opp_Rebounds)


stat_vector <- c("Point_Differential", "Points_Scored", "Opp_Points_Scored",
                 "Assists_Differential", "Assists", "Opp_Assists",
                 "Rebounds_Differential", "Rebounds", "Opp_Rebounds")

nba_player_list$PLAYER_NAME_underlined

all_graphs <- expand.grid(stat_vector, nba_player_list$PLAYER_NAME_underlined)


p <- ggplot(data = all_nba_data_filtered, aes(x = GAME_DATE_EST, y = Point_Differential, fill = Eric_Paschall, Opponent_Initials = Opponent_Initials, Points_Scored = Points_Scored, Opp_Points_Scored = Opp_Points_Scored)) +
  geom_bar(stat = "identity", width = .8) #+
# annotate(geom = 'text', size = 3, label = paste(round(mean(did_not_play$Point_Differential), 2), " When Playing"), x = max(all_nba_data_filtered$GAME_DATE_EST) + 10, y = Inf, hjust = 0, vjust = 1) +
# annotate(geom = 'text', size = 3, label = paste(round(mean(played$Point_Differential), 2), " When Not Playing"), x = max(all_nba_data_filtered$GAME_DATE_EST) + 10, y = Inf, hjust = 0, vjust = 3) 
# 
ggplotly(p, tooltip = c("Opponent_Initials", "Points_Scored", "Opp_Points_Scored"))




filtered_gg <- function(the_stat = "Point_Differential", z = 2, tester = "Eric_Paschall"){
  # played <- all_nba_data_filtered[all_nba_data_filtered[[tester]] == "Played", ]
  
  
  # all_nba_data_filtered <- all_nba_data[!all_nba_data[unlist(nba_player_list[z, 1])] == "Not On Team", ]
  # graph_player <- unlist(nba_player_list[z, 1])
  tester <- rlang::sym(tester)
  the_stat <- rlang::sym(the_stat)
  # print(tester)
  # print(quo(tester))
  # did_not_play <- all_nba_data_filtered[all_nba_data_filtered[[!! tester]] == "Did Not Play", ]
  # 
  # ggp <- ggplot(data = all_nba_data_filtered,
  #               aes(x = !! x.var,
  #                   y = !! the_stat)) +
  #   geom_point()
  # 
  # return(ggp)
  
  
  p <- ggplot(data = all_nba_data_filtered, aes(x = GAME_DATE_EST, y = !! the_stat, fill = !! tester, Opponent_Initials = Opponent_Initials, Points_Scored = Points_Scored, Opp_Points_Scored = Opp_Points_Scored)) +
    geom_bar(stat = "identity", width = .8) #+
  # annotate(geom = 'text', size = 3, label = paste(round(mean(did_not_play$Point_Differential), 2), " When Playing"), x = max(all_nba_data_filtered$GAME_DATE_EST) + 10, y = Inf, hjust = 0, vjust = 1) +
  # annotate(geom = 'text', size = 3, label = paste(round(mean(played$Point_Differential), 2), " When Not Playing"), x = max(all_nba_data_filtered$GAME_DATE_EST) + 10, y = Inf, hjust = 0, vjust = 3) 
  # 
  # ggplotly(p, tooltip = c("Opponent_Initials", "Points_Scored", "Opp_Points_Scored"))
  
  # ggplot(data = all_nba_data_filtered, aes(x = GAME_DATE_EST, y = Point_Differential, fill = Eric_Paschall, Opponent_Initials = Opponent_Initials, Points_Scored = Points_Scored, Opp_Points_Scored = Opp_Points_Scored)) +
  #  geom_bar(stat = "identity", width = .8) 
  # ggplot(data = all_nba_data_filtered, aes(x = GAME_DATE_EST, y = Point_Differential, fill = Eric_Paschall, Opponent_Initials = Opponent_Initials, Points_Scored = Points_Scored, Opp_Points_Scored = Opp_Points_Scored)) +
  #  geom_bar(stat = "identity", width = .8)
  
  
  # p <- ggplot(data = all_nba_data_filtered, aes(x = GAME_DATE_EST, y = Point_Differential, fill = `Stephen Curry`, Opponent_Initials = Opponent_Initials, Points_Scored = Points_Scored, Opp_Points_Scored = Opp_Points_Scored)) +
  #   geom_bar(stat = "identity", width = .8) + 
  #   annotate(geom = 'text', size = 3, label = paste(round(mean(did_not_play$Point_Differential), 2), " When Playing"), x = max(all_nba_data_filtered$GAME_DATE_EST) + 10, y = Inf, hjust = 0, vjust = 1) +
  #   annotate(geom = 'text', size = 3, label = paste(round(mean(played$Point_Differential), 2), " When Not Playing"), x = max(all_nba_data_filtered$GAME_DATE_EST) + 10, y = Inf, hjust = 0, vjust = 3) +
  #   coord_cartesian(xlim = c(min(all_nba_data_filtered$GAME_DATE_EST), max(all_nba_data_filtered$GAME_DATE_EST)), # This focuses the x-axis on the range of interest
  #                   clip = 'off') +
  #   geom_hline(yintercept = mean(all_nba_data_filtered$Point_Differential[all_nba_data_filtered$`Stephen Curry` == "Played"]), linetype="dashed", 
  #              color = color_pallette[2], size=.5) +
  #   geom_hline(yintercept = mean(all_nba_data_filtered$Point_Differential[all_nba_data_filtered$`Stephen Curry` == "Did Not Play"]), linetype="dashed", 
  #              color = color_pallette[1], size=.5) +
  #   dark_theme_gray() +
  #   theme(text = element_text(family="Roboto"),
  #         panel.grid.major = element_line(color = "grey30", size = 0.2),
  #         panel.grid.minor = element_line(color = "grey30", size = 0.2)) +
  #   labs(title = paste("Stephen Curry:", "Point Differential"), 
  #        subtitle = "Miss Me?",
  #        y = "Point Differential",
  #        x = "Date") +
  #   scale_fill_manual(values=c(color_pallette[1], color_pallette[2])) 
  # 
  # ggplotly(p, tooltip = c("Opponent_Initials", "Points_Scored", "Opp_Points_Scored"))
  
  
}



filtered_gg(the_stat = Point_Differential)
filtered_gg(the_stat = Points_Scored)
filtered_gg(the_stat = Opp_Points_Scored)
filtered_gg(the_stat = Assists)
filtered_gg(the_stat = Opp_Assists)
filtered_gg(the_stat = Rebounds)
filtered_gg(the_stat = Opp_Rebounds)



p <- ggplot(data = all_nba_data_filtered, aes(x = GAME_DATE_EST, y = Point_Differential, fill = `Stephen Curry`, Opponent_Initials = Opponent_Initials, Points_Scored = Points_Scored, Opp_Points_Scored = Opp_Points_Scored)) +
  geom_bar(stat = "identity", width = .8) + 
  annotate(geom = 'text', size = 3, label = paste(round(mean(did_not_play$Point_Differential), 2), " When Playing"), x = max(all_nba_data_filtered$GAME_DATE_EST) + 10, y = Inf, hjust = 0, vjust = 1) +
  annotate(geom = 'text', size = 3, label = paste(round(mean(played$Point_Differential), 2), " When Not Playing"), x = max(all_nba_data_filtered$GAME_DATE_EST) + 10, y = Inf, hjust = 0, vjust = 3) +
  coord_cartesian(xlim = c(min(all_nba_data_filtered$GAME_DATE_EST), max(all_nba_data_filtered$GAME_DATE_EST)), # This focuses the x-axis on the range of interest
                  clip = 'off') +
  geom_hline(yintercept = mean(all_nba_data_filtered$Point_Differential[all_nba_data_filtered$`Stephen Curry` == "Played"]), linetype="dashed", 
             color = color_pallette[2], size=.5) +
  geom_hline(yintercept = mean(all_nba_data_filtered$Point_Differential[all_nba_data_filtered$`Stephen Curry` == "Did Not Play"]), linetype="dashed", 
             color = color_pallette[1], size=.5) +
  dark_theme_gray() +
  theme(text = element_text(family="Roboto"),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2)) +
  labs(title = paste("Stephen Curry:", "Point Differential"), 
       subtitle = "Miss Me?",
       y = "Point Differential",
       x = "Date") +
  scale_fill_manual(values=c(color_pallette[1], color_pallette[2])) 

ggplotly(p, tooltip = c("Opponent_Initials", "Points_Scored", "Opp_Points_Scored"))





# fig <- fig %>%
#    layout(title = 'AAPL High')




ggplot(data = all_nba_data_filtered, aes(x = GAME_DATE_EST, y = Assists, fill = `Stephen Curry`)) +
  geom_bar(stat = "identity") + 
  annotate(geom = 'text', size = 3, label = paste(round(mean(all_nba_data_filtered$Assists[all_nba_data_filtered$`Stephen Curry` == "Played"]), 2), " When Playing"), x = max(all_nba_data_filtered$GAME_DATE_EST) + 10, y = Inf, hjust = 0, vjust = 1) +
  annotate(geom = 'text', size = 3, label = paste(round(mean(all_nba_data_filtered$Assists[all_nba_data_filtered$`Stephen Curry` == "Did Not Play"]), 2), " When Not Playing"), x = max(all_nba_data_filtered$GAME_DATE_EST) + 10, y = Inf, hjust = 0, vjust = 3) +
  coord_cartesian(xlim = c(min(all_nba_data_filtered$GAME_DATE_EST), max(all_nba_data_filtered$GAME_DATE_EST)), # This focuses the x-axis on the range of interest
                  clip = 'off')



ggplot(data = all_nba_data_filtered, aes(x = GAME_DATE_EST, y = Rebounds, color = `Stephen Curry`)) +
  geom_bar(stat = "identity") 
mean(all_nba_data_filtered$Rebounds[all_nba_data_filtered$`Stephen Curry` == "Played"])
mean(all_nba_data_filtered$Rebounds[all_nba_data_filtered$`Stephen Curry` == "Did Not Play"])

ggplot(data = all_nba_data_filtered, aes(x = GAME_DATE_EST, y = Points_Scored, color = `Stephen Curry`)) +
  geom_bar(stat = "identity") 
mean(all_nba_data_filtered$Points_Scored[all_nba_data_filtered$`Stephen Curry` == "Played"])
mean(all_nba_data_filtered$Points_Scored[all_nba_data_filtered$`Stephen Curry` == "Did Not Play"])

ggplot(data = all_nba_data_filtered, aes(x = GAME_DATE_EST, y = Opp_Points_Scored, color = `Stephen Curry`)) +
  geom_bar(stat = "identity") 
mean(all_nba_data_filtered$Opp_Points_Scored[all_nba_data_filtered$`Stephen Curry` == "Played"])
mean(all_nba_data_filtered$Opp_Points_Scored[all_nba_data_filtered$`Stephen Curry` == "Did Not Play"])

ggplot(data = all_nba_data_filtered, aes(x = GAME_DATE_EST, y = Field_Goal_Percentage, color = `Stephen Curry`)) +
  geom_bar(stat = "identity") 
mean(all_nba_data_filtered$Field_Goal_Percentage[all_nba_data_filtered$`Stephen Curry` == "Played"])
mean(all_nba_data_filtered$Field_Goal_Percentage[all_nba_data_filtered$`Stephen Curry` == "Did Not Play"])

ggplot(data = all_nba_data_filtered, aes(x = GAME_DATE_EST, y = Opp_Field_Goal_Percentage, color = `Stephen Curry`)) +
  geom_bar(stat = "identity") 
mean(all_nba_data_filtered$Opp_Field_Goal_Percentage[all_nba_data_filtered$`Stephen Curry` == "Played"])
mean(all_nba_data_filtered$Opp_Field_Goal_Percentage[all_nba_data_filtered$`Stephen Curry` == "Did Not Play"])



