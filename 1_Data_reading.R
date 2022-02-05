# Necessary packages
library(tidyverse)

############################################### CREATING INITIAL DATABASE ######################################

##reading in non-tracking data

#includes schedule info for games
df_games <- read_csv("games.csv", col_types = cols())

#includes play-by-play info on specific plays
df_plays <- read_csv("plays.csv", col_types = cols())

#includes background info for players
df_players <- read_csv("players.csv", col_types = cols())

##Reading tracking data (needs to be done iteratively)

#weeks of NFL season
weeks <- seq(1, 17)

#blank dataframe to store tracking data
df_tracking <- data.frame()

#iterating through all weeks
for(w in weeks){
  
  #temporary dataframe used for reading week for given iteration
  df_tracking_temp <- read_csv(paste0("week",w,".csv"),
                               col_types = cols())
  
  #storing temporary dataframe in full season dataframe
  df_tracking <- bind_rows(df_tracking_temp, df_tracking)
  
}

#Standardizing tracking data so its always in direction of offense vs raw on-field coordinates.
df_tracking <- df_tracking %>%
  mutate(x = ifelse(playDirection == "left", 120-x, x),
         y = ifelse(playDirection == "left", 160/3 - y, y))

#merging plays and tracking data
df_merged <- inner_join(df_games,
                        df_plays,
                        by = c("gameId" = "gameId"))

#merging games data to previously merged frame
df_merged <- inner_join(df_merged,
                        df_tracking,
                        by = c("gameId" = "gameId",
                               "playId" = "playId"))
