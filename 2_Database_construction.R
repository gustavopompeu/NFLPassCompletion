# Necessary packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(raster)
library(caret)

# Loading rdata
# File available on google drive - Read readme.txt
load("data2.rda")

# selecting only the variables we will use
novo <- df_merged[,c("gameId", "playId", "frameId", "nflId", "playDescription", "displayName", "passResult", "homeTeamAbbr", "visitorTeamAbbr", "possessionTeam",
                     "quarter", "down", "yardsToGo", "yardlineSide", "yardlineNumber", "offenseFormation", "defendersInTheBox", "numberOfPassRushers",
                     "typeDropback", "preSnapVisitorScore", "preSnapHomeScore", "gameClock", "penaltyCodes", "position", "team", "x", "y", "event")]

merged_sack <- novo %>%
  #removing sacks, plays with penalties, plays that were spikes or throwaways, and observations that had NAs in some variables
  filter(passResult !="S",
         is.na(penaltyCodes),
         !is.na(offenseFormation),
         !is.na(defendersInTheBox),
         !is.na(gameClock),
         !str_detect(playDescription, "spiked"),
         !str_detect(playDescription, " away")) %>%
  dplyr::select(-c(penaltyCodes)) %>%
  # filtering only the frames between the events of pass forward and the result of the play
  group_by(gameId, playId) %>%
  mutate(PassFrame = frameId[event == "pass_forward" | event == "pass_shovel"][1]) %>%
  mutate(OutcomeFrame = frameId[event == "pass_outcome_caught" | event == "pass_outcome_touchdown" | event == "pass_outcome_incomplete" | event == "pass_outcome_interception"][1]) %>%
  filter(frameId >= PassFrame & frameId <= OutcomeFrame) %>%
  ungroup() %>%
  # excluding variables we will not use anymore
  dplyr::select(-c(PassFrame, OutcomeFrame, event))

# Creating a dataframe with only gameId, playId and playDescription to obtain the passers and targets of the plays
playDesc <- merged_sack %>%
  dplyr::select(gameId,playId,playDescription) %>%
  group_by(gameId,playId) %>%
  slice(1)

# Obtaining the passers and targets of the plays
passer <- c()
target <- c()
for(i in 1:length(playDesc$playDescription)){
  # Separating sentences
  sep1 <- str_extract_all(playDesc$playDescription[i], boundary("sentence"))
  # Creating a lot of exceptions that were necessary to get the players right for every play
  if((str_detect(playDesc$playDescription[i], "eligible.+-.+FUMBLES, and recovers") == T)){
    sep1 <- sep1[[1]][4]
  }else if((str_detect(playDesc$playDescription[i], "eligible") == T) | 
           (str_detect(playDesc$playDescription[i], "Direct snap to [A-Z]") == T) | 
           (str_detect(playDesc$playDescription[i], "#7 - T.Hill") == T) | 
           (str_detect(playDesc$playDescription[i], "\\(Aborted\\)") == T) | 
           (str_detect(playDesc$playDescription[i], "Jackson at QB") == T) | 
           (str_detect(playDesc$playDescription[i], "4-C.Keenum") == T) | 
           (str_detect(playDesc$playDescription[i], "3-J.Rosen") == T) | 
           (str_detect(playDesc$playDescription[i], "N.Peterman.+at.+QB\\.") == T) | 
           (str_detect(playDesc$playDescription[i], "Sweat of Philadelphia has an ankle injury") == T) | 
           (str_detect(playDesc$playDescription[i], "Ball spotted at V47") == T) | 
           (str_detect(playDesc$playDescription[i], "S.Darnold returns at QB") == T)){
    sep1 <- sep1[[1]][2]
  }else if((str_detect(playDesc$playDescription[i], "Aborted") == T) | 
           (str_detect(playDesc$playDescription[i], "FUMBLES \\(D.Hand\\)") == T) | 
           (str_detect(playDesc$playDescription[i], "FUMBLES \\(F.Clark\\)") == T) | 
           (str_detect(playDesc$playDescription[i], "FUMBLES \\(S.Ebukam\\)") == T) | 
           (str_detect(playDesc$playDescription[i], "No. 5 - Joshua Dobbs") == T) | 
           (str_detect(playDesc$playDescription[i], "N.Martin to HST 22 for") == T) | 
           (str_detect(playDesc$playDescription[i], "-.+FUMBLES, and recovers") == T)){
    sep1 <- sep1[[1]][3]
  }else if((str_detect(playDesc$playDescription[i], "sacked.+for.+for.+REVERSED") == T) | 
           (str_detect(playDesc$playDescription[i], "C.Kessler right end to HST 5 for 1 yard") == T)){
    sep1 <- sep1[[1]][5]
  }else if((str_detect(playDesc$playDescription[i], "sacked.+REVERSED") == T)){
    sep1 <- sep1[[1]][4]
    # Or else the standard
  }else{
    sep1 <- sep1[[1]][1]
  }
  # More exceptions
  if(str_detect(sep1, "\\[") == T){
    sep2 <- str_extract_all(sep1, ".+(?<=\\[)")
    sep2 <- sep2[[1]][1]
    a <- strsplit(sep2, " ")
  }else{
    a <- strsplit(sep1, " ")
  }
  # Now getting the pattern, e.g., "M.Ryan"
  aa <- c()
  for(j in 1:length(a[[1]])){
    aa[j] <- str_detect(a[[1]][j], "[A-Z]\\..+?[a-z]") | str_detect(a[[1]][j], "[A-Z][a-z]\\..+?[a-z]")
  }
  certo <- a[[1]][aa]
  if(length(certo) > 0){
    for(k in 1:length(certo)){
      if(str_sub(certo[k], nchar(certo[k])) == "."){
        certo[k] <- str_sub(certo[k], 1, nchar(certo[k])-1)
      }
    }
  }else{
    passer[i] <- NA
    target[i] <- NA
  }
  # More exceptions
  if((str_detect(sep1, "#7 B.Gabbert") == T) |
     (str_detect(sep1, "\\{J.Flacco") == T)){
    passer[i] <- certo[2]
    target[i] <- certo[3]
  }else if((str_detect(sep1, "Dam.") == T)){
    passer[i] <- certo[1]
    target[i] <- "Dam.Williams"
  }else if((str_detect(sep1, "Dar\\.") == T)){
    passer[i] <- certo[1]
    target[i] <- "Dar.Williams"
  }else if((str_detect(sep1, "deep right \\(D.Hopkins\\)") == T) |
           (str_detect(sep1, "short right \\(M.Davenport\\)") == T) | 
           (str_detect(sep1, "short middle \\(D.Lowry\\)") == T) | 
           (str_detect(sep1, "deep middle \\(S.Neasman\\)") == T)){
    passer[i] <- certo[1]
    target[i] <- certo[3]
  }else{
    passer[i] <- certo[1]
    target[i] <- certo[2]
  }
}

# Joining the info about passers and targets with the rest of the database
playDesc$passer <- passer
playDesc$target <- target
merged_sack <- left_join(merged_sack, playDesc, by=c("gameId", "playId"))

# Creating a dataframe with only nflId and displayName to obtain the players names in the same pattern of passers and targets
playerNames <- merged_sack %>%
  dplyr::select(nflId, displayName) %>%
  group_by(nflId) %>%
  slice(1)

# Obtaining the players names in the same pattern of passers and targets
name <- c()
for(i in 1:length(playerNames$displayName)){
  if(is.na(playerNames$nflId[i])){
    name[i] <- "F.NA"
  }else{
    # Specific cases of 2 players (Ty.Williams e De.Thomas)
    if((playerNames$nflId[i] == 497328) | (playerNames$nflId[i] == 2553913)){
      first <- substr(str_split(playerNames$displayName[i],"[:space:]")[[1]][1], 1, 2)
      # Specific cases of 2 players (Dam.Williams e Dar.Williams)
    }else if((playerNames$nflId[i] == 2550512) | (playerNames$nflId[i] == 2560935)){
      first <- substr(str_split(playerNames$displayName[i],"[:space:]")[[1]][1], 1, 3)
    }else{
      first <- substr(str_split(playerNames$displayName[i],"[:space:]")[[1]][1], 1, 1)
    }
    # Specific case of Equanimeous St. Brown
    if(playerNames$nflId[i] == 2560883){
      second <- "St"
    }else{
      second <- str_split(playerNames$displayName[i],"[:space:]")[[1]][2]
    }
    name[i] <- paste(first, second, sep=".")
  }
}

# Joining the info with the rest of the database
playerNames$name <- name
merged_sack <- left_join(merged_sack, playerNames, by="nflId")

# transforming gameClock in seconds
res <- hms(merged_sack$gameClock)
tempo <- hour(res)*60 + minute(res)

novo2 <- merged_sack %>%
  # new variable gameClock in seconds
  mutate(gameClockSec = tempo) %>%
  # changing intercepted passes to incomplete
  mutate(passResult = ifelse(passResult=="IN" | passResult=="I", "I", "C")) %>%
  # creating variable Home (if attacking team is the home team or not)
  mutate(Home = ifelse(homeTeamAbbr == possessionTeam, T, F)) %>%
  # variables that indicate which team is ATK and DEF
  mutate(DefTeam = ifelse(homeTeamAbbr == possessionTeam, visitorTeamAbbr, homeTeamAbbr)) %>%
  rename(AtkTeam = possessionTeam) %>%
  relocate(DefTeam, .after = AtkTeam) %>%
  # FieldPosition: which spot of the field the play starts (from 1 to 99 yds)
  mutate(FieldPosition = ifelse(yardlineNumber == 50, yardlineNumber, ifelse(AtkTeam == yardlineSide, yardlineNumber, 100-yardlineNumber))) %>%
  # how many points the ATK and DEF teams had before the play started
  mutate(preSnapAtkScore = ifelse(homeTeamAbbr == AtkTeam, preSnapHomeScore, preSnapVisitorScore)) %>%
  mutate(preSnapDefScore = ifelse(homeTeamAbbr == AtkTeam, preSnapVisitorScore, preSnapHomeScore)) %>%
  # excluding variables we will not use anymore
  dplyr::select(-c(playDescription.x,playDescription.y, homeTeamAbbr, visitorTeamAbbr, yardlineSide, yardlineNumber, preSnapHomeScore, preSnapVisitorScore, gameClock,
                   AtkTeam, DefTeam)) %>%
  # transforming the team of players from home and away to ATK and DEF
  mutate(team = ifelse(team == "football", "BALL", ifelse((Home == F & team == "away") | (Home == T & team == "home"), "ATK", "DEF")))

novo3 <- novo2 %>%
  group_by(gameId, playId, frameId) %>%
  # Filtering a few plays that didn't have location info of the ball
  filter(sum(displayName.x=="Football") == 1) %>%
  # Gettin ball location on each frame
  mutate(xFootball = x[displayName.x == "Football"],
         yFootball = y[displayName.x == "Football"]) %>%
  ungroup() %>%
  # Leaving the database in order
  arrange(gameId, playId, frameId) %>%
  # Gettin the ball location on previous frame
  group_by(gameId, playId, nflId) %>%
  mutate(xFootballPrev = lag(xFootball),
         yFootballPrev = lag(yFootball)) %>%
  ungroup()

# When the ball doesn't change location (x,y) from one frame to another, the previous (x,y) is from more previous frames
n1 <- 1
n2 <- 2
while(n1 != 0){
  novo3 <- novo3 %>%
    group_by(gameId, playId, nflId) %>%
    mutate(xFootballPrev = ifelse((xFootballPrev==xFootball) & (yFootballPrev==yFootball), lag(xFootball,n2), xFootballPrev),
           yFootballPrev = ifelse((xFootballPrev==xFootball) & (yFootballPrev==yFootball), lag(yFootball,n2), yFootballPrev)) %>%
    ungroup()
  n1 <- sum((novo3$xFootballPrev==novo3$xFootball) & (novo3$yFootballPrev==novo3$yFootball), na.rm=T)
  n2 <- n2+1
}

# Function to calculate distance from point a to line created by points b and c
dist2d <- function(a,b,c) {
  v1 <- c - b
  v2 <- b - a
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
  return(d)
}

novo4 <- novo3 %>%
  group_by(gameId, playId, frameId, nflId) %>%
  # There were plays with duplicated players, so taking off these duplications with slice
  slice(1) %>%
  # Calculating distance from players to line projection of ball direction
  mutate(DistToProj = dist2d(c(x,y), c(xFootball, yFootball), c(xFootballPrev, yFootballPrev)),
         # In cases that a player is exactly on top of the line, distance changes to 0.01 instead of 0
         DistToProj = ifelse(DistToProj==0, 0.01, DistToProj)) %>%
  # Calculating the real (euclidian) distance of each player to ball location
  mutate(DistReal = pointDistance(c(x,y), c(xFootball, yFootball), lonlat = F)) %>%
  ungroup() %>%
  # Taking of the lines of ball information
  filter(displayName.x!="Football") %>%
  dplyr::select(-c(displayName.x, displayName.y)) %>%
  # Transforming the passer from ATK to PASS
  group_by(gameId, playId, frameId) %>%
  mutate(team = ifelse(passer==name, "PASS", team)) %>%
  # Checking if the ball went backwards from one frame to another
  mutate(ballback = xFootball-xFootballPrev) %>%
  ungroup() %>%
  # Calculating the distance difference from player to ball from one frame to another
  group_by(gameId, playId, nflId) %>%
  mutate(DistRealDiff = DistReal - lag(DistReal)) %>%
  ungroup() %>%
  # Filtering off the first frame on each play
  filter(!is.na(DistRealDiff)) %>%
  # Filtering off initial frames until the ball starts to move
  filter(!is.na(DistToProj))

# Filtering for each play to start only when the ball really starts going forward
nr1 <- 1
nr2 <- 2
while(nr1 != nr2){
  nr1 <- nrow(novo4)
  novo4 <- novo4 %>%
    group_by(gameId, playId, nflId) %>%
    mutate(ballbackT = ballback <= 0) %>%
    group_by(gameId, playId, nflId) %>%
    mutate(ballbackbeg = ifelse(frameId == min(frameId) & ballbackT == T, T, F)) %>%
    ungroup() %>%
    filter(!ballbackbeg)
  nr2 <- nrow(novo4)
}

# Taking out variables that we won't use anymore
novo4 <- novo4 %>%
  dplyr::select(-c(ballback,ballbackT,ballbackbeg))

# Calling this database with another name, it will be utilized to create the base for the pass completion prediction models
novoDEF <- novo4
# changing because the passer was Russell Wilson but there was a Ramik Wilson  on the defense and he also became PASS (passer)
novoDEF[novoDEF$gameId==2018100711 & novoDEF$playId==2566 & novoDEF$nflId==2552320,"team"] <- "DEF"