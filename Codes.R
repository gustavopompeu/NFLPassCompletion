# Necessary packages
library(plyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(raster)
library(caret)
library(gganimate)
library(cowplot)
library(repr)
library(randomForest)
library(reshape2)
library(nlme)
library(plotROC)
library(pROC)
library(scales)
library(DescTools)

############################################### CREATING DATABASES ######################################

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

########################################## TARGET PREDICTION #####################################

# Filtering only ATK players
novo4 <- novo4 %>%
  filter(team=="ATK")

novo5 <- novo4 %>%
  # Calculating a probability of players to be the target based on DistToProj
  group_by(gameId,playId,frameId) %>%
  mutate(minDistToProj = min(DistToProj),
         RelDist = minDistToProj/DistToProj,
         ProbMin = 1/sum(RelDist),
         minDistRealDiff = min(DistRealDiff)) %>%
  ungroup() %>%
  group_by(gameId,playId,frameId, nflId) %>%
  # Padronizing for the smaller distance on DistRealDiff to be 1, but keeping the same difference to the others
  mutate(DistRealDiff2 = ifelse(minDistRealDiff>0, 1-minDistRealDiff+DistRealDiff, DistRealDiff+abs(minDistRealDiff)+1)) %>%
  ungroup() %>%
  # Calculating a probability of players to be the target based on DistRealDiff2
  group_by(gameId,playId,frameId) %>%
  mutate(minDistRealDiff2 = min(DistRealDiff2),
         RelDistRealDiff = minDistRealDiff2/DistRealDiff2,
         ProbMin2 = 1/sum(RelDistRealDiff),
         EmpProb1 = RelDist*ProbMin,
         EmpProb2 = RelDistRealDiff*ProbMin2,
         # Combining both probabilities without weight
         EmpProb = 0.5*EmpProb1+0.5*EmpProb2)

# Pegando a DistRealDiff do jogador com a menor DistToProj em cada frame
# Tanto para os valores reais quanto para os padronizados acima
novoW <- novo5 %>%
  group_by(gameId,playId,frameId,nflId) %>%
  mutate(# Gettin the DistRealDiff of the player with the lowest DistToProj in each frame
         distproj_mindiff = ifelse(DistToProj==minDistToProj, DistRealDiff, NA),
         # Gettin the DistRealDiff2 of the player with the lowest DistToProj in each frame
         distproj_mindiff2 = ifelse(DistToProj==minDistToProj, DistRealDiff2, NA),
         # Gettin the DistReal of the player with the lowest DistToProj in each frame
         distproj_mindiff3 = ifelse(DistToProj==minDistToProj, DistReal, NA),
         # Gettin the DistReal of the player with the lowest DistRealDiff in each frame
         distproj_mindiff4 = ifelse(DistRealDiff==minDistRealDiff, DistReal, NA)) %>%
  ungroup()

# Obtaining weights by order statistics of the values obtained above

rankprob <- novoW %>%
  dplyr::select(distproj_mindiff) %>%
  filter(!is.na(distproj_mindiff)) %>%
  group_by(distproj_mindiff) %>%
  slice(1)
probs <- seq(0, 1, length.out = length(rankprob$distproj_mindiff))
rankprob$WeightProb <- probs
novoW <- left_join(novoW, rankprob)

rankprob2 <- novoW %>%
  dplyr::select(distproj_mindiff2) %>%
  filter(!is.na(distproj_mindiff2)) %>%
  group_by(distproj_mindiff2) %>%
  slice(1)
probs2 <- seq(0, 1, length.out = length(rankprob2$distproj_mindiff2))
rankprob2$WeightProb2 <- probs2
novoW <- left_join(novoW, rankprob2)

rankprob3 <- novoW %>%
  dplyr::select(distproj_mindiff3) %>%
  filter(!is.na(distproj_mindiff3)) %>%
  group_by(distproj_mindiff3) %>%
  slice(1)
probs3 <- seq(0, 1, length.out = length(rankprob3$distproj_mindiff3))
rankprob3$WeightProb3 <- probs3
novoW <- left_join(novoW, rankprob3)

rankprob4 <- novoW %>%
  dplyr::select(distproj_mindiff4) %>%
  filter(!is.na(distproj_mindiff4)) %>%
  group_by(distproj_mindiff4) %>%
  slice(1)
probs4 <- seq(0, 1, length.out = length(rankprob4$distproj_mindiff4))
rankprob4$WeightProb4 <- probs4
novoW <- left_join(novoW, rankprob4)

# Applying the weights on the probabilities and obtaining the weighted probabilities
novoW <- novoW %>%
  group_by(gameId,playId,frameId) %>%
  mutate(WeightProb = min(WeightProb, na.rm=T),
         WeightProb2 = min(WeightProb2, na.rm=T),
         WeightProb3 = min(WeightProb3, na.rm=T),
         WeightProb4 = min(WeightProb4, na.rm=T)) %>%
  mutate(# The smaller the DistRealDiff of the lowest DistToProj
    # More weight goes to the EmpProb of DistToProj
    EmpProbWeighted = (1-WeightProb)*EmpProb1+WeightProb*EmpProb2,
    EmpProbWeighted2 = (1-WeightProb2)*EmpProb1+WeightProb2*EmpProb2,
    # The smaller the DistReal of the lowest DistToProj
    # More weight goes to the EmpProb of DistToProj
    EmpProbWeighted3 = (1-WeightProb3)*EmpProb1+WeightProb3*EmpProb2,
    # The smaller the DistReal of the lowest DistRealDiff
    # More weight goes to the EmpProb of DistRealDiff
    EmpProbWeighted4 = WeightProb4*EmpProb1+(1-WeightProb4)*EmpProb2)

# Predicting who will be the target on each frame according to each weighted probability
test_pred <- novoW %>%
  group_by(gameId,playId,frameId) %>%
  mutate(predicted.classesNW = EmpProb==max(EmpProb),
         predicted.classesW = EmpProbWeighted==max(EmpProbWeighted),
         predicted.classesW2 = EmpProbWeighted2==max(EmpProbWeighted2),
         predicted.classesW3 = EmpProbWeighted3==max(EmpProbWeighted3),
         predicted.classesW4 = EmpProbWeighted4==max(EmpProbWeighted4))

novo6 <- novo4 %>%
  # Getting info on which frame is the last in each play
  group_by(gameId, playId) %>%
  mutate(maxFrame = max(frameId)) %>%
  ungroup() %>%
  # If we don't already know who the real target is, the player closest to the ball on the last frame is the target
  group_by(gameId, playId, frameId) %>%
  mutate(minDistReal = ifelse(frameId==maxFrame, min(DistReal), NA),
         Target = ifelse(is.na(target), DistReal == minDistReal, target==name)) %>%
  ungroup() %>%
  # Applying the target for all the frames on the play in the cases above
  group_by(gameId, playId, nflId) %>%
  mutate(Target = as.logical(max(Target, na.rm=T))) %>%
  ungroup() %>%
  # Excluding variables we won't use anymore
  dplyr::select(-c(maxFrame, minDistReal))

# Updating the Targets
test_pred$Target <- novo6$Target
test_pred <- test_pred %>%
  group_by(gameId, playId, frameId) %>%
  # Filtering plays where there's no info on the real target of the play for some reason
  filter(sum(Target)==1) %>%
  ungroup()

# Checking the accuracy (% of frames that the target really was the player with highest probability of being the target) for all weights
acc <- test_pred %>%
  group_by(gameId,playId,frameId) %>%
  summarize(NW = sum(predicted.classesNW==Target) == length(Target),
            W = sum(predicted.classesW==Target) == length(Target),
            W2 = sum(predicted.classesW2==Target) == length(Target),
            W3 = sum(predicted.classesW3==Target) == length(Target),
            W4 = sum(predicted.classesW4==Target) == length(Target))
mean(acc$NW)
mean(acc$W)
mean(acc$W2)
mean(acc$W3)
mean(acc$W4)

# Adjusting probabilities when there'sa player very close to the ball
test_pred2 <- test_pred %>%
  group_by(gameId,playId,frameId) %>%
  mutate(# Checking in each frame if there's a player very close to the ball in real and distance to line projection of the ball
    PlayerClose = ifelse(DistReal < 2 & DistToProj < 2, T, F),
    # Caso tenha um jogador proximo e a distancia dos outros jogadores seja maior que proxima
    # Checking if there is a close player and if the others are close too or not
    BallBeyond = ifelse(sum(PlayerClose) > 0 & DistReal > 2, T, F),
    # If there is a close player, the probabilities of those who aren't go to the closest player with higher probability of being the target
    EmpProbF = ifelse(BallBeyond==T, 0, EmpProb),
    EmpProbF = ifelse(sum(BallBeyond)>0 & EmpProbF==max(EmpProbF), EmpProbF+sum(EmpProb[BallBeyond]), EmpProbF),
    EmpProbWeightedF = ifelse(BallBeyond==T, 0, EmpProbWeighted),
    EmpProbWeightedF = ifelse(sum(BallBeyond)>0 & EmpProbWeightedF==max(EmpProbWeightedF), EmpProbWeightedF+sum(EmpProbWeighted[BallBeyond]), EmpProbWeightedF),
    EmpProbWeighted2F = ifelse(BallBeyond==T, 0, EmpProbWeighted2),
    EmpProbWeighted2F = ifelse(sum(BallBeyond)>0 & EmpProbWeighted2F==max(EmpProbWeighted2F), EmpProbWeighted2F+sum(EmpProbWeighted2[BallBeyond]), EmpProbWeighted2F),
    EmpProbWeighted3F = ifelse(BallBeyond==T, 0, EmpProbWeighted3),
    EmpProbWeighted3F = ifelse(sum(BallBeyond)>0 & EmpProbWeighted3F==max(EmpProbWeighted3F), EmpProbWeighted3F+sum(EmpProbWeighted3[BallBeyond]), EmpProbWeighted3F),
    EmpProbWeighted4F = ifelse(BallBeyond==T, 0, EmpProbWeighted4),
    EmpProbWeighted4F = ifelse(sum(BallBeyond)>0 & EmpProbWeighted4F==max(EmpProbWeighted4F), EmpProbWeighted4F+sum(EmpProbWeighted4[BallBeyond]), EmpProbWeighted4F),
    predicted.classesNW = EmpProbF==max(EmpProbF),
    predicted.classesW = EmpProbWeightedF==max(EmpProbWeightedF),
    predicted.classesW2 = EmpProbWeighted2F==max(EmpProbWeighted2F),
    predicted.classesW3 = EmpProbWeighted3F==max(EmpProbWeighted3F),
    predicted.classesW4 = EmpProbWeighted4F==max(EmpProbWeighted4F))

# Checking the distribution of the number of frames on the plays
nfr <- acc %>%
  group_by(gameId,playId) %>%
  summarize(n = n())
summary(as.factor(nfr$n))
# Mean number of frames on plays
sum(summary(as.factor(nfr$n))*as.numeric(names(summary(as.factor(nfr$n)))))/nrow(nfr)

# SSlogis function
g <- function(i,scal=1){
  SSlogis(i,1,13.34183,scal)
}

# creating weights w23
weight <- g(1:46,scal=2.57)

# applying the weights on w2 and w3
test_pred3 <- test_pred2 %>%
  group_by(gameId,playId) %>%
  mutate(frameBeg = frameId-min(frameId)+1) %>%
  ungroup() %>%
  group_by(gameId,playId,frameId) %>%
  mutate(EmpProbWeighted23F = weight[frameBeg]*EmpProbWeighted3F + (1-weight[frameBeg])*EmpProbWeighted2F,
         predicted.classesW23 = EmpProbWeighted23F==max(EmpProbWeighted23F)) %>%
  ungroup()

# Checking accuracy of W23
acc3 <- test_pred3 %>%
  group_by(gameId,playId,frameId) %>%
  summarize(W23 = sum(predicted.classesW23==Target) == length(Target))

# Joining data from W23 with the others
acc$W23 <- acc3$W23

# Creating a database with the evolution of accuracies from beginning and end of plays
max.frames <- 46
first_mean <- data.frame()
last_mean <- data.frame()
for(i in 1:max.frames){
  first <- acc %>%
    group_by(gameId, playId) %>%
    filter(length(playId)>=i) %>%
    slice(1:i)
  last <- acc %>%
    group_by(gameId, playId) %>%
    filter(length(playId)>=i) %>%
    slice((length(playId)-(i-1)):(length(playId)))
  first_means <- colMeans(first[,4:9])
  last_means <- colMeans(last[,4:9])
  first_mean <- rbind(first_mean,first_means)
  last_mean <- rbind(last_mean,last_means)
}
colnames(first_mean) <- colnames(last_mean) <- colnames(acc[,4:9])
means_first <- data.frame(first_mean[,1:5], n=1:max.frames)
mfplot <- melt(means_first, id.vars = "n")
levels(mfplot$variable)[1] <- "EW"

# Plotting accuracy from beginning of plays
ggplot(data=mfplot, aes(x=n, y=value, colour=variable)) +
  geom_line()+
  geom_point()+
  labs(x = "n", y = "Accuracy", title="(a)",
       colour="") + xlim(0,25) + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

means_last <- data.frame(last_mean[,1:5], n=1:max.frames)
mlplot <- melt(means_last, id.vars = "n")
levels(mlplot$variable)[1] <- "EW"

# Plotting accuracy from end of plays
ggplot(data=mlplot, aes(x=n, y=value, colour=variable)) +
  geom_line()+
  geom_point()+
  labs(x = "n", y = "Accuracy", title="(b)",
       colour="") + xlim(0,25) + ylim(0.7,0.95) + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

################################ PASS COMPLETION PROBABILITIES ########################################

# Joining the target predictions with the database with defenders
testDEF_Target <- left_join(novoDEF, test_pred)
# Removing variables we won't use
testDEF_Target <- testDEF_Target[,-c(32:59)]

testDEF_Target <- testDEF_Target %>%
  group_by(gameId, playId, frameId) %>%
  # Filtering plays where there's no info on the real target for some reason
  filter(sum(Target, na.rm=T)==1,
         # Filtering plays where there's no info on the defense players
         !sum(team=="DEF")==0,
         # Filtering plays where there's no info on the passer
         sum(team=="PASS")!=0) %>%
  ungroup() %>%
  # Grouping player positions on less categories
  mutate(position = ifelse(position=="DE" | position=="DT" | position=="DL" | position=="NT", "DL",
                           ifelse(position=="LB" | position=="ILB" | position=="MLB" | position=="OLB", "LB",
                                  ifelse(position=="FS" | position=="SS" | position=="S", "S",
                                         ifelse(position=="FB" | position=="RB" | position=="HB", "RB",
                                                ifelse(position=="CB" | position=="DB", "DB", position))))))

new1 <- testDEF_Target %>%
  group_by(gameId, playId, frameId) %>%
  # Gettin location of the target
  mutate(Target = ifelse(is.na(Target), F, Target),
         xTarget = x[Target==T],
         yTarget = y[Target==T]) %>%
  ungroup() %>%
  group_by(gameId, playId, nflId) %>%
  # Previous location of the target
  mutate(xTargetPrev = lag(xTarget),
         yTargetPrev = lag(yTarget)) %>%
  group_by(gameId, playId, frameId, nflId) %>%
  # Calculating distance of each player to line projection created by target direction
  mutate(DistToProjTarget = dist2d(c(x,y), c(xTarget, yTarget), c(xTargetPrev, yTargetPrev)),
         # If a player is on top of the line, distance is 0.01 instead of 0
         DistToProjTarget = ifelse(DistToProjTarget==0 & team=="DEF", 0.01, DistToProjTarget)) %>%
  # Calculating real distance from players to target
  mutate(DistRealTarget = pointDistance(c(x,y), c(xTarget, yTarget), lonlat = F)) %>%
  ungroup() %>%
  # Calculating distance difference from players to target from one frame to another
  group_by(gameId, playId, nflId) %>%
  mutate(DistRealDiffTarget = DistRealTarget - lag(DistRealTarget)) %>%
  ungroup() %>%
  # Filtering first frame of each play
  filter(!is.na(DistRealDiffTarget)) %>%
  # Filtering when Target doesn't change location from one frame to another (probably error on the database)
  filter(!is.na(DistToProjTarget))

new2 <- new1 %>%
  group_by(gameId, playId, frameId) %>%
  # Gettin passer location
  mutate(xPASS = x[team=="PASS"],
         yPASS = y[team=="PASS"]) %>%
  ungroup() %>%
  # Gettin distance from target to passer 
  group_by(gameId, playId, frameId, nflId) %>%
  mutate(DistPasserToTarget = ifelse(Target==T, pointDistance(c(x,y), c(xPASS, yPASS), lonlat = F), NA)) %>%
  ungroup() %>%
  # Gettin the distance from target to passer only on the first frame of the play and applying it on every frame
  group_by(gameId, playId) %>%
  mutate(DistPasserToTarget1F = DistPasserToTarget[!is.na(DistPasserToTarget)][1]) %>%
  ungroup() %>%
  dplyr::select(-c(xPASS,yPASS,DistPasserToTarget))

new3 <- new2 %>%
  # Filtering only defensive players and the target
  filter(!(team != "DEF" & Target == F)) %>%
  group_by(gameId, playId, frameId) %>%
  # Gettin the defensive players closest to the line projection of the target and real closest to the target
  mutate(MinDEFProj = sort(DistToProjTarget)[2],
         MinDEFReal = sort(DistRealTarget)[2],
         ClosestDEFProj = DistToProjTarget == MinDEFProj,
         ClosestDEFReal = DistRealTarget == MinDEFReal,
         # If there are 2 DEF tied for closest to projection, if one of them is the real closest, this are the two we will use
         ClosestDEFProj = ifelse(sum(ClosestDEFProj)!=1 & ClosestDEFReal==T, F, ClosestDEFProj),
         # If there are 2 DEF tied for closest to projection, and none of them is the real closest
         # The one with the lowest real distance is considered the closest to proj
         ClosestDEFProjDistRealTarget = ifelse(ClosestDEFProj==T, DistRealTarget, 200),
         ClosestDEFProj = ifelse(sum(ClosestDEFProj)!=1 & ClosestDEFProjDistRealTarget!=min(ClosestDEFProjDistRealTarget, na.rm=T), F, ClosestDEFProj)) %>%
  ungroup() %>%
  # Removing the variable created to help on the previous step
  dplyr::select(-c(ClosestDEFProjDistRealTarget)) %>%
  group_by(gameId, playId, frameId, nflId) %>%
  # Variable to help identify if the defender closest to projection is also the real closest
  mutate(BothT = ifelse(ClosestDEFProj == T & ClosestDEFReal == T, T, F)) %>%
  ungroup() %>%
  group_by(gameId, playId, frameId) %>%
  mutate(
    # If the same defender is the closest to projection and the real closest
    # The second utilized defender will be the second real closest
    MinDEFReal2 = ifelse(sum(BothT) > 0, sort(DistRealTarget)[3], NA),
    Closest2DEFReal = DistRealTarget == MinDEFReal2,
    NA2 = ifelse(is.na(Closest2DEFReal), T, F),
    # Get position, distance to projection, real distance and real distance difference TO THE BALL from the 2 chosen defenders
    positionTarget = position[Target],
    positionClosestProj = position[ClosestDEFProj],
    position2Closest = ifelse(NA2 == T, position[ClosestDEFReal], position[Closest2DEFReal]),
    DisToProjT = DistToProj[Target],
    DistRealT = DistReal[Target],
    DistRealDiffT = DistRealDiff[Target],
    DisToProjClosest = DistToProj[ClosestDEFProj],
    DistRealClosest = DistReal[ClosestDEFProj],
    DistRealDiffClosest = DistRealDiff[ClosestDEFProj],
    DisToProj2Closest = ifelse(NA2 == T, DistToProj[ClosestDEFReal], DistToProj[Closest2DEFReal]),
    DistReal2Closest = ifelse(NA2 == T, DistReal[ClosestDEFReal], DistReal[Closest2DEFReal]),
    DistRealDiff2Closest = ifelse(NA2 == T, DistRealDiff[ClosestDEFReal], DistRealDiff[Closest2DEFReal]),
    # Get distance to projection, real distance and real distance difference from the 2 chosen defenders to the target
    DisToProjClosestT = DistToProjTarget[ClosestDEFProj],
    DistRealClosestT = DistRealTarget[ClosestDEFProj],
    DistRealDiffClosestT = DistRealDiffTarget[ClosestDEFProj],
    DisToProj2ClosestT = ifelse(NA2 == T, DistToProjTarget[ClosestDEFReal], DistToProjTarget[Closest2DEFReal]),
    DistReal2ClosestT = ifelse(NA2 == T, DistRealTarget[ClosestDEFReal], DistRealTarget[Closest2DEFReal]),
    DistRealDiff2ClosestT = ifelse(NA2 == T, DistRealDiffTarget[ClosestDEFReal], DistRealDiffTarget[Closest2DEFReal])) %>%
  slice(1) %>%
  ungroup() %>%
  # Grouping defensive players when they are on the offense in the same class
  # Grouping offensive players when they are on the defense in the same class
  mutate(positionTarget = ifelse(positionTarget=="DB" | positionTarget=="DL" | positionTarget=="LB" |
                                   positionTarget=="S", "DEF", positionTarget),
         positionClosestProj = ifelse(positionClosestProj=="QB" | positionClosestProj=="WR" | positionClosestProj=="TE" |
                                        positionClosestProj=="RB", "OFF", positionClosestProj),
         position2Closest = ifelse(position2Closest=="QB" | position2Closest=="WR" | position2Closest=="TE" |
                                     position2Closest=="RB", "OFF", position2Closest))

# Getting the distance from the target to the nearest sideline on every frame
new3 <- new3 %>%
  group_by(gameId,playId,frameId) %>%
  mutate(sidelineDist = min(53.3-yTarget, yTarget)) %>%
  ungroup()

# Transforming some variables in factor
new3$passResult <- as.factor(new3$passResult)
new3$quarter <- as.factor(new3$quarter)
new3$down <- as.factor(new3$down)

# Removing variables that won't be used in the models
datamod <- new3 %>%
  dplyr::select(-c(gameId,playId,frameId, nflId, position, team, x, y, 
                   passer, target, name, xFootball, yFootball, xFootballPrev,
                   yFootballPrev, DistToProj, DistReal, DistRealDiff, Target,
                   xTarget, yTarget, xTargetPrev, yTargetPrev, DistToProjTarget, DistRealTarget, 
                   DistRealDiffTarget, MinDEFProj, MinDEFReal, ClosestDEFProj, ClosestDEFReal, BothT,
                   MinDEFReal2, Closest2DEFReal, NA2))
# This will be the database for cross-validation

# Creating a code for every different play on the database
cola <- paste(new3$gameId, new3$playId, sep="")
# Seed
set.seed(123)
# Creating folds
folds <- groupKFold(cola, k=10)

# Code to allow us to train a random forest model changing the values of mtry and ntree
customRF <- list(type = "Classification",
                 library = "randomForest",
                 loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"),
                                  class = rep("numeric", 2),
                                  label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs) {
  randomForest(x, y,
               mtry = param$mtry,
               ntree=param$ntree)
}
#Predict label
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
#Predict prob
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# Specifying the values of the parameters
tunegrid <- expand.grid(.mtry=c(15),
                        .ntree=c(500))

# Using trainControl to define the cross-validation
train.control <- trainControl(method="cv", number=10, index=folds, savePredictions=T, classProbs=T, summaryFunction=twoClassSummary)

# Applying the cross-validation with the methods
mod <- train(passResult~., data=datamod, method="rf", trControl=train.control, metric="ROC")

# Plotting the ROC curve
ggplot(mod$pred, 
       aes(m = I, d = factor(obs, levels = c("I", "C")))) + 
  geom_roc(hjust = -0.4, vjust = 1.5) + coord_equal() + xlab("False Positive Fraction") + ylab("True Positive Fraction") + 
  theme_bw()

# Now to create the database with all offensive players instead with only the target
testDEF_Target <- left_join(novoDEF, test_pred)
testDEF_Target <- testDEF_Target[,-c(32:56,58:59)]

testDEF_Target <- testDEF_Target %>%
  group_by(gameId, playId, frameId) %>%
  # Filtering plays where there's no info on the real target for some reason
  filter(sum(Target, na.rm=T)==1,
         # Filtering plays where there's no info on the defense players
         !sum(team=="DEF")==0,
         # Filtering plays where there's no info on the passer
         sum(team=="PASS")!=0) %>%
  ungroup() %>%
  # Grouping player positions on less categories
  mutate(position = ifelse(position=="DE" | position=="DT" | position=="DL" | position=="NT", "DL",
                           ifelse(position=="LB" | position=="ILB" | position=="MLB" | position=="OLB", "LB",
                                  ifelse(position=="FS" | position=="SS" | position=="S", "S",
                                         ifelse(position=="FB" | position=="RB" | position=="HB", "RB",
                                                ifelse(position=="CB" | position=="DB", "DB", position))))))

# Creating a loop to get the same previous database but calculating everything considering a different offensive player
# to be the target on every loop
frames <- c()
new3_full <- data.frame()
for(i in 1:5){
  new1_2 <- testDEF_Target %>%
    group_by(gameId, playId, frameId) %>%
    # Filtering to get only an offensive player
    filter((team=="DEF") | (team=="PASS") | (nflId==nflId[team=="ATK"][i])) %>%
    filter(!sum(team=="ATK")==0) %>%
    # Saving the Id of tha player being considered the target right now
    mutate(nflIdATK = nflId[team=="ATK"]) %>%
    # Gettin the location of the considered target
    mutate(xTarget = x[team=="ATK"],
           yTarget = y[team=="ATK"]) %>%
    ungroup() %>%
    group_by(gameId, playId, nflId) %>%
    # Gettin the location of the considered target on previous frame
    mutate(xTargetPrev = lag(xTarget),
           yTargetPrev = lag(yTarget)) %>%
    ungroup()
  
  
  # When the target doesn't change location (x,y) from one frame to another, the previous (x,y) is from more previous frames
  nr1 <- 1
  nr2 <- 2
  while(nr1 != 0){
    new1_2 <- new1_2 %>%
      group_by(gameId, playId, nflId) %>%
      mutate(xTargetPrev = ifelse((xTargetPrev==xTarget) & (yTargetPrev==yTarget), lag(xTarget,nr2), xTargetPrev),
             yTargetPrev = ifelse((xTargetPrev==xTarget) & (yTargetPrev==yTarget), lag(yTarget,nr2), yTargetPrev)) %>%
      ungroup()
    nr1 <- sum((new1_2$xTargetPrev==new1_2$xTarget) & (new1_2$yTargetPrev==new1_2$yTarget), na.rm=T)
    nr2 <- nr2+1
  }
  
  new1_2 <- new1_2 %>%
    group_by(gameId, playId, frameId, nflId) %>%
    # Calculating the distance of each player to the line projection of the considered target direction
    mutate(DistToProjTarget = dist2d(c(x,y), c(xTarget, yTarget), c(xTargetPrev, yTargetPrev)),
           # If a player is on top of the line, distance is 0.01 instead of 0
           DistToProjTarget = ifelse(DistToProjTarget==0 & team=="DEF", 0.01, DistToProjTarget)) %>%
    # Calculating real distance from players to target
    mutate(DistRealTarget = pointDistance(c(x,y), c(xTarget, yTarget), lonlat = F)) %>%
    ungroup() %>%
    # Calculating distance difference from players to target from one frame to another
    group_by(gameId, playId, nflId) %>%
    mutate(DistRealDiffTarget = DistRealTarget - lag(DistRealTarget)) %>%
    ungroup() %>%
    # Filtering first frame of each play
    filter(!is.na(DistRealDiffTarget))
  
  # Saving the frames we will end up removing for some reason
  teste <- new1_2 %>%
    filter(is.na(DistToProjTarget))
  frames <- unique(c(frames,unique(paste(paste(teste$gameId, teste$playId, sep=""), teste$frameId, sep=""))))
  
  new2_2 <- new1_2 %>%
    # Filtering the initial frames until the considered target starts to move
    filter(!is.na(DistToProjTarget)) %>%
    group_by(gameId, playId, frameId) %>%
    # Gettin passer location
    mutate(xPASS = x[team=="PASS"],
           yPASS = y[team=="PASS"]) %>%
    ungroup() %>%
    # Gettin distance from target to passer 
    group_by(gameId, playId, frameId, nflId) %>%
    mutate(DistPasserToTarget = ifelse(team=="ATK", pointDistance(c(x,y), c(xPASS, yPASS), lonlat = F), NA)) %>%
    ungroup() %>%
    # Gettin the distance from target to passer only on the first frame of the play and applying it on every frame
    group_by(gameId, playId) %>%
    mutate(DistPasserToTarget1F = DistPasserToTarget[!is.na(DistPasserToTarget)][1]) %>%
    ungroup() %>%
    dplyr::select(-c(xPASS,yPASS,DistPasserToTarget))
  
  new3_2 <- new2_2 %>%
    # Removing the passer
    filter(!team=="PASS") %>%
    group_by(gameId, playId, frameId) %>%
    # Gettin the defensive players closest to the line projection of the considered target and real closest to the considered target
    mutate(MinDEFProj = sort(DistToProjTarget)[2],
           MinDEFReal = sort(DistRealTarget)[2],
           ClosestDEFProj = DistToProjTarget == MinDEFProj,
           ClosestDEFReal = DistRealTarget == MinDEFReal,
           # If there are 2 DEF tied for closest to projection, if one of them is the real closest, this are the two we will use
           ClosestDEFProj = ifelse(sum(ClosestDEFProj)!=1 & ClosestDEFReal==T, F, ClosestDEFProj),
           # If there are 2 DEF tied for closest to projection, and none of them is the real closest
           # The one with the lowest real distance is considered the closest to proj
           ClosestDEFProjDistRealTarget = ifelse(ClosestDEFProj==T, DistRealTarget, 200),
           ClosestDEFProj = ifelse(sum(ClosestDEFProj)!=1 & ClosestDEFProjDistRealTarget!=min(ClosestDEFProjDistRealTarget, na.rm=T), F, ClosestDEFProj)) %>%
    ungroup() %>%
    # Removing the variable created to help on the previous step
    dplyr::select(-c(ClosestDEFProjDistRealTarget)) %>%
    group_by(gameId, playId, frameId, nflId) %>%
    # Variable to help identify if the defender closest to projection is also the real closest
    mutate(BothT = ifelse(ClosestDEFProj == T & ClosestDEFReal == T, T, F)) %>%
    ungroup() %>%
    group_by(gameId, playId, frameId) %>%
    mutate(
      # If the same defender is the closest to projection and the real closest
      # The second utilized defender will be the second real closest
      MinDEFReal2 = ifelse(sum(BothT) > 0, sort(DistRealTarget)[3], NA),
      Closest2DEFReal = DistRealTarget == MinDEFReal2,
      NA2 = ifelse(is.na(Closest2DEFReal), T, F),
      # Get position, distance to projection, real distance and real distance difference TO THE BALL from the 2 chosen defenders
      positionTarget = position[team=="ATK"],
      positionClosestProj = position[ClosestDEFProj],
      position2Closest = ifelse(NA2 == T, position[ClosestDEFReal], position[Closest2DEFReal]),
      DisToProjT = DistToProj[team=="ATK"],
      DistRealT = DistReal[team=="ATK"],
      DistRealDiffT = DistRealDiff[team=="ATK"],
      DisToProjClosest = DistToProj[ClosestDEFProj],
      DistRealClosest = DistReal[ClosestDEFProj],
      DistRealDiffClosest = DistRealDiff[ClosestDEFProj],
      DisToProj2Closest = ifelse(NA2 == T, DistToProj[ClosestDEFReal], DistToProj[Closest2DEFReal]),
      DistReal2Closest = ifelse(NA2 == T, DistReal[ClosestDEFReal], DistReal[Closest2DEFReal]),
      DistRealDiff2Closest = ifelse(NA2 == T, DistRealDiff[ClosestDEFReal], DistRealDiff[Closest2DEFReal]),
      # Get distance to projection, real distance and real distance difference from the 2 chosen defenders to the target
      DisToProjClosestT = DistToProjTarget[ClosestDEFProj],
      DistRealClosestT = DistRealTarget[ClosestDEFProj],
      DistRealDiffClosestT = DistRealDiffTarget[ClosestDEFProj],
      DisToProj2ClosestT = ifelse(NA2 == T, DistToProjTarget[ClosestDEFReal], DistToProjTarget[Closest2DEFReal]),
      DistReal2ClosestT = ifelse(NA2 == T, DistRealTarget[ClosestDEFReal], DistRealTarget[Closest2DEFReal]),
      DistRealDiff2ClosestT = ifelse(NA2 == T, DistRealDiffTarget[ClosestDEFReal], DistRealDiffTarget[Closest2DEFReal])) %>%
    slice(1) %>%
    ungroup() %>%
    # Grouping defensive players when they are on the offense in the same class
    # Grouping offensive players when they are on the defense in the same class
    mutate(positionTarget = ifelse(positionTarget=="DB" | positionTarget=="DL" | positionTarget=="LB" |
                                     positionTarget=="S", "DEF", positionTarget),
           positionClosestProj = ifelse(positionClosestProj=="QB" | positionClosestProj=="WR" | positionClosestProj=="TE" |
                                          positionClosestProj=="RB", "OFF", positionClosestProj),
           position2Closest = ifelse(position2Closest=="QB" | position2Closest=="WR" | position2Closest=="TE" |
                                       position2Closest=="RB", "OFF", position2Closest))
  
  new3_full <- rbind(new3_full, new3_2)
}

# Organizing the new database
new3_full <- new3_full %>%
  relocate(nflIdATK, .after=frameId) %>%
  dplyr::select(-c(nflId)) %>%
  arrange(gameId, playId, frameId)

# Removing from the base the frames we removed for some offensive player
cola <- paste(paste(new3_full$gameId, new3_full$playId, sep=""), new3_full$frameId, sep="")
new3_full <- new3_full[!is.element(cola, frames),]

# Getting the distance from the considered target to the nearest sideline on every frame
new3_full <- new3_full %>%
  group_by(gameId,playId,frameId,nflIdATK) %>%
  mutate(sidelineDist = min(53.3-yTarget, yTarget)) %>%
  ungroup()

# Transforming some variables in factor
new3_full$passResult <- as.factor(new3_full$passResult)
new3_full$quarter <- as.factor(new3_full$quarter)
new3_full$down <- as.factor(new3_full$down)

# Removing variables that won't be used in the models
datamod_2 <- new3_full %>%
  dplyr::select(-c(gameId,playId,frameId, nflIdATK, position, team, x, y, 
                   passer, target, name, xFootball, yFootball, xFootballPrev,
                   yFootballPrev, DistToProj, DistReal, DistRealDiff, Target, predicted.classesW2,
                   xTarget, yTarget, xTargetPrev, yTargetPrev, DistToProjTarget, DistRealTarget, 
                   DistRealDiffTarget, MinDEFProj, MinDEFReal, ClosestDEFProj, ClosestDEFReal, BothT,
                   MinDEFReal2, Closest2DEFReal, NA2))



# Creating a code for every different play on the database
cola <- paste(new3$gameId, new3$playId, sep="")
# Seed
set.seed(123)
# Creating folds
folds <- groupKFold(cola, k=10)

datafull <- list()
for(i in 1:10){
  # Separating observations from fold i from the base with info only on the real targets
  teste <- paste(new3_full$gameId, new3_full$playId, sep="")
  training <- new3[folds[[i]],]
  
  # Separating observations that will be predicted from the base with info on all offensive players
  teste2 <- paste(training$gameId, training$playId, sep="")
  test <- new3_full[!is.element(teste, teste2),]
  
  # Removing variables that won't be used in the model
  datamod_train <- training %>%
    dplyr::select(-c(gameId,playId,frameId, nflId, position, team, x, y,
                     passer, target, name, xFootball, yFootball, xFootballPrev,
                     yFootballPrev, DistToProj, DistReal, DistRealDiff, Target,
                     xTarget, yTarget, xTargetPrev, yTargetPrev, DistToProjTarget, DistRealTarget,
                     DistRealDiffTarget, MinDEFProj, MinDEFReal, ClosestDEFProj, ClosestDEFReal, BothT,
                     MinDEFReal2, Closest2DEFReal, NA2))
  
  # Removing variables that won't be used in the model
  datamod_test <- test %>%
    dplyr::select(-c(gameId,playId,frameId, nflIdATK, position, team, x, y,
                     passer, target, name, xFootball, yFootball, xFootballPrev,
                     yFootballPrev, DistToProj, DistReal, DistRealDiff, Target, predicted.classesW2,
                     xTarget, yTarget, xTargetPrev, yTargetPrev, DistToProjTarget, DistRealTarget,
                     DistRealDiffTarget, MinDEFProj, MinDEFReal, ClosestDEFProj, ClosestDEFReal, BothT,
                     MinDEFReal2, Closest2DEFReal, NA2))
  
  # Creating the model
  modrf <- randomForest(as.formula(datamod_train), data=datamod_train, ntree=500, mtry=15)
  # Predicting the probabilities
  pred <- predict(modrf, datamod_test, type="prob")
  # Joining them on the same database
  test[,68:69] <- pred
  test <- test %>%
    rename(nflId = nflIdATK)
  datafull[[i]] <- test
}

# Creating one single database with all the probabilities for every player on every frame
datapred <- data.frame()
for(i in 1:10){
  datapred <- rbind(datapred,datafull[[i]])
}

# Changing the name of the probability column
colnames(datapred)[68] <- "V1"

# Joining everything on the base that has the probabilities of players being the target
test_pred3$quarter <- as.factor(test_pred3$quarter)
test_pred3$down <- as.factor(test_pred3$down)
data_pred <- left_join(test_pred3, datapred, by=c("gameId","playId","frameId","nflId"))

data_pred2 <- data_pred %>%
  # Removing the plays that weren't in the final database for the model
  filter(!is.na(V1)) %>%
  # Selecting only the relevant variables
  dplyr::select(gameId,playId,frameId,nflId,predicted.classesW23,EmpProbWeighted23F,V1) %>%
  # Calculating the general probability with the law of total probability
  group_by(gameId,playId,frameId) %>%
  mutate(p = sum(EmpProbWeighted23F * V1)) %>%
  ungroup() %>%
  # Selecting only relevant variables to make the animations
  dplyr::select(gameId,playId,frameId,nflId,predicted.classesW23,V1,p)

# Making a single model with all the observations
modrf <- randomForest(as.formula(datamod), data=datamod, ntree=500, mtry=15)
pred <- predict(modrf, datamod_2, type="prob")
new3_full[,69:70] <- pred
new3_full <- new3_full %>%
  rename(nflId = nflIdATK)
datapred2 <- new3_full
colnames(datapred2)[69] <- "V1"
data_predF <- left_join(test_pred3, datapred2, by=c("gameId","playId","frameId","nflId"))

# Calculating accuracy of pass completion prediction with 0.5 threshold
teste <- data_predF %>%
  filter(!is.na(V1)) %>%
  dplyr::select(gameId,playId,frameId,nflId,predicted.classesW23,EmpProbWeighted23F,V1, passResult.x) %>%
  filter(predicted.classesW23) %>%
  mutate(pred = ifelse(V1>=0.5, "C", "I"))
mean(teste$passResult.x == teste$pred)

################################### ANIMATING PLAYS ##############################################

team_color_codes <- read.csv2("team_color_codes.csv", header=F)

# Function to split the title when it's very large
wrap_sentence <- function(string, width) {
  words <- unlist(strsplit(string, " "))
  fullsentence <- ""
  checklen <- ""
  for(i in 1:length(words)) {
    checklen <- paste(checklen, words[i])
    if(nchar(checklen)>(width+1)) {
      fullsentence <- paste0(fullsentence, "\n")
      checklen <- ""
    }
    fullsentence <- paste(fullsentence, words[i])
  }
  fullsentence <- sub("^\\s", "", fullsentence)
  fullsentence <- gsub("\n ", "\n", fullsentence)
  return(fullsentence)
}


data_anim <- left_join(data_pred2, df_plays)

# General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3

#picking a random play
#set.seed(5643)

example_play <- data_anim %>%
  dplyr::select(gameId, playId, playDescription) %>% 
  # For a random play
  #sample_n(1)
  # GERONIMO TD
  #filter(gameId==2018090912, playId==2775) %>%
  # GRONK TD
  #filter(gameId==2018090905, playId==228) %>%
  # JULIO catch
  #filter(gameId==2018090600, playId==75) %>%
  # easy pass completion
  #filter(gameId==2018091603, playId==3683) %>%
  # easy pass incompletion
  #filter(gameId==2018122313, playId==1724) %>%
  # COUSINS HAIL MARY
  #filter(gameId==2018122307, playId==2368) %>%
  slice(1)


#merging games data to play
example_play <- inner_join(example_play,
                           df_games,
                           by = c("gameId" = "gameId"))

#merging tracking data to play
example_play <- inner_join(example_play,
                           df_tracking,
                           by = c("gameId" = "gameId",
                                  "playId" = "playId"))

#merging tracking data to probabilities
example_play <- left_join(example_play,
                          data_anim[,1:7],
                          by = c("gameId" = "gameId",
                                 "playId" = "playId",
                                 "frameId" = "frameId",
                                 "nflId" = "nflId"))

# Creating variables that show the jersey number of the predicted target on every frame
# and the probability of pass completion given predicted target
example_play <- example_play %>%
  group_by(gameId,playId,frameId) %>%
  mutate(jerseyNumberTarget = ifelse(sum(is.na(predicted.classesW23)) == length(predicted.classesW23),
                                     NA, jerseyNumber[predicted.classesW23][!is.na(jerseyNumber[predicted.classesW23])]),
         CprobTarget = ifelse(sum(is.na(predicted.classesW23)) == length(predicted.classesW23),
                              NA, V1[predicted.classesW23][!is.na(V1[predicted.classesW23])])) %>%
  ungroup()


# Pegando as cores dos times envolvidos na jogada
homeColor <- team_color_codes$V2[example_play$homeTeamAbbr[1] == team_color_codes$V1]
visitorColor <- team_color_codes$V2[example_play$visitorTeamAbbr[1] == team_color_codes$V1]
homeColor2 <- team_color_codes$V3[example_play$homeTeamAbbr[1] == team_color_codes$V1]
visitorColor2 <- team_color_codes$V3[example_play$visitorTeamAbbr[1] == team_color_codes$V1]
cols_fill <- c(visitorColor, "#663300", homeColor)
cols_col <- c(visitorColor2, "#663300", homeColor2)

plot_title <- str_trim(gsub("\\s*\\([^\\)]+\\)","",as.character(example_play$playDescription[1])))
plot_subtitle <- paste(paste(example_play$visitorTeamAbbr[1], "@"), example_play$homeTeamAbbr[1])

# Specific boundaries for a given play
ymin <- max(round(min(example_play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example_play$x, na.rm = TRUE) + 10, -1), 120)

#hash marks
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

#plotting
plot_anim <- ggplot() +
  
  #setting size and color parameters
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = cols_fill, guide = FALSE) + 
  scale_colour_manual(values = cols_col, guide = FALSE) +
  
  #adding hash marks
  annotate("text", x = df.hash$x[df.hash$x < 55/2], 
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 55/2], 
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  
  #adding yard lines
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  
  #adding field yardline text
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  
  #adding field exterior
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  
  #adding players
  geom_point(data = example_play, aes(x = (xmax-y),
                                      y = x, 
                                      shape = team,
                                      fill = team,
                                      group = nflId,
                                      size = team,
                                      colour = team), 
             alpha = 0.7) +  
  
  #adding jersey numbers
  geom_text(data = example_play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) + 
  
  #adding the general pass completion probability
  geom_text(data = example_play, aes(x = xmax+11, y = (ymax+ymin)/2+12.5, label = round(p,3)), colour = "black", 
            vjust = 0.5, size = 7, hjust = 0.5) + 
  
  annotate("text", x = xmax+11.5, y = (ymax+ymin)/2 + 15, label = "Completion Probability", vjust = 0.5, size = 4, hjust = 0.5,
           fontface="bold") +
  
  #adding who is the predicted target
  geom_text(data = example_play, aes(x = xmax+11, y = (ymax+ymin)/2 - 2.5, label = jerseyNumberTarget), colour = "black", 
            vjust = 0.5, size = 7, hjust = 0.5) + 
  
  annotate("text", x = xmax+11.5, y = (ymax+ymin)/2, label = "Predicted Target", vjust = 0.5, size = 4, hjust = 0.5,
           fontface="bold") +
  
  #adding the pass completion probability given predicted target
  geom_text(data = example_play, aes(x = xmax+11, y = (ymax+ymin)/2 - 19, label = CprobTarget), colour = "black", 
            vjust = 0.5, size = 7, hjust = 0.5) + 
  
  annotate("text", x = xmax+11.5, y = (ymax+ymin)/2 - 15, label = "Completion Probability \n given Predicted Target", vjust = 0.5, size = 4, hjust = 0.5,
           fontface="bold") +
  
  #adding frame number
  geom_text(data = example_play, aes(x = xmax+18, y = (ymax+ymin)/2 - 25, label = frameId), colour = "black", 
            vjust = 0.5, size = 3, hjust = 0.5) + 
  
  annotate("text", x = xmax+18, y = (ymax+ymin)/2 - 23, label = "Frame", vjust = 0.5, size = 3, hjust = 0.5,
           fontface="bold") +
  
  
  
  #applying plot limits
  ylim(ymin, ymax) + 
  xlim(xmin,xmax+20) +
  #coord_fixed() +
  
  #applying theme
  theme_nothing() + 
  theme(plot.title = element_text()) +
  theme(plot.subtitle = element_text(size = 10)) +
  
  #titling plot with play description
  labs(title = wrap_sentence(plot_title,60), subtitle = plot_subtitle) +
  
  #setting animation parameters
  transition_time(frameId)  +
  ease_aes('linear') + 
  NULL

gganimate::animate(plot_anim, nframes=max(example_play$frameId), fps=4)

# Creating the plots to compare the plays on the nfl next gen stats article
example_play2 <- example_play
example_play <- example_play2 %>%
  group_by(frameId) %>%
  filter(sum(is.na(p)) != length(frameId)) %>%
  ungroup() %>%
  filter(predicted.classesW23==T) %>%
  dplyr::select(frameId, V1, p)

# Geronimo Allison play
data2 <- melt(example_play, id="frameId")
mediav1 <- mean(example_play$V1)
mediap <- mean(example_play$p)
ggplot(data2, aes(x=frameId,y=value, colour=variable)) + geom_line(alpha=0.35) + geom_point(alpha=0.35) + 
  geom_hline(aes(yintercept =  mediav1, linetype="Average Predicted Target"), colour="blue") + 
  geom_hline(aes(yintercept =  mediap, linetype="Average General"), colour="red") +
  geom_hline(aes(yintercept =  .147, linetype="Next Gen Stats"), colour="black") +
  scale_linetype_manual(name = "", values = c(2, 2, 2), guide = guide_legend(override.aes = list(color = c("red", "blue", "black")))) +
  scale_color_manual(labels=c("Predicted Target", "General"), values = c("blue", "red")) + ylim(0.1,0.65) + theme_bw() + 
  labs(x = "Frame number", y = "Completion Probability", colour="Completion Probability", title="(a)") + theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=57, y=0.232, label="0.240", color="red") + annotate("text", x=74, y=0.216, label="0.224", color="blue") +
  annotate("text", x=79.5, y=.635, label="0.624", color="violet") +
  annotate("text", x=65, y=mediav1+.01, label=as.character(round(mediav1,3)), color="blue", size=3) +
  annotate("text", x=65, y=mediap+.01, label=as.character(round(mediap,3)), color="red", size=3) +
  annotate("text", x=65, y=.157, label=0.147, color="black", size=3)

# Rob Gronkowski play
data2 <- melt(example_play, id="frameId")
mediav1 <- mean(example_play$V1)
mediap <- mean(example_play$p)
ggplot(data2, aes(x=frameId,y=value, colour=variable)) + geom_line(alpha=0.35) + geom_point(alpha=0.35) + 
  geom_hline(aes(yintercept =  mediav1, linetype="Average Predicted Target"), colour="blue") + 
  geom_hline(aes(yintercept =  mediap, linetype="Average General"), colour="red") +
  geom_hline(aes(yintercept =  .188, linetype="Next Gen Stats"), colour="black") +
  scale_linetype_manual(name = "", values = c(2, 2, 2), guide = guide_legend(override.aes = list(color = c("red", "blue", "black")))) +
  scale_color_manual(labels=c("Predicted Target", "General"), values = c("blue", "red")) + theme_bw() + 
  labs(x = "Frame number", y = "Completion Probability", colour="Completion Probability", title="(b)") + theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=36, y=0.1, label="0.118", color="red") + annotate("text", x=36, y=0.155, label="0.136", color="blue") +
  annotate("text", x=49.8, y=.31, label="0.288", color="violet") +
  annotate("text", x=47, y=.81, label="0.796", color="violet") +
  annotate("text", x=42.5, y=mediav1+.01, label=as.character(round(mediav1,3)), color="blue", size=3) +
  annotate("text", x=42.5, y=mediap+.01, label=as.character(round(mediap,3)), color="red", size=3) +
  annotate("text", x=42.5, y=.178, label=0.188, color="black", size=3)

################################### OTHER PLOTS #######################################

### Relations between explanatory variables and the response variable
# Relation between sideline distance and pass completion
teste2 <- datamod %>%
  dplyr::select(sidelineDist,passResult) %>%
  mutate(sidelineDist = round_any(sidelineDist,0.5)) %>%
  group_by(sidelineDist) %>%
  mutate(pct = sum(passResult=="C")/length(passResult),
         n = length(passResult)) %>%
  slice(1)

ggplot(teste2, aes(x=sidelineDist, y=pct)) + geom_point(aes(size=n), colour="#0E86D4") + 
  scale_size_continuous(range = c(1, 4)) + theme_bw() + xlab("Distance from target to the nearest sideline (yards)") + ylab("Completion %") +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) + xlim(-1,27) + theme(text = element_text(size=17.5))

# Relation between distance from passer to target at moment of pass and pass completion
teste3 <- datamod %>%
  dplyr::select(DistPasserToTarget1F,passResult) %>%
  mutate(DistPasserToTarget1F = round_any(DistPasserToTarget1F,0.5)) %>%
  group_by(DistPasserToTarget1F) %>%
  mutate(pct = sum(passResult=="C")/length(passResult),
         n = length(passResult)) %>%
  slice(1)

ggplot(teste3, aes(x=DistPasserToTarget1F, y=pct)) + geom_point(aes(size=n), colour="#0E86D4") + 
  scale_size_continuous(range = c(1, 6)) + theme_bw() + xlab("Distance from passer to target at the moment of pass (yards)") + ylab("Completion %") +
  scale_y_continuous(labels = scales::percent) + xlim(0,37) + theme(text = element_text(size=17.5))

# Relation between number of pass rushers and pass completion
teste4 <- datamod %>%
  dplyr::select(numberOfPassRushers,passResult) %>%
  mutate(numberOfPassRushers = round_any(numberOfPassRushers,1)) %>%
  group_by(numberOfPassRushers) %>%
  mutate(pct = sum(passResult=="C")/length(passResult),
         n = length(passResult)) %>%
  slice(1)

ggplot(teste4, aes(x=numberOfPassRushers, y=pct)) + geom_point(aes(size=n), colour="#0E86D4") + 
  scale_size_continuous(range = c(1, 6)) + theme_bw() + xlab("Number of pass rushers on defense") + ylab("Completion %") +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) + xlim(0,8) + theme(text = element_text(size=17.5))

# Relation between field position and pass completion
teste5 <- datamod %>%
  dplyr::select(FieldPosition,passResult) %>%
  mutate(FieldPosition = round_any(FieldPosition,5)) %>%
  group_by(FieldPosition) %>%
  mutate(pct = sum(passResult=="C")/length(passResult),
         n = length(passResult)) %>%
  slice(1)

ggplot(teste5, aes(x=FieldPosition, y=pct)) + geom_point(aes(size=n), colour="#0E86D4") + 
  scale_size_continuous(range = c(1, 6)) + theme_bw() + xlab("Field position (1-99 yards)") + ylab("Completion %") +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) + xlim(1,99) + theme(text = element_text(size=17.5))

# Relation between distance from target to line projection of the ball and pass completion
teste6 <- datamod %>%
  dplyr::select(DisToProjT,passResult) %>%
  mutate(DisToProjT = round_any(DisToProjT,0.5)) %>%
  group_by(DisToProjT) %>%
  mutate(pct = sum(passResult=="C")/length(passResult),
         n = length(passResult)) %>%
  slice(1)

ggplot(teste6, aes(x=DisToProjT, y=pct)) + geom_point(aes(size=n), colour="#0E86D4") + 
  scale_size_continuous(range = c(1, 6)) + theme_bw() + xlab("Distance from target to line projection of the ball (yards)") + ylab("Completion %") +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) + xlim(0,16) + theme(text = element_text(size=17.5))

# Relation between distance from closest defender to line projection of the target and pass completion
teste7 <- datamod %>%
  dplyr::select(DisToProjClosestT,passResult) %>%
  mutate(DisToProjClosestT = round_any(DisToProjClosestT,0.25)) %>%
  group_by(DisToProjClosestT) %>%
  mutate(pct = sum(passResult=="C")/length(passResult),
         n = length(passResult)) %>%
  slice(1)

ggplot(teste7, aes(x=DisToProjClosestT, y=pct)) + geom_point(aes(size=n), colour="#0E86D4") + 
  scale_size_continuous(range = c(1, 6)) + theme_bw() + xlab("Distance from closest defender to line projection of the target (yds)") + ylab("Completion %") +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) + xlim(0,10) + theme(text = element_text(size=17.5))


### Relations between pass completion probabilities and real pass completion
# P(C) per frame
teste <- data_anim %>%
  group_by(gameId,playId,frameId) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(p,passResult) %>%
  mutate(p = round_any(p,0.05)) %>%
  group_by(p) %>%
  mutate(pct = sum(passResult=="C")/length(passResult),
         n = length(passResult)) %>%
  slice(1)

ggplot(teste, aes(x=p, y=pct)) + geom_point(aes(size=n), colour="#0E86D4") + 
  scale_size_continuous(range = c(1, 2)) + theme_bw() + xlab("Completion Probability") + ylab("Completion %") + 
  scale_y_continuous(labels = scales::percent) + labs(title="(a)") + theme(plot.title = element_text(hjust = 0.5))

# P(C|Ti) per frame
tested <- data_anim %>%
  filter(predicted.classesW23==T) %>%
  group_by(gameId,playId,frameId) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(p = round_any(V1,0.05)) %>%
  dplyr::select(p,passResult) %>%
  group_by(p) %>%
  mutate(pct = sum(passResult=="C")/length(passResult),
         n = length(passResult)) %>%
  slice(1)

ggplot(tested, aes(x=p, y=pct)) + geom_point(aes(size=n), colour="#0E86D4") + 
  scale_size_continuous(range = c(1, 4)) + theme_bw() + xlab("Completion Probability") + ylab("Completion %") + 
  scale_y_continuous(labels = scales::percent) + labs(title="(b)") + theme(plot.title = element_text(hjust = 0.5))

# P(C) per play
testeb <- data_anim %>%
  group_by(gameId,playId,frameId) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(gameId,playId) %>%
  mutate(p=mean(p)) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(p,passResult) %>%
  mutate(p = round_any(p,0.05)) %>%
  group_by(p) %>%
  mutate(pct = sum(passResult=="C")/length(passResult),
         n = length(passResult)) %>%
  slice(1)

ggplot(testeb, aes(x=p, y=pct)) + geom_point(aes(size=n), colour="#0E86D4") + 
  scale_size_continuous(range = c(1, 4)) + theme_bw() + xlab("Completion Probability") + ylab("Completion %") + 
  scale_y_continuous(labels = scales::percent) + labs(title="(c)") + theme(plot.title = element_text(hjust = 0.5))

# P(C|Ti) per play
testec <- data_anim %>%
  filter(predicted.classesW23==T) %>%
  group_by(gameId,playId) %>%
  mutate(p=mean(V1)) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(p,passResult) %>%
  mutate(p = round_any(p,0.05)) %>%
  group_by(p) %>%
  mutate(pct = sum(passResult=="C")/length(passResult),
         n = length(passResult)) %>%
  slice(1)

ggplot(testec, aes(x=p, y=pct)) + geom_point(aes(size=n), colour="#0E86D4") + 
  scale_size_continuous(range = c(1, 4)) + theme_bw() + xlab("Completion Probability") + ylab("Completion %") + 
  scale_y_continuous(labels = scales::percent) + labs(title="(d)") + theme(plot.title = element_text(hjust = 0.5))

# Correlations and concordances
cors <- c(cor(teste$p, teste$pct), cor(testeb$p, testeb$pct), cor(testec$p, testec$pct), cor(tested$p, tested$pct))
conc <- c(as.numeric(CCC(teste$p, teste$pct)$rho.c[1]),as.numeric(CCC(testeb$p, testeb$pct)$rho.c[1]),
          as.numeric(CCC(testec$p, testec$pct)$rho.c[1]),as.numeric(CCC(tested$p, tested$pct)$rho.c[1]))