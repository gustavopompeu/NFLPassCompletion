# Necessary packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(raster)
library(caret)
library(randomForest)
library(plotROC)
library(pROC)

# Loading rdata
# File available on google drive - Read readme.txt
load("data4.rda")
rm(novo2,novo3,novo4,novo5,novo6,novoW,rankprob,rankprob2,rankprob3,rankprob4,aa,certo,first,i,j,k,nr1,nr2,
   probs,probs2,probs3,probs4,second,sep1,sep2,acc,n1,n2,name,passer,target,playDesc,playerNames,a)

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