# Necessary packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(raster)
library(caret)
library(reshape2)
library(nlme)

# Loading rdata
# File available on google drive - Read readme.txt
load("data3.rda")


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
  labs(x = "n", y = "Accuracy", title="(A)",
       colour="") + xlim(0,25) + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

means_last <- data.frame(last_mean[,1:5], n=1:max.frames)
mlplot <- melt(means_last, id.vars = "n")
levels(mlplot$variable)[1] <- "EW"

# Plotting accuracy from end of plays
ggplot(data=mlplot, aes(x=n, y=value, colour=variable)) +
  geom_line()+
  geom_point()+
  labs(x = "n", y = "Accuracy", title="(B)",
       colour="") + xlim(0,25) + ylim(0.7,0.95) + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))