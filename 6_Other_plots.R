# Necessary packages
library(plyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(DescTools)

# Loading rdata
# File available on google drive - Read readme.txt
load("data5_1.rda")
load("data5_2.rda")
load("data6_1.rda")
load("data6_2.rda")
rm(novoDEF,testDEF_Target,dist2d,new2)

data_anim <- left_join(data_pred2, df_plays)

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
  scale_y_continuous(labels = scales::percent) + labs(title="(A)") + theme(plot.title = element_text(hjust = 0.5))

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
  scale_y_continuous(labels = scales::percent) + labs(title="(B)") + theme(plot.title = element_text(hjust = 0.5))

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
  scale_y_continuous(labels = scales::percent) + labs(title="(C)") + theme(plot.title = element_text(hjust = 0.5))

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
  scale_y_continuous(labels = scales::percent) + labs(title="(D)") + theme(plot.title = element_text(hjust = 0.5))

# Correlations and concordances
cors <- c(cor(teste$p, teste$pct), cor(testeb$p, testeb$pct), cor(testec$p, testec$pct), cor(tested$p, tested$pct))
conc <- c(as.numeric(CCC(teste$p, teste$pct)$rho.c[1]),as.numeric(CCC(testeb$p, testeb$pct)$rho.c[1]),
          as.numeric(CCC(testec$p, testec$pct)$rho.c[1]),as.numeric(CCC(tested$p, tested$pct)$rho.c[1]))