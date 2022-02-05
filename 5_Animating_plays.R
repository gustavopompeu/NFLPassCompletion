# Necessary packages
library(tidyverse)
library(gganimate)
library(cowplot)
library(repr)
library(reshape2)

# Loading rdata
# File available on google drive - Read readme.txt
load("data5_1.rda")
load("data5_2.rda")
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

# Uncomment one of the filters
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
  # easy pass completion - Cleveland play
  #filter(gameId==2018091603, playId==3683) %>%
  # easy pass incompletion - Pittsburgh play
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
# Change example_play to GERONIMO TD
data2 <- melt(example_play, id="frameId")
mediav1 <- mean(example_play$V1)
mediap <- mean(example_play$p)
ggplot(data2, aes(x=frameId,y=value, colour=variable)) + geom_line(alpha=0.35) + geom_point(alpha=0.35) + 
  geom_hline(aes(yintercept =  mediav1, linetype="Average Predicted Target"), colour="blue") + 
  geom_hline(aes(yintercept =  mediap, linetype="Average General"), colour="red") +
  geom_hline(aes(yintercept =  .147, linetype="Next Gen Stats"), colour="black") +
  scale_linetype_manual(name = "", values = c(2, 2, 2), guide = guide_legend(override.aes = list(color = c("red", "blue", "black")))) +
  scale_color_manual(labels=c("Predicted Target", "General"), values = c("blue", "red")) + ylim(0.1,0.65) + theme_bw() + 
  labs(x = "Frame number", y = "Completion Probability", colour="Completion Probability", title="(A)") + theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=57, y=0.232, label="0.240", color="red") + annotate("text", x=74, y=0.216, label="0.224", color="blue") +
  annotate("text", x=79.5, y=.635, label="0.624", color="violet") +
  annotate("text", x=65, y=mediav1+.01, label=as.character(round(mediav1,3)), color="blue", size=3) +
  annotate("text", x=65, y=mediap+.01, label=as.character(round(mediap,3)), color="red", size=3) +
  annotate("text", x=65, y=.157, label=0.147, color="black", size=3)

# Rob Gronkowski play
# Change example_play to GRONK TD
data2 <- melt(example_play, id="frameId")
mediav1 <- mean(example_play$V1)
mediap <- mean(example_play$p)
ggplot(data2, aes(x=frameId,y=value, colour=variable)) + geom_line(alpha=0.35) + geom_point(alpha=0.35) + 
  geom_hline(aes(yintercept =  mediav1, linetype="Average Predicted Target"), colour="blue") + 
  geom_hline(aes(yintercept =  mediap, linetype="Average General"), colour="red") +
  geom_hline(aes(yintercept =  .188, linetype="Next Gen Stats"), colour="black") +
  scale_linetype_manual(name = "", values = c(2, 2, 2), guide = guide_legend(override.aes = list(color = c("red", "blue", "black")))) +
  scale_color_manual(labels=c("Predicted Target", "General"), values = c("blue", "red")) + theme_bw() + 
  labs(x = "Frame number", y = "Completion Probability", colour="Completion Probability", title="(B)") + theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=36, y=0.1, label="0.118", color="red") + annotate("text", x=36, y=0.155, label="0.136", color="blue") +
  annotate("text", x=49.8, y=.31, label="0.288", color="violet") +
  annotate("text", x=47, y=.81, label="0.796", color="violet") +
  annotate("text", x=42.5, y=mediav1+.01, label=as.character(round(mediav1,3)), color="blue", size=3) +
  annotate("text", x=42.5, y=mediap+.01, label=as.character(round(mediap,3)), color="red", size=3) +
  annotate("text", x=42.5, y=.178, label=0.188, color="black", size=3)