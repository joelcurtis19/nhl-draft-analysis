#draft data retrieved from hockeydb.com on 2020-05-23
#scraper used to pull this data can be found on my GitHub 
# https://github.com/joelcurtis19/nhl-draft-analysis

library(tidyverse)#required package

#path to read in data
draft_read_path <- "NHL Draft Data.csv" #file to be found on my GitHub

draft <- read_csv(draft_read_path)#read in csv file

#data cleaning
draft <- draft %>%
  mutate(GP = ifelse(is.na(GP), 0, GP),#replace NA's with 0's in GP
         G = ifelse(is.na(G), 0, G), #Na to 0 in G
         A = ifelse(is.na(A), 0, A), #Na to 0 in A
         Pts = ifelse(is.na(Pts), 0, Pts), #Na to 0 in Pts
         PIM = ifelse(is.na(PIM), 0, PIM), #Na to 0 in PIM
         Pos = as.factor(Pos), #Convert Pos to factor
         Drafted.By = as.factor(Drafted.By)) #convert Drafted.By team to factor

#create df of players still active in 2019-20 season
active_2020 <- draft %>% 
  group_by(Draft.Year, Last.Season)%>% #columns to group by
  count()%>% #counts the number of players by Draft.Year & Last.Season
  mutate(n_players = n)%>% #create new column with descriptive name
  select(-n)%>% #remove old count column
  filter(Last.Season == "2019-20") #remove players not active in 2019-20 season
#create line chart displaying the number players active in 2019-20 season
#by their draft year
active_2020_chart <- active_2020%>%
  ggplot(aes(x = Draft.Year, y = n_players))+ #draft year on x axis and number of players still active in draft on y axis
  geom_line(color = "darkblue", size = 1.2)+ #line chart with dark blue line
  ggtitle("NHL Players Active in 2019-20 Season by Draft Year")+ #descriptive title
  xlab("Draft Year")+ #descriptive x axis label
  ylab("Active Players")# descriptive y axis lavel

active_2020_chart

#elbow at 2002, so all players drafted in 2002 or before will be used

#filtering to 2002 draft and drafts before
draft_02 <- draft%>%
  filter(Draft.Year <= 2002)

#calculate number of players with a given number of games played
n_players <- draft_02 %>%
  group_by(GP)%>% #group by games played
  tally()%>% #count the instances of players in each group of GP
  mutate(player_count = n)%>% #store the count in new col: player_count
  mutate(total_players = sum(player_count))%>% #calculate total players
  mutate(prob = player_count/total_players)%>% #percent of players in each group of GP
  mutate(cumul_prob = cumsum(prob))%>% #calculate the cumulative percent of players
  mutate(prob_n_games = 1 - ifelse(is.na(lag(cumul_prob)), #the number of players that have played at least n games
                                   0,lag(cumul_prob)))%>% 
  select(-n) #remove unncessary column

#these are the milestones to be highlighted
milestones <- c(1, 25, 100, 250, 500, 750, 1000, 1250, 1500)
#these are the values in GP closest to the milestones
#that are also not below the milestone
miles_filter <- c(1, 25, 100, 250, 500, 750, 1000, 1251, 1514)

#create df to show probs of reaching given milestone
player_probs <- n_players %>%
  filter(GP %in% miles_filter)%>% #filter to value closest to milestone
  mutate(GP = as.factor(milestones), #change GP to a discrete variable
         prob_n_games = round(prob_n_games,3))%>% #round probability to 3 decimals
  select(GP, prob_n_games) #select only games played and probability of n games cols

#create line chart looking at number of picks per draft over time
picks_per_draft_chart <- draft %>%
  group_by(Draft.Year)%>% #group data by draft year
  summarise(n_picks = max(Num.))%>% #find the number of picks used in draft
  ggplot(aes(x = Draft.Year, y = n_picks))+ #draft year on x axis, number of picks in draft on y axis
  geom_line(color = "darkblue", size = 1.2)+ #line chart with dark blue line
  ggtitle("Number of Picks per Draft")+ #descriptive title for chart
  xlab("Draft Year")+ #descriptive title for x axis
  ylab("Number of Picks Used") #descriptive title for y axis

picks_per_draft_chart #display the chart

#create line chart displaying the number of rounds be draft over time
rounds_per_draft_chart <- draft %>%
  group_by(Draft.Year)%>% #group by draft year
  summarise(n_rounds = max(Round))%>% #find the number of rounds each draft had
  ggplot(aes(x = Draft.Year, y = n_rounds))+ #plot draft year on x axis, and number of rounds on y axis
  geom_line(color = "darkblue", size = 1.2)+ #line chart with dark blue line
  ggtitle("Number of Rounds by Draft")+ #descriptive title for chart
  xlab("Draft Year")+ #x axis label
  ylab("Number of Rounds") #y axis label

rounds_per_draft_chart #display the chart

#box and whisker plot of Games Played by draft round
gp_round_chart <- draft_02%>% 
  ggplot(aes(x = as.factor(Round), y = GP))+ #Draft Round on x axis, Games Played on y axis
  geom_boxplot(color = "darkblue")+ #boxplot colored dark blue
  ggtitle("NHL Players Total Games Played by Draft Round")+ #descriptive title
  xlab("Draft Round (excluding drafts after 2002)")+ #x axis label
  ylab("Games Played") #y axis label

gp_round_chart #display chart

#histogram of players games played
gp_hist <- draft_02 %>%
  ggplot(aes(x = GP))+ #create hist of GP
  geom_histogram(bins = 10)+ # have 10 bins
  ggtitle("Distribution of NHL Drafted Players Total Games Played")+ #descriptive title
  ylab("Count of Drafted Players")+ #y axis label
  xlab("Games Played")#x axis label

gp_hist #display chart

#histogram of players that played at least 1 game
gp_hist_played <- draft_02 %>%
  filter(GP > 0)%>% #filter out players that have 0 NHL games
  ggplot(aes(x = GP))+ #create hist of GP
  geom_histogram(bins = 10)+ #have 10 bins
  ggtitle("Distribution of NHL Players Total Games Played")+ #descriptive title
  ylab("Count of Players (at least 1 GP)")+ #y axis label
  xlab("Games Played") #x axis label

gp_hist_played #dsiplay chart

#probabilities of drafted player reaching gp milestone chart
prob_bar <- player_probs%>%
  ggplot(aes(x = prob_n_games, y = GP))+ #create col chart with probability of n games and milestone
  geom_col(fill = "dodgerblue4")+ #make bar color deep blue
  geom_text(aes(label=label_percent(accuracy = 0.1)(prob_n_games)),#add data labels in percent form
            position=position_dodge(width=0.9), hjust=-0.25)+ #place the lables at end of bars
  ggtitle("Probabilities of Drafted Player Reach GP Milestone (n=7799)")+ #descriptive title
  ylab("Games Played Milestone")+ #y axis label
  xlab("Probability of Milestone")+ #x axis label
  scale_x_continuous(limits = c(0,1.05), expand = c(0,0), ##remove extra space before graph
                     labels = scales::percent) #make the x axis labels percent form

prob_bar #display chart
