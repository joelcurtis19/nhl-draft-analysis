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

#create line chart looking at number of picks per draft over time
picks_per_draft_chart <- draft %>%
  group_by(Draft.Year)%>% #group data by draft year
  summarise(n_picks = max(Num.))%>% #find the number of picks used in draft
  ggplot(aes(x = Draft.Year, y = n_picks))+ #draft year on x axis, number of picks in draft on y axis
  geom_line(color = "darkblue", size = 1.2)+ #line chart with dark blue line
  ggtitle("Number of Picks per Draft")+ #descriptive title for chart
  xlab("Draft Year")+
  ylab("Number of Picks Used")

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

#box and whisker plot of Games Played by draft round
gp_round_chart <- draft_02%>% 
  ggplot(aes(x = as.factor(Round), y = GP))+ #Draft Round on x axis, Games Played on y axis
  geom_boxplot(color = "darkblue")+ #boxplot colored dark blue
  ggtitle("NHL Players Total Games Played by Draft Round")+ #descriptive title
  xlab("Draft Round (excluding drafts after 2002)")+ #x axis label
  ylab("Games Played") #y axis label

gp_round_chart #display chart
