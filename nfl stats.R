pckgs <- c('nflfastR', 'ggimage', 'tidyverse', 'ggrepel', 'scales', 'ggpubr')

for (i in 1:length(pckgs)) {
  if(!(pckgs[[i]] %in% installed.packages())) {
    install.packages(pckgs[[i]])
  }
  lapply(pckgs[[i]], library, character.only = T)
}

options(scipen = 9999)

df <- load_player_stats(seasons = c(2004:2021), stat_type = 'offense')

names(df)


#load data for team colors
teamCols <- teams_colors_logos


#let's look at all the QBs in 2021 who played at least 10 games & had 100+ passing attempts
#get a sense of their average EPAs over the course of the season
qbs2021 <- df %>%
  filter(season == 2021) %>%
  select(player_name, recent_team, season, week, completions, attempts, passing_yards, passing_tds,
         interceptions, passing_air_yards, passing_yards_after_catch,
         passing_first_downs, passing_epa) %>%
  left_join(teams_colors_logos, by = c('recent_team' = 'team_abbr'))


#now, get their average epa and total attempts, grouped by player per team
epas2021 <- qbs2021 %>%
  group_by(recent_team, player_name, team_color) %>%
  summarize(
    epa = mean(passing_epa, na.rm = T),
    attempts = sum(attempts),
    n = n()
  ) %>%
  na.omit() %>%
  filter(attempts >= 100) %>%
  arrange(epa) %>%
  as.data.frame

#separate df for aaron rodgers (they'll have to be merged)
arodg <- filter(epas2021, player_name == 'Aa.Rodgers' | player_name == 'A.Rodgers')

#add his stats together
rodgers <- arodg[2,]

rodgers[1,4] <- mean(c(arodg[1,4], arodg[2,4]))
rodgers[1,5] <- sum(c(arodg[1,5], arodg[2,5]))
rodgers[1,6] <- sum(c(arodg[1,6], arodg[2,6]))

#remove him from the original database
epas2021 <- filter(epas2021, player_name != 'Aa.Rodgers' & player_name != 'A.Rodgers')

#add his updated numbers back into the database
epas2021 <- rbind(epas2021, rodgers)

#and finally, we graph it in an ordered bar graph
epas2021 %>%
  ggplot(aes(reorder(recent_team, -epa), epa)) +
  geom_bar(stat = 'identity', alpha = .75, position = 'dodge', fill = epas2021$team_color) +
  theme_bw() +
  geom_text_repel(aes(label = player_name)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 12)) +
  labs(y = 'Passing - Expected Points Added',
       x = '',
       title = 'Average EPA of Starting QBs (2021)',
       subtitle = 'Minimum of 100 Attempts') +
  theme(plot.title = element_text(size = 15, hjust = .5, face = 'bold'),
        plot.subtitle = element_text(size = 7, hjust = .5, face = 'italic')) +
  coord_flip()

# ggsave('passing game epa (2021).jpg', device = 'jpeg', units = 'cm', path = 'misc R/nfl analyses')


#now let's look at the 2021 season again, this time focusing on team efficiency
#we'll do this by determining each team's passing and receiving epa values
eff2021 <- df %>%
  filter(season == 2021) %>%
  select(player_name, recent_team, passing_epa, receiving_epa) %>%
  left_join(teams_colors_logos, by = c('recent_team' = 'team_abbr')) %>%
  group_by(team_nick, team_color) %>%
  summarize(
    passingEpa = mean(passing_epa, na.rm = T),
    receivingEpa = mean(receiving_epa, na.rm = T),
    n = n()
  ) %>%
  na.omit() %>%
  as.data.frame

#then visualize it
#add a regression line to predict where they "should" be and see if they're off that line
ggplot(eff2021, aes(passingEpa, receivingEpa)) +
  geom_point(color = eff2021$team_color, alpha = .75, size = 4) +
  geom_hline(yintercept = mean(eff2021$receivingEpa), color = 'red', linetype = 'dashed', alpha = .4) +
  geom_vline(xintercept = mean(eff2021$passingEpa), color = 'red', linetype = 'dashed', alpha = .4) +
  geom_smooth(method = 'lm', se = F, color = 'gray70') +
  geom_text_repel(aes(label = team_nick)) +
  labs(x = 'Passing EPA',
       y = 'Receiving EPA',
       title = 'Passing Game Efficiency (2021)') +
  theme_bw() +
  theme(plot.title = element_text(size = 10, hjust = .5, face = 'bold')) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 16)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 16))

# ggsave('passing game efficiency (2021).jpg', device = 'jpeg', units = 'cm', path = 'misc R/nfl analyses')



#now let's take a look how teams fared on 3rd down over the course of the season (2021)

#load the data for the 2021 season
df21 <- load_pbp(2021)

down34 <- df21 %>%
  select(home_team, away_team, week, game_id, season_type, play_type, yards_gained, epa, wp, down, third_down_converted, fourth_down_converted) %>%
  filter(season_type == 'REG') %>%
  filter(down == 3 | down == 4) %>%
  filter(play_type == 'run' | play_type == 'pass')

#we have some more cleaning to do
#we can't have two team names in the same dataframe in the same row
#instead, make a separate df for home teams and away teams, then merge them
home <- down34 %>%
  select(-away_team) %>%
  rename(team = home_team) %>%
  mutate(gameType = 'Home') %>%
  left_join(teamCols, by = c('team' = 'team_abbr'))

away <- down34 %>%
  select(-home_team) %>%
  rename(team = away_team) %>%
  mutate(gameType = 'Away') %>%
  left_join(teamCols, by = c('team' = 'team_abbr'))

downs <- rbind(home, away)


#now let's look at third and fourth downs
thirdConv <- downs %>%
  filter(down == 3) %>%
  select(-fourth_down_converted) %>%
  group_by(team, play_type, team_color) %>%
  summarize(
    convertPerc = mean(third_down_converted)
  ) %>%
  as.data.frame

fourthConv <- downs %>%
  filter(down == 4) %>%
  select(-third_down_converted) %>%
  group_by(team, play_type, team_color) %>%
  summarize(
    convertPerc = mean(fourth_down_converted)
  ) %>%
  as.data.frame


#now let's make some plots of 3rd vs 4th down success
thirdPass <- thirdConv %>%
  filter(play_type == 'pass') %>%
  arrange(convertPerc)
thirdRun <- thirdConv %>%
  filter(play_type == 'run') %>%
  arrange(convertPerc)

fourthPass <- fourthConv %>%
  filter(play_type == 'pass') %>%
  arrange(convertPerc)
fourthRun <- fourthConv %>%
  filter(play_type == 'run') %>%
  arrange(convertPerc)


#start with conversion percentage
#third downs
pass3rd <- thirdPass %>%
  ggplot(aes(reorder(team, -convertPerc), convertPerc)) +
  geom_bar(stat = 'identity', alpha = .75, fill = thirdPass$team_color) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0,1,.1)) +
  expand_limits(y = 1) +
  theme_bw() +
  coord_flip() +
  labs(x = '',
       y = 'Third Down Conversion %',
       title = 'Pass Plays') +
  theme(plot.title = element_text(size = 10, face = 'bold'))

run3rd <- thirdRun %>%
  ggplot(aes(reorder(team, -convertPerc), convertPerc)) +
  geom_bar(stat = 'identity', alpha = .75, fill = thirdRun$team_color) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0,1,.1)) +
  expand_limits(y = 1) +
  theme_bw() +
  coord_flip() +
  labs(x = '',
       y = 'Third Down Conversion %',
       title = 'Rush Plays') +
  theme(plot.title = element_text(size = 10, face = 'bold'))


ggarrange(pass3rd, run3rd,
          ncol = 2)

# ggsave('third down success rates.jpg', device = 'jpeg', units = 'cm', path = 'misc R/nfl analyses')

#fourth downs
pass4th <- fourthPass %>%
  ggplot(aes(reorder(team, -convertPerc), convertPerc)) +
  geom_bar(stat = 'identity', alpha = .75, fill = fourthPass$team_color) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0,1,.2)) +
  expand_limits(y = 1) +
  theme_bw() +
  coord_flip() +
  labs(x = '',
       y = 'Fourth Down Conversion %',
       title = 'Pass Plays') +
  theme(plot.title = element_text(size = 10, face = 'bold'))

run4th <- fourthRun %>%
  ggplot(aes(reorder(team, -convertPerc), convertPerc)) +
  geom_bar(stat = 'identity', alpha = .75, fill = fourthRun$team_color) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0,1,.2)) +
  expand_limits(y = 1) +
  theme_bw() +
  coord_flip() +
  labs(x = '',
       y = 'Fourth Down Conversion %',
       title = 'Rush Plays') +
  theme(plot.title = element_text(size = 10, face = 'bold'))


ggarrange(pass4th, run4th,
          ncol = 2)

# ggsave('fourth down success rates.jpg', device = 'jpeg', units = 'cm', path = 'misc R/nfl analyses')


#now let's get interesting
#make two additional plots looking at mean team epa vs mean yards gained for successful 3rd downs
#then do it with 4th down conversions
thirdSuccessP <- downs %>%
  filter(third_down_converted == 1 & play_type == 'pass') %>%
  group_by(team, team_color) %>%
  summarize(
    epa = mean(epa),
    yards = mean(yards_gained)
  ) %>%
  as.data.frame

thirdSuccessR <- downs %>%
  filter(third_down_converted == 1 & play_type == 'run') %>%
  group_by(team, team_color) %>%
  summarize(
    epa = mean(epa),
    yards = mean(yards_gained)
  ) %>%
  as.data.frame

fourthSuccessP <- downs %>%
  filter(fourth_down_converted == 1 & play_type == 'pass') %>%
  group_by(team, play_type, team_color) %>%
  summarize(
    epa = mean(epa),
    yards = mean(yards_gained)
  ) %>%
  as.data.frame

fourthSuccessR <- downs %>%
  filter(fourth_down_converted == 1 & play_type == 'run') %>%
  group_by(team, play_type, team_color) %>%
  summarize(
    epa = mean(epa),
    yards = mean(yards_gained)
  ) %>%
  as.data.frame


thirdSuccessP %>%
  ggplot(aes(yards, epa)) +
  geom_point(size = 4, alpha = .5, color = thirdSuccessP$team_color) +
  geom_hline(yintercept = mean(thirdSuccessP$epa), color = 'red', linetype = 'dashed', alpha = .4) +
  geom_vline(xintercept = mean(thirdSuccessP$yards), color = 'red', linetype = 'dashed', alpha = .4) +
  theme_bw() +
  geom_text_repel(aes(label = team))

thirdSuccessR %>%
  ggplot(aes(yards, epa)) +
  geom_point(size = 4, alpha = .5, color = thirdSuccessR$team_color) +
  geom_hline(yintercept = mean(thirdSuccessR$epa), color = 'red', linetype = 'dashed', alpha = .4) +
  geom_vline(xintercept = mean(thirdSuccessR$yards), color = 'red', linetype = 'dashed', alpha = .4) +
  theme_bw() +
  geom_text_repel(aes(label = team))


fourthSuccessP %>%
  ggplot(aes(yards, epa)) +
  geom_point(size = 4, alpha = .5, color = fourthSuccessP$team_color) +
  geom_hline(yintercept = mean(fourthSuccessP$epa), color = 'red', linetype = 'dashed', alpha = .4) +
  geom_vline(xintercept = mean(fourthSuccessP$yards), color = 'red', linetype = 'dashed', alpha = .4) +
  theme_bw() +
  geom_text_repel(aes(label = team))

fourthSuccessR %>%
  ggplot(aes(yards, epa)) +
  geom_point(size = 4, alpha = .5, color = fourthSuccessR$team_color) +
  geom_hline(yintercept = mean(fourthSuccessR$epa), color = 'red', linetype = 'dashed', alpha = .4) +
  geom_vline(xintercept = mean(fourthSuccessR$yards), color = 'red', linetype = 'dashed', alpha = .4) +
  theme_bw() +
  geom_text_repel(aes(label = team))


















