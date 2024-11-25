library(tidyverse)
library(ggforce)
library(cowplot)

olympic_data <- read_csv('olympic_womens_dataset.csv')

# Checking what games I have in the data

unique_combinations <-
  olympic_data %>%
  distinct(game_date, `Home Team`, `Away Team`)

# Filter out collegiate games
olympic_only <-
  olympic_data %>%
  filter(str_detect(`Home Team`, "Olympic"))

# Filtering to just Olympics
olympic_only <-
  olympic_only %>%
  filter(game_date < "2019-01-01")
  
# Adding in game winning goal
# first have to calculate go ahead goal


# Player data
# assists ?

player_data <- olympic_only %>%
  group_by(Player, Team) %>%
  summarize(
    Total_Shots = sum(Event == "Shot", na.rm = TRUE),
    Goals = sum(Event == "Goal", na.rm = TRUE),
    Passes = sum(Event == "Play", na.rm = TRUE),
    Incomplete_Passes = sum(Event == "Incomplete Play", na.rm = TRUE),
    Takeaways = sum(Event == "Takeaway", na.rm = TRUE),
    Puck_Recoveries = sum(Event == "Puck Recovery", na.rm = TRUE),
    Dump_Ins = sum(Event == "Zone Entry" & `Detail 1` == "Dumped"),
    Dump_Outs = sum(Event == "Dump Out", na.rm = TRUE),
    Zone_Entries = sum(Event == "Zone Entry" & `Detail 1` == "Carried"),
    Faceoff_Wins = sum(Event == "Faceoff Win", na.rm = TRUE),
    Penalties_Taken = sum(Event == "Penalty Taken", na.rm = TRUE),
    Shot_Traffic = sum(Event == "Shot" & "Detail 3" == "true", na.rm = TRUE),
    Shot_OneTimers = sum(Event == "Shot" & "Detail 4" == "true", na.rm = TRUE),
  ) %>%
  ungroup()


# Can mess around with this table, but going with Poulin as MVP overall with her number of goals and passes as the two main variables

  player_data %>%
  mutate(
    Shooting_Accuracy = Goals /Total_Shots,
    Passing_Accuracy = Passes / (Passes + Incomplete_Passes)
  ) %>%
    dplyr::select(Player, Team, Goals, Passes, Puck_Recoveries, Zone_Entries, Passing_Accuracy, Shooting_Accuracy) %>%
  arrange(desc(Goals), desc(Passes), desc(Puck_Recoveries), desc(Zone_Entries), 
          desc(Passing_Accuracy), desc(Shooting_Accuracy)) %>%
    head(10)


# Powerplay
# What teams are best on the powerplay

  # Opposing team's ability to clear the puck on the PP
  # defining as puck recovery where x coordinate is in D zone
olympic_only <- 
  olympic_only %>%
  mutate(
    PPGF_home = ifelse((Event == "Goal" & `Home Team` == Team & `Home Team Skaters` > `Away Team Skaters` ), TRUE, FALSE) ,
    PPGF_away = ifelse((Event == "Goal" & `Away Team` == Team & `Away Team Skaters` > `Home Team Skaters` ), TRUE, FALSE),
    PPshots_home = ifelse((Event == "Shot" & `Home Team` == Team & `Home Team Skaters` > `Away Team Skaters` ), TRUE, FALSE),
    PPshots_away =  ifelse((Event == "Shot" & `Away Team` == Team & `Away Team Skaters` > `Home Team Skaters` ), TRUE, FALSE),
    penalties_home = ifelse(Event == "Penalty Taken" & `Home Team` == Team, TRUE, FALSE),
    penalties_away = ifelse(Event == "Penalty Taken" & `Away Team` == Team, TRUE, FALSE),
    PP_recoveries_home = ifelse(`X Coordinate` < 50 & Event == "Puck Recovery" &  `Home Team` == Team & `Home Team Skaters` > `Away Team Skaters`, TRUE, FALSE),
    PP_recoveries_away = ifelse(`X Coordinate` < 50 & Event == "Puck Recovery" &  `Away Team` == Team & `Away Team Skaters` > `Home Team Skaters`, TRUE, FALSE)
  )



# Game data
game_data <- 
  olympic_only %>%
  filter(`Home Team Skaters` == 5 & `Away Team Skaters` == 5) %>%
  group_by(game_date, `Home Team`, `Away Team`) %>%
  summarise(
    HomeGoals = sum(Event == "Goal" & `Home Team` == Team),
    AwayGoals = sum(Event == "Goal" & `Away Team` == Team),
    HomePP_goals = sum(PPGF_home),
    AwayPP_goals = sum(PPGF_away),
    HomePP_shots = sum(PPshots_home),
    AwayPP_shots = sum(PPshots_away),
    Home_penalties = sum(penalties_home),
    Away_penalties = sum(penalties_away),
    Home_recoveries = sum(PP_recoveries_home),
    Away_recoveries = sum(PP_recoveries_away),
    .groups = "drop")

game_data <- game_data %>%
  mutate(
    Home_opposing_penalties = Away_penalties,
    Away_opposing_penalties = Home_penalties
  )

# longer
long_data <- game_data %>%
  pivot_longer(
    cols = c(HomeGoals, AwayGoals, HomePP_goals, AwayPP_goals, HomePP_shots, AwayPP_shots,
             Home_recoveries, Away_recoveries,
             Home_opposing_penalties, Away_opposing_penalties),
    names_to = c("Type", ".value"),
    names_pattern = "(Home|Away)(.*)"
  ) %>% ungroup()

long_data <- long_data %>%
  mutate(
    Team = ifelse(Type == "Home", `Home Team`, `Away Team`),
    Opposing_penalties = ifelse(Type == "Home", Away_penalties, Home_penalties)  # Map opposing penalties explicitly
  ) %>%
  select(game_date, Team, Goals, PP_goals, Opposing_penalties, PP_shots, `_recoveries`) 


# Calculate power play success percentage
pp_data <- long_data %>%
  mutate(
    PP_success_percentage = ifelse(Opposing_penalties > 0, (PP_goals / Opposing_penalties) * 100, 0)
  )

pp_data2 <-
  pp_data %>%
  group_by(Team) %>%
  summarise(
    Goals = sum(Goals),
    PP_goals = sum(PP_goals),
    Opportunities = sum(Opposing_penalties),
    PP_shots = sum(PP_shots),
    recoveries = sum(`_recoveries`)
    
    
  )

pp_data2 <-
  pp_data2 %>%
  mutate(Success_Rate = PP_goals / Opportunities,
         Shots_per_opportunity = PP_shots / Opportunities,
         Recoveries_per_opportunity = recoveries / Opportunities) %>%
  arrange(desc(Success_Rate))

print(pp_data2)
# Canada has the highest success rate on the powerplay, followed by the U.S.
# Fewer shots per opportunity, but barely
# They didn't have to recover in their own zone as often (interpreting as a version of clears)

# What are the attributes/characteristics of the top powerplay teams that make them successful?


canada_powerplay <-
  olympic_only %>%
  filter((`Home Team` == "Olympic (Women) - Canada" & `Home Team Skaters` > `Away Team Skaters` |
           `Away Team` == "Olympic (Women) - Canada" & `Away Team Skaters` > `Home Team Skaters`))  %>%
  filter(`Home Team Skaters` != 6,
         `Away Team Skaters` != 6)


# Shots while on the PP

# Source in function to create the rink plot in ggplot
source("plot_rink.R")

canada_pp_shots = canada_powerplay %>%
  filter(Event %in% c("Shot", "Goal")) %>%
  filter(Team == "Olympic (Women) - Canada") %>%
  mutate(event_successful = ifelse(Event == "Goal", TRUE, FALSE))

plot_rink(ggplot(canada_pp_shots)) +
  geom_point(aes(x = `X Coordinate`, y = `Y Coordinate`, fill = event_successful), shape = 21, size = 3.5)

# Plotting density

plot_rink(ggplot(canada_pp_shots)) +
  geom_bin2d(aes(x = `X Coordinate`, y = `Y Coordinate`), bins = 10) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(fill = "Shot Density") +
  theme_minimal()

plot_rink(ggplot(canada_pp_shots)) +
  geom_density2d_filled(aes(x = `X Coordinate`, y = `Y Coordinate`, fill = ..level..)) +
  scale_fill_viridis_d() +  # Use a perceptually uniform colormap
  labs(fill = "Shot Density") +
  theme_minimal()

plot_rink(ggplot(canada_pp_shots))

plot_rink(ggplot(canada_pp_shots)) +
  stat_density_2d(aes(x = `X Coordinate`, y = `Y Coordinate`, fill = ..density..), geom = "raster", contour = FALSE, alpha = 0.8) +
  scale_fill_viridis_c() +  # Use a continuous colormap
  labs(fill = "Shot Density") +
  theme_minimal()

# Trying Non-Canada powerplays

non_canada_pp <-
  olympic_only %>%
  filter((`Home Team` != "Olympic (Women) - Canada" & `Home Team Skaters` > `Away Team Skaters` |
            `Away Team` != "Olympic (Women) - Canada" & `Away Team Skaters` > `Home Team Skaters`))  %>%
  filter(`Home Team Skaters` != 6,
         `Away Team Skaters` != 6) %>%
  mutate(TeamOnPP = ifelse(`Home Team Skaters` > `Away Team Skaters`, `Home Team`, `Away Team`))

non_canada_pp_shots <- 
  non_canada_pp %>%
  filter(Event %in% c("Shot", "Goal")) %>%
  mutate(event_successful = ifelse(Event == "Goal", TRUE, FALSE))

plot_rink(ggplot(non_canada_pp_shots)) +
  stat_density_2d(aes(x = `X Coordinate`, y = `Y Coordinate`, fill = ..density..), geom = "raster", contour = FALSE, alpha = 0.8) +
  scale_fill_viridis_c() +  # Use a continuous colormap
  labs(fill = "Shot Density") +
  theme_minimal()
# Canada had most of their shots right in front of the net, other teams had more perimeter shots from defensemen
# Argument is they had more high danger shots



# Passing Success While on PP

canada_pp_passing <-
  canada_powerplay %>%
  filter(Event == "Play" | Event == "Incomplete Play",
         Team == "Olympic (Women) - Canada") %>%
  mutate(TeamOnPP = "Olympic (Women) - Canada")

non_canada_pp_passing <-
  non_canada_pp %>%
  filter(Event == "Play" | Event == "Incomplete Play",
         Team == TeamOnPP)
  
all_pp_plays <-
  rbind(canada_pp_passing, non_canada_pp_passing)

team_level_pp_passing <-
  all_pp_plays %>%
  group_by(Team) %>%
  summarise(
    Passes = sum(Event == "Play", na.rm = TRUE),
    Incomplete_Passes = sum(Event == "Incomplete Play", na.rm = TRUE)
  ) %>%
  mutate(Passing_success_rate = Passes / (Passes + Incomplete_Passes))

# Interesting: they had the second worse pass success rate

# Formation? Maybe look at where completed passes went to?
# This is cool whether you are looking at origin or destination of pass

canada_complete_pp_passes <- canada_pp_passing %>%
  filter(Event == "Play", 
         `X Coordinate` > 125)

plot_rink(ggplot(canada_complete_pp_passes)) +
  stat_density_2d(aes(x = `X Coordinate 2`, y = `Y Coordinate 2`, fill = ..density..), geom = "raster", contour = FALSE, alpha = 0.6) +
  scale_fill_viridis_c() +  # Use a continuous colormap
  labs(fill = "Pass Destination") +
  theme_minimal()

non_canada_complete_pp_passes <- non_canada_pp_passing %>%
  filter(Event == "Play",
         `X Coordinate` > 125) 

plot_rink(ggplot(non_canada_complete_pp_passes)) +
  stat_density_2d(aes(x = `X Coordinate 2`, y = `Y Coordinate 2`, fill = ..density..), geom = "raster", contour = FALSE, alpha = 0.6) +
  scale_fill_viridis_c() +  # Use a continuous colormap
  labs(fill = "Pass Destination") +
  theme_minimal()

# Opposing team's ability to clear the puck on the PP
# defining as puck recovery where x coordinate is in D zone

# Entry denials
# let's start with percentage that are carries against
# Remember to filter to 5 on 5 events
zone_entries <- 
  olympic_only %>%
  filter(Event == "Zone Entry",
         `Home Team Skaters` == 5 & `Away Team Skaters` == 5)

zone_entry_d <-
  zone_entries %>%
  group_by(`Player 2`) %>%
  summarise(
    Carries = sum(`Detail 1` == "Carried", na.rm = TRUE),
    Dumped = sum(`Detail 1` == "Dumped", na.rm = TRUE),
    Played = sum(`Detail 1` == "Played", na.rm = TRUE)
  ) %>%
  mutate(Total = Carries + Dumped + Played,
         Percent_Carries = Carries/Total,
         Percent_Played = Played / Total)

# lowest percentage of carries against
zone_entry_d %>%
  arrange(Percent_Carries) %>%
  head(10)

# highest percentage played
zone_entry_d %>%
  arrange(desc(Percent_Played))

# let's try and look into what happens right after a zone entry

zone_entry_events <-
  olympic_only %>%
  filter(Event == "Zone Entry" | lag(Event) == "Zone Entry") %>%
  mutate(Defender = ifelse(Event == "Zone Entry", `Player 2`, lag(`Player 2`)),
         Diff_team = ifelse(Team == lag(Team), FALSE, TRUE)) %>%
  filter(Event != "Zone Entry")

zone_entry_events_player <-
  zone_entry_events %>%
  group_by(Defender) %>%
  summarise(
    Gained_Possession = sum(Diff_team),
    Appearances = n(),
    Gained_Possession_Percent = Gained_Possession / Appearances
  )

# Team Analysis
# Ok I am going to start by defining Entry denials as either when the defending team has the puck next (sorta showed that above)
# OR if the next event's location is not in the offensive zone
# and then want to incorporate fewest offensive chances next
# so subtract the amount of shots/chances generated from zone entries
# Maybe if an offensive play/shot is next
# filter to 5 on 5 events

entry_denials <- 
  olympic_only %>%
  mutate(EntryDenial = ifelse(Event == "Zone Entry" & lead(Team) != Team & lead(Event) != "Faceoff Win" |
                                Event == "Zone Entry" & lead(Team) == Team & lead(`X Coordinate` < 125),
                              TRUE, FALSE
                              ))
# Just need to clarify - this will involve carries where the defenseman ends up 
# Eventually recovering the puck
# OK logic change: we're going to say if its a failed dump in or if its played 
# and the next possession is the defending team

entry_denials <-
  olympic_only %>%
  mutate(EntryDenial = ifelse((Event == "Dump In/Out" & `Detail 1` == "Lost") |
                               ( Event == "Zone Entry" & `Detail 1` == "Played" & lead(Team) != Team),
         TRUE, FALSE))
# is an icing an entry denial? Probably not
# Ok now a metric talking about limiting offense
entry_denials <-
  entry_denials %>%
  mutate(OffensiveEntry = ifelse(Event == "Zone Entry" & 
                                   (lead(Event) == "Play" | lead(Event) == "Shot" | lead(Event) == "Goal")
                                 & lead(Team) == Team, TRUE, FALSE),
         DefendingTeam = ifelse(Team == `Home Team`, `Away Team`, `Home Team`))

entry_denials2 <-
  entry_denials %>%
  filter(Event == "Zone Entry" | Event == "Dump In/Out") %>%
  group_by(DefendingTeam) %>%
  summarise(
    OffensiveEntriesAllowed = sum(OffensiveEntry),
    EntryDenials = sum(EntryDenial),
    TotalEntriesAgainst = n()
  ) %>%
  mutate(EDR = EntryDenials / TotalEntriesAgainst,
         OER = OffensiveEntriesAllowed / TotalEntriesAgainst,
         Tradeoff = EDR - OER) %>%
  arrange(desc(Tradeoff))

# Visualization displaying Canada's Entry Denials
# first just looking at where the zone entries happen
canada_entry <- 
  entry_denials %>%
  filter(
    `Home Team` == "Olympic (Women) - Canada" | `Away Team` == "Olympic (Women) - Canada",
    Team != "Olympic (Women) - Canada")
  

entry_denials_only <-
  entry_denials %>%
  filter(EntryDenial == TRUE,
        `Home Team` == "Olympic (Women) - Canada" | `Away Team` == "Olympic (Women) - Canada",
        Team != "Olympic (Women) - Canada")


plot_rink(ggplot(data = entry_denials_only)) +
  stat_density_2d(aes(x = `X Coordinate`, y = `Y Coordinate`, fill = ..density..), geom = "raster", contour = FALSE, alpha = 0.6) +
  scale_fill_viridis_c() +  # Use a continuous colormap
  labs(fill = "Pass Destination") +
  theme_minimal()

offensive_entries_only <-
  entry_denials %>%
  filter(OffensiveEntry == TRUE,
         `Home Team` == "Olympic (Women) - Canada" | `Away Team` == "Olympic (Women) - Canada",
         Team != "Olympic (Women) - Canada")


plot_rink(ggplot(data = offensive_entries_only)) +
  stat_density_2d(aes(x = `X Coordinate`, y = `Y Coordinate`, fill = ..density..), geom = "raster", contour = FALSE, alpha = 0.6) +
  scale_fill_viridis_c() +  # Use a continuous colormap
  labs(fill = "Pass Destination") +
  theme_minimal()

ggplot(data = offensive_entries_only, aes(x = `Y Coordinate`)) + geom_histogram() +
  theme_bw()

ggplot(data = entry_denials_only, aes(x = `Y Coordinate`)) + geom_histogram() +
  theme_bw()

# overlayed histograms



ggplot() +
  geom_histogram(data = offensive_entries_only, 
                 aes(x = `Y Coordinate`, fill = "Offensive Entries"), 
                 alpha = 0.5, bins = 30) +
  geom_histogram(data = entry_denials_only, 
                 aes(x = `Y Coordinate`, fill = "Entry Denials"), 
                 alpha = 0.5, bins = 30) +
  labs(title = "Canada's Offensive Zone Entries Against vs. Entry Denials Blue Line Frequency",
       x = "Y Coordinate",
       y = "Frequency",
       fill = "Category") +  # Legend title
  theme_minimal()

# grouped bar plot
# or stacked?

zone_entries_only <-
  olympic_only %>%
  filter(Event == "Zone Entry") %>%
  mutate(defending_team = ifelse(`Home Team` == Team, `Away Team`, `Home Team`),
         Canada = ifelse(defending_team == "Olympic (Women) - Canada", "Canada", "Average of Other Teams"))

ggplot(data = zone_entries_only, aes(fill = `Detail 1`, x = Canada)) + 
  geom_bar(position = "fill") + theme_bw()





