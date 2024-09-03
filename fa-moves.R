library(tidyverse)
library(nflreadr)




rosters_wk = load_rosters_weekly(2002:2023)
contracts = load_contracts()
teams = load_teams()
rosters = load_rosters(1990:2024)



teams2 = teams |> dplyr::select(team_abbr, team_nick) |> mutate(team_abbr = clean_team_abbrs(team_abbr))



contracts |> filter(player == "Isaiah Searight")



contracts_red = contracts |>
  filter(year_signed != 0, inflated_apy != 0, !is.na(years)) |> 
  left_join(teams2, by = c("team" = "team_nick")) |> 
  dplyr::select(player, team_abbr, position, year_signed, years, inflated_apy) |> 
  group_by(player) |> 
  arrange(year_signed) |> 
  mutate(year_till = as.integer(lead(year_signed) - 1)) |> 
  ungroup() |> 
  mutate(year_till = if_else(is.na(year_till), as.integer(year_signed + years), year_till)) |>
  rowwise() %>%
  mutate(years = list(seq(year_signed, year_till))) %>%
  unnest(years) |>
  dplyr::select(-year_signed, -year_till) |> 
  arrange(years)
contracts_red |> filter(player == "Justin Simmons")


fa_player_df = rosters |> 
  dplyr::select(season, team, full_name, gsis_id) |> 
  mutate(team = clean_team_abbrs(team)) |> 
  group_by(full_name, gsis_id) |> 
  arrange(season) |> 
  mutate(prev_team = lag(team)) |>
  ungroup() |> 
  filter(season >= 2000, team != prev_team) |> 
  left_join(contracts_red, by = c("full_name" = "player", "team" = "team_abbr", "season" = "years")) |> 
  filter(!is.na(inflated_apy))


fa_additions = fa_player_df |> 
  group_by(team, season) |> 
  summarise(value_added = sum(inflated_apy),
            .groups = "drop") |> 
  # filter(season == 2024) |>
  arrange(-value_added)


fa_losses = fa_player_df |> 
  group_by(prev_team, season) |> 
  summarise(value_lost = sum(inflated_apy),
            .groups = "drop") |> 
  # filter(season == 2024) |> 
  arrange(-value_lost)




# OFFSEASON IMPROVEMENT!!!
fa_df = fa_additions |> 
  left_join(fa_losses, by = c("team" = "prev_team", "season")) |> 
  mutate(value_added = if_else(is.na(value_added), 0, value_added),
         value_lost = if_else(is.na(value_lost), 0, value_lost)) |> 
  mutate(net_value = value_added - value_lost) |>
  mutate(season = season - 1) |>
  # Some have none, which will just adjust to 0
  arrange(season, team)


# write.csv(fa_df, file = "offeseason-player-value-changes.csv")


# Quick 2024 Analysis

fa_df = read_csv("coding-projects/nfl-fast-r/offeseason-player-value-changes.csv")[, -1]


fa_df |> filter(season == 2023) |> arrange(-value_added)

ggplot(fa_df |> filter(season == 2023), 
       aes(x = net_value, y = reorder(team, net_value))) +
  labs(
    title = "NFL FA Changes (2024)",
    subtitle = "based on cap-adjusted apy for each player change",
    caption = "By: Sam Burch  |  Data @nflfastR",
    x = "Total Player Value Added (millions)"
  ) +
  scale_x_continuous(breaks = seq(-75, 75, 25)) +
  geom_col(aes(color = team, fill = team), alpha = .8, width = 1) +
  nflplotR::scale_fill_nfl(type = "primary") +
  nflplotR::scale_color_nfl(type = "secondary") +
  theme(
    plot.subtitle = element_text(size = 8, hjust = .5),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank(),  # Remove horizontal major grid lines
    panel.grid.minor.y = element_blank(),  # Remove horizontal minor grid lines
    panel.grid.major.x = element_line(color = "lightgray", size = 0.5, linetype = 2),  # Customize vertical major grid lines
    panel.grid.minor.x = element_blank(),
    panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .03, alpha = .8)

# ggsave("nfl-fa-value-24.png", width = 16, height = 12, units = "cm")





# Injuries ----------------------------------------------------------------



injury_df = rosters_wk |>
  mutate(team = clean_team_abbrs(team)) |> 
  group_by(full_name, gsis_id, team, season, position) |>
  # Doesn't account for just inactive games
  summarise(ir_games = sum(status == "RES"),
            games = n(),
            ir_rate = ir_games / games,
            .groups = "drop") |> 
  filter(ir_games >= 1) |> 
  # filter(position == "QB") |> 
  dplyr::select(season, team, position, full_name, ir_rate) |> 
  arrange(-ir_rate)


injury_df_final = contracts_red |> 
  filter(years < 2024, !is.na(team_abbr)) |> 
  # filter(player == "Aaron Rodgers") |>
  left_join(injury_df, by = c("player" = "full_name", 
                              "team_abbr" = "team", 
                              "years" = "season",
                              "position")) |> 
  mutate(ir_rate = if_else(is.na(ir_rate), 0, ir_rate)) |>
  mutate(injured_value_lost = inflated_apy * ir_rate) |>
  # arrange(-injured_value_lost) |> 
  rename(team = team_abbr,
         season = years) |> 
  group_by(team, season) |>
  summarise(tot_injured_value_lost = sum(injured_value_lost),
            .groups = "drop") |>
  arrange(-tot_injured_value_lost)

injury_df_final |> filter(season == 2023) |> arrange(-tot_injured_value_lost)

# write.csv(injury_df_final, file = "injury-value-lost.csv")


# Quick 2023 Analysis

injury_df = read_csv("coding-projects/nfl-fast-r/injury-value-lost.csv")[, -1]


injury_df |> filter(season == 2023) |> arrange(-tot_injured_value_lost)



ggplot(injury_df |> filter(season == 2023), 
       aes(x = tot_injured_value_lost, 
           y = reorder(team, tot_injured_value_lost))) +
  labs(
    title = "NFL Injury Luck (2023)",
    subtitle = "value lost due to player on IR  |  player value = cap-adjusted apy",
    caption = "By: Sam Burch  |  Data @nflfastR",
    x = "Total Player Value Lost (millions)"
  ) +
  # scale_x_continuous(breaks = seq(-75, 75, 25)) +
  geom_col(aes(color = team, fill = team), alpha = .8, width = 1) +
  nflplotR::scale_fill_nfl(type = "primary") +
  nflplotR::scale_color_nfl(type = "secondary") +
  theme(
    plot.subtitle = element_text(size = 8, hjust = .5),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank(),  # Remove horizontal major grid lines
    panel.grid.minor.y = element_blank(),  # Remove horizontal minor grid lines
    panel.grid.major.x = element_line(color = "lightgray", size = 0.5, linetype = 2),  # Customize vertical major grid lines
    panel.grid.minor.x = element_blank(),
    panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .03, alpha = .8)


# ggsave("nfl-injury-luck-23.png", width = 16, height = 12, units = "cm")


