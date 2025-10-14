# ============================================================================
# NFL PLAYER PROPS MODEL - COMBINED (Weather + Headshots)
# Purpose: Project individual player performance with visual headshots AND weather
# Save as: nfl_props_headshots.R in C:/Users/Patsc/Documents/nfl/
# ============================================================================

# Clear environment
rm(list = ls())
gc()

# ============================================================================
# CONFIGURATION
# ============================================================================

SEASON <- 2025
WEEK <- 7
BASE_DIR <- 'C:/Users/Patsc/Documents/nfl'

# Minimum attempts/targets for inclusion
MIN_PASS_ATTEMPTS <- 10
MIN_RUSH_ATTEMPTS <- 5
MIN_TARGETS <- 3

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║     NFL PLAYER PROPS - WEATHER + HEADSHOTS COMBINED           ║\n")
cat("║        Season:", SEASON, "| Projecting Week:", WEEK, "                        ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# ============================================================================
# LOAD PACKAGES
# ============================================================================

cat("Loading required packages...\n")

required_packages <- c(
  "nflverse", "nflreadr", "nflfastR", "nflplotR", "dplyr", "tidyr", 
  "writexl", "ggplot2", "zoo", "ggimage", "scales"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

cat("✓ Packages loaded!\n\n")

# ============================================================================
# LOAD PLAYER DATA WITH HEADSHOTS
# ============================================================================

cat("Loading player data with headshots...\n")

players_db <- nflreadr::load_players() %>%
  select(
    gsis_id,
    display_name,
    short_name,
    position,
    position_group,
    headshot,
    team = latest_team,
    status,
    jersey_number,
    height,
    weight,
    college_name,
    years_of_experience
  ) %>%
  filter(!is.na(headshot), status == "ACT")

cat("✓ Loaded", nrow(players_db), "active players with headshots\n\n")

teams_colors <- nflreadr::load_teams() %>%
  select(team_abbr, team_name, team_color, team_color2, team_logo_espn)

# ============================================================================
# LOAD PLAY-BY-PLAY DATA
# ============================================================================

cat("Loading play-by-play data...\n")

pbp_raw <- nflreadr::load_pbp(SEASON)

pbp_clean <- pbp_raw %>%
  filter(
    season_type == "REG",
    week < !!WEEK,
    !is.na(down)
  )

cat("✓ Loaded", nrow(pbp_clean), "plays from weeks 1-", WEEK - 1, "\n")
cat("  (Using this data to project Week", WEEK, ")\n\n")

# ============================================================================
# AGGREGATE QUARTERBACK STATS
# ============================================================================

cat("Processing quarterback statistics...\n")

qb_weekly <- pbp_clean %>%
  filter(play_type %in% c("pass", "run"), !is.na(passer_player_id)) %>%
  group_by(season, week, passer_player_id, passer_player_name, posteam) %>%
  summarise(
    games = n_distinct(game_id),
    pass_attempts = sum(pass_attempt, na.rm = TRUE),
    completions = sum(complete_pass, na.rm = TRUE),
    pass_yards = sum(passing_yards, na.rm = TRUE),
    pass_tds = sum(pass_touchdown, na.rm = TRUE),
    interceptions = sum(interception, na.rm = TRUE),
    sacks = sum(sack, na.rm = TRUE),
    rushing_yards = sum(if_else(rusher_player_id == passer_player_id, rushing_yards, 0), na.rm = TRUE),
    rush_attempts = sum(if_else(rusher_player_id == passer_player_id, 1, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(pass_attempts >= MIN_PASS_ATTEMPTS)

qb_rolling <- qb_weekly %>%
  arrange(passer_player_id, week) %>%
  group_by(passer_player_id) %>%
  mutate(
    roll3_pass_yards = zoo::rollmean(pass_yards, k = 3, fill = NA, align = "right"),
    roll3_pass_attempts = zoo::rollmean(pass_attempts, k = 3, fill = NA, align = "right"),
    roll3_completions = zoo::rollmean(completions, k = 3, fill = NA, align = "right"),
    roll3_pass_tds = zoo::rollmean(pass_tds, k = 3, fill = NA, align = "right"),
    season_avg_pass_yards = mean(pass_yards, na.rm = TRUE),
    season_avg_attempts = mean(pass_attempts, na.rm = TRUE)
  ) %>%
  ungroup()

cat("✓ Processed", n_distinct(qb_rolling$passer_player_id), "quarterbacks\n")

# ============================================================================
# AGGREGATE RUNNING BACK STATS
# ============================================================================

cat("Processing running back statistics...\n")

rb_weekly <- pbp_clean %>%
  filter(play_type == "run", !is.na(rusher_player_id)) %>%
  group_by(season, week, rusher_player_id, rusher_player_name, posteam) %>%
  summarise(
    games = n_distinct(game_id),
    rush_attempts = n(),
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    rush_tds = sum(rush_touchdown, na.rm = TRUE),
    fumbles = sum(fumble_lost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(rush_attempts >= MIN_RUSH_ATTEMPTS)

rb_receiving <- pbp_clean %>%
  filter(play_type == "pass", !is.na(receiver_player_id)) %>%
  group_by(season, week, receiver_player_id, receiver_player_name, posteam) %>%
  summarise(
    targets = n(),
    receptions = sum(complete_pass, na.rm = TRUE),
    receiving_yards = sum(receiving_yards, na.rm = TRUE),
    rec_tds = sum(pass_touchdown, na.rm = TRUE),
    .groups = "drop"
  )

rb_combined <- rb_weekly %>%
  left_join(
    rb_receiving,
    by = c("season", "week", "rusher_player_id" = "receiver_player_id", 
           "rusher_player_name" = "receiver_player_name", "posteam")
  ) %>%
  mutate(
    targets = replace_na(targets, 0),
    receptions = replace_na(receptions, 0),
    receiving_yards = replace_na(receiving_yards, 0),
    rec_tds = replace_na(rec_tds, 0)
  )

rb_rolling <- rb_combined %>%
  arrange(rusher_player_id, week) %>%
  group_by(rusher_player_id) %>%
  mutate(
    roll3_rush_yards = zoo::rollmean(rushing_yards, k = 3, fill = NA, align = "right"),
    roll3_rush_attempts = zoo::rollmean(rush_attempts, k = 3, fill = NA, align = "right"),
    roll3_rec_yards = zoo::rollmean(receiving_yards, k = 3, fill = NA, align = "right"),
    season_avg_rush_yards = mean(rushing_yards, na.rm = TRUE),
    season_avg_rush_attempts = mean(rush_attempts, na.rm = TRUE),
    season_avg_rec_yards = mean(receiving_yards, na.rm = TRUE)
  ) %>%
  ungroup()

cat("✓ Processed", n_distinct(rb_rolling$rusher_player_id), "running backs\n")

# ============================================================================
# AGGREGATE RECEIVER STATS
# ============================================================================

cat("Processing receiver statistics...\n")

wr_weekly <- pbp_clean %>%
  filter(play_type == "pass", !is.na(receiver_player_id)) %>%
  group_by(season, week, receiver_player_id, receiver_player_name, posteam) %>%
  summarise(
    games = n_distinct(game_id),
    targets = n(),
    receptions = sum(complete_pass, na.rm = TRUE),
    receiving_yards = sum(receiving_yards, na.rm = TRUE),
    rec_tds = sum(pass_touchdown, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(targets >= MIN_TARGETS)

wr_rolling <- wr_weekly %>%
  arrange(receiver_player_id, week) %>%
  group_by(receiver_player_id) %>%
  mutate(
    roll3_rec_yards = zoo::rollmean(receiving_yards, k = 3, fill = NA, align = "right"),
    roll3_targets = zoo::rollmean(targets, k = 3, fill = NA, align = "right"),
    roll3_receptions = zoo::rollmean(receptions, k = 3, fill = NA, align = "right"),
    season_avg_rec_yards = mean(receiving_yards, na.rm = TRUE),
    season_avg_targets = mean(targets, na.rm = TRUE)
  ) %>%
  ungroup()

cat("✓ Processed", n_distinct(wr_rolling$receiver_player_id), "receivers\n\n")

# ============================================================================
# CALCULATE OPPONENT DEFENSIVE STATS
# ============================================================================

cat("Calculating defensive matchup data...\n")

pass_def <- pbp_clean %>%
  filter(play_type == "pass", !is.na(defteam)) %>%
  group_by(defteam) %>%
  summarise(
    total_games = n_distinct(game_id),
    total_pass_yards = sum(passing_yards, na.rm = TRUE),
    total_attempts = sum(pass_attempt, na.rm = TRUE),
    def_pass_yards_per_game = total_pass_yards / total_games,
    def_pass_yards_per_attempt = total_pass_yards / total_attempts,
    .groups = "drop"
  )

rush_def <- pbp_clean %>%
  filter(play_type == "run", !is.na(defteam)) %>%
  group_by(defteam) %>%
  summarise(
    total_games = n_distinct(game_id),
    total_rush_yards = sum(rushing_yards, na.rm = TRUE),
    total_carries = n(),
    def_rush_yards_per_game = total_rush_yards / total_games,
    def_rush_yards_per_carry = total_rush_yards / total_carries,
    .groups = "drop"
  )

cat("✓ Defensive stats calculated\n\n")

# ============================================================================
# LOAD WEATHER DATA (OPTIONAL)
# ============================================================================

cat("Loading weather data for Week", WEEK, "...\n")

weather_file <- file.path(BASE_DIR, paste0('week', WEEK), 'weather_data.csv')
if (file.exists(weather_file)) {
  weather_data <- read.csv(weather_file) %>%
    filter(week == !!WEEK)
  cat("✓ Weather data loaded for", nrow(weather_data), "games\n\n")
  HAS_WEATHER <- TRUE
} else {
  weather_data <- data.frame()
  cat("ℹ️  No weather data found - projections without weather adjustments\n\n")
  HAS_WEATHER <- FALSE
}

# ============================================================================
# CREATE PROJECTIONS WITH HEADSHOTS AND WEATHER
# ============================================================================

cat("Creating projections for Week", WEEK, "...\n")

schedule <- nflreadr::load_schedules(SEASON) %>%
  filter(week == !!WEEK, !is.na(away_team), !is.na(home_team))

if (nrow(schedule) == 0) {
  stop("❌ No games scheduled for Week ", WEEK, " in ", SEASON, " season!")
}

cat("✓ Found", nrow(schedule), "games scheduled for Week", WEEK, "\n\n")

# QB Projections
project_qb_full <- function(qb_data, schedule, defense_data, players_data, teams_data, weather_df) {
  latest_stats <- qb_data %>%
    group_by(passer_player_id) %>%
    arrange(desc(week)) %>%
    slice(1) %>%
    ungroup()
  
  away_games <- latest_stats %>%
    inner_join(schedule %>% select(away_team, home_team), 
               by = c("posteam" = "away_team")) %>%
    mutate(opponent = home_team, is_home = FALSE)
  
  home_games <- latest_stats %>%
    inner_join(schedule %>% select(away_team, home_team), 
               by = c("posteam" = "home_team")) %>%
    mutate(opponent = away_team, is_home = TRUE)
  
  projections <- bind_rows(away_games, home_games) %>%
    left_join(defense_data, by = c("opponent" = "defteam"))
  
  projections$weather_impact_score <- 0
  projections$is_dome <- FALSE
  
  if (nrow(weather_df) > 0) {
    weather_lookup <- weather_df %>%
      select(home_team, away_team, weather_impact_score, is_dome)
    
    for (i in 1:nrow(projections)) {
      team <- projections$posteam[i]
      opp <- projections$opponent[i]
      is_home_team <- projections$is_home[i]
      
      if (is_home_team) {
        weather_match <- weather_lookup %>%
          filter(home_team == team, away_team == opp)
      } else {
        weather_match <- weather_lookup %>%
          filter(away_team == team, home_team == opp)
      }
      
      if (nrow(weather_match) > 0) {
        projections$weather_impact_score[i] <- weather_match$weather_impact_score[1]
        projections$is_dome[i] <- weather_match$is_dome[1]
      }
    }
  }
  
  projections <- projections %>%
    mutate(
      base_projection = 0.6 * roll3_pass_yards + 0.4 * season_avg_pass_yards,
      opponent_factor = def_pass_yards_per_game / 250,
      home_adjustment = if_else(is_home, 1.05, 1.0),
      weather_adjustment = if_else(weather_impact_score > 0, 1 - (weather_impact_score * 0.03), 1.0),
      projected_pass_yards = base_projection * opponent_factor * home_adjustment * weather_adjustment,
      projected_pass_yards = round(projected_pass_yards, 1)
    ) %>%
    filter(!is.na(projected_pass_yards)) %>%
    left_join(players_data, by = c("passer_player_id" = "gsis_id")) %>%
    left_join(teams_data, by = c("posteam" = "team_abbr")) %>%
    select(
      player_id = passer_player_id,
      player_name = passer_player_name,
      team = posteam,
      opponent,
      is_home,
      recent_avg = roll3_pass_yards,
      season_avg = season_avg_pass_yards,
      opp_yards_allowed = def_pass_yards_per_game,
      projected_pass_yards,
      weather_impact_score,
      is_dome,
      headshot,
      team_color,
      team_color2,
      team_logo = team_logo_espn,
      jersey = jersey_number,
      position,
      experience = years_of_experience
    )
  
  return(projections)
}

# RB Projections
project_rb_full <- function(rb_data, schedule, defense_data, players_data, teams_data, weather_df) {
  latest_stats <- rb_data %>%
    group_by(rusher_player_id) %>%
    arrange(desc(week)) %>%
    slice(1) %>%
    ungroup()
  
  away_games <- latest_stats %>%
    inner_join(schedule %>% select(away_team, home_team), 
               by = c("posteam" = "away_team")) %>%
    mutate(opponent = home_team, is_home = FALSE)
  
  home_games <- latest_stats %>%
    inner_join(schedule %>% select(away_team, home_team), 
               by = c("posteam" = "home_team")) %>%
    mutate(opponent = away_team, is_home = TRUE)
  
  projections <- bind_rows(away_games, home_games) %>%
    left_join(defense_data, by = c("opponent" = "defteam"))
  
  projections$weather_impact_score <- 0
  projections$is_dome <- FALSE
  
  if (nrow(weather_df) > 0) {
    weather_lookup <- weather_df %>%
      select(home_team, away_team, weather_impact_score, is_dome)
    
    for (i in 1:nrow(projections)) {
      team <- projections$posteam[i]
      opp <- projections$opponent[i]
      is_home_team <- projections$is_home[i]
      
      if (is_home_team) {
        weather_match <- weather_lookup %>%
          filter(home_team == team, away_team == opp)
      } else {
        weather_match <- weather_lookup %>%
          filter(away_team == team, home_team == opp)
      }
      
      if (nrow(weather_match) > 0) {
        projections$weather_impact_score[i] <- weather_match$weather_impact_score[1]
        projections$is_dome[i] <- weather_match$is_dome[1]
      }
    }
  }
  
  projections <- projections %>%
    mutate(
      base_projection = 0.6 * roll3_rush_yards + 0.4 * season_avg_rush_yards,
      opponent_factor = def_rush_yards_per_game / 100,
      home_adjustment = if_else(is_home, 1.03, 1.0),
      weather_adjustment = if_else(weather_impact_score > 0, 1 + (weather_impact_score * 0.02), 1.0),
      projected_rush_yards = base_projection * opponent_factor * home_adjustment * weather_adjustment,
      projected_rush_yards = round(projected_rush_yards, 1)
    ) %>%
    filter(!is.na(projected_rush_yards)) %>%
    left_join(players_data, by = c("rusher_player_id" = "gsis_id")) %>%
    left_join(teams_data, by = c("posteam" = "team_abbr")) %>%
    select(
      player_id = rusher_player_id,
      player_name = rusher_player_name,
      team = posteam,
      opponent,
      is_home,
      recent_avg = roll3_rush_yards,
      season_avg = season_avg_rush_yards,
      opp_yards_allowed = def_rush_yards_per_game,
      projected_rush_yards,
      weather_impact_score,
      is_dome,
      headshot,
      team_color,
      team_color2,
      team_logo = team_logo_espn,
      jersey = jersey_number,
      position,
      experience = years_of_experience
    )
  
  return(projections)
}

# WR Projections
project_wr_full <- function(wr_data, schedule, players_data, teams_data, weather_df) {
  latest_stats <- wr_data %>%
    group_by(receiver_player_id) %>%
    arrange(desc(week)) %>%
    slice(1) %>%
    ungroup()
  
  away_games <- latest_stats %>%
    inner_join(schedule %>% select(away_team, home_team), 
               by = c("posteam" = "away_team")) %>%
    mutate(opponent = home_team, is_home = FALSE)
  
  home_games <- latest_stats %>%
    inner_join(schedule %>% select(away_team, home_team), 
               by = c("posteam" = "home_team")) %>%
    mutate(opponent = away_team, is_home = TRUE)
  
  projections <- bind_rows(away_games, home_games)
  
  projections$weather_impact_score <- 0
  projections$is_dome <- FALSE
  
  if (nrow(weather_df) > 0) {
    weather_lookup <- weather_df %>%
      select(home_team, away_team, weather_impact_score, is_dome)
    
    for (i in 1:nrow(projections)) {
      team <- projections$posteam[i]
      opp <- projections$opponent[i]
      is_home_team <- projections$is_home[i]
      
      if (is_home_team) {
        weather_match <- weather_lookup %>%
          filter(home_team == team, away_team == opp)
      } else {
        weather_match <- weather_lookup %>%
          filter(away_team == team, home_team == opp)
      }
      
      if (nrow(weather_match) > 0) {
        projections$weather_impact_score[i] <- weather_match$weather_impact_score[1]
        projections$is_dome[i] <- weather_match$is_dome[1]
      }
    }
  }
  
  projections <- projections %>%
    mutate(
      base_projection = 0.6 * roll3_rec_yards + 0.4 * season_avg_rec_yards,
      home_adjustment = if_else(is_home, 1.02, 1.0),
      weather_adjustment = if_else(weather_impact_score > 0, 1 - (weather_impact_score * 0.025), 1.0),
      projected_rec_yards = base_projection * home_adjustment * weather_adjustment,
      projected_rec_yards = round(projected_rec_yards, 1)
    ) %>%
    filter(!is.na(projected_rec_yards)) %>%
    left_join(players_data, by = c("receiver_player_id" = "gsis_id")) %>%
    left_join(teams_data, by = c("posteam" = "team_abbr")) %>%
    select(
      player_id = receiver_player_id,
      player_name = receiver_player_name,
      team = posteam,
      opponent,
      is_home,
      recent_avg = roll3_rec_yards,
      season_avg = season_avg_rec_yards,
      projected_rec_yards,
      weather_impact_score,
      is_dome,
      headshot,
      team_color,
      team_color2,
      team_logo = team_logo_espn,
      jersey = jersey_number,
      position,
      experience = years_of_experience
    )
  
  return(projections)
}

# Generate projections
qb_projections <- project_qb_full(qb_rolling, schedule, pass_def, players_db, teams_colors, weather_data) %>%
  arrange(desc(projected_pass_yards))

rb_projections <- project_rb_full(rb_rolling, schedule, rush_def, players_db, teams_colors, weather_data) %>%
  arrange(desc(projected_rush_yards))

wr_projections <- project_wr_full(wr_rolling, schedule, players_db, teams_colors, weather_data) %>%
  arrange(desc(projected_rec_yards))

cat("✓ Created projections with headshots for:\n")
cat("  ", nrow(qb_projections), "quarterbacks\n")
cat("  ", nrow(rb_projections), "running backs\n")
cat("  ", nrow(wr_projections), "receivers\n")

if (HAS_WEATHER) {
  cat("✓ Weather adjustments applied\n")
}
cat("\n")

# ============================================================================
# CREATE ADVANCED VISUALIZATIONS WITH HEADSHOTS
# ============================================================================

cat("Creating advanced visualizations with player headshots...\n")

props_output_dir <- file.path(BASE_DIR, paste0('week', WEEK), 'player_props')
if (!dir.exists(props_output_dir)) {
  dir.create(props_output_dir, recursive = TRUE)
}

subtitle_text <- if(HAS_WEATHER) {
  "Model: 60% Recent + 40% Season | Weather Adjusted | Opponent Defense"
} else {
  "Model: 60% Recent Form + 40% Season Average | Adjusted for Opponent Defense"
}

# QB Chart
qb_plot_headshots <- qb_projections %>%
  head(12) %>%
  mutate(
    player_name = reorder(player_name, projected_pass_yards),
    team_color = if_else(is.na(team_color), "#0066CC", team_color)
  ) %>%
  ggplot(aes(x = player_name, y = projected_pass_yards)) +
  geom_col(aes(fill = team_color), color = "white", linewidth = 0.8) +
  scale_fill_identity() +
  nflplotR::geom_nfl_headshots(
    aes(player_gsis = player_id),
    height = 0.08,
    vjust = 0.5
  ) +
  geom_text(
    aes(label = paste0(round(projected_pass_yards), " yds")),
    hjust = -0.2,
    size = 3.5,
    fontface = "bold"
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  labs(
    title = paste("Week", WEEK, "QB Projections - Top Passing Yards"),
    subtitle = subtitle_text,
    x = NULL,
    y = "Projected Passing Yards",
    caption = "Headshots via NFL | Model by nflverse"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, hjust = 1, color = "gray50"),
    axis.text.y = element_text(face = "bold", size = 11),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 50, 10, 10)
  )

ggsave(
  file.path(props_output_dir, "qb_projections_headshots.png"),
  qb_plot_headshots,
  width = 12,
  height = 10,
  dpi = 300
)

# RB Chart
rb_plot_headshots <- rb_projections %>%
  head(12) %>%
  mutate(
    player_name = reorder(player_name, projected_rush_yards),
    team_color = if_else(is.na(team_color), "#CC0000", team_color)
  ) %>%
  ggplot(aes(x = player_name, y = projected_rush_yards)) +
  geom_col(aes(fill = team_color), color = "white", linewidth = 0.8) +
  scale_fill_identity() +
  nflplotR::geom_nfl_headshots(
    aes(player_gsis = player_id),
    height = 0.08,
    vjust = 0.5
  ) +
  geom_text(
    aes(label = paste0(round(projected_rush_yards), " yds")),
    hjust = -0.2,
    size = 3.5,
    fontface = "bold"
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  labs(
    title = paste("Week", WEEK, "RB Projections - Top Rushing Yards"),
    subtitle = subtitle_text,
    x = NULL,
    y = "Projected Rushing Yards",
    caption = "Headshots via NFL | Model by nflverse"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, hjust = 1, color = "gray50"),
    axis.text.y = element_text(face = "bold", size = 11),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 50, 10, 10)
  )

ggsave(
  file.path(props_output_dir, "rb_projections_headshots.png"),
  rb_plot_headshots,
  width = 12,
  height = 10,
  dpi = 300
)

# WR Chart
wr_plot_headshots <- wr_projections %>%
  head(12) %>%
  mutate(
    player_name = reorder(player_name, projected_rec_yards),
    team_color = if_else(is.na(team_color), "#00AA00", team_color)
  ) %>%
  ggplot(aes(x = player_name, y = projected_rec_yards)) +
  geom_col(aes(fill = team_color), color = "white", linewidth = 0.8) +
  scale_fill_identity() +
  nflplotR::geom_nfl_headshots(
    aes(player_gsis = player_id),
    height = 0.08,
    vjust = 0.5
  ) +
  geom_text(
    aes(label = paste0(round(projected_rec_yards), " yds")),
    hjust = -0.2,
    size = 3.5,
    fontface = "bold"
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  labs(
    title = paste("Week", WEEK, "WR/TE Projections - Top Receiving Yards"),
    subtitle = subtitle_text,
    x = NULL,
    y = "Projected Receiving Yards",
    caption = "Headshots via NFL | Model by nflverse"
  ) +
  theme_minimal(base_size = 13)
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, hjust = 1, color = "gray50"),
    axis.text.y = element_text(face = "bold", size = 11),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 50, 10, 10)
  )

ggsave(
  file.path(props_output_dir, "wr_projections_headshots.png"),
  wr_plot_headshots,
  width = 12,
  height = 10,
  dpi = 300
)

cat("✓ Headshot visualizations saved\n\n")

# ============================================================================
# CREATE PLAYER CARDS VISUAL
# ============================================================================

cat("Creating bonus player cards visualization...\n")

create_player_cards <- function(data, stat_col, title, output_file) {
  top_players <- data %>%
    head(6) %>%
    mutate(
      stat_value = .data[[stat_col]],
      matchup = paste(team, "vs", opponent),
      home_away = if_else(is_home, "HOME", "AWAY")
    )
  
  card_plot <- ggplot(top_players, aes(x = 1, y = 1)) +
    facet_wrap(~player_name, ncol = 3) +
    geom_rect(
      aes(xmin = 0, xmax = 2, ymin = 0, ymax = 2, fill = team_color),
      alpha = 0.3
    ) +
    scale_fill_identity() +
    nflplotR::geom_nfl_headshots(
      aes(player_gsis = player_id, x = 1, y = 1.5),
      height = 0.4
    ) +
    geom_text(
      aes(label = paste0(round(stat_value), " YDS"), x = 1, y = 0.7),
      size = 8,
      fontface = "bold"
    ) +
    geom_text(
      aes(label = matchup, x = 1, y = 0.4),
      size = 4,
      color = "gray30"
    ) +
    geom_text(
      aes(label = home_away, x = 1, y = 0.2),
      size = 3.5,
      fontface = "bold",
      color = "gray50"
    ) +
    coord_cartesian(xlim = c(0, 2), ylim = c(0, 2), clip = "off") +
    labs(
      title = title,
      subtitle = paste("Week", WEEK, "Projections"),
      caption = "Model: 60% Recent + 40% Season | nflverse"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5, margin = margin(10, 0, 5, 0)),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40", margin = margin(0, 0, 15, 0)),
      plot.caption = element_text(size = 10, hjust = 0.5, color = "gray50", margin = margin(15, 0, 5, 0)),
      strip.text = element_text(face = "bold", size = 12, margin = margin(5, 0, 5, 0)),
      plot.margin = margin(15, 15, 15, 15)
    )
  
  ggsave(output_file, card_plot, width = 14, height = 10, dpi = 300)
}

if (nrow(qb_projections) >= 6) {
  create_player_cards(
    qb_projections,
    "projected_pass_yards",
    "Top QB Projections - Player Cards",
    file.path(props_output_dir, "qb_player_cards.png")
  )
}

if (nrow(rb_projections) >= 6) {
  create_player_cards(
    rb_projections,
    "projected_rush_yards",
    "Top RB Projections - Player Cards",
    file.path(props_output_dir, "rb_player_cards.png")
  )
}

if (nrow(wr_projections) >= 6) {
  create_player_cards(
    wr_projections,
    "projected_rec_yards",
    "Top WR Projections - Player Cards",
    file.path(props_output_dir, "wr_player_cards.png")
  )
}

cat("✓ Player cards created\n\n")

# ============================================================================
# EXPORT ENHANCED DATA
# ============================================================================

cat("Exporting enhanced projections...\n")

write_xlsx(
  list(
    QB_Projections = qb_projections,
    RB_Projections = rb_projections,
    WR_Projections = wr_projections
  ),
  file.path(props_output_dir, paste0("player_projections_enhanced_week", WEEK, ".xlsx"))
)

write.csv(qb_projections, 
          file.path(props_output_dir, "qb_projections_enhanced.csv"), 
          row.names = FALSE)

write.csv(rb_projections, 
          file.path(props_output_dir, "rb_projections_enhanced.csv"), 
          row.names = FALSE)

write.csv(wr_projections, 
          file.path(props_output_dir, "wr_projections_enhanced.csv"), 
          row.names = FALSE)

cat("✓ Files exported\n\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  ENHANCED PLAYER PROPS COMPLETE                               ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("📊 TOP PROJECTIONS:\n\n")

cat("QUARTERBACKS:\n")
if (HAS_WEATHER) {
  print(qb_projections %>% head(5) %>% 
          select(player_name, team, opponent, projected_pass_yards, weather_impact_score, is_dome), 
        row.names = FALSE)
} else {
  print(qb_projections %>% head(5) %>% 
          select(player_name, team, opponent, projected_pass_yards, jersey, experience), 
        row.names = FALSE)
}

cat("\nRUNNING BACKS:\n")
if (HAS_WEATHER) {
  print(rb_projections %>% head(5) %>% 
          select(player_name, team, opponent, projected_rush_yards, weather_impact_score, is_dome), 
        row.names = FALSE)
} else {
  print(rb_projections %>% head(5) %>% 
          select(player_name, team, opponent, projected_rush_yards, jersey, experience), 
        row.names = FALSE)
}

cat("\nRECEIVERS:\n")
if (HAS_WEATHER) {
  print(wr_projections %>% head(5) %>% 
          select(player_name, team, opponent, projected_rec_yards, weather_impact_score, is_dome), 
        row.names = FALSE)
} else {
  print(wr_projections %>% head(5) %>% 
          select(player_name, team, opponent, projected_rec_yards, jersey, experience), 
        row.names = FALSE)
}

cat("\n📁 Files saved to:", props_output_dir, "\n")
cat("   ✓ Enhanced projections with headshot URLs\n")
cat("   ✓ Visual charts with player headshots\n")
cat("   ✓ Player cards with team colors\n")

if (HAS_WEATHER) {
  cat("   ✓ Weather adjustments included\n")
}

cat("\n✓ Done! Combined model (Weather + Headshots) complete! 🏈⛈️\n\n")