# ============================================================================
# NFL PLAYER PROPS MODEL - WEEK 6 (FULLY INTEGRATED)
# Purpose: Project individual player performance with weather adjustments
# ============================================================================

# Clear environment
rm(list = ls())
gc()

# ============================================================================
# CONFIGURATION
# ============================================================================

SEASON <- 2025
WEEK <- 6
BASE_DIR <- 'C:/Users/Patsc/Documents/nfl'

# Minimum attempts/targets for inclusion
MIN_PASS_ATTEMPTS <- 10
MIN_RUSH_ATTEMPTS <- 5
MIN_TARGETS <- 3

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║        NFL PLAYER PROPS MODEL - WEEK 6                         ║\n")
cat("║        Season:", SEASON, "| Projecting Week:", WEEK, "                        ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# ============================================================================
# LOAD PACKAGES
# ============================================================================

cat("Loading required packages...\n")

required_packages <- c(
  "nflverse", "nflreadr", "nflfastR", "dplyr", "tidyr", 
  "writexl", "ggplot2", "zoo"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

cat("✓ Packages loaded!\n\n")

# ============================================================================
# LOAD TEAM COLORS
# ============================================================================

cat("Loading team colors...\n")

teams_colors <- nflreadr::load_teams() %>%
  select(team_abbr, team_color, team_color2)

rosters <- nflreadr::load_rosters(SEASON) %>%
  select(full_name, team, position, headshot_url, gsis_id) %>%
  filter(!is.na(headshot_url))

cat("✓ Loaded team data\n\n")

# ============================================================================
# LOAD PLAY-BY-PLAY DATA
# ============================================================================

cat("Loading play-by-play data...\n")

pbp_raw <- nflreadr::load_pbp(SEASON)

pbp_clean <- pbp_raw %>%
  filter(
    season_type == "REG",
    !is.na(week),
    week < !!WEEK,
    !is.na(down)
  )

cat("✓ Loaded", nrow(pbp_clean), "plays from weeks 1-", WEEK - 1, "\n\n")

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
# LOAD WEATHER DATA
# ============================================================================

cat("Loading weather data for Week", WEEK, "...\n")

weather_file <- file.path(BASE_DIR, paste0('week', WEEK), 'weather_data.csv')
if (file.exists(weather_file)) {
  weather_data <- read.csv(weather_file) %>%
    filter(week == !!WEEK)
  cat("✓ Weather data loaded for", nrow(weather_data), "games\n")
} else {
  weather_data <- data.frame()
  cat("⚠ No weather data found - projections without weather adjustments\n")
}

cat("\n")

# ============================================================================
# LOAD SCHEDULE
# ============================================================================

cat("Loading schedule for Week", WEEK, "...\n")

schedule <- nflreadr::load_schedules(SEASON) %>%
  filter(week == !!WEEK, !is.na(away_team), !is.na(home_team))

cat("✓ Loaded", nrow(schedule), "games\n\n")

# ============================================================================
# PROJECTION FUNCTIONS
# ============================================================================

# QB Projection Function
project_qb <- function(qb_data, schedule, defense_data, weather_df) {
  
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
  
  # Initialize weather columns
  projections$weather_impact_score <- 0
  projections$is_dome <- FALSE
  has_weather <- FALSE
  
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
    has_weather <- TRUE
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
    filter(!is.na(projected_pass_yards))
  
  if (has_weather) {
    projections <- projections %>%
      select(player_name = passer_player_name, team = posteam, opponent, is_home,
             recent_avg = roll3_pass_yards, season_avg = season_avg_pass_yards,
             opp_yards_allowed = def_pass_yards_per_game, projected_pass_yards,
             weather_impact_score, is_dome)
  } else {
    projections <- projections %>%
      select(player_name = passer_player_name, team = posteam, opponent, is_home,
             recent_avg = roll3_pass_yards, season_avg = season_avg_pass_yards,
             opp_yards_allowed = def_pass_yards_per_game, projected_pass_yards)
  }
  
  return(projections)
}

# RB Projection Function
project_rb <- function(rb_data, schedule, defense_data, weather_df) {
  
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
  has_weather <- FALSE
  
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
    has_weather <- TRUE
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
    filter(!is.na(projected_rush_yards))
  
  if (has_weather) {
    projections <- projections %>%
      select(player_name = rusher_player_name, team = posteam, opponent, is_home,
             recent_avg = roll3_rush_yards, season_avg = season_avg_rush_yards,
             opp_yards_allowed = def_rush_yards_per_game, projected_rush_yards,
             weather_impact_score, is_dome)
  } else {
    projections <- projections %>%
      select(player_name = rusher_player_name, team = posteam, opponent, is_home,
             recent_avg = roll3_rush_yards, season_avg = season_avg_rush_yards,
             opp_yards_allowed = def_rush_yards_per_game, projected_rush_yards)
  }
  
  return(projections)
}

# WR Projection Function
project_wr <- function(wr_data, schedule, weather_df) {
  
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
  has_weather <- FALSE
  
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
    has_weather <- TRUE
  }
  
  projections <- projections %>%
    mutate(
      base_projection = 0.6 * roll3_rec_yards + 0.4 * season_avg_rec_yards,
      home_adjustment = if_else(is_home, 1.02, 1.0),
      weather_adjustment = if_else(weather_impact_score > 0, 1 - (weather_impact_score * 0.025), 1.0),
      projected_rec_yards = base_projection * home_adjustment * weather_adjustment,
      projected_rec_yards = round(projected_rec_yards, 1)
    ) %>%
    filter(!is.na(projected_rec_yards))
  
  if (has_weather) {
    projections <- projections %>%
      select(player_name = receiver_player_name, team = posteam, opponent, is_home,
             recent_avg = roll3_rec_yards, season_avg = season_avg_rec_yards,
             projected_rec_yards, weather_impact_score, is_dome)
  } else {
    projections <- projections %>%
      select(player_name = receiver_player_name, team = posteam, opponent, is_home,
             recent_avg = roll3_rec_yards, season_avg = season_avg_rec_yards,
             projected_rec_yards)
  }
  
  return(projections)
}

# ============================================================================
# GENERATE PROJECTIONS
# ============================================================================

cat("Creating projections for Week", WEEK, "...\n")

qb_projections <- project_qb(qb_rolling, schedule, pass_def, weather_data) %>%
  arrange(desc(projected_pass_yards)) %>%
  left_join(teams_colors, by = c("team" = "team_abbr")) %>%
  left_join(rosters, by = c("player_name" = "full_name"))

rb_projections <- project_rb(rb_rolling, schedule, rush_def, weather_data) %>%
  arrange(desc(projected_rush_yards)) %>%
  left_join(teams_colors, by = c("team" = "team_abbr")) %>%
  left_join(rosters, by = c("player_name" = "full_name"))

wr_projections <- project_wr(wr_rolling, schedule, weather_data) %>%
  arrange(desc(projected_rec_yards)) %>%
  left_join(teams_colors, by = c("team" = "team_abbr")) %>%
  left_join(rosters, by = c("player_name" = "full_name"))

cat("✓ Created projections for:\n")
cat("  ", nrow(qb_projections), "quarterbacks\n")
cat("  ", nrow(rb_projections), "running backs\n")
cat("  ", nrow(wr_projections), "receivers\n\n")

if (nrow(weather_data) > 0) {
  cat("✓ Weather adjustments applied\n\n")
}

# ============================================================================
# EXPORT RESULTS
# ============================================================================

cat("Exporting projections...\n")

props_output_dir <- file.path(BASE_DIR, paste0('week', WEEK), 'player_props')
if (!dir.exists(props_output_dir)) {
  dir.create(props_output_dir, recursive = TRUE)
}

write_xlsx(
  list(
    QB_Projections = qb_projections,
    RB_Projections = rb_projections,
    WR_Projections = wr_projections,
    QB_Historical = qb_rolling %>% arrange(desc(week), desc(pass_yards)),
    RB_Historical = rb_rolling %>% arrange(desc(week), desc(rushing_yards)),
    WR_Historical = wr_rolling %>% arrange(desc(week), desc(receiving_yards))
  ),
  file.path(props_output_dir, paste0("player_projections_week", WEEK, ".xlsx"))
)

write.csv(qb_projections, file.path(props_output_dir, "qb_projections.csv"), row.names = FALSE)
write.csv(rb_projections, file.path(props_output_dir, "rb_projections.csv"), row.names = FALSE)
write.csv(wr_projections, file.path(props_output_dir, "wr_projections.csv"), row.names = FALSE)

cat("✓ Files exported\n\n")

# ============================================================================
# CREATE VISUALIZATIONS
# ============================================================================

cat("Creating visualizations...\n")

# QB Plot
qb_plot <- qb_projections %>%
  head(15) %>%
  mutate(
    player_name = reorder(player_name, projected_pass_yards),
    team_color = if_else(is.na(team_color), "#0066CC", team_color)
  ) %>%
  ggplot(aes(x = player_name, y = projected_pass_yards)) +
  geom_col(aes(fill = team_color), color = "white", linewidth = 0.5) +
  scale_fill_identity() +
  geom_text(aes(label = round(projected_pass_yards)), hjust = -0.3, size = 3.5, fontface = "bold") +
  coord_flip() +
  labs(
    title = paste("Week", WEEK, "- Projected Passing Yards (Top 15 QBs)"),
    subtitle = if(nrow(weather_data) > 0) "Includes weather adjustments | Colored by team" else "Based on recent form + opponent defense | Colored by team",
    x = NULL,
    y = "Projected Passing Yards"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(face = "bold", size = 10),
    panel.grid.major.y = element_blank()
  )

ggsave(file.path(props_output_dir, "qb_projections.png"), qb_plot, width = 11, height = 9, dpi = 300)

# RB Plot
rb_plot <- rb_projections %>%
  head(15) %>%
  mutate(
    player_name = reorder(player_name, projected_rush_yards),
    team_color = if_else(is.na(team_color), "#CC0000", team_color)
  ) %>%
  ggplot(aes(x = player_name, y = projected_rush_yards)) +
  geom_col(aes(fill = team_color), color = "white", linewidth = 0.5) +
  scale_fill_identity() +
  geom_text(aes(label = round(projected_rush_yards)), hjust = -0.3, size = 3.5, fontface = "bold") +
  coord_flip() +
  labs(
    title = paste("Week", WEEK, "- Projected Rushing Yards (Top 15 RBs)"),
    subtitle = if(nrow(weather_data) > 0) "Includes weather adjustments | Colored by team" else "Based on recent form + opponent defense | Colored by team",
    x = NULL,
    y = "Projected Rushing Yards"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(face = "bold", size = 10),
    panel.grid.major.y = element_blank()
  )

ggsave(file.path(props_output_dir, "rb_projections.png"), rb_plot, width = 11, height = 9, dpi = 300)

# WR Plot
wr_plot <- wr_projections %>%
  head(15) %>%
  mutate(
    player_name = reorder(player_name, projected_rec_yards),
    team_color = if_else(is.na(team_color), "#00AA00", team_color)
  ) %>%
  ggplot(aes(x = player_name, y = projected_rec_yards)) +
  geom_col(aes(fill = team_color), color = "white", linewidth = 0.5) +
  scale_fill_identity() +
  geom_text(aes(label = round(projected_rec_yards)), hjust = -0.3, size = 3.5, fontface = "bold") +
  coord_flip() +
  labs(
    title = paste("Week", WEEK, "- Projected Receiving Yards (Top 15 WRs)"),
    subtitle = if(nrow(weather_data) > 0) "Includes weather adjustments | Colored by team" else "Based on recent form | Colored by team",
    x = NULL,
    y = "Projected Receiving Yards"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(face = "bold", size = 10),
    panel.grid.major.y = element_blank()
  )

ggsave(file.path(props_output_dir, "wr_projections.png"), wr_plot, width = 11, height = 9, dpi = 300)

cat("✓ Visualizations saved\n