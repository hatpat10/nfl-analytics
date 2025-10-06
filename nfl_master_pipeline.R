# ============================================================================
# NFL BETTING MODEL - MASTER PIPELINE WITH TEAM COLORS
# ============================================================================

# Clear environment
rm(list = ls())
gc()

# ============================================================================
# CONFIGURATION
# ============================================================================

SEASON <- 2025
WEEK <- 5
BASE_DIR <- 'C:/Users/Patsc/Documents/nfl'

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║        NFL BETTING MODEL - MASTER PIPELINE                     ║\n")
cat("║        Season:", SEASON, "| Week:", WEEK, "                                  ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# ============================================================================
# LOAD PACKAGES
# ============================================================================

cat("Loading required packages...\n")

required_packages <- c(
  "nflverse", "nflreadr", "nflfastR", "nflplotR", "nflseedR", "nfl4th",
  "dplyr", "tidyr", "writexl", "openxlsx", "ggplot2", "gt", "gtExtras",
  "gridExtra", "zoo", "ggrepel"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("  Installing", pkg, "...\n")
    install.packages(pkg)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

cat("✓ All packages loaded successfully!\n\n")

# ============================================================================
# LOAD TEAM VISUAL DATA
# ============================================================================

cat("Loading team visual data...\n")
teams_colors_logos <- nflreadr::load_teams() %>%
  select(team_abbr, team_name, team_color, team_color2, team_logo_espn, team_wordmark)
cat("✓ Team visual data loaded for", nrow(teams_colors_logos), "teams\n\n")

# ============================================================================
# PHASE 1: DATA CLEANING & PREPARATION
# ============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PHASE 1: CLEANING & PREPARING DATA                           ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

start_time_phase1 <- Sys.time()

# Create output directory
output_dir <- file.path(BASE_DIR, paste0('week', WEEK))
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

# Load raw play-by-play data
cat("[1/7] Loading play-by-play data...\n")
pbp_raw <- nflreadr::load_pbp(SEASON)
cat("      Loaded:", nrow(pbp_raw), "plays\n")

# Add 4th down decision analysis
cat("[2/7] Adding 4th down analytics...\n")
pbp_raw <- pbp_raw %>% nfl4th::add_4th_probs()

# Filter to relevant plays
cat("[3/7] Filtering to relevant plays...\n")
pbp_clean <- pbp_raw %>%
  filter(
    season_type == "REG",
    week <= !!WEEK,
    play_type %in% c("pass", "run", "field_goal", "punt", "qb_kneel", "qb_spike"),
    !(qtr %in% c(2, 4) & game_seconds_remaining <= 120 & score_differential_post > 8),
    is.na(penalty_team) | penalty_type %in% c("Defensive Offside", "Neutral Zone Infraction", "Defensive Delay of Game")
  )
cat("      Remaining:", nrow(pbp_clean), "plays\n")

# Create clean variables
cat("[4/7] Creating clean variables...\n")
pbp_analysis <- pbp_clean %>%
  mutate(
    game_id = as.character(game_id),
    season = as.numeric(season),
    week = as.numeric(week),
    game_date = as.Date(game_date),
    offense_team = posteam,
    defense_team = defteam,
    play_type_clean = case_when(
      play_type == "pass" ~ "pass",
      play_type == "run" & qb_scramble == 1 ~ "qb_scramble",
      play_type == "run" ~ "run",
      play_type == "field_goal" ~ "field_goal",
      play_type == "punt" ~ "punt",
      TRUE ~ "other"
    ),
    down = as.integer(down),
    ydstogo = as.integer(ydstogo),
    yards_gained = as.numeric(yards_gained),
    yardline_100 = as.numeric(yardline_100),
    red_zone = if_else(yardline_100 <= 20 & yardline_100 > 0, 1, 0),
    success = as.numeric(success),
    epa = as.numeric(epa),
    wpa = as.numeric(wpa),
    explosive_pass = if_else(play_type == "pass" & yards_gained >= 20, 1, 0),
    explosive_run = if_else(play_type == "run" & yards_gained >= 10, 1, 0),
    explosive_play = if_else(explosive_pass == 1 | explosive_run == 1, 1, 0),
    stuffed = if_else(yards_gained <= 0, 1, 0),
    sack = as.numeric(sack),
    interception = as.numeric(interception),
    fumble_lost = as.numeric(fumble_lost),
    turnover = if_else(interception == 1 | fumble_lost == 1, 1, 0),
    touchdown = as.numeric(touchdown),
    pass_touchdown = as.numeric(pass_touchdown),
    rush_touchdown = as.numeric(rush_touchdown),
    score_differential = as.numeric(score_differential),
    quarter = as.integer(qtr)
  ) %>%
  mutate(across(c(success, explosive_pass, explosive_run, explosive_play, stuffed, turnover, touchdown), 
                ~replace_na(., 0)))

# Aggregate team stats
cat("[5/7] Aggregating team statistics...\n")

team_offense_weekly <- pbp_analysis %>%
  filter(play_type_clean %in% c("pass", "run", "qb_scramble")) %>%
  group_by(season, week, offense_team) %>%
  summarise(
    games_played = n_distinct(game_id),
    total_plays = n(),
    pass_plays = sum(play_type_clean == "pass"),
    run_plays = sum(play_type_clean %in% c("run", "qb_scramble")),
    pass_rate = pass_plays / total_plays,
    avg_yards_per_play = mean(yards_gained, na.rm = TRUE),
    avg_yards_per_pass = mean(if_else(play_type_clean == "pass", yards_gained, NA_real_), na.rm = TRUE),
    avg_yards_per_run = mean(if_else(play_type_clean %in% c("run", "qb_scramble"), yards_gained, NA_real_), na.rm = TRUE),
    avg_epa_per_play = mean(epa, na.rm = TRUE),
    avg_epa_pass = mean(if_else(play_type_clean == "pass", epa, NA_real_), na.rm = TRUE),
    avg_epa_run = mean(if_else(play_type_clean %in% c("run", "qb_scramble"), epa, NA_real_), na.rm = TRUE),
    success_rate = mean(success, na.rm = TRUE),
    explosive_play_rate = mean(explosive_play, na.rm = TRUE),
    stuff_rate = mean(stuffed, na.rm = TRUE),
    sack_rate = sum(sack) / pass_plays,
    turnovers_lost = sum(turnover, na.rm = TRUE),
    turnover_rate = turnovers_lost / total_plays,
    touchdowns = sum(touchdown, na.rm = TRUE),
    .groups = "drop"
  )

team_defense_weekly <- pbp_analysis %>%
  filter(play_type_clean %in% c("pass", "run", "qb_scramble")) %>%
  group_by(season, week, defense_team) %>%
  summarise(
    def_total_plays = n(),
    def_avg_yards_per_play = mean(yards_gained, na.rm = TRUE),
    def_avg_epa_per_play = mean(epa, na.rm = TRUE),
    def_success_rate_allowed = mean(success, na.rm = TRUE),
    def_sacks = sum(sack, na.rm = TRUE),
    def_stuff_rate = mean(stuffed, na.rm = TRUE),
    def_turnovers_forced = sum(turnover, na.rm = TRUE),
    def_turnover_rate = def_turnovers_forced / def_total_plays,
    def_explosive_rate_allowed = mean(explosive_play, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate rolling averages
cat("[6/7] Calculating rolling averages...\n")

offense_rolling <- team_offense_weekly %>%
  arrange(offense_team, week) %>%
  group_by(offense_team) %>%
  mutate(
    roll3_epa = zoo::rollmean(avg_epa_per_play, k = 3, fill = NA, align = "right"),
    roll3_success_rate = zoo::rollmean(success_rate, k = 3, fill = NA, align = "right"),
    roll3_explosive_rate = zoo::rollmean(explosive_play_rate, k = 3, fill = NA, align = "right")
  ) %>%
  ungroup()

defense_rolling <- team_defense_weekly %>%
  arrange(defense_team, week) %>%
  group_by(defense_team) %>%
  mutate(
    def_roll3_epa = zoo::rollmean(def_avg_epa_per_play, k = 3, fill = NA, align = "right"),
    def_roll3_success_rate = zoo::rollmean(def_success_rate_allowed, k = 3, fill = NA, align = "right")
  ) %>%
  ungroup()

# Create rankings
available_weeks <- unique(offense_rolling$week)
analysis_week <- max(available_weeks)

cat("      Creating rankings for week", analysis_week, "...\n")

offense_rankings <- offense_rolling %>%
  filter(week == analysis_week) %>%
  mutate(
    composite_epa = (0.6 * avg_epa_pass) + (0.4 * avg_epa_run),
    epa_rank = rank(-composite_epa),
    pass_epa_rank = rank(-avg_epa_pass),
    run_epa_rank = rank(-avg_epa_run),
    success_rank = rank(-success_rate)
  )

defense_rankings <- defense_rolling %>%
  filter(week == analysis_week) %>%
  mutate(
    composite_epa_allowed = def_avg_epa_per_play,
    def_epa_rank = rank(composite_epa_allowed),
    def_success_rank = rank(def_success_rate_allowed),
    def_turnover_rank = rank(-def_turnover_rate)
  )

# Game summaries
game_summaries <- pbp_analysis %>%
  group_by(game_id, season, week, game_date, home_team, away_team) %>%
  summarise(
    home_score = max(if_else(posteam == home_team, posteam_score, defteam_score), na.rm = TRUE),
    away_score = max(if_else(posteam == away_team, posteam_score, defteam_score), na.rm = TRUE),
    total_points = home_score + away_score,
    .groups = "drop"
  )

# Export Phase 1 data
cat("[7/7] Exporting cleaned data...\n")

write.csv(offense_rolling, file.path(output_dir, 'offense_weekly.csv'), row.names = FALSE)
write.csv(defense_rolling, file.path(output_dir, 'defense_weekly.csv'), row.names = FALSE)
write.csv(offense_rankings, file.path(output_dir, 'offense_rankings.csv'), row.names = FALSE)
write.csv(defense_rankings, file.path(output_dir, 'defense_rankings.csv'), row.names = FALSE)

end_time_phase1 <- Sys.time()
cat("\n✓ Phase 1 complete in", round(difftime(end_time_phase1, start_time_phase1, units = "secs"), 1), "seconds\n")
cat("  Data through Week", analysis_week, "\n")
cat("  Teams:", n_distinct(offense_rankings$offense_team), "\n\n")

# ============================================================================
# DEFINE VISUALIZATION FUNCTIONS
# ============================================================================

create_epa_rankings_plot <- function(offense_ranks, defense_ranks, teams_data) {
  combined_ranks <- offense_ranks %>%
    inner_join(defense_ranks, 
               by = c("season", "week", "offense_team" = "defense_team"),
               suffix = c("_off", "_def")) %>%
    rename(team = offense_team) %>%
    mutate(
      overall_score = ((33 - epa_rank) + (33 - def_epa_rank)) / 2,
      overall_rank = rank(-overall_score)
    ) %>%
    arrange(overall_rank) %>%
    head(16) %>%
    left_join(teams_data, by = c("team" = "team_abbr"))
  
  ggplot(combined_ranks, aes(x = reorder(team, -overall_rank), y = overall_score)) +
    geom_col(aes(fill = team_color), color = "white", linewidth = 0.8) +
    scale_fill_identity() +
    geom_text(aes(label = paste0("#", as.integer(overall_rank))), 
              vjust = -0.5, fontface = "bold", size = 4) +
    labs(
      title = "NFL Power Rankings",
      subtitle = paste("Combined Offensive & Defensive EPA - Week", max(combined_ranks$week)),
      x = NULL, y = "Overall Power Score"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      axis.text.x = element_text(face = "bold", size = 11),
      panel.grid.major.x = element_blank()
    )
}

create_offense_defense_scatter <- function(offense_ranks, defense_ranks, teams_data) {
  combined <- offense_ranks %>%
    inner_join(defense_ranks, 
               by = c("season", "week", "offense_team" = "defense_team"),
               suffix = c("_off", "_def")) %>%
    rename(team = offense_team) %>%
    left_join(teams_data, by = c("team" = "team_abbr"))
  
  ggplot(combined, aes(x = composite_epa, y = -composite_epa_allowed)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_point(aes(fill = team_color), size = 5, shape = 21, color = "white", stroke = 1.5) +
    scale_fill_identity() +
    ggrepel::geom_text_repel(
      aes(label = team), size = 3.5, fontface = "bold",
      box.padding = 0.5, point.padding = 0.3
    ) +
    labs(
      title = "NFL Team Efficiency Matrix",
      subtitle = paste("Offensive EPA vs Defensive EPA - Week", max(combined$week)),
      x = "Offensive EPA per Play (Better →)",
      y = "Defensive EPA per Play (Better →)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5)
    )
}

# ============================================================================
# PHASE 2: MATCHUP ANALYSIS & VISUALIZATIONS
# ============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PHASE 2: MATCHUP ANALYSIS & VISUALIZATIONS                   ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

start_time_phase2 <- Sys.time()

# Load schedule
schedule <- nflreadr::load_schedules(SEASON) %>%
  filter(week == !!WEEK, !is.na(away_team), !is.na(home_team))

cat("Analyzing", nrow(schedule), "games for Week", WEEK, "\n\n")

# Create matchup analysis directory
matchup_dir <- file.path(output_dir, 'matchup_analysis')
if (!dir.exists(matchup_dir)) {
  dir.create(matchup_dir, recursive = TRUE)
}

# Calculate matchup advantages
cat("[1/3] Calculating matchup advantages...\n")

calculate_matchup_advantage <- function(team_a, team_b, offense_data, defense_data) {
  team_a_offense <- offense_data %>% filter(offense_team == team_a) %>% arrange(desc(week)) %>% slice(1)
  team_b_defense <- defense_data %>% filter(defense_team == team_b) %>% arrange(desc(week)) %>% slice(1)
  
  if (nrow(team_a_offense) == 0 || nrow(team_b_defense) == 0) {
    return(NULL)
  }
  
  tibble(
    offense_team = team_a,
    defense_team = team_b,
    epa_advantage = team_a_offense$avg_epa_per_play - team_b_defense$def_avg_epa_per_play
  )
}

matchup_summaries <- list()

for (i in 1:nrow(schedule)) {
  game <- schedule[i, ]
  home <- game$home_team
  away <- game$away_team
  
  home_adv <- calculate_matchup_advantage(home, away, offense_rolling, defense_rolling)
  away_adv <- calculate_matchup_advantage(away, home, offense_rolling, defense_rolling)
  
  if (!is.null(home_adv) && !is.null(away_adv)) {
    summary <- tibble(
      game = paste(away, "at", home),
      home_team = home,
      away_team = away,
      home_epa_advantage = home_adv$epa_advantage,
      away_epa_advantage = away_adv$epa_advantage,
      net_home_advantage = home_adv$epa_advantage - away_adv$epa_advantage,
      projected_margin = (home_adv$epa_advantage - away_adv$epa_advantage) * 15 + 2.5
    )
    matchup_summaries[[i]] <- summary
  }
}

matchup_summary_df <- bind_rows(matchup_summaries)

# Create visualizations with team colors
cat("[2/3] Creating visualizations with team colors...\n")

# Power rankings
power_plot <- create_epa_rankings_plot(offense_rankings, defense_rankings, teams_colors_logos)
ggsave(file.path(matchup_dir, "power_rankings_colored.png"), 
       power_plot, width = 14, height = 8, dpi = 300)
cat("      ✓ Power rankings plot saved\n")

# Efficiency matrix
efficiency_plot <- create_offense_defense_scatter(offense_rankings, defense_rankings, teams_colors_logos)
ggsave(file.path(matchup_dir, "efficiency_matrix.png"), 
       efficiency_plot, width = 12, height = 10, dpi = 300)
cat("      ✓ Efficiency matrix saved\n")

# Week overview
overview_plot <- matchup_summary_df %>%
  arrange(desc(abs(net_home_advantage))) %>%
  head(10) %>%
  mutate(game = reorder(game, net_home_advantage)) %>%
  ggplot(aes(x = game, y = net_home_advantage)) +
  geom_col(fill = "#0066CC") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(
    title = paste("Week", WEEK, "- Net Matchup Advantages"),
    x = NULL, y = "Net EPA Advantage"
  ) +
  theme_minimal()

ggsave(file.path(matchup_dir, "week_overview.png"), overview_plot, width = 12, height = 10, dpi = 300)
cat("      ✓ Week overview saved\n")

# Export data
cat("[3/3] Exporting matchup analysis...\n")

write.csv(matchup_summary_df, file.path(matchup_dir, "matchup_summary.csv"), row.names = FALSE)

betting_recs <- matchup_summary_df %>%
  mutate(
    spread_lean = case_when(
      net_home_advantage > 0.15 ~ paste("HOME", home_team, "has significant edge"),
      net_home_advantage < -0.15 ~ paste("AWAY", away_team, "has significant edge"),
      TRUE ~ "Even matchup"
    ),
    confidence = case_when(
      abs(net_home_advantage) > 0.15 ~ "HIGH",
      abs(net_home_advantage) > 0.05 ~ "MEDIUM",
      TRUE ~ "LOW"
    )
  ) %>%
  arrange(desc(abs(net_home_advantage)))

write.csv(betting_recs, file.path(matchup_dir, "betting_recommendations.csv"), row.names = FALSE)

end_time_phase2 <- Sys.time()
cat("\n✓ Phase 2 complete in", round(difftime(end_time_phase2, start_time_phase2, units = "secs"), 1), "seconds\n\n")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

total_time <- difftime(Sys.time(), start_time_phase1, units = "secs")

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PIPELINE COMPLETE!                                            ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("Total execution time:", round(total_time, 1), "seconds\n\n")
cat("📁 Output Files:\n")
cat("   Main:", output_dir, "\n")
cat("   Matchups:", matchup_dir, "\n")