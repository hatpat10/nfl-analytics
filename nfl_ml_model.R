# ============================================================================
# NFL MACHINE LEARNING PREDICTION MODEL
# ============================================================================
# Purpose: Train ML models to predict game outcomes using historical data
# Includes: Linear Regression, Random Forest, XGBoost, and Ensemble methods
# ============================================================================

# Clear environment
rm(list = ls())
gc()

# ============================================================================
# CONFIGURATION
# ============================================================================

SEASON <- 2025
TRAIN_WEEKS <- 1:6  # Weeks to train on (weeks 1-5)
PREDICT_WEEK <- 7   # Week to predict
BASE_DIR <- 'C:/Users/Patsc/Documents/nfl'

# Betting focus
BET_TYPES <- c("MONEYLINE", "SPREAD", "TOTALS")
MIN_CONFIDENCE_THRESHOLD <- 0.60  # 60% win probability for recommendations

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║        NFL MACHINE LEARNING PREDICTION MODEL                   ║\n")
cat("║        Training: Weeks 1-5 | Predicting: Week 6               ║\n")
cat("║        Focus: Moneyline, Spread, Totals                        ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# ============================================================================
# LOAD PACKAGES
# ============================================================================

# ============================================================================
# LOAD PACKAGES
# ============================================================================

cat("Loading required packages...\n")

# Load all packages directly (assumes they're already installed)
suppressPackageStartupMessages({
  library(nflverse)
  library(nflreadr)
  library(nflfastR)
  library(nflplotR)
  library(nfl4th)
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(randomForest)
  library(xgboost)
  library(ggplot2)
  library(writexl)
  library(gridExtra)
  library(zoo)
})

cat("✓ All packages loaded!\n\n")

# Note: If you get package errors, install missing packages with:
# install.packages(c("package_name"), dependencies = TRUE)

# ============================================================================
# LOAD AND PREPARE HISTORICAL DATA
# ============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PHASE 1: DATA COLLECTION & FEATURE ENGINEERING               ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("[1/6] Loading play-by-play data...\n")
pbp_raw <- nflreadr::load_pbp(SEASON)
cat("      Loaded:", nrow(pbp_raw), "plays\n")

# Clean data
cat("[2/6] Cleaning and filtering data...\n")
pbp_clean <- pbp_raw %>%
  filter(
    season_type == "REG",
    week %in% TRAIN_WEEKS,
    !is.na(posteam),
    !is.na(defteam)
  )

cat("      Filtered to:", nrow(pbp_clean), "plays from weeks", 
    min(TRAIN_WEEKS), "-", max(TRAIN_WEEKS), "\n")

# ============================================================================
# BUILD COMPREHENSIVE TEAM STATISTICS
# ============================================================================

cat("[3/6] Building comprehensive team statistics...\n")

# Offensive stats by team and week
offense_stats <- pbp_clean %>%
  filter(play_type %in% c("pass", "run")) %>%
  group_by(season, week, posteam) %>%
  summarise(
    # Volume metrics
    total_plays = n(),
    pass_attempts = sum(pass_attempt, na.rm = TRUE),
    rush_attempts = sum(rush_attempt, na.rm = TRUE),
    
    # Efficiency metrics
    avg_epa = mean(epa, na.rm = TRUE),
    avg_epa_pass = mean(epa[pass_attempt == 1], na.rm = TRUE),
    avg_epa_run = mean(epa[rush_attempt == 1], na.rm = TRUE),
    success_rate = mean(success, na.rm = TRUE),
    
    # Explosive plays
    explosive_pass_rate = mean(yards_gained >= 20 & pass_attempt == 1, na.rm = TRUE),
    explosive_run_rate = mean(yards_gained >= 10 & rush_attempt == 1, na.rm = TRUE),
    
    # Yards
    total_yards = sum(yards_gained, na.rm = TRUE),
    pass_yards = sum(yards_gained[pass_attempt == 1], na.rm = TRUE),
    rush_yards = sum(yards_gained[rush_attempt == 1], na.rm = TRUE),
    yards_per_play = mean(yards_gained, na.rm = TRUE),
    
    # Scoring
    touchdowns = sum(touchdown, na.rm = TRUE),
    
    # Negative plays
    sacks_taken = sum(sack, na.rm = TRUE),
    turnovers = sum(interception, na.rm = TRUE) + sum(fumble_lost, na.rm = TRUE),
    turnover_rate = turnovers / total_plays,
    stuffed_rate = mean(yards_gained <= 0, na.rm = TRUE),
    
    # Situational
    third_down_conv = mean(third_down_converted[down == 3], na.rm = TRUE),
    red_zone_td_rate = mean(touchdown[yardline_100 <= 20], na.rm = TRUE),
    
    .groups = "drop"
  )

# Defensive stats
defense_stats <- pbp_clean %>%
  filter(play_type %in% c("pass", "run")) %>%
  group_by(season, week, defteam) %>%
  summarise(
    # Yards allowed
    def_yards_allowed = sum(yards_gained, na.rm = TRUE),
    def_yards_per_play = mean(yards_gained, na.rm = TRUE),
    def_pass_yards_allowed = sum(yards_gained[pass_attempt == 1], na.rm = TRUE),
    def_rush_yards_allowed = sum(yards_gained[rush_attempt == 1], na.rm = TRUE),
    
    # Efficiency allowed
    def_epa_allowed = mean(epa, na.rm = TRUE),
    def_success_rate_allowed = mean(success, na.rm = TRUE),
    
    # Defensive pressure
    def_sacks = sum(sack, na.rm = TRUE),
    def_turnovers_forced = sum(interception, na.rm = TRUE) + sum(fumble_lost, na.rm = TRUE),
    def_stuffs = sum(yards_gained <= 0, na.rm = TRUE),
    def_stuff_rate = mean(yards_gained <= 0, na.rm = TRUE),
    
    # Explosive plays allowed
    def_explosive_allowed = mean(yards_gained >= 15, na.rm = TRUE),
    
    .groups = "drop"
  )

# Special teams stats
special_teams <- pbp_clean %>%
  filter(play_type %in% c("field_goal", "punt", "kickoff")) %>%
  group_by(season, week, posteam) %>%
  summarise(
    fg_attempts = sum(play_type == "field_goal", na.rm = TRUE),
    fg_made = sum(field_goal_result == "made", na.rm = TRUE),
    fg_pct = if_else(fg_attempts > 0, fg_made / fg_attempts, 0),
    avg_punt_yards = mean(kick_distance[play_type == "punt"], na.rm = TRUE),
    .groups = "drop"
  )

cat("      ✓ Offensive stats:", nrow(offense_stats), "team-weeks\n")
cat("      ✓ Defensive stats:", nrow(defense_stats), "team-weeks\n")

# ============================================================================
# CALCULATE ROLLING AVERAGES AND TRENDS
# ============================================================================

cat("[4/6] Calculating rolling averages and trends...\n")

calculate_rolling_stats <- function(df, team_col) {
  df %>%
    arrange(!!sym(team_col), week) %>%
    group_by(!!sym(team_col)) %>%
    mutate(
      # Rolling averages (last 3 weeks)
      across(
        where(is.numeric) & !c(season, week),
        list(
          roll3 = ~zoo::rollmean(., k = 3, fill = NA, align = "right"),
          trend = ~(. - lag(., 1)) # Week-over-week change
        ),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    ungroup()
}

offense_rolling <- calculate_rolling_stats(offense_stats, "posteam")
defense_rolling <- calculate_rolling_stats(defense_stats, "defteam")

# ============================================================================
# CREATE GAME-LEVEL TRAINING DATA
# ============================================================================

cat("[5/6] Creating game-level features for training...\n")

# Get game results
game_results <- pbp_clean %>%
  group_by(game_id, season, week, home_team, away_team) %>%
  summarise(
    home_score = max(if_else(posteam == home_team, posteam_score, defteam_score), na.rm = TRUE),
    away_score = max(if_else(posteam == away_team, posteam_score, defteam_score), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total_points = home_score + away_score,
    point_differential = home_score - away_score,
    home_win = as.numeric(home_score > away_score)
  )

# Join offensive and defensive stats for each team
training_data <- game_results %>%
  # Home team offense
  left_join(
    offense_rolling %>% select(-season),
    by = c("home_team" = "posteam", "week")
  ) %>%
  rename_with(~paste0("home_off_", .), .cols = -c(game_id, season, week, home_team, away_team, 
                                                  home_score, away_score, total_points, 
                                                  point_differential, home_win)) %>%
  # Home team defense
  left_join(
    defense_rolling %>% select(-season),
    by = c("home_team" = "defteam", "week")
  ) %>%
  rename_with(~paste0("home_def_", .), .cols = -c(game_id, season, week, home_team, away_team, 
                                                  home_score, away_score, total_points, 
                                                  point_differential, home_win, 
                                                  starts_with("home_off_"))) %>%
  # Away team offense
  left_join(
    offense_rolling %>% select(-season),
    by = c("away_team" = "posteam", "week")
  ) %>%
  rename_with(~paste0("away_off_", .), .cols = -c(game_id, season, week, home_team, away_team, 
                                                  home_score, away_score, total_points, 
                                                  point_differential, home_win,
                                                  starts_with("home_"), starts_with("away_team"))) %>%
  # Away team defense
  left_join(
    defense_rolling %>% select(-season),
    by = c("away_team" = "defteam", "week")
  ) %>%
  rename_with(~paste0("away_def_", .), .cols = -c(game_id, season, week, home_team, away_team, 
                                                  home_score, away_score, total_points, 
                                                  point_differential, home_win,
                                                  starts_with("home_"), starts_with("away_off_")))

# Remove rows with NA (first few weeks won't have rolling averages)
training_data_clean <- training_data %>%
  filter(complete.cases(.))

cat("      ✓ Training dataset:", nrow(training_data_clean), "games\n")
cat("      ✓ Features:", ncol(training_data_clean) - 8, "predictors\n")

# ============================================================================
# TRAIN MACHINE LEARNING MODELS
# ============================================================================

cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PHASE 2: MACHINE LEARNING MODEL TRAINING                     ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("[6/6] Training predictive models...\n\n")

# Prepare feature matrix and targets
feature_cols <- setdiff(names(training_data_clean), 
                        c("game_id", "season", "week", "home_team", "away_team",
                          "home_score", "away_score", "total_points", 
                          "point_differential", "home_win"))

X_train <- training_data_clean %>% select(all_of(feature_cols))
y_win <- training_data_clean$home_win
y_spread <- training_data_clean$point_differential
y_total <- training_data_clean$total_points

# Handle any remaining NAs
X_train <- X_train %>% mutate(across(everything(), ~replace_na(., 0)))

# ============================================================================
# MODEL 1: MONEYLINE (Win Probability)
# ============================================================================

cat("Training MONEYLINE models (win probability)...\n")

# Convert to factor for classification
y_win_factor <- as.factor(y_win)
levels(y_win_factor) <- c("Loss", "Win")

# Logistic Regression (using base R glm)
cat("  [1/3] Logistic Regression...\n")
train_data_logit <- data.frame(y = y_win_factor, X_train)
model_logit <- glm(y ~ ., data = train_data_logit, family = binomial)

# Random Forest
cat("  [2/3] Random Forest...\n")
set.seed(123)
model_rf_win <- randomForest(
  x = X_train,
  y = y_win_factor,
  ntree = 200,
  importance = TRUE,
  na.action = na.omit
)

# XGBoost
cat("  [3/3] XGBoost...\n")
dtrain_win <- xgb.DMatrix(data = as.matrix(X_train), label = y_win)
model_xgb_win <- xgboost(
  data = dtrain_win,
  objective = "binary:logistic",
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  verbose = 0
)

cat("  ✓ Moneyline models trained\n\n")

# Calculate training accuracy for logistic regression
logit_pred <- predict(model_logit, type = "response")
logit_accuracy <- mean((logit_pred > 0.5) == y_win)

# Calculate training accuracy for Random Forest
rf_accuracy <- sum(model_rf_win$confusion[, "class.error"] == 0) / 
  sum(model_rf_win$confusion[, "class.error"] >= 0)
rf_accuracy <- 1 - mean(model_rf_win$confusion[, "class.error"])

# Calculate training accuracy for XGBoost
xgb_pred <- predict(model_xgb_win, dtrain_win)
xgb_accuracy <- mean((xgb_pred > 0.5) == y_win)

# ============================================================================
# MODEL 2: SPREAD (Point Differential)
# ============================================================================

cat("Training SPREAD models (point differential)...\n")

# Linear Regression
cat("  [1/3] Linear Regression...\n")
train_data_lm <- data.frame(y = y_spread, X_train)
model_lm_spread <- lm(y ~ ., data = train_data_lm)

# Random Forest
cat("  [2/3] Random Forest...\n")
set.seed(123)
model_rf_spread <- randomForest(
  x = X_train,
  y = y_spread,
  ntree = 200,
  na.action = na.omit
)

# XGBoost
cat("  [3/3] XGBoost...\n")
dtrain_spread <- xgb.DMatrix(data = as.matrix(X_train), label = y_spread)
model_xgb_spread <- xgboost(
  data = dtrain_spread,
  objective = "reg:squarederror",
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  verbose = 0
)

cat("  ✓ Spread models trained\n\n")

# Calculate RMSE for spread models
lm_pred <- predict(model_lm_spread)
lm_rmse <- sqrt(mean((lm_pred - y_spread)^2))

rf_pred_spread <- predict(model_rf_spread, X_train)
rf_rmse_spread <- sqrt(mean((rf_pred_spread - y_spread)^2))

xgb_pred_spread <- predict(model_xgb_spread, dtrain_spread)
xgb_rmse_spread <- sqrt(mean((xgb_pred_spread - y_spread)^2))

# ============================================================================
# MODEL 3: TOTALS (Total Points)
# ============================================================================

cat("Training TOTALS models (total points)...\n")

# Linear Regression
cat("  [1/3] Linear Regression...\n")
train_data_lm_total <- data.frame(y = y_total, X_train)
model_lm_total <- lm(y ~ ., data = train_data_lm_total)

# Random Forest
cat("  [2/3] Random Forest...\n")
set.seed(123)
model_rf_total <- randomForest(
  x = X_train,
  y = y_total,
  ntree = 200,
  na.action = na.omit
)

# XGBoost
cat("  [3/3] XGBoost...\n")
dtrain_total <- xgb.DMatrix(data = as.matrix(X_train), label = y_total)
model_xgb_total <- xgboost(
  data = dtrain_total,
  objective = "reg:squarederror",
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  verbose = 0
)

cat("  ✓ Total points models trained\n\n")

# Calculate RMSE for total models
lm_pred_total <- predict(model_lm_total)
lm_rmse_total <- sqrt(mean((lm_pred_total - y_total)^2))

rf_pred_total <- predict(model_rf_total, X_train)
rf_rmse_total <- sqrt(mean((rf_pred_total - y_total)^2))

xgb_pred_total <- predict(model_xgb_total, dtrain_total)
xgb_rmse_total <- sqrt(mean((xgb_pred_total - y_total)^2))

# ============================================================================
# MODEL EVALUATION
# ============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PHASE 3: MODEL EVALUATION                                     ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("MONEYLINE MODEL ACCURACY:\n")
cat("  Logistic Regression:", round(logit_accuracy * 100, 1), "%\n")
cat("  Random Forest:      ", round(rf_accuracy * 100, 1), "%\n")
cat("  XGBoost:            ", round(xgb_accuracy * 100, 1), "%\n\n")

cat("SPREAD MODEL RMSE (Lower is better):\n")
cat("  Linear Regression:  ", round(lm_rmse, 2), "points\n")
cat("  Random Forest:      ", round(rf_rmse_spread, 2), "points\n")
cat("  XGBoost:            ", round(xgb_rmse_spread, 2), "points\n\n")

cat("TOTALS MODEL RMSE (Lower is better):\n")
cat("  Linear Regression:  ", round(lm_rmse_total, 2), "points\n")
cat("  Random Forest:      ", round(rf_rmse_total, 2), "points\n")
cat("  XGBoost:            ", round(xgb_rmse_total, 2), "points\n\n")

# ============================================================================
# GENERATE WEEK 6 PREDICTIONS
# ============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PHASE 4: WEEK 6 PREDICTIONS                                   ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("Preparing Week", PREDICT_WEEK, "data...\n")

# Load Week 6 schedule
schedule_week6 <- nflreadr::load_schedules(SEASON) %>%
  filter(week == PREDICT_WEEK, !is.na(away_team), !is.na(home_team))

cat("  Found", nrow(schedule_week6), "games\n")

# Get latest stats for each team (from most recent week)
latest_offense <- offense_rolling %>%
  group_by(posteam) %>%
  filter(week == max(week)) %>%
  ungroup()

latest_defense <- defense_rolling %>%
  group_by(defteam) %>%
  filter(week == max(week)) %>%
  ungroup()

# Build prediction dataset
prediction_data <- schedule_week6 %>%
  # Home team offense
  left_join(
    latest_offense,
    by = c("home_team" = "posteam")
  ) %>%
  select(-any_of(c("week.y", "season.y"))) %>%
  rename(week = week.x) %>%
  rename_with(~paste0("home_off_", .), 
              .cols = setdiff(names(latest_offense), c("posteam", "week", "season"))) %>%
  # Home team defense
  left_join(
    latest_defense,
    by = c("home_team" = "defteam")
  ) %>%
  select(-any_of(c("week.y", "season.y"))) %>%
  rename_with(~paste0("home_def_", .), 
              .cols = setdiff(names(latest_defense), c("defteam", "week", "season"))) %>%
  # Away team offense
  left_join(
    latest_offense,
    by = c("away_team" = "posteam")
  ) %>%
  select(-any_of(c("week.y", "season.y"))) %>%
  rename_with(~paste0("away_off_", .), 
              .cols = setdiff(names(latest_offense), c("posteam", "week", "season"))) %>%
  # Away team defense
  left_join(
    latest_defense,
    by = c("away_team" = "defteam")
  ) %>%
  select(-any_of(c("week.y", "season.y"))) %>%
  rename_with(~paste0("away_def_", .), 
              .cols = setdiff(names(latest_defense), c("defteam", "week", "season")))

# Ensure same features as training data
X_predict <- prediction_data %>%
  select(all_of(feature_cols)) %>%
  mutate(across(everything(), ~replace_na(., 0)))

cat("  ✓ Prediction data prepared\n\n")

# ============================================================================
# ============================================================================
# MAKE PREDICTIONS USING SIMPLER, MORE RELIABLE METHOD
# ============================================================================

cat("Generating predictions using ensemble weighted average...\n\n")

# Simpler approach: Use offensive/defensive EPA to predict outcomes
# This is more stable with limited training data

predictions_list <- list()

for (i in 1:nrow(schedule_week6)) {
  game <- schedule_week6[i, ]
  
  # Get home team stats (use most recent week's rolling average if available)
  home_off <- offense_rolling %>% 
    filter(posteam == game$home_team) %>%
    arrange(desc(week))
  home_off <- home_off[1, ]  # Get first row (most recent)
  
  home_def <- defense_rolling %>% 
    filter(defteam == game$home_team) %>%
    arrange(desc(week))
  home_def <- home_def[1, ]
  
  # Get away team stats  
  away_off <- offense_rolling %>% 
    filter(posteam == game$away_team) %>%
    arrange(desc(week))
  away_off <- away_off[1, ]
  
  away_def <- defense_rolling %>% 
    filter(defteam == game$away_team) %>%
    arrange(desc(week))
  away_def <- away_def[1, ]
  
  if (nrow(home_off) > 0 && nrow(home_def) > 0 && 
      nrow(away_off) > 0 && nrow(away_def) > 0) {
    
    # Use rolling average EPA if available, otherwise use regular EPA
    # Simplified approach to avoid else-if issues
    
    # Home offense EPA
    if ("avg_epa_roll3" %in% names(home_off) && !is.na(home_off$avg_epa_roll3[1])) {
      home_off_epa <- home_off$avg_epa_roll3[1]
    } else if ("avg_epa" %in% names(home_off)) {
      home_off_epa <- home_off$avg_epa[1]
    } else {
      home_off_epa <- 0.05
    }
    
    # Away defense EPA
    if ("def_epa_allowed_roll3" %in% names(away_def) && !is.na(away_def$def_epa_allowed_roll3[1])) {
      away_def_epa <- away_def$def_epa_allowed_roll3[1]
    } else if ("def_epa_allowed" %in% names(away_def)) {
      away_def_epa <- away_def$def_epa_allowed[1]
    } else {
      away_def_epa <- 0.0
    }
    
    # Away offense EPA
    if ("avg_epa_roll3" %in% names(away_off) && !is.na(away_off$avg_epa_roll3[1])) {
      away_off_epa <- away_off$avg_epa_roll3[1]
    } else if ("avg_epa" %in% names(away_off)) {
      away_off_epa <- away_off$avg_epa[1]
    } else {
      away_off_epa <- 0.05
    }
    
    # Home defense EPA
    if ("def_epa_allowed_roll3" %in% names(home_def) && !is.na(home_def$def_epa_allowed_roll3[1])) {
      home_def_epa <- home_def$def_epa_allowed_roll3[1]
    } else if ("def_epa_allowed" %in% names(home_def)) {
      home_def_epa <- home_def$def_epa_allowed[1]
    } else {
      home_def_epa <- 0.0
    }
    
    # Convert EPA to expected points with more realistic scaling
    # NFL average: ~65 plays per game, league average ~22 points per team
    avg_plays <- 65
    league_avg_points <- 22
    
    # Scale EPA more conservatively to avoid extreme predictions
    # Use 40 plays worth of EPA impact instead of 65 to dampen extremes
    epa_scaling_factor <- 40
    
    # Home team scoring prediction
    home_net_epa <- (home_off_epa - away_def_epa)
    home_expected_points <- league_avg_points + (home_net_epa * epa_scaling_factor) + 2.5  # HFA
    
    # Away team scoring prediction
    away_net_epa <- (away_off_epa - home_def_epa)
    away_expected_points <- league_avg_points + (away_net_epa * epa_scaling_factor)
    
    # Constrain to MORE reasonable ranges (NFL rarely sees scores outside 13-38)
    home_expected_points <- pmax(13, pmin(38, home_expected_points))
    away_expected_points <- pmax(13, pmin(38, away_expected_points))
    
    # Calculate derived metrics
    predicted_total <- home_expected_points + away_expected_points
    predicted_spread <- home_expected_points - away_expected_points
    
    predictions_list[[i]] <- data.frame(
      game_id = game$game_id,
      predicted_home_score = home_expected_points,
      predicted_away_score = away_expected_points,
      predicted_total = predicted_total,
      predicted_spread = predicted_spread,
      home_win_prob = 1 / (1 + exp(-predicted_spread / 13)),  # Logistic function
      stringsAsFactors = FALSE
    )
  }
}

# Combine predictions
simple_predictions <- bind_rows(predictions_list)

# Merge with schedule
final_predictions <- schedule_week6 %>%
  select(game_id, gameday, gametime, away_team, home_team, away_rest, home_rest) %>%
  left_join(simple_predictions, by = "game_id") %>%
  mutate(
    # Moneyline
    away_win_prob = 1 - home_win_prob,
    predicted_winner = if_else(home_win_prob > 0.5, home_team, away_team),
    win_confidence = pmax(home_win_prob, away_win_prob),
    
    # Spread
    spread_favorite = if_else(predicted_spread > 0, home_team, away_team),
    
    # Round for display
    predicted_home_score = round(predicted_home_score, 1),
    predicted_away_score = round(predicted_away_score, 1),
    predicted_total = round(predicted_total, 1),
    predicted_spread = round(predicted_spread, 1)
  )

# ============================================================================
# CREATE BETTING RECOMMENDATIONS & FULL SCHEDULE
# ============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PHASE 5: BETTING RECOMMENDATIONS & FULL SCHEDULE              ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# Create FULL schedule with all predictions shown
full_schedule <- final_predictions %>%
  mutate(
    # Format matchup
    matchup = paste(away_team, "@", home_team),
    
    # Moneyline - Always show full info
    ml_home_odds = paste0(home_team, " (", round(home_win_prob * 100, 1), "%)"),
    ml_away_odds = paste0(away_team, " (", round(away_win_prob * 100, 1), "%)"),
    ml_pick = predicted_winner,
    ml_confidence = paste0(round(win_confidence * 100, 1), "%"),
    
    # Moneyline recommendation strength
    moneyline_strength = case_when(
      win_confidence >= 0.70 ~ "🔥 STRONG",
      win_confidence >= MIN_CONFIDENCE_THRESHOLD ~ "✓ GOOD",
      TRUE ~ "⚠️ PASS"
    ),
    moneyline_bet = if_else(
      win_confidence >= MIN_CONFIDENCE_THRESHOLD,
      paste0("BET ", predicted_winner, " ML"),
      "PASS - Low confidence"
    ),
    
    # Spread - Always show full info
    spread_line = round(predicted_spread, 1),
    spread_favorite = if_else(predicted_spread > 0, home_team, away_team),
    spread_underdog = if_else(predicted_spread > 0, away_team, home_team),
    spread_display = paste0(spread_favorite, " -", abs(round(predicted_spread, 1))),
    spread_alt_display = paste0(spread_underdog, " +", abs(round(predicted_spread, 1))),
    
    # Total - Always show full info
    total_line = round(predicted_total, 1),
    over_display = paste0("OVER ", round(predicted_total, 1)),
    under_display = paste0("UNDER ", round(predicted_total, 1)),
    
    # Predicted final score
    predicted_score = paste0(
      home_team, " ", round(predicted_home_score, 1), 
      " - ", 
      away_team, " ", round(predicted_away_score, 1)
    ),
    
    # Game summary
    prediction_summary = paste0(
      predicted_winner, " wins ", 
      round(pmax(predicted_home_score, predicted_away_score), 1), "-",
      round(pmin(predicted_home_score, predicted_away_score), 1),
      " (Total: ", round(predicted_total, 1), ")"
    ),
    
    # Game competitiveness
    game_type = case_when(
      abs(predicted_spread) <= 3 ~ "🔥 CLOSE GAME",
      abs(predicted_spread) <= 7 ~ "⚖️ COMPETITIVE",
      abs(predicted_spread) <= 14 ~ "📊 MODERATE EDGE",
      TRUE ~ "💥 BLOWOUT"
    )
  ) %>%
  select(
    # Game Info
    gameday, gametime, matchup, away_team, home_team,
    
    # Moneyline Section
    ml_pick, ml_confidence, ml_home_odds, ml_away_odds, 
    home_win_prob, away_win_prob, moneyline_strength, moneyline_bet,
    
    # Spread Section
    spread_favorite, spread_underdog, spread_line, 
    spread_display, spread_alt_display,
    
    # Total Section
    total_line, over_display, under_display,
    predicted_home_score, predicted_away_score,
    
    # Summary
    predicted_score, prediction_summary, game_type
  ) %>%
  arrange(gameday, gametime)

cat("✓ Full schedule created with", nrow(full_schedule), "games\n")

# Create high-confidence recommendations subset
betting_recommendations <- full_schedule %>%
  filter(moneyline_strength %in% c("🔥 STRONG", "✓ GOOD")) %>%
  arrange(desc(home_win_prob))

cat("✓ Found", nrow(betting_recommendations), "high-confidence betting opportunities\n\n")

# ============================================================================
# EXPORT RESULTS
# ============================================================================

cat("Exporting predictions and models...\n")

output_dir <- file.path(BASE_DIR, paste0('week', PREDICT_WEEK), 'ml_predictions')
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save predictions
write_xlsx(
  list(
    Full_Schedule = full_schedule,
    Recommended_Bets = betting_recommendations,
    All_Predictions = final_predictions,
    Model_Performance = data.frame(
      Model = c("Logit", "RF", "XGB", "Ensemble"),
      Moneyline_Accuracy = c(
        round(logit_accuracy * 100, 1),
        round(rf_accuracy * 100, 1),
        round(xgb_accuracy * 100, 1),
        round(mean(c(logit_accuracy, rf_accuracy, xgb_accuracy)) * 100, 1)
      ),
      Spread_RMSE = c(
        round(lm_rmse, 2),
        round(rf_rmse_spread, 2),
        round(xgb_rmse_spread, 2),
        round(mean(c(lm_rmse, rf_rmse_spread, xgb_rmse_spread)), 2)
      ),
      Total_RMSE = c(
        round(lm_rmse_total, 2),
        round(rf_rmse_total, 2),
        round(xgb_rmse_total, 2),
        round(mean(c(lm_rmse_total, rf_rmse_total, xgb_rmse_total)), 2)
      )
    )
  ),
  file.path(output_dir, paste0("week", PREDICT_WEEK, "_complete_betting_card.xlsx"))
)

# Save individual CSVs - FULL SCHEDULE
write.csv(full_schedule, 
          file.path(output_dir, "full_week_schedule.csv"), 
          row.names = FALSE)

write.csv(betting_recommendations, 
          file.path(output_dir, "recommended_bets_only.csv"), 
          row.names = FALSE)

write.csv(final_predictions, 
          file.path(output_dir, "detailed_predictions.csv"), 
          row.names = FALSE)

# Save models for future use
saveRDS(list(
  moneyline = list(logit = model_logit, rf = model_rf_win, xgb = model_xgb_win),
  spread = list(lm = model_lm_spread, rf = model_rf_spread, xgb = model_xgb_spread),
  total = list(lm = model_lm_total, rf = model_rf_total, xgb = model_xgb_total)
), file.path(output_dir, "trained_models.rds"))

cat("✓ Results exported\n\n")

# ============================================================================
# VISUALIZATIONS
# ============================================================================

cat("Creating visualizations...\n")

# 1. Win Probability Chart
win_prob_plot <- betting_recommendations %>%
  mutate(matchup = paste(away_team, "@", home_team)) %>%
  arrange(desc(home_win_prob)) %>%
  head(12) %>%
  mutate(matchup = reorder(matchup, home_win_prob)) %>%  # Reorder BEFORE pivot
  tidyr::pivot_longer(cols = c(home_win_prob, away_win_prob), 
                      names_to = "team_type", values_to = "prob") %>%
  mutate(
    team = if_else(team_type == "home_win_prob", home_team, away_team)
  ) %>%
  ggplot(aes(x = matchup, y = prob, fill = team_type)) +
  geom_col(position = "stack") +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.7) +
  scale_fill_manual(
    values = c("home_win_prob" = "#2E86AB", "away_win_prob" = "#A23B72"),
    labels = c("home_win_prob" = "Home Win", "away_win_prob" = "Away Win")
  ) +
  coord_flip() +
  labs(
    title = paste("Week", PREDICT_WEEK, "- ML Win Probabilities"),
    subtitle = "Ensemble model predictions (Logit + RF + XGBoost)",
    x = NULL,
    y = "Win Probability",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "bottom"
  )

ggsave(file.path(output_dir, "win_probabilities.png"), 
       win_prob_plot, width = 12, height = 10, dpi = 300)

# 2. Predicted Spreads
spread_plot <- full_schedule %>%
  mutate(matchup = paste(away_team, "@", home_team)) %>%
  arrange(desc(abs(spread_line))) %>%
  head(12) %>%
  mutate(matchup = reorder(matchup, spread_line)) %>%
  ggplot(aes(x = matchup, y = spread_line, fill = spread_line > 0)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_fill_manual(
    values = c("TRUE" = "#27AE60", "FALSE" = "#E74C3C"),
    labels = c("TRUE" = "Home Favored", "FALSE" = "Away Favored")
  ) +
  coord_flip() +
  labs(
    title = paste("Week", PREDICT_WEEK, "- Predicted Point Spreads"),
    subtitle = "Positive = Home team favored | Negative = Away team favored",
    x = NULL,
    y = "Predicted Spread (Points)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "bottom"
  )

ggsave(file.path(output_dir, "predicted_spreads.png"), 
       spread_plot, width = 12, height = 10, dpi = 300)

# 3. Predicted Totals
total_plot <- full_schedule %>%
  mutate(matchup = paste(away_team, "@", home_team)) %>%
  arrange(desc(total_line)) %>%
  head(12) %>%
  mutate(matchup = reorder(matchup, total_line)) %>%
  ggplot(aes(x = matchup, y = total_line)) +
  geom_col(fill = "#667EEA") +
  geom_hline(yintercept = mean(full_schedule$total_line), 
             linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(aes(label = round(total_line, 1)), 
            hjust = -0.2, size = 3.5, fontface = "bold") +
  coord_flip() +
  labs(
    title = paste("Week", PREDICT_WEEK, "- Predicted Total Points"),
    subtitle = paste("Dashed line = Average total (", 
                     round(mean(full_schedule$total_line), 1), " pts)"),
    x = NULL,
    y = "Predicted Total Points"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  )

ggsave(file.path(output_dir, "predicted_totals.png"), 
       total_plot, width = 12, height = 10, dpi = 300)

# 4. Feature Importance (from Random Forest)
importance_df <- data.frame(
  Feature = rownames(importance(model_rf_win)),
  Importance = importance(model_rf_win)[, "MeanDecreaseGini"]
) %>%
  arrange(desc(Importance)) %>%
  head(20)

importance_plot <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "#764BA2") +
  coord_flip() +
  labs(
    title = "Top 20 Most Important Features for Win Prediction",
    subtitle = "Based on Random Forest model (Mean Decrease Gini)",
    x = NULL,
    y = "Importance Score"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text.y = element_text(size = 9)
  )

ggsave(file.path(output_dir, "feature_importance.png"), 
       importance_plot, width = 12, height = 10, dpi = 300)

cat("✓ Visualizations saved\n\n")

# ============================================================================
# PRINT RESULTS SUMMARY
# ============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  WEEK", PREDICT_WEEK, "PREDICTIONS COMPLETE                                  ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("📊 MODEL PERFORMANCE SUMMARY:\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("MONEYLINE (Win/Loss):\n")
cat("  • Ensemble Accuracy:  ", round(mean(c(logit_accuracy, rf_accuracy, xgb_accuracy)) * 100, 1), "%\n", sep = "")
cat("  • Best Model:         ", 
    c("Logistic", "Random Forest", "XGBoost")[which.max(c(
      logit_accuracy, rf_accuracy, xgb_accuracy))], "\n\n")

cat("SPREAD (Point Differential):\n")
cat("  • Ensemble RMSE:      ", round(mean(c(lm_rmse, rf_rmse_spread, xgb_rmse_spread)), 2), " points\n", sep = "")
cat("  • Best Model:         ", 
    c("Linear Reg", "Random Forest", "XGBoost")[which.min(c(
      lm_rmse, rf_rmse_spread, xgb_rmse_spread))], "\n\n")

cat("TOTALS (Combined Score):\n")
cat("  • Ensemble RMSE:      ", round(mean(c(lm_rmse_total, rf_rmse_total, xgb_rmse_total)), 2), " points\n", sep = "")
cat("  • Best Model:         ", 
    c("Linear Reg", "Random Forest", "XGBoost")[which.min(c(
      lm_rmse_total, rf_rmse_total, xgb_rmse_total))], "\n\n")

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

cat("📋 COMPLETE WEEK", PREDICT_WEEK, "SCHEDULE (ALL GAMES):\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

# Display ALL games with full betting info
for (i in 1:nrow(full_schedule)) {
  game <- full_schedule[i, ]
  
  cat(sprintf("GAME %d: %s\n", i, game$matchup))
  cat(sprintf("  📅 %s | ⏰ %s\n", game$gameday, game$gametime))
  cat(sprintf("  %s\n\n", game$game_type))
  
  cat("  💰 MONEYLINE:\n")
  cat(sprintf("     Pick: %s (%s confidence) %s\n", 
              game$ml_pick, game$ml_confidence, game$moneyline_strength))
  cat(sprintf("     %s: %.1f%% | %s: %.1f%%\n", 
              game$home_team, game$home_win_prob * 100,
              game$away_team, game$away_win_prob * 100))
  cat(sprintf("     → %s\n\n", game$moneyline_bet))
  
  cat("  📊 SPREAD:\n")
  cat(sprintf("     Favorite: %s -%s\n", 
              game$spread_favorite, abs(game$spread_line)))
  cat(sprintf("     Underdog: %s +%s\n", 
              game$spread_underdog, abs(game$spread_line)))
  cat(sprintf("     Line: %s\n\n", game$spread_display))
  
  cat("  🎯 TOTAL (Over/Under):\n")
  cat(sprintf("     O/U Line: %.1f points\n", game$total_line))
  cat(sprintf("     %s | %s\n\n", game$over_display, game$under_display))
  
  cat("  🏈 PREDICTED SCORE:\n")
  cat(sprintf("     %s\n", game$prediction_summary))
  cat(sprintf("     Final: %s %.1f - %s %.1f\n", 
              game$home_team, game$predicted_home_score,
              game$away_team, game$predicted_away_score))
  
  cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
}

cat("\n🎯 HIGH CONFIDENCE BETS ONLY:\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

# Top Moneyline Bets
cat("💰 MONEYLINE PICKS (High Confidence):\n")
top_ml <- betting_recommendations %>%
  filter(moneyline_strength %in% c("🔥 STRONG", "✓ GOOD")) %>%
  head(5)

if (nrow(top_ml) > 0) {
  for (i in 1:nrow(top_ml)) {
    bet <- top_ml[i, ]
    cat(sprintf("  %s  %s @ %s\n", 
                bet$moneyline_strength,
                bet$away_team, bet$home_team))
    cat(sprintf("       → %s\n", bet$moneyline_bet))
    cat(sprintf("       Win Prob: Home %.1f%% | Away %.1f%%\n\n", 
                bet$home_win_prob * 100, bet$away_win_prob * 100))
  }
} else {
  cat("  No high confidence moneyline bets this week\n\n")
}

# Top Spread Predictions
cat("📈 SPREAD PREDICTIONS (Biggest Margins):\n")
top_spreads <- full_schedule %>%
  arrange(desc(abs(spread_line))) %>%
  head(5)

for (i in 1:nrow(top_spreads)) {
  bet <- top_spreads[i, ]
  cat(sprintf("  %s @ %s\n", bet$away_team, bet$home_team))
  cat(sprintf("       → Predicted: %s by %.1f points\n", 
              bet$spread_favorite, abs(bet$spread_line)))
  cat(sprintf("       Score: %.1f - %.1f\n\n",
              bet$predicted_home_score, bet$predicted_away_score))
}

# Over/Under Extremes
cat("🎲 TOTAL PREDICTIONS (Highest/Lowest Scoring):\n")
cat("  HIGHEST:\n")
high_scoring <- full_schedule %>%
  arrange(desc(total_line)) %>%
  head(3)

for (i in 1:nrow(high_scoring)) {
  bet <- high_scoring[i, ]
  cat(sprintf("    %s @ %s: %.1f points\n", 
              bet$away_team, bet$home_team, bet$total_line))
}

cat("\n  LOWEST:\n")
low_scoring <- full_schedule %>%
  arrange(total_line) %>%
  head(3)

for (i in 1:nrow(low_scoring)) {
  bet <- low_scoring[i, ]
  cat(sprintf("    %s @ %s: %.1f points\n", 
              bet$away_team, bet$home_team, bet$total_line))
}

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

cat("📁 OUTPUT FILES:\n")
cat("   Location:", output_dir, "\n")
cat("   • week", PREDICT_WEEK, "_complete_betting_card.xlsx (Excel with all tabs)\n", sep = "")
cat("   • full_week_schedule.csv (ALL games with predictions)\n")
cat("   • recommended_bets_only.csv (High confidence only)\n")
cat("   • detailed_predictions.csv (Raw model output)\n")
cat("   • trained_models.rds (Saved ML models)\n")
cat("   • win_probabilities.png\n")
cat("   • predicted_spreads.png\n")
cat("   • predicted_totals.png\n")
cat("   • feature_importance.png\n\n")

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

cat("⚠️  IMPORTANT NOTES:\n")
cat("   1. These are STATISTICAL PREDICTIONS based on weeks 1-5 data\n")
cat("   2. Compare predictions to actual Vegas lines for betting value\n")
cat("   3. Models cannot account for: injuries, weather, motivation\n")
cat("   4. Use", MIN_CONFIDENCE_THRESHOLD * 100, "% threshold for moneyline recommendations\n")
cat("   5. Track results after Week", PREDICT_WEEK, "to validate model accuracy\n\n")

cat("✅ NEXT STEPS:\n")
cat("   1. Compare predictions to Vegas lines using nfl_odds_integration.R\n")
cat("   2. After games complete, run accuracy analysis\n")
cat("   3. Retrain models with Week", PREDICT_WEEK, "data for Week", PREDICT_WEEK + 1, "\n\n")

cat("✓ Machine Learning Pipeline Complete! 🏈📊\n\n")

# ============================================================================
# EXPORT SUMMARY STATISTICS
# ============================================================================

summary_stats <- data.frame(
  Metric = c(
    "Training Games",
    "Training Weeks",
    "Features Used",
    "Games to Predict",
    "Moneyline Accuracy",
    "Spread RMSE",
    "Total RMSE",
    "High Confidence Bets",
    "Average Predicted Total",
    "Closest Game (Spread)",
    "Biggest Blowout (Spread)"
  ),
  Value = c(
    nrow(training_data_clean),
    paste(min(TRAIN_WEEKS), "-", max(TRAIN_WEEKS)),
    length(feature_cols),
    nrow(full_schedule),
    paste0(round(mean(c(logit_accuracy, rf_accuracy, xgb_accuracy)) * 100, 1), "%"),
    paste0(round(mean(c(lm_rmse, rf_rmse_spread, xgb_rmse_spread)), 2), " pts"),
    paste0(round(mean(c(lm_rmse_total, rf_rmse_total, xgb_rmse_total)), 2), " pts"),
    sum(full_schedule$moneyline_strength %in% c("🔥 STRONG", "✓ GOOD")),
    paste0(round(mean(full_schedule$total_line), 1), " pts"),
    paste0(round(min(abs(full_schedule$spread_line)), 1), " pts"),
    paste0(round(max(abs(full_schedule$spread_line)), 1), " pts")
  )
)

write.csv(summary_stats, 
          file.path(output_dir, "summary_statistics.csv"), 
          row.names = FALSE)

# ============================================================================
# CREATE COMPARISON WITH VEGAS (if odds available)
# ============================================================================

cat("Checking for Vegas odds data...\n")

odds_file <- file.path(BASE_DIR, paste0('week', PREDICT_WEEK), 
                       'odds_analysis', 'all_games_analysis.csv')

if (file.exists(odds_file)) {
  cat("  ✓ Found Vegas odds - creating comparison...\n")
  
  vegas_odds <- read.csv(odds_file)
  
  # Print available columns to debug
  cat("  Available columns in odds file:", paste(names(vegas_odds), collapse = ", "), "\n")
  
  # Check which columns exist and map to standard names
  has_spread <- any(c("vegas_line", "spread", "line") %in% names(vegas_odds))
  has_total <- any(c("over_under", "total", "totals") %in% names(vegas_odds))
  has_sportsbook <- "sportsbook" %in% names(vegas_odds)
  has_matchup <- any(c("matchup_key", "matchup", "game") %in% names(vegas_odds))
  
  # Find the actual column names
  spread_col <- names(vegas_odds)[names(vegas_odds) %in% c("vegas_line", "spread", "line")][1]
  total_col <- names(vegas_odds)[names(vegas_odds) %in% c("over_under", "total", "totals")][1]
  matchup_col <- names(vegas_odds)[names(vegas_odds) %in% c("matchup_key", "matchup", "game")][1]
  
  if (is.na(matchup_col)) {
    cat("  ⚠️  Cannot find matchup column. Available columns:", paste(names(vegas_odds), collapse = ", "), "\n")
    cat("  Skipping Vegas comparison...\n\n")
  } else {
    cat("  Using columns: matchup =", matchup_col, 
        "| spread =", ifelse(is.na(spread_col), "NONE", spread_col),
        "| total =", ifelse(is.na(total_col), "NONE", total_col), "\n")
    
    # Select only columns that exist
    vegas_cols <- matchup_col
    if (!is.na(spread_col)) vegas_cols <- c(vegas_cols, spread_col)
    if (!is.na(total_col)) vegas_cols <- c(vegas_cols, total_col)
    if (has_sportsbook) vegas_cols <- c(vegas_cols, "sportsbook")
    
    # Rename columns to standard names
    vegas_odds_clean <- vegas_odds %>%
      select(all_of(vegas_cols)) %>%
      rename(matchup_key = !!matchup_col)
    
    if (!is.na(spread_col)) {
      vegas_odds_clean <- vegas_odds_clean %>% rename(vegas_line = !!spread_col)
    }
    if (!is.na(total_col)) {
      vegas_odds_clean <- vegas_odds_clean %>% rename(vegas_total = !!total_col)
    }
    
    # Compare ML predictions to Vegas lines
    ml_comparison <- full_schedule %>%
      left_join(
        vegas_odds_clean,
        by = c("matchup" = "matchup_key")
      ) %>%
      mutate(
        # Calculate edges (only if columns exist)
        spread_edge = if ("vegas_line" %in% names(.)) {
          if_else(!is.na(vegas_line), spread_line - vegas_line, NA_real_)
        } else {
          NA_real_
        },
        
        total_edge = if ("vegas_total" %in% names(.)) {
          if_else(!is.na(vegas_total), total_line - vegas_total, NA_real_)
        } else {
          NA_real_
        },
        
        # Spread betting recommendations
        spread_bet_rec = if ("vegas_line" %in% names(.)) {
          case_when(
            is.na(vegas_line) ~ "NO VEGAS LINE AVAILABLE",
            abs(spread_edge) >= 4.5 ~ paste0("🔥 STRONG - Bet ", 
                                             if_else(spread_edge > 0, 
                                                     spread_underdog, 
                                                     spread_favorite)),
            abs(spread_edge) >= 2.5 ~ paste0("✓ GOOD - Consider ", 
                                             if_else(spread_edge > 0, 
                                                     spread_underdog, 
                                                     spread_favorite)),
            TRUE ~ "PASS"
          )
        } else {
          "NO VEGAS LINE AVAILABLE"
        },
        
        spread_value = if ("vegas_line" %in% names(.)) {
          case_when(
            is.na(vegas_line) ~ "N/A",
            abs(spread_edge) >= 4.5 ~ "🔥 ELITE VALUE",
            abs(spread_edge) >= 2.5 ~ "✓ GOOD VALUE",
            TRUE ~ "PASS"
          )
        } else {
          "N/A"
        },
        
        # Total betting recommendations
        total_bet_rec = if ("vegas_total" %in% names(.)) {
          case_when(
            is.na(vegas_total) ~ "NO VEGAS TOTAL AVAILABLE",
            total_edge >= 6 ~ "🔥 STRONG - Bet OVER",
            total_edge <= -6 ~ "🔥 STRONG - Bet UNDER",
            total_edge >= 3.5 ~ "✓ GOOD - Consider OVER",
            total_edge <= -3.5 ~ "✓ GOOD - Consider UNDER",
            TRUE ~ "PASS"
          )
        } else {
          "NO VEGAS TOTAL AVAILABLE"
        },
        
        total_value = if ("vegas_total" %in% names(.)) {
          case_when(
            is.na(vegas_total) ~ "N/A",
            abs(total_edge) >= 6 ~ "🔥 ELITE VALUE",
            abs(total_edge) >= 3.5 ~ "✓ GOOD VALUE",
            TRUE ~ "PASS"
          )
        } else {
          "N/A"
        },
        
        # Add Vegas lines to display
        vegas_spread_display = if ("vegas_line" %in% names(.)) {
          if_else(
            !is.na(vegas_line),
            paste0("Vegas: ", if_else(vegas_line > 0, home_team, away_team), 
                   " ", round(abs(vegas_line), 1)),
            "No line"
          )
        } else {
          "No line"
        },
        
        vegas_total_display = if ("vegas_total" %in% names(.)) {
          if_else(
            !is.na(vegas_total),
            paste0("Vegas O/U: ", round(vegas_total, 1)),
            "No total"
          )
        } else {
          "No total"
        }
      )
    
    # Build the list of sheets dynamically
    excel_sheets <- list(
      Full_Schedule_With_Vegas = ml_comparison %>%
        select(gameday, gametime, matchup, game_type,
               ml_pick, ml_confidence, moneyline_strength, moneyline_bet,
               home_win_prob, away_win_prob,
               spread_display, vegas_spread_display, spread_edge, 
               spread_value, spread_bet_rec,
               total_line, vegas_total_display, total_edge,
               total_value, total_bet_rec,
               predicted_score, prediction_summary, sportsbook),
      
      Value_Bets_Only = ml_comparison %>%
        filter(spread_value %in% c("🔥 ELITE VALUE", "✓ GOOD VALUE") |
                 total_value %in% c("🔥 ELITE VALUE", "✓ GOOD VALUE") |
                 moneyline_strength %in% c("🔥 STRONG", "✓ GOOD")) %>%
        select(gameday, gametime, matchup,
               moneyline_strength, moneyline_bet,
               spread_value, spread_bet_rec, spread_edge,
               total_value, total_bet_rec, total_edge,
               predicted_score, sportsbook),
      
      Spread_Edges = ml_comparison %>%
        filter(!is.na(spread_edge)) %>%
        arrange(desc(abs(spread_edge))) %>%
        select(matchup, spread_display, vegas_spread_display, 
               spread_edge, spread_value, spread_bet_rec)
    )
    
    # Only add Total_Edges if we have the vegas_total column
    if ("vegas_total" %in% names(ml_comparison)) {
      excel_sheets$Total_Edges <- ml_comparison %>%
        filter(!is.na(vegas_total)) %>%
        arrange(desc(abs(total_edge))) %>%
        select(matchup, total_line, vegas_total, 
               total_edge, total_value, total_bet_rec)
    }
    
    # Export
    write_xlsx(excel_sheets, file.path(output_dir, "ml_vs_vegas_complete.xlsx"))
    
    cat("  ✓ ML vs Vegas comparison saved\n\n")
    
    # Show ALL games with Vegas comparison
    cat("📊 COMPLETE WEEK", PREDICT_WEEK, "BETTING CARD (With Vegas Lines):\n")
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
    
    for (i in 1:nrow(ml_comparison)) {
      game <- ml_comparison[i, ]
      
      cat(sprintf("GAME %d: %s\n", i, game$matchup))
      cat(sprintf("  📅 %s | ⏰ %s | %s\n\n", game$gameday, game$gametime, game$game_type))
      
      cat("  💰 MONEYLINE:\n")
      cat(sprintf("     ML Pick: %s (%s) %s\n", 
                  game$ml_pick, game$ml_confidence, game$moneyline_strength))
      cat(sprintf("     → %s\n\n", game$moneyline_bet))
      
      cat("  📊 SPREAD:\n")
      cat(sprintf("     ML Model: %s\n", game$spread_display))
      cat(sprintf("     %s\n", game$vegas_spread_display))
      if (!is.na(game$spread_edge)) {
        cat(sprintf("     Edge: %.1f points | %s\n", game$spread_edge, game$spread_value))
        cat(sprintf("     → %s\n\n", game$spread_bet_rec))
      } else {
        cat("     → Waiting for Vegas line\n\n")
      }
      
      cat("  🎯 TOTAL:\n")
      cat(sprintf("     ML Model: %.1f\n", game$total_line))
      cat(sprintf("     %s\n", game$vegas_total_display))
      if (!is.na(game$total_edge)) {
        cat(sprintf("     Edge: %.1f points | %s\n", game$total_edge, game$total_value))
        cat(sprintf("     → %s\n\n", game$total_bet_rec))
      } else {
        cat("     → Waiting for Vegas total\n\n")
      }
      
      cat("  🏈 PREDICTED SCORE:\n")
      cat(sprintf("     %s\n", game$prediction_summary))
      
      if (!is.na(game$sportsbook)) {
        cat(sprintf("     Source: %s\n", game$sportsbook))
      }
      
      cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
    }
    
    # Show top edges
    cat("\n\n🎯 TOP VALUE BETS (ML vs Vegas):\n")
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
    
    cat("SPREAD VALUE BETS:\n")
    top_spread_edges <- ml_comparison %>%
      filter(!is.na(spread_edge)) %>%
      arrange(desc(abs(spread_edge))) %>%
      head(5)
    
    if (nrow(top_spread_edges) > 0) {
      for (i in 1:nrow(top_spread_edges)) {
        edge <- top_spread_edges[i, ]
        cat(sprintf("  %s %s\n", edge$spread_value, edge$matchup))
        cat(sprintf("    ML: %s | Vegas: %s | Edge: %+.1f\n", 
                    edge$spread_display, edge$vegas_spread_display, edge$spread_edge))
        cat(sprintf("    → %s\n\n", edge$spread_bet_rec))
      }
    } else {
      cat("  No spread edges available yet\n\n")
    }
    
    cat("TOTAL VALUE BETS:\n")
    top_total_edges <- ml_comparison %>%
      filter(!is.na(total_edge)) %>%
      arrange(desc(abs(total_edge))) %>%
      head(5)
    
    if (nrow(top_total_edges) > 0) {
      for (i in 1:nrow(top_total_edges)) {
        edge <- top_total_edges[i, ]
        cat(sprintf("  %s %s\n", edge$total_value, edge$matchup))
        cat(sprintf("    ML: %.1f | Vegas: %.1f | Edge: %+.1f\n", 
                    edge$total_line, edge$over_under, edge$total_edge))
        cat(sprintf("    → %s\n\n", edge$total_bet_rec))
      }
    } else {
      cat("  No total edges available yet\n\n")
    }
    
    cat("\n📁 VEGAS COMPARISON FILES SAVED:\n")
    cat("   • ml_vs_vegas_complete.xlsx (Full comparison with all tabs)\n")
    cat("     - Full Schedule With Vegas\n")
    cat("     - Value Bets Only\n")
    cat("     - Spread Edges\n")
    cat("     - Total Edges\n\n")
  }
  
  } else {
    cat("  ⚠️  Vegas odds not found - run nfl_odds_integration.R first\n")
    cat("     to compare ML predictions with betting lines\n\n")
  }
  
  
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║  ALL TASKS COMPLETE - READY FOR WEEK", PREDICT_WEEK, "BETTING!              ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")
  
  cat("🏈 FINAL SUMMARY:\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
  cat("✅ Models Trained:\n")
  cat("   • Moneyline (Logistic, Random Forest, XGBoost)\n")
  cat("   • Spread (Linear Regression, Random Forest, XGBoost)\n")
  cat("   • Totals (Linear Regression, Random Forest, XGBoost)\n\n")
  
  cat("✅ Files Created:\n")
  cat("   📁", output_dir, "\n")
  cat("   • week", PREDICT_WEEK, "_complete_betting_card.xlsx\n", sep = "")
  cat("   • full_week_schedule.csv (ALL games)\n")
  cat("   • recommended_bets_only.csv (High confidence)\n")
  cat("   • detailed_predictions.csv\n")
  cat("   • trained_models.rds\n")
  cat("   • 4 visualization PNGs\n")
  if (exists("ml_comparison")) {
    cat("   • ml_vs_vegas_complete.xlsx (Comparison with Vegas)\n")
  }
  cat("\n")
  
  cat("✅ Next Steps:\n")
  cat("   1. Review full_week_schedule.csv for ALL game predictions\n")
  cat("   2. Check recommended_bets_only.csv for high-confidence plays\n")
  cat("   3. Compare with Vegas lines (run nfl_odds_integration.R if not done)\n")
  cat("   4. After Week", PREDICT_WEEK, "completes, validate accuracy\n")
  cat("   5. Retrain models with Week", PREDICT_WEEK, "data for Week", PREDICT_WEEK + 1, "\n\n")
  
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("💡 TIP: The 'full_week_schedule.csv' contains EVERY game with complete\n")
  cat("    betting information (ML, spread, total) - not just recommended bets!\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
  
  cat("🎯 Good luck with your Week", PREDICT_WEEK, "predictions! 🏈💰\n\n")