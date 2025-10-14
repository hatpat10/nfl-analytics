# ============================================================================
# NFL MODEL TESTING & ACCURACY TRACKER
# ============================================================================
# Purpose: Compare model predictions to actual results and track accuracy
# Run AFTER games complete to evaluate model performance
# ============================================================================

# Clear environment
rm(list = ls())
gc()

# ============================================================================
# CONFIGURATION
# ============================================================================

SEASON <- 2025
WEEK <- 5  # Week to test (games must be completed)
BASE_DIR <- 'C:/Users/Patsc/Documents/nfl'
RESULTS_DB <- file.path(BASE_DIR, 'model_performance_history.csv')

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║        NFL MODEL ACCURACY TRACKER                              ║\n")
cat("║        Testing Week:", WEEK, "Results                                  ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# ============================================================================
# LOAD PACKAGES
# ============================================================================

cat("Loading required packages...\n")

required_packages <- c("nflreadr", "dplyr", "tidyr", "writexl", "ggplot2")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

cat("✓ Packages loaded!\n\n")

# ============================================================================
# LOAD MODEL PREDICTIONS
# ============================================================================

cat("Loading model predictions for Week", WEEK, "...\n")

week_dir <- file.path(BASE_DIR, paste0('week', WEEK))

# Load game predictions
matchup_file <- file.path(week_dir, 'matchup_analysis', 'matchup_summary_enhanced.csv')
if (!file.exists(matchup_file)) {
  matchup_file <- file.path(week_dir, 'matchup_analysis', 'matchup_summary.csv')
}

if (!file.exists(matchup_file)) {
  stop("❌ No matchup predictions found for Week ", WEEK, "!\n",
       "   Run nfl_master_pipeline.R first.")
}

model_predictions <- read.csv(matchup_file)

# Load betting recommendations if available
betting_file <- file.path(week_dir, 'odds_analysis', 'all_games_analysis.csv')
if (file.exists(betting_file)) {
  betting_analysis <- read.csv(betting_file)
  has_betting <- TRUE
} else {
  has_betting <- FALSE
  cat("⚠ No betting analysis found - will test game predictions only\n")
}

# Load player props predictions
qb_file <- file.path(week_dir, 'player_props', 'qb_projections.csv')
rb_file <- file.path(week_dir, 'player_props', 'rb_projections.csv')
wr_file <- file.path(week_dir, 'player_props', 'wr_projections.csv')

has_props <- file.exists(qb_file) && file.exists(rb_file) && file.exists(wr_file)

if (has_props) {
  qb_predictions <- read.csv(qb_file)
  rb_predictions <- read.csv(rb_file)
  wr_predictions <- read.csv(wr_file)
  cat("✓ Player props predictions loaded\n")
} else {
  cat("⚠ No player props found - will test game predictions only\n")
}

cat("✓ Model predictions loaded\n\n")

# ============================================================================
# LOAD ACTUAL RESULTS
# ============================================================================

cat("Loading actual results from Week", WEEK, "...\n")

# Load completed games
actual_scores <- nflreadr::load_schedules(SEASON) %>%
  filter(week == !!WEEK, !is.na(result)) %>%
  select(game_id, week, gameday, home_team, away_team, 
         home_score, away_score, result, home_moneyline, away_moneyline,
         spread_line, home_spread_odds, away_spread_odds)

if (nrow(actual_scores) == 0) {
  stop("❌ No completed games found for Week ", WEEK, "!\n",
       "   Games must be finished before testing accuracy.\n",
       "   Check if the week is correct or if scores are available yet.")
}

cat("✓ Found", nrow(actual_scores), "completed games\n")

# Load actual player stats
pbp <- nflreadr::load_pbp(SEASON) %>%
  filter(week == !!WEEK, season_type == "REG")

if (nrow(pbp) == 0) {
  stop("❌ No play-by-play data found for Week ", WEEK, "!")
}

# Aggregate actual QB stats
actual_qb <- pbp %>%
  filter(play_type == "pass", !is.na(passer_player_id)) %>%
  group_by(passer_player_name, posteam) %>%
  summarise(
    actual_pass_yards = sum(passing_yards, na.rm = TRUE),
    pass_attempts = sum(pass_attempt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(pass_attempts >= 10)

# Aggregate actual RB stats
actual_rb <- pbp %>%
  filter(play_type == "run", !is.na(rusher_player_id)) %>%
  group_by(rusher_player_name, posteam) %>%
  summarise(
    actual_rush_yards = sum(rushing_yards, na.rm = TRUE),
    rush_attempts = n(),
    .groups = "drop"
  ) %>%
  filter(rush_attempts >= 5)

# Aggregate actual WR stats
actual_wr <- pbp %>%
  filter(play_type == "pass", !is.na(receiver_player_id)) %>%
  group_by(receiver_player_name, posteam) %>%
  summarise(
    actual_rec_yards = sum(receiving_yards, na.rm = TRUE),
    targets = n(),
    .groups = "drop"
  ) %>%
  filter(targets >= 3)

cat("✓ Actual player stats loaded\n\n")

# ============================================================================
# TEST GAME PREDICTIONS
# ============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("TESTING GAME PREDICTIONS\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Match predictions to actual results
game_results <- model_predictions %>%
  inner_join(
    actual_scores %>% mutate(matchup_key = paste(away_team, "at", home_team)),
    by = c("game" = "matchup_key")
  ) %>%
  mutate(
    actual_margin = home_score - away_score,
    predicted_margin = projected_margin,
    error = actual_margin - predicted_margin,
    abs_error = abs(error),
    
    # Did we predict winner correctly?
    predicted_winner = if_else(predicted_margin > 0, home_team.x, away_team.x),
    actual_winner = if_else(actual_margin > 0, home_team.x, away_team.x),
    correct_winner = predicted_winner == actual_winner,
    
    # How close was the margin?
    within_3 = abs_error <= 3,
    within_7 = abs_error <= 7,
    within_10 = abs_error <= 10
  )

# Calculate metrics
game_metrics <- data.frame(
  week = WEEK,
  games_tested = nrow(game_results),
  mean_abs_error = mean(game_results$abs_error, na.rm = TRUE),
  median_abs_error = median(game_results$abs_error, na.rm = TRUE),
  rmse = sqrt(mean(game_results$error^2, na.rm = TRUE)),
  winner_accuracy = mean(game_results$correct_winner, na.rm = TRUE) * 100,
  within_3_pct = mean(game_results$within_3, na.rm = TRUE) * 100,
  within_7_pct = mean(game_results$within_7, na.rm = TRUE) * 100,
  within_10_pct = mean(game_results$within_10, na.rm = TRUE) * 100
)

cat("📊 GAME PREDICTION ACCURACY:\n\n")
cat("  Games Tested:", game_metrics$games_tested, "\n")
cat("  Mean Absolute Error:", round(game_metrics$mean_abs_error, 2), "points\n")
cat("  Median Absolute Error:", round(game_metrics$median_abs_error, 2), "points\n")
cat("  Root Mean Squared Error:", round(game_metrics$rmse, 2), "points\n")
cat("  Winner Prediction Accuracy:", round(game_metrics$winner_accuracy, 1), "%\n")
cat("  Within 3 points:", round(game_metrics$within_3_pct, 1), "%\n")
cat("  Within 7 points:", round(game_metrics$within_7_pct, 1), "%\n")
cat("  Within 10 points:", round(game_metrics$within_10_pct, 1), "%\n\n")

# Show biggest misses
cat("🔴 BIGGEST PREDICTION ERRORS:\n\n")
biggest_errors <- game_results %>%
  arrange(desc(abs_error)) %>%
  head(3) %>%
  select(game, predicted_margin, actual_margin, error)

for (i in 1:nrow(biggest_errors)) {
  err <- biggest_errors[i, ]
  cat(sprintf("  %s\n", err$game))
  cat(sprintf("    Predicted: %+.1f | Actual: %+.1f | Error: %.1f pts\n\n",
              err$predicted_margin, err$actual_margin, err$abs_error))
}

# ============================================================================
# TEST BETTING PICKS (if available)
# ============================================================================

if (has_betting && nrow(betting_analysis) > 0) {
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("TESTING BETTING PICKS\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  betting_results <- betting_analysis %>%
    filter(ev_tier != "❌ PASS") %>%
    inner_join(
      actual_scores %>% mutate(matchup_key = paste(away_team, "at", home_team)),
      by = "matchup_key"
    ) %>%
    mutate(
      actual_margin = home_score - away_score,
      
      # The join creates home_team and away_team columns from actual_scores
      # We need to reference them correctly
      home = home_team,
      away = away_team,
      
      # Extract which team we bet on from bet_recommendation
      # The bet_recommendation format is like "BET KC +3.5 (-110)"
      bet_team = case_when(
        grepl(home, bet_recommendation, fixed = TRUE) ~ home,
        grepl(away, bet_recommendation, fixed = TRUE) ~ away,
        TRUE ~ NA_character_
      ),
      
      # Determine if we bet home or away
      bet_on_home = bet_team == home,
      
      # Calculate if bet won
      # Spread is from home perspective: negative = home favored
      # If we bet home: we win if they cover (actual_margin > -vegas_line)
      # If we bet away: we win if they cover (actual_margin < -vegas_line)
      bet_result = case_when(
        is.na(bet_team) ~ "ERROR",
        bet_on_home & (actual_margin > -vegas_line + 0.5) ~ "WIN",
        !bet_on_home & (actual_margin < -vegas_line - 0.5) ~ "WIN",
        abs(actual_margin + vegas_line) <= 0.5 ~ "PUSH",
        TRUE ~ "LOSS"
      ),
      
      bet_won = bet_result == "WIN",
      is_push = bet_result == "PUSH"
    )
  
  # Debug: show any ERROR results
  error_bets <- betting_results %>% filter(bet_result == "ERROR")
  if (nrow(error_bets) > 0) {
    cat("⚠️  WARNING: Could not determine bet team for", nrow(error_bets), "bets\n")
    cat("   Check bet_recommendation format\n\n")
  }
  
  betting_metrics <- data.frame(
    week = WEEK,
    total_bets = nrow(betting_results),
    bets_won = sum(betting_results$bet_result == "WIN", na.rm = TRUE),
    bets_lost = sum(betting_results$bet_result == "LOSS", na.rm = TRUE),
    pushes = sum(betting_results$bet_result == "PUSH", na.rm = TRUE),
    win_rate = if(sum(betting_results$bet_result %in% c("WIN", "LOSS")) > 0) {
      sum(betting_results$bet_result == "WIN", na.rm = TRUE) / 
        sum(betting_results$bet_result %in% c("WIN", "LOSS")) * 100
    } else {
      NA_real_
    },
    elite_bets = sum(betting_results$ev_tier == "🔥 ELITE"),
    elite_wins = sum(betting_results$ev_tier == "🔥 ELITE" & betting_results$bet_result == "WIN", na.rm = TRUE)
  )
  
  cat("💰 BETTING PERFORMANCE:\n\n")
  cat("  Total Bets Made:", betting_metrics$total_bets, "\n")
  cat("  Wins:", betting_metrics$bets_won, "✅\n")
  cat("  Losses:", betting_metrics$bets_lost, "❌\n")
  cat("  Pushes:", betting_metrics$pushes, "🟡\n")
  
  if (!is.na(betting_metrics$win_rate)) {
    cat("  Win Rate:", round(betting_metrics$win_rate, 1), "%\n")
    
    if (betting_metrics$win_rate > 55) {
      cat("  📈 PROFITABLE - Above 55% win rate\n")
    } else if (betting_metrics$win_rate >= 52.4) {
      cat("  ✓ BREAK-EVEN - Above 52.4% threshold\n")
    } else {
      cat("  📉 UNPROFITABLE - Below break-even\n")
    }
  } else {
    cat("  Win Rate: N/A\n")
  }
  
  if (betting_metrics$elite_bets > 0) {
    elite_rate <- (betting_metrics$elite_wins / betting_metrics$elite_bets) * 100
    cat("  Elite Bets:", betting_metrics$elite_bets, 
        "(", betting_metrics$elite_wins, "✅ /", 
        betting_metrics$elite_bets - betting_metrics$elite_wins, "❌ = ",
        round(elite_rate, 1), "%)\n")
  }
  cat("\n  ℹ️  Break-even rate with -110 odds: 52.4%\n\n")
  
  # Show all bet results
  cat("📋 ALL BET RESULTS:\n\n")
  bet_summary <- betting_results %>%
    arrange(desc(abs(edge))) %>%
    select(matchup_key, ev_tier, edge, vegas_line, actual_margin, bet_team, bet_result)
  
  for (i in 1:nrow(bet_summary)) {
    bet <- bet_summary[i, ]
    result_emoji <- if (bet$bet_result == "PUSH") "🟡" else if (bet$bet_result == "WIN") "✅" else "❌"
    
    cat(sprintf("  %s %s - %s\n", result_emoji, bet$matchup_key, bet$bet_result))
    cat(sprintf("      Bet on: %s | Tier: %s | Edge: %.1f\n",
                bet$bet_team, bet$ev_tier, bet$edge))
    cat(sprintf("      Vegas Line: %.1f | Actual Margin: %+.0f\n\n",
                bet$vegas_line, bet$actual_margin))
  }
}

# ============================================================================
# TEST PLAYER PROPS (if available)
# ============================================================================

if (has_props) {
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("TESTING PLAYER PROPS\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # Fix column names if they have .x or .y suffixes
  if ("team.x" %in% names(qb_predictions) && !"team" %in% names(qb_predictions)) {
    qb_predictions <- qb_predictions %>% rename(team = team.x)
  }
  if ("team.x" %in% names(rb_predictions) && !"team" %in% names(rb_predictions)) {
    rb_predictions <- rb_predictions %>% rename(team = team.x)
  }
  if ("team.x" %in% names(wr_predictions) && !"team" %in% names(wr_predictions)) {
    wr_predictions <- wr_predictions %>% rename(team = team.x)
  }
  
  # QB accuracy
  qb_results <- qb_predictions %>%
    inner_join(actual_qb, by = c("player_name" = "passer_player_name", 
                                 "team" = "posteam"),
               suffix = c("_pred", "_actual")) %>%
    mutate(
      error = actual_pass_yards - projected_pass_yards,
      abs_error = abs(error),
      pct_error = (error / actual_pass_yards) * 100
    )
  
  if (nrow(qb_results) > 0) {
    qb_mae <- mean(qb_results$abs_error, na.rm = TRUE)
    qb_mape <- mean(abs(qb_results$pct_error), na.rm = TRUE)
    
    # Calculate hit rate (within 20% of actual)
    qb_results <- qb_results %>%
      mutate(hit = abs(pct_error) <= 20)
    qb_hit_rate <- mean(qb_results$hit, na.rm = TRUE) * 100
    
    cat("🎯 QUARTERBACK PROJECTIONS:\n\n")
    cat("  QBs Tested:", nrow(qb_results), "\n")
    cat("  Mean Absolute Error:", round(qb_mae, 1), "yards\n")
    cat("  Mean Absolute % Error:", round(qb_mape, 1), "%\n")
    cat("  Hit Rate (within 20%):", round(qb_hit_rate, 1), "%\n\n")
    
    cat("  Top 5 Most Accurate:\n")
    top_qb <- qb_results %>% arrange(abs_error) %>% head(5)
    for (i in 1:nrow(top_qb)) {
      hit_symbol <- if(top_qb$hit[i]) "✅" else "❌"
      cat(sprintf("    %s %s: Proj %.0f | Actual %.0f (off by %.0f)\n",
                  hit_symbol, top_qb$player_name[i], top_qb$projected_pass_yards[i],
                  top_qb$actual_pass_yards[i], top_qb$abs_error[i]))
    }
    cat("\n")
  }
  
  # RB accuracy
  rb_results <- rb_predictions %>%
    inner_join(actual_rb, by = c("player_name" = "rusher_player_name", 
                                 "team" = "posteam"),
               suffix = c("_pred", "_actual")) %>%
    mutate(
      error = actual_rush_yards - projected_rush_yards,
      abs_error = abs(error),
      pct_error = (error / actual_rush_yards) * 100
    )
  
  if (nrow(rb_results) > 0) {
    rb_mae <- mean(rb_results$abs_error, na.rm = TRUE)
    rb_mape <- mean(abs(rb_results$pct_error), na.rm = TRUE)
    
    # Calculate hit rate
    rb_results <- rb_results %>%
      mutate(hit = abs(pct_error) <= 25)  # 25% threshold for RBs (more variance)
    rb_hit_rate <- mean(rb_results$hit, na.rm = TRUE) * 100
    
    cat("🏃 RUNNING BACK PROJECTIONS:\n\n")
    cat("  RBs Tested:", nrow(rb_results), "\n")
    cat("  Mean Absolute Error:", round(rb_mae, 1), "yards\n")
    cat("  Mean Absolute % Error:", round(rb_mape, 1), "%\n")
    cat("  Hit Rate (within 25%):", round(rb_hit_rate, 1), "%\n\n")
  }
  
  # WR accuracy
  wr_results <- wr_predictions %>%
    inner_join(actual_wr, by = c("player_name" = "receiver_player_name", 
                                 "team" = "posteam"),
               suffix = c("_pred", "_actual")) %>%
    mutate(
      error = actual_rec_yards - projected_rec_yards,
      abs_error = abs(error),
      pct_error = (error / actual_rec_yards) * 100
    )
  
  if (nrow(wr_results) > 0) {
    wr_mae <- mean(wr_results$abs_error, na.rm = TRUE)
    wr_mape <- mean(abs(wr_results$pct_error[is.finite(wr_results$pct_error)]), na.rm = TRUE)
    
    # Calculate hit rate
    wr_results <- wr_results %>%
      mutate(hit = abs(pct_error) <= 25)  # 25% threshold for WRs
    wr_hit_rate <- mean(wr_results$hit, na.rm = TRUE) * 100
    
    cat("📡 WIDE RECEIVER PROJECTIONS:\n\n")
    cat("  WRs Tested:", nrow(wr_results), "\n")
    cat("  Mean Absolute Error:", round(wr_mae, 1), "yards\n")
    cat("  Mean Absolute % Error:", round(wr_mape, 1), "%\n")
    cat("  Hit Rate (within 25%):", round(wr_hit_rate, 1), "%\n\n")
  }
  
  # Combine player metrics
  if (exists("qb_mae") && exists("rb_mae") && exists("wr_mae")) {
    player_metrics <- data.frame(
      week = WEEK,
      qb_mae = qb_mae,
      qb_mape = qb_mape,
      qb_hit_rate = qb_hit_rate,
      rb_mae = rb_mae,
      rb_mape = rb_mape,
      rb_hit_rate = rb_hit_rate,
      wr_mae = wr_mae,
      wr_mape = wr_mape,
      wr_hit_rate = wr_hit_rate
    )
  }
}

# ============================================================================
# SAVE RESULTS TO HISTORY
# ============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("SAVING RESULTS TO HISTORY\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Combine all metrics for this week
week_summary <- game_metrics

if (has_betting && exists("betting_metrics")) {
  week_summary <- cbind(week_summary, betting_metrics[, -1])  # Exclude duplicate week column
}

if (has_props && exists("player_metrics")) {
  week_summary <- cbind(week_summary, player_metrics[, -1])
}

# Load existing history or create new
if (file.exists(RESULTS_DB)) {
  history <- read.csv(RESULTS_DB)
  # Remove this week if it already exists (updating)
  history <- history %>% filter(week != WEEK)
  # Add new results
  history <- bind_rows(history, week_summary)
} else {
  history <- week_summary
}

# Save updated history
tryCatch({
  write.csv(history, RESULTS_DB, row.names = FALSE)
  cat("✓ Results saved to:", RESULTS_DB, "\n\n")
}, error = function(e) {
  # If file is locked, save to a backup file
  backup_file <- file.path(BASE_DIR, paste0('model_performance_history_week', WEEK, '.csv'))
  write.csv(history, backup_file, row.names = FALSE)
  cat("⚠️  Could not save to main file (is it open in Excel?)\n")
  cat("✓ Results saved to backup:", backup_file, "\n")
  cat("   Please close the main file and merge manually\n\n")
})

# ============================================================================
# GENERATE VISUALIZATIONS
# ============================================================================

cat("Creating accuracy visualizations...\n")

viz_dir <- file.path(week_dir, 'accuracy_analysis')
if (!dir.exists(viz_dir)) {
  dir.create(viz_dir, recursive = TRUE)
}

# 1. Game prediction accuracy plot
if (nrow(game_results) > 0) {
  accuracy_plot <- game_results %>%
    mutate(game_short = paste(away_team.x, "vs", home_team.x)) %>%
    ggplot(aes(x = reorder(game_short, abs_error), y = error)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_col(aes(fill = correct_winner)) +
    scale_fill_manual(values = c("TRUE" = "#00AA00", "FALSE" = "#CC0000"),
                      labels = c("Winner Correct", "Winner Wrong")) +
    coord_flip() +
    labs(
      title = paste("Week", WEEK, "- Game Prediction Errors"),
      subtitle = "Positive = Model favored home too much | Negative = Model favored away too much",
      x = NULL,
      y = "Prediction Error (Actual - Predicted Margin)",
      fill = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(file.path(viz_dir, "game_accuracy.png"), 
         accuracy_plot, width = 10, height = 8, dpi = 300)
}

# 2. Historical accuracy trend (if multiple weeks)
if (file.exists(RESULTS_DB) && nrow(history) > 1) {
  trend_plot <- history %>%
    ggplot(aes(x = week, y = mean_abs_error)) +
    geom_line(color = "#0066CC", linewidth = 1.2) +
    geom_point(color = "#0066CC", size = 3) +
    geom_hline(yintercept = 10, linetype = "dashed", color = "red",
               alpha = 0.5) +
    labs(
      title = "Model Accuracy Over Time",
      subtitle = "Lower is better | Red line = 10 point threshold",
      x = "Week",
      y = "Mean Absolute Error (points)"
    ) +
    theme_minimal()
  
  ggsave(file.path(viz_dir, "accuracy_trend.png"), 
         trend_plot, width = 10, height = 6, dpi = 300)
  
  cat("✓ Trend visualization saved\n")
}

cat("✓ Visualizations saved to:", viz_dir, "\n\n")

# ============================================================================
# EXPORT DETAILED RESULTS
# ============================================================================

cat("Exporting detailed results...\n")

# Export this week's detailed results
write_xlsx(
  list(
    Summary = week_summary,
    Game_Results = game_results %>% 
      select(game, predicted_margin, actual_margin, error, correct_winner),
    Betting_Results = if (exists("betting_results")) betting_results else data.frame(),
    QB_Results = if (exists("qb_results")) qb_results else data.frame(),
    RB_Results = if (exists("rb_results")) rb_results else data.frame(),
    WR_Results = if (exists("wr_results")) wr_results else data.frame()
  ),
  file.path(viz_dir, paste0("week", WEEK, "_accuracy_report.xlsx"))
)

cat("✓ Detailed report exported\n\n")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  ACCURACY TESTING COMPLETE                                     ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("📁 FILES SAVED:\n")
cat("  - Model history:", RESULTS_DB, "\n")
cat("  - Visualizations:", viz_dir, "\n")
cat("  - Detailed report:", file.path(viz_dir, paste0("week", WEEK, "_accuracy_report.xlsx")), "\n\n")

cat("📈 OVERALL ASSESSMENT:\n\n")

if (game_metrics$mean_abs_error < 10) {
  cat("  ✅ EXCELLENT: Average error under 10 points\n")
} else if (game_metrics$mean_abs_error < 14) {
  cat("  ✓ GOOD: Average error 10-14 points (typical for NFL)\n")
} else {
  cat("  ⚠️  NEEDS IMPROVEMENT: Average error over 14 points\n")
}

if (game_metrics$winner_accuracy > 65) {
  cat("  ✅ EXCELLENT: Predicting winners at", round(game_metrics$winner_accuracy, 1), "%\n")
} else if (game_metrics$winner_accuracy > 55) {
  cat("  ✓ DECENT: Predicting winners at", round(game_metrics$winner_accuracy, 1), "% (above 50%)\n")
} else {
  cat("  ⚠️  POOR: Winner accuracy below 55%\n")
}

if (has_betting && exists("betting_metrics")) {
  if (betting_metrics$win_rate > 55) {
    cat("  ✅ PROFITABLE: Betting win rate at", round(betting_metrics$win_rate, 1), "% (above 52.4% break-even)\n")
  } else if (betting_metrics$win_rate > 50) {
    cat("  ⚠️  MARGINAL: Betting win rate at", round(betting_metrics$win_rate, 1), "% (close to break-even)\n")
  } else {
    cat("  ❌ UNPROFITABLE: Betting win rate below 50%\n")
  }
}

cat("\n✓ Done! Run this script again after Week", WEEK + 1, "to continue tracking.\n\n")