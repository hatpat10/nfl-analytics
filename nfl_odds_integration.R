# ============================================================================
# NFL BETTING MODEL - SPORTSDATA.IO ODDS INTEGRATION
# ============================================================================
# Purpose: Fetch real-time odds and compare against model predictions
# Run AFTER nfl_master_pipeline.R completes
# ============================================================================

# Clear environment
rm(list = ls())
gc()

# ============================================================================
# CONFIGURATION
# ============================================================================

SPORTSDATA_API_KEY <- "39cfe83be78c4bcca9afb2eb9d6cf7ca"
SEASON <- 2025
WEEK <- 5
BASE_DIR <- 'C:/Users/Patsc/Documents/nfl'
MIN_EDGE_THRESHOLD <- 1.5

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║     NFL BETTING MODEL - SPORTSDATA.IO INTEGRATION              ║\n")
cat("║     Week:", WEEK, "                                                    ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# ============================================================================
# LOAD PACKAGES
# ============================================================================

cat("Loading required packages...\n")

required_packages <- c("httr", "jsonlite", "dplyr", "tidyr", "writexl", "ggplot2")

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

cat("Loading model predictions...\n")

matchup_dir <- file.path(BASE_DIR, paste0('week', WEEK), 'matchup_analysis')

if (!file.exists(file.path(matchup_dir, "betting_recommendations.csv"))) {
  stop("\n❌ ERROR: Run nfl_master_pipeline.R first!\n")
}

model_predictions <- read.csv(file.path(matchup_dir, "betting_recommendations.csv"))
matchup_summary <- read.csv(file.path(matchup_dir, "matchup_summary.csv"))

cat("✓ Loaded", nrow(model_predictions), "predictions\n\n")

# ============================================================================
# FETCH ODDS FROM SPORTSDATA.IO
# ============================================================================

cat("Fetching odds from SportsData.io...\n")

fetch_sportsdata_odds <- function(api_key, season, week) {
  
  # SportsData.io endpoint for odds by week
  base_url <- "https://api.sportsdata.io/v3/nfl/odds/json/GameOddsByWeek"
  url <- paste0(base_url, "/", season, "/", week)
  
  response <- httr::GET(
    url = url,
    httr::add_headers("Ocp-Apim-Subscription-Key" = api_key)
  )
  
  if (httr::status_code(response) != 200) {
    stop("API Error: Status ", httr::status_code(response), 
         "\nMessage: ", httr::content(response, "text"))
  }
  
  # Parse response
  odds_data <- httr::content(response, "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)
  
  return(odds_data)
}

tryCatch({
  odds_raw <- fetch_sportsdata_odds(SPORTSDATA_API_KEY, SEASON, WEEK)
  
  if (is.null(odds_raw) || length(odds_raw) == 0 || nrow(odds_raw) == 0) {
    stop("No data returned from API for Week ", WEEK)
  }
  
  cat("✓ Fetched odds data for", nrow(odds_raw), "games\n\n")
  
}, error = function(e) {
  stop("\n❌ Error fetching odds:\n", e$message, 
       "\n\nTroubleshooting:\n",
       "  1. Check your API key is valid\n",
       "  2. Verify you have access to NFL Odds data (may require paid subscription)\n",
       "  3. Check if Week ", WEEK, " data is available yet\n",
       "  4. Try: https://api.sportsdata.io/v3/nfl/scores/json/Scores/", SEASON, "/", WEEK, "\n")
})

# ============================================================================
# PROCESS SPORTSDATA.IO ODDS
# ============================================================================

cat("Processing odds data...\n")

# SportsData.io returns nested PregameOdds - we need to extract spreads
process_game_odds <- function(game_row) {
  
  # Basic game info - use correct field names from SportsData.io
  game_info <- list(
    game_key = game_row$GameId,
    home_team = game_row$HomeTeamName,  # Changed from HomeTeam
    away_team = game_row$AwayTeamName,  # Changed from AwayTeam
    game_date = game_row$DateTime,
    status = game_row$Status
  )
  
  # Extract odds from PregameOdds
  if (is.null(game_row$PregameOdds) || length(game_row$PregameOdds) == 0) {
    return(NULL)
  }
  
  pregame_odds <- game_row$PregameOdds
  if (!is.data.frame(pregame_odds)) {
    pregame_odds <- as.data.frame(pregame_odds)
  }
  
  # Find consensus or major sportsbook odds
  # Priority: Consensus > DraftKings > FanDuel > First available (including Scrambled)
  priority_books <- c("Consensus", "DraftKings", "FanDuel", "BetMGM", "Caesars")
  
  best_odds <- NULL
  for (book in priority_books) {
    book_odds <- pregame_odds[pregame_odds$Sportsbook == book, ]
    if (nrow(book_odds) > 0) {
      best_odds <- book_odds[1, ]
      break
    }
  }
  
  # If no priority book found, use first available (even if "Scrambled")
  # Note: "Scrambled" means free tier - actual sportsbook hidden but odds are real
  if (is.null(best_odds) && nrow(pregame_odds) > 0) {
    best_odds <- pregame_odds[1, ]
    # If sportsbook is "Scrambled", label it clearly
    if (best_odds$Sportsbook == "Scrambled") {
      best_odds$Sportsbook <- "Market Average (Free Tier)"
    }
  }
  
  if (is.null(best_odds)) {
    return(NULL)
  }
  
  # Extract spread data
  game_info$sportsbook <- best_odds$Sportsbook
  game_info$home_spread <- as.numeric(best_odds$HomePointSpread)
  game_info$away_spread <- as.numeric(best_odds$AwayPointSpread)
  game_info$home_spread_odds <- as.numeric(best_odds$HomePointSpreadPayout)
  game_info$away_spread_odds <- as.numeric(best_odds$AwayPointSpreadPayout)
  game_info$over_under <- as.numeric(best_odds$OverUnder)
  
  # Validate we got actual numeric values
  if (is.na(game_info$home_spread) || is.na(game_info$away_spread)) {
    return(NULL)
  }
  
  return(game_info)
}

# Process all games
all_games <- list()
failed_games <- 0

for (i in 1:nrow(odds_raw)) {
  game_odds <- process_game_odds(odds_raw[i, ])
  if (!is.null(game_odds)) {
    all_games[[length(all_games) + 1]] <- game_odds
  } else {
    failed_games <- failed_games + 1
  }
}

cat("Processed", nrow(odds_raw), "games -", length(all_games), "with odds,", failed_games, "without odds\n")

if (length(all_games) == 0) {
  cat("\n⚠️  WARNING: No odds found in API response.\n")
  cat("   Debugging raw data...\n\n")
  
  # Show structure of first game
  if (nrow(odds_raw) > 0) {
    cat("   First game in API response:\n")
    first_game <- odds_raw[1, ]
    cat("   - GameKey:", first_game$GameKey, "\n")
    cat("   - Home:", first_game$HomeTeam, "\n")
    cat("   - Away:", first_game$AwayTeam, "\n")
    cat("   - Date:", first_game$DateTime, "\n")
    cat("   - Status:", first_game$Status, "\n")
    
    # Check PregameOdds structure
    cat("\n   Checking PregameOdds field:\n")
    if ("PregameOdds" %in% names(first_game)) {
      pregame <- first_game$PregameOdds
      if (!is.null(pregame) && length(pregame) > 0) {
        cat("   - PregameOdds exists with", 
            if(is.list(pregame)) length(pregame) else if(is.data.frame(pregame)) nrow(pregame) else "unknown",
            "entries\n")
        cat("   - Type:", class(pregame), "\n")
        
        if (is.data.frame(pregame) && nrow(pregame) > 0) {
          cat("   - Available columns:", paste(names(pregame), collapse = ", "), "\n")
          cat("   - First sportsbook:", pregame$Sportsbook[1], "\n")
          cat("   - Sample spread:", pregame$HomePointSpread[1], "\n")
        } else if (is.list(pregame)) {
          cat("   - First entry type:", class(pregame[[1]]), "\n")
          if (is.data.frame(pregame[[1]])) {
            cat("   - Columns:", paste(names(pregame[[1]]), collapse = ", "), "\n")
          }
        }
      } else {
        cat("   - PregameOdds is NULL or empty\n")
        cat("   - This means odds haven't been posted yet for Week", WEEK, "\n")
      }
    } else {
      cat("   - PregameOdds field doesn't exist\n")
      cat("   - Available fields:", paste(names(first_game), collapse = ", "), "\n")
    }
  }
  
  stop("\n❌ No spread data available. Week ", WEEK, " odds may not be posted yet.\n",
       "   Try checking Week 4 or an earlier completed week to test the script.\n")
}

# Convert to dataframe - with safe handling
odds_df <- do.call(rbind, lapply(all_games, function(x) {
  data.frame(
    game_key = if (is.null(x$game_key)) NA else x$game_key,
    home_team = if (is.null(x$home_team)) NA else x$home_team,
    away_team = if (is.null(x$away_team)) NA else x$away_team,
    sportsbook = if (is.null(x$sportsbook)) "Unknown" else x$sportsbook,
    home_spread = if (is.null(x$home_spread) || length(x$home_spread) == 0) NA else as.numeric(x$home_spread),
    away_spread = if (is.null(x$away_spread) || length(x$away_spread) == 0) NA else as.numeric(x$away_spread),
    home_spread_odds = if (is.null(x$home_spread_odds) || length(x$home_spread_odds) == 0) NA else as.numeric(x$home_spread_odds),
    away_spread_odds = if (is.null(x$away_spread_odds) || length(x$away_spread_odds) == 0) NA else as.numeric(x$away_spread_odds),
    over_under = if (is.null(x$over_under) || length(x$over_under) == 0) NA else as.numeric(x$over_under),
    stringsAsFactors = FALSE
  )
}))

# Remove rows with missing critical data
odds_df <- odds_df %>%
  filter(!is.na(home_team), !is.na(away_team), !is.na(home_spread))

cat("✓ Extracted spreads for", nrow(odds_df), "games from", 
    length(unique(odds_df$sportsbook)), "sportsbook(s)\n")
cat("   Using:", paste(unique(odds_df$sportsbook), collapse = ", "), "\n\n")

# ============================================================================
# STANDARDIZE TEAM NAMES
# ============================================================================

cat("Matching team names...\n")

# SportsData.io uses team abbreviations, which should match nflverse
# The API returns: AwayTeamName and HomeTeamName with abbreviations
odds_df <- odds_df %>%
  mutate(
    matchup_key = paste(away_team, "at", home_team)
  )

cat("✓ Team names ready for matching\n")
cat("   Sample matchups from odds:\n")
for (i in 1:min(3, nrow(odds_df))) {
  cat("   -", odds_df$matchup_key[i], "\n")
}
cat("\n")

# ============================================================================
# MERGE WITH MODEL PREDICTIONS
# ============================================================================

cat("Merging with model predictions...\n")

model_predictions <- model_predictions %>%
  mutate(matchup_key = game)

betting_analysis <- model_predictions %>%
  inner_join(odds_df, by = "matchup_key")

if (nrow(betting_analysis) == 0) {
  cat("\n⚠️  Could not match odds with model predictions.\n")
  cat("   Model predictions:\n")
  print(head(model_predictions$matchup_key))
  cat("\n   Odds matchups:\n")
  print(head(odds_df$matchup_key))
  stop("\n❌ Team names don't match. Check abbreviations.\n")
}

# Check what columns we actually have after the join
cat("✓ Matched", nrow(betting_analysis), "games\n")
cat("   Available columns:", paste(names(betting_analysis), collapse = ", "), "\n\n")

betting_analysis <- betting_analysis %>%
  mutate(
    vegas_line = home_spread,
    model_line = projected_margin,
    edge = model_line - vegas_line,
    abs_edge = abs(edge),
    
    # Use the correct column names from the join
    bet_side = case_when(
      edge >= MIN_EDGE_THRESHOLD ~ home_team.y,  # from odds_df
      edge <= -MIN_EDGE_THRESHOLD ~ away_team.y,  # from odds_df
      TRUE ~ "PASS"
    ),
    
    bet_spread = case_when(
      edge >= MIN_EDGE_THRESHOLD ~ home_spread,
      edge <= -MIN_EDGE_THRESHOLD ~ away_spread,
      TRUE ~ NA_real_
    ),
    
    bet_odds = case_when(
      edge >= MIN_EDGE_THRESHOLD ~ home_spread_odds,
      edge <= -MIN_EDGE_THRESHOLD ~ away_spread_odds,
      TRUE ~ NA_real_
    ),
    
    ev_tier = case_when(
      abs_edge >= 5 ~ "🔥 ELITE",
      abs_edge >= 3.5 ~ "⭐ STRONG",
      abs_edge >= MIN_EDGE_THRESHOLD ~ "✓ GOOD",
      TRUE ~ "❌ PASS"
    ),
    
    bet_recommendation = if_else(
      bet_side != "PASS",
      paste0("BET ", bet_side, " ", sprintf("%+.1f", bet_spread), " (", sprintf("%+.0f", bet_odds), ")"),
      "NO BET"
    )
  ) %>%
  # Clean up the column names for easier use
  rename(
    away_team = away_team.y,
    home_team = home_team.y
  )

# ============================================================================
# FILTER PROFITABLE BETS
# ============================================================================

profitable_bets <- betting_analysis %>%
  filter(ev_tier != "❌ PASS") %>%
  arrange(desc(abs_edge)) %>%
  select(
    matchup_key, ev_tier, confidence,
    bet_recommendation, edge, vegas_line, model_line,
    sportsbook, home_team, away_team, over_under
  )

cat("✓ Found", nrow(profitable_bets), "profitable bets\n\n")

# ============================================================================
# CREATE SUMMARY
# ============================================================================

summary_stats <- data.frame(
  Metric = c(
    "Total Games Analyzed",
    "Profitable Bets Found",
    "Elite Value Bets",
    "Strong Value Bets",
    "Good Value Bets",
    "Average Edge (Profitable)",
    "Largest Edge"
  ),
  Value = c(
    nrow(betting_analysis),
    nrow(profitable_bets),
    sum(betting_analysis$ev_tier == "🔥 ELITE"),
    sum(betting_analysis$ev_tier == "⭐ STRONG"),
    sum(betting_analysis$ev_tier == "✓ GOOD"),
    if(nrow(profitable_bets) > 0) round(mean(profitable_bets$abs_edge, na.rm = TRUE), 2) else 0,
    round(max(betting_analysis$abs_edge), 2)
  )
)

# ============================================================================
# EXPORT RESULTS
# ============================================================================

cat("Exporting results...\n")

odds_output_dir <- file.path(BASE_DIR, paste0('week', WEEK), 'odds_analysis')
if (!dir.exists(odds_output_dir)) {
  dir.create(odds_output_dir, recursive = TRUE)
}

# Excel file
write_xlsx(
  list(
    Summary = summary_stats,
    Profitable_Bets = profitable_bets,
    All_Games = betting_analysis %>% 
      select(matchup_key, ev_tier, edge, vegas_line, model_line, 
             bet_recommendation, confidence, sportsbook, over_under) %>%
      arrange(desc(abs(edge)))
  ),
  file.path(odds_output_dir, paste0("betting_picks_week", WEEK, ".xlsx"))
)

# CSV files
write.csv(profitable_bets, 
          file.path(odds_output_dir, "profitable_bets.csv"), 
          row.names = FALSE)

write.csv(betting_analysis %>% 
            select(matchup_key, ev_tier, edge, vegas_line, model_line, 
                   bet_recommendation, confidence, sportsbook) %>%
            arrange(desc(abs(edge))), 
          file.path(odds_output_dir, "all_games_analysis.csv"), 
          row.names = FALSE)

cat("✓ Files exported\n\n")

# ============================================================================
# CREATE VISUALIZATION
# ============================================================================

cat("Creating visualization...\n")

if (nrow(betting_analysis) > 0) {
  edge_plot <- betting_analysis %>%
    mutate(
      game_label = paste0(away_team, " @ ", home_team),
      has_value = abs_edge >= MIN_EDGE_THRESHOLD
    ) %>%
    arrange(desc(abs(edge))) %>%
    head(min(12, nrow(betting_analysis))) %>%
    ggplot(aes(x = reorder(game_label, edge), y = edge, fill = has_value)) +
    geom_col() +
    geom_hline(yintercept = c(-MIN_EDGE_THRESHOLD, MIN_EDGE_THRESHOLD), 
               linetype = "dashed", color = "red", linewidth = 1) +
    geom_hline(yintercept = 0, color = "black") +
    scale_fill_manual(
      values = c("TRUE" = "#00AA00", "FALSE" = "#CCCCCC"),
      labels = c("TRUE" = "Value Bet", "FALSE" = "No Value")
    ) +
    coord_flip() +
    labs(
      title = paste("Week", WEEK, "- Model vs Vegas Spreads"),
      subtitle = paste("Positive = Bet Home | Negative = Bet Away | Min Edge:", MIN_EDGE_THRESHOLD, "pts"),
      x = NULL,
      y = "Edge (Model Line - Vegas Line)",
      fill = NULL,
      caption = paste("Source:", unique(betting_analysis$sportsbook)[1])
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.caption = element_text(size = 9, hjust = 1),
      legend.position = "bottom"
    )
  
  ggsave(file.path(odds_output_dir, "betting_edges.png"), 
         edge_plot, width = 12, height = 8, dpi = 300)
  
  cat("✓ Visualization saved\n\n")
}

# ============================================================================
# PRINT RESULTS
# ============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  BETTING ANALYSIS COMPLETE                                     ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("📊 SUMMARY:\n")
print(summary_stats, row.names = FALSE)

cat("\n\n🎯 TOP BETTING PICKS:\n")
cat("════════════════════════════════════════════════════════════════\n\n")

if (nrow(profitable_bets) > 0) {
  for (i in 1:min(5, nrow(profitable_bets))) {
    bet <- profitable_bets[i, ]
    cat(sprintf("%s  %s\n", bet$ev_tier, bet$matchup_key))
    cat(sprintf("   %s\n", bet$bet_recommendation))
    cat(sprintf("   Edge: %.1f pts | Vegas: %.1f | Model: %.1f\n", 
                bet$edge, bet$vegas_line, bet$model_line))
    cat(sprintf("   Model Confidence: %s | Sportsbook: %s\n", 
                bet$confidence, bet$sportsbook))
    cat(sprintf("   Over/Under: %.1f\n\n", bet$over_under))
  }
} else {
  cat("No profitable bets found this week.\n")
  cat("The model and Vegas are in close agreement.\n\n")
}

cat("════════════════════════════════════════════════════════════════\n")
cat("\n📁 Files saved to:", odds_output_dir, "\n")
cat("   - betting_picks_week", WEEK, ".xlsx\n", sep = "")
cat("   - profitable_bets.csv\n")
cat("   - all_games_analysis.csv\n")
cat("   - betting_edges.png\n")

cat("\n✓ Done! Good luck! 🏈💰\n\n")
