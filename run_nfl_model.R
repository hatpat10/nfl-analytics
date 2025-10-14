# ============================================================================
# NFL MODEL - WEEK SWITCHER & PIPELINE RUNNER
# Purpose: Easily change weeks and run all scripts at once
# ============================================================================

# ============================================================================
# CONFIGURATION - CHANGE THESE!
# ============================================================================

TARGET_WEEK <- 7           # Week to project
SEASON <- 2025
BASE_DIR <- "C:/Users/Patsc/Documents/nfl"

# What to run (set to FALSE to skip)
RUN_MASTER_PIPELINE <- TRUE    # Team analysis & matchups
RUN_ODDS_INTEGRATION <- TRUE   # Compare to Vegas
RUN_PLAYER_PROPS <- TRUE       # Player projections with headshots
LAUNCH_DASHBOARD <- TRUE       # Open Shiny app when done

# ============================================================================
# VALIDATION
# ============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║                  NFL MODEL - PIPELINE RUNNER                   ║\n")
cat("║                                                                ║\n")
cat(sprintf("║  Season: %d | Week: %d                                     ║\n", SEASON, TARGET_WEEK))
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Check week is valid
if (TARGET_WEEK < 1 || TARGET_WEEK > 18) {
  stop("❌ Week must be between 1 and 18!")
}

# Check directory exists
if (!dir.exists(BASE_DIR)) {
  stop("❌ Base directory doesn't exist: ", BASE_DIR)
}

cat("✓ Configuration valid\n\n")

# ============================================================================
# SET ENVIRONMENT VARIABLES
# ============================================================================

cat("Setting environment variables...\n")
Sys.setenv(NFL_MODEL_WEEK = TARGET_WEEK)
Sys.setenv(NFL_MODEL_SEASON = SEASON)
Sys.setenv(NFL_MODEL_BASE_DIR = BASE_DIR)
cat("✓ Environment configured\n\n")

# ============================================================================
# EXECUTE PIPELINE
# ============================================================================

start_time_total <- Sys.time()
results <- list()

# ============================================================================
# STEP 1: MASTER PIPELINE
# ============================================================================

if (RUN_MASTER_PIPELINE) {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║  STEP 1: MASTER PIPELINE                                       ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")
  
  start_time <- Sys.time()
  
  tryCatch({
    .GlobalEnv$SEASON <- SEASON
    .GlobalEnv$WEEK <- TARGET_WEEK
    .GlobalEnv$BASE_DIR <- BASE_DIR
    
    source("nfl_master_pipeline.R", local = FALSE)
    
    elapsed <- difftime(Sys.time(), start_time, units = "secs")
    results$master_pipeline <- list(success = TRUE, time = elapsed)
    cat("\n✓ Master Pipeline complete in", round(elapsed, 1), "seconds\n")
    
  }, error = function(e) {
    cat("\n❌ Master Pipeline failed:", e$message, "\n")
    results$master_pipeline <- list(success = FALSE, error = e$message)
  })
}

# ============================================================================
# STEP 2: ODDS INTEGRATION
# ============================================================================

if (RUN_ODDS_INTEGRATION) {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║  STEP 2: ODDS INTEGRATION                                      ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")
  
  start_time <- Sys.time()
  
  tryCatch({
    .GlobalEnv$SEASON <- SEASON
    .GlobalEnv$WEEK <- TARGET_WEEK
    .GlobalEnv$BASE_DIR <- BASE_DIR
    
    source("nfl_odds_integration.R", local = FALSE)
    
    elapsed <- difftime(Sys.time(), start_time, units = "secs")
    results$odds_integration <- list(success = TRUE, time = elapsed)
    cat("\n✓ Odds Integration complete in", round(elapsed, 1), "seconds\n")
    
  }, error = function(e) {
    cat("\n❌ Odds Integration failed:", e$message, "\n")
    cat("   (This is normal if odds aren't available yet for Week", TARGET_WEEK, ")\n")
    results$odds_integration <- list(success = FALSE, error = e$message)
  })
}

# ============================================================================
# STEP 3: PLAYER PROPS
# ============================================================================

if (RUN_PLAYER_PROPS) {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║  STEP 3: PLAYER PROPS WITH HEADSHOTS                           ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")
  
  start_time <- Sys.time()
  
  tryCatch({
    .GlobalEnv$SEASON <- SEASON
    .GlobalEnv$WEEK <- TARGET_WEEK
    .GlobalEnv$BASE_DIR <- BASE_DIR
    
    source("nfl_props_headshots.R", local = FALSE)
    
    elapsed <- difftime(Sys.time(), start_time, units = "secs")
    results$player_props <- list(success = TRUE, time = elapsed)
    cat("\n✓ Player Props complete in", round(elapsed, 1), "seconds\n")
    
  }, error = function(e) {
    cat("\n❌ Player Props failed:", e$message, "\n")
    results$player_props <- list(success = FALSE, error = e$message)
  })
}

# ============================================================================
# FINAL SUMMARY
# ============================================================================

total_time <- difftime(Sys.time(), start_time_total, units = "mins")

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PIPELINE COMPLETE                                             ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("📊 EXECUTION SUMMARY:\n\n")

for (step_name in names(results)) {
  step <- results[[step_name]]
  status <- if (step$success) "✓ SUCCESS" else "❌ FAILED"
  time_str <- if (step$success) paste0(" (", round(step$time, 1), "s)") else ""
  
  cat("  ", step_name, ": ", status, time_str, "\n")
}

cat("\n⏱️  Total time:", round(total_time, 2), "minutes\n\n")

# ============================================================================
# LAUNCH DASHBOARD
# ============================================================================

if (LAUNCH_DASHBOARD) {
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║  LAUNCHING DASHBOARD                                           ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")
  
  cat("Opening Shiny dashboard...\n")
  cat("  Select Week", TARGET_WEEK, "and click 'Load Data'\n\n")
  
  if (file.exists("nfl_dashboard_shiny.R")) {
    library(shiny)
    runApp("nfl_dashboard_shiny.R")
  } else {
    cat("❌ Dashboard file not found: nfl_dashboard_shiny.R\n")
  }
}

cat("\n✓ All done! 🏈\n\n")