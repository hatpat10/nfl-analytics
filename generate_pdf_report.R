# ============================================================================
# NFL ANALYTICS - PDF REPORT GENERATOR
# Purpose: Create shareable PDF report of your NFL analysis
# Save as: generate_pdf_report.R
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
OUTPUT_NAME <- paste0("NFL_Analytics_Week", WEEK, "_Report.pdf")

# ============================================================================
# LOAD PACKAGES
# ============================================================================

cat("Loading packages...\n")

required_packages <- c("dplyr", "ggplot2", "gridExtra", "grid", "knitr", "kableExtra")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

cat("✓ Packages loaded\n\n")

# ============================================================================
# LOAD DATA
# ============================================================================

cat("Loading data...\n")

week_path <- file.path(BASE_DIR, paste0('week', WEEK))

# Load matchup data
matchup_file <- file.path(week_path, "matchup_analysis", "matchup_summary.csv")
games <- read.csv(matchup_file)

# Load odds data
odds_file <- file.path(week_path, "odds_analysis", "all_games_analysis.csv")
odds <- read.csv(odds_file)

# Load player projections
qb <- read.csv(file.path(week_path, "player_props", "qb_projections.csv"))
rb <- read.csv(file.path(week_path, "player_props", "rb_projections.csv"))
wr <- read.csv(file.path(week_path, "player_props", "wr_projections.csv"))

cat("✓ Data loaded\n\n")

# ============================================================================
# CREATE VISUALIZATIONS
# ============================================================================

cat("Creating visualizations...\n")

# 1. Model vs Vegas Chart
spreads_plot <- odds %>%
  arrange(desc(abs(edge))) %>%
  head(10) %>%
  mutate(
    matchup_key = reorder(matchup_key, edge),
    has_value = abs(edge) >= 2.5
  ) %>%
  ggplot(aes(x = matchup_key, y = edge, fill = has_value)) +
  geom_col(color = "white", linewidth = 0.8) +
  geom_hline(yintercept = c(-2.5, 2.5), linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, color = "black") +
  scale_fill_manual(
    values = c("TRUE" = "#00AA00", "FALSE" = "#CCCCCC"),
    labels = c("TRUE" = "Value Bet", "FALSE" = "No Value")
  ) +
  coord_flip() +
  labs(
    title = paste("Week", WEEK, "- Model vs Vegas Spreads"),
    subtitle = "Positive = Bet Home Team | Negative = Bet Away Team",
    x = NULL,
    y = "Edge (Model Line - Vegas Line)",
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "bottom"
  )

# 2. QB Projections
qb_plot <- qb %>%
  head(12) %>%
  mutate(player_name = reorder(player_name, projected_pass_yards)) %>%
  ggplot(aes(x = player_name, y = projected_pass_yards)) +
  geom_col(fill = "#667eea", color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(projected_pass_yards)), 
            hjust = -0.2, size = 3, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Top QB Projections - Passing Yards",
    x = NULL,
    y = "Projected Passing Yards"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.text.y = element_text(face = "bold", size = 9),
    panel.grid.major.y = element_blank()
  )

# 3. RB Projections
rb_plot <- rb %>%
  head(12) %>%
  mutate(player_name = reorder(player_name, projected_rush_yards)) %>%
  ggplot(aes(x = player_name, y = projected_rush_yards)) +
  geom_col(fill = "#e74c3c", color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(projected_rush_yards)), 
            hjust = -0.2, size = 3, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Top RB Projections - Rushing Yards",
    x = NULL,
    y = "Projected Rushing Yards"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.text.y = element_text(face = "bold", size = 9),
    panel.grid.major.y = element_blank()
  )

# 4. WR Projections
wr_plot <- wr %>%
  head(12) %>%
  mutate(player_name = reorder(player_name, projected_rec_yards)) %>%
  ggplot(aes(x = player_name, y = projected_rec_yards)) +
  geom_col(fill = "#27ae60", color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(projected_rec_yards)), 
            hjust = -0.2, size = 3, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Top WR Projections - Receiving Yards",
    x = NULL,
    y = "Projected Receiving Yards"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.text.y = element_text(face = "bold", size = 9),
    panel.grid.major.y = element_blank()
  )

cat("✓ Visualizations created\n\n")

# ============================================================================
# CREATE PDF REPORT
# ============================================================================

cat("Generating PDF report...\n")

output_path <- file.path(week_path, OUTPUT_NAME)

pdf(output_path, width = 11, height = 8.5)

# Page 1: Title and Overview
grid.newpage()
grid.text("NFL ANALYTICS REPORT", 
          x = 0.5, y = 0.92, 
          gp = gpar(fontsize = 26, fontface = "bold", col = "#1e3c72"))
grid.text(paste("Week", WEEK, "- Season", SEASON), 
          x = 0.5, y = 0.87, 
          gp = gpar(fontsize = 15, col = "#6c757d"))
grid.text("Data-Driven Football Predictions & Analysis", 
          x = 0.5, y = 0.83, 
          gp = gpar(fontsize = 13, col = "#6c757d"))

# Summary metrics
grid.text("SUMMARY STATISTICS", 
          x = 0.5, y = 0.72, 
          gp = gpar(fontsize = 16, fontface = "bold", col = "#1e3c72"))

summary_text <- paste0(
  "Games Analyzed: ", nrow(games), "\n",
  "Quarterbacks Projected: ", nrow(qb), "\n",
  "Running Backs Projected: ", nrow(rb), "\n",
  "Wide Receivers Projected: ", nrow(wr), "\n",
  "Value Opportunities Found: ", sum(odds$ev_tier %in% c("🔥 ELITE", "⭐ STRONG", "✓ GOOD"))
)

grid.text(summary_text, 
          x = 0.5, y = 0.58, 
          gp = gpar(fontsize = 11, lineheight = 1.4))

# Methodology overview
grid.text("METHODOLOGY", 
          x = 0.5, y = 0.38, 
          gp = gpar(fontsize = 15, fontface = "bold", col = "#1e3c72"))

method_text <- paste0(
  "This analysis uses play-by-play data from all ", SEASON, " season games through Week ", WEEK-1, ".\n\n",
  "Game Predictions: Based on EPA (Expected Points Added) comparing offensive\n",
  "and defensive strength with matchup-specific adjustments.\n\n",
  "Player Projections: 60% recent form (3-week rolling average) + 40% season\n",
  "average, adjusted for opponent strength and home field advantage.\n\n",
  "Value Analysis: Identifies games where the model disagrees with Vegas by\n",
  "2.5+ points, indicating potential analytical edges."
)

grid.text(method_text, 
          x = 0.5, y = 0.18, 
          gp = gpar(fontsize = 9.5, lineheight = 1.3),
          just = "center")

# Page 2: Model vs Vegas
grid.newpage()
print(spreads_plot)

# Page 3: QB Projections
grid.newpage()
print(qb_plot)

# Page 4: RB Projections
grid.newpage()
print(rb_plot)

# Page 5: WR Projections
grid.newpage()
print(wr_plot)

# Page 6: Value Opportunities Table
grid.newpage()
grid.text("VALUE OPPORTUNITIES", 
          x = 0.5, y = 0.96, 
          gp = gpar(fontsize = 18, fontface = "bold", col = "#1e3c72"))

grid.text("Games where the model identifies significant disagreement with Vegas lines", 
          x = 0.5, y = 0.92, 
          gp = gpar(fontsize = 10, col = "#6c757d"))

# Create table
value_bets <- odds %>%
  filter(ev_tier %in% c("🔥 ELITE", "⭐ STRONG", "✓ GOOD")) %>%
  arrange(desc(abs(edge))) %>%
  head(10) %>%
  select(matchup_key, ev_tier, edge, confidence)

if (nrow(value_bets) > 0) {
  # Format data for better display
  value_bets$edge <- round(value_bets$edge, 1)
  names(value_bets) <- c("Game", "Value Tier", "Edge (pts)", "Confidence")
  
  pushViewport(viewport(x = 0.5, y = 0.5, width = 0.9, height = 0.8))
  grid.table(value_bets, rows = NULL, 
             theme = ttheme_default(base_size = 9))
  popViewport()
} else {
  grid.text("No significant value opportunities found this week.", 
            x = 0.5, y = 0.5, 
            gp = gpar(fontsize = 11))
}

# Page 7: How to Interpret
grid.newpage()
grid.text("HOW TO INTERPRET THIS REPORT", 
          x = 0.5, y = 0.96, 
          gp = gpar(fontsize = 18, fontface = "bold", col = "#1e3c72"))

interpretation_text <- paste0(
  "MODEL VS VEGAS CHART\n",
  "• Positive bars = Model favors home team more than Vegas\n",
  "• Negative bars = Model favors away team more than Vegas\n",
  "• Green bars = Edge exceeds 2.5 points (potential value)\n",
  "• Larger bars = Bigger disagreement between model and Vegas\n\n",
  
  "PLAYER PROJECTIONS\n",
  "• Based on recent performance trends and opponent matchups\n",
  "• Higher projections don't guarantee better performance\n",
  "• Injuries, weather, and game script can impact outcomes\n",
  "• Use as one data point among many for analysis\n\n",
  
  "VALUE OPPORTUNITIES\n",
  "• Elite: 5+ point edge (model strongly disagrees with Vegas)\n",
  "• Strong: 3.5-5 point edge (significant disagreement)\n",
  "• Good: 2.5-3.5 point edge (moderate disagreement)\n\n",
  
  "IMPORTANT LIMITATIONS\n",
  "• Cannot predict injuries, weather, or coaching decisions\n",
  "• Football has inherent randomness\n",
  "• Model accuracy should be tested over multiple weeks\n",
  "• For research and analytical demonstration purposes"
)

grid.text(interpretation_text, 
          x = 0.5, y = 0.50, 
          gp = gpar(fontsize = 9, lineheight = 1.35),
          just = "center")

dev.off()

cat("✓ PDF report generated!\n\n")
cat("══════════════════════════════════════════════════════\n")
cat("Report saved to:", output_path, "\n")
cat("══════════════════════════════════════════════════════\n\n")
cat("You can now:\n")
cat("1. Email this PDF to anyone\n")
cat("2. Upload to Google Drive/Dropbox and share the link\n")
cat("3. Print it for presentations\n")
cat("4. Include in a portfolio\n\n")