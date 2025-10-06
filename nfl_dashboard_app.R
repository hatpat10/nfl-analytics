# ============================================================================
# NFL ANALYTICS DASHBOARD - R SHINY APP
# Purpose: Interactive visualization of NFL prediction models
# Save as: nfl_dashboard_app.R
# Run with: shiny::runApp("nfl_dashboard_app.R")
# ============================================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)

# ============================================================================
# UI DEFINITION
# ============================================================================

ui <- fluidPage(
  theme = bslib::bs_theme(
    bootswatch = "cosmo",
    primary = "#667eea",
    base_font = bslib::font_google("Inter")
  ),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      .info-box {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 20px;
      }
      .metric-box {
        text-align: center;
        padding: 20px;
        background: #f8f9fa;
        border-radius: 10px;
        margin: 10px;
      }
      .metric-value {
        font-size: 2.5em;
        font-weight: bold;
        color: #667eea;
      }
      .metric-label {
        color: #6c757d;
        margin-top: 5px;
      }
      .player-card {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 10px;
        text-align: center;
        margin: 10px;
      }
    "))
  ),
  
  # Header
  div(
    style = "background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%); color: white; padding: 40px; text-align: center; margin-bottom: 20px;",
    h1("NFL Analytics Dashboard", style = "font-size: 2.5em; margin-bottom: 10px;"),
    p("Data-Driven Football Analysis & Predictions", style = "font-size: 1.2em; opacity: 0.9;")
  ),
  
  # Controls
  fluidRow(
    column(3,
           selectInput("week", "Select Week:", 
                       choices = 1:18, selected = 5)
    ),
    column(6,
           textInput("base_dir", "Data Directory:", 
                     value = "C:/Users/Patsc/Documents/nfl", 
                     width = "100%")
    ),
    column(3,
           actionButton("load_data", "Load Data", 
                        class = "btn-primary btn-lg",
                        style = "margin-top: 25px; width: 100%;")
    )
  ),
  
  hr(),
  
  # Main content with tabs
  tabsetPanel(
    id = "main_tabs",
    type = "pills",
    
    # Overview Tab
    tabPanel(
      "Overview",
      br(),
      div(
        class = "info-box",
        h3("What This Dashboard Shows"),
        p("This dashboard analyzes NFL games using statistics from every play. It predicts game outcomes, projects individual player performance, and compares these predictions to professional oddsmakers (Vegas). Think of it as a data-driven 'crystal ball' for football - it can't see the future perfectly, but it uses patterns from past games to make educated guesses about what might happen next.")
      ),
      
      fluidRow(
        column(3,
               div(class = "metric-box",
                   div(class = "metric-value", textOutput("total_games")),
                   div(class = "metric-label", "Games Analyzed")
               )
        ),
        column(3,
               div(class = "metric-box",
                   div(class = "metric-value", textOutput("total_qbs")),
                   div(class = "metric-label", "QBs Projected")
               )
        ),
        column(3,
               div(class = "metric-box",
                   div(class = "metric-value", textOutput("total_rbs")),
                   div(class = "metric-label", "RBs Projected")
               )
        ),
        column(3,
               div(class = "metric-box",
                   div(class = "metric-value", textOutput("total_wrs")),
                   div(class = "metric-label", "WRs Projected")
               )
        )
      ),
      
      br(),
      h3("Value Opportunities"),
      p("Games where the model disagrees significantly with Vegas:"),
      DTOutput("value_bets_table")
    ),
    
    # Game Predictions Tab
    tabPanel(
      "Game Predictions",
      br(),
      div(
        class = "info-box",
        h3("How Game Predictions Work"),
        p(strong("EPA (Expected Points Added)"), " is the main metric. The model looks at:"),
        tags$ul(
          tags$li("Offensive Strength: How well a team moves the ball and scores"),
          tags$li("Defensive Strength: How well a team stops opponents"),
          tags$li("Recent Form: Are they playing better or worse lately?"),
          tags$li("Matchup: Does Team A's offense match up well against Team B's defense?")
        )
      ),
      
      h3("All Game Predictions"),
      DTOutput("games_table"),
      
      br(),
      h3("Model vs Vegas Spreads"),
      plotlyOutput("spreads_chart", height = "500px")
    ),
    
    # Player Props Tab
    tabPanel(
      "Player Projections",
      br(),
      div(
        class = "info-box",
        h3("Player Performance Projections"),
        p("Predicts how many yards each player will get based on their recent games and the opponent they're facing. The model uses a ", strong("60/40 split"), ": 60% weight on the last 3 games (recent form) and 40% weight on the whole season average. It also adjusts for opponent strength and home field advantage.")
      ),
      
      h3("Quarterbacks - Passing Yards"),
      plotlyOutput("qb_chart", height = "600px"),
      DTOutput("qb_table"),
      
      br(),
      h3("Running Backs - Rushing Yards"),
      plotlyOutput("rb_chart", height = "600px"),
      DTOutput("rb_table"),
      
      br(),
      h3("Wide Receivers - Receiving Yards"),
      plotlyOutput("wr_chart", height = "600px"),
      DTOutput("wr_table")
    ),
    
    # Accuracy Tab
    tabPanel(
      "Model Accuracy",
      br(),
      div(
        class = "info-box",
        h3("Testing Model Accuracy"),
        p("The only way to know if a prediction model actually works is to test it against real results. This section compares the model's predictions to what actually happened in games. Lower errors = better predictions. Random guessing would have huge errors.")
      ),
      
      h3("Instructions for Testing Accuracy"),
      p("After games complete, you can measure model accuracy:"),
      tags$ol(
        tags$li("Wait for all Week ", textOutput("current_week", inline = TRUE), " games to finish"),
        tags$li("Re-run nfl_master_pipeline.R to get actual results"),
        tags$li("Compare projected_margin to actual game outcomes"),
        tags$li("Calculate Mean Absolute Error (MAE) for each prediction type")
      ),
      
      br(),
      h3("Prediction Accuracy Metrics"),
      uiOutput("accuracy_output")
    ),
    
    # How It Works Tab
    tabPanel(
      "How It Works",
      br(),
      h2("How This Model Works (In Plain English)"),
      
      h3("Step 1: Collect the Data"),
      p("The model downloads every play from every NFL game this season - thousands of individual plays. Each play includes: who ran/threw, how many yards gained, whether it worked, field position, time remaining, etc."),
      
      h3("Step 2: Calculate Team Strength"),
      p("Instead of just counting points scored, the model uses ", strong("EPA"), " (Expected Points Added). Think of it this way: gaining 5 yards on 3rd-and-2 is much more valuable than gaining 5 yards on 3rd-and-15. EPA captures this nuance."),
      
      h3("Step 3: Predict Matchups"),
      p("When Team A plays Team B, the model looks at: How good is Team A's offense? How good is Team B's defense? Then it does the reverse for Team B's offense vs Team A's defense. The team with the bigger advantage should win."),
      
      h3("Step 4: Compare to Vegas"),
      p("Professional bookmakers in Vegas set betting lines based on tons of data and billions of dollars wagered. When this model disagrees with Vegas by a lot, that's called 'finding value' - either the model is right and Vegas is wrong, or vice versa."),
      
      h3("Step 5: Test Accuracy"),
      p("After games happen, compare predictions to actual results. If the model consistently predicts games better than random guessing, it might have value. If it's no better than a coin flip, it's just noise."),
      
      br(),
      h3("Key Concepts:"),
      tags$ul(
        tags$li(strong("EPA:"), " Measures play value (better than just yards)"),
        tags$li(strong("Rolling Average:"), " Recent games matter more than old games"),
        tags$li(strong("Matchup Advantage:"), " Your strength vs their weakness"),
        tags$li(strong("Edge:"), " How much your prediction differs from Vegas"),
        tags$li(strong("Regression to Mean:"), " Extreme performances tend to normalize over time")
      ),
      
      br(),
      h3("Important Limitations"),
      p("This model ", strong("cannot predict:"),
        tags$ul(
          tags$li("Injuries that happen before the game"),
          tags$li("Weather conditions (rain, wind, snow)"),
          tags$li("Coaching decisions or game scripts"),
          tags$li("Motivation, rivalry, or 'trap game' factors"),
          tags$li("Unusual events (fumbles are partly random)")
        )
      ),
      p("Even with perfect data, football has inherent randomness. The better team doesn't always win. That's why testing accuracy over many weeks is essential.")
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  # Reactive values to store data
  data <- reactiveValues(
    games = NULL,
    qb = NULL,
    rb = NULL,
    wr = NULL,
    odds = NULL,
    loaded = FALSE
  )
  
  # Load data when button is clicked
  observeEvent(input$load_data, {
    
    week <- input$week
    base_path <- input$base_dir
    week_path <- file.path(base_path, paste0("week", week))
    
    # Show loading notification
    showNotification("Loading data...", type = "message", duration = 2)
    
    tryCatch({
      # Load matchup data
      matchup_file <- file.path(week_path, "matchup_analysis", "matchup_summary.csv")
      if (file.exists(matchup_file)) {
        data$games <- read.csv(matchup_file)
      }
      
      # Load odds data
      odds_file <- file.path(week_path, "odds_analysis", "all_games_analysis.csv")
      if (file.exists(odds_file)) {
        data$odds <- read.csv(odds_file)
      }
      
      # Load player projections
      qb_file <- file.path(week_path, "player_props", "qb_projections.csv")
      if (file.exists(qb_file)) {
        data$qb <- read.csv(qb_file)
      }
      
      rb_file <- file.path(week_path, "player_props", "rb_projections.csv")
      if (file.exists(rb_file)) {
        data$rb <- read.csv(rb_file)
      }
      
      wr_file <- file.path(week_path, "player_props", "wr_projections.csv")
      if (file.exists(wr_file)) {
        data$wr <- read.csv(wr_file)
      }
      
      data$loaded <- TRUE
      showNotification("Data loaded successfully!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(
        paste("Error loading data:", e$message, 
              "\nMake sure you've run the R scripts first!"),
        type = "error",
        duration = 10
      )
    })
  })
  
  # Overview metrics
  output$total_games <- renderText({
    if (!is.null(data$games)) nrow(data$games) else "0"
  })
  
  output$total_qbs <- renderText({
    if (!is.null(data$qb)) nrow(data$qb) else "0"
  })
  
  output$total_rbs <- renderText({
    if (!is.null(data$rb)) nrow(data$rb) else "0"
  })
  
  output$total_wrs <- renderText({
    if (!is.null(data$wr)) nrow(data$wr) else "0"
  })
  
  output$current_week <- renderText({
    input$week
  })
  
  # Value bets table
  output$value_bets_table <- renderDT({
    if (!is.null(data$odds)) {
      data$odds %>%
        filter(ev_tier %in% c("🔥 ELITE", "⭐ STRONG", "✓ GOOD")) %>%
        select(matchup_key, ev_tier, edge, bet_recommendation, confidence) %>%
        arrange(desc(abs(edge))) %>%
        datatable(
          options = list(pageLength = 10, dom = 'tip'),
          rownames = FALSE,
          colnames = c("Game", "Value Tier", "Edge (pts)", "Recommendation", "Model Confidence")
        )
    }
  })
  
  # Games table
  output$games_table <- renderDT({
    if (!is.null(data$games)) {
      data$games %>%
        select(game, home_team, away_team, projected_margin, 
               home_epa_advantage, away_epa_advantage) %>%
        datatable(
          options = list(pageLength = 15, dom = 'tip'),
          rownames = FALSE,
          colnames = c("Matchup", "Home", "Away", "Projected Margin", 
                       "Home EPA Adv", "Away EPA Adv")
        ) %>%
        formatRound(columns = c("projected_margin", "home_epa_advantage", 
                                "away_epa_advantage"), digits = 2)
    }
  })
  
  # Spreads chart
  output$spreads_chart <- renderPlotly({
    if (!is.null(data$odds)) {
      p <- data$odds %>%
        arrange(desc(abs(edge))) %>%
        head(12) %>%
        mutate(matchup_key = reorder(matchup_key, edge)) %>%
        ggplot(aes(x = matchup_key, y = edge, fill = ev_tier)) +
        geom_col() +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        coord_flip() +
        scale_fill_manual(
          values = c("🔥 ELITE" = "#d73027", "⭐ STRONG" = "#fc8d59", 
                     "✓ GOOD" = "#fee08b", "❌ PASS" = "#d9d9d9")
        ) +
        labs(
          title = "Model vs Vegas: Biggest Disagreements",
          subtitle = "Positive = Bet Home | Negative = Bet Away",
          x = NULL,
          y = "Edge (Model Spread - Vegas Spread)",
          fill = "Value Tier"
        ) +
        theme_minimal(base_size = 12)
      
      ggplotly(p, tooltip = c("x", "y", "fill"))
    }
  })
  
  # QB Chart
  output$qb_chart <- renderPlotly({
    if (!is.null(data$qb)) {
      # Handle both 'team' and 'team.x' column names
      team_col <- if("team" %in% names(data$qb)) "team" else if("team.x" %in% names(data$qb)) "team.x" else NULL
      
      if (!is.null(team_col)) {
        p <- data$qb %>%
          head(15) %>%
          mutate(player_name = reorder(player_name, projected_pass_yards)) %>%
          ggplot(aes(x = player_name, y = projected_pass_yards, 
                     text = paste0(player_name, "<br>", 
                                   .data[[team_col]], " vs ", opponent, "<br>",
                                   round(projected_pass_yards), " yards"))) +
          geom_col(fill = "#667eea") +
          coord_flip() +
          labs(
            title = "Top QB Projections - Passing Yards",
            x = NULL,
            y = "Projected Passing Yards"
          ) +
          theme_minimal(base_size = 12)
        
        ggplotly(p, tooltip = "text")
      }
    }
  })
  
  # QB Table
  output$qb_table <- renderDT({
    if (!is.null(data$qb)) {
      data$qb %>%
        head(20) %>%
        select(player_name, team, opponent, is_home, 
               recent_avg, season_avg, projected_pass_yards) %>%
        datatable(
          options = list(pageLength = 10, dom = 'tip'),
          rownames = FALSE,
          colnames = c("Player", "Team", "Opponent", "Home?", 
                       "Recent Avg", "Season Avg", "Projection")
        ) %>%
        formatRound(columns = c("recent_avg", "season_avg", "projected_pass_yards"), 
                    digits = 1)
    }
  })
  
  # RB Chart
  output$rb_chart <- renderPlotly({
    if (!is.null(data$rb)) {
      # Handle both 'team' and 'team.x' column names
      team_col <- if("team" %in% names(data$rb)) "team" else if("team.x" %in% names(data$rb)) "team.x" else NULL
      
      if (!is.null(team_col)) {
        p <- data$rb %>%
          head(15) %>%
          mutate(player_name = reorder(player_name, projected_rush_yards)) %>%
          ggplot(aes(x = player_name, y = projected_rush_yards,
                     text = paste0(player_name, "<br>",
                                   .data[[team_col]], " vs ", opponent, "<br>",
                                   round(projected_rush_yards), " yards"))) +
          geom_col(fill = "#e74c3c") +
          coord_flip() +
          labs(
            title = "Top RB Projections - Rushing Yards",
            x = NULL,
            y = "Projected Rushing Yards"
          ) +
          theme_minimal(base_size = 12)
        
        ggplotly(p, tooltip = "text")
      }
    }
  })
  
  # RB Table
  output$rb_table <- renderDT({
    if (!is.null(data$rb)) {
      data$rb %>%
        head(20) %>%
        select(player_name, team, opponent, is_home,
               recent_avg, season_avg, projected_rush_yards) %>%
        datatable(
          options = list(pageLength = 10, dom = 'tip'),
          rownames = FALSE,
          colnames = c("Player", "Team", "Opponent", "Home?",
                       "Recent Avg", "Season Avg", "Projection")
        ) %>%
        formatRound(columns = c("recent_avg", "season_avg", "projected_rush_yards"),
                    digits = 1)
    }
  })
  
  # WR Chart
  output$wr_chart <- renderPlotly({
    if (!is.null(data$wr)) {
      # Handle both 'team' and 'team.x' column names
      team_col <- if("team" %in% names(data$wr)) "team" else if("team.x" %in% names(data$wr)) "team.x" else NULL
      
      if (!is.null(team_col)) {
        p <- data$wr %>%
          head(15) %>%
          mutate(player_name = reorder(player_name, projected_rec_yards)) %>%
          ggplot(aes(x = player_name, y = projected_rec_yards,
                     text = paste0(player_name, "<br>",
                                   .data[[team_col]], " vs ", opponent, "<br>",
                                   round(projected_rec_yards), " yards"))) +
          geom_col(fill = "#27ae60") +
          coord_flip() +
          labs(
            title = "Top WR Projections - Receiving Yards",
            x = NULL,
            y = "Projected Receiving Yards"
          ) +
          theme_minimal(base_size = 12)
        
        ggplotly(p, tooltip = "text")
      }
    }
  })
  
  # WR Table
  output$wr_table <- renderDT({
    if (!is.null(data$wr)) {
      data$wr %>%
        head(20) %>%
        select(player_name, team, opponent, is_home,
               recent_avg, season_avg, projected_rec_yards) %>%
        datatable(
          options = list(pageLength = 10, dom = 'tip'),
          rownames = FALSE,
          colnames = c("Player", "Team", "Opponent", "Home?",
                       "Recent Avg", "Season Avg", "Projection")
        ) %>%
        formatRound(columns = c("recent_avg", "season_avg", "projected_rec_yards"),
                    digits = 1)
    }
  })
  
  # Accuracy output
  output$accuracy_output <- renderUI({
    div(
      class = "info-box",
      style = "background: #f8f9fa; color: #333;",
      h4("After Week Completes"),
      p("Run this code in R to calculate accuracy:"),
      tags$pre(
        style = "background: white; padding: 15px; border-radius: 5px; overflow-x: auto;",
        HTML("
# Load actual results
actuals <- nflreadr::load_pbp(2025) %>%
  filter(week == ", input$week, ", play_type == 'pass') %>%
  group_by(passer_player_name) %>%
  summarise(actual_yards = sum(passing_yards, na.rm = TRUE))

# Compare to projections
comparison <- qb_projections %>%
  left_join(actuals, by = c('player_name' = 'passer_player_name')) %>%
  mutate(error = abs(projected_pass_yards - actual_yards))

# Calculate Mean Absolute Error
mean(comparison$error, na.rm = TRUE)
        ")
      )
    )
  })
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)