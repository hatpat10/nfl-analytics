# ============================================================================
# NFL ANALYTICS DASHBOARD - R SHINY APP WITH ML PREDICTIONS
# Purpose: Interactive visualization with ML model predictions
# Save as: nfl_dashboard_headshots.R
# Run with: shiny::runApp("nfl_dashboard_headshots.R")
# ============================================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)
library(nflplotR)
library(readxl)

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
        background: white;
        border: 2px solid #e0e0e0;
        border-radius: 10px;
        padding: 15px;
        margin: 10px;
        text-align: center;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .player-headshot {
        width: 80px;
        height: 80px;
        border-radius: 50%;
        border: 3px solid #667eea;
        margin: 10px auto;
      }
      .player-name {
        font-weight: bold;
        font-size: 1.1em;
        margin: 10px 0 5px 0;
      }
      .player-stat {
        font-size: 1.8em;
        font-weight: bold;
        color: #667eea;
        margin: 5px 0;
      }
      .player-team {
        color: #6c757d;
        font-size: 0.9em;
      }
      .team-logo {
        width: 30px;
        height: 30px;
        vertical-align: middle;
      }
    "))
  ),
  
  # Header
  div(
    style = "background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%); color: white; padding: 40px; text-align: center; margin-bottom: 20px;",
    h1("🏈 NFL Analytics Dashboard", style = "font-size: 2.5em; margin-bottom: 10px;"),
    p("Machine Learning Predictions with Player Headshots", style = "font-size: 1.2em; opacity: 0.9;")
  ),
  
  # Controls
  fluidRow(
    column(3,
           selectInput("season", "Season:", 
                       choices = 2023:2025, selected = 2025)
    ),
    column(3,
           selectInput("week", "Select Week:", 
                       choices = 1:18, selected = 6)
    ),
    column(3,
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
        p("This dashboard uses machine learning to predict NFL game outcomes and player performance. It analyzes every play from the season to predict scores, spreads, and totals, then compares these predictions to Vegas odds to find betting value.")
      ),
      
      fluidRow(
        column(3,
               div(class = "metric-box",
                   div(class = "metric-value", textOutput("total_games")),
                   div(class = "metric-label", "Games This Week")
               )
        ),
        column(3,
               div(class = "metric-box",
                   div(class = "metric-value", textOutput("ml_accuracy")),
                   div(class = "metric-label", "ML Model Accuracy")
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
                   div(class = "metric-value", textOutput("strong_bets")),
                   div(class = "metric-label", "Strong Bets")
               )
        )
      ),
      
      br(),
      h3("🌟 Top Player Projections"),
      fluidRow(
        column(12,
               uiOutput("top_players_cards")
        )
      ),
      
      br(),
      h3("💰 ML Model Value Opportunities"),
      p("Games where the ML model disagrees significantly with Vegas:"),
      DTOutput("ml_value_bets")
    ),
    
    # ML Predictions Tab
    tabPanel(
      "ML Predictions",
      br(),
      div(
        class = "info-box",
        h3("Machine Learning Game Predictions"),
        p("The ML model uses Logistic Regression, Random Forest, and XGBoost trained on weeks 1-5 data. It predicts win probabilities, point spreads, and total scores using EPA (Expected Points Added) and team performance metrics.")
      ),
      
      h3("🎯 Full Week Predictions"),
      DTOutput("ml_predictions_table"),
      
      br(),
      h3("📊 ML Model vs Vegas Comparison"),
      plotlyOutput("ml_vs_vegas_chart", height = "500px"),
      
      br(),
      h3("🔥 Best Betting Opportunities"),
      DTOutput("ml_recommended_bets")
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
        p("Predicts how many yards each player will get based on their recent games and the opponent they're facing. The model uses a ", strong("60/40 split"), ": 60% weight on the last 3 games (recent form) and 40% weight on the whole season average.")
      ),
      
      h3("🎯 Quarterbacks - Passing Yards"),
      uiOutput("qb_cards"),
      br(),
      plotlyOutput("qb_chart", height = "600px"),
      DTOutput("qb_table"),
      
      br(),
      h3("🏃 Running Backs - Rushing Yards"),
      uiOutput("rb_cards"),
      br(),
      plotlyOutput("rb_chart", height = "600px"),
      DTOutput("rb_table"),
      
      br(),
      h3("🙌 Wide Receivers - Receiving Yards"),
      uiOutput("wr_cards"),
      br(),
      plotlyOutput("wr_chart", height = "600px"),
      DTOutput("wr_table")
    ),
    
    # How It Works Tab
    tabPanel(
      "How It Works",
      br(),
      h2("How This Model Works (In Plain English)"),
      
      h3("Step 1: Machine Learning Training"),
      p("The model trains on historical data using three algorithms:"),
      tags$ul(
        tags$li(strong("Logistic Regression:"), " Simple probability model"),
        tags$li(strong("Random Forest:"), " Ensemble of decision trees"),
        tags$li(strong("XGBoost:"), " Advanced gradient boosting")
      ),
      
      h3("Step 2: Calculate Team Strength"),
      p("Instead of just counting points scored, the model uses ", strong("EPA"), " (Expected Points Added). Think of it this way: gaining 5 yards on 3rd-and-2 is much more valuable than gaining 5 yards on 3rd-and-15. EPA captures this nuance."),
      
      h3("Step 3: Make Predictions"),
      p("The trained models predict:"),
      tags$ul(
        tags$li(strong("Moneyline:"), " Win probability for each team"),
        tags$li(strong("Spread:"), " Point differential (who wins by how much)"),
        tags$li(strong("Total:"), " Combined score of both teams")
      ),
      
      h3("Step 4: Player Projections"),
      p("For individual players, the model:"),
      tags$ul(
        tags$li("Weights recent performance (last 3 games) at 60%"),
        tags$li("Weights season average at 40%"),
        tags$li("Adjusts for opponent defensive strength"),
        tags$li("Applies home field advantage bonus"),
        tags$li("Displays player headshots for easy identification")
      ),
      
      h3("Step 5: Find Value vs Vegas"),
      p("When the ML model disagrees with Vegas by 4+ points on spreads or 6+ points on totals, that's a potential value bet. Either the model found an edge, or Vegas knows something we don't!"),
      
      br(),
      h3("Key Concepts:"),
      tags$ul(
        tags$li(strong("EPA:"), " Measures play value (better than just yards)"),
        tags$li(strong("Ensemble:"), " Combines multiple models for better accuracy"),
        tags$li(strong("Training Data:"), " Uses weeks 1-5 to predict future weeks"),
        tags$li(strong("Edge:"), " How much your prediction differs from Vegas"),
        tags$li(strong("RMSE:"), " Root Mean Square Error - lower is better")
      )
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  # Reactive values to store data
  data <- reactiveValues(
    ml_predictions = NULL,
    ml_performance = NULL,
    games = NULL,
    qb = NULL,
    rb = NULL,
    wr = NULL,
    odds = NULL,
    schedule = NULL,
    loaded = FALSE,
    week_info = NULL
  )
  
  # Load data when button is clicked
  observeEvent(input$load_data, {
    
    season <- input$season
    week <- input$week
    base_path <- input$base_dir
    week_path <- file.path(base_path, paste0("week", week))
    
    showNotification("Loading data...", type = "message", duration = 2)
    
    tryCatch({
      # Load schedule first
      cat("Loading schedule for Week", week, "...\n")
      full_schedule <- nflreadr::load_schedules(as.numeric(season))
      week_schedule <- full_schedule %>% filter(week == !!week)
      
      if (nrow(week_schedule) > 0) {
        data$schedule <- week_schedule
        
        # Check if games have been played
        current_date <- Sys.Date()
        games_complete <- sum(as.Date(week_schedule$gameday) < current_date, na.rm = TRUE)
        games_future <- sum(as.Date(week_schedule$gameday) >= current_date, na.rm = TRUE)
        
        data$week_info <- list(
          week = week,
          total_games = nrow(week_schedule),
          games_complete = games_complete,
          games_future = games_future,
          is_future_week = games_future > 0
        )
        
        cat("✓ Schedule loaded:", nrow(week_schedule), "games\n")
      }
      
      # Load ML predictions
      ml_path <- file.path(week_path, "ml_predictions")
      ml_schedule_file <- file.path(ml_path, "full_week_schedule.csv")
      ml_excel_file <- file.path(ml_path, paste0("week", week, "_complete_betting_card.xlsx"))
      
      if (file.exists(ml_schedule_file)) {
        data$ml_predictions <- read.csv(ml_schedule_file)
        cat("✓ ML predictions loaded\n")
        
        # Load performance metrics if available
        if (file.exists(ml_excel_file)) {
          data$ml_performance <- read_excel(ml_excel_file, sheet = "Model_Performance")
          cat("✓ ML performance metrics loaded\n")
        }
      } else {
        cat("⚠️  No ML predictions yet (run nfl_ml_model.R first)\n")
      }
      
      # Load matchup data
      matchup_file <- file.path(week_path, "matchup_analysis", "matchup_summary.csv")
      if (file.exists(matchup_file)) {
        data$games <- read.csv(matchup_file)
        cat("✓ Matchup analysis loaded\n")
      }
      
      # Load odds data
      odds_file <- file.path(week_path, "odds_analysis", "all_games_analysis.csv")
      if (file.exists(odds_file)) {
        data$odds <- read.csv(odds_file)
        cat("✓ Odds analysis loaded\n")
      }
      
      # Load player projections
      qb_file <- file.path(week_path, "player_props", "qb_projections_enhanced.csv")
      if (file.exists(qb_file)) {
        data$qb <- read.csv(qb_file)
        cat("✓ QB projections loaded\n")
      }
      
      rb_file <- file.path(week_path, "player_props", "rb_projections_enhanced.csv")
      if (file.exists(rb_file)) {
        data$rb <- read.csv(rb_file)
        cat("✓ RB projections loaded\n")
      }
      
      wr_file <- file.path(week_path, "player_props", "wr_projections_enhanced.csv")
      if (file.exists(wr_file)) {
        data$wr <- read.csv(wr_file)
        cat("✓ WR projections loaded\n")
      }
      
      data$loaded <- TRUE
      showNotification("Data loaded successfully!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(
        paste("Error loading data:", e$message),
        type = "error",
        duration = 10
      )
    })
  })
  
  # Overview metrics
  output$total_games <- renderText({
    if (!is.null(data$ml_predictions)) {
      nrow(data$ml_predictions)
    } else if (!is.null(data$schedule)) {
      nrow(data$schedule)
    } else {
      "0"
    }
  })
  
  output$ml_accuracy <- renderText({
    if (!is.null(data$ml_performance)) {
      ensemble_acc <- data$ml_performance %>% 
        filter(Model == "Ensemble") %>% 
        pull(Moneyline_Accuracy)
      paste0(ensemble_acc, "%")
    } else {
      "N/A"
    }
  })
  
  output$total_qbs <- renderText({
    if (!is.null(data$qb)) nrow(data$qb) else "0"
  })
  
  output$strong_bets <- renderText({
    if (!is.null(data$ml_predictions)) {
      sum(data$ml_predictions$moneyline_strength == "🔥 STRONG", na.rm = TRUE)
    } else {
      "0"
    }
  })
  
  # ML Predictions Table
  output$ml_predictions_table <- renderDT({
    req(data$ml_predictions)
    
    data$ml_predictions %>%
      select(matchup, ml_pick, ml_confidence, spread_display, total_line, 
             predicted_score, game_type) %>%
      datatable(
        options = list(pageLength = 15, dom = 'tip'),
        rownames = FALSE,
        colnames = c("Game", "Winner", "Confidence", "Spread", "Total", 
                     "Predicted Score", "Game Type")
      )
  })
  
  # ML Value Bets
  output$ml_value_bets <- renderDT({
    req(data$ml_predictions)
    
    data$ml_predictions %>%
      filter(moneyline_strength %in% c("🔥 STRONG", "✓ GOOD")) %>%
      select(matchup, moneyline_strength, ml_pick, ml_confidence, moneyline_bet) %>%
      datatable(
        options = list(pageLength = 10, dom = 'tip'),
        rownames = FALSE,
        colnames = c("Game", "Strength", "Pick", "Confidence", "Recommendation")
      )
  })
  
  # ML Recommended Bets
  output$ml_recommended_bets <- renderDT({
    req(data$ml_predictions)
    
    data$ml_predictions %>%
      filter(moneyline_strength == "🔥 STRONG") %>%
      select(matchup, ml_pick, ml_confidence, spread_display, total_line,
             predicted_score) %>%
      datatable(
        options = list(pageLength = 10, dom = 'tip'),
        rownames = FALSE,
        colnames = c("Game", "Pick", "Confidence", "Spread", "Total", "Score")
      )
  })
  
  # ML vs Vegas Chart
  output$ml_vs_vegas_chart <- renderPlotly({
    req(data$ml_predictions)
    
    p <- data$ml_predictions %>%
      arrange(desc(abs(spread_line))) %>%
      head(12) %>%
      mutate(matchup = reorder(matchup, spread_line)) %>%
      ggplot(aes(x = matchup, y = spread_line, fill = moneyline_strength)) +
      geom_col() +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      coord_flip() +
      scale_fill_manual(
        values = c("🔥 STRONG" = "#d73027", "✓ GOOD" = "#fee08b", "⚠️ PASS" = "#d9d9d9")
      ) +
      labs(
        title = "ML Model Predicted Spreads",
        subtitle = "Positive = Home Favored | Negative = Away Favored",
        x = NULL,
        y = "Predicted Spread (Points)",
        fill = "Confidence"
      ) +
      theme_minimal(base_size = 12)
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Top players cards (Overview)
  output$top_players_cards <- renderUI({
    req(data$loaded)
    
    cards <- list()
    
    # Top 3 QBs
    if (!is.null(data$qb) && nrow(data$qb) > 0) {
      top_qbs <- data$qb %>% head(3)
      for (i in 1:nrow(top_qbs)) {
        player <- top_qbs[i, ]
        cards[[length(cards) + 1]] <- column(
          4,
          div(
            class = "player-card",
            if (!is.na(player$headshot)) {
              tags$img(src = player$headshot, class = "player-headshot")
            },
            div(class = "player-name", player$player_name),
            div(class = "player-stat", paste0(round(player$projected_pass_yards), " YDS")),
            div(class = "player-team", paste(player$team, "vs", player$opponent)),
            tags$small(style = "color: #999;", "QB - Passing")
          )
        )
      }
    }
    
    do.call(fluidRow, cards)
  })
  
  # QB Cards
  output$qb_cards <- renderUI({
    req(data$qb)
    
    top_qbs <- data$qb %>% head(6)
    cards <- list()
    
    for (i in 1:nrow(top_qbs)) {
      player <- top_qbs[i, ]
      cards[[length(cards) + 1]] <- column(
        2,
        div(
          class = "player-card",
          if (!is.na(player$headshot)) {
            tags$img(src = player$headshot, class = "player-headshot")
          },
          div(class = "player-name", player$player_name),
          div(class = "player-stat", paste0(round(player$projected_pass_yards))),
          div(class = "player-team", paste(player$team, "vs", player$opponent)),
          tags$small(paste("Recent:", round(player$recent_avg, 1)))
        )
      )
    }
    
    do.call(fluidRow, cards)
  })
  
  # RB Cards
  output$rb_cards <- renderUI({
    req(data$rb)
    
    top_rbs <- data$rb %>% head(6)
    cards <- list()
    
    for (i in 1:nrow(top_rbs)) {
      player <- top_rbs[i, ]
      cards[[length(cards) + 1]] <- column(
        2,
        div(
          class = "player-card",
          if (!is.na(player$headshot)) {
            tags$img(src = player$headshot, class = "player-headshot")
          },
          div(class = "player-name", player$player_name),
          div(class = "player-stat", paste0(round(player$projected_rush_yards))),
          div(class = "player-team", paste(player$team, "vs", player$opponent)),
          tags$small(paste("Recent:", round(player$recent_avg, 1)))
        )
      )
    }
    
    do.call(fluidRow, cards)
  })
  
  # WR Cards
  output$wr_cards <- renderUI({
    req(data$wr)
    
    top_wrs <- data$wr %>% head(6)
    cards <- list()
    
    for (i in 1:nrow(top_wrs)) {
      player <- top_wrs[i, ]
      cards[[length(cards) + 1]] <- column(
        2,
        div(
          class = "player-card",
          if (!is.na(player$headshot)) {
            tags$img(src = player$headshot, class = "player-headshot")
          },
          div(class = "player-name", player$player_name),
          div(class = "player-stat", paste0(round(player$projected_rec_yards))),
          div(class = "player-team", paste(player$team, "vs", player$opponent)),
          tags$small(paste("Recent:", round(player$recent_avg, 1)))
        )
      )
    }
    
    do.call(fluidRow, cards)
  })
  
  # Value bets table (from odds)
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
  
  # Spreads chart (from odds)
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
      p <- data$qb %>%
        head(15) %>%
        mutate(player_name = reorder(player_name, projected_pass_yards)) %>%
        ggplot(aes(x = player_name, y = projected_pass_yards, 
                   text = paste0(player_name, "<br>", 
                                 team, " vs ", opponent, "<br>",
                                 round(projected_pass_yards), " yards<br>",
                                 "Recent: ", round(recent_avg, 1)))) +
        geom_col(aes(fill = team_color)) +
        scale_fill_identity() +
        coord_flip() +
        labs(
          title = "Top QB Projections - Passing Yards",
          x = NULL,
          y = "Projected Passing Yards"
        ) +
        theme_minimal(base_size = 12)
      
      ggplotly(p, tooltip = "text")
    }
  })
  
  # QB Table
  output$qb_table <- renderDT({
    if (!is.null(data$qb)) {
      data$qb %>%
        head(20) %>%
        select(player_name, team, opponent, is_home, 
               recent_avg, season_avg, projected_pass_yards, jersey, experience) %>%
        datatable(
          options = list(pageLength = 10, dom = 'tip'),
          rownames = FALSE,
          colnames = c("Player", "Team", "Opponent", "Home?", 
                       "Recent Avg", "Season Avg", "Projection", "Jersey", "Exp")
        ) %>%
        formatRound(columns = c("recent_avg", "season_avg", "projected_pass_yards"), 
                    digits = 1)
    }
  })
  
  # RB Chart
  output$rb_chart <- renderPlotly({
    if (!is.null(data$rb)) {
      p <- data$rb %>%
        head(15) %>%
        mutate(player_name = reorder(player_name, projected_rush_yards)) %>%
        ggplot(aes(x = player_name, y = projected_rush_yards,
                   text = paste0(player_name, "<br>",
                                 team, " vs ", opponent, "<br>",
                                 round(projected_rush_yards), " yards<br>",
                                 "Recent: ", round(recent_avg, 1)))) +
        geom_col(aes(fill = team_color)) +
        scale_fill_identity() +
        coord_flip() +
        labs(
          title = "Top RB Projections - Rushing Yards",
          x = NULL,
          y = "Projected Rushing Yards"
        ) +
        theme_minimal(base_size = 12)
      
      ggplotly(p, tooltip = "text")
    }
  })
  
  # RB Table
  output$rb_table <- renderDT({
    if (!is.null(data$rb)) {
      data$rb %>%
        head(20) %>%
        select(player_name, team, opponent, is_home,
               recent_avg, season_avg, projected_rush_yards, jersey, experience) %>%
        datatable(
          options = list(pageLength = 10, dom = 'tip'),
          rownames = FALSE,
          colnames = c("Player", "Team", "Opponent", "Home?",
                       "Recent Avg", "Season Avg", "Projection", "Jersey", "Exp")
        ) %>%
        formatRound(columns = c("recent_avg", "season_avg", "projected_rush_yards"),
                    digits = 1)
    }
  })
  
  # WR Chart
  output$wr_chart <- renderPlotly({
    if (!is.null(data$wr)) {
      p <- data$wr %>%
        head(15) %>%
        mutate(player_name = reorder(player_name, projected_rec_yards)) %>%
        ggplot(aes(x = player_name, y = projected_rec_yards,
                   text = paste0(player_name, "<br>",
                                 team, " vs ", opponent, "<br>",
                                 round(projected_rec_yards), " yards<br>",
                                 "Recent: ", round(recent_avg, 1)))) +
        geom_col(aes(fill = team_color)) +
        scale_fill_identity() +
        coord_flip() +
        labs(
          title = "Top WR Projections - Receiving Yards",
          x = NULL,
          y = "Projected Receiving Yards"
        ) +
        theme_minimal(base_size = 12)
      
      ggplotly(p, tooltip = "text")
    }
  })
  
  # WR Table
  output$wr_table <- renderDT({
    if (!is.null(data$wr)) {
      data$wr %>%
        head(20) %>%
        select(player_name, team, opponent, is_home,
               recent_avg, season_avg, projected_rec_yards, jersey, experience) %>%
        datatable(
          options = list(pageLength = 10, dom = 'tip'),
          rownames = FALSE,
          colnames = c("Player", "Team", "Opponent", "Home?",
                       "Recent Avg", "Season Avg", "Projection", "Jersey", "Exp")
        ) %>%
        formatRound(columns = c("recent_avg", "season_avg", "projected_rec_yards"),
                    digits = 1)
    }
  })
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)