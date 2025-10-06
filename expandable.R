# ============================================================================
# NFL ANALYTICS - STANDALONE HTML GENERATOR WITH EXPANDABLE CARDS
# Purpose: Create self-contained HTML file with detailed expandable analysis
# Save as: generate_html_dashboard.R
# ============================================================================

library(dplyr)
library(jsonlite)

# ============================================================================
# CONFIGURATION
# ============================================================================

SEASON <- 2025
WEEK <- 5
BASE_DIR <- 'C:/Users/Patsc/Documents/nfl'

cat("Loading data and generating HTML dashboard...\n")

# ============================================================================
# LOAD DATA
# ============================================================================

week_path <- file.path(BASE_DIR, paste0('week', WEEK))

games <- read.csv(file.path(week_path, "matchup_analysis", "matchup_summary.csv"))
odds <- read.csv(file.path(week_path, "odds_analysis", "all_games_analysis.csv"))
qb <- read.csv(file.path(week_path, "player_props", "qb_projections.csv"))
rb <- read.csv(file.path(week_path, "player_props", "rb_projections.csv"))
wr <- read.csv(file.path(week_path, "player_props", "wr_projections.csv"))

# Handle team column naming
team_col_qb <- if("team" %in% names(qb)) "team" else "team.x"
team_col_rb <- if("team" %in% names(rb)) "team" else "team.x"
team_col_wr <- if("team" %in% names(wr)) "team" else "team.x"

# Print column names to debug
cat("\nQB columns:", paste(names(qb), collapse=", "), "\n")
cat("RB columns:", paste(names(rb), collapse=", "), "\n")
cat("WR columns:", paste(names(wr), collapse=", "), "\n")
cat("Games columns:", paste(names(games), collapse=", "), "\n")

# Prepare comprehensive player data with all available stats
qb_data <- qb %>% head(15)
rb_data <- rb %>% head(15)
wr_data <- wr %>% head(15)

# Prepare comprehensive game data with all available stats
odds_data <- odds %>%
  filter(ev_tier %in% c("🔥 ELITE", "⭐ STRONG", "✓ GOOD")) %>%
  arrange(desc(abs(edge)))

games_data <- games

# Convert to JSON (all columns for detailed view)
qb_json <- toJSON(qb_data, dataframe = "rows", na = "null")
rb_json <- toJSON(rb_data, dataframe = "rows", na = "null")
wr_json <- toJSON(wr_data, dataframe = "rows", na = "null")
odds_json <- toJSON(odds_data, dataframe = "rows", na = "null")
games_json <- toJSON(games_data, dataframe = "rows", na = "null")

# ============================================================================
# GENERATE HTML IN CHUNKS
# ============================================================================

# HTML Head and Styles
html_part1 <- '<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>NFL Analytics Dashboard - Week %WEEK%</title>
    <script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.0/dist/chart.umd.min.js"></script>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            padding: 20px;
        }
        .container {
            max-width: 1400px;
            margin: 0 auto;
            background: white;
            border-radius: 20px;
            box-shadow: 0 20px 60px rgba(0,0,0,0.3);
            overflow: hidden;
        }
        header {
            background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%);
            color: white;
            padding: 40px;
            text-align: center;
        }
        header h1 { font-size: 2.5em; margin-bottom: 10px; }
        header p { font-size: 1.2em; opacity: 0.9; }
        .tabs {
            display: flex;
            background: #f8f9fa;
            border-bottom: 2px solid #e9ecef;
            flex-wrap: wrap;
        }
        .tab {
            padding: 15px 25px;
            cursor: pointer;
            font-weight: 600;
            color: #6c757d;
            border-bottom: 3px solid transparent;
            transition: all 0.3s;
        }
        .tab:hover { color: #667eea; background: rgba(102, 126, 234, 0.1); }
        .tab.active { color: #667eea; border-bottom-color: #667eea; }
        .content { padding: 40px; }
        .tab-content { display: none; }
        .tab-content.active { display: block; animation: fadeIn 0.4s; }
        @keyframes fadeIn {
            from { opacity: 0; transform: translateY(10px); }
            to { opacity: 1; transform: translateY(0); }
        }
        .info-box {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 25px;
            border-radius: 12px;
            margin-bottom: 30px;
            line-height: 1.6;
        }
        .info-box h3 { margin-bottom: 10px; font-size: 1.3em; }
'

html_part2 <- '        .grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
            gap: 20px;
            margin: 20px 0;
        }
        .card {
            background: white;
            border: 2px solid #e9ecef;
            border-radius: 12px;
            padding: 20px;
            box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        }
        .card h3 { color: #1e3c72; margin-bottom: 15px; }
        .metric-box {
            text-align: center;
            padding: 20px;
            background: #f8f9fa;
            border-radius: 10px;
        }
        .metric-value {
            font-size: 2.5em;
            font-weight: bold;
            color: #667eea;
        }
        .metric-label { color: #6c757d; margin-top: 5px; }
        .expandable-card {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            border-radius: 12px;
            margin-bottom: 15px;
            overflow: hidden;
            transition: all 0.3s ease;
            cursor: pointer;
            box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        }
        .expandable-card:hover {
            transform: translateY(-2px);
            box-shadow: 0 6px 12px rgba(0,0,0,0.15);
        }
        .expandable-card.expanded {
            background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%);
        }
        .card-header {
            padding: 20px;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }
        .card-main-info { flex: 1; }
        .card-main-info h4 { font-size: 1.2em; margin-bottom: 5px; }
        .card-subtitle { opacity: 0.9; font-size: 0.9em; }
        .card-highlight { text-align: right; }
        .card-stat-big { font-size: 2.2em; font-weight: 700; line-height: 1; }
        .card-stat-label { opacity: 0.8; font-size: 0.85em; margin-top: 5px; }
'

html_part3 <- '        .expand-icon {
            font-size: 1.5em;
            transition: transform 0.3s;
            margin-left: 15px;
        }
        .expandable-card.expanded .expand-icon { transform: rotate(180deg); }
        .card-details {
            max-height: 0;
            overflow: hidden;
            transition: max-height 0.4s ease;
            background: rgba(255,255,255,0.1);
        }
        .expandable-card.expanded .card-details { max-height: 1000px; }
        .card-details-content {
            padding: 20px;
            border-top: 1px solid rgba(255,255,255,0.2);
        }
        .detail-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
            gap: 15px;
            margin-top: 15px;
        }
        .detail-item {
            background: rgba(255,255,255,0.1);
            padding: 12px;
            border-radius: 8px;
        }
        .detail-label { font-size: 0.85em; opacity: 0.8; margin-bottom: 5px; }
        .detail-value { font-size: 1.3em; font-weight: 600; }
        .detail-section { margin-top: 20px; }
        .detail-section h5 { font-size: 1.1em; margin-bottom: 10px; opacity: 0.9; }
        table { width: 100%; border-collapse: collapse; margin-top: 15px; }
        th, td { padding: 12px; text-align: left; border-bottom: 1px solid #e9ecef; }
        th { background: #f8f9fa; font-weight: 600; color: #495057; }
        tr:hover { background: #f8f9fa; }
        footer {
            background: #f8f9fa;
            padding: 30px;
            text-align: center;
            color: #6c757d;
            border-top: 2px solid #e9ecef;
        }
        .game-card {
            display: grid;
            grid-template-columns: 1fr auto 1fr;
            align-items: center;
            gap: 20px;
        }
        .team-info { text-align: center; }
        .vs-divider { font-size: 1.5em; font-weight: bold; opacity: 0.6; }
    </style>
</head>
<body>
    <div class="container">
        <header>
            <h1>NFL Analytics Dashboard</h1>
            <p>Week %WEEK% - Data-Driven Football Analysis</p>
        </header>
'

html_part4 <- '        <div class="tabs">
            <div class="tab active" onclick="showTab(\'overview\')">Overview</div>
            <div class="tab" onclick="showTab(\'games\')">Game Predictions</div>
            <div class="tab" onclick="showTab(\'players\')">Player Props</div>
            <div class="tab" onclick="showTab(\'explain\')">How It Works</div>
        </div>
        <div class="content">
            <div id="overview" class="tab-content active">
                <div class="info-box">
                    <h3>What This Dashboard Shows</h3>
                    <p>This dashboard analyzes NFL games using statistics from every play. Click on any card to expand and see detailed analysis.</p>
                </div>
                <div class="grid">
                    <div class="metric-box">
                        <div class="metric-value">%NGAMES%</div>
                        <div class="metric-label">Games Analyzed</div>
                    </div>
                    <div class="metric-box">
                        <div class="metric-value">%NQB%</div>
                        <div class="metric-label">QBs Projected</div>
                    </div>
                    <div class="metric-box">
                        <div class="metric-value">%NRB%</div>
                        <div class="metric-label">RBs Projected</div>
                    </div>
                    <div class="metric-box">
                        <div class="metric-value">%NWR%</div>
                        <div class="metric-label">WRs Projected</div>
                    </div>
                </div>
                <div class="card">
                    <h3>Value Opportunities (Click to Expand)</h3>
                    <p>Games where the model disagrees significantly with Vegas:</p>
                    <div id="valueCards"></div>
                </div>
            </div>
            <div id="games" class="tab-content">
                <div class="info-box">
                    <h3>Game Predictions</h3>
                    <p>Based on EPA. Click on any game to see detailed matchup analysis.</p>
                </div>
                <div id="gamesCards"></div>
            </div>
'

html_part5 <- '            <div id="players" class="tab-content">
                <div class="info-box">
                    <h3>Player Projections</h3>
                    <p>Click on any player to see detailed stats and recent performance.</p>
                </div>
                <div class="card">
                    <h3>Top Quarterback Projections</h3>
                    <div id="qbCards"></div>
                </div>
                <div class="card">
                    <h3>Top Running Back Projections</h3>
                    <div id="rbCards"></div>
                </div>
                <div class="card">
                    <h3>Top Receiver Projections</h3>
                    <div id="wrCards"></div>
                </div>
            </div>
            <div id="explain" class="tab-content">
                <div class="card">
                    <h3>How This Model Works</h3>
                    <h4 style="margin-top: 25px; color: #667eea;">Step 1: Collect the Data</h4>
                    <p>Downloads every play from every NFL game this season.</p>
                    <h4 style="margin-top: 20px; color: #667eea;">Step 2: Calculate Team Strength</h4>
                    <p>Uses EPA (Expected Points Added) instead of just points scored.</p>
                    <h4 style="margin-top: 20px; color: #667eea;">Step 3: Predict Matchups</h4>
                    <p>Compares Team A offense vs Team B defense, and vice versa.</p>
                    <h4 style="margin-top: 20px; color: #667eea;">Step 4: Compare to Vegas</h4>
                    <p>When the model disagrees with Vegas by 2.5+ points, that indicates value.</p>
                    <h4 style="margin-top: 20px; color: #667eea;">Important Limitations</h4>
                    <ul style="margin: 15px 0 0 20px; line-height: 1.8;">
                        <li>Cannot predict injuries, weather, or coaching decisions</li>
                        <li>Football has inherent randomness</li>
                        <li>Model accuracy should be tested over multiple weeks</li>
                        <li>For research and analytical purposes</li>
                    </ul>
                </div>
            </div>
        </div>
        <footer>
            <p><strong>NFL Analytics Dashboard</strong> | Built for statistical analysis</p>
            <p style="margin-top: 10px; font-size: 0.9em;">Data source: nflverse | Week %WEEK%, %SEASON%</p>
        </footer>
    </div>
'

# JavaScript part 1
js_part1 <- '    <script>
        const qbData = %QB_JSON%;
        const rbData = %RB_JSON%;
        const wrData = %WR_JSON%;
        const oddsData = %ODDS_JSON%;
        const gamesData = %GAMES_JSON%;
        
        function showTab(tabName) {
            document.querySelectorAll(\'.tab\').forEach(t => t.classList.remove(\'active\'));
            document.querySelectorAll(\'.tab-content\').forEach(c => c.classList.remove(\'active\'));
            event.target.classList.add(\'active\');
            document.getElementById(tabName).classList.add(\'active\');
        }
        
        function toggleCard(cardId) {
            document.getElementById(cardId).classList.toggle(\'expanded\');
        }
        
        function formatStat(value, decimals = 1) {
            if (value === null || value === undefined) return \'-\';
            return typeof value === \'number\' ? value.toFixed(decimals) : value;
        }
'

# JavaScript part 2
js_part2 <- '        function createPlayerCard(player, index, type) {
            const cardId = `${type}-card-${index}`;
            const yardKey = type === \'qb\' ? \'projected_pass_yards\' : 
                           type === \'rb\' ? \'projected_rush_yards\' : \'projected_rec_yards\';
            const label = type === \'qb\' ? \'Pass Yards\' : 
                         type === \'rb\' ? \'Rush Yards\' : \'Rec Yards\';
            const teamKey = player.team || player["team.x"] || player["team.y"] || \'-\';
            
            console.log("Player data:", player);
            
            let detailsHTML = `<div class="card-details-content"><div class="detail-grid">`;
            if (type === \'qb\') {
                detailsHTML += `
                    <div class="detail-item"><div class="detail-label">Avg Pass Yards</div>
                    <div class="detail-value">${formatStat(player.avg_pass_yards || player.season_avg_pass_yards, 0)}</div></div>
                    <div class="detail-item"><div class="detail-label">Recent Form</div>
                    <div class="detail-value">${formatStat(player.recent_avg_pass_yards || player.recent_pass_yards, 0)}</div></div>
                    <div class="detail-item"><div class="detail-label">Pass TDs/Game</div>
                    <div class="detail-value">${formatStat(player.avg_pass_td || player.season_avg_pass_td)}</div></div>
                    <div class="detail-item"><div class="detail-label">Completions</div>
                    <div class="detail-value">${formatStat(player.avg_completions || player.season_avg_completions, 1)}</div></div>`;
            } else if (type === \'rb\') {
                detailsHTML += `
                    <div class="detail-item"><div class="detail-label">Avg Rush Yards</div>
                    <div class="detail-value">${formatStat(player.avg_rush_yards || player.season_avg_rush_yards, 0)}</div></div>
                    <div class="detail-item"><div class="detail-label">Recent Form</div>
                    <div class="detail-value">${formatStat(player.recent_avg_rush_yards || player.recent_rush_yards, 0)}</div></div>
                    <div class="detail-item"><div class="detail-label">Rush TDs/Game</div>
                    <div class="detail-value">${formatStat(player.avg_rush_td || player.season_avg_rush_td)}</div></div>
                    <div class="detail-item"><div class="detail-label">Carries/Game</div>
                    <div class="detail-value">${formatStat(player.avg_carries || player.season_avg_carries, 1)}</div></div>`;
            } else {
                detailsHTML += `
                    <div class="detail-item"><div class="detail-label">Avg Rec Yards</div>
                    <div class="detail-value">${formatStat(player.avg_rec_yards || player.season_avg_rec_yards, 0)}</div></div>
                    <div class="detail-item"><div class="detail-label">Recent Form</div>
                    <div class="detail-value">${formatStat(player.recent_avg_rec_yards || player.recent_rec_yards, 0)}</div></div>
                    <div class="detail-item"><div class="detail-label">Rec TDs/Game</div>
                    <div class="detail-value">${formatStat(player.avg_rec_td || player.season_avg_rec_td)}</div></div>
                    <div class="detail-item"><div class="detail-label">Targets/Game</div>
                    <div class="detail-value">${formatStat(player.avg_targets || player.season_avg_targets, 1)}</div></div>`;
            }
            detailsHTML += `</div><div class="detail-section"><h5>Performance Analysis</h5>
                <div class="detail-grid"><div class="detail-item"><div class="detail-label">Games Played</div>
                <div class="detail-value">${player.games_played || player.games || \'-\'}</div></div>
                <div class="detail-item"><div class="detail-label">Opponent Defense</div>
                <div class="detail-value">${player.opponent || \'-\'}</div></div></div></div></div>`;
            return `<div class="expandable-card" id="${cardId}" onclick="toggleCard(\'${cardId}\')">
                <div class="card-header"><div class="card-main-info"><h4>${player.player_name}</h4>
                <div class="card-subtitle">${teamKey} vs ${player.opponent || \'-\'}</div></div>
                <div class="card-highlight"><div class="card-stat-big">${Math.round(player[yardKey] || 0)}</div>
                <div class="card-stat-label">${label}</div></div><div class="expand-icon">▼</div></div>
                <div class="card-details">${detailsHTML}</div></div>`;
        }
'

# JavaScript part 3
js_part3 <- '        function createGameCard(game, index) {
            const cardId = `game-card-${index}`;
            const detailsHTML = `<div class="card-details-content"><div class="detail-section">
                <h5>Team Statistics</h5><div class="detail-grid">
                <div class="detail-item"><div class="detail-label">Home EPA/Play</div>
                <div class="detail-value">${formatStat(game.home_epa)}</div></div>
                <div class="detail-item"><div class="detail-label">Away EPA/Play</div>
                <div class="detail-value">${formatStat(game.away_epa)}</div></div>
                <div class="detail-item"><div class="detail-label">Home Advantage</div>
                <div class="detail-value">${formatStat(game.net_home_advantage)}</div></div>
                <div class="detail-item"><div class="detail-label">Proj Margin</div>
                <div class="detail-value">${formatStat(game.projected_margin)}</div></div></div></div>
                <div class="detail-section"><h5>Offense vs Defense</h5><div class="detail-grid">
                <div class="detail-item"><div class="detail-label">Home Off EPA</div>
                <div class="detail-value">${formatStat(game.home_offense_epa)}</div></div>
                <div class="detail-item"><div class="detail-label">Away Def EPA</div>
                <div class="detail-value">${formatStat(game.away_defense_epa)}</div></div>
                <div class="detail-item"><div class="detail-label">Away Off EPA</div>
                <div class="detail-value">${formatStat(game.away_offense_epa)}</div></div>
                <div class="detail-item"><div class="detail-label">Home Def EPA</div>
                <div class="detail-value">${formatStat(game.home_defense_epa)}</div></div>
                </div></div></div>`;
            return `<div class="expandable-card" id="${cardId}" onclick="toggleCard(\'${cardId}\')">
                <div class="card-header"><div class="card-main-info"><div class="game-card">
                <div class="team-info"><h4>${game.away_team}</h4><div class="card-subtitle">Away</div></div>
                <div class="vs-divider">@</div>
                <div class="team-info"><h4>${game.home_team}</h4><div class="card-subtitle">Home</div></div>
                </div></div><div class="card-highlight"><div class="card-stat-big">${formatStat(Math.abs(game.projected_margin))}</div>
                <div class="card-stat-label">${game.projected_margin > 0 ? game.home_team : game.away_team} by</div>
                </div><div class="expand-icon">▼</div></div><div class="card-details">${detailsHTML}</div></div>`;
        }
        
        function createValueCard(odds, index) {
            const cardId = `value-card-${index}`;
            const detailsHTML = `<div class="card-details-content"><div class="detail-section">
                <h5>Betting Analysis</h5><div class="detail-grid">
                <div class="detail-item"><div class="detail-label">Edge</div>
                <div class="detail-value">${formatStat(odds.edge)} pts</div></div>
                <div class="detail-item"><div class="detail-label">Confidence</div>
                <div class="detail-value">${odds.confidence || \'-\'}</div></div>
                <div class="detail-item"><div class="detail-label">Model Line</div>
                <div class="detail-value">${formatStat(odds.model_spread)}</div></div>
                <div class="detail-item"><div class="detail-label">Vegas Line</div>
                <div class="detail-value">${formatStat(odds.vegas_spread)}</div></div></div></div>
                <div class="detail-section"><h5>Recommendation</h5>
                <p style="opacity: 0.9; line-height: 1.6;">${odds.bet_recommendation || \'No specific recommendation\'}</p>
                </div></div>`;
            return `<div class="expandable-card" id="${cardId}" onclick="toggleCard(\'${cardId}\')">
                <div class="card-header"><div class="card-main-info"><h4>${odds.matchup_key}</h4>
                <div class="card-subtitle">${odds.ev_tier}</div></div><div class="card-highlight">
                <div class="card-stat-big">${formatStat(Math.abs(odds.edge))}</div>
                <div class="card-stat-label">Edge (pts)</div></div><div class="expand-icon">▼</div></div>
                <div class="card-details">${detailsHTML}</div></div>`;
        }
        
        document.getElementById(\'qbCards\').innerHTML = qbData.map((p, i) => createPlayerCard(p, i, \'qb\')).join(\'\');
        document.getElementById(\'rbCards\').innerHTML = rbData.map((p, i) => createPlayerCard(p, i, \'rb\')).join(\'\');
        document.getElementById(\'wrCards\').innerHTML = wrData.map((p, i) => createPlayerCard(p, i, \'wr\')).join(\'\');
        document.getElementById(\'gamesCards\').innerHTML = gamesData.map((g, i) => createGameCard(g, i)).join(\'\');
        document.getElementById(\'valueCards\').innerHTML = oddsData.map((o, i) => createValueCard(o, i)).join(\'\');
    </script>
</body>
</html>'

# Combine all parts with replacements
html_content <- paste0(
  html_part1, html_part2, html_part3, html_part4, html_part5,
  js_part1, js_part2, js_part3
)

# Replace placeholders
html_content <- gsub("%WEEK%", WEEK, html_content)
html_content <- gsub("%SEASON%", SEASON, html_content)
html_content <- gsub("%NGAMES%", nrow(games), html_content)
html_content <- gsub("%NQB%", nrow(qb), html_content)
html_content <- gsub("%NRB%", nrow(rb), html_content)
html_content <- gsub("%NWR%", nrow(wr), html_content)
html_content <- gsub("%QB_JSON%", qb_json, html_content)
html_content <- gsub("%RB_JSON%", rb_json, html_content)
html_content <- gsub("%WR_JSON%", wr_json, html_content)
html_content <- gsub("%ODDS_JSON%", odds_json, html_content)
html_content <- gsub("%GAMES_JSON%", games_json, html_content)

# ============================================================================
# SAVE HTML FILE
# ============================================================================

output_file <- file.path(week_path, paste0("NFL_Analytics_Week", WEEK, ".html"))
writeLines(html_content, output_file)

cat("HTML dashboard generated!\n")
cat("File saved to:", output_file, "\n")
cat("Click any card to expand detailed analysis\n")