# ============================================================================
# NFL ANALYTICS - STANDALONE HTML GENERATOR
# Purpose: Create self-contained HTML file with embedded data
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

# Prepare data for embedding
qb_data <- qb %>% 
  select(player_name, team = all_of(team_col_qb), opponent, projected_pass_yards) %>%
  head(15)

rb_data <- rb %>% 
  select(player_name, team = all_of(team_col_rb), opponent, projected_rush_yards) %>%
  head(15)

wr_data <- wr %>% 
  select(player_name, team = all_of(team_col_wr), opponent, projected_rec_yards) %>%
  head(15)

odds_data <- odds %>%
  filter(ev_tier %in% c("🔥 ELITE", "⭐ STRONG", "✓ GOOD")) %>%
  select(matchup_key, ev_tier, edge, bet_recommendation, confidence) %>%
  arrange(desc(abs(edge)))

games_data <- games %>%
  select(game, home_team, away_team, projected_margin, net_home_advantage)

# Convert to JSON
qb_json <- toJSON(qb_data, dataframe = "rows")
rb_json <- toJSON(rb_data, dataframe = "rows")
wr_json <- toJSON(wr_data, dataframe = "rows")
odds_json <- toJSON(odds_data, dataframe = "rows")
games_json <- toJSON(games_data, dataframe = "rows")

# ============================================================================
# GENERATE HTML
# ============================================================================

html_content <- paste0('
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>NFL Analytics Dashboard - Week ', WEEK, '</title>
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
        
        .grid {
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
        
        table {
            width: 100%;
            border-collapse: collapse;
            margin-top: 15px;
        }
        
        th, td {
            padding: 12px;
            text-align: left;
            border-bottom: 1px solid #e9ecef;
        }
        
        th {
            background: #f8f9fa;
            font-weight: 600;
            color: #495057;
        }
        
        tr:hover { background: #f8f9fa; }
        
        .chart-container {
            position: relative;
            height: 500px;
            margin: 20px 0;
        }
        
        .player-grid {
            display: grid;
            grid-template-columns: repeat(auto-fill, minmax(200px, 1fr));
            gap: 15px;
            margin-top: 20px;
        }
        
        .player-card {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 20px;
            border-radius: 12px;
            text-align: center;
            transition: transform 0.3s;
        }
        
        .player-card:hover { transform: translateY(-5px); }
        .player-card h4 { font-size: 1.1em; margin-bottom: 5px; }
        .player-card .team { opacity: 0.9; font-size: 0.9em; margin-bottom: 10px; }
        .player-card .projection { font-size: 2em; font-weight: 700; margin: 10px 0; }
        .player-card .label { opacity: 0.8; font-size: 0.85em; }
        
        footer {
            background: #f8f9fa;
            padding: 30px;
            text-align: center;
            color: #6c757d;
            border-top: 2px solid #e9ecef;
        }
    </style>
</head>
<body>
    <div class="container">
        <header>
            <h1>🏈 NFL Analytics Dashboard</h1>
            <p>Week ', WEEK, ' - Data-Driven Football Analysis</p>
        </header>
        
        <div class="tabs">
            <div class="tab active" onclick="showTab(\'overview\')">Overview</div>
            <div class="tab" onclick="showTab(\'games\')">Game Predictions</div>
            <div class="tab" onclick="showTab(\'players\')">Player Props</div>
            <div class="tab" onclick="showTab(\'explain\')">How It Works</div>
        </div>
        
        <div class="content">
            <div id="overview" class="tab-content active">
                <div class="info-box">
                    <h3>📊 What This Dashboard Shows</h3>
                    <p>This dashboard analyzes NFL games using statistics from every play. It predicts game outcomes, projects individual player performance, and compares these predictions to Vegas oddsmakers.</p>
                </div>
                
                <div class="grid">
                    <div class="metric-box">
                        <div class="metric-value">', nrow(games), '</div>
                        <div class="metric-label">Games Analyzed</div>
                    </div>
                    <div class="metric-box">
                        <div class="metric-value">', nrow(qb), '</div>
                        <div class="metric-label">QBs Projected</div>
                    </div>
                    <div class="metric-box">
                        <div class="metric-value">', nrow(rb), '</div>
                        <div class="metric-label">RBs Projected</div>
                    </div>
                    <div class="metric-box">
                        <div class="metric-value">', nrow(wr), '</div>
                        <div class="metric-label">WRs Projected</div>
                    </div>
                </div>
                
                <div class="card">
                    <h3>💰 Value Opportunities</h3>
                    <p>Games where the model disagrees significantly with Vegas:</p>
                    <div id="valueTable"></div>
                </div>
            </div>
            
            <div id="games" class="tab-content">
                <div class="info-box">
                    <h3>🏈 Game Predictions</h3>
                    <p>Based on EPA (Expected Points Added) - a metric that measures how much each play helps or hurts a team. The model compares offensive strength vs defensive strength to predict outcomes.</p>
                </div>
                
                <div class="card">
                    <h3>📊 All Game Predictions</h3>
                    <div id="gamesTable"></div>
                </div>
            </div>
            
            <div id="players" class="tab-content">
                <div class="info-box">
                    <h3>👥 Player Projections</h3>
                    <p>Predicts yards based on 60% recent form (last 3 games) + 40% season average, adjusted for opponent strength and home field advantage.</p>
                </div>
                
                <div class="card">
                    <h3>🎯 Top Quarterback Projections</h3>
                    <div id="qbGrid" class="player-grid"></div>
                </div>
                
                <div class="card">
                    <h3>🏃 Top Running Back Projections</h3>
                    <div id="rbGrid" class="player-grid"></div>
                </div>
                
                <div class="card">
                    <h3>🙌 Top Receiver Projections</h3>
                    <div id="wrGrid" class="player-grid"></div>
                </div>
            </div>
            
            <div id="explain" class="tab-content">
                <div class="card">
                    <h3>🧠 How This Model Works</h3>
                    
                    <h4 style="margin-top: 25px; color: #667eea;">Step 1: Collect the Data</h4>
                    <p>Downloads every play from every NFL game this season - thousands of plays with details about yards, success, field position, etc.</p>
                    
                    <h4 style="margin-top: 20px; color: #667eea;">Step 2: Calculate Team Strength</h4>
                    <p>Uses <strong>EPA</strong> (Expected Points Added) instead of just points scored. EPA captures that gaining 5 yards on 3rd-and-2 is more valuable than 5 yards on 3rd-and-15.</p>
                    
                    <h4 style="margin-top: 20px; color: #667eea;">Step 3: Predict Matchups</h4>
                    <p>Compares Team A offense vs Team B defense, and vice versa. The team with bigger advantages should win.</p>
                    
                    <h4 style="margin-top: 20px; color: #667eea;">Step 4: Compare to Vegas</h4>
                    <p>When the model disagrees with Vegas by 2.5+ points, that indicates potential analytical value.</p>
                    
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
            <p style="margin-top: 10px; font-size: 0.9em;">Data source: nflverse R packages | Week ', WEEK, ', ', SEASON, '</p>
        </footer>
    </div>
    
    <script>
        // Embedded data
        const qbData = ', qb_json, ';
        const rbData = ', rb_json, ';
        const wrData = ', wr_json, ';
        const oddsData = ', odds_json, ';
        const gamesData = ', games_json, ';
        
        function showTab(tabName) {
            document.querySelectorAll(\'.tab\').forEach(t => t.classList.remove(\'active\'));
            document.querySelectorAll(\'.tab-content\').forEach(c => c.classList.remove(\'active\'));
            
            event.target.classList.add(\'active\');
            document.getElementById(tabName).classList.add(\'active\');
        }
        
        function createTable(data, columns) {
            let html = \'<table><thead><tr>\';
            columns.forEach(col => html += `<th>${col.label}</th>`);
            html += \'</tr></thead><tbody>\';
            
            data.forEach(row => {
                html += \'<tr>\';
                columns.forEach(col => {
                    let val = row[col.key];
                    if (col.format === \'number\') val = typeof val === \'number\' ? val.toFixed(1) : val;
                    html += `<td>${val || \'-\'}</td>`;
                });
                html += \'</tr>\';
            });
            
            html += \'</tbody></table>\';
            return html;
        }
        
        function createPlayerCard(player, type) {
            const yardKey = type === \'qb\' ? \'projected_pass_yards\' : 
                           type === \'rb\' ? \'projected_rush_yards\' : \'projected_rec_yards\';
            const label = type === \'qb\' ? \'Passing Yards\' : 
                         type === \'rb\' ? \'Rushing Yards\' : \'Receiving Yards\';
            
            return `
                <div class="player-card">
                    <h4>${player.player_name}</h4>
                    <div class="team">${player.team} vs ${player.opponent}</div>
                    <div class="projection">${Math.round(player[yardKey])}</div>
                    <div class="label">${label}</div>
                </div>
            `;
        }
        
        // Populate tables
        document.getElementById(\'valueTable\').innerHTML = createTable(oddsData, [
            {key: \'matchup_key\', label: \'Game\'},
            {key: \'ev_tier\', label: \'Value Tier\'},
            {key: \'edge\', label: \'Edge (pts)\', format: \'number\'},
            {key: \'confidence\', label: \'Confidence\'}
        ]);
        
        document.getElementById(\'gamesTable\').innerHTML = createTable(gamesData, [
            {key: \'game\', label: \'Matchup\'},
            {key: \'home_team\', label: \'Home\'},
            {key: \'away_team\', label: \'Away\'},
            {key: \'projected_margin\', label: \'Projected Margin\', format: \'number\'}
        ]);
        
        // Populate player grids
        document.getElementById(\'qbGrid\').innerHTML = qbData.map(p => createPlayerCard(p, \'qb\')).join(\'\');
        document.getElementById(\'rbGrid\').innerHTML = rbData.map(p => createPlayerCard(p, \'rb\')).join(\'\');
        document.getElementById(\'wrGrid\').innerHTML = wrData.map(p => createPlayerCard(p, \'wr\')).join(\'\');
    </script>
</body>
</html>
')

# ============================================================================
# SAVE HTML FILE
# ============================================================================

output_file <- file.path(week_path, paste0("NFL_Analytics_Week", WEEK, ".html"))
writeLines(html_content, output_file)

cat("✓ HTML dashboard generated!\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("File saved to:", output_file, "\n")
cat("══════════════════════════════════════════════════════════════\n\n")
cat("To use:\n")
cat("1. Open the HTML file in any web browser\n")
cat("2. Upload to GitHub Pages, Netlify, or any web host\n")
cat("3. Send the file via email (it\'s self-contained)\n")
cat("4. Works completely offline - no internet needed\n\n")