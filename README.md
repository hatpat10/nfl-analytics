# nfl-analytics
A data-driven NFL analytics platform that provides weekly game predictions, player projections, and betting analysis using Expected Points Added (EPA) metrics.


# NFL Analytics Dashboard

A data-driven NFL analytics platform that provides weekly game predictions, player projections, and betting analysis using Expected Points Added (EPA) metrics.

## Features

### Game Analysis
- **EPA-based predictions** for all weekly matchups
- **Projected point spreads** compared against Vegas lines
- **Value opportunities** highlighting games where the model significantly disagrees with oddsmakers
- **Individual game focus view** with detailed team and player breakdowns

### Player Projections
- **Quarterback passing yards** projections
- **Running back rushing yards** projections
- **Wide receiver receiving yards** projections
- Weighted calculation: 60% recent form (last 3 games) + 40% season average
- Adjusted for opponent defensive strength

### Interactive Features
- **Expandable cards** - Click any player, game, or value opportunity to see detailed statistics
- **Game Focus tab** - Select any game to see focused analysis with all relevant players
- **Multiple views** - Overview, Game Predictions, Player Props, and detailed explanations

## How It Works

### Data Collection
The dashboard uses R scripts to download play-by-play data from the nflverse package suite, which provides comprehensive NFL statistics.

### Methodology
1. **EPA Calculation** - Measures how much each play changes expected points (accounts for down, distance, field position)
2. **Team Strength** - Aggregates offensive and defensive EPA across all plays
3. **Matchup Prediction** - Compares Team A offense vs Team B defense (and vice versa)
4. **Vegas Comparison** - Identifies value when model disagrees with betting markets by 2.5+ points

### Statistical Approach
- Home field advantage: +2.5 points
- Player projections: Weighted recent performance with opponent adjustments
- Confidence levels: Based on historical model accuracy and data quality

## Technology Stack

- **Backend**: R (dplyr, jsonlite, nflverse packages)
- **Frontend**: Pure HTML/CSS/JavaScript
- **Hosting**: GitHub Pages (static site)
- **Data Source**: nflverse R packages

## Usage

### Viewing the Dashboard
Simply open the HTML file in any modern web browser. No installation or server required.

**Live site**: [Add your GitHub Pages URL here]

### Updating Data
1. Run the R script `generate_html_dashboard.R` each week
2. Update `WEEK` variable to current NFL week
3. Script downloads latest data and generates new HTML file
4. Upload updated HTML to GitHub to publish

### Focusing on Specific Games
Use the "Game Focus" tab and select any game from the dropdown to see:
- Detailed matchup analysis
- All players from both teams
- Betting edge and recommendations
- Projected outcomes

## Important Disclaimers

- **Research purposes only** - This tool is for statistical analysis and educational purposes
- **No guarantees** - Football has inherent randomness; no model can predict outcomes with certainty
- **Missing factors** - Cannot account for injuries, weather, coaching decisions, or player motivation
- **Not financial advice** - Betting information is analytical only, not recommendations
- **Accuracy testing** - Model performance should be tracked over multiple weeks

## File Structure

```
nfl-analytics/
├── NFL_Analytics_Week5.html     # Self-contained dashboard
├── generate_html_dashboard.R    # Main generator script
├── generate_single_game.R       # Single game focus generator (optional)
└── README.md                    # This file
```

## Data Sources

All data comes from the nflverse project:
- Play-by-play data
- Team statistics  
- Player performance metrics
- Schedule information

**Credit**: nflverse contributors and the R community

## Updates

Dashboard is updated weekly during NFL season:
- Generated after Thursday games complete
- Updated throughout weekend as needed
- Final version published Monday

## Contributing

This is a personal analytics project. If you'd like to suggest improvements:
1. Open an issue describing the enhancement
2. Fork the repository
3. Submit a pull request with changes

## License

Data from nflverse is provided under their respective licenses. Dashboard code is provided as-is for personal use.

## Contact

[Add your contact information or leave blank]

---

**Week 5, 2025 Season** | Built with R and nflverse
