# nba_playoff_predictions

### Files

-`functions.R`: Has helper functions for many of the scripts. This will be called from the scripts as necessary and doesn't need to be used directly.</br>

For scraping:</br>
-`scrape_bball_ref_data.R`: Scrapes team statistics from basketball-reference.com. Saves as multiple csv files, you will use the next script to merge them.</br>
-`combine.R`: Combines team data into one csv file. </br>
-`scrape_new_data.R`: Scrapes new team statistics and adds to existing data previously obtained using `scrape_bball_ref_data.R`. Saves as multiple csv files, and you could/would merge them using `combine.R`. </br>
-`scrape_bball_ref_player_data.R`: Scrapes player statistics (including awards and draft data) from basketball-reference.com. Saves as one csv file.</br>

For initial feature selection and engineering: </br>
-`feature_engineering.R`: Initial feature selection and feature engineering for team data. Importantly, matches each response variable with data from only prior seasons (instead of the same season as it is when originally scraped).</br>
-`player_value.R`: Initial feature selection and feature engineering for player data. Importantly, matches each response variable with data from only prior seasons (instead of the same season as it is when originally scraped). Also groups player data by team and by opening day team to use as variables for team models.</br>

For modeling: </br>
-`train_team_models.R`: Trains various classification models for playoffs or not </br>
-`train_player_models.R`: Trains various classification models for whether a player makes an all-nba team or not

Model comparison: </br>
-`model_comparison.R`: Compares final performance metrics for different models
