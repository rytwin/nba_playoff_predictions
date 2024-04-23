# nba_playoff_predictions

### Files

-`functions.R`: Has helper functions for many of the scripts. This will be called from the scripts as necessary and doesn't need to be used directly.</br>

For scraping:</br>
-`scrape_bball_ref_data.R`: Scrapes team statistics from basketball-reference.com. Saves as multiple csv files, you will use the next script to merge them.</br>
-`combine.R`: Combines team data into one csv file. </br>
-`scrape_new_data.R`: Scrapes new team statistics and adds to existing data previously obtained using `scrape_bball_ref_data.R` and `combine.R`. </br>
-`scrape_bball_ref_player_data.R`: Scrapes player statistics (including awards and draft data) from basketball-reference.com. Saves as one csv file.</br>

For initial feature selection and engineering: </br>
-`feature_engineering.R`: Initial feature selection and feature engineering for team data. Importantly, matches each response variable with data from only prior seasons (instead of the same season as it is when originally scraped).</br>
-`player_value.R`: Initial feature selection and feature engineering for player data. Importantly, matches each response variable with data from only prior seasons (instead of the same season as it is when originally scraped).</br>


To do:

-Filter and use 1996 - present



-Baseline model: always predict playoffs (playoff probability = 16/30)

-Second baseline model: always predict the same as the year before

-Use feature engineering and feature selection to improve performance



-Try different models, ex: KNN, Naive Bayes, Logistic Regression, Logistic Regression w/ regularization, Random Forest, etc...



-Build on this model to predict probability of advancing to each round as well



-Add opening day rosters with a measure of player value, and add this as a feature to help predictions

-Create separate model to predict player value and use this as a feature instead, and see how that improves performance

