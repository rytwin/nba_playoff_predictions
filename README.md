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
-`player_value.R`: Initial feature selection and feature engineering for player data. Importantly, matches each response variable with data from only prior seasons (instead of the same season as it is when originally scraped).</br>

For modeling: </br>
-`train_team_models.R`: Trains various classification models for playoffs or not


### To do:
-Filter and use 1996 - present</br></br>
-Baseline model: always predict playoffs (playoff probability = approx. 16/30) </br>
-Second baseline model: always predict the same as the year before </br>
-Use feature engineering and feature selection to improve performance </br>
-Add player performance as a feature (but not a model, purely just past performance) </br>
-Add results of player performance model as a feature </br></br>

-Create separate model to predict player value and use this as a feature instead, and see how that improves performance </br>
-Try different models, ex: KNN, Naive Bayes, Logistic Regression, Logistic Regression w/ regularization, Random Forest, etc... </br>
-Build on this model to predict probability of advancing to each round as well
