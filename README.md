# nba_playoff_predictions

Run `scrape_bball_ref_data.R` to get all data. This will call `functions.R`.

Then run `combine.R` to combine the data into one csv file.


To do:

-Scrape player stats (regular season, playoffs, awards, draft, biographical)

-Update with rosters as of the start of the season


-Create features that are from the year(s) before

-Filter and use 1996 - present


-Baseline model: always predict playoffs (playoff probability = 16/30)

-Second baseline model: always predict the same as the year before

-Use feature engineering and feature selection to improve performance


-Try different models: KNN, Naive Bayes, Logistic Regression, Logistic Regression w/ regularization


-Do the same stats with player stats, and add a classification model for All NBA prediction as a feature to help the predictions