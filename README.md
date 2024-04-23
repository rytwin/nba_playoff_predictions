# nba_playoff_predictions

Run `scrape_bball_ref_data.R` to get all data. This will call `functions.R`.</br> Then run `combine.R` to combine the data into one csv file.


To do:

-Create features that are from the year(s) before

-Filter and use 1996 - present



-Baseline model: always predict playoffs (playoff probability = 16/30)

-Second baseline model: always predict the same as the year before

-Use feature engineering and feature selection to improve performance



-Try different models, ex: KNN, Naive Bayes, Logistic Regression, Logistic Regression w/ regularization, Random Forest, etc...



-Build on this model to predict probability of advancing to each round as well



-Add opening day rosters with a measure of player value, and add this as a feature to help predictions

-Create separate model to predict player value and use this as a feature instead, and see how that improves performance

