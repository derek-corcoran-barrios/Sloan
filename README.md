#Spatial data in the NBA

##Scraping and gathering the data

The file *SloanConference.rmd* is the one that scraped the data of the shot charts.

##Gathered data

The scraped data is stored as RDS files, where the offensive and deffensive shots for each season where separated by team as *shotDatafDef2013.rds* for defensive shot charts and *shotDataTotal2013.rds* for offensive shot charts for each year from 2012 to 2016.

##Boosted Regression Tree

In the DATASET.R script boosted Regression tree and plots come from the odds in vegas vs our odds, all graphs and models for that come from these algorithms.

###NBAOdds data

The NBAOdds csv file has 6 columns that we will use:

*Date*: Date the game was played.

*Away*: Away team.

*Score (3rd column)*: Score from the Away team.

*Home*: Home team.

*Score (5th column)*: Score from the home team.

*Home Spread*: Predicted difference (spread) by Vegas, negative values indicate the home team winning and positive values indicate the away team winning.

