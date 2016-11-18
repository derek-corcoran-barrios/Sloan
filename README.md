#Spatial data in the NBA

##Scraping and gathering the data

The file *SloanConference.rmd* is the one that scraped the data of the shot charts.

##Gathered data

The scraped data is stored as RDS files, where the offensive and deffensive shots for each season where separated by team as *shotDatafDef2013.rds* for defensive shot charts and *shotDataTotal2013.rds* for offensive shot charts for each year from 2012 to 2016.

##Difference.R

File to make the function *ShotComparison*, to compare between 2 teams in a season, the files called *datos2012.csv* to *datos2016.csv* where made with the loop at the end of the script


##ShotPlots.R

File to make function *ShotComparisonGraph* to plot the differences between 2 teams

##Boosted Regression Tree

In the DATASET.R script boosted Regression tree and plots come from the odds in vegas vs our odds, all graphs and models for that come from these algorithms.

###NBAOdds data

The NBAOdds csv file has 6 columns that we will use:

**Date**: Date the game was played.

**Away**: Away team.

**Score (3rd column)**: Score from the Away team.

**Home**: Home team.

**Score (5th column)**: Score from the home team.

**Home Spread**: Predicted difference (spread) by Vegas, negative values indicate the home team winning and positive values indicate the away team winning.

We added the following columns:

**Diff**: The observed data, that is the away score - the home score, that will result in a negative value if the home team won, compared to a possitive value if the home team wins

**Year**: The year of the result

**Defapps** The above average points per shot of the home team when the away team is defending

**Offapps** The above average points per shot of the Away team when the home team is Attacking

The resulting Dataframe was exported as *FinalOdds.csv*

After this a Boosted Regression Trees was fitted in R



#Comparison

RMSE was used to select the optimal model using  the smallest value.
The final values used for the model were n.trees = 200, interaction.depth = 1, shrinkage
 = 0.01 and n.minobsinnode = 10. 

##Model without pace adjust 

###Our model
      RMSE |  Rsquared
----------------------- 
15.7584135 | 0.1211844 

###Vegas

      RMSE |  Rsquared 
----------------------
14.0461242 | 0.2934513 

###Variable importance

    var |  rel.inf
--------------------
defAPPS |  53.66145
offAPPS |  46.33855


#Comparison

##Model with pace adjust 

RMSE was used to select the optimal model using  the smallest value.
The final values used for the model were n.trees = 250, interaction.depth = 1, shrinkage
 = 0.01 and n.minobsinnode = 10. 

###Our model


      RMSE |  Rsquared 
-----------------------
15.6367226 | 0.1226283 

###Vegas

      RMSE |  Rsquared
------------------------- 
14.0461242 | 0.2934513 


###Variable importance

    var |  rel.inf
--------------------
defAPPS |  47.35940
offAPPS |  41.66317
Pace    |  10.97743
