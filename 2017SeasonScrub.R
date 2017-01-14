library(rjson)
library(grid)
library(gridExtra)
library(png)
library(RCurl)
library(ggplot2)
library(jpeg)
library(hexbin)
library(sp)
library(knitr)
library(raster)
library(rasterVis)
library(dplyr)

by_team <- read.csv("~/Sloan/by_team.csv")
by_team <- by_team[-1,-1]

teamID <- by_team$team_id
teamName <- by_team$team_city
defensiveURL <- list()
shotData <- list()
shotDatafDef <- list()


shotURLtotal <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2016-17&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=0&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0&PlayerPosition=", sep = "")

# import from JSON
shotDataTotal <- fromJSON(file = shotURLtotal, method="C")

# unlist shot data, save into a data frame
shotchartTotal2017 <- data.frame(matrix(ncol = 24, nrow = 1))

colnames(shotchartTotal2017)<- shotDataTotal$resultSets[[1]][[2]]

#fist shot of the season
for(i in 1:length(shotDataTotal$resultSets[[1]][[3]])){
  shotchartTotal2017[i,] <- unlist(shotDataTotal$resultSets[[1]][[3]][[i]])
}

# covert x and y coordinates into numeric
shotchartTotal2017$LOC_X <- as.numeric(as.character(shotchartTotal2017$LOC_X))
shotchartTotal2017$LOC_Y <- as.numeric(as.character(shotchartTotal2017$LOC_Y))
shotchartTotal2017$SHOT_DISTANCE <- as.numeric(as.character(shotchartTotal2017$SHOT_DISTANCE))


shotDataTotal2017 <- shotchartTotal2017

saveRDS(shotDataTotal2017, 'shotDataTotal2017.rds')

shotDatafDef2017 <- shotDatafDef

####

#Parameter names
shotchartTotal2017 <- data.frame(matrix(ncol = 24, nrow = 1))

colnames(shotchartTotal2017)<- shotDataTotal$resultSets[[1]][[2]]

#fist shot of the season
for(i in 1:length(shotDataTotal$resultSets[[1]][[3]])){
shotchartTotal2017[i,] <- unlist(shotDataTotal$resultSets[[1]][[3]][[i]])
}



#####Defensive end


for (i in 1:length(teamID)){
  defensiveURL[[i]] <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2016-17&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=", by_team$team_id[i],"&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=0&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0&PlayerPosition=", sep = "")
  
  # import from JSON
  shotData[[i]] <- fromJSON(file = defensiveURL[[i]], method="C")
  
  # unlist shot data, save into a data frame
  shotDatafDef[[i]] <- data.frame(matrix(ncol = 24, nrow = 1))
  colnames(shotDatafDef[[i]]) <-  shotData[[i]]$resultSets[[1]][[2]]
  
  for(j in 1:length(shotData[[i]]$resultSets[[1]][[3]])){
    shotDatafDef[[i]][j,] <- unlist(shotData[[i]]$resultSets[[1]][[3]][[j]])
  }
  
  # covert x and y coordinates into numeric
  shotDatafDef[[i]]$LOC_X <- as.numeric(as.character(shotDatafDef[[i]]$LOC_X))
  shotDatafDef[[i]]$LOC_Y <- as.numeric(as.character(shotDatafDef[[i]]$LOC_Y))
  shotDatafDef[[i]]$SHOT_DISTANCE <- as.numeric(as.character(shotDatafDef[[i]]$SHOT_DISTANCE))
}

names(shotDatafDef) <- teamName

shotDatafDef2017 <- shotDatafDef
