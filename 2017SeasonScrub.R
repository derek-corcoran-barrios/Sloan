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

shotDataTotal2017$TEAM_NAME <- gsub("Detroit Pistons", "Det", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Atlanta Hawks", "Atl", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Chicago Bulls", "Chi", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Boston Celtics", "Bos", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Cleveland Cavaliers", "Cle", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("New Orleans Pelicans", "NO", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Golden State Warriors", "GSW", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Orlando Magic", "ORL", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Washington Wizards", "Was", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Philadelphia 76ers", "Phi", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Brooklyn Nets", "Bkn", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Utah Jazz", "Uta", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Miami Heat", "Mia", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Charlotte Hornets", "Cha", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Toronto Raptors", "Tor", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Indiana Pacers", "Ind", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Houston Rockets", "Hou", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Denver Nuggets", "Den", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Memphis Grizzlies", "Mem", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("New York Knicks", "NY", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Milwaukee Bucks", "Mil", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Oklahoma City Thunder", "Okc", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("San Antonio Spurs", "Sas", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Dallas Mavericks", "Dal", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Phoenix Suns", "Pho", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Portland Trail Blazers", "Por", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("LA Clippers", "Lac", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Sacramento Kings", "Sac", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Los Angeles Lakers", "Lal", shotDataTotal2017$TEAM_NAME)
shotDataTotal2017$TEAM_NAME <- gsub("Minnesota Timberwolves", "Min", shotDataTotal2017$TEAM_NAME)

     

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

saveRDS(shotDatafDef2017, 'shotDatafDef2017.rds')


######################################################Make 2017 ppaa gird
#####################################

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

#function to make the hexbin dataframe

#shot Comparison function

ShotComparison <- function(OffTeam, DefTown, SeasondataOff, SeasonDataDef, nbins = 40) {
  #Filter the offensive data of the Offensive Team
  Off <- filter(SeasondataOff, TEAM_NAME == OffTeam)
  #Filter the Deffensive data of the Defensive team
  deff <- SeasonDataDef[names(SeasonDataDef) == DefTown][[1]]
  #Get the maximum and minumum values for x and y
  xbnds <- range(c(SeasondataOff$LOC_X, deff$LOC_X))
  ybnds <- range(c(SeasondataOff$LOC_Y, deff$LOC_Y))
  #Make hexbin dataframes out of the teams
  makeHexData <- function(df) {
    h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
    data.frame(hcell2xy(h),
               PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
               ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
               cid = h@cell)
  }
  ##Total NBA data
  Totalhex <- makeHexData(SeasondataOff)
  ##Defensive team data
  Defhex <- makeHexData(deff)
  ##Offensive team data
  Offhex <- makeHexData(Off)
  #Merge offensive and deffensive data with total data by Cell id
  DeffbyCell <- merge(Totalhex, Defhex, by = "cid", all = T)
  OffByCell <- merge(Totalhex, Offhex, by = "cid", all = T)
  ##  when calculating the difference empty cells should count as 0
  DeffbyCell$PPS.x[is.na(DeffbyCell$PPS.x)] <- 0
  DeffbyCell$PPS.y[is.na(DeffbyCell$PPS.y)] <- 0
  DeffbyCell$ST.y[is.na(DeffbyCell$ST.y)] <- 0
  
  OffByCell$PPS.x[is.na(OffByCell$PPS.x)] <- 0
  OffByCell$PPS.y[is.na(OffByCell$PPS.y)] <- 0
  OffByCell$ST.y[is.na(OffByCell$ST.y)] <- 0
  #  make a "difference" data.frame
  DiffDeff <- data.frame(x = ifelse(is.na(DeffbyCell$x.x), DeffbyCell$x.y, DeffbyCell$x.x),
                         y = ifelse(is.na(DeffbyCell$y.x), DeffbyCell$y.y, DeffbyCell$y.x),
                         PPS= DeffbyCell$PPS.y - DeffbyCell$PPS.x,
                         cid= DeffbyCell$cid, 
                         ST = DeffbyCell$ST.y)
  
  DiffOff <- data.frame(x = ifelse(is.na(OffByCell$x.x), OffByCell$x.y, OffByCell$x.x),
                        y = ifelse(is.na(OffByCell$y.x), OffByCell$y.y, OffByCell$y.x),
                        PPS= OffByCell$PPS.y - OffByCell$PPS.x,
                        ST = OffByCell$ST.x,
                        cid = OffByCell$cid, 
                        ST = OffByCell$ST.y)
  #make team comparisons
  Comparison <- merge(DiffOff, DiffDeff, by = "cid", all = T) 
  Comparison <- Comparison[,-c(6:7)]
  Comparison$Diff <- c(Comparison$PPS.x + Comparison$PPS.y)
  
  
  PPSAA <- weighted.mean((Comparison$PPS.x + Comparison$PPS.y), Comparison$ST.x)
  
  
  return(list(PPSAA = PPSAA))
}


Offensive_teams <- as.character(unique(shotDataTotal2017$TEAM_NAME))

defenseve_names <- names(shotDatafDef2017)

df <- data.frame(matrix(ncol = 30, nrow = 30))
colnames(df) <- as.character(unique(shotDataTotal2017$TEAM_NAME))
rownames(df) <- names(shotDatafDef2017)

for (i in 1:length(Offensive_teams)) {
  for (j in 1:length(defenseve_names)){
    df[rownames(df) == defenseve_names[j],colnames(df) == Offensive_teams[i]] <- ShotComparison(OffTeam = Offensive_teams[i], DefTown = defenseve_names[j], SeasondataOff = shotDataTotal2016, SeasonDataDef = shotDatafDef2016, nbins = 30)$PPSAA
  }
}

colnames(df) <- gsub("Detroit.Pistons", "DET", colnames(df))
rownames(df) <- gsub("Detroit", "DET", rownames(df))
colnames(df) <- gsub("Atlanta.Hawks", "ATL", colnames(df))
rownames(df) <- gsub("Atlanta", "ATL", rownames(df))
colnames(df) <- gsub("Chicago.Bulls", "CHI", colnames(df))
rownames(df) <- gsub("Chicago", "CHI", rownames(df))
colnames(df) <- gsub("Boston.Celtics", "BOS", colnames(df))
rownames(df) <- gsub("Boston", "BOS", rownames(df))
colnames(df) <- gsub("Cleveland.Cavaliers", "CLE", colnames(df))
rownames(df) <- gsub("Cleveland", "CLE", rownames(df))
colnames(df) <- gsub("New.Orleans.Pelicans", "NOH", colnames(df))
rownames(df) <- gsub("New Orleans", "NOH", rownames(df))
colnames(df) <- gsub("Golden.State.Warriors", "GS", colnames(df))
rownames(df) <- gsub("Golden State", "GS", rownames(df))
colnames(df) <- gsub("Orlando.Magic", "ORL", colnames(df))
rownames(df) <- gsub("Orlando", "ORL", rownames(df))
colnames(df) <- gsub("Washington.Wizards", "WAS", colnames(df))
rownames(df) <- gsub("Washington", "WAS", rownames(df))
colnames(df) <- gsub("Philadelphia.76ers", "PHI", colnames(df))
rownames(df) <- gsub("Philadelphia", "PHI", rownames(df))
colnames(df) <- gsub("Brooklyn.Nets", "BKN", colnames(df))
rownames(df) <- gsub("Brooklyn", "BKN", rownames(df))
colnames(df) <- gsub("Utah.Jazz", "UTA", colnames(df))
rownames(df) <- gsub("Utah.Jazz", "UTA", rownames(df))
colnames(df) <- gsub("Miami.Heat", "MIA", colnames(df))
rownames(df) <- gsub("Miami", "MIA", rownames(df))
colnames(df) <- gsub("Charlotte.Hornets", "CHR", colnames(df))
rownames(df) <- gsub("Charlotte", "CHR", rownames(df))
colnames(df) <- gsub("Toronto.Raptors", "TOR", colnames(df))
rownames(df) <- gsub("Toronto", "TOR", rownames(df))
colnames(df) <- gsub("Indiana.Pacers", "IND", colnames(df))
rownames(df) <- gsub("Indiana", "IND", rownames(df))
colnames(df) <- gsub("Houston.Rockets", "HOU", colnames(df))
rownames(df) <- gsub("Houston", "HOU", rownames(df))
colnames(df) <- gsub("Denver.Nuggets", "DEN", colnames(df))
rownames(df) <- gsub("Denver", "DEN", rownames(df))
colnames(df) <- gsub("Memphis.Grizzlies", "MEM", colnames(df))
rownames(df) <- gsub("Memphis", "MEM", rownames(df))
colnames(df) <- gsub("New.York.Knicks", "NY", colnames(df))
rownames(df) <- gsub("New York", "NY", rownames(df))
colnames(df) <- gsub("Milwaukee.Bucks", "MIL", colnames(df))
rownames(df) <- gsub("Milwaukee", "MIL", rownames(df))
colnames(df) <- gsub("Oklahoma.City.Thunder", "OKC", colnames(df))
rownames(df) <- gsub("Oklahoma City", "OKC", rownames(df))
colnames(df) <- gsub("San.Antonio.Spurs", "SAN", colnames(df))
rownames(df) <- gsub("San Antonio", "SAN", rownames(df))
colnames(df) <- gsub("Dallas.Mavericks", "DAL", colnames(df))
rownames(df) <- gsub("Dallas", "DAL", rownames(df))
colnames(df) <- gsub("Phoenix.Suns", "PHO", colnames(df))
rownames(df) <- gsub("Phoenix", "PHO", rownames(df))
colnames(df) <- gsub("Portland.Trail.Blazers", "POR", colnames(df))
rownames(df) <- gsub("Portland", "POR", rownames(df))
colnames(df) <- gsub("Los.Angeles.Clippers", "LAC", colnames(df))
rownames(df) <- gsub("LA", "LAC", rownames(df))
colnames(df) <- gsub("Sacramento.Kings", "SAC", colnames(df))
rownames(df) <- gsub("Sacramento", "SAC", rownames(df))
colnames(df) <- gsub("Los.Angeles.Lakers", "LAL", colnames(df))
rownames(df) <- gsub("Los Angeles", "LAL", rownames(df))
colnames(df) <- gsub("Minnesota.Timberwolves", "MIN", colnames(df))
rownames(df) <- gsub("Minnesota", "MIN", rownames(df))

datos2017 <- df

write.csv(datos2017, "datos2017.csv")