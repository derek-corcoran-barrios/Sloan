if (!require("pacman")) install.packages("pacman")

pacman::p_load(rjson, grid, gridExtra, png, RCurl, ggplot2, jpeg, hexbin, sp, knitr, raster, rasterVis, dplyr)

#library(rjson)
#library(grid)
#library(gridExtra)
#library(png)
#library(RCurl)
#library(ggplot2)
#library(jpeg)
#library(hexbin)
#library(sp)
#library(knitr)
#library(raster)
#library(rasterVis)
#library(dplyr)

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

names(shotDatafDef2017) <- c("Atl", "Bos", "Cle", "NO", "Chi", "Dal", "Den", "GSW", "Hou", "Lac", "Lal", "Mia", "Mil", "Min", "Bkn", "NY", "ORL", "Ind", "Phi", "Pho", "Por", "Sac", "Sas", "Okc", "Tor", "Uta", "Mem", "Was", "Det", "Cha")

saveRDS(shotDatafDef2017, 'shotDatafDef2017.rds')


######################################################Make 2017 ppaa gird
#####################################

#function to make the hexbin dataframe

#shot Comparison function

ComparisonPPS <- function(OffTeam, DefTown, SeasondataOff, SeasonDataDef, nbins = 40) {
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
    df[rownames(df) == defenseve_names[j],colnames(df) == Offensive_teams[i]] <- ComparisonPPS(OffTeam = Offensive_teams[i], DefTown = defenseve_names[j], SeasondataOff = shotDataTotal2017, SeasonDataDef = shotDatafDef2017, nbins = 30)$PPSAA
  }
}


datos2017 <- df

write.csv(datos2017, "datos2017.csv")

###Best defense against Golden State

# View(df[order(df$GSW),])
####################ScheduleScraper
pacman::p_load(XML, lubridate)


Months <- c("october", "november", "december", "january", "february", "march", "april")

URLs <- paste("http://www.basketball-reference.com/leagues/NBA_2017_games-", Months,".html", sep = "")


schedule <- list()
# import from JSON

for(i in 1:length(Months)){
  schedule[[i]] <- readHTMLTable(URLs[[i]])
  schedule[[i]] <- schedule[[i]]$schedule
}

schedule <- do.call("rbind", schedule)

schedule$Date <- mdy(as.character(schedule$Date))
schedule[,4] <- as.numeric(as.character(schedule[,4]))
schedule[,6] <- as.numeric(as.character(schedule[,6]))

schedule[,3] <- gsub("Detroit Pistons", "Det", schedule[,3])
schedule[,3] <- gsub("Atlanta Hawks", "Atl", schedule[,3])
schedule[,3] <- gsub("Chicago Bulls", "Chi", schedule[,3])
schedule[,3] <- gsub("Boston Celtics", "Bos", schedule[,3])
schedule[,3] <- gsub("Cleveland Cavaliers", "Cle", schedule[,3])
schedule[,3] <- gsub("New Orleans Pelicans", "NO", schedule[,3])
schedule[,3] <- gsub("Golden State Warriors", "GSW", schedule[,3])
schedule[,3] <- gsub("Orlando Magic", "ORL", schedule[,3])
schedule[,3] <- gsub("Washington Wizards", "Was", schedule[,3])
schedule[,3] <- gsub("Philadelphia 76ers", "Phi", schedule[,3])
schedule[,3] <- gsub("Brooklyn Nets", "Bkn", schedule[,3])
schedule[,3] <- gsub("Utah Jazz", "Uta", schedule[,3])
schedule[,3] <- gsub("Miami Heat", "Mia", schedule[,3])
schedule[,3] <- gsub("Charlotte Hornets", "Cha", schedule[,3])
schedule[,3] <- gsub("Toronto Raptors", "Tor", schedule[,3])
schedule[,3] <- gsub("Indiana Pacers", "Ind", schedule[,3])
schedule[,3] <- gsub("Houston Rockets", "Hou", schedule[,3])
schedule[,3] <- gsub("Denver Nuggets", "Den", schedule[,3])
schedule[,3] <- gsub("Memphis Grizzlies", "Mem", schedule[,3])
schedule[,3] <- gsub("New York Knicks", "NY", schedule[,3])
schedule[,3] <- gsub("Milwaukee Bucks", "Mil", schedule[,3])
schedule[,3] <- gsub("Oklahoma City Thunder", "Okc", schedule[,3])
schedule[,3] <- gsub("San Antonio Spurs", "Sas", schedule[,3])
schedule[,3] <- gsub("Dallas Mavericks", "Dal", schedule[,3])
schedule[,3] <- gsub("Phoenix Suns", "Pho", schedule[,3])
schedule[,3] <- gsub("Portland Trail Blazers", "Por", schedule[,3])
schedule[,3] <- gsub("Los Angeles Clippers", "Lac", schedule[,3])
schedule[,3] <- gsub("Sacramento Kings", "Sac", schedule[,3])
schedule[,3] <- gsub("Los Angeles Lakers", "Lal", schedule[,3])
schedule[,3] <- gsub("Minnesota Timberwolves", "Min", schedule[,3])

schedule[,5] <- gsub("Detroit Pistons", "Det", schedule[,5])
schedule[,5] <- gsub("Atlanta Hawks", "Atl", schedule[,5])
schedule[,5] <- gsub("Chicago Bulls", "Chi", schedule[,5])
schedule[,5] <- gsub("Boston Celtics", "Bos", schedule[,5])
schedule[,5] <- gsub("Cleveland Cavaliers", "Cle", schedule[,5])
schedule[,5] <- gsub("New Orleans Pelicans", "NO", schedule[,5])
schedule[,5] <- gsub("Golden State Warriors", "GSW", schedule[,5])
schedule[,5] <- gsub("Orlando Magic", "ORL", schedule[,5])
schedule[,5] <- gsub("Washington Wizards", "Was", schedule[,5])
schedule[,5] <- gsub("Philadelphia 76ers", "Phi", schedule[,5])
schedule[,5] <- gsub("Brooklyn Nets", "Bkn", schedule[,5])
schedule[,5] <- gsub("Utah Jazz", "Uta", schedule[,5])
schedule[,5] <- gsub("Miami Heat", "Mia", schedule[,5])
schedule[,5] <- gsub("Charlotte Hornets", "Cha", schedule[,5])
schedule[,5] <- gsub("Toronto Raptors", "Tor", schedule[,5])
schedule[,5] <- gsub("Indiana Pacers", "Ind", schedule[,5])
schedule[,5] <- gsub("Houston Rockets", "Hou", schedule[,5])
schedule[,5] <- gsub("Denver Nuggets", "Den", schedule[,5])
schedule[,5] <- gsub("Memphis Grizzlies", "Mem", schedule[,5])
schedule[,5] <- gsub("New York Knicks", "NY", schedule[,5])
schedule[,5] <- gsub("Milwaukee Bucks", "Mil", schedule[,5])
schedule[,5] <- gsub("Oklahoma City Thunder", "Okc", schedule[,5])
schedule[,5] <- gsub("San Antonio Spurs", "Sas", schedule[,5])
schedule[,5] <- gsub("Dallas Mavericks", "Dal", schedule[,5])
schedule[,5] <- gsub("Phoenix Suns", "Pho", schedule[,5])
schedule[,5] <- gsub("Portland Trail Blazers", "Por", schedule[,5])
schedule[,5] <- gsub("Los Angeles Clippers", "Lac", schedule[,5])
schedule[,5] <- gsub("Sacramento Kings", "Sac", schedule[,5])
schedule[,5] <- gsub("Los Angeles Lakers", "Lal", schedule[,5])
schedule[,5] <- gsub("Minnesota Timberwolves", "Min", schedule[,5])

  

future_games <- schedule[schedule$Date >= Sys.Date(),]
future_games$defAPPS <- NA
future_games$offAPPS <- NA
future_games$spread <- NA

for(i in 1:NROW(future_games)) {
  future_games$defAPPS[i] <- ComparisonPPS(OffTeam = future_games$`Home/Neutral`[i], DefTown = future_games$`Visitor/Neutral`[i], SeasondataOff = shotDataTotal2017, SeasonDataDef = shotDatafDef2017)
}

for(i in 1:NROW(future_games)) {
  future_games$offAPPS[i] <- ComparisonPPS(OffTeam = future_games$`Visitor/Neutral`[i], DefTown = future_games$`Home/Neutral`[i], SeasondataOff = shotDataTotal2017, SeasonDataDef = shotDatafDef2017)
}

pacman::p_load(caret)

BRT <- readRDS("BRT2017_17_Jan.rds")

future_games$spread <- predict(BRT, data.frame(defAPPS = unlist(future_games$defAPPS), offAPPS = unlist(future_games$offAPPS)), type="raw")


future_games <- future_games[,c(1,3,5,12)]


future_games$Home <- ifelse(future_games$spread < 0, "W", "L")

future_games$Visit <- ifelse(future_games$spread > 0, "W", "L")

write.csv(future_games, "future_games.csv")
saveRDS(future_games, "future_games.rds")


Home <- cbind(future_games$`Home/Neutral`, future_games$Home)

colnames(Home) <- c("Team", "Result")

Visit <- cbind(future_games$`Visitor/Neutral`, future_games$Visit)

colnames(Visit) <- c("Team", "Result")

AddedStand <- data.frame(rbind(Home, Visit))
#Wins
AddedStand_W <- dplyr::filter(AddedStand, Result == "W")

AddedStand_W <- group_by(AddedStand_W, Team)

AddedStand_W <- dplyr::summarize(AddedStand_W, W = n())

#Loses

AddedStand_L <- dplyr::filter(AddedStand, Result == "L")

AddedStand_L <- group_by(AddedStand_L, Team)

AddedStand_L <- dplyr::summarize(AddedStand_L, L = n())

AddedStand <- merge.data.frame(AddedStand_W, AddedStand_L, all = TRUE)

write.csv(AddedStand, "AddedStand.csv")
saveRDS(future_games, "future_games.rds")

#####Standing scraper

Standings <- "http://www.basketball-reference.com/leagues/NBA_2017.html"

Standings <- readHTMLTable(Standings)

Standings <- list(Western = Standings$confs_standings_W, Eastern = Standings$confs_standings_E)

Standings[[1]]$Conference <- c("West")

Standings[[2]]$Conference <- c("East")

colnames(Standings[[1]]) <- c("Team", "Current-W", "Current-L", "pct", "GB", "PS/G", "PA/G", "SRS", "Conference")

colnames(Standings[[2]]) <- c("Team", "Current-W", "Current-L", "pct", "GB", "PS/G", "PA/G", "SRS", "Conference")


Standings <- rbind(Standings[[1]], Standings[[2]])

Standings <- Standings[,c(1,2,3,9)]

Standings$Team <- gsub("76ers", "Phi", Standings$Team)


Standings$Team <- gsub("(?<=\\b[A-Z])[^A-Z]+", "", Standings$Team, perl = TRUE)

Standings$Team <- gsub("DP", "Det", Standings$Team)
Standings$Team<- gsub("AH", "Atl", Standings$Team)
Standings$Team <- gsub("CB", "Chi", Standings$Team)
Standings$Team<- gsub("BC", "Bos", Standings$Team)
Standings$Team<- gsub("CC", "Cle", Standings$Team)
Standings$Team<- gsub("NOP", "NO", Standings$Team)
Standings$Team<- gsub("OM", "ORL", Standings$Team)
Standings$Team<- gsub("WW", "Was", Standings$Team)
Standings$Team<- gsub("BN", "Bkn", Standings$Team)
Standings$Team<- gsub("UJ", "Uta", Standings$Team)
Standings$Team<- gsub("MH", "Mia", Standings$Team)
Standings$Team<- gsub("CH", "Cha", Standings$Team)
Standings$Team<- gsub("TR", "Tor", Standings$Team)
Standings$Team<- gsub("IP", "Ind", Standings$Team)
Standings$Team<- gsub("HR", "Hou", Standings$Team)
Standings$Team<- gsub("DN", "Den", Standings$Team)
Standings$Team<- gsub("MG", "Mem", Standings$Team)
Standings$Team<- gsub("NYK", "NY", Standings$Team)
Standings$Team<- gsub("MB", "Mil", Standings$Team)
Standings$Team<- gsub("OCT", "Okc", Standings$Team)
Standings$Team<- gsub("SAS", "Sas", Standings$Team)
Standings$Team<- gsub("DM", "Dal", Standings$Team)
Standings$Team<- gsub("PS", "Pho", Standings$Team)
Standings$Team<- gsub("PTB", "Por", Standings$Team)
Standings$Team<- gsub("LAC", "Lac", Standings$Team)
Standings$Team<- gsub("SK", "Sac", Standings$Team)
Standings$Team<- gsub("LAL", "Lal", Standings$Team)
Standings$Team<- gsub("MT", "Min", Standings$Team)
Standings$Team<- gsub("PP", "Phi", Standings$Team)

ProjStand <- merge.data.frame(Standings, AddedStand, all = TRUE)

ProjStand[,2] <- as.numeric(as.character(ProjStand[,2]))
ProjStand[,3] <- as.numeric(as.character(ProjStand[,3]))


ProjStand$W <- ProjStand$`Current-W`+ProjStand$W
ProjStand$L <- ProjStand$`Current-L`+ProjStand$L

ProjStand <- arrange(ProjStand, Conference,desc(W))


colnames(ProjStand) <- c("Team", "Current-W", "Current-L", "Conference", "Projected-W", "Projected-L")

write.csv(ProjStand, "ProjStand.csv")
saveRDS(ProjStand, "ProjStand.rds")

ProjStandW <- dplyr::filter(ProjStand, Conference == "West")

ProjStandE <- dplyr::filter(ProjStand, Conference == "East")

write.csv(ProjStandE, "ProjStandE.csv")
write.csv(ProjStandW, "ProjStandW.csv")
