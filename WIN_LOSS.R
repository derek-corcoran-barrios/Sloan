FinalOdds <- read.csv("FinalOdds.csv")
FinalOdds <- FinalOdds[,-1]

library(dismo)
library(gbm)
library(caret)

ctrl <- trainControl(method = "repeatedcv", number=10, repeats=3)


grid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                    n.trees = seq(100, 1000, by = 50),
                    shrinkage = c(0.01, 0.1),
                    n.minobsinnode=c(1,5,10))


# train the GBM model
set.seed(7)
modelGbmforcastWL <- train(x = FinalOdds[,14:15],y = FinalOdds[,6], method = "gbm",  preProcess = c("center", "scale"), verbose = FALSE, trControl = ctrl, tuneGrid = grid)
saveRDS(modelGbmforcastWL, "WL_function.rds")
##############

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
future_games$HomeRes <- NA
#######load data and functions, not needed in the real one
shotDataTotal2017<- readRDS("shotDataTotal2017.rds")
shotDatafDef2017 <- readRDS("shotDatafDef2017.rds")

pacman::p_load(rjson, grid, gridExtra, png, RCurl, ggplot2, jpeg, hexbin, sp, knitr, raster, rasterVis, dplyr)

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


#######
for(i in 1:NROW(future_games)) {
  future_games$defAPPS[i] <- ComparisonPPS(OffTeam = future_games$`Home/Neutral`[i], DefTown = future_games$`Visitor/Neutral`[i], SeasondataOff = shotDataTotal2017, SeasonDataDef = shotDatafDef2017)
}

for(i in 1:NROW(future_games)) {
  future_games$offAPPS[i] <- ComparisonPPS(OffTeam = future_games$`Visitor/Neutral`[i], DefTown = future_games$`Home/Neutral`[i], SeasondataOff = shotDataTotal2017, SeasonDataDef = shotDatafDef2017)
}

pacman::p_load(caret)

###############
#get win loss probability for each game
ForecastWL <- predict(modelGbmforcastWL, data.frame(defAPPS = unlist(future_games[,10]), offAPPS = unlist(future_games[,11])), type = "prob")
colnames(ForecastWL)<- c("Prob_W", "Prob_L")

future_games<- cbind(future_games, ForecastWL)

#####

#Simulate results, each i element of the list is one season
Seasons <- list()
for (i in 1:100){
  Results <- list()
  for(j in 1:nrow(future_games)){
    Results[[j]] <- sample(c("W", "L"), size = 1, replace = TRUE, prob = future_games[j,c(13,14)])
  }
  Seasons[[i]] <- future_games
  Seasons[[i]]$Home <- unlist(Results)
  Seasons[[i]]$Visit <- ifelse(Seasons[[i]]$Home == "W", "L", "W")
  
}
