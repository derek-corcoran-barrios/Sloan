
pacman::p_load(XML, lubridate)


#Gather data
## Year 2012

URL <- "http://www.basketball-reference.com/playoffs/NBA_2017_games.html"

schedule <- readHTMLTable(URL)

schedule <- schedule[[1]]


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
schedule[,3] <- gsub("Charlotte Bobcats", "Cha", schedule[,3])
schedule[,3]<- gsub("New Orleans Hornets", "NO", schedule[,3])



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
schedule[,5] <- gsub("Charlotte Bobcats", "Cha", schedule[,5])
schedule[,5]<- gsub("New Orleans Hornets", "NO", schedule[,5])


schedule$Date <- mdy(as.character(schedule$Date))
schedule[,4] <- as.numeric(as.character(schedule[,4]))
schedule[,6] <- as.numeric(as.character(schedule[,6]))

schedule$defAPPS <- NA
schedule$offAPPS <- NA
schedule$HomeRes <- NA

schedule <- schedule[,-c(2,7:9)]


colnames(schedule) <- c("Date", "Visitor", "Visit_PTS", "Home", "Home_PTS", "defAPPS", "offAPPS", "HomeRes")

schedule$Season <- 2017


schedule$HomeRes <- schedule$Visit_PTS - schedule$Home_PTS

library(SpatialBall)

data("season2017")

Playoffs2017 <- list()

for (i in 1:nrow(schedule)){
  Playoffs2017[[i]] <- Get_Apps(HomeTeam = schedule$Home[i], VisitorTeam = schedule$Visitor[i], Seasondata = season2017)
  }

Playoffs2017 <- do.call(rbind, Playoffs2017)

schedule$defAPPS <- Playoffs2017$defAPPS

schedule$offAPPS <- Playoffs2017$offAPPS

schedule$PredBRT <- Playoffs2017$spread

schedule$Type <- "Playoffs"


Playoffs2017 <- schedule

saveRDS(Playoffs2017, "Playoffs2017.rds")

season2016 <- readRDS("shotDataTotal2016.rds")

tests <- readRDS("tests.rds")
test <- tests[[7]]

test$PredBRT <- NA

for (i in 1:nrow(test)){
  if (test$Season == 2017){
  test$PredBRT[i] <- Get_Apps(HomeTeam = test$Home[i], VisitorTeam = test$Visitor[i], Seasondata = season2017)$spread
  }
  else if (test$Season == 2016){
    test$PredBRT[i] <- Get_Apps(HomeTeam = test$Home[i], VisitorTeam = test$Visitor[i], Seasondata = season2016)$spread
    
  }
}


Test2017 <- merge(test, Playoffs2017, all = TRUE)

saveRDS(Test2017, "Test2017.rds")

write.csv(Test2017, "Test2017.csv")