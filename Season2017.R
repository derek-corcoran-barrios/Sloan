#Data 2018

if (!require("pacman")) install.packages("pacman")

pacman::p_load(rjson, grid, gridExtra, png, RCurl, ggplot2, jpeg, hexbin, sp, knitr, dplyr, lubridate)

by_team <- readRDS("by_team.rds")

teamID <- by_team$team_id
teamName <- by_team$team_city
Season2018 <- list()


shotURLtotal <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2017-18&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=0&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2017-18&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0&PlayerPosition=", sep = "")

# import from JSON
Season2018 <- fromJSON(file = shotURLtotal, method="C")
Names <- Season2018$resultSets[[1]][[2]]
# unlist shot data, save into a data frame
Season2018 <- data.frame(matrix(unlist(Season2018$resultSets[[1]][[3]]), ncol = 24, byrow = TRUE))

colnames(Season2018) <- Names


# covert x and y coordinates into numeric
Season2018$LOC_X <- as.numeric(as.character(Season2018$LOC_X))
Season2018$LOC_Y <- as.numeric(as.character(Season2018$LOC_Y))


Season2018$TEAM_NAME <- gsub("Detroit Pistons", "Det", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Atlanta Hawks", "Atl", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Chicago Bulls", "Chi", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Boston Celtics", "Bos", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Cleveland Cavaliers", "Cle", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("New Orleans Pelicans", "NO", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Golden State Warriors", "GSW", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Orlando Magic", "ORL", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Washington Wizards", "Was", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Philadelphia 76ers", "Phi", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Brooklyn Nets", "Bkn", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Utah Jazz", "Uta", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Miami Heat", "Mia", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Charlotte Hornets", "Cha", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Toronto Raptors", "Tor", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Indiana Pacers", "Ind", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Houston Rockets", "Hou", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Denver Nuggets", "Den", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Memphis Grizzlies", "Mem", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("New York Knicks", "NY", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Milwaukee Bucks", "Mil", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Oklahoma City Thunder", "Okc", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("San Antonio Spurs", "Sas", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Dallas Mavericks", "Dal", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Phoenix Suns", "Pho", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Portland Trail Blazers", "Por", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("LA Clippers", "Lac", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Sacramento Kings", "Sac", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Los Angeles Lakers", "Lal", Season2018$TEAM_NAME)
Season2018$TEAM_NAME <- gsub("Minnesota Timberwolves", "Min", Season2018$TEAM_NAME)


####HOME VISITOR


Season2018$HTM <- gsub("DET", "Det", Season2018$HTM)
Season2018$HTM <- gsub("ATL", "Atl", Season2018$HTM)
Season2018$HTM <- gsub("CHI", "Chi", Season2018$HTM)
Season2018$HTM <- gsub("BOS", "Bos", Season2018$HTM)
Season2018$HTM <- gsub("CLE", "Cle", Season2018$HTM)
Season2018$HTM <- gsub("NOP", "NO", Season2018$HTM)
Season2018$HTM <- gsub("GSW", "GSW", Season2018$HTM)
Season2018$HTM <- gsub("ORL", "ORL", Season2018$HTM)
Season2018$HTM <- gsub("WAS", "Was", Season2018$HTM)
Season2018$HTM <- gsub("PHI", "Phi", Season2018$HTM)
Season2018$HTM <- gsub("BKN", "Bkn", Season2018$HTM)
Season2018$HTM <- gsub("UTA", "Uta", Season2018$HTM)
Season2018$HTM <- gsub("MIA", "Mia", Season2018$HTM)
Season2018$HTM <- gsub("CHA", "Cha", Season2018$HTM)
Season2018$HTM <- gsub("TOR", "Tor", Season2018$HTM)
Season2018$HTM <- gsub("IND", "Ind", Season2018$HTM)
Season2018$HTM <- gsub("HOU", "Hou", Season2018$HTM)
Season2018$HTM <- gsub("DEN", "Den", Season2018$HTM)
Season2018$HTM <- gsub("MEM", "Mem", Season2018$HTM)
Season2018$HTM <- gsub("NYK", "NY", Season2018$HTM)
Season2018$HTM <- gsub("MIL", "Mil", Season2018$HTM)
Season2018$HTM <- gsub("OKC", "Okc", Season2018$HTM)
Season2018$HTM <- gsub("SAS", "Sas", Season2018$HTM)
Season2018$HTM <- gsub("DAL", "Dal", Season2018$HTM)
Season2018$HTM <- gsub("PHX", "Pho", Season2018$HTM)
Season2018$HTM <- gsub("POR", "Por", Season2018$HTM)
Season2018$HTM <- gsub("LAC", "Lac", Season2018$HTM)
Season2018$HTM <- gsub("SAC", "Sac", Season2018$HTM)
Season2018$HTM <- gsub("LAL", "Lal", Season2018$HTM)
Season2018$HTM <- gsub("MIN", "Min", Season2018$HTM)

###Visitor


Season2018$VTM <- gsub("DET", "Det", Season2018$VTM)
Season2018$VTM <- gsub("ATL", "Atl", Season2018$VTM)
Season2018$VTM <- gsub("CHI", "Chi", Season2018$VTM)
Season2018$VTM <- gsub("BOS", "Bos", Season2018$VTM)
Season2018$VTM <- gsub("CLE", "Cle", Season2018$VTM)
Season2018$VTM <- gsub("NOP", "NO", Season2018$VTM)
Season2018$VTM <- gsub("GSW", "GSW", Season2018$VTM)
Season2018$VTM <- gsub("ORL", "ORL", Season2018$VTM)
Season2018$VTM <- gsub("WAS", "Was", Season2018$VTM)
Season2018$VTM <- gsub("PHI", "Phi", Season2018$VTM)
Season2018$VTM <- gsub("BKN", "Bkn", Season2018$VTM)
Season2018$VTM <- gsub("UTA", "Uta", Season2018$VTM)
Season2018$VTM <- gsub("MIA", "Mia", Season2018$VTM)
Season2018$VTM <- gsub("CHA", "Cha", Season2018$VTM)
Season2018$VTM <- gsub("TOR", "Tor", Season2018$VTM)
Season2018$VTM <- gsub("IND", "Ind", Season2018$VTM)
Season2018$VTM <- gsub("HOU", "Hou", Season2018$VTM)
Season2018$VTM <- gsub("DEN", "Den", Season2018$VTM)
Season2018$VTM <- gsub("MEM", "Mem", Season2018$VTM)
Season2018$VTM <- gsub("NYK", "NY", Season2018$VTM)
Season2018$VTM <- gsub("MIL", "Mil", Season2018$VTM)
Season2018$VTM <- gsub("OKC", "Okc", Season2018$VTM)
Season2018$VTM <- gsub("SAS", "Sas", Season2018$VTM)
Season2018$VTM <- gsub("DAL", "Dal", Season2018$VTM)
Season2018$VTM <- gsub("PHX", "Pho", Season2018$VTM)
Season2018$VTM <- gsub("POR", "Por", Season2018$VTM)
Season2018$VTM <- gsub("LAC", "Lac", Season2018$VTM)
Season2018$VTM <- gsub("SAC", "Sac", Season2018$VTM)
Season2018$VTM <- gsub("LAL", "Lal", Season2018$VTM)
Season2018$VTM <- gsub("MIN", "Min", Season2018$VTM)
Season2018$GAME_DATE <- ymd(Season2018$GAME_DATE)

saveRDS(Season2018, 'Season2018.rds')

########################################################################################
#######################################Schedule#########################################
########################################################################################

pacman::p_load(XML, lubridate, rvest, dplyr)
#Gather data

Months <- c("october", "november", "december", "january", "february", "march", "april")
Years <- c(2018)
URLs <- list()
for(i in 1:length(Years)){
  URLs[[i]] <- paste("http://www.basketball-reference.com/leagues/NBA_", Years[i],"_games-", Months,".html", sep = "")
}

URLs <- do.call("c", URLs)
URLs <- data.frame(URLs = URLs, Year = as.numeric(gsub("\\D", "", URLs)))
URLs$URLs <- as.character(URLs$URLs)
schedule <- list()
# import from JSON

for(i in 1:nrow(URLs)){
  schedule[[i]] <- read_html(URLs$URLs[i])%>% html_table(fill=TRUE)%>% .[[1]]
}

schedule <- do.call("rbind", schedule)

schedule$Date <- mdy(as.character(schedule$Date))
schedule[,4] <- as.numeric(as.character(schedule[,4]))
schedule[,6] <- as.numeric(as.character(schedule[,6]))


schedule$Season <- 2018


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



Standings <- "http://www.basketball-reference.com/leagues/NBA_2018.html"

Standings <- read_html(Standings)%>% html_table(fill=TRUE)%>% .[1:2]

Standings <- list(Western = Standings[[2]], Eastern = Standings[[1]])

Standings[[1]]$Conference <- c("West")

Standings[[2]]$Conference <- c("East")

colnames(Standings[[1]]) <- c("Team", "Current-W", "Current-L", "pct", "GB", "PS/G", "PA/G", "SRS", "Conference")

colnames(Standings[[2]]) <- c("Team", "Current-W", "Current-L", "pct", "GB", "PS/G", "PA/G", "SRS", "Conference")


Standings <- rbind(Standings[[1]], Standings[[2]])
