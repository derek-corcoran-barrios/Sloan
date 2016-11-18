library(XML)
library(dplyr)
library(ggplot2)

years <- c(2012:2016)
pacetable <- list()
webtable<- vector()
for(i in 1:length(years)){
  webtable[i] <- paste("http://www.espn.com/nba/hollinger/teamstats/_/sort/paceFactor/year/", years[i], sep = "")
  pacetable.temp <- readHTMLTable(webtable[i])[[1]]
  colnames(pacetable.temp) <- as.character(t(pacetable.temp[1,]))
  pacetable.temp <- pacetable.temp[-1,2:3]
  pacetable.temp$TEAM <- as.factor(as.character(pacetable.temp$TEAM))
  pacetable.temp$PACE <- as.numeric(as.character(pacetable.temp$PACE))
  pacetable.temp$YEAR <- years[i]
  pacetable[[i]] <- pacetable.temp
}

pacetable <- do.call("rbind", pacetable)

library(ggplot2)
ggplot(pacetable, aes(y = PACE, x=as.factor(YEAR))) + geom_boxplot() + xlab("Year") + ylab("Pace")


pacetable$TEAM <- gsub("Detroit", "DET",pacetable$TEAM )
pacetable$TEAM <- gsub("Atlanta", "ATL", pacetable$TEAM)
pacetable$TEAM <- gsub("Chicago", "CHI", pacetable$TEAM)
pacetable$TEAM <- gsub("Boston", "BOS", pacetable$TEAM)
pacetable$TEAM <- gsub("Cleveland", "CLE", pacetable$TEAM)
pacetable$TEAM <- gsub("New Orleans", "NOH", pacetable$TEAM)
pacetable$TEAM <- gsub("Golden State", "GS", pacetable$TEAM)
pacetable$TEAM <- gsub("Orlando", "ORL", pacetable$TEAM)
pacetable$TEAM <- gsub("Washington", "WAS", pacetable$TEAM)
pacetable$TEAM <- gsub("Philadelphia", "PHI", pacetable$TEAM)
pacetable$TEAM <- gsub("Brooklyn", "BKN", pacetable$TEAM)
pacetable$TEAM <- gsub("New Jersey", "BKN", pacetable$TEAM)
pacetable$TEAM <- gsub("Utah", "UTA", pacetable$TEAM)
pacetable$TEAM <- gsub("Miami", "MIA", pacetable$TEAM)
pacetable$TEAM <- gsub("Charlotte", "CHR", pacetable$TEAM)
pacetable$TEAM <- gsub("Toronto", "TOR", pacetable$TEAM)
pacetable$TEAM <- gsub("Indiana", "IND", pacetable$TEAM)
pacetable$TEAM <- gsub("Houston", "HOU", pacetable$TEAM)
pacetable$TEAM <- gsub("Denver", "DEN", pacetable$TEAM)
pacetable$TEAM <- gsub("Memphis", "MEM", pacetable$TEAM)
pacetable$TEAM <- gsub("New York", "NY", pacetable$TEAM)
pacetable$TEAM <- gsub("Milwaukee", "MIL", pacetable$TEAM)
pacetable$TEAM <- gsub("Oklahoma City", "OKC", pacetable$TEAM)
pacetable$TEAM <- gsub("San Antonio", "SAN", pacetable$TEAM)
pacetable$TEAM <- gsub("Dallas", "DAL", pacetable$TEAM)
pacetable$TEAM <- gsub("Phoenix", "PHO", pacetable$TEAM)
pacetable$TEAM <- gsub("Portland", "POR", pacetable$TEAM)
pacetable$TEAM <- gsub("LAC Clippers", "LAC", pacetable$TEAM)
pacetable$TEAM <- gsub("LA Clippers", "LAC", pacetable$TEAM)
pacetable$TEAM <- gsub("Sacramento", "SAC", pacetable$TEAM)
pacetable$TEAM <- gsub("LAC Lakers", "LAL", pacetable$TEAM)
pacetable$TEAM <- gsub("LA Lakers", "LAL", pacetable$TEAM)
pacetable$TEAM <- gsub("Minnesota", "MIN", pacetable$TEAM)


pacemean <- function(team1, team2, year, pacetable) {
  pacetable.temp <- dplyr::filter(pacetable, YEAR == year)
  pacetable.temp <- dplyr::filter(pacetable.temp, TEAM == team1 | TEAM == team2)
  mean(pacetable.temp$PACE)
}

teams2012 <- unique(dplyr::filter(pacetable, YEAR == 2012)$TEAM)
teams2013 <- unique(dplyr::filter(pacetable, YEAR == 2013)$TEAM)
teams2014 <- unique(dplyr::filter(pacetable, YEAR == 2014)$TEAM)
teams2015 <- unique(dplyr::filter(pacetable, YEAR == 2015)$TEAM)
teams2016 <- unique(dplyr::filter(pacetable, YEAR == 2016)$TEAM)

###2016
pacetables2016 <- data.frame(matrix(ncol = 30, nrow = 30))
colnames(pacetables2016) <-teams2016
rownames(pacetables2016) <-teams2016

  
for (i in 1:length(teams2016)) {
  for (j in 1:length(teams2016)){
    pacetables2016[rownames(pacetables2016) == teams2016[j],colnames(pacetables2016) == teams2016[i]] <- pacemean(team1 = teams2016[i], team2 = teams2016[j], year = 2016, pacetable = pacetable)

  }
}
saveRDS(pacetables2016, "pacetables2016.rds")

####2015
pacetables2015 <- data.frame(matrix(ncol = 30, nrow = 30))
colnames(pacetables2015) <-teams2015
rownames(pacetables2015) <-teams2015


for (i in 1:length(teams2015)) {
  for (j in 1:length(teams2015)){
    pacetables2015[rownames(pacetables2015) == teams2015[j],colnames(pacetables2015) == teams2015[i]] <- pacemean(team1 = teams2015[i], team2 = teams2015[j], year = 2015, pacetable = pacetable)
    
  }
}

saveRDS(pacetables2015, "pacetables2015.rds")


###2014


pacetables2014 <- data.frame(matrix(ncol = 30, nrow = 30))
colnames(pacetables2014) <-teams2014
rownames(pacetables2014) <-teams2014


for (i in 1:length(teams2014)) {
  for (j in 1:length(teams2014)){
    pacetables2014[rownames(pacetables2014) == teams2014[j],colnames(pacetables2014) == teams2014[i]] <- pacemean(team1 = teams2014[i], team2 = teams2014[j], year = 2014, pacetable = pacetable)
    
  }
}

saveRDS(pacetables2014, "pacetables2014.rds")

###2013


pacetables2013 <- data.frame(matrix(ncol = 30, nrow = 30))
colnames(pacetables2013) <-teams2013
rownames(pacetables2013) <-teams2013


for (i in 1:length(teams2013)) {
  for (j in 1:length(teams2013)){
    pacetables2013[rownames(pacetables2013) == teams2013[j],colnames(pacetables2013) == teams2013[i]] <- pacemean(team1 = teams2013[i], team2 = teams2013[j], year = 2013, pacetable = pacetable)
    
  }
}

saveRDS(pacetables2013, "pacetables2013.rds")

####2012


pacetables2012 <- data.frame(matrix(ncol = 30, nrow = 30))
colnames(pacetables2012) <-teams2012
rownames(pacetables2012) <-teams2012


for (i in 1:length(teams2012)) {
  for (j in 1:length(teams2012)){
    pacetables2012[rownames(pacetables2012) == teams2012[j],colnames(pacetables2012) == teams2012[i]] <- pacemean(team1 = teams2012[i], team2 = teams2012[j], year = 2012, pacetable = pacetable)
    
  }
}

saveRDS(pacetables2012, "pacetables2012.rds")