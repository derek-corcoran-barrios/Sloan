#Data

#Load packages
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)

##Read Vegas odds and transform the fromats
NBAOdds <- read.csv("~/Sloan/NBAOdds.csv")
NBAOdds$Date <- mdy(NBAOdds$Date)

#Observed data The observed data, that is the away score - the home score, that will result in a negative value if the home team won, compared to a possitive value if the home team wins
NBAOdds$Diff <- (NBAOdds$Score.1 - NBAOdds$Score)

NBAOdds$Diff <- (NBAOdds$Score - NBAOdds$Score.1)

#Year of the result
NBAOdds$Year <- year(NBAOdds$Date)

#Read the spatial average points per shot difference per year, the Column names are the Offensive team and the row is the defensive team, the value in each cell is the value above average that 
#the offensive team would score against the deffensive team, that is, if the value is positive the offensive team would score above NBA average on that team, and if it's negative the opposite is true
datos2016 <- read.csv("~/Sloan/datos2016.csv", row.names=1)
datos2015 <- read.csv("~/Sloan/datos2015.csv", row.names=1)
datos2014 <- read.csv("~/Sloan/datos2014.csv", row.names=1)
datos2013 <- read.csv("~/Sloan/datos2013.csv", row.names=1)
datos2012 <- read.csv("~/Sloan/datos2012.csv", row.names=1)



##Equalize home and away team names


colnames(datos2016) <- gsub("Detroit.Pistons", "DET", colnames(datos2016))
rownames(datos2016) <- gsub("Detroit", "DET", rownames(datos2016))
colnames(datos2016) <- gsub("Atlanta.Hawks", "ATL", colnames(datos2016))
rownames(datos2016) <- gsub("Atlanta", "ATL", rownames(datos2016))
colnames(datos2016) <- gsub("Chicago.Bulls", "CHI", colnames(datos2016))
rownames(datos2016) <- gsub("Chicago", "CHI", rownames(datos2016))
colnames(datos2016) <- gsub("Boston.Celtics", "BOS", colnames(datos2016))
rownames(datos2016) <- gsub("Boston", "BOS", rownames(datos2016))
colnames(datos2016) <- gsub("Cleveland.Cavaliers", "CLE", colnames(datos2016))
rownames(datos2016) <- gsub("Cleveland", "CLE", rownames(datos2016))
colnames(datos2016) <- gsub("New.Orleans.Pelicans", "NOH", colnames(datos2016))
rownames(datos2016) <- gsub("New Orleans", "NOH", rownames(datos2016))
colnames(datos2016) <- gsub("Golden.State.Warriors", "GS", colnames(datos2016))
rownames(datos2016) <- gsub("Golden State", "GS", rownames(datos2016))
colnames(datos2016) <- gsub("Orlando.Magic", "ORL", colnames(datos2016))
rownames(datos2016) <- gsub("Orlando", "ORL", rownames(datos2016))
colnames(datos2016) <- gsub("Washington.Wizards", "WAS", colnames(datos2016))
rownames(datos2016) <- gsub("Washington", "WAS", rownames(datos2016))
colnames(datos2016) <- gsub("Philadelphia.76ers", "PHI", colnames(datos2016))
rownames(datos2016) <- gsub("Philadelphia", "PHI", rownames(datos2016))
colnames(datos2016) <- gsub("Brooklyn.Nets", "BKN", colnames(datos2016))
rownames(datos2016) <- gsub("Brooklyn", "BKN", rownames(datos2016))
colnames(datos2016) <- gsub("Utah.Jazz", "UTA", colnames(datos2016))
rownames(datos2016) <- gsub("Utah.Jazz", "UTA", rownames(datos2016))
colnames(datos2016) <- gsub("Miami.Heat", "MIA", colnames(datos2016))
rownames(datos2016) <- gsub("Miami", "MIA", rownames(datos2016))
colnames(datos2016) <- gsub("Charlotte.Hornets", "CHR", colnames(datos2016))
rownames(datos2016) <- gsub("Charlotte", "CHR", rownames(datos2016))
colnames(datos2016) <- gsub("Toronto.Raptors", "TOR", colnames(datos2016))
rownames(datos2016) <- gsub("Toronto", "TOR", rownames(datos2016))
colnames(datos2016) <- gsub("Indiana.Pacers", "IND", colnames(datos2016))
rownames(datos2016) <- gsub("Indiana", "IND", rownames(datos2016))
colnames(datos2016) <- gsub("Houston.Rockets", "HOU", colnames(datos2016))
rownames(datos2016) <- gsub("Houston", "HOU", rownames(datos2016))
colnames(datos2016) <- gsub("Denver.Nuggets", "DEN", colnames(datos2016))
rownames(datos2016) <- gsub("Denver", "DEN", rownames(datos2016))
colnames(datos2016) <- gsub("Memphis.Grizzlies", "MEM", colnames(datos2016))
rownames(datos2016) <- gsub("Memphis", "MEM", rownames(datos2016))
colnames(datos2016) <- gsub("New.York.Knicks", "NY", colnames(datos2016))
rownames(datos2016) <- gsub("New York", "NY", rownames(datos2016))
colnames(datos2016) <- gsub("Milwaukee.Bucks", "MIL", colnames(datos2016))
rownames(datos2016) <- gsub("Milwaukee", "MIL", rownames(datos2016))
colnames(datos2016) <- gsub("Oklahoma.City.Thunder", "OKC", colnames(datos2016))
rownames(datos2016) <- gsub("Oklahoma City", "OKC", rownames(datos2016))
colnames(datos2016) <- gsub("San.Antonio.Spurs", "SAN", colnames(datos2016))
rownames(datos2016) <- gsub("San Antonio", "SAN", rownames(datos2016))
colnames(datos2016) <- gsub("Dallas.Mavericks", "DAL", colnames(datos2016))
rownames(datos2016) <- gsub("Dallas", "DAL", rownames(datos2016))
colnames(datos2016) <- gsub("Phoenix.Suns", "PHO", colnames(datos2016))
rownames(datos2016) <- gsub("Phoenix", "PHO", rownames(datos2016))
colnames(datos2016) <- gsub("Portland.Trail.Blazers", "POR", colnames(datos2016))
rownames(datos2016) <- gsub("Portland", "POR", rownames(datos2016))
colnames(datos2016) <- gsub("Los.Angeles.Clippers", "LAC", colnames(datos2016))
rownames(datos2016) <- gsub("LA", "LAC", rownames(datos2016))
colnames(datos2016) <- gsub("Sacramento.Kings", "SAC", colnames(datos2016))
rownames(datos2016) <- gsub("Sacramento", "SAC", rownames(datos2016))
colnames(datos2016) <- gsub("Los.Angeles.Lakers", "LAL", colnames(datos2016))
rownames(datos2016) <- gsub("Los Angeles", "LAL", rownames(datos2016))
colnames(datos2016) <- gsub("Minnesota.Timberwolves", "MIN", colnames(datos2016))
rownames(datos2016) <- gsub("Minnesota", "MIN", rownames(datos2016))


colnames(datos2015) <- gsub("Detroit.Pistons", "DET", colnames(datos2015))
rownames(datos2015) <- gsub("Detroit", "DET", rownames(datos2015))
colnames(datos2015) <- gsub("Atlanta.Hawks", "ATL", colnames(datos2015))
rownames(datos2015) <- gsub("Atlanta", "ATL", rownames(datos2015))
colnames(datos2015) <- gsub("Chicago.Bulls", "CHI", colnames(datos2015))
rownames(datos2015) <- gsub("Chicago", "CHI", rownames(datos2015))
colnames(datos2015) <- gsub("Boston.Celtics", "BOS", colnames(datos2015))
rownames(datos2015) <- gsub("Boston", "BOS", rownames(datos2015))
colnames(datos2015) <- gsub("Cleveland.Cavaliers", "CLE", colnames(datos2015))
rownames(datos2015) <- gsub("Cleveland", "CLE", rownames(datos2015))
colnames(datos2015) <- gsub("New.Orleans.Pelicans", "NOH", colnames(datos2015))
rownames(datos2015) <- gsub("New Orleans", "NOH", rownames(datos2015))
colnames(datos2015) <- gsub("Golden.State.Warriors", "GS", colnames(datos2015))
rownames(datos2015) <- gsub("Golden State", "GS", rownames(datos2015))
colnames(datos2015) <- gsub("Orlando.Magic", "ORL", colnames(datos2015))
rownames(datos2015) <- gsub("Orlando", "ORL", rownames(datos2015))
colnames(datos2015) <- gsub("Washington.Wizards", "WAS", colnames(datos2015))
rownames(datos2015) <- gsub("Washington", "WAS", rownames(datos2015))
colnames(datos2015) <- gsub("Philadelphia.76ers", "PHI", colnames(datos2015))
rownames(datos2015) <- gsub("Philadelphia", "PHI", rownames(datos2015))
colnames(datos2015) <- gsub("Brooklyn.Nets", "BKN", colnames(datos2015))
rownames(datos2015) <- gsub("Brooklyn", "BKN", rownames(datos2015))
colnames(datos2015) <- gsub("Utah.Jazz", "UTA", colnames(datos2015))
rownames(datos2015) <- gsub("Utah.Jazz", "UTA", rownames(datos2015))
colnames(datos2015) <- gsub("Miami.Heat", "MIA", colnames(datos2015))
rownames(datos2015) <- gsub("Miami", "MIA", rownames(datos2015))
colnames(datos2015) <- gsub("Charlotte.Hornets", "CHR", colnames(datos2015))
rownames(datos2015) <- gsub("Charlotte", "CHR", rownames(datos2015))
colnames(datos2015) <- gsub("Toronto.Raptors", "TOR", colnames(datos2015))
rownames(datos2015) <- gsub("Toronto", "TOR", rownames(datos2015))
colnames(datos2015) <- gsub("Indiana.Pacers", "IND", colnames(datos2015))
rownames(datos2015) <- gsub("Indiana", "IND", rownames(datos2015))
colnames(datos2015) <- gsub("Houston.Rockets", "HOU", colnames(datos2015))
rownames(datos2015) <- gsub("Houston", "HOU", rownames(datos2015))
colnames(datos2015) <- gsub("Denver.Nuggets", "DEN", colnames(datos2015))
rownames(datos2015) <- gsub("Denver", "DEN", rownames(datos2015))
colnames(datos2015) <- gsub("Memphis.Grizzlies", "MEM", colnames(datos2015))
rownames(datos2015) <- gsub("Memphis", "MEM", rownames(datos2015))
colnames(datos2015) <- gsub("New.York.Knicks", "NY", colnames(datos2015))
rownames(datos2015) <- gsub("New York", "NY", rownames(datos2015))
colnames(datos2015) <- gsub("Milwaukee.Bucks", "MIL", colnames(datos2015))
rownames(datos2015) <- gsub("Milwaukee", "MIL", rownames(datos2015))
colnames(datos2015) <- gsub("Oklahoma.City.Thunder", "OKC", colnames(datos2015))
rownames(datos2015) <- gsub("Oklahoma City", "OKC", rownames(datos2015))
colnames(datos2015) <- gsub("San.Antonio.Spurs", "SAN", colnames(datos2015))
rownames(datos2015) <- gsub("San Antonio", "SAN", rownames(datos2015))
colnames(datos2015) <- gsub("Dallas.Mavericks", "DAL", colnames(datos2015))
rownames(datos2015) <- gsub("Dallas", "DAL", rownames(datos2015))
colnames(datos2015) <- gsub("Phoenix.Suns", "PHO", colnames(datos2015))
rownames(datos2015) <- gsub("Phoenix", "PHO", rownames(datos2015))
colnames(datos2015) <- gsub("Portland.Trail.Blazers", "POR", colnames(datos2015))
rownames(datos2015) <- gsub("Portland", "POR", rownames(datos2015))
colnames(datos2015) <- gsub("Los.Angeles.Clippers", "LAC", colnames(datos2015))
rownames(datos2015) <- gsub("LA", "LAC", rownames(datos2015))
colnames(datos2015) <- gsub("Sacramento.Kings", "SAC", colnames(datos2015))
rownames(datos2015) <- gsub("Sacramento", "SAC", rownames(datos2015))
colnames(datos2015) <- gsub("Los.Angeles.Lakers", "LAL", colnames(datos2015))
rownames(datos2015) <- gsub("Los Angeles", "LAL", rownames(datos2015))
colnames(datos2015) <- gsub("Minnesota.Timberwolves", "MIN", colnames(datos2015))
rownames(datos2015) <- gsub("Minnesota", "MIN", rownames(datos2015))

colnames(datos2014) <- gsub("Detroit.Pistons", "DET", colnames(datos2014))
rownames(datos2014) <- gsub("Detroit", "DET", rownames(datos2014))
colnames(datos2014) <- gsub("Atlanta.Hawks", "ATL", colnames(datos2014))
rownames(datos2014) <- gsub("Atlanta", "ATL", rownames(datos2014))
colnames(datos2014) <- gsub("Chicago.Bulls", "CHI", colnames(datos2014))
rownames(datos2014) <- gsub("Chicago", "CHI", rownames(datos2014))
colnames(datos2014) <- gsub("Boston.Celtics", "BOS", colnames(datos2014))
rownames(datos2014) <- gsub("Boston", "BOS", rownames(datos2014))
colnames(datos2014) <- gsub("Cleveland.Cavaliers", "CLE", colnames(datos2014))
rownames(datos2014) <- gsub("Cleveland", "CLE", rownames(datos2014))
colnames(datos2014) <- gsub("New.Orleans.Pelicans", "NOH", colnames(datos2014))
rownames(datos2014) <- gsub("New Orleans", "NOH", rownames(datos2014))
colnames(datos2014) <- gsub("Golden.State.Warriors", "GS", colnames(datos2014))
rownames(datos2014) <- gsub("Golden State", "GS", rownames(datos2014))
colnames(datos2014) <- gsub("Orlando.Magic", "ORL", colnames(datos2014))
rownames(datos2014) <- gsub("Orlando", "ORL", rownames(datos2014))
colnames(datos2014) <- gsub("Washington.Wizards", "WAS", colnames(datos2014))
rownames(datos2014) <- gsub("Washington", "WAS", rownames(datos2014))
colnames(datos2014) <- gsub("Philadelphia.76ers", "PHI", colnames(datos2014))
rownames(datos2014) <- gsub("Philadelphia", "PHI", rownames(datos2014))
colnames(datos2014) <- gsub("Brooklyn.Nets", "BKN", colnames(datos2014))
rownames(datos2014) <- gsub("Brooklyn", "BKN", rownames(datos2014))
colnames(datos2014) <- gsub("Utah.Jazz", "UTA", colnames(datos2014))
rownames(datos2014) <- gsub("Utah.Jazz", "UTA", rownames(datos2014))
colnames(datos2014) <- gsub("Miami.Heat", "MIA", colnames(datos2014))
rownames(datos2014) <- gsub("Miami", "MIA", rownames(datos2014))
colnames(datos2014) <- gsub("Charlotte.Bobcats", "CHR", colnames(datos2014))
rownames(datos2014) <- gsub("Charlotte", "CHR", rownames(datos2014))
colnames(datos2014) <- gsub("Toronto.Raptors", "TOR", colnames(datos2014))
rownames(datos2014) <- gsub("Toronto", "TOR", rownames(datos2014))
colnames(datos2014) <- gsub("Indiana.Pacers", "IND", colnames(datos2014))
rownames(datos2014) <- gsub("Indiana", "IND", rownames(datos2014))
colnames(datos2014) <- gsub("Houston.Rockets", "HOU", colnames(datos2014))
rownames(datos2014) <- gsub("Houston", "HOU", rownames(datos2014))
colnames(datos2014) <- gsub("Denver.Nuggets", "DEN", colnames(datos2014))
rownames(datos2014) <- gsub("Denver", "DEN", rownames(datos2014))
colnames(datos2014) <- gsub("Memphis.Grizzlies", "MEM", colnames(datos2014))
rownames(datos2014) <- gsub("Memphis", "MEM", rownames(datos2014))
colnames(datos2014) <- gsub("New.York.Knicks", "NY", colnames(datos2014))
rownames(datos2014) <- gsub("New York", "NY", rownames(datos2014))
colnames(datos2014) <- gsub("Milwaukee.Bucks", "MIL", colnames(datos2014))
rownames(datos2014) <- gsub("Milwaukee", "MIL", rownames(datos2014))
colnames(datos2014) <- gsub("Oklahoma.City.Thunder", "OKC", colnames(datos2014))
rownames(datos2014) <- gsub("Oklahoma City", "OKC", rownames(datos2014))
colnames(datos2014) <- gsub("San.Antonio.Spurs", "SAN", colnames(datos2014))
rownames(datos2014) <- gsub("San Antonio", "SAN", rownames(datos2014))
colnames(datos2014) <- gsub("Dallas.Mavericks", "DAL", colnames(datos2014))
rownames(datos2014) <- gsub("Dallas", "DAL", rownames(datos2014))
colnames(datos2014) <- gsub("Phoenix.Suns", "PHO", colnames(datos2014))
rownames(datos2014) <- gsub("Phoenix", "PHO", rownames(datos2014))
colnames(datos2014) <- gsub("Portland.Trail.Blazers", "POR", colnames(datos2014))
rownames(datos2014) <- gsub("Portland", "POR", rownames(datos2014))
colnames(datos2014) <- gsub("Los.Angeles.Clippers", "LAC", colnames(datos2014))
rownames(datos2014) <- gsub("LA", "LAC", rownames(datos2014))
colnames(datos2014) <- gsub("Sacramento.Kings", "SAC", colnames(datos2014))
rownames(datos2014) <- gsub("Sacramento", "SAC", rownames(datos2014))
colnames(datos2014) <- gsub("Los.Angeles.Lakers", "LAL", colnames(datos2014))
rownames(datos2014) <- gsub("Los Angeles", "LAL", rownames(datos2014))
colnames(datos2014) <- gsub("Minnesota.Timberwolves", "MIN", colnames(datos2014))
rownames(datos2014) <- gsub("Minnesota", "MIN", rownames(datos2014))

colnames(datos2013) <- gsub("Detroit.Pistons", "DET", colnames(datos2013))
rownames(datos2013) <- gsub("Detroit", "DET", rownames(datos2013))
colnames(datos2013) <- gsub("Atlanta.Hawks", "ATL", colnames(datos2013))
rownames(datos2013) <- gsub("Atlanta", "ATL", rownames(datos2013))
colnames(datos2013) <- gsub("Chicago.Bulls", "CHI", colnames(datos2013))
rownames(datos2013) <- gsub("Chicago", "CHI", rownames(datos2013))
colnames(datos2013) <- gsub("Boston.Celtics", "BOS", colnames(datos2013))
rownames(datos2013) <- gsub("Boston", "BOS", rownames(datos2013))
colnames(datos2013) <- gsub("Cleveland.Cavaliers", "CLE", colnames(datos2013))
rownames(datos2013) <- gsub("Cleveland", "CLE", rownames(datos2013))
colnames(datos2013) <- gsub("New.Orleans.Pelicans", "NOH", colnames(datos2013))
rownames(datos2013) <- gsub("New Orleans", "NOH", rownames(datos2013))
colnames(datos2013) <- gsub("Golden.State.Warriors", "GS", colnames(datos2013))
rownames(datos2013) <- gsub("Golden State", "GS", rownames(datos2013))
colnames(datos2013) <- gsub("Orlando.Magic", "ORL", colnames(datos2013))
rownames(datos2013) <- gsub("Orlando", "ORL", rownames(datos2013))
colnames(datos2013) <- gsub("Washington.Wizards", "WAS", colnames(datos2013))
rownames(datos2013) <- gsub("Washington", "WAS", rownames(datos2013))
colnames(datos2013) <- gsub("Philadelphia.76ers", "PHI", colnames(datos2013))
rownames(datos2013) <- gsub("Philadelphia", "PHI", rownames(datos2013))
colnames(datos2013) <- gsub("Brooklyn.Nets", "BKN", colnames(datos2013))
rownames(datos2013) <- gsub("Brooklyn", "BKN", rownames(datos2013))
colnames(datos2013) <- gsub("Utah.Jazz", "UTA", colnames(datos2013))
rownames(datos2013) <- gsub("Utah.Jazz", "UTA", rownames(datos2013))
colnames(datos2013) <- gsub("Miami.Heat", "MIA", colnames(datos2013))
rownames(datos2013) <- gsub("Miami", "MIA", rownames(datos2013))
colnames(datos2013) <- gsub("Charlotte.Hornets", "CHR", colnames(datos2013))
rownames(datos2013) <- gsub("Charlotte", "CHR", rownames(datos2013))
colnames(datos2013) <- gsub("Toronto.Raptors", "TOR", colnames(datos2013))
rownames(datos2013) <- gsub("Toronto", "TOR", rownames(datos2013))
colnames(datos2013) <- gsub("Indiana.Pacers", "IND", colnames(datos2013))
rownames(datos2013) <- gsub("Indiana", "IND", rownames(datos2013))
colnames(datos2013) <- gsub("Houston.Rockets", "HOU", colnames(datos2013))
rownames(datos2013) <- gsub("Houston", "HOU", rownames(datos2013))
colnames(datos2013) <- gsub("Denver.Nuggets", "DEN", colnames(datos2013))
rownames(datos2013) <- gsub("Denver", "DEN", rownames(datos2013))
colnames(datos2013) <- gsub("Memphis.Grizzlies", "MEM", colnames(datos2013))
rownames(datos2013) <- gsub("Memphis", "MEM", rownames(datos2013))
colnames(datos2013) <- gsub("New.York.Knicks", "NY", colnames(datos2013))
rownames(datos2013) <- gsub("New York", "NY", rownames(datos2013))
colnames(datos2013) <- gsub("Milwaukee.Bucks", "MIL", colnames(datos2013))
rownames(datos2013) <- gsub("Milwaukee", "MIL", rownames(datos2013))
colnames(datos2013) <- gsub("Oklahoma.City.Thunder", "OKC", colnames(datos2013))
rownames(datos2013) <- gsub("Oklahoma City", "OKC", rownames(datos2013))
colnames(datos2013) <- gsub("San.Antonio.Spurs", "SAN", colnames(datos2013))
rownames(datos2013) <- gsub("San Antonio", "SAN", rownames(datos2013))
colnames(datos2013) <- gsub("Dallas.Mavericks", "DAL", colnames(datos2013))
rownames(datos2013) <- gsub("Dallas", "DAL", rownames(datos2013))
colnames(datos2013) <- gsub("Phoenix.Suns", "PHO", colnames(datos2013))
rownames(datos2013) <- gsub("Phoenix", "PHO", rownames(datos2013))
colnames(datos2013) <- gsub("Portland.Trail.Blazers", "POR", colnames(datos2013))
rownames(datos2013) <- gsub("Portland", "POR", rownames(datos2013))
colnames(datos2013) <- gsub("Los.Angeles.Clippers", "LAC", colnames(datos2013))
rownames(datos2013) <- gsub("LA", "LAC", rownames(datos2013))
colnames(datos2013) <- gsub("Sacramento.Kings", "SAC", colnames(datos2013))
rownames(datos2013) <- gsub("Sacramento", "SAC", rownames(datos2013))
colnames(datos2013) <- gsub("Los.Angeles.Lakers", "LAL", colnames(datos2013))
rownames(datos2013) <- gsub("Los Angeles", "LAL", rownames(datos2013))
colnames(datos2013) <- gsub("Minnesota.Timberwolves", "MIN", colnames(datos2013))
rownames(datos2013) <- gsub("Minnesota", "MIN", rownames(datos2013))

colnames(datos2012) <- gsub("Detroit.Pistons", "DET", colnames(datos2012))
rownames(datos2012) <- gsub("Detroit", "DET", rownames(datos2012))
colnames(datos2012) <- gsub("Atlanta.Hawks", "ATL", colnames(datos2012))
rownames(datos2012) <- gsub("Atlanta", "ATL", rownames(datos2012))
colnames(datos2012) <- gsub("Chicago.Bulls", "CHI", colnames(datos2012))
rownames(datos2012) <- gsub("Chicago", "CHI", rownames(datos2012))
colnames(datos2012) <- gsub("Boston.Celtics", "BOS", colnames(datos2012))
rownames(datos2012) <- gsub("Boston", "BOS", rownames(datos2012))
colnames(datos2012) <- gsub("Cleveland.Cavaliers", "CLE", colnames(datos2012))
rownames(datos2012) <- gsub("Cleveland", "CLE", rownames(datos2012))
colnames(datos2012) <- gsub("New.Orleans.Pelicans", "NOH", colnames(datos2012))
rownames(datos2012) <- gsub("New Orleans", "NOH", rownames(datos2012))
colnames(datos2012) <- gsub("Golden.State.Warriors", "GS", colnames(datos2012))
rownames(datos2012) <- gsub("Golden State", "GS", rownames(datos2012))
colnames(datos2012) <- gsub("Orlando.Magic", "ORL", colnames(datos2012))
rownames(datos2012) <- gsub("Orlando", "ORL", rownames(datos2012))
colnames(datos2012) <- gsub("Washington.Wizards", "WAS", colnames(datos2012))
rownames(datos2012) <- gsub("Washington", "WAS", rownames(datos2012))
colnames(datos2012) <- gsub("Philadelphia.76ers", "PHI", colnames(datos2012))
rownames(datos2012) <- gsub("Philadelphia", "PHI", rownames(datos2012))
colnames(datos2012) <- gsub("Brooklyn.Nets", "BKN", colnames(datos2012))
rownames(datos2012) <- gsub("Brooklyn", "BKN", rownames(datos2012))
colnames(datos2012) <- gsub("Utah.Jazz", "UTA", colnames(datos2012))
rownames(datos2012) <- gsub("Utah", "UTA", rownames(datos2012))
colnames(datos2012) <- gsub("Miami.Heat", "MIA", colnames(datos2012))
rownames(datos2012) <- gsub("Miami", "MIA", rownames(datos2012))
colnames(datos2012) <- gsub("Charlotte.Bobcats", "CHR", colnames(datos2012))
rownames(datos2012) <- gsub("Charlotte", "CHR", rownames(datos2012))
colnames(datos2012) <- gsub("Toronto.Raptors", "TOR", colnames(datos2012))
rownames(datos2012) <- gsub("Toronto", "TOR", rownames(datos2012))
colnames(datos2012) <- gsub("Indiana.Pacers", "IND", colnames(datos2012))
rownames(datos2012) <- gsub("Indiana", "IND", rownames(datos2012))
colnames(datos2012) <- gsub("Houston.Rockets", "HOU", colnames(datos2012))
rownames(datos2012) <- gsub("Houston", "HOU", rownames(datos2012))
colnames(datos2012) <- gsub("Denver.Nuggets", "DEN", colnames(datos2012))
rownames(datos2012) <- gsub("Denver", "DEN", rownames(datos2012))
colnames(datos2012) <- gsub("Memphis.Grizzlies", "MEM", colnames(datos2012))
rownames(datos2012) <- gsub("Memphis", "MEM", rownames(datos2012))
colnames(datos2012) <- gsub("New.York.Knicks", "NY", colnames(datos2012))
rownames(datos2012) <- gsub("New York", "NY", rownames(datos2012))
colnames(datos2012) <- gsub("Milwaukee.Bucks", "MIL", colnames(datos2012))
rownames(datos2012) <- gsub("Milwaukee", "MIL", rownames(datos2012))
colnames(datos2012) <- gsub("Oklahoma.City.Thunder", "OKC", colnames(datos2012))
rownames(datos2012) <- gsub("Oklahoma City", "OKC", rownames(datos2012))
colnames(datos2012) <- gsub("San.Antonio.Spurs", "SAN", colnames(datos2012))
rownames(datos2012) <- gsub("San Antonio", "SAN", rownames(datos2012))
colnames(datos2012) <- gsub("Dallas.Mavericks", "DAL", colnames(datos2012))
rownames(datos2012) <- gsub("Dallas", "DAL", rownames(datos2012))
colnames(datos2012) <- gsub("Phoenix.Suns", "PHO", colnames(datos2012))
rownames(datos2012) <- gsub("Phoenix", "PHO", rownames(datos2012))
colnames(datos2012) <- gsub("Portland.Trail.Blazers", "POR", colnames(datos2012))
rownames(datos2012) <- gsub("Portland", "POR", rownames(datos2012))
colnames(datos2012) <- gsub("Los.Angeles.Clippers", "LAC", colnames(datos2012))
rownames(datos2012) <- gsub("LA", "LAC", rownames(datos2012))
colnames(datos2012) <- gsub("Sacramento.Kings", "SAC", colnames(datos2012))
rownames(datos2012) <- gsub("Sacramento", "SAC", rownames(datos2012))
colnames(datos2012) <- gsub("Los.Angeles.Lakers", "LAL", colnames(datos2012))
rownames(datos2012) <- gsub("Los Angeles", "LAL", rownames(datos2012))
colnames(datos2012) <- gsub("Minnesota.Timberwolves", "MIN", colnames(datos2012))
rownames(datos2012) <- gsub("Minnesota", "MIN", rownames(datos2012))


#Add two columns Aberage points per shot
#Def apps is when the away team is defending 
#Off apps is when the away team is attacking
NBAOdds$defAPPS <- NA
NBAOdds$offAPPS <- NA

datos <- list(datos2012 = datos2012 ,datos2013 = datos2013, datos2014 = datos2014, datos2015 = datos2015, datos2016= datos2016)


for (i in 1:nrow(NBAOdds)) {
  print(i)
  if (NBAOdds$Year[i] == 2016){
  NBAOdds$offAPPS[i]<- datos$datos2016[rownames(datos$datos2016) == NBAOdds$Home[i],colnames(datos$datos2016) == NBAOdds$Away[i]]
  NBAOdds$defAPPS[i] <- datos$datos2016[rownames(datos$datos2016) == NBAOdds$Away[i],colnames(datos$datos2016) == NBAOdds$Home[i]]
  }
  if (NBAOdds$Year[i] == 2015){
    NBAOdds$offAPPS[i]<- datos$datos2015[rownames(datos$datos2015) == NBAOdds$Home[i],colnames(datos$datos2015) == NBAOdds$Away[i]]
    NBAOdds$defAPPS[i] <- datos$datos2015[rownames(datos$datos2015) == NBAOdds$Away[i],colnames(datos$datos2015) == NBAOdds$Home[i]]
  }
  if (NBAOdds$Year[i] == 2014){
    NBAOdds$offAPPS[i]<- datos$datos2014[rownames(datos$datos2014) == NBAOdds$Home[i],colnames(datos$datos2014) == NBAOdds$Away[i]]
    NBAOdds$defAPPS[i] <- datos$datos2014[rownames(datos$datos2014) == NBAOdds$Away[i],colnames(datos$datos2014) == NBAOdds$Home[i]]
  }
  if (NBAOdds$Year[i] == 2013){
    NBAOdds$offAPPS[i]<- datos$datos2013[rownames(datos$datos2013) == NBAOdds$Home[i],colnames(datos$datos2013) == NBAOdds$Away[i]]
    NBAOdds$defAPPS[i] <- datos$datos2013[rownames(datos$datos2013) == NBAOdds$Away[i],colnames(datos$datos2013) == NBAOdds$Home[i]]
  }
  if (NBAOdds$Year[i] == 2012){
    NBAOdds$offAPPS[i]<- datos$datos2012[rownames(datos$datos2012) == NBAOdds$Home[i],colnames(datos$datos2012) == NBAOdds$Away[i]]
    NBAOdds$defAPPS[i] <- datos$datos2012[rownames(datos$datos2012) == NBAOdds$Away[i],colnames(datos$datos2012) == NBAOdds$Home[i]]
  }
}

ggplot(NBAOdds, aes(x = Home.Spread, y = Diff)) + geom_point() + geom_smooth()
ggplot(NBAOdds, aes(x = defAPPS, y = Diff)) + geom_point() + geom_smooth()
ggplot(NBAOdds, aes(x = offAPPS, y = Diff)) + geom_point() + geom_smooth()

summary(lm(Diff ~ offAPPS + defAPPS, data=NBAOdds))
summary(lm(Diff ~ defAPPS, data=NBAOdds))
summary(lm(Diff ~ offAPPS, data=NBAOdds))

FinalOdds <- NBAOdds
write.csv(FinalOdds, "FinalOdds.csv")

####Regression trees

library(dismo)
library(gbm)


#Divide odds into 2016 vs 2012-2015
NBAOdds2012_2015 <- filter(NBAOdds, Year != 2016)
NBAOdds2016 <- filter(NBAOdds, Year == 2016)


#####Forcast

####Caret version
library(caret)
ctrl <- trainControl(method = "repeatedcv", number=10, repeats=3)


grid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                    n.trees = seq(100, 1000, by = 50),
                    shrinkage = c(0.01, 0.1),
                    n.minobsinnode=c(1,5,10))


# train the GBM model
set.seed(7)
modelGbmforcast <- train(x = NBAOdds2012_2015[,14:15],y = NBAOdds2012_2015[,12], method = "gbm",  preProcess = c("center", "scale"), verbose = FALSE, trControl = ctrl, tuneGrid = grid)

gbmcaretforecast <- predict(modelGbmforcast, NBAOdds2016[,14:15])
####

#Add BRT fit to model

#Add caret fit to model
NBAOdds2016$GBMfit <- gbmcaretforecast


#Plot predictions
predict_comparison <- melt(NBAOdds2016, id.vars = "Diff", measure.vars = c("Home.Spread", "GBMfit"))
predict_comparison$variable <- ifelse(predict_comparison$variable == "Home.Spread", "Vegas odds","PPS odds")
ggplot(predict_comparison, aes(x = Diff, y = value)) + geom_point(aes(color = variable)) + geom_smooth(method = lm, aes(fill=variable, color = variable)) + xlab("Observed spread") + ylab("Predicted spread") + theme(legend.position = "bottom")


#get The predicted

postResample(pred = NBAOdds2016$GBMfit, obs = NBAOdds2016$Diff)
postResample(pred = NBAOdds2016$Home.Spread, obs = NBAOdds2016$Diff)

####Surface plot


For.predictions <- expand.grid(defAPPS = seq(from = min(NBAOdds$defAPPS), to = max(NBAOdds$defAPPS), length.out = 100), 
                    offAPPS =seq(from= min(NBAOdds$offAPPS),to = max(NBAOdds$offAPPS), length.out = 100))

For.predictions$Spread <- predict(modelGbmforcast, For.predictions)

wireframe(Spread ~  offAPPS + defAPPS, data = For.predictions, colorkey = TRUE, drape = TRUE, pretty = TRUE,scales = list(arrows = FALSE), screen = list(z = -220, x = -80))

z = seq(from = -6, to = 6, length.out = 20)

for (i in 1:20){
  print(wireframe(Spread ~  offAPPS + defAPPS, data = For.predictions, colorkey = TRUE, drape = TRUE, pretty = TRUE,scales = list(arrows = FALSE), screen = list(z = z[i]), main = print(z[i])))
  
}





