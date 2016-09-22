#Data
library(ggplot2)
library(lubridate)
library(dplyr)
NBAOdds <- read.csv("~/Sloan/NBAOdds.csv")
NBAOdds$Date <- mdy(NBAOdds$Date)
NBAOdds$Diff <- (NBAOdds$Score.1 - NBAOdds$Score)

NBAOdds$Diff <- (NBAOdds$Score - NBAOdds$Score.1)

NBAOdds$Year <- year(NBAOdds$Date)


Odds2015_2016 <- filter(NBAOdds, Year == 2016 | Year == 2015)

datos2016 <- read.csv("~/Sloan/datos2016.csv", row.names=1)
datos2015 <- read.csv("~/Sloan/datos2015.csv", row.names=1)

#LA == clippers, Los Angeles == LAKERS

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





Odds2015_2016$defAPPS <- NA
Odds2015_2016$offAPPS <- NA

datos <- list(datos2015 = datos2015, datos2016= datos2016)


for (i in 1:nrow(Odds2015_2016)) {
  if (Odds2015_2016$Year[i] == 2016){
  Odds2015_2016$offAPPS[i]<- datos$datos2016[rownames(datos$datos2016) == Odds2015_2016$Home[i],colnames(datos$datos2016) == Odds2015_2016$Away[i]]
  Odds2015_2016$defAPPS[i] <- datos$datos2016[rownames(datos$datos2016) == Odds2015_2016$Away[i],colnames(datos$datos2016) == Odds2015_2016$Home[i]]
  }
  if (Odds2015_2016$Year[i] == 2015){
    Odds2015_2016$offAPPS[i]<- datos$datos2015[rownames(datos$datos2015) == Odds2015_2016$Home[i],colnames(datos$datos2015) == Odds2015_2016$Away[i]]
    Odds2015_2016$defAPPS[i] <- datos$datos2015[rownames(datos$datos2015) == Odds2015_2016$Away[i],colnames(datos$datos2015) == Odds2015_2016$Home[i]]
  }
}
ggplot(Odds2015_2016, aes(x = defAPPS, y = Diff)) + geom_point() + geom_smooth()
ggplot(Odds2015_2016, aes(x = offAPPS, y = Diff)) + geom_point() + geom_smooth()


#Odds2016$defAPPS[1]<- datos2016[rownames(datos2016) == Odds2016$Home[1],colnames(datos2016) == Odds2016$Away[1]]
#Odds2016$offAPPS[1] <- datos2016[rownames(datos2016) == Odds2016$Away[1],colnames(datos2016) == Odds2016$Home[1]]