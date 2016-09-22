#Data
library(ggplot2)
library(lubridate)
library(dplyr)
NBAOdds <- read.csv("~/Sloan/NBAOdds.csv")
NBAOdds$Date <- mdy(NBAOdds$Date)
NBAOdds$Diff <- (NBAOdds$Score.1 - NBAOdds$Score)

NBAOdds$Diff <- (NBAOdds$Score - NBAOdds$Score.1)

NBAOdds$Year <- year(NBAOdds$Date)

plot(NBAOdds$Diff, NBAOdds$Home.Spread)


ggplot(NBAOdds, aes(x = Home.Spread, y = Diff)) + geom_point() + geom_smooth()

View(NBAOdds)

Odds2016 <- filter(NBAOdds, Year == 2016)

datos2016 <- read.csv("~/Sloan/datos2016.csv", row.names=1)

summary(Odds2016$Away)

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
rownames(datos2016) <- gsub("Miami.Heat", "MIA", rownames(datos2016))
colnames(datos2016) <- gsub("Charlotte.Hornets", "CHR", colnames(datos2016))
rownames(datos2016) <- gsub("Charlotte", "CHR", rownames(datos2016))
colnames(datos2016) <- gsub("Toronto.Raptors", "TOR", colnames(datos2016))
rownames(datos2016) <- gsub("Toronto", "TOR", rownames(datos2016))

Odds2016$defAPPS <- NA
Odds2016$offAPPS <- NA


Odds2016$defAPPS[1]<- datos2016[rownames(datos2016) == Odds2016$Home[1],colnames(datos2016) == Odds2016$Away[1]]
Odds2016$offAPPS[1] <- datos2016[rownames(datos2016) == Odds2016$Away[1],colnames(datos2016) == Odds2016$Home[1]]