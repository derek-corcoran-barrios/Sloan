library(SpatialBall)
library(ggplot2)

data("season2017")
season2016 <- readRDS("shotDataTotal2016.rds")
season2015 <- readRDS("shotDataTotal2015.rds")
season2014 <- readRDS("shotDataTotal2014.rds")
season2013 <- readRDS("shotDataTotal2013.rds")





SAD13 <- DefShotSeasonGraphTeam(season2013, team = "Sas",quant = 0.4)   + ggtitle("Def Sas 2013")
SAD14 <- DefShotSeasonGraphTeam(season2014, team = "Sas",quant = 0.4) + ggtitle("Def Sas 2014")
SAD15 <- DefShotSeasonGraphTeam(season2015, team = "Sas",quant = 0.4) + ggtitle("Def Sas 2015")
SAD16 <- DefShotSeasonGraphTeam(season2016, team = "Sas",quant = 0.4) + ggtitle("Def Sas 2016")
SAD17 <- DefShotSeasonGraphTeam(season2017, team = "Sas",quant = 0.4) + ggtitle("Def Sas 2017")


library(gridExtra)

grid.arrange(KL13, KL14, KL15, KL16, KL17)

s15 <- season2015
s15$Season <- 2015
s16 <- season2016
s16$Season <- 2016
s17 <- season2017
s17$Season <- 2017




seasons <- rbind(s15, s16, s17)
library(gganimate)

KL <- OffShotSeasonGraphPlayer(seasons, player = "Kawhi Leonard",quant = 0.4, aes(frame = Season))
saveGIF(for(i in 1:5){OffShotSeasonGraphPlayer(seasons[[i]], player = "Kawhi Leonard",quant = 0.4)})


KP16 <- OffShotSeasonGraphPlayer(Seasondata = season2016, player = "Kristaps Porzingis",quant = 0.4)
KP17 <- OffShotSeasonGraphPlayer(Seasondata = season2017, player = "Kristaps Porzingis",quant = 0.4)