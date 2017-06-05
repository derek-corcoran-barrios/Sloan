library(SpatialBall)
data(season2017)
season2016 <- readRDS("shotDataTotal2016.rds")
season2015 <- readRDS("shotDataTotal2015.rds")


#Golden State home

Get_Apps(HomeTeam = "GSW", VisitorTeam = "Cle", Seasondata = season2017)

OffShotSeasonGraphTeam(Seasondata = season2017, team = "GSW")
DefShotSeasonGraphTeam(Seasondata = season2017, team = "GSW")

OffShotSeasonGraphTeam(Seasondata = season2017, team = "Cle")
DefShotSeasonGraphTeam(Seasondata = season2017, team = "Cle")


Get_Apps(HomeTeam = "GSW", VisitorTeam = "Cle", Seasondata = season2016)
Get_Apps(HomeTeam = "GSW", VisitorTeam = "Cle", Seasondata = season2015)

#Cleveland Home

Get_Apps(HomeTeam = "Cle", VisitorTeam = "GSW", Seasondata = season2017)
Get_Apps(HomeTeam = "Cle", VisitorTeam = "GSW", Seasondata = season2016)
Get_Apps(HomeTeam = "Cle", VisitorTeam = "GSW", Seasondata = season2015)