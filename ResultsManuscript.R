library(SpatialBall)

data("season2017")

season2016 <- readRDS("shotDataTotal2016.rds")
season2015 <- readRDS("shotDataTotal2015.rds")
season2014 <- readRDS("shotDataTotal2014.rds")
season2013 <- readRDS("shotDataTotal2013.rds")
season2012 <- readRDS("shotDataTotal2012.rds")

#Number of shots study

sum(nrow(season2017), nrow(season2016),nrow(season2015), nrow(season2014), nrow(season2013), nrow(season2012))


#SpatialNetRating

Rank <- SpatialRating(season2017)

