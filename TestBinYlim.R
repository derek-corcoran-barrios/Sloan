parameters <- expand.grid(nbins = seq(20, 40, by = 5), ylim = seq(270, 350, by = 10))

past_games_c <- readRDS("past_games.rds")

shotDataTotal2017<- readRDS("shotDataTotal2017.rds")
#####2016
shotDataTotal2016<- readRDS("shotDataTotal2016.rds")
#####2015

shotDataTotal2015<- readRDS("shotDataTotal2015.rds")

####2014
shotDataTotal2014<- readRDS("shotDataTotal2014.rds")

#####2013
shotDataTotal2013<- readRDS("shotDataTotal2013.rds")
pacman::p_load(SpatialBall)

ALL <- list()

for(j in 1:NROW(parameters)){

print(paste("Started analysis", i , "of", NROW(parameters)))
  
APPS <- list()
for(i in 1:NROW(past_games_c)) {
  if (past_games_c$Season[i] == 2017) {
    APPS[[i]] <- Get_Apps(nbins = parameters$nbins[j], MAX_Y = parameters$ylim[j], HomeTeam = past_games_c$Home[i], VisitorTeam = past_games_c$Visitor[i], Seasondata = dplyr::filter(shotDataTotal2017, GAME_DATE < past_games_c$Date[i]))
  }else if (past_games_c$Season[i] == 2016){
    APPS[[i]] <- Get_Apps(nbins = parameters$nbins[j], MAX_Y = parameters$ylim[j], HomeTeam = past_games_c$Home[i], VisitorTeam = past_games_c$Visitor[i], Seasondata = dplyr::filter(shotDataTotal2016, GAME_DATE < past_games_c$Date[i]))
  }else if (past_games_c$Season[i] == 2015){
    APPS[[i]] <- Get_Apps(nbins = parameters$nbins[j], MAX_Y = parameters$ylim[j], HomeTeam = past_games_c$Home[i], VisitorTeam = past_games_c$Visitor[i], Seasondata = dplyr::filter(shotDataTotal2015, GAME_DATE < past_games_c$Date[i]))
  }else if (past_games_c$Season[i] == 2014){
    APPS[[i]] <- Get_Apps(nbins = parameters$nbins[j], MAX_Y = parameters$ylim[j], HomeTeam = past_games_c$Home[i], VisitorTeam = past_games_c$Visitor[i], Seasondata = dplyr::filter(shotDataTotal2014, GAME_DATE < past_games_c$Date[i]))
  }else if (past_games_c$Season[i] == 2013){
    APPS[[i]] <- Get_Apps(nbins = parameters$nbins[j], MAX_Y = parameters$ylim[j], HomeTeam = past_games_c$Home[i], VisitorTeam = past_games_c$Visitor[i], Seasondata = dplyr::filter(shotDataTotal2013, GAME_DATE < past_games_c$Date[i]))
  }else{
    APPS[[i]] <- NA
  }
  print(paste(i, "of", NROW(past_games_c)))
}

APPS <- do.call("rbind", APPS)
past_games_c[,7:8] <- APPS[,1:2]

ALL[[j]] <- past_games_c
}

saveRDS(ALL, "ALL.rds")

readRDS("DFDates_c.rds")


