past_games_c <- readRDS("past_games.rds")

shotDataTotal2017<- readRDS("shotDataTotal2017.rds")
shotDatafDef2017 <- readRDS("shotDatafDef2017.rds")

#####2016
shotDataTotal2016<- readRDS("shotDataTotal2016.rds")
shotDatafDef2016 <- readRDS("shotDatafDef2016.rds")

#####2015

shotDataTotal2015<- readRDS("shotDataTotal2015.rds")
shotDatafDef2015 <- readRDS("shotDatafDef2015.rds")

####2014

shotDataTotal2014<- readRDS("shotDataTotal2014.rds")
shotDatafDef2014 <- readRDS("shotDatafDef2014.rds")


#####2013

shotDataTotal2013<- readRDS("shotDataTotal2013.rds")
shotDatafDef2013 <- readRDS("shotDatafDef2013.rds")



pacman::p_load(rjson, grid, gridExtra, png, RCurl, ggplot2, jpeg, hexbin, sp, knitr, raster, rasterVis, dplyr)

ComparisonPPSc <- function(OffTeam, DefTown, SeasondataOff, SeasonDataDef, nbins = 40) {
  #Filter the offensive data of the Offensive Team
  Off <- filter(SeasondataOff, TEAM_NAME == OffTeam)
  #Filter the Deffensive data of the Defensive team
  deff <- SeasonDataDef[names(SeasonDataDef) == DefTown][[1]]
  #Get the maximum and minumum values for x and y
  xbnds <- range(c(SeasondataOff$LOC_X, deff$LOC_X))
  ybnds <- range(c(SeasondataOff$LOC_Y, deff$LOC_Y))
  #Make hexbin dataframes out of the teams
  makeHexData <- function(df) {
    h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
    data.frame(hcell2xy(h),
               PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
               ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
               cid = h@cell)
  }
  ##Total NBA data
  Totalhex <- makeHexData(SeasondataOff)
  ##Defensive team data
  Defhex <- makeHexData(deff)
  ##Offensive team data
  Offhex <- makeHexData(Off)
  #Merge offensive and deffensive data with total data by Cell id
  DeffbyCell <- merge(Totalhex, Defhex, by = "cid", all = T)
  OffByCell <- merge(Totalhex, Offhex, by = "cid", all = T)
  ##  when calculating the difference empty cells should count as 0
  DeffbyCell$PPS.x[is.na(DeffbyCell$PPS.x)] <- 0
  DeffbyCell$PPS.y[is.na(DeffbyCell$PPS.y)] <- 0
  DeffbyCell$ST.y[is.na(DeffbyCell$ST.y)] <- 0
  
  OffByCell$PPS.x[is.na(OffByCell$PPS.x)] <- 0
  OffByCell$PPS.y[is.na(OffByCell$PPS.y)] <- 0
  OffByCell$ST.y[is.na(OffByCell$ST.y)] <- 0
  #  make a "difference" data.frame
  DiffDeff <- data.frame(x = ifelse(is.na(DeffbyCell$x.x), DeffbyCell$x.y, DeffbyCell$x.x),
                         y = ifelse(is.na(DeffbyCell$y.x), DeffbyCell$y.y, DeffbyCell$y.x),
                         PPS= DeffbyCell$PPS.y - DeffbyCell$PPS.x,
                         cid= DeffbyCell$cid, 
                         ST = DeffbyCell$ST.y)
  
  DiffOff <- data.frame(x = ifelse(is.na(OffByCell$x.x), OffByCell$x.y, OffByCell$x.x),
                        y = ifelse(is.na(OffByCell$y.x), OffByCell$y.y, OffByCell$y.x),
                        PPS= OffByCell$PPS.y - OffByCell$PPS.x,
                        ST = OffByCell$ST.x,
                        cid = OffByCell$cid, 
                        ST = OffByCell$ST.y)
  #make team comparisons
  Comparison <- merge(DiffOff, DiffDeff, by = "cid", all = T) 
  Comparison <- Comparison[,-c(6:7)]
  Comparison$Diff <- c(Comparison$PPS.x + Comparison$PPS.y)
  
  
  PPSAA <- weighted.mean((Comparison$PPS.x + Comparison$PPS.y), Comparison$ST.x, na.rm = TRUE)
  Offa <- dplyr::filter(SeasondataOff, HTM == OffTeam | VTM == OffTeam)
  OffCorrection <- nrow(dplyr::filter(Offa, TEAM_NAME == OffTeam))/nrow(dplyr::filter(Offa, TEAM_NAME != OffTeam))
  Defa <- dplyr::filter(SeasondataOff, HTM == DefTown | VTM == DefTown)
  DefCorrection <- nrow(dplyr::filter(Defa, TEAM_NAME != DefTown))/nrow(dplyr::filter(Defa, TEAM_NAME == DefTown))
  PPSAAc = PPSAA*((OffCorrection*DefCorrection)/2)
  return(list(PPSAAc = PPSAAc))
}


############################################

for(i in 1:NROW(past_games_c)) {
  if (past_games_c$Season[i] == 2017) {
    shotDatafDef2017Temp <- shotDatafDef2017
    shotDatafDef2017Temp[[past_games_c$Home[i]]] <- dplyr::filter(shotDatafDef2017[[past_games_c$Home[i]]], GAME_DATE < past_games_c$Date[i])
    past_games_c$defAPPS[i] <- ComparisonPPSc(OffTeam = past_games_c$Home[i], DefTown = past_games_c$Visitor[i], SeasondataOff = dplyr::filter(shotDataTotal2017, GAME_DATE < past_games_c$Date[i]), SeasonDataDef = shotDatafDef2017Temp)
  }else if (past_games_c$Season[i] == 2016){
    shotDatafDef2016Temp <- shotDatafDef2016
    shotDatafDef2016Temp[[past_games_c$Home[i]]] <- dplyr::filter(shotDatafDef2016[[past_games_c$Home[i]]], GAME_DATE < past_games_c$Date[i])
    past_games_c$defAPPS[i] <- ComparisonPPSc(OffTeam = past_games_c$Home[i], DefTown = past_games_c$Visitor[i], SeasondataOff = dplyr::filter(shotDataTotal2016, GAME_DATE < past_games_c$Date[i]), SeasonDataDef = shotDatafDef2016Temp)
  }else if (past_games_c$Season[i] == 2015){
    shotDatafDef2015Temp <- shotDatafDef2015
    shotDatafDef2015Temp[[past_games_c$Home[i]]] <- dplyr::filter(shotDatafDef2015[[past_games_c$Home[i]]], GAME_DATE < past_games_c$Date[i])
    past_games_c$defAPPS[i] <- ComparisonPPSc(OffTeam = past_games_c$Home[i], DefTown = past_games_c$Visitor[i], SeasondataOff = dplyr::filter(shotDataTotal2015, GAME_DATE < past_games_c$Date[i]), SeasonDataDef = shotDatafDef2015Temp)
  }else if (past_games_c$Season[i] == 2014 & i >10){
    shotDatafDef2014Temp <- shotDatafDef2014
    shotDatafDef2014Temp[[past_games_c$Home[i]]] <- dplyr::filter(shotDatafDef2014[[past_games_c$Home[i]]], GAME_DATE < past_games_c$Date[i])
    past_games_c$defAPPS[i] <- ComparisonPPSc(OffTeam = past_games_c$Home[i], DefTown = past_games_c$Visitor[i], SeasondataOff = dplyr::filter(shotDataTotal2014, GAME_DATE < past_games_c$Date[i]), SeasonDataDef = shotDatafDef2014Temp)
  }else if (past_games_c$Season[i] == 2013 & i >20){
    shotDatafDef2013Temp <- shotDatafDef2013
    shotDatafDef2013Temp[[past_games_c$Home[i]]] <- dplyr::filter(shotDatafDef2013[[past_games_c$Home[i]]], GAME_DATE < past_games_c$Date[i])
    past_games_c$defAPPS[i] <- ComparisonPPSc(OffTeam = past_games_c$Home[i], DefTown = past_games_c$Visitor[i], SeasondataOff = dplyr::filter(shotDataTotal2013, GAME_DATE < past_games_c$Date[i]), SeasonDataDef = shotDatafDef2013Temp)
  }else{
    past_games_c$defAPPS[i] <- NA
  }
}

##offAPPS

for(i in 1:NROW(past_games_c)) {
  if (past_games_c$Season[i] == 2017) {
    shotDatafDef2017Temp <- shotDatafDef2017
    shotDatafDef2017Temp[[past_games_c$Visitor[i]]] <- dplyr::filter(shotDatafDef2017[[past_games_c$Visitor[i]]], GAME_DATE < past_games_c$Date[i])
    past_games_c$offAPPS[i] <- ComparisonPPSc(OffTeam = past_games_c$Visitor[i], DefTown = past_games_c$Home[i], SeasondataOff = dplyr::filter(shotDataTotal2017, GAME_DATE < past_games_c$Date[i]), SeasonDataDef = shotDatafDef2017Temp)
  }else if (past_games_c$Season[i] == 2016){
    shotDatafDef2016Temp <- shotDatafDef2016
    shotDatafDef2016Temp[[past_games_c$Visitor[i]]] <- dplyr::filter(shotDatafDef2016[[past_games_c$Visitor[i]]], GAME_DATE < past_games_c$Date[i])
    past_games_c$offAPPS[i] <- ComparisonPPSc(OffTeam = past_games_c$Visitor[i], DefTown = past_games_c$Home[i], SeasondataOff = dplyr::filter(shotDataTotal2016, GAME_DATE < past_games_c$Date[i]), SeasonDataDef = shotDatafDef2016Temp)
  }else if (past_games_c$Season[i] == 2015){
    shotDatafDef2015Temp <- shotDatafDef2015
    shotDatafDef2015Temp[[past_games_c$Visitor[i]]] <- dplyr::filter(shotDatafDef2015[[past_games_c$Visitor[i]]], GAME_DATE < past_games_c$Date[i])
    past_games_c$offAPPS[i] <- ComparisonPPSc(OffTeam = past_games_c$Visitor[i], DefTown = past_games_c$Home[i], SeasondataOff = dplyr::filter(shotDataTotal2015, GAME_DATE < past_games_c$Date[i]), SeasonDataDef = shotDatafDef2015Temp)
  }else if (past_games_c$Season[i] == 2014 & i >10){
    shotDatafDef2014Temp <- shotDatafDef2014
    shotDatafDef2014Temp[[past_games_c$Visitor[i]]] <- dplyr::filter(shotDatafDef2014[[past_games_c$Visitor[i]]], GAME_DATE < past_games_c$Date[i])
    past_games_c$offAPPS[i] <- ComparisonPPSc(OffTeam = past_games_c$Visitor[i], DefTown = past_games_c$Home[i], SeasondataOff = dplyr::filter(shotDataTotal2014, GAME_DATE < past_games_c$Date[i]), SeasonDataDef = shotDatafDef2014Temp)
  }else if (past_games_c$Season[i] == 2013 & i >20){
    shotDatafDef2013Temp <- shotDatafDef2013
    shotDatafDef2013Temp[[past_games_c$Visitor[i]]] <- dplyr::filter(shotDatafDef2013[[past_games_c$Visitor[i]]], GAME_DATE < past_games_c$Date[i])
    past_games_c$offAPPS[i] <- ComparisonPPSc(OffTeam = past_games_c$Visitor[i], DefTown = past_games_c$Home[i], SeasondataOff = dplyr::filter(shotDataTotal2013, GAME_DATE < past_games_c$Date[i]), SeasonDataDef = shotDatafDef2013Temp)
  }else{
    past_games_c$offAPPS[i] <- NA
  }
}

past_games_c$defAPPS <- unlist(past_games_c$defAPPS)

past_games_c$offAPPS <- unlist(past_games_c$offAPPS)

saveRDS(past_games_c, "past_games_c.rds")

past_games_c <- readRDS("past_games_c.rds")

dates2017_c <- unique(past_games_c$Date)[unique(past_games_c$Date) >= dmy("25-10-2016")]

DF2017_c <- data.frame(Season = rep(2017, times =length(dates2017_c)), day = rep(NA, times =length(dates2017_c)), defAPPS = rep(NA, times = length(dates2017_c)), dates = dates2017_c)

for(i in 10:length(dates2017_c)) {
  shotDatafDef2017Temp <- shotDatafDef2017
  DF2017_c$day[i] <- i
  print(i)
  shotDatafDef2017Temp[["Cle"]] <- dplyr::filter(shotDatafDef2017Temp[["Cle"]], GAME_DATE < dates2017_c[i])
  DF2017_c[i,3] <- ComparisonPPSc(OffTeam = "GSW", DefTown = "Cle", SeasondataOff = dplyr::filter(shotDataTotal2017, GAME_DATE < dates2017_c[i]), SeasonDataDef = shotDatafDef2017Temp)
}

DF2017_c <- DF2017_c[complete.cases(DF2017_c),]


#Params <- data.frame(season = 2017, As)
#ceiling(summary(Model1)$coefficients[3])

dates2016_c <- unique(past_games_c$Date)[unique(past_games_c$Date) >= dmy("27-10-2015") & unique(past_games_c$Date) <= dmy("13-04-2016")]
DF2016_c <- data.frame(Season = rep(2016, times =length(dates2016_c)), day = rep(NA, times =length(dates2016_c)), defAPPS = rep(NA, times = length(dates2016_c)), dates = dates2016_c)

for(i in 10:length(dates2016_c)) {
  shotDatafDef2016Temp <- shotDatafDef2016
  DF2016_c$day[i] <- i
  print(i)
  shotDatafDef2016Temp[["Cle"]] <- dplyr::filter(shotDatafDef2016Temp[["Cle"]], GAME_DATE < dates2016_c[i])
  DF2016_c[i,3] <- ComparisonPPSc(OffTeam = "GSW", DefTown = "Cle", SeasondataOff = dplyr::filter(shotDataTotal2016, GAME_DATE < dates2016_c[i]), SeasonDataDef = shotDatafDef2016Temp)
}

DF2016_c <- DF2016_c[complete.cases(DF2016_c),]



dates2015_c <- unique(past_games_c$Date)[unique(past_games_c$Date) >= dmy("28-10-2014") & unique(past_games_c$Date) <= dmy("15-04-2015")]
DF2015_c <- data.frame(Season = rep(2015, times =length(dates2015_c)), day = rep(NA, times =length(dates2015_c)), defAPPS = rep(NA, times = length(dates2015_c)), dates = dates2015_c)

for(i in 10:length(dates2015_c)) {
  shotDatafDef2015Temp <- shotDatafDef2015
  DF2015_c$day[i] <- i
  print(i)
  shotDatafDef2015Temp[["Cle"]] <- dplyr::filter(shotDatafDef2015Temp[["Cle"]], GAME_DATE < dates2015_c[i])
  DF2015_c[i,3] <- ComparisonPPSc(OffTeam = "GSW", DefTown = "Cle", SeasondataOff = dplyr::filter(shotDataTotal2015, GAME_DATE < dates2015_c[i]), SeasonDataDef = shotDatafDef2015Temp)
}

DF2015_c <- DF2015_c[complete.cases(DF2015_c),]


dates2014_c <- unique(past_games_c$Date)[unique(past_games_c$Date) >= dmy("29-10-2013") & unique(past_games_c$Date) <= dmy("16-04-2014")]
DF2014_c <- data.frame(Season = rep(2014, times =length(dates2014_c)), day = rep(NA, times =length(dates2014_c)), defAPPS = rep(NA, times = length(dates2014_c)), dates = dates2014_c)

for(i in 10:length(dates2014_c)) {
  shotDatafDef2014Temp <- shotDatafDef2014
  DF2014_c$day[i] <- i
  print(i)
  shotDatafDef2014Temp[["Cle"]] <- dplyr::filter(shotDatafDef2014Temp[["Cle"]], GAME_DATE < dates2014_c[i])
  DF2014_c[i,3] <- ComparisonPPSc(OffTeam = "GSW", DefTown = "Cle", SeasondataOff = dplyr::filter(shotDataTotal2014, GAME_DATE < dates2014_c[i]), SeasonDataDef = shotDatafDef2014Temp)
}

DF2014_c <- DF2014_c[complete.cases(DF2014_c),]


dates2013_c <- unique(past_games_c$Date)[unique(past_games_c$Date) >= dmy("20-10-2012") & unique(past_games_c$Date) <= dmy("17-04-2013")]
DF2013_c <- data.frame(Season = rep(2013, times =length(dates2013_c)), day = rep(NA, times =length(dates2013_c)), defAPPS = rep(NA, times = length(dates2013_c)), dates = dates2013_c)

for(i in 10:length(dates2013_c)) {
  shotDatafDef2013Temp <- shotDatafDef2013
  DF2013_c$day[i] <- i
  print(i)
  shotDatafDef2013Temp[["Cle"]] <- dplyr::filter(shotDatafDef2013Temp[["Cle"]], GAME_DATE < dates2013_c[i])
  DF2013_c[i,3] <- ComparisonPPSc(OffTeam = "GSW", DefTown = "Cle", SeasondataOff = dplyr::filter(shotDataTotal2013, GAME_DATE < dates2013_c[i]), SeasonDataDef = shotDatafDef2013Temp)
}

DF2013_c <- DF2013_c[complete.cases(DF2013_c),]


DFDates_c <- rbind(DF2013_c, DF2014_c ,DF2015_c, DF2016_c, DF2017_c)
Model1_c <-nls(defAPPS ~ SSasympOff(day, A, lrc, c0), data = DFDates_c)
DFDates_c$pred <- predict(Model1_c)
ceiling(summary(Model1_c)$coefficients[3])
for_filtering_c <- DFDates_c[DFDates_c$day ==ceiling(summary(Model1_c)$coefficients[3]),]

DFDates_c$Season <- as.factor(DFDates_c$Season)

ggplot(DFDates_c, aes(x = day, y = defAPPS))+ geom_point(aes(color = Season)) + geom_line(aes(y = pred)) + geom_vline(xintercept = ceiling(summary(Model1_c)$coefficients[3]))

saveRDS(DFDates_c, "DFDates_c.rds")

past_gamesFilt <- dplyr::filter(past_games, Date >= for_filtering$dates[1] & Season == 2013 
                                | Date >= for_filtering$dates[2] & Season == 2014 
                                | Date >= for_filtering$dates[3] & Season == 2015
                                | Date >= for_filtering$dates[4] & Season == 2016
                                | Date >= for_filtering$dates[5] & Season == 2017)
