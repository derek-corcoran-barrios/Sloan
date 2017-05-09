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
  SeasondataOff <- dplyr::filter(SeasondataOff, LOC_Y < 270)
  Off <- filter(SeasondataOff, TEAM_NAME == OffTeam)
  #Filter the Deffensive data of the Defensive team
  deff <- SeasonDataDef[names(SeasonDataDef) == DefTown][[1]]
  deff <- dplyr::filter(deff, LOC_Y < 270)
  
  #Get the maximum and minumum values for x and y
  xbnds <- range(c(SeasondataOff$LOC_X, deff$LOC_X))
  ybnds <- range(c(SeasondataOff$LOC_Y, deff$LOC_Y))
  print(xbnds)
  print(ybnds)
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
  }else if (past_games_c$Season[i] == 2014){
    shotDatafDef2014Temp <- shotDatafDef2014
    shotDatafDef2014Temp[[past_games_c$Home[i]]] <- dplyr::filter(shotDatafDef2014[[past_games_c$Home[i]]], GAME_DATE < past_games_c$Date[i])
    past_games_c$defAPPS[i] <- ComparisonPPSc(OffTeam = past_games_c$Home[i], DefTown = past_games_c$Visitor[i], SeasondataOff = dplyr::filter(shotDataTotal2014, GAME_DATE < past_games_c$Date[i]), SeasonDataDef = shotDatafDef2014Temp)
  }else if (past_games_c$Season[i] == 2013){
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

#saveRDS(past_games_c, "past_games_c.rds")

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

#saveRDS(DFDates_c, "DFDates_c.rds")

past_gamesFilt_c <- dplyr::filter(past_games_c, Date >= for_filtering_c$dates[1] & Season == 2013 
                                | Date >= for_filtering_c$dates[2] & Season == 2014 
                                | Date >= for_filtering_c$dates[3] & Season == 2015
                                | Date >= for_filtering_c$dates[4] & Season == 2016
                                | Date >= for_filtering_c$dates[5] & Season == 2017)


past_gamesFilt_c <- past_gamesFilt_c[complete.cases(past_gamesFilt_c),]


ggplot(past_gamesFilt_c, aes(x = HomeRes, y = defAPPS)) + geom_smooth()
past_gamesFilt$Type <- "regular_season"


#saveRDS(for_filtering_c, "for_filtering_c.rds")
#saveRDS(past_gamesFilt_c, "past_gamesFilt_c.rds")

FinalOdds <- readRDS("FinalOdds.rds")
FinalOdds <- dplyr::filter(FinalOdds, Season != 2012)

FinalOdds$Visitor <- gsub("MIN" , "Min", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("LAL", "Lal", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("SAC", "Sac", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("LAC", "Lac", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("POR", "Por", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("PHX", "Pho", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("DAL", "Dal", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("SAS", "Sas", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("SAN", "Sas", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("OKC", "Okc", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("MIL", "Mil", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("NYK", "NY", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("MEM", "Mem", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("DEN", "Den", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("HOU", "Hou", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("IND", "Ind", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("TOR", "Tor", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("CHA", "Cha", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("CHR", "Cha", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("MIA", "Mia", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("UTA", "Uta", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("BKN", "Bkn", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("PHI", "Phi", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("WAS", "Was", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("ORL", "ORL", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("GSW", "GSW", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("NOH", "NO", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("NOP", "NO", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("CLE", "Cle", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("BOS", "Bos", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("CHI", "Chi", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("ATL", "Atl", as.character(FinalOdds$Visitor))
FinalOdds$Visitor <- gsub("DET", "Det", as.character(FinalOdds$Visitor))

FinalOdds$Home <- gsub("MIN" , "Min", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("LAL", "Lal", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("SAC", "Sac", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("LAC", "Lac", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("POR", "Por", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("PHX", "Pho", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("DAL", "Dal", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("SAS", "Sas", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("SAN", "Sas", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("OKC", "Okc", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("MIL", "Mil", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("NYK", "NY", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("MEM", "Mem", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("DEN", "Den", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("HOU", "Hou", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("IND", "Ind", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("TOR", "Tor", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("CHA", "Cha", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("CHR", "Cha", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("MIA", "Mia", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("UTA", "Uta", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("BKN", "Bkn", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("PHI", "Phi", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("WAS", "Was", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("ORL", "ORL", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("GSW", "GSW", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("NOH", "NO", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("NOP", "NO", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("CLE", "Cle", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("BOS", "Bos", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("CHI", "Chi", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("ATL", "Atl", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("DET", "Det", as.character(FinalOdds$Home))

for(i in 1:NROW(FinalOdds)) {
 if (FinalOdds$Season[i] == 2016){
    shotDatafDef2016Temp <- shotDatafDef2016
    shotDatafDef2016Temp[[FinalOdds$Visitor[i]]] <- dplyr::filter(shotDatafDef2016[[FinalOdds$Visitor[i]]], GAME_DATE < FinalOdds$Date[i])
    FinalOdds$offAPPS[i] <- ComparisonPPSc(OffTeam = FinalOdds$Visitor[i], DefTown = FinalOdds$Home[i], SeasondataOff = dplyr::filter(shotDataTotal2016, GAME_DATE < FinalOdds$Date[i]), SeasonDataDef = shotDatafDef2016Temp)
  }else if (FinalOdds$Season[i] == 2015){
    shotDatafDef2015Temp <- shotDatafDef2015
    shotDatafDef2015Temp[[FinalOdds$Visitor[i]]] <- dplyr::filter(shotDatafDef2015[[FinalOdds$Visitor[i]]], GAME_DATE < FinalOdds$Date[i])
    FinalOdds$offAPPS[i] <- ComparisonPPSc(OffTeam = FinalOdds$Visitor[i], DefTown = FinalOdds$Home[i], SeasondataOff = dplyr::filter(shotDataTotal2015, GAME_DATE < FinalOdds$Date[i]), SeasonDataDef = shotDatafDef2015Temp)
  }else if (FinalOdds$Season[i] == 2014 & i >10){
    shotDatafDef2014Temp <- shotDatafDef2014
    shotDatafDef2014Temp[[FinalOdds$Visitor[i]]] <- dplyr::filter(shotDatafDef2014[[FinalOdds$Visitor[i]]], GAME_DATE < FinalOdds$Date[i])
    FinalOdds$offAPPS[i] <- ComparisonPPSc(OffTeam = FinalOdds$Visitor[i], DefTown = FinalOdds$Home[i], SeasondataOff = dplyr::filter(shotDataTotal2014, GAME_DATE < FinalOdds$Date[i]), SeasonDataDef = shotDatafDef2014Temp)
  }else if (FinalOdds$Season[i] == 2013 & i >20){
    shotDatafDef2013Temp <- shotDatafDef2013
    shotDatafDef2013Temp[[FinalOdds$Visitor[i]]] <- dplyr::filter(shotDatafDef2013[[FinalOdds$Visitor[i]]], GAME_DATE < FinalOdds$Date[i])
    FinalOdds$offAPPS[i] <- ComparisonPPSc(OffTeam = FinalOdds$Visitor[i], DefTown = FinalOdds$Home[i], SeasondataOff = dplyr::filter(shotDataTotal2013, GAME_DATE < FinalOdds$Date[i]), SeasonDataDef = shotDatafDef2013Temp)
  }else{
    FinalOdds$offAPPS[i] <- NA
  }
}

FinalOdds$defAPPS <- unlist(FinalOdds$defAPPS)

FinalOdds$offAPPS <- unlist(FinalOdds$offAPPS)

FinalOdds_c <- FinalOdds
#saveRDS(FinalOdds_c, "FinalOdds_c.rds")

past_gamesFiltPlayoff_c <- plyr::rbind.fill(past_gamesFilt_c, FinalOdds_c)
past_gamesFiltPlayoff_c <- dplyr::arrange(past_gamesFiltPlayoff_c, Date)
#saveRDS(past_gamesFiltPlayoff_c, "past_gamesFiltPlayoff_c.rds")


##Train model

#Divide in train and test set
##START FROM HERE
past_gamesFiltPlayoff_c <- readRDS("past_gamesFiltPlayoff_c.rds")

#Train set playoffs 2013 through 2015 and regular season 2013 through 2016
trainNBA_c <- dplyr::filter(past_gamesFiltPlayoff_c, Season != 2017 & Type == "regular_season" | Season != 2016 & Type == "Playoffs")
testNBA_c <- dplyr::filter(past_gamesFiltPlayoff_c, Season == 2017 & Type == "regular_season" | Season == 2016 & Type == "Playoffs")


#####Forcast

####Caret version
library(caret)
ctrl <- trainControl(method = "repeatedcv", number=10, repeats=10)


grid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                    n.trees = seq(100, 1000, by = 50),
                    shrinkage = c(0.01, 0.1),
                    n.minobsinnode=c(1,5,10))


# train the GBM model
set.seed(7)
BRT2017_20_Abr_c <- train(x = trainNBA_c[,c(7,8)],y = trainNBA_c[,9], method = "gbm",  preProcess = c("center", "scale"), verbose = TRUE, trControl = ctrl, tuneGrid = grid)
#saveRDS(BRT2017_20_Abr_c, "BRT2017_20_Abr_c.rds")

BRT2017_20_Abr_c <- readRDS("BRT2017_20_Abr_c.rds")

testNBA_c$PredictedBRT <- predict(BRT2017_20_Abr_c, testNBA_c[,7:8])


ggplot(testNBA_c, aes(x = HomeRes, y = PredictedBRT)) + geom_smooth() + geom_point() + xlab("Diferencia") + ylab("Diferencia predicha")

################################HASTA ACA!!!!

For.predictions_c <- expand.grid(defAPPS = seq(from = min(past_gamesFiltPlayoff_c$defAPPS), to = max(past_gamesFiltPlayoff_c$defAPPS), length.out = 100), 
                               offAPPS =seq(from= min(past_gamesFiltPlayoff_c$offAPPS, na.rm = TRUE),to = max(past_gamesFiltPlayoff_c$offAPPS, na.rm = TRUE), length.out = 100))

For.predictions_c$Spread <- predict(BRT2017_20_Abr_c, For.predictions_c)

For.predictions2_c <- For.predictions_c
For.predictions2_c$Type <- c("Predicted")
For.predictions3_c <- For.predictions2_c[seq(from =1, to = NROW(For.predictions_c), by = 100),]
For.predictions3_c$Spread <- 0
For.predictions3_c$Type <- c("Push")
For.predictions2_c <- rbind(For.predictions2_c, For.predictions3_c)

#Test 1
wireframe(Spread ~  offAPPS + defAPPS, group = Type, data = For.predictions2_c, colorkey = TRUE, drape = TRUE, pretty = TRUE,scales = list(arrows = FALSE), screen = list(z = -220, x = -80), par.settings = list(regions=list(alpha=0.75)))

postResample(pred = testNBA_c$PredictedBRT, obs = testNBA_c$HomeRes)

summary(BRT2017_20_Abr_c$resample)

WLtestNBA_c <- testNBA_c

WLtestNBA_c$HomeRes <- as.factor(ifelse(WLtestNBA_c$HomeRes < 0, "W", "L"))
WLtestNBA_c$PredictedBRT <- as.factor(ifelse(WLtestNBA_c$PredictedBRT < 0, "W", "L"))

confusionMatrix(WLtestNBA_c$PredictedBRT, WLtestNBA_c$HomeRes, positive = "W")

df <- data.frame(matrix(ncol = 30, nrow = 30))
colnames(df) <- as.character(unique(shotDataTotal2017$TEAM_NAME))
rownames(df) <- names(shotDatafDef2017)
 
Offensive_teams <- as.character(unique(shotDataTotal2017$TEAM_NAME))
defenseve_names <- names(shotDatafDef2017)

for (i in 1:length(Offensive_teams)) {
  for (j in 1:length(defenseve_names)){
     df[rownames(df) == defenseve_names[j],colnames(df) == Offensive_teams[i]] <- ComparisonPPSc(OffTeam = Offensive_teams[i], DefTown = defenseve_names[j], SeasondataOff = shotDataTotal2017, SeasonDataDef = shotDatafDef2017)
   }
}

ROWS <- sort(rownames(df))
COLS <- sort(colnames(df))
df2 <- df
for (i in 1:length(ROWS)) {
  df[rownames(df) == COLS[i], colnames(df) == ROWS[i]] <- NA 
}

offrating <- colMeans(df, na.rm = TRUE)*50
defrating <- rowMeans(df, na.rm = TRUE)*-50

offratingDF <- data.frame(Team = colnames(df), offrating = offrating)
defratingDF <- data.frame(Team = rownames(df), defrating = defrating)

netDF <- merge.data.frame(offratingDF, defratingDF)
netDF$netrating <- netDF$offrating + netDF$defrating

