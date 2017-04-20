
pacman::p_load(XML, lubridate)


#Gather data
## Year 2012

Months <- c("october", "november", "december", "january", "february", "march", "april")
Years <- c(2013, 2014, 2015, 2016, 2017)
URLs <- list()
for(i in 1:length(Years)){
  URLs[[i]] <- paste("http://www.basketball-reference.com/leagues/NBA_", Years[i],"_games-", Months,".html", sep = "")
}

URLs <- do.call("c", URLs)

URLs <- data.frame(URLs = URLs, Year = as.numeric(gsub("\\D", "", URLs)))
URLs$URLs <- as.character(URLs$URLs)
schedule <- list()
# import from JSON

for(i in 1:nrow(URLs)){
  schedule[[i]] <- readHTMLTable(URLs$URLs[i])
  schedule[[i]] <- schedule[[i]]$schedule
}

schedule <- do.call("rbind", schedule)

schedule$Date <- mdy(as.character(schedule$Date))
schedule[,4] <- as.numeric(as.character(schedule[,4]))
schedule[,6] <- as.numeric(as.character(schedule[,6]))

###Set dates by season
season2013 <- data.frame(Season = c(2013), Start_date_reg = dmy(c("30-10-2012")), End_date_reg = dmy(c("17-04-2013")), Start_date_playoff = dmy(c("20-04-2013")), End_date_playoff = dmy(c("20-06-2013")))
season2014 <- data.frame(Season = c(2014), Start_date_reg = dmy(c("29-10-2013")), End_date_reg = dmy(c("16-04-2014")), Start_date_playoff = dmy(c("19-04-2014")), End_date_playoff = dmy(c("15-06-2014")))
season2015 <- data.frame(Season = c(2015), Start_date_reg = dmy(c("28-10-2014")), End_date_reg = dmy(c("15-04-2015")), Start_date_playoff = dmy(c("18-04-2015")), End_date_playoff = dmy(c("16-06-2015")))
season2016 <- data.frame(Season = c(2016), Start_date_reg = dmy(c("27-10-2015")), End_date_reg = dmy(c("13-04-2016")), Start_date_playoff = dmy(c("16-04-2016")), End_date_playoff = dmy(c("19-06-2016")))
season2017 <- data.frame(Season = c(2017), Start_date_reg = dmy(c("25-10-2016")), End_date_reg = dmy(c("12-04-2017")), Start_date_playoff = dmy(c("15-04-2017")), End_date_playoff = dmy(c("18-06-2017")))

seasons <- list(season2013, season2014, season2015, season2016, season2017)
####Asign seasons

schedule$Season <- NA

for(i in 1:length(seasons)){
  schedule$Season <- ifelse(schedule$Date >= seasons[[i]]$Start_date_reg & schedule$Date <= seasons[[i]]$End_date_playoff, seasons[[i]]$Season, schedule$Season)
}


schedule[,3] <- gsub("Detroit Pistons", "Det", schedule[,3])
schedule[,3] <- gsub("Atlanta Hawks", "Atl", schedule[,3])
schedule[,3] <- gsub("Chicago Bulls", "Chi", schedule[,3])
schedule[,3] <- gsub("Boston Celtics", "Bos", schedule[,3])
schedule[,3] <- gsub("Cleveland Cavaliers", "Cle", schedule[,3])
schedule[,3] <- gsub("New Orleans Pelicans", "NO", schedule[,3])
schedule[,3] <- gsub("Golden State Warriors", "GSW", schedule[,3])
schedule[,3] <- gsub("Orlando Magic", "ORL", schedule[,3])
schedule[,3] <- gsub("Washington Wizards", "Was", schedule[,3])
schedule[,3] <- gsub("Philadelphia 76ers", "Phi", schedule[,3])
schedule[,3] <- gsub("Brooklyn Nets", "Bkn", schedule[,3])
schedule[,3] <- gsub("Utah Jazz", "Uta", schedule[,3])
schedule[,3] <- gsub("Miami Heat", "Mia", schedule[,3])
schedule[,3] <- gsub("Charlotte Hornets", "Cha", schedule[,3])
schedule[,3] <- gsub("Toronto Raptors", "Tor", schedule[,3])
schedule[,3] <- gsub("Indiana Pacers", "Ind", schedule[,3])
schedule[,3] <- gsub("Houston Rockets", "Hou", schedule[,3])
schedule[,3] <- gsub("Denver Nuggets", "Den", schedule[,3])
schedule[,3] <- gsub("Memphis Grizzlies", "Mem", schedule[,3])
schedule[,3] <- gsub("New York Knicks", "NY", schedule[,3])
schedule[,3] <- gsub("Milwaukee Bucks", "Mil", schedule[,3])
schedule[,3] <- gsub("Oklahoma City Thunder", "Okc", schedule[,3])
schedule[,3] <- gsub("San Antonio Spurs", "Sas", schedule[,3])
schedule[,3] <- gsub("Dallas Mavericks", "Dal", schedule[,3])
schedule[,3] <- gsub("Phoenix Suns", "Pho", schedule[,3])
schedule[,3] <- gsub("Portland Trail Blazers", "Por", schedule[,3])
schedule[,3] <- gsub("Los Angeles Clippers", "Lac", schedule[,3])
schedule[,3] <- gsub("Sacramento Kings", "Sac", schedule[,3])
schedule[,3] <- gsub("Los Angeles Lakers", "Lal", schedule[,3])
schedule[,3] <- gsub("Minnesota Timberwolves", "Min", schedule[,3])
schedule[,3] <- gsub("Charlotte Bobcats", "Cha", schedule[,3])
schedule[,3]<- gsub("New Orleans Hornets", "NO", schedule[,3])



schedule[,5] <- gsub("Detroit Pistons", "Det", schedule[,5])
schedule[,5] <- gsub("Atlanta Hawks", "Atl", schedule[,5])
schedule[,5] <- gsub("Chicago Bulls", "Chi", schedule[,5])
schedule[,5] <- gsub("Boston Celtics", "Bos", schedule[,5])
schedule[,5] <- gsub("Cleveland Cavaliers", "Cle", schedule[,5])
schedule[,5] <- gsub("New Orleans Pelicans", "NO", schedule[,5])
schedule[,5] <- gsub("Golden State Warriors", "GSW", schedule[,5])
schedule[,5] <- gsub("Orlando Magic", "ORL", schedule[,5])
schedule[,5] <- gsub("Washington Wizards", "Was", schedule[,5])
schedule[,5] <- gsub("Philadelphia 76ers", "Phi", schedule[,5])
schedule[,5] <- gsub("Brooklyn Nets", "Bkn", schedule[,5])
schedule[,5] <- gsub("Utah Jazz", "Uta", schedule[,5])
schedule[,5] <- gsub("Miami Heat", "Mia", schedule[,5])
schedule[,5] <- gsub("Charlotte Hornets", "Cha", schedule[,5])
schedule[,5] <- gsub("Toronto Raptors", "Tor", schedule[,5])
schedule[,5] <- gsub("Indiana Pacers", "Ind", schedule[,5])
schedule[,5] <- gsub("Houston Rockets", "Hou", schedule[,5])
schedule[,5] <- gsub("Denver Nuggets", "Den", schedule[,5])
schedule[,5] <- gsub("Memphis Grizzlies", "Mem", schedule[,5])
schedule[,5] <- gsub("New York Knicks", "NY", schedule[,5])
schedule[,5] <- gsub("Milwaukee Bucks", "Mil", schedule[,5])
schedule[,5] <- gsub("Oklahoma City Thunder", "Okc", schedule[,5])
schedule[,5] <- gsub("San Antonio Spurs", "Sas", schedule[,5])
schedule[,5] <- gsub("Dallas Mavericks", "Dal", schedule[,5])
schedule[,5] <- gsub("Phoenix Suns", "Pho", schedule[,5])
schedule[,5] <- gsub("Portland Trail Blazers", "Por", schedule[,5])
schedule[,5] <- gsub("Los Angeles Clippers", "Lac", schedule[,5])
schedule[,5] <- gsub("Sacramento Kings", "Sac", schedule[,5])
schedule[,5] <- gsub("Los Angeles Lakers", "Lal", schedule[,5])
schedule[,5] <- gsub("Minnesota Timberwolves", "Min", schedule[,5])
schedule[,5] <- gsub("Charlotte Bobcats", "Cha", schedule[,5])
schedule[,5]<- gsub("New Orleans Hornets", "NO", schedule[,5])


###seasons dates


past_games <- schedule[schedule$Date < Sys.Date(),]
past_games$defAPPS <- NA
past_games$offAPPS <- NA
past_games$HomeRes <- NA

past_games <- past_games[,c(1,3,4,5,6,10,11,12,13)]

colnames(past_games) <- c("Date", "Visitor", "Visit_PTS", "Home", "Home_PTS", "Season", "defAPPS", "offAPPS", "HomeRes")

past_games$HomeRes <- past_games$Visit_PTS - past_games$Home_PTS
past_games <- past_games[!is.na(past_games$Date),]
#######load data and functions, not needed in the real one
shotDataTotal2017<- readRDS("shotDataTotal2017.rds")
shotDataTotal2017$GAME_DATE <- ymd(shotDataTotal2017$GAME_DATE)
saveRDS(shotDataTotal2017, "shotDataTotal2017.rds")

shotDatafDef2017 <- readRDS("shotDatafDef2017.rds")

for (i in 1:length(shotDatafDef2017)){
  shotDatafDef2017[[i]]$GAME_DATE <- ymd(shotDatafDef2017[[i]]$GAME_DATE)
}
saveRDS(shotDatafDef2017, "shotDatafDef2017.rds")

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

for (i in 1:length(names(shotDatafDef2013))) {
  shotDatafDef2013[[i]]$GAME_DATE <- lubridate::ymd(shotDatafDef2013[[i]]$GAME_DATE)
}
saveRDS(shotDatafDef2013, "shotDatafDef2013.rds")

pacman::p_load(rjson, grid, gridExtra, png, RCurl, ggplot2, jpeg, hexbin, sp, knitr, raster, rasterVis, dplyr)

ComparisonPPS <- function(OffTeam, DefTown, SeasondataOff, SeasonDataDef, nbins = 40) {
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
  
  
  return(list(PPSAA = PPSAA))
}

#######defAPPS
for(i in 1:NROW(past_games)) {
if (past_games$Season[i] == 2017) {
    shotDatafDef2017Temp <- shotDatafDef2017
    shotDatafDef2017Temp[[past_games$Home[i]]] <- dplyr::filter(shotDatafDef2017[[past_games$Home[i]]], GAME_DATE < past_games$Date[i])
    past_games$defAPPS[i] <- ComparisonPPS(OffTeam = past_games$Home[i], DefTown = past_games$Visitor[i], SeasondataOff = dplyr::filter(shotDataTotal2017, GAME_DATE < past_games$Date[i]), SeasonDataDef = shotDatafDef2017Temp)
}else if (past_games$Season[i] == 2016){
  shotDatafDef2016Temp <- shotDatafDef2016
  shotDatafDef2016Temp[[past_games$Home[i]]] <- dplyr::filter(shotDatafDef2016[[past_games$Home[i]]], GAME_DATE < past_games$Date[i])
  past_games$defAPPS[i] <- ComparisonPPS(OffTeam = past_games$Home[i], DefTown = past_games$Visitor[i], SeasondataOff = dplyr::filter(shotDataTotal2016, GAME_DATE < past_games$Date[i]), SeasonDataDef = shotDatafDef2016Temp)
}else if (past_games$Season[i] == 2015){
  shotDatafDef2015Temp <- shotDatafDef2015
  shotDatafDef2015Temp[[past_games$Home[i]]] <- dplyr::filter(shotDatafDef2015[[past_games$Home[i]]], GAME_DATE < past_games$Date[i])
  past_games$defAPPS[i] <- ComparisonPPS(OffTeam = past_games$Home[i], DefTown = past_games$Visitor[i], SeasondataOff = dplyr::filter(shotDataTotal2015, GAME_DATE < past_games$Date[i]), SeasonDataDef = shotDatafDef2015Temp)
}else if (past_games$Season[i] == 2014 & i >10){
  shotDatafDef2014Temp <- shotDatafDef2014
  shotDatafDef2014Temp[[past_games$Home[i]]] <- dplyr::filter(shotDatafDef2014[[past_games$Home[i]]], GAME_DATE < past_games$Date[i])
  past_games$defAPPS[i] <- ComparisonPPS(OffTeam = past_games$Home[i], DefTown = past_games$Visitor[i], SeasondataOff = dplyr::filter(shotDataTotal2014, GAME_DATE < past_games$Date[i]), SeasonDataDef = shotDatafDef2014Temp)
}else if (past_games$Season[i] == 2013 & i >20){
  shotDatafDef2013Temp <- shotDatafDef2013
  shotDatafDef2013Temp[[past_games$Home[i]]] <- dplyr::filter(shotDatafDef2013[[past_games$Home[i]]], GAME_DATE < past_games$Date[i])
  past_games$defAPPS[i] <- ComparisonPPS(OffTeam = past_games$Home[i], DefTown = past_games$Visitor[i], SeasondataOff = dplyr::filter(shotDataTotal2013, GAME_DATE < past_games$Date[i]), SeasonDataDef = shotDatafDef2013Temp)
}else{
    past_games$defAPPS[i] <- NA
  }
}

##offAPPS

for(i in 1:NROW(past_games)) {
  if (past_games$Season[i] == 2017) {
    shotDatafDef2017Temp <- shotDatafDef2017
    shotDatafDef2017Temp[[past_games$Visitor[i]]] <- dplyr::filter(shotDatafDef2017[[past_games$Visitor[i]]], GAME_DATE < past_games$Date[i])
    past_games$offAPPS[i] <- ComparisonPPS(OffTeam = past_games$Visitor[i], DefTown = past_games$Home[i], SeasondataOff = dplyr::filter(shotDataTotal2017, GAME_DATE < past_games$Date[i]), SeasonDataDef = shotDatafDef2017Temp)
  }else if (past_games$Season[i] == 2016){
    shotDatafDef2016Temp <- shotDatafDef2016
    shotDatafDef2016Temp[[past_games$Visitor[i]]] <- dplyr::filter(shotDatafDef2016[[past_games$Visitor[i]]], GAME_DATE < past_games$Date[i])
    past_games$offAPPS[i] <- ComparisonPPS(OffTeam = past_games$Visitor[i], DefTown = past_games$Home[i], SeasondataOff = dplyr::filter(shotDataTotal2016, GAME_DATE < past_games$Date[i]), SeasonDataDef = shotDatafDef2016Temp)
  }else if (past_games$Season[i] == 2015){
    shotDatafDef2015Temp <- shotDatafDef2015
    shotDatafDef2015Temp[[past_games$Visitor[i]]] <- dplyr::filter(shotDatafDef2015[[past_games$Visitor[i]]], GAME_DATE < past_games$Date[i])
    past_games$offAPPS[i] <- ComparisonPPS(OffTeam = past_games$Visitor[i], DefTown = past_games$Home[i], SeasondataOff = dplyr::filter(shotDataTotal2015, GAME_DATE < past_games$Date[i]), SeasonDataDef = shotDatafDef2015Temp)
  }else if (past_games$Season[i] == 2014 & i >10){
    shotDatafDef2014Temp <- shotDatafDef2014
    shotDatafDef2014Temp[[past_games$Visitor[i]]] <- dplyr::filter(shotDatafDef2014[[past_games$Visitor[i]]], GAME_DATE < past_games$Date[i])
    past_games$offAPPS[i] <- ComparisonPPS(OffTeam = past_games$Visitor[i], DefTown = past_games$Home[i], SeasondataOff = dplyr::filter(shotDataTotal2014, GAME_DATE < past_games$Date[i]), SeasonDataDef = shotDatafDef2014Temp)
  }else if (past_games$Season[i] == 2013 & i >20){
    shotDatafDef2013Temp <- shotDatafDef2013
    shotDatafDef2013Temp[[past_games$Visitor[i]]] <- dplyr::filter(shotDatafDef2013[[past_games$Visitor[i]]], GAME_DATE < past_games$Date[i])
    past_games$offAPPS[i] <- ComparisonPPS(OffTeam = past_games$Visitor[i], DefTown = past_games$Home[i], SeasondataOff = dplyr::filter(shotDataTotal2013, GAME_DATE < past_games$Date[i]), SeasonDataDef = shotDatafDef2013Temp)
  }else{
    past_games$offAPPS[i] <- NA
  }
}

past_games$defAPPS <- unlist(past_games$defAPPS)

past_games$offAPPS <- unlist(past_games$offAPPS)

saveRDS(past_games, "past_games.rds")

past_games <- readRDS("past_games.rds")
##Variability


dates2017 <- unique(past_games$Date)[unique(past_games$Date) >= dmy("25-10-2016")]

DF2017 <- data.frame(Season = rep(2017, times =length(dates2017)), day = rep(NA, times =length(dates2017)), defAPPS = rep(NA, times = length(dates2017)), dates = dates2017)

for(i in 10:length(dates2017)) {
    shotDatafDef2017Temp <- shotDatafDef2017
    DF2017$day[i] <- i
    print(i)
    shotDatafDef2017Temp[["Cle"]] <- dplyr::filter(shotDatafDef2017Temp[["Cle"]], GAME_DATE < dates2017[i])
    DF2017[i,3] <- ComparisonPPS(OffTeam = "GSW", DefTown = "Cle", SeasondataOff = dplyr::filter(shotDataTotal2017, GAME_DATE < dates2017[i]), SeasonDataDef = shotDatafDef2017Temp)
  }

DF2017 <- DF2017[complete.cases(DF2017),]


#Params <- data.frame(season = 2017, As)
#ceiling(summary(Model1)$coefficients[3])
   
dates2016 <- unique(past_games$Date)[unique(past_games$Date) >= dmy("27-10-2015") & unique(past_games$Date) <= dmy("13-04-2016")]
DF2016 <- data.frame(Season = rep(2016, times =length(dates2016)), day = rep(NA, times =length(dates2016)), defAPPS = rep(NA, times = length(dates2016)), dates = dates2016)

for(i in 10:length(dates2016)) {
  shotDatafDef2016Temp <- shotDatafDef2016
  DF2016$day[i] <- i
  print(i)
  shotDatafDef2016Temp[["Cle"]] <- dplyr::filter(shotDatafDef2016Temp[["Cle"]], GAME_DATE < dates2016[i])
  DF2016[i,3] <- ComparisonPPS(OffTeam = "GSW", DefTown = "Cle", SeasondataOff = dplyr::filter(shotDataTotal2016, GAME_DATE < dates2016[i]), SeasonDataDef = shotDatafDef2016Temp)
}

DF2016 <- DF2016[complete.cases(DF2016),]



dates2015 <- unique(past_games$Date)[unique(past_games$Date) >= dmy("28-10-2014") & unique(past_games$Date) <= dmy("15-04-2015")]
DF2015 <- data.frame(Season = rep(2015, times =length(dates2015)), day = rep(NA, times =length(dates2015)), defAPPS = rep(NA, times = length(dates2015)), dates = dates2015)

for(i in 10:length(dates2015)) {
  shotDatafDef2015Temp <- shotDatafDef2015
  DF2015$day[i] <- i
  print(i)
  shotDatafDef2015Temp[["Cle"]] <- dplyr::filter(shotDatafDef2015Temp[["Cle"]], GAME_DATE < dates2015[i])
  DF2015[i,3] <- ComparisonPPS(OffTeam = "GSW", DefTown = "Cle", SeasondataOff = dplyr::filter(shotDataTotal2015, GAME_DATE < dates2015[i]), SeasonDataDef = shotDatafDef2015Temp)
}

DF2015 <- DF2015[complete.cases(DF2015),]


dates2014 <- unique(past_games$Date)[unique(past_games$Date) >= dmy("29-10-2013") & unique(past_games$Date) <= dmy("16-04-2014")]
DF2014 <- data.frame(Season = rep(2014, times =length(dates2014)), day = rep(NA, times =length(dates2014)), defAPPS = rep(NA, times = length(dates2014)), dates = dates2014)

for(i in 10:length(dates2014)) {
  shotDatafDef2014Temp <- shotDatafDef2014
  DF2014$day[i] <- i
  print(i)
  shotDatafDef2014Temp[["Cle"]] <- dplyr::filter(shotDatafDef2014Temp[["Cle"]], GAME_DATE < dates2014[i])
  DF2014[i,3] <- ComparisonPPS(OffTeam = "GSW", DefTown = "Cle", SeasondataOff = dplyr::filter(shotDataTotal2014, GAME_DATE < dates2014[i]), SeasonDataDef = shotDatafDef2014Temp)
}

DF2014 <- DF2014[complete.cases(DF2014),]


dates2013 <- unique(past_games$Date)[unique(past_games$Date) >= dmy("20-10-2012") & unique(past_games$Date) <= dmy("17-04-2013")]
DF2013 <- data.frame(Season = rep(2013, times =length(dates2013)), day = rep(NA, times =length(dates2013)), defAPPS = rep(NA, times = length(dates2013)), dates = dates2013)

for(i in 10:length(dates2013)) {
  shotDatafDef2013Temp <- shotDatafDef2013
  DF2013$day[i] <- i
  print(i)
  shotDatafDef2013Temp[["Cle"]] <- dplyr::filter(shotDatafDef2013Temp[["Cle"]], GAME_DATE < dates2013[i])
  DF2013[i,3] <- ComparisonPPS(OffTeam = "GSW", DefTown = "Cle", SeasondataOff = dplyr::filter(shotDataTotal2013, GAME_DATE < dates2013[i]), SeasonDataDef = shotDatafDef2013Temp)
}

DF2013 <- DF2013[complete.cases(DF2013),]


DFDates <- rbind(DF2013, DF2014 ,DF2015, DF2016, DF2017)
Model1 <-nls(defAPPS ~ SSasympOff(day, A, lrc, c0), data = DFDates)
DFDates$pred <- predict(Model1)
ceiling(summary(Model1)$coefficients[3])
for_filtering <- DFDates[DFDates$day ==ceiling(summary(Model1)$coefficients[3]),]

DFDates$Season <- as.factor(DFDates$Season)

ggplot(DFDates, aes(x = day, y = defAPPS))+ geom_point(aes(color = Season)) + geom_line(aes(y = pred)) + geom_vline(xintercept = ceiling(summary(Model1)$coefficients[3]))

saveRDS(DFDates, "DFDates.rds")

past_gamesFilt <- dplyr::filter(past_games, Date >= for_filtering$dates[1] & Season == 2013 
                                          | Date >= for_filtering$dates[2] & Season == 2014 
                                          | Date >= for_filtering$dates[3] & Season == 2015
                                          | Date >= for_filtering$dates[4] & Season == 2016
                                          | Date >= for_filtering$dates[5] & Season == 2017)


past_gamesFilt <- past_gamesFilt[complete.cases(past_gamesFilt),]

ggplot(past_gamesFilt, aes(x = HomeRes, y = defAPPS)) + geom_smooth()
past_gamesFilt$Type <- "regular_season"

saveRDS(for_filtering, "for_filtering.rds")
saveRDS(past_gamesFilt, "past_gamesFilt.rds")

FinalOdds <- readRDS("FinalOdds.rds")

past_gamesFiltPlayoff <- plyr::rbind.fill(past_gamesFilt, FinalOdds)
past_gamesFiltPlayoff <- dplyr::arrange(past_gamesFiltPlayoff, Date)
saveRDS(past_gamesFiltPlayoff, "past_gamesFiltPlayoff.rds")

##Train model

#Divide in train and test set
##START FROM HERE
past_gamesFiltPlayoff <- readRDS("past_gamesFiltPlayoff.rds")

#Train set playoffs 2012 through 2015 and regular season 2013 through 2016
trainNBA <- dplyr::filter(past_gamesFiltPlayoff, Season != 2017 & Type == "regular_season" | Season != 2016 & Type == "Playoffs")
testNBA <- dplyr::filter(past_gamesFiltPlayoff, Season == 2017 & Type == "regular_season" | Season == 2016 & Type == "Playoffs")


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
BRT2017_20_Abr <- train(x = trainNBA[,c(7,8)],y = trainNBA[,9], method = "gbm",  preProcess = c("center", "scale"), verbose = FALSE, trControl = ctrl, tuneGrid = grid)
saveRDS(BRT2017_20_Abr, "BRT2017_20_Abr.rds")

BRT2017_20_Abr <- readRDS("BRT2017_20_Abr.rds")

testNBA$PredictedBRT <- predict(BRT2017_20_Abr, testNBA[,7:8])


ggplot(testNBA, aes(x = HomeRes, y = PredictedBRT)) + geom_smooth() + geom_point() + xlab("Diferencia") + ylab("Diferencia predicha")

#####3d plot

For.predictions <- expand.grid(defAPPS = seq(from = min(past_gamesFiltPlayoff$defAPPS), to = max(past_gamesFiltPlayoff$defAPPS), length.out = 100), 
                               offAPPS =seq(from= min(past_gamesFiltPlayoff$offAPPS),to = max(past_gamesFiltPlayoff$offAPPS), length.out = 100))

For.predictions$Spread <- predict(BRT2017_20_Abr, For.predictions)

For.predictions2 <- For.predictions
For.predictions2$Type <- c("Predicted")
For.predictions3 <- For.predictions2[seq(from =1, to = NROW(For.predictions), by = 100),]
For.predictions3$Spread <- 0
For.predictions3$Type <- c("Push")
For.predictions2 <- rbind(For.predictions2, For.predictions3)

#Test 1
wireframe(Spread ~  offAPPS + defAPPS, group = Type, data = For.predictions2, colorkey = TRUE, drape = TRUE, pretty = TRUE,scales = list(arrows = FALSE), screen = list(z = -220, x = -80), par.settings = list(regions=list(alpha=0.75)))

