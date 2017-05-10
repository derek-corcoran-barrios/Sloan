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

APPS <- list()
for(i in 1:NROW(past_games_c)) {
  if (past_games_c$Season[i] == 2017) {
    APPS[[i]] <- Get_Apps(MAX_Y = 280, HomeTeam = past_games_c$Home[i], VisitorTeam = past_games_c$Visitor[i], Seasondata = dplyr::filter(shotDataTotal2017, GAME_DATE < past_games_c$Date[i]))
  }else if (past_games_c$Season[i] == 2016){
    APPS[[i]] <- Get_Apps(MAX_Y = 280, HomeTeam = past_games_c$Home[i], VisitorTeam = past_games_c$Visitor[i], Seasondata = dplyr::filter(shotDataTotal2016, GAME_DATE < past_games_c$Date[i]))
  }else if (past_games_c$Season[i] == 2015){
    APPS[[i]] <- Get_Apps(MAX_Y = 280, HomeTeam = past_games_c$Home[i], VisitorTeam = past_games_c$Visitor[i], Seasondata = dplyr::filter(shotDataTotal2015, GAME_DATE < past_games_c$Date[i]))
  }else if (past_games_c$Season[i] == 2014){
    APPS[[i]] <- Get_Apps(MAX_Y = 280, HomeTeam = past_games_c$Home[i], VisitorTeam = past_games_c$Visitor[i], Seasondata = dplyr::filter(shotDataTotal2014, GAME_DATE < past_games_c$Date[i]))
  }else if (past_games_c$Season[i] == 2013){
    APPS[[i]] <- Get_Apps(MAX_Y = 280, HomeTeam = past_games_c$Home[i], VisitorTeam = past_games_c$Visitor[i], Seasondata = dplyr::filter(shotDataTotal2013, GAME_DATE < past_games_c$Date[i]))
  }else{
    APPS[[i]] <- NA
  }
  print(paste(i, "of", NROW(past_games_c)))
}

APPS <- do.call("rbind", APPS)
saveRDS(APPS, "APPS.rds")

past_games_c[,7:8] <- APPS[,1:2]
#saveRDS(past_games_c, "past_games_c.rds")
#past_games_c <- readRDS("past_games_c.rds")

library(lubridate)

dates2017_c <- unique(past_games_c$Date)[unique(past_games_c$Date) >= dmy("25-10-2016")]
DF2017_c <- data.frame(Season = rep(2017, times =length(dates2017_c)), day = rep(NA, times =length(dates2017_c)), Date = dates2017_c)

for(i in 1:length(dates2017_c)) {
  DF2017_c$day[i] <- i
  print(i)
}

#Params <- data.frame(season = 2017, As)
#ceiling(summary(Model1)$coefficients[3])
library(lubridate)
dates2016_c <- unique(past_games_c$Date)[unique(past_games_c$Date) >= dmy("27-10-2015") & unique(past_games_c$Date) <= dmy("13-04-2016")]
DF2016_c <- data.frame(Season = rep(2016, times =length(dates2016_c)), day = rep(NA, times =length(dates2016_c)), Date = dates2016_c)

for(i in 1:length(dates2016_c)) {
  DF2016_c$day[i] <- i
  print(i)
}


dates2015_c <- unique(past_games_c$Date)[unique(past_games_c$Date) >= dmy("28-10-2014") & unique(past_games_c$Date) <= dmy("15-04-2015")]
DF2015_c <- data.frame(Season = rep(2015, times =length(dates2015_c)), day = rep(NA, times =length(dates2015_c)), Date = dates2015_c)

for(i in 1:length(dates2015_c)) {
  DF2015_c$day[i] <- i
  print(i)
}


dates2014_c <- unique(past_games_c$Date)[unique(past_games_c$Date) >= dmy("29-10-2013") & unique(past_games_c$Date) <= dmy("16-04-2014")]
DF2014_c <- data.frame(Season = rep(2014, times =length(dates2014_c)), day = rep(NA, times =length(dates2014_c)), Date = dates2014_c)

for(i in 1:length(dates2014_c)) {
  DF2014_c$day[i] <- i
  print(i)
}


dates2013_c <- unique(past_games_c$Date)[unique(past_games_c$Date) >= dmy("20-10-2012") & unique(past_games_c$Date) <= dmy("17-04-2013")]
DF2013_c <- data.frame(Season = rep(2013, times =length(dates2013_c)), day = rep(NA, times =length(dates2013_c)), Date = dates2013_c)

for(i in 1:length(dates2013_c)) {
  DF2013_c$day[i] <- i
  print(i)
}

DFDates_c <- rbind(DF2013_c, DF2014_c ,DF2015_c, DF2016_c, DF2017_c)

past_games_c <- merge(past_games_c, DFDates_c, all = TRUE)

library(ggplot2)
library(gridExtra)
a <- ggplot(past_games_c, aes(x = day, y = offAPPS)) + geom_line(aes(color = Visitor, lty= as.factor(Season))) + theme(legend.position = "bottom") + guides(color=guide_legend(nrow=2, byrow=TRUE)) + scale_linetype(guide = 'none')
b <- ggplot(past_games_c, aes(x = day, y = defAPPS)) + geom_line(aes(color = Visitor, lty= as.factor(Season)))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

e <- g_legend(a)

c <- a + theme(legend.position = "none")
d <- b + theme(legend.position = "none")

grid.arrange(c, d, e, heights=c(0.45, 0.45, 0.10))
past_games_c <- past_games_c[!is.na(past_games_c$defAPPS),]


past_gamesFilt_c <- dplyr::filter(past_games_c, day >= 25)

past_gamesFilt_c$Type <- "regular_season"


saveRDS(past_gamesFilt_c, "past_gamesFilt_c.rds")

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


Playoffs <- list()
for(i in 1:NROW(FinalOdds)) {
  if (FinalOdds$Season[i] == 2016){
    Playoffs[[i]] <- Get_Apps(VisitorTeam = FinalOdds$Visitor[i], HomeTeam = FinalOdds$Home[i], Seasondata = dplyr::filter(shotDataTotal2016, GAME_DATE < FinalOdds$Date[i]))
  }else if (FinalOdds$Season[i] == 2015){
    Playoffs[[i]] <- Get_Apps(VisitorTeam = FinalOdds$Visitor[i], HomeTeam = FinalOdds$Home[i], Seasondata = dplyr::filter(shotDataTotal2015, GAME_DATE < FinalOdds$Date[i]))
  }else if (FinalOdds$Season[i] == 2014){
    Playoffs[[i]] <- Get_Apps(VisitorTeam = FinalOdds$Visitor[i], HomeTeam = FinalOdds$Home[i], Seasondata = dplyr::filter(shotDataTotal2014, GAME_DATE < FinalOdds$Date[i]))
  }else if (FinalOdds$Season[i] == 2013){
    Playoffs[[i]] <- Get_Apps(VisitorTeam = FinalOdds$Visitor[i], HomeTeam = FinalOdds$Home[i], Seasondata = dplyr::filter(shotDataTotal2013, GAME_DATE < FinalOdds$Date[i]))  
  }else{
    Playoffs[[i]] <- NA
  }
  print(paste(i, "of", NROW(FinalOdds)))
}


Playoffs <- do.call("rbind", Playoffs)

FinalOdds[,7:8] <- Playoffs[,1:2]


FinalOdds_c <- FinalOdds
saveRDS(FinalOdds_c, "FinalOdds_c.rds")

past_gamesFiltPlayoff_c <- plyr::rbind.fill(past_gamesFilt_c, FinalOdds_c)
past_gamesFiltPlayoff_c <- dplyr::arrange(past_gamesFiltPlayoff_c, Date)
saveRDS(past_gamesFiltPlayoff_c, "past_gamesFiltPlayoff_c.rds")


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
BRT2017_10_May_c <- train(x = trainNBA_c[,c(7,8)],y = trainNBA_c[,9], method = "gbm",  preProcess = c("center", "scale"), verbose = TRUE, trControl = ctrl, tuneGrid = grid)
saveRDS(BRT2017_10_May_c, "BRT2017_10_May_c.rds")

BRT2017_10_May_c <- readRDS("BRT2017_10_May_c.rds")

testNBA_c$PredictedBRT <- predict(BRT2017_10_May_c, testNBA_c[,7:8])


ggplot(testNBA_c, aes(x = HomeRes, y = PredictedBRT)) + geom_smooth() + geom_point() + xlab("Diferencia") + ylab("Diferencia predicha")

################################HASTA ACA!!!!

For.predictions_c <- expand.grid(defAPPS = seq(from = min(past_gamesFiltPlayoff_c$defAPPS), to = max(past_gamesFiltPlayoff_c$defAPPS), length.out = 100), 
                                 offAPPS =seq(from= min(past_gamesFiltPlayoff_c$offAPPS, na.rm = TRUE),to = max(past_gamesFiltPlayoff_c$offAPPS, na.rm = TRUE), length.out = 100))

For.predictions_c$Spread <- predict(BRT2017_10_May_c, For.predictions_c)

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

