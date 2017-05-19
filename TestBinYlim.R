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

Get_Apps2 <- function (HomeTeam, VisitorTeam, Seasondata, nbins = 40, MAX_Y = 280) 
{
  ComparisonPPS <- function(OffTeam, DefTeam, Seasondata, nbins = 40, 
                            MAX_Y = 280) {
    Seasondata <- dplyr::filter(Seasondata, LOC_Y < MAX_Y)
    Off <- filter(Seasondata, TEAM_NAME == OffTeam)
    deff <- dplyr::filter(Seasondata, HTM == DefTeam | VTM == 
                            DefTeam & TEAM_NAME != DefTeam)
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = c(-250, 
                                                       250), ybnds = c(-51, MAX_Y), IDs = TRUE)
      data.frame(hcell2xy(h), PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG)) * 
                                             ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 
                                                    3, 2), h@cID, FUN = function(z) sum(z)/length(z)), 
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)), 
                 cid = h@cell)
    }
    Totalhex <- makeHexData(Seasondata)
    Defhex <- makeHexData(deff)
    Offhex <- makeHexData(Off)
    DeffbyCell <- merge(Totalhex, Defhex, by = "cid", all = T)
    OffByCell <- merge(Totalhex, Offhex, by = "cid", all = T)
    DiffDeff <- data.frame(x = ifelse(is.na(DeffbyCell$x.x), 
                                      DeffbyCell$x.y, DeffbyCell$x.x), y = ifelse(is.na(DeffbyCell$y.x), 
                                                                                  DeffbyCell$y.y, DeffbyCell$y.x), PPS = DeffbyCell$PPS.y - 
                             DeffbyCell$PPS.x, cid = DeffbyCell$cid, ST = DeffbyCell$ST.y)
    DiffOff <- data.frame(x = ifelse(is.na(OffByCell$x.x), 
                                     OffByCell$x.y, OffByCell$x.x), y = ifelse(is.na(OffByCell$y.x), 
                                                                               OffByCell$y.y, OffByCell$y.x), PPS = OffByCell$PPS.y - 
                            OffByCell$PPS.x, ST = OffByCell$ST.x, cid = OffByCell$cid, 
                          ST = OffByCell$ST.y)
    Comparison <- merge(DiffOff, DiffDeff, by = "cid", all = T)
    Comparison <- Comparison[, -c(6:7)]
    Comparison$Diff <- c(Comparison$PPS.x + Comparison$PPS.y)
    PPSAA <- weighted.mean((Comparison$PPS.x + Comparison$PPS.y), 
                           Comparison$ST.x, na.rm = TRUE)
    Offa <- dplyr::filter(Seasondata, HTM == OffTeam | VTM == 
                            OffTeam)
    OffCorrection <- nrow(dplyr::filter(Offa, TEAM_NAME == 
                                          OffTeam))/nrow(dplyr::filter(Offa, TEAM_NAME != OffTeam))
    Defa <- dplyr::filter(Seasondata, HTM == DefTeam | VTM == 
                            DefTeam)
    DefCorrection <- nrow(dplyr::filter(Defa, TEAM_NAME != 
                                          DefTeam))/nrow(dplyr::filter(Defa, TEAM_NAME == DefTeam))
    PPSAAc = PPSAA * ((OffCorrection * DefCorrection)/2)
    return(PPSAAc)
  }
  data("BRT")
  defAPPS <- ComparisonPPS(OffTeam = HomeTeam, DefTeam = VisitorTeam, 
                           Seasondata = Seasondata, nbins = nbins)
  offAPPS <- ComparisonPPS(OffTeam = VisitorTeam, DefTeam = HomeTeam, 
                           Seasondata = Seasondata, nbins = nbins)
  spread <- predict(BRT, data.frame(defAPPS = defAPPS, offAPPS = offAPPS))
  return(data.frame(defAPPS = defAPPS, offAPPS = offAPPS, spread = spread))
}

ALL <- list()

for(j in 1:NROW(parameters)){

print(paste("Started analysis", j , "of", NROW(parameters)))
  
APPS <- list()
for(i in 1:NROW(past_games_c)) {
  if (past_games_c$Season[i] == 2017) {
    APPS[[i]] <- Get_Apps2(nbins = parameters$nbins[j], MAX_Y = parameters$ylim[j], HomeTeam = past_games_c$Home[i], VisitorTeam = past_games_c$Visitor[i], Seasondata = dplyr::filter(shotDataTotal2017, GAME_DATE < past_games_c$Date[i]))
  }else if (past_games_c$Season[i] == 2016){
    APPS[[i]] <- Get_Apps2(nbins = parameters$nbins[j], MAX_Y = parameters$ylim[j], HomeTeam = past_games_c$Home[i], VisitorTeam = past_games_c$Visitor[i], Seasondata = dplyr::filter(shotDataTotal2016, GAME_DATE < past_games_c$Date[i]))
  }else if (past_games_c$Season[i] == 2015){
    APPS[[i]] <- Get_Apps2(nbins = parameters$nbins[j], MAX_Y = parameters$ylim[j], HomeTeam = past_games_c$Home[i], VisitorTeam = past_games_c$Visitor[i], Seasondata = dplyr::filter(shotDataTotal2015, GAME_DATE < past_games_c$Date[i]))
  }else if (past_games_c$Season[i] == 2014){
    APPS[[i]] <- Get_Apps2(nbins = parameters$nbins[j], MAX_Y = parameters$ylim[j], HomeTeam = past_games_c$Home[i], VisitorTeam = past_games_c$Visitor[i], Seasondata = dplyr::filter(shotDataTotal2014, GAME_DATE < past_games_c$Date[i]))
  }else if (past_games_c$Season[i] == 2013){
    APPS[[i]] <- Get_Apps2(nbins = parameters$nbins[j], MAX_Y = parameters$ylim[j], HomeTeam = past_games_c$Home[i], VisitorTeam = past_games_c$Visitor[i], Seasondata = dplyr::filter(shotDataTotal2013, GAME_DATE < past_games_c$Date[i]))
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

DFDates_c <- readRDS("DFDates_c.rds")
ALL <-  readRDS( "ALL.rds")


for(i in 1:length(ALL)){
  ALL[[i]] <-  merge(ALL[[i]], DFDates_c, all = TRUE)
  ALL[[i]] <- ALL[[i]][!is.na(ALL[[i]]$defAPPS),]
  ALL[[i]] <- ALL[[i]][!is.na(ALL[[i]]$day),]
}


ALLFilt_c <- list()

for(i in 1:length(ALL)){
ALLFilt_c[[i]] <- dplyr::filter(ALL[[i]], day >= 25)
ALLFilt_c[[i]]$Type <- "regular_season"
}
saveRDS(ALLFilt_c, "ALLFilt_c.rds")

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
FinalOdds$Visitor <- gsub("GS", "GSW", as.character(FinalOdds$Visitor))
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
FinalOdds$Home <- gsub("GS", "GSW", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("NOH", "NO", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("NOP", "NO", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("CLE", "Cle", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("BOS", "Bos", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("CHI", "Chi", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("ATL", "Atl", as.character(FinalOdds$Home))
FinalOdds$Home <- gsub("DET", "Det", as.character(FinalOdds$Home))

########################################################################

PLAYOFFS <- list()
for(j in 1:NROW(parameters)){
  print(paste("Started analysis", j , "of", NROW(parameters)))
  
Playoffs <- list()
for(i in 1:NROW(FinalOdds)) {
  if (FinalOdds$Season[i] == 2016){
    Playoffs[[i]] <- Get_Apps(nbins = parameters$nbins[j], MAX_Y = parameters$ylim[j], VisitorTeam = FinalOdds$Visitor[i], HomeTeam = FinalOdds$Home[i], Seasondata = dplyr::filter(shotDataTotal2016, GAME_DATE < FinalOdds$Date[i]))
  }else if (FinalOdds$Season[i] == 2015){
    Playoffs[[i]] <- Get_Apps(nbins = parameters$nbins[j], MAX_Y = parameters$ylim[j], VisitorTeam = FinalOdds$Visitor[i], HomeTeam = FinalOdds$Home[i], Seasondata = dplyr::filter(shotDataTotal2015, GAME_DATE < FinalOdds$Date[i]))
  }else if (FinalOdds$Season[i] == 2014){
    Playoffs[[i]] <- Get_Apps(nbins = parameters$nbins[j], MAX_Y = parameters$ylim[j], VisitorTeam = FinalOdds$Visitor[i], HomeTeam = FinalOdds$Home[i], Seasondata = dplyr::filter(shotDataTotal2014, GAME_DATE < FinalOdds$Date[i]))
  }else if (FinalOdds$Season[i] == 2013){
    Playoffs[[i]] <- Get_Apps(nbins = parameters$nbins[j], MAX_Y = parameters$ylim[j], VisitorTeam = FinalOdds$Visitor[i], HomeTeam = FinalOdds$Home[i], Seasondata = dplyr::filter(shotDataTotal2013, GAME_DATE < FinalOdds$Date[i]))  
  }else{
    Playoffs[[i]] <- NA
  }
  print(paste(i, "of", NROW(FinalOdds)))
}


Playoffs <- do.call("rbind", Playoffs)

FinalOdds[,7:8] <- Playoffs[,1:2]
PLAYOFFS[[j]] <- FinalOdds

}


saveRDS(PLAYOFFS, "PLAYOFFS.rds")

ALLFiltPlayoffs_c <- list()

for (i in 1:length(PLAYOFFS)){
  ALLFiltPlayoffs_c[[i]] <- plyr::rbind.fill(ALLFilt_c[[i]], PLAYOFFS[[i]])
  ALLFiltPlayoffs_c[[i]] <- dplyr::arrange(ALLFiltPlayoffs_c[[i]], Date)
}


saveRDS(ALLFiltPlayoffs_c, "ALLFiltPlayoffs_c.rds")


##Train model

#Divide in train and test set
##START FROM HERE
ALLFiltPlayoffs_c <- readRDS("ALLFiltPlayoffs_c.rds")

trains <- list()
tests <- list()

for (i in 1:length(ALLFiltPlayoffs_c)){
  trains[[i]] <- dplyr::filter(ALLFiltPlayoffs_c[[i]], Season != 2017 & Type == "regular_season" | Season != 2016 & Type == "Playoffs")
  tests[[i]] <- dplyr::filter(ALLFiltPlayoffs_c[[i]], Season == 2017 & Type == "regular_season" | Season == 2016 & Type == "Playoffs")
}

saveRDS(trains, "trains.rds")
saveRDS(tests, "tests.rds")


#####Forcast

####Caret version
library(caret)
ctrl <- trainControl(method = "repeatedcv", number=10, repeats=10)


grid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                    n.trees = seq(100, 1000, by = 50),
                    shrinkage = c(0.01, 0.1),
                    n.minobsinnode=c(1,5,10))


# train the GBM model
MODELS <- list()
for (i in 1:length(trains)){
  set.seed(7)
  print(paste("Model", i, "Started"))
  MODELS[[i]] <- train(x = trains[[i]][,c(7,8)],y = trains[[i]][,9], method = "gbm",  preProcess = c("center", "scale"), verbose = FALSE, trControl = ctrl, tuneGrid = grid)
}

saveRDS(MODELS, "MODELS.rds")

for (i in 1:length(MODELS)){
  tests[[i]]$PredictedBRT <- predict(MODELS[[i]], tests[[i]][,7:8])
}


for(i in 1:length(MODELS)){
 parameters$Rsq[i] <- postResample(pred = tests[[i]]$PredictedBRT, obs = tests[[i]]$HomeRes)[2]
}

for(i in 1:length(MODELS)){
  parameters$RsqSub[i] <- postResample(pred = tests[[i]][!is.na(tests[[i]]$VegasPred),]$PredictedBRT, obs = tests[[i]][!is.na(tests[[i]]$VegasPred),]$HomeRes)[2]
}

parameters$ID <- c(1:NROW(parameters))
for(i in 1:length(MODELS)){
  parameters$VegasRsq[i] <- postResample(pred = tests[[i]][!is.na(tests[[i]]$VegasPred),]$VegasPred, obs = tests[[i]][!is.na(tests[[i]]$VegasPred),]$HomeRes)[2]
}
saveRDS(parameters, "parameters.rds")
wireframe(Rsq ~  nbins + ylim, data = parameters, colorkey = TRUE, drape = TRUE, pretty = TRUE,scales = list(arrows = FALSE), par.settings = list(regions=list(alpha=0.75)))
wireframe(RsqSub ~  nbins + ylim, data = parameters, colorkey = TRUE, drape = TRUE, pretty = TRUE,scales = list(arrows = FALSE), par.settings = list(regions=list(alpha=0.75)))

library(tidyr)

NewPar <- gather(parameters, key = Source, velue = Rsq, Rsq, RsqSub, VegasRsq)

ggplot(NewPar, aes(x = nbins, y = Rsq)) + geom_smooth(aes(fill = Source, color= Source)) + geom_point(aes(color= Source)) + geom_line(aes(color=Source)) 
