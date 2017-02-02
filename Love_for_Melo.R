
library(rjson)
library(grid)
library(gridExtra)
library(png)
library(RCurl)
library(ggplot2)
library(jpeg)
library(hexbin)
library(sp)
library(knitr)
library(raster)
library(rasterVis)
library(dplyr)

#function to make the hexbin dataframe

#shot Comparison function

shotDatafDef2017 <- readRDS("shotDatafDef2017.rds")
shotDataTotal2017 <- readRDS("shotDataTotal2017.rds")

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
  
  
  PPSAA <- weighted.mean((Comparison$PPS.x + Comparison$PPS.y), Comparison$ST.x)
  
  
  return(list(PPSAA = PPSAA))
}

#Normal Stuff

Offensive_teams <- as.character(unique(shotDataTotal2017$TEAM_NAME))

defenseve_names <- names(shotDatafDef2017)

dfNORMAL <- data.frame(matrix(ncol = 30, nrow = 30))
colnames(dfNORMAL) <- as.character(unique(shotDataTotal2017$TEAM_NAME))
rownames(dfNORMAL) <- names(shotDatafDef2017)

for (i in 1:length(Offensive_teams)) {
  for (j in 1:length(defenseve_names)){
    dfNORMAL[rownames(dfNORMAL) == defenseve_names[j],colnames(dfNORMAL) == Offensive_teams[i]] <- ComparisonPPS(OffTeam = Offensive_teams[i], DefTown = defenseve_names[j], SeasondataOff = shotDataTotal2017, SeasonDataDef = shotDatafDef2017, nbins = 30)$PPSAA
  }
}


###################Love to Knicks

shotDataTotal2017b <- shotDataTotal2017


shotDataTotal2017b$TEAM_NAME <-ifelse(shotDataTotal2017b$PLAYER_NAME == "Stephen Curry", "Injured", shotDataTotal2017b$TEAM_NAME)
shotDataTotal2017b$TEAM_NAME <-ifelse(shotDataTotal2017b$PLAYER_NAME == "Zaza Pachulia", "Injured", shotDataTotal2017b$TEAM_NAME)


dfTRADE <- data.frame(matrix(ncol = 30, nrow = 30))
colnames(dfTRADE) <- as.character(unique(shotDataTotal2017$TEAM_NAME))
rownames(dfTRADE) <- names(shotDatafDef2017)

for (i in 1:length(Offensive_teams)) {
  for (j in 1:length(defenseve_names)){
    dfTRADE[rownames(dfTRADE) == defenseve_names[j],colnames(dfTRADE) == Offensive_teams[i]] <- ComparisonPPS(OffTeam = Offensive_teams[i], DefTown = defenseve_names[j], SeasondataOff = shotDataTotal2017b, SeasonDataDef = shotDatafDef2017, nbins = 30)$PPSAA
  }
}


###############Projected Season

future_games <- schedule[schedule$Date == Sys.Date(),]
future_games$defAPPS <- NA
future_games$offAPPS <- NA
future_games$spread <- NA

for(i in 1:NROW(future_games)) {
  future_games$defAPPS[i] <- ComparisonPPS(OffTeam = future_games$`Home/Neutral`[i], DefTown = future_games$`Visitor/Neutral`[i], SeasondataOff = shotDataTotal2017b, SeasonDataDef = shotDatafDef2017)
}

for(i in 1:NROW(future_games)) {
  future_games$offAPPS[i] <- ComparisonPPS(OffTeam = future_games$`Visitor/Neutral`[i], DefTown = future_games$`Home/Neutral`[i], SeasondataOff = shotDataTotal2017b, SeasonDataDef = shotDatafDef2017)
}

library(caret) 

BRT <- readRDS("BRT2017_17_Jan.rds")


future_trade<- future_games

future_trade$spread <- predict(BRT, data.frame(defAPPS = unlist(future_trade$defAPPS), offAPPS = unlist(future_trade$offAPPS)), type="raw")


future_trade <- future_trade[,c(1,3,5,12)]


future_trade$Home <- ifelse(future_trade$spread < 0, "W", "L")

future_trade$Visit <- ifelse(future_trade$spread > 0, "W", "L")


Home <- cbind(future_trade$`Home/Neutral`, future_trade$Home)

colnames(Home) <- c("Team", "Result")

Visit <- cbind(future_trade$`Visitor/Neutral`, future_trade$Visit)

colnames(Visit) <- c("Team", "Result")

Trade_Added_Stand <- data.frame(rbind(Home, Visit))
#Wins
Trade_Added_Stand_W <- dplyr::filter(Trade_Added_Stand, Result == "W")

Trade_Added_Stand_W <- group_by(Trade_Added_Stand_W, Team)

Trade_Added_Stand_W <- dplyr::summarize(Trade_Added_Stand_W, W = n())

#Loses

Trade_Added_Stand_L <- dplyr::filter(Trade_Added_Stand, Result == "L")

Trade_Added_Stand_L <- group_by(Trade_Added_Stand_L, Team)

Trade_Added_Stand_L <- dplyr::summarize(Trade_Added_Stand_L, L = n())

Trade_Added_Stand <- merge.data.frame(Trade_Added_Stand_W, Trade_Added_Stand_L, all = TRUE)


#####Standing scraper

library(XML)

Trade_Standings <- "http://www.basketball-reference.com/leagues/NBA_2017.html"

Trade_Standings <- readHTMLTable(Trade_Standings)

Trade_Standings <- list(Western = Trade_Standings$confs_standings_W, Eastern = Trade_Standings$confs_standings_E)

Trade_Standings[[1]]$Conference <- c("West")

Trade_Standings[[2]]$Conference <- c("East")

colnames(Trade_Standings[[1]]) <- c("Team", "Current-W", "Current-L", "pct", "GB", "PS/G", "PA/G", "SRS", "Conference")

colnames(Trade_Standings[[2]]) <- c("Team", "Current-W", "Current-L", "pct", "GB", "PS/G", "PA/G", "SRS", "Conference")


Trade_Standings <- rbind(Trade_Standings[[1]], Trade_Standings[[2]])

Trade_Standings <- Trade_Standings[,c(1,2,3,9)]

Trade_Standings$Team <- gsub("76ers", "Phi", Trade_Standings$Team)


Trade_Standings$Team <- gsub("(?<=\\b[A-Z])[^A-Z]+", "", Trade_Standings$Team, perl = TRUE)

Trade_Standings$Team <- gsub("DP", "Det", Trade_Standings$Team)
Trade_Standings$Team<- gsub("AH", "Atl", Trade_Standings$Team)
Trade_Standings$Team <- gsub("CB", "Chi", Trade_Standings$Team)
Trade_Standings$Team<- gsub("BC", "Bos", Trade_Standings$Team)
Trade_Standings$Team<- gsub("CC", "Cle", Trade_Standings$Team)
Trade_Standings$Team<- gsub("NOP", "NO", Trade_Standings$Team)
Trade_Standings$Team<- gsub("OM", "ORL", Trade_Standings$Team)
Trade_Standings$Team<- gsub("WW", "Was", Trade_Standings$Team)
Trade_Standings$Team<- gsub("BN", "Bkn", Trade_Standings$Team)
Trade_Standings$Team<- gsub("UJ", "Uta", Trade_Standings$Team)
Trade_Standings$Team<- gsub("MH", "Mia", Trade_Standings$Team)
Trade_Standings$Team<- gsub("CH", "Cha", Trade_Standings$Team)
Trade_Standings$Team<- gsub("TR", "Tor", Trade_Standings$Team)
Trade_Standings$Team<- gsub("IP", "Ind", Trade_Standings$Team)
Trade_Standings$Team<- gsub("HR", "Hou", Trade_Standings$Team)
Trade_Standings$Team<- gsub("DN", "Den", Trade_Standings$Team)
Trade_Standings$Team<- gsub("MG", "Mem", Trade_Standings$Team)
Trade_Standings$Team<- gsub("NYK", "NY", Trade_Standings$Team)
Trade_Standings$Team<- gsub("MB", "Mil", Trade_Standings$Team)
Trade_Standings$Team<- gsub("OCT", "Okc", Trade_Standings$Team)
Trade_Standings$Team<- gsub("SAS", "Sas", Trade_Standings$Team)
Trade_Standings$Team<- gsub("DM", "Dal", Trade_Standings$Team)
Trade_Standings$Team<- gsub("PS", "Pho", Trade_Standings$Team)
Trade_Standings$Team<- gsub("PTB", "Por", Trade_Standings$Team)
Trade_Standings$Team<- gsub("LAC", "Lac", Trade_Standings$Team)
Trade_Standings$Team<- gsub("SK", "Sac", Trade_Standings$Team)
Trade_Standings$Team<- gsub("LAL", "Lal", Trade_Standings$Team)
Trade_Standings$Team<- gsub("MT", "Min", Trade_Standings$Team)
Trade_Standings$Team<- gsub("PP", "Phi", Trade_Standings$Team)

Trade_Proj_Stand <- merge.data.frame(Trade_Standings, Trade_Added_Stand, all = TRUE)

Trade_Proj_Stand[,2] <- as.numeric(as.character(Trade_Proj_Stand[,2]))
Trade_Proj_Stand[,3] <- as.numeric(as.character(Trade_Proj_Stand[,3]))


Trade_Proj_Stand$W <- Trade_Proj_Stand$`Current-W`+Trade_Proj_Stand$W
Trade_Proj_Stand$L <- Trade_Proj_Stand$`Current-L`+Trade_Proj_Stand$L

Trade_Proj_Stand <- arrange(Trade_Proj_Stand, Conference,desc(W))

write.csv(Trade_Proj_Stand, "Trade_Proj_Stand.csv")
saveRDS(Trade_Proj_Stand, "Trade_Proj_Stand.rds")

ShotComparisonGraph2 <- function(OffTeam, DefTown, SeasondataOff, SeasonDataDef, nbins = 30, maxsize = 7, quant = 0.7, focus = "all") {
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
  
  
  PPSAA <- weighted.mean((Comparison$PPS.x + Comparison$PPS.y), Comparison$ST.x)
  
  
  #Legend extractor
  g_legend <- function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  #Function to transform hexbins into polygons
  hex_coord_df <- function(x, y, width, height, size = 1) {
    # like hex_coord but returns a dataframe of vertices grouped by an id variable
    dx <- log(size * width / 6)
    dy <- log(size * height / 2 / sqrt(3))
    
    hex_x <- rbind(x - 2 * dx, x - dx, x + dx, x + 2 * dx, x + dx, x - dx)
    hex_y <- rbind(y, y + dy, y + dy, y, y - dy, y - dy)
    id    <- rep(1:length(x), each=6)
    
    data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
  }
  
  #Filter by quantile and focus
  if (focus == "all") {
    DiffOff <- filter(DiffOff, ST > quantile(DiffOff$ST, probs = quant))
    DiffDeff <- filter(DiffDeff, ST > quantile(DiffDeff$ST, probs = quant))
    Comparison <- filter(Comparison, ST.x > quantile(Comparison$ST.x, probs = quant))
  }
  if (focus == "plus"){
    DiffOff <- filter(DiffOff, ST > quantile(DiffOff$ST, probs = quant))
    DiffDeff <- filter(DiffDeff, ST > quantile(DiffDeff$ST, probs = quant))
    Comparison <- filter(Comparison, ST.x > quantile(Comparison$ST.x, probs = quant))
    Comparison <- filter(Comparison, Diff >= 0)
  }
  
  if (focus == "minus") {
    DiffOff <- filter(DiffOff, ST > quantile(DiffOff$ST, probs = quant))
    DiffDeff <- filter(DiffDeff, ST > quantile(DiffDeff$ST, probs = quant))
    Comparison <- filter(Comparison, ST.x > quantile(Comparison$ST.x, probs = quant))
    Comparison <- filter(Comparison, Diff <= 0)
  }
  #Transform Hexbins into polygons
  
  DFOFF <- hex_coord_df(DiffOff$x, DiffOff$y, (0.05*DiffOff$ST), (0.05*DiffOff$ST), size =1)
  DFOFF$PPS <- rep(DiffOff$PPS, each = 6)
  
  DFDEF <- hex_coord_df(DiffDeff$x, DiffDeff$y, DiffDeff$ST, DiffDeff$ST, size =1)
  DFDEF$PPS <- rep(DiffDeff$PPS, each = 6)
  
  DFDIF <- hex_coord_df(Comparison$x.x, Comparison$y.x, (0.05*Comparison$ST.x),(0.05*Comparison$ST.x), size =1)
  DFDIF$Dif <- rep(Comparison$Diff, each = 6)
  
  #Create Legend
  OFFLEG <- ggplot(DFOFF, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(low ="blue", high = "red", limits=c(-1.4, 1.4)) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270))+ theme(legend.position="bottom") +  ggtitle(paste(OffTeam, "Offensive\n Shot Chart", sep = " "))
  leg<-g_legend(OFFLEG)
  
  OFF <- ggplot(DFOFF, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(low ="blue", high = "red", limits=c(-1.4, 1.4)) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270))+ theme(legend.position="none") +  ggtitle(paste(OffTeam, "Offensive\n Shot Chart", sep = " "))
  DEF <- ggplot(DFDEF, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS))+ scale_fill_gradient2(low ="blue", high = "red", limits=c(-1.4, 1.4)) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270))+ theme(legend.position="none") + ggtitle(paste(DefTown, "defensive\n Shot Chart", sep = " "))
  
  COMP <- ggplot(DFDIF, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = Dif)) + scale_fill_gradient2(low ="blue", high = "red", limits=c(-1.4, 1.4)) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) +  ylim(c(-40, 270))+ theme(legend.position="none") + ggtitle("Comparison\n Shot Chart")
  charts <- arrangeGrob(DEF,OFF, COMP, ncol = 3)
  p <- grid.arrange(arrangeGrob(arrangeGrob(DEF,OFF, COMP, ncol = 3),leg,ncol=1,heights=c(7/8,1/8)))
  
  return(list(Off = DiffOff, deff = DiffDeff, Comparison = Comparison, Total = Totalhex, PPSAA = PPSAA, p = p, leg = leg, charts = charts))
}


Com1 <- ShotComparisonGraph2(OffTeam = "GSW", DefTown = "Cha", SeasondataOff = shotDataTotal2017, SeasonDataDef = shotDatafDef2017, nbins = 30, quant = 0.8, focus = "plus")
Com3 <- ShotComparisonGraph2(OffTeam = "GSW", DefTown = "Cha", SeasondataOff = shotDataTotal2017b, SeasonDataDef = shotDatafDef2017, nbins = 30, quant = 0.8, focus = "plus")


grid.arrange(Com1$charts,Com3$charts,Com1$leg,ncol=1,heights=c(3/7, 3/7 ,1/7))

library(caret)
predict(BRT, data.frame(defAPPS =Com2$PPSAA, offAPPS = Com3$PPSAA), type="raw")
predict(BRT, data.frame(defAPPS =Com3$PPSAA, offAPPS = Com2$PPSAA), type="raw")
