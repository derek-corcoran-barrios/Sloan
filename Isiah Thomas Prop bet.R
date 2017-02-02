#Isiah Thomas Prop bet

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



shotDatafDef2017 <- readRDS("shotDatafDef2017.rds")
shotDataTotal2017 <- readRDS("shotDataTotal2017.rds")

shotDataTotal2017b <- shotDataTotal2017


shotDataTotal2017b$TEAM_NAME <-ifelse(shotDataTotal2017b$PLAYER_NAME == "Isaiah Thomas", "Isaiah", shotDataTotal2017b$TEAM_NAME)



url <-  "http://www.basketball-reference.com/players/t/thomais02/gamelog/2017/#pgl_basic::none"

Scores <- XML::readHTMLTable(url)

Scores <- Scores$pgl_basic

Scores$PTS <- as.numeric(as.character(Scores$PTS))

Scores$Opp <- gsub("DET", "Det", Scores$Opp)
Scores$Opp <- gsub("ATL", "Atl", Scores$Opp)
Scores$Opp <- gsub("CHI", "Chi", Scores$Opp)
Scores$Opp <- gsub("Boston Celtics", "Bos", Scores$Opp)
Scores$Opp <- gsub("CLE", "Cle", Scores$Opp)
Scores$Opp <- gsub("NOP", "NO", Scores$Opp)
Scores$Opp <- gsub("GSW", "GSW", Scores$Opp)
Scores$Opp <- gsub("ORL", "ORL", Scores$Opp)
Scores$Opp <- gsub("WAS", "Was", Scores$Opp)
Scores$Opp <- gsub("PHI", "Phi", Scores$Opp)
Scores$Opp <- gsub("BRK", "Bkn", Scores$Opp)
Scores$Opp <- gsub("UTA", "Uta", Scores$Opp)
Scores$Opp <- gsub("MIA", "Mia", Scores$Opp)
Scores$Opp <- gsub("CHO", "Cha", Scores$Opp)
Scores$Opp <- gsub("TOR", "Tor", Scores$Opp)
Scores$Opp <- gsub("IND", "Ind", Scores$Opp)
Scores$Opp <- gsub("HOU", "Hou", Scores$Opp)
Scores$Opp <- gsub("DEN", "Den", Scores$Opp)
Scores$Opp <- gsub("MEM", "Mem", Scores$Opp)
Scores$Opp <- gsub("NYK", "NY", Scores$Opp)
Scores$Opp <- gsub("MIL", "Mil", Scores$Opp)
Scores$Opp <- gsub("OKC", "Okc", Scores$Opp)
Scores$Opp <- gsub("SAS", "Sas", Scores$Opp)
Scores$Opp <- gsub("DAL", "Dal", Scores$Opp)
Scores$Opp <- gsub("Phoenix Suns", "Pho", Scores$Opp)
Scores$Opp <- gsub("POR", "Por", Scores$Opp)
Scores$Opp <- gsub("Los Angeles Clippers", "Lac", Scores$Opp)
Scores$Opp <- gsub("SAC", "Sac", Scores$Opp)
Scores$Opp <- gsub("Los Angeles Lakers", "Lal", Scores$Opp)
Scores$Opp <- gsub("MIN", "Min", Scores$Opp)

Scores <- cbind(Scores$Opp, Scores$PTS)
colnames(Scores) <- c("Opp", "PTS")

Scores <- Scores[complete.cases(Scores),]

Scores <- dplyr::arrange(as.data.frame(Scores), desc(PTS))


Scores$offAPPS <- NA

offAPPS <- list()

for(i in 1:NROW(Scores)) {
 offAPPS[[i]] <- ComparisonPPS(OffTeam = "Isaiah", DefTown = Scores$Opp[i], SeasondataOff = shotDataTotal2017b, SeasonDataDef = shotDatafDef2017)
}

Scores$offAPPS <- unlist(offAPPS)
Scores$PTS <- as.numeric(as.character(Scores$PTS))

ggplot2::ggplot(Scores, aes(x = offAPPS, y = PTS)) + geom_point() + geom_smooth(method = "lm")


summary(lm(PTS~offAPPS, data = Scores))

IT <- lm(PTS~offAPPS, data = Scores)
predict(IT, data.frame(offAPPS = c(-0.1559862)), interval = "prediction")

hist(Scores$offAPPS)
summary(Scores$offAPPS*100)


###########################################plots

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
  OFFLEG <- ggplot(DFOFF, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(low ="blue", high = "red", limits=c(-2.2, 2.2)) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270))+ theme(legend.position="bottom") +  ggtitle(paste(OffTeam, "Offensive\n Shot Chart", sep = " "))
  leg<-g_legend(OFFLEG)
  
  OFF <- ggplot(DFOFF, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(low ="blue", high = "red", limits=c(-2.2, 2.2)) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270))+ theme(legend.position="none") +  ggtitle(paste(OffTeam, "Offensive\n Shot Chart", sep = " "))
  DEF <- ggplot(DFDEF, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS))+ scale_fill_gradient2(low ="blue", high = "red", limits=c(-2.2, 2.2)) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270))+ theme(legend.position="none") + ggtitle(paste(DefTown, "defensive\n Shot Chart", sep = " "))
  
  COMP <- ggplot(DFDIF, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = Dif)) + scale_fill_gradient2(low ="blue", high = "red", limits=c(-2.2, 2.2)) +
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

court <- readRDS("Court.rds")

Com1 <- ShotComparisonGraph2(OffTeam = "Isaiah", DefTown = "Lac", SeasondataOff = shotDataTotal2017b, SeasonDataDef = shotDatafDef2017, nbins = 30, quant = 0.75, focus = "plus")

grid.arrange(Com1$charts, ncol=1)


summary(Com1$Comparison$Diff)

Top <- head(Scores$Opp, 4)

Bottom <- tail(Scores$Opp, 4)

TopGraphs <- list()

for (i in 1:length(Top)) {
  TopGraphs[[i]] <- ShotComparisonGraph2(OffTeam = "Isaiah", DefTown = Top[i], SeasondataOff = shotDataTotal2017b, SeasonDataDef = shotDatafDef2017, nbins = 30, quant = 0.75, focus = "plus")
}

grid.arrange(TopGraphs[[1]]$charts, TopGraphs[[2]]$charts, TopGraphs[[3]]$charts, TopGraphs[[4]]$charts, ncol=1)

BottomGraphs <- list()

for (i in 1:length(Top)) {
  BottomGraphs[[i]] <- ShotComparisonGraph2(OffTeam = "Isaiah", DefTown = Bottom[i], SeasondataOff = shotDataTotal2017b, SeasonDataDef = shotDatafDef2017, nbins = 30, quant = 0.75, focus = "plus")
}

grid.arrange(BottomGraphs[[1]]$charts, BottomGraphs[[2]]$charts, BottomGraphs[[3]]$charts, BottomGraphs[[4]]$charts, ncol=1)



######
#######RUSSEL AGAINST PORTLAND

shotDataTotal2017c <- shotDataTotal2017


shotDataTotal2017c$TEAM_NAME <-ifelse(shotDataTotal2017c$PLAYER_NAME == "Russell Westbrook", "Westbrook", shotDataTotal2017c$TEAM_NAME)



url <-  "http://www.basketball-reference.com/players/w/westbru01/gamelog/2017/#pgl_basic::none"

Scores <- XML::readHTMLTable(url)

Scores <- Scores$pgl_basic

Scores$PTS <- as.numeric(as.character(Scores$PTS))

Scores$Opp <- gsub("DET", "Det", Scores$Opp)
Scores$Opp <- gsub("ATL", "Atl", Scores$Opp)
Scores$Opp <- gsub("CHI", "Chi", Scores$Opp)
Scores$Opp <- gsub("BOS", "Bos", Scores$Opp)
Scores$Opp <- gsub("CLE", "Cle", Scores$Opp)
Scores$Opp <- gsub("NOP", "NO", Scores$Opp)
Scores$Opp <- gsub("GSW", "GSW", Scores$Opp)
Scores$Opp <- gsub("ORL", "ORL", Scores$Opp)
Scores$Opp <- gsub("WAS", "Was", Scores$Opp)
Scores$Opp <- gsub("PHI", "Phi", Scores$Opp)
Scores$Opp <- gsub("BRK", "Bkn", Scores$Opp)
Scores$Opp <- gsub("UTA", "Uta", Scores$Opp)
Scores$Opp <- gsub("MIA", "Mia", Scores$Opp)
Scores$Opp <- gsub("CHO", "Cha", Scores$Opp)
Scores$Opp <- gsub("TOR", "Tor", Scores$Opp)
Scores$Opp <- gsub("IND", "Ind", Scores$Opp)
Scores$Opp <- gsub("HOU", "Hou", Scores$Opp)
Scores$Opp <- gsub("DEN", "Den", Scores$Opp)
Scores$Opp <- gsub("MEM", "Mem", Scores$Opp)
Scores$Opp <- gsub("NYK", "NY", Scores$Opp)
Scores$Opp <- gsub("MIL", "Mil", Scores$Opp)
Scores$Opp <- gsub("OKC", "Okc", Scores$Opp)
Scores$Opp <- gsub("SAS", "Sas", Scores$Opp)
Scores$Opp <- gsub("DAL", "Dal", Scores$Opp)
Scores$Opp <- gsub("PHO", "Pho", Scores$Opp)
Scores$Opp <- gsub("POR", "Por", Scores$Opp)
Scores$Opp <- gsub("LAC", "Lac", Scores$Opp)
Scores$Opp <- gsub("SAC", "Sac", Scores$Opp)
Scores$Opp <- gsub("LAL", "Lal", Scores$Opp)
Scores$Opp <- gsub("MIN", "Min", Scores$Opp)

Scores <- cbind(Scores$Opp, Scores$PTS)
colnames(Scores) <- c("Opp", "PTS")

Scores <- Scores[complete.cases(Scores),]

Scores <- dplyr::arrange(as.data.frame(Scores), desc(PTS))


Scores$offAPPS <- NA

offAPPS <- list()

for(i in 1:NROW(Scores)) {
  offAPPS[[i]] <- ComparisonPPS(OffTeam = "Westbrook", DefTown = Scores$Opp[i], SeasondataOff = shotDataTotal2017c, SeasonDataDef = shotDatafDef2017)
}

Scores$offAPPS <- unlist(offAPPS)
Scores$PTS <- as.numeric(as.character(Scores$PTS))

ggplot2::ggplot(Scores, aes(x = offAPPS, y = PTS))  + geom_smooth(method = "lm")


summary(lm(PTS~offAPPS, data = Scores))
Russell <- lm(PTS~offAPPS, data = Scores)
predict(Russell, data.frame(offAPPS = c(-0.1456983)))
hist(Scores$offAPPS)
summary(Scores$offAPPS*100)


###########################################plots



Com1 <- ShotComparisonGraph2(OffTeam = "Westbrook", DefTown = "Por", SeasondataOff = shotDataTotal2017c, SeasonDataDef = shotDatafDef2017, nbins = 30, quant = 0.75, focus = "plus")

grid.arrange(Com1$charts, ncol=1)


summary(Com1$Comparison$Diff)

Top <- head(Scores$Opp, 4)

Bottom <- tail(Scores$Opp, 4)

TopGraphs <- list()

for (i in 1:length(Top)) {
  TopGraphs[[i]] <- ShotComparisonGraph2(OffTeam = "Westbrook", DefTown = Top[i], SeasondataOff = shotDataTotal2017c, SeasonDataDef = shotDatafDef2017, nbins = 30, quant = 0.75, focus = "plus")
}

grid.arrange(TopGraphs[[1]]$charts, TopGraphs[[2]]$charts, TopGraphs[[3]]$charts, TopGraphs[[4]]$charts, ncol=1)

BottomGraphs <- list()

for (i in 1:length(Top)) {
  BottomGraphs[[i]] <- ShotComparisonGraph2(OffTeam = "Westbrook", DefTown = Bottom[i], SeasondataOff = shotDataTotal2017c, SeasonDataDef = shotDatafDef2017, nbins = 30, quant = 0.75, focus = "plus")
}

grid.arrange(BottomGraphs[[1]]$charts, BottomGraphs[[2]]$charts, BottomGraphs[[3]]$charts, BottomGraphs[[4]]$charts, ncol=1)


##########################
##########################
############################
#############################
#############################

#Test Steph

######
#######RUSSEL AGAINST PORTLAND

shotDataTotal2017d <- shotDataTotal2017


shotDataTotal2017d$TEAM_NAME <-ifelse(shotDataTotal2017d$PLAYER_NAME == "Stephen Curry", "Curry", shotDataTotal2017d$TEAM_NAME)



url <-  "http://www.basketball-reference.com/players/c/curryst01/gamelog/2017/#pgl_basic::none"

Scores <- XML::readHTMLTable(url)

Scores <- Scores$pgl_basic

Scores$PTS <- as.numeric(as.character(Scores$PTS))

Scores$Opp <- gsub("DET", "Det", Scores$Opp)
Scores$Opp <- gsub("ATL", "Atl", Scores$Opp)
Scores$Opp <- gsub("CHI", "Chi", Scores$Opp)
Scores$Opp <- gsub("BOS", "Bos", Scores$Opp)
Scores$Opp <- gsub("CLE", "Cle", Scores$Opp)
Scores$Opp <- gsub("NOP", "NO", Scores$Opp)
Scores$Opp <- gsub("GSW", "GSW", Scores$Opp)
Scores$Opp <- gsub("ORL", "ORL", Scores$Opp)
Scores$Opp <- gsub("WAS", "Was", Scores$Opp)
Scores$Opp <- gsub("PHI", "Phi", Scores$Opp)
Scores$Opp <- gsub("BRK", "Bkn", Scores$Opp)
Scores$Opp <- gsub("UTA", "Uta", Scores$Opp)
Scores$Opp <- gsub("MIA", "Mia", Scores$Opp)
Scores$Opp <- gsub("CHO", "Cha", Scores$Opp)
Scores$Opp <- gsub("TOR", "Tor", Scores$Opp)
Scores$Opp <- gsub("IND", "Ind", Scores$Opp)
Scores$Opp <- gsub("HOU", "Hou", Scores$Opp)
Scores$Opp <- gsub("DEN", "Den", Scores$Opp)
Scores$Opp <- gsub("MEM", "Mem", Scores$Opp)
Scores$Opp <- gsub("NYK", "NY", Scores$Opp)
Scores$Opp <- gsub("MIL", "Mil", Scores$Opp)
Scores$Opp <- gsub("OKC", "Okc", Scores$Opp)
Scores$Opp <- gsub("SAS", "Sas", Scores$Opp)
Scores$Opp <- gsub("DAL", "Dal", Scores$Opp)
Scores$Opp <- gsub("PHO", "Pho", Scores$Opp)
Scores$Opp <- gsub("POR", "Por", Scores$Opp)
Scores$Opp <- gsub("LAC", "Lac", Scores$Opp)
Scores$Opp <- gsub("SAC", "Sac", Scores$Opp)
Scores$Opp <- gsub("LAL", "Lal", Scores$Opp)
Scores$Opp <- gsub("MIN", "Min", Scores$Opp)

Scores <- cbind(Scores$Opp, Scores$PTS)
colnames(Scores) <- c("Opp", "PTS")

Scores <- Scores[complete.cases(Scores),]

Scores <- dplyr::arrange(as.data.frame(Scores), desc(PTS))


Scores$offAPPS <- NA

offAPPS <- list()

for(i in 1:NROW(Scores)) {
  offAPPS[[i]] <- ComparisonPPS(OffTeam = "Curry", DefTown = Scores$Opp[i], SeasondataOff = shotDataTotal2017d, SeasonDataDef = shotDatafDef2017)
}

Scores$offAPPS <- unlist(offAPPS)
Scores$PTS <- as.numeric(as.character(Scores$PTS))

ggplot2::ggplot(Scores, aes(x = offAPPS, y = PTS))  + geom_smooth(method = "lm")


summary(lm(PTS~offAPPS, data = Scores))
Russell <- lm(PTS~offAPPS, data = Scores)
predict(Russell, data.frame(offAPPS = c(-0.1456983)))
hist(Scores$offAPPS)
summary(Scores$offAPPS*100)


###########################################plots



Com1 <- ShotComparisonGraph2(OffTeam = "Westbrook", DefTown = "Por", SeasondataOff = shotDataTotal2017c, SeasonDataDef = shotDatafDef2017, nbins = 30, quant = 0.75, focus = "plus")

grid.arrange(Com1$charts, ncol=1)


summary(Com1$Comparison$Diff)

Top <- head(Scores$Opp, 4)

Bottom <- tail(Scores$Opp, 4)

TopGraphs <- list()

for (i in 1:length(Top)) {
  TopGraphs[[i]] <- ShotComparisonGraph2(OffTeam = "Westbrook", DefTown = Top[i], SeasondataOff = shotDataTotal2017c, SeasonDataDef = shotDatafDef2017, nbins = 30, quant = 0.75, focus = "plus")
}

grid.arrange(TopGraphs[[1]]$charts, TopGraphs[[2]]$charts, TopGraphs[[3]]$charts, TopGraphs[[4]]$charts, ncol=1)

BottomGraphs <- list()

for (i in 1:length(Top)) {
  BottomGraphs[[i]] <- ShotComparisonGraph2(OffTeam = "Westbrook", DefTown = Bottom[i], SeasondataOff = shotDataTotal2017c, SeasonDataDef = shotDatafDef2017, nbins = 30, quant = 0.75, focus = "plus")
}

grid.arrange(BottomGraphs[[1]]$charts, BottomGraphs[[2]]$charts, BottomGraphs[[3]]$charts, BottomGraphs[[4]]$charts, ncol=1)


