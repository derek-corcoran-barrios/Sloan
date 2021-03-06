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

court <- readRDS("court.rds")
shotDatafDef2016 <- readRDS("shotDatafDef2016.rds")
shotDataTotal2016 <- readRDS("shotDataTotal2016.rds")
shotDatafDef2013 <- readRDS("shotDatafDef2013.rds")
shotDataTotal2013 <- readRDS("shotDataTotal2013.rds")

ShotComparisonGraph <- function(OffTeam, DefTown, SeasondataOff, SeasonDataDef, nbins = 30, maxsize = 7, quant = 0.7) {
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
  
  OFFLEG <- ggplot(DiffOff) + annotation_custom(court, -250, 250, -52, 418) + geom_point(aes(x = x, y = y, color = PPS, size = ST), stat = "identity") +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) +scale_color_gradient2(low = "blue", high = "red", name = "APPS") + ylim(c(-51, 400))+ theme(legend.position="bottom") +  ggtitle(paste(OffTeam, "Offensive\n Shot Chart", sep = " "))
  leg<-g_legend(OFFLEG)
  
  DiffOff <- filter(DiffOff, ST > quantile(DiffOff$ST, probs = quant))
  DiffDeff <- filter(DiffDeff, ST > quantile(DiffDeff$ST, probs = quant))
  Comparison <- filter(Comparison, ST.x > quantile(Comparison$ST.x, probs = quant))
  
  
  
  OFF <- ggplot(DiffOff) + annotation_custom(court, -250, 250, -52, 418) + geom_point(aes(x = x, y = y, color = PPS, size = ST), stat = "identity") +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + scale_color_gradient2(low = "blue", high = "red", name = "APPS") + ylim(c(-51, 400))+ theme(legend.position="none") +  ggtitle(paste(OffTeam, "Offensive\n Shot Chart", sep = " "))
  DEF <- ggplot(DiffDeff)  + annotation_custom(court, -250, 250, -52, 418) + geom_point(aes(x = x, y = y, color = PPS, size = ST), stat = "identity") +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + scale_color_gradient2(low = "blue", high = "red", name = "APPS") + ylim(c(-51, 400))+ theme(legend.position="none") + ggtitle(paste(DefTown, "defensive\n Shot Chart", sep = " "))
  
  COMP <- ggplot(Comparison) + annotation_custom(court, -250, 250, -52, 418) + geom_point(aes(x = x.x, y = y.x, color = Diff, size = ST.x), stat = "identity") +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + scale_color_gradient2(low = "blue", high = "red", name = "APPS")+ ylim(c(-51, 400))+ theme(legend.position="none") + ggtitle("Comparison\n Shot Chart")
  charts <- arrangeGrob(DEF,OFF, COMP, ncol = 3)
  p <- grid.arrange(arrangeGrob(arrangeGrob(DEF,OFF, COMP, ncol = 3),leg,ncol=1,heights=c(7/8,1/8)))
  
  return(list(Off = DiffOff, deff = DiffDeff, Comparison = Comparison, Total = Totalhex, PPSAA = PPSAA, p = p, leg = leg, charts = charts))
}

Com1 <- ShotComparisonGraph(OffTeam = "Golden State Warriors", DefTown = "Cleveland", SeasondataOff = shotDataTotal2016, SeasonDataDef = shotDatafDef2016, nbins = 30, quant = 0.7)
Com2 <- ShotComparisonGraph(OffTeam = "Cleveland Cavaliers", DefTown = "Golden State", SeasondataOff = shotDataTotal2016, SeasonDataDef = shotDatafDef2016, nbins = 30, quant = 0.7)

grid.arrange(Com1$charts,Com2$charts,Com1$leg,ncol=1,heights=c(3/7, 3/7 ,1/7))

