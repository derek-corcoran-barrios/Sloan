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

ShotComparison <- function(OffTeam, DefTown, SeasondataOff, SeasonDataDef, nbins = 40) {
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
  
  OFF <- ggplot(DiffOff)  + 
    annotation_custom(court, -250, 250, -52, 418) +
    geom_hex(aes(x = x, y = y, fill = PPS),
             stat = "identity", alpha = 0.8) +
    guides(alpha = FALSE, size = FALSE) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ggtitle(paste(OffTeam, "Offensive\n Shot Chart", sep = " ")) + scale_fill_gradient2(name="Off PPS")
  DEF <- ggplot(DiffDeff)  + 
    annotation_custom(court, -250, 250, -52, 418) +
    geom_hex(aes(x = x, y = y, fill = PPS),
             stat = "identity", alpha = 0.8) +
    guides(alpha = FALSE, size = FALSE) +
    
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ggtitle(paste(DefTown, "defensive\n Shot Chart", sep = " ")) + scale_fill_gradient2(name="Def PPS")
  
  COMP <- ggplot(Comparison)  + 
    annotation_custom(court, -250, 250, -52, 418) +
    geom_hex(aes(x = x.x, y = y.x, fill = Diff),
             stat = "identity", alpha = 0.8) +
    guides(alpha = FALSE, size = FALSE) +
    
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ggtitle("Comparison\n Shot Chart") + scale_fill_gradient2(name="Difference\n PPS")

  grid.arrange(DEF, OFF, COMP, ncol=3)
  
  return(list(Off = DiffOff, deff = DiffDeff, Comparison = Comparison, Total = Totalhex, PPSAA = PPSAA))
}

str(ShotComparison(OffTeam = "Boston Celtics", DefTown = "San Antonio", SeasondataOff = shotDataTotal2016, SeasonDataDef = shotDatafDef2016, nbins = 40))


Offensive_teams <- as.character(unique(shotDataTotal2016$TEAM_NAME))

defenseve_names <- names(shotDatafDef2016)

df <- data.frame(matrix(ncol = 30, nrow = 30))
colnames(df) <- as.character(unique(shotDataTotal2016$TEAM_NAME))
rownames(df) <- names(shotDatafDef2016)

for (i in 1:3) {
  for (j in 1:3){
    df[rownames(df) == defenseve_names[j],colnames(df) == Offensive_teams[i]] <- ShotComparison(OffTeam = Offensive_team, DefTown = defenseve_names[j], SeasondataOff = shotDataTotal2016, SeasonDataDef = shotDatafDef2016, nbins = 30)
  }
}


###Example

Cols <- c(1:3)
Rows <- c(4:6)
DF <- data.frame(matrix(nrow = 3, ncol = 3))
colnames(DF) <- Cols
rownames(DF) <- Rows

a <- 1
b <- 2
c <- 3
d <- 4
e <- 5
f <- 6

FX <- function(x , y){
  S <- (x^2)+ (y^3)
  return(S)
}

for (i in 1:length(Cols)){
  for(j in 1:length(Rows)){
    DF[i, j] <- FX(Rows[i], Cols[j])
  }
}



