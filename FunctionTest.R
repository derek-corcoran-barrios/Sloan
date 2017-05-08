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


#shot Comparison function
shotDataTotal2016 <- readRDS("shotDataTotal2016.rds")

shotDataTotal2017 <- readRDS("shotDataTotal2017.rds")
court <- readRDS("court.rds")

#####ShotChart for season by PPS or PCT


ShotSeasonGraph <- function(SeasondataOff, nbins = 30, maxsize = 7, quant = 0.7, type = "PPS", MAX_Y = 270) {
  SeasondataOff <- dplyr::filter(SeasondataOff, LOC_Y < MAX_Y)
  #Get the maximum and minumum values for x and y
  xbnds <- range(SeasondataOff$LOC_X)
  ybnds <- range(SeasondataOff$LOC_Y)
  #Make hexbin dataframes out of the teams
  
  if (type == "PPS"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }
  
  if (type == "PCT"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG)), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }
  
  
  ##Total NBA data
  Totalhex <- makeHexData(SeasondataOff)
  Totalhex <- filter(Totalhex, ST > quantile(Totalhex$ST, probs = quant))
  Totalhex$ST <- ifelse(Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2], 0.06,
                        ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] , 0.12 ,
                               ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] , 0.25 ,        
                                      ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[5] , 0.5 ,
                                             1))))
  
  
  #Function to transform hexbins into polygons
  hex_coord_df <- function(x, y, width, height, size = 1) {
    # like hex_coord but returns a dataframe of vertices grouped by an id variable
    dx <- size * width / 6
    dy <- size * height / 2 / sqrt(3)
    
    hex_y <- rbind(y - 2 * dy, y - dy, y + dy, y + 2 * dy, y + dy, y - dy)
    hex_x <- rbind(x, x + dx, x + dx, x, x - dx, x - dx)
    id    <- rep(1:length(x), each=6)
    
    data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
  }
  
  #Transform Hexbins into polygons
  
  Total <- hex_coord_df(Totalhex$x, Totalhex$y, 30*Totalhex$ST, 10*Totalhex$ST, size =1)
  Total$PPS <- rep(Totalhex$PPS, each = 6)
  
  #Make Graph
  if(type == "PPS"){
  GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = 1, low = "blue", high = "red", limits=c(0, 3)) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270))+ theme(legend.position="bottom")
  }else{
    GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = 0.5, low = "blue", high = "red", limits=c(0, 1)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            legend.title = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270)) + xlim(c(-250, 250))+ theme(legend.position="bottom")
  }
  if(type == "PPS"){
    GRAPH <- GRAPH +  ggtitle("Points per Shot")
  }  else {GRAPH <- GRAPH +  ggtitle("Shooting percentage")}
  
  
  return(GRAPH)
}


ShotSeasonGraph(shotDataTotal2017, quant = 0.4, MAX_Y = 270)
ShotSeasonGraph(shotDataTotal2017, quant = 0.4, MAX_Y = 270, type = "PCT")

#library(animation)

#seasons <- list(shotDataTotal2013, shotDataTotal2014, shotDataTotal2015, shotDataTotal2016, shotDataTotal2017)

#saveGIF(for(i in 1:length(seasons)){ShotSeasonGraph(seasons[[i]], quant = 0.4, MAX_Y = 270)}, movie.name = "NBAseasons.gif", img.name = "Rplot", convert = "convert", clean = TRUE)



#####Shot charts by team, PCT or PPS

OffShotSeasonGraphTeam <- function(SeasondataOff, team, nbins = 30, maxsize = 7, quant = 0.7, type = "PPS", MAX_Y = 270) {
    SeasondataOff <- dplyr::filter(SeasondataOff, LOC_Y < MAX_Y)
    SeasondataOff <- dplyr::filter(SeasondataOff, TEAM_NAME == team)
    #Get the maximum and minumum values for x and y
    xbnds <- range(SeasondataOff$LOC_X)
    ybnds <- range(SeasondataOff$LOC_Y)
    #Make hexbin dataframes out of the teams
    
    if (type == "PPS"){
      makeHexData <- function(df) {
        h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
        data.frame(hcell2xy(h),
                   PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
                   ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                   cid = h@cell)
      }
    }
    
    if (type == "PCT"){
      makeHexData <- function(df) {
        h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
        data.frame(hcell2xy(h),
                   PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG)), h@cID, FUN = function(z) sum(z)/length(z)),
                   ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                   cid = h@cell)
      }
    }
    
    
    ##Total NBA data
    Totalhex <- makeHexData(SeasondataOff)
    Totalhex <- filter(Totalhex, ST > quantile(Totalhex$ST, probs = quant))
    Totalhex$ST <- ifelse(Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2], 0.06,
                          ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] , 0.12 ,
                                 ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] , 0.25 ,        
                                        ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[5] , 0.5 ,
                                               1))))
    
    
    #Function to transform hexbins into polygons
    hex_coord_df <- function(x, y, width, height, size = 1) {
      # like hex_coord but returns a dataframe of vertices grouped by an id variable
      dx <- size * width / 6
      dy <- size * height / 2 / sqrt(3)
      
      hex_y <- rbind(y - 2 * dy, y - dy, y + dy, y + 2 * dy, y + dy, y - dy)
      hex_x <- rbind(x, x + dx, x + dx, x, x - dx, x - dx)
      id    <- rep(1:length(x), each=6)
      
      data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
    }
    
    #Transform Hexbins into polygons
    
    Total <- hex_coord_df(Totalhex$x, Totalhex$y, 30*Totalhex$ST, 10*Totalhex$ST, size =1)
    Total$PPS <- rep(Totalhex$PPS, each = 6)
    
    #Make Graph
    if(type == "PPS"){
    GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = 1, low = "blue", high = "red", limits=c(0, 3)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            legend.title = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270))+ theme(legend.position="bottom")
    }else{
      GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = 0.5, low = "blue", high = "red", limits=c(0, 1)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            legend.title = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270))+ theme(legend.position="bottom")}
    if(type == "PPS"){
      GRAPH <- GRAPH +  ggtitle(paste("Points per Shot of", team, sep =" "))
    }  else {GRAPH <- GRAPH +  ggtitle(paste("Shooting percentage", team, sep =" ")
)}
    
    
    return(GRAPH)
  }


OffShotSeasonGraphTeam(shotDataTotal2017, team = "GSW",quant = 0.4)

OffShotSeasonGraphTeam(shotDataTotal2017, team = "Hou",quant = 0.4)

OffShotSeasonGraphTeam(shotDataTotal2017, team = "ORL",quant = 0.4)

OffShotSeasonGraphTeam(shotDataTotal2017, team = "ORL",quant = 0.4, type = "PCT")

###Season shot chart by player

OffShotSeasonGraphPlayer <- function(SeasondataOff, player, nbins = 30, maxsize = 7, quant = 0.7, type = "PPS", MAX_Y = 270) {
  SeasondataOff <- dplyr::filter(SeasondataOff, LOC_Y < MAX_Y)
  SeasondataOff <- dplyr::filter(SeasondataOff, PLAYER_NAME == player)
  #Get the maximum and minumum values for x and y
  xbnds <- range(SeasondataOff$LOC_X)
  ybnds <- range(SeasondataOff$LOC_Y)
  #Make hexbin dataframes out of the players
  
  if (type == "PPS"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }
  
  if (type == "PCT"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG)), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }
  
  
  ##Total NBA data
  Totalhex <- makeHexData(SeasondataOff)
  Totalhex <- filter(Totalhex, ST > quantile(Totalhex$ST, probs = quant))
  Totalhex$ST <- ifelse(Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2], 0.06,
                        ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] , 0.12 ,
                               ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] , 0.25 ,        
                                      ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[5] , 0.5 ,
                                             1))))
  
  
  #Function to transform hexbins into polygons
  hex_coord_df <- function(x, y, width, height, size = 1) {
    # like hex_coord but returns a dataframe of vertices grouped by an id variable
    dx <- size * width / 6
    dy <- size * height / 2 / sqrt(3)
    
    hex_y <- rbind(y - 2 * dy, y - dy, y + dy, y + 2 * dy, y + dy, y - dy)
    hex_x <- rbind(x, x + dx, x + dx, x, x - dx, x - dx)
    id    <- rep(1:length(x), each=6)
    
    data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
  }
  
  #Transform Hexbins into polygons
  
  Total <- hex_coord_df(Totalhex$x, Totalhex$y, 30*Totalhex$ST, 10*Totalhex$ST, size =1)
  Total$PPS <- rep(Totalhex$PPS, each = 6)
  
  #Make Graph
  if(type == "PPS"){
    GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = 1, low = "blue", high = "red", limits=c(0, 3)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            legend.title = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270))+ xlim(c(-250, 250)) + theme(legend.position="bottom")
  }else{
    GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = 0.5, low = "blue", high = "red", limits=c(0, 1)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            legend.title = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270))+ xlim(c(-250, 250)) + theme(legend.position="bottom")
  }
  if(type == "PPS"){
    GRAPH <- GRAPH +  ggtitle(paste("Points per Shot of", player, sep =" "))
  }  else {GRAPH <- GRAPH +  ggtitle(paste("Shooting percentage", player, sep =" ")
  )}
  
  
  return(GRAPH)
}


OffShotSeasonGraphPlayer(shotDataTotal2017, player = "Stephen Curry",quant = 0.4) + theme_void
OffShotSeasonGraphPlayer(shotDataTotal2017, player = "DeAndre Jordan",quant = 0.4)
OffShotSeasonGraphPlayer(shotDataTotal2017, player = "DeMar DeRozan",quant = 0.4)
OffShotSeasonGraphPlayer(shotDataTotal2017, player = "Isaiah Thomas",quant = 0.4)

OffShotSeasonGraphPlayer(shotDataTotal2017, player = "Stephen Curry",quant = 0.4, type = "PCT")
OffShotSeasonGraphPlayer(shotDataTotal2017, player = "DeAndre Jordan",quant = 0.4, type = "PCT")


##Shot points for whole season

PointShotSeasonGraph <- function(SeasondataOff) {
  SeasondataOff$SHOT_MADE_FLAG <- ifelse(SeasondataOff$SHOT_MADE_FLAG == "1", "Made", "Missed")
  SeasondataOff$SHOT_MADE_FLAG <- as.factor(SeasondataOff$SHOT_MADE_FLAG)
  #Make Graph
  GRAPH <- ggplot(SeasondataOff, aes(x=LOC_X, y = LOC_Y))+ annotation_custom(court, -250, 250, -52, 418) + geom_point(aes(color = SHOT_MADE_FLAG), alpha = 0.2) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270))+ theme(legend.position="bottom")
  return(GRAPH)
}

PointShotSeasonGraph(shotDataTotal2017)


##Point shots by Team

PointShotSeasonGraphTeam <- function(SeasondataOff, offteam) {
  SeasondataOff <- dplyr::filter(SeasondataOff, TEAM_NAME == offteam)
  SeasondataOff$SHOT_MADE_FLAG <- ifelse(SeasondataOff$SHOT_MADE_FLAG == "1", "Made", "Missed")
  SeasondataOff$SHOT_MADE_FLAG <- as.factor(SeasondataOff$SHOT_MADE_FLAG)
  #Make Graph
  GRAPH <- ggplot(SeasondataOff, aes(x=LOC_X, y = LOC_Y))+ annotation_custom(court, -250, 250, -52, 418) + geom_point(aes(color = SHOT_MADE_FLAG), alpha = 0.2) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270))+ theme(legend.position="bottom") +  ggtitle(offteam)
  return(GRAPH)
}


Hou <- PointShotSeasonGraphTeam(shotDataTotal2017, "Hou")
Orl <- PointShotSeasonGraphTeam(shotDataTotal2017, "ORL")

library(gridExtra)

grid.arrange(Hou,Orl, ncol = 2)

#####################################
###########################################HASTA AQUI esta listo

######################################################################################
