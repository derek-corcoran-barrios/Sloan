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
  GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = median(Total$PPS), low = "blue", high = "red") +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270))+ theme(legend.position="bottom")
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




###########################################HASTA AQUI esta listo








SeasondataOff <- shotDataTotal2017

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
  
  #Transform Hexbins into polygons
  Totalhex <- filter(Totalhex, ST > quantile(Totalhex$ST, probs = quant))
  
  Total <- hex_coord_df(Totalhex$x, Totalhex$y, (0.05*Totalhex$ST), (0.05*Totalhex$ST), size =1)
  Total$PPS <- rep(Totalhex$PPS, each = 6)
  
  #Make Graph
  GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = median(Total$PPS), low = "blue", high = "red") +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270))+ theme(legend.position="bottom")
if(type == "PPS"){
  GRAPH <- GRAPH +  ggtitle("Points per Shot")
}  else {GRAPH <- GRAPH +  ggtitle("Shooting percentage")}

  
  return(GRAPH)
}

max_x750 <- ShotSeasonGraph(shotDataTotal2017, quant = 0.75, MAX_Y = 750)
max_x500 <- ShotSeasonGraph(shotDataTotal2017, quant = 0.75, MAX_Y = 500)
max_x270 <- ShotSeasonGraph(shotDataTotal2017, quant = 0.75, MAX_Y = 270)
ShotSeasonGraph(shotDataTotal2017, quant = 0.4, MAX_Y = 270)
library(gridExtra)

grid.arrange(max_x750,max_x500, max_x270, ncol = 3)


OffShotSeasonGraphTeam <- function(SeasondataOff, team, nbins = 30, maxsize = 7, quant = 0.7, type = "PPS") {
  
  dataOff <- dplyr::filter(SeasondataOff, TEAM_NAME == team)
  
  #Get the maximum and minumum values for x and y
  xbnds <- range(dataOff$LOC_X)
  ybnds <- range(dataOff$LOC_Y)
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
  Totalhex <- makeHexData(dataOff)
  
  
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
  
  #Transform Hexbins into polygons
  Totalhex <- filter(Totalhex, ST > quantile(Totalhex$ST, probs = quant))
  
  Total <- hex_coord_df(Totalhex$x, Totalhex$y, (0.05*Totalhex$ST), (0.05*Totalhex$ST), size =1)
  Total$PPS <- rep(Totalhex$PPS, each = 6)
  
  #Make Graph
  GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = median(1.0), low = "blue", high = "red", limits=c(0.3, 2.1)) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270))+ theme(legend.position="bottom")
  if(type == "PPS"){
    GRAPH <- GRAPH +  ggtitle(paste("Points per Shot of", team, sep =" "))
  }  else {GRAPH <- GRAPH +  ggtitle("Shooting percentage")}
  
  
  return(GRAPH)
}

a <- OffShotSeasonGraphTeam(shotDataTotal2017, team = "GSW",quant = 0.7)

b <- OffShotSeasonGraphTeam(shotDataTotal2017, team = "Hou",quant = 0.7)

C <- OffShotSeasonGraphTeam(shotDataTotal2017, team = "Phi",quant = 0.7)

d <- OffShotSeasonGraphTeam(shotDataTotal2017, team = "ORL",quant = 0.7)

library(gridExtra)

grid.arrange(b,d, ncol = 2)


######################################################################################


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
  
  
  #Function to transform hexbins into polygons
  hex_coord_df <- function(x, y, width, height, size = 1) {
    # like hex_coord but returns a dataframe of vertices grouped by an id variable
    dx <- log(size * width / 6)
    dy <- log(size * height / 2 / sqrt(3))
    
    hex_y <- rbind(y - 2 * dy, y - dy, y + dy, y + 2 * dy, y + dy, y - dy)
    hex_x <- rbind(x, x + dx, x + dx, x, x - dx, x - dx)
    id    <- rep(1:length(x), each=6)
    
    data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
  }
  
  #Transform Hexbins into polygons
  Totalhex <- filter(Totalhex, ST > quantile(Totalhex$ST, probs = quant))
  
  Total <- hex_coord_df(Totalhex$x, Totalhex$y, (0.13*Totalhex$ST), (0.01*Totalhex$ST), size =1)
  Total$PPS <- rep(Totalhex$PPS, each = 6)
  
  #Make Graph
  GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = median(Total$PPS), low = "blue", high = "red") +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270))+ theme(legend.position="bottom")
  if(type == "PPS"){
    GRAPH <- GRAPH +  ggtitle("Points per Shot")
  }  else {GRAPH <- GRAPH +  ggtitle("Shooting percentage")}
  
  
  return(GRAPH)
}


ShotSeasonGraph(shotDataTotal2017, quant = 0.4, MAX_Y = 270)
library(gridExtra)



##################################
##################################

ShotSeasonGraph <- function(SeasondataOff) {
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

ShotSeasonGraph(shotDataTotal2017)


ShotSeasonGraphTeam <- function(SeasondataOff, offteam) {
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


Hou <- ShotSeasonGraphTeam(shotDataTotal2017, "Hou")
Orl <- ShotSeasonGraphTeam(shotDataTotal2017, "ORL")

library(gridExtra)

grid.arrange(Hou,Orl, ncol = 2)

#####################################




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
  GRAPH <- ggplot(Total, aes(x=x, y = y))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes(group = id, fill = PPS)) + scale_fill_gradient2(midpoint = median(Total$PPS), low = "blue", high = "red") +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ scale_size(range = c(0, maxsize)) + ylim(c(-40, 270))+ theme(legend.position="bottom")
  if(type == "PPS"){
    GRAPH <- GRAPH +  ggtitle("Points per Shot")
  }  else {GRAPH <- GRAPH +  ggtitle("Shooting percentage")}
  
  
  return(GRAPH)
}


ShotSeasonGraph(shotDataTotal2017, quant = 0.4, MAX_Y = 270)
