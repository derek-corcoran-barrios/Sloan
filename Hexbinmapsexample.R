library(hexbin)
library(lattice)

#########
#Function to plot
library(plotrix)
hexmap <- function(xcor,ycor,colval, label, title){
  plot(min(c(xcor,ycor)):(max(c(xcor,ycor))+1),min(c(xcor,ycor)):(max(c(xcor,ycor))+1), type="n", frame.plot=F, xaxt="n", yaxt="n", xlab="", ylab="", main = title)
  data <- data.frame(xcor,ycor)
  apply(data, 1, function(zone) hexagon(zone[1],zone[2],col=colval, unitcell=0.95,border="white"))
  text(xcor+ 0.5,ycor + 0.5,labels=label, cex=0.9)
}
#Attacking away team +  - -
dataAtackA <- data.frame(where = c( "30/100", "70/150", "0/1"), x = c(2.5, 2.0, 3.0), y = c(4,3,3), colour = c(3))
hexmap(dataAtackA$x,dataAtackA$y,"firebrick1", dataAtackA$where, NULL)
#Defending home team + - +
dataAtackB <- data.frame(where = c( "25/90", "75/130", "30/78"), x = c(2.5, 2.0, 3.0), y = c(4,3,3), colour = c(1))
hexmap(dataAtackB$x,dataAtackB$y,"dodgerblue1", dataAtackB$where, NULL)

#Average NBA
dataAtackC <- data.frame(where = c( "250/1000", "800/1600", "450/1000"), x = c(2.5, 2.0, 3.0), y = c(4,3,3), colour = c(1))
hexmap(dataAtackC$x,dataAtackC$y,3, dataAtackC$where, NULL)

#####Points per shot


#Attacking away team +  - -
dataAtackA <- data.frame(where = c( "30x3/100 = 0.9", "70x2/150=0.93", "0x2/1=0"), x = c(2.5, 2.0, 3.0), y = c(4,3,3), colour = c(3))
hexmap(dataAtackA$x,dataAtackA$y,"firebrick1", dataAtackA$where, NULL)
#Defending home team + - +
dataAtackB <- data.frame(where = c( "25x3/90 = 0.83", "75x2/130=1.15", "30x2/78=0.77"), x = c(2.5, 2.0, 3.0), y = c(4,3,3), colour = c(1))
hexmap(dataAtackB$x,dataAtackB$y,"dodgerblue1", dataAtackB$where, NULL)

#Average NBA
dataAtackC <- data.frame(where = c( "250x3/1000 = 0.75", "800x2/1600=1", "450x2/1000=0.9"), x = c(2.5, 2.0, 3.0), y = c(4,3,3), colour = c(1))
hexmap(dataAtackC$x,dataAtackC$y,3, dataAtackC$where, NULL)

#######Points over average

#Attacking away team +  - -
dataAtackA <- data.frame(where = c( "0.9-0.75=0.15", "0.93-1=-0.07", "0-0.9=-0.9"), x = c(2.5, 2.0, 3.0), y = c(4,3,3), colour = c(3))
hexmap(dataAtackA$x,dataAtackA$y,"firebrick1", dataAtackA$where, NULL)
#Defending home team + - +
dataAtackB <- data.frame(where = c( "0.83-0.75 \n =0.08", "1.15-0.15=1", "0.77-0.9=-0.13"), x = c(2.5, 2.0, 3.0), y = c(4,3,3), colour = c(1))
hexmap(dataAtackB$x,dataAtackB$y,"dodgerblue1", dataAtackB$where, NULL)


######Avobe Average PPS (AAPPS)

dataAtackA <- data.frame(where = c("0.15+0.08+0.75 \n =0.98", "-0.07+1+1=1.93", "-0.9+-0.13+1=-0.03"), x = c(2.5, 2.0, 3.0), y = c(4,3,3), colour = c(3))
hexmap(dataAtackA$x,dataAtackA$y,"firebrick1", dataAtackA$where, NULL)





hexmap2 <- function(xcor,ycor,colval, label, title){
  plot(min(c(xcor,ycor)):(max(c(xcor,ycor))+1),min(c(xcor,ycor)):(max(c(xcor,ycor))+1), type="n", frame.plot=F, xaxt="n", yaxt="n", xlab="", ylab="", main = title)
  data <- data.frame(xcor,ycor)
  apply(data, 1, function(zone) hexagon(zone[1],zone[2],col=colval, unitcell=0.95,border="white"))
  text(xcor+ 0.5,ycor + 0.5,labels=label, cex=0.7)
  text(3.7, 4.5, "3point")
}

