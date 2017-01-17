#Data

#Load packages
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)

##Read Vegas odds and transform the fromats
NBAOdds <- read.csv("~/Sloan/NBAOdds.csv")
NBAOdds$Date <- mdy(NBAOdds$Date)

#Observed data The observed data, that is the away score - the home score, that will result in a negative value if the home team won, compared to a possitive value if the home team wins
NBAOdds$Diff <- (NBAOdds$Score.1 - NBAOdds$Score)

NBAOdds$Diff <- (NBAOdds$Score - NBAOdds$Score.1)

#Year of the result
NBAOdds$Year <- year(NBAOdds$Date)

#Read the spatial average points per shot difference per year, the Column names are the Offensive team and the row is the defensive team, the value in each cell is the value above average that 
#the offensive team would score against the deffensive team, that is, if the value is positive the offensive team would score above NBA average on that team, and if it's negative the opposite is true
datos2016 <- read.csv("~/Sloan/datos2016.csv", row.names=1)
datos2015 <- read.csv("~/Sloan/datos2015.csv", row.names=1)
datos2014 <- read.csv("~/Sloan/datos2014.csv", row.names=1)
datos2013 <- read.csv("~/Sloan/datos2013.csv", row.names=1)
datos2012 <- read.csv("~/Sloan/datos2012.csv", row.names=1)



#Add two columns Aberage points per shot
#Def apps is when the away team is defending 
#Off apps is when the away team is attacking
NBAOdds$defAPPS <- NA
NBAOdds$offAPPS <- NA

datos <- list(datos2012 = datos2012 ,datos2013 = datos2013, datos2014 = datos2014, datos2015 = datos2015, datos2016= datos2016)


for (i in 1:nrow(NBAOdds)) {
  print(i)
  if (NBAOdds$Year[i] == 2016){
  NBAOdds$offAPPS[i]<- datos$datos2016[rownames(datos$datos2016) == NBAOdds$Home[i],colnames(datos$datos2016) == NBAOdds$Away[i]]
  NBAOdds$defAPPS[i] <- datos$datos2016[rownames(datos$datos2016) == NBAOdds$Away[i],colnames(datos$datos2016) == NBAOdds$Home[i]]
  }
  if (NBAOdds$Year[i] == 2015){
    NBAOdds$offAPPS[i]<- datos$datos2015[rownames(datos$datos2015) == NBAOdds$Home[i],colnames(datos$datos2015) == NBAOdds$Away[i]]
    NBAOdds$defAPPS[i] <- datos$datos2015[rownames(datos$datos2015) == NBAOdds$Away[i],colnames(datos$datos2015) == NBAOdds$Home[i]]
  }
  if (NBAOdds$Year[i] == 2014){
    NBAOdds$offAPPS[i]<- datos$datos2014[rownames(datos$datos2014) == NBAOdds$Home[i],colnames(datos$datos2014) == NBAOdds$Away[i]]
    NBAOdds$defAPPS[i] <- datos$datos2014[rownames(datos$datos2014) == NBAOdds$Away[i],colnames(datos$datos2014) == NBAOdds$Home[i]]
  }
  if (NBAOdds$Year[i] == 2013){
    NBAOdds$offAPPS[i]<- datos$datos2013[rownames(datos$datos2013) == NBAOdds$Home[i],colnames(datos$datos2013) == NBAOdds$Away[i]]
    NBAOdds$defAPPS[i] <- datos$datos2013[rownames(datos$datos2013) == NBAOdds$Away[i],colnames(datos$datos2013) == NBAOdds$Home[i]]
  }
  if (NBAOdds$Year[i] == 2012){
    NBAOdds$offAPPS[i]<- datos$datos2012[rownames(datos$datos2012) == NBAOdds$Home[i],colnames(datos$datos2012) == NBAOdds$Away[i]]
    NBAOdds$defAPPS[i] <- datos$datos2012[rownames(datos$datos2012) == NBAOdds$Away[i],colnames(datos$datos2012) == NBAOdds$Home[i]]
  }
}

ggplot(NBAOdds, aes(x = Home.Spread, y = Diff)) + geom_point() + geom_smooth()
ggplot(NBAOdds, aes(x = defAPPS, y = Diff)) + geom_point() + geom_smooth()
ggplot(NBAOdds, aes(x = offAPPS, y = Diff)) + geom_point() + geom_smooth()

summary(lm(Diff ~ offAPPS + defAPPS, data=NBAOdds))
summary(lm(Diff ~ defAPPS, data=NBAOdds))
summary(lm(Diff ~ offAPPS, data=NBAOdds))

FinalOdds <- NBAOdds
write.csv(FinalOdds, "FinalOdds.csv")

####Regression trees

library(dismo)
library(gbm)


#Divide odds into 2016 vs 2012-2015
NBAOdds2012_2015 <- filter(NBAOdds, Year != 2016)
NBAOdds2016 <- filter(NBAOdds, Year == 2016)


#####Forcast

####Caret version
library(caret)
ctrl <- trainControl(method = "repeatedcv", number=10, repeats=3)


grid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                    n.trees = seq(100, 1000, by = 50),
                    shrinkage = c(0.01, 0.1),
                    n.minobsinnode=c(1,5,10))


# train the GBM model
set.seed(7)
modelGbmforcast <- train(x = NBAOdds2012_2015[,14:15],y = NBAOdds2012_2015[,12], method = "gbm",  preProcess = c("center", "scale"), verbose = FALSE, trControl = ctrl, tuneGrid = grid)

gbmcaretforecast <- predict(modelGbmforcast, NBAOdds2016[,14:15])
####

BRT2017_17_Jan <- train(x = NBAOdds[,14:15],y = NBAOdds[,12], method = "gbm",  preProcess = c("center", "scale"), verbose = FALSE, trControl = ctrl, tuneGrid = grid)
saveRDS(BRT2017_17_Jan, "BRT2017_17_Jan.rds")

#Add BRT fit to model

#Add caret fit to model
NBAOdds2016$GBMfit <- gbmcaretforecast


#Plot predictions
predict_comparison <- melt(NBAOdds2016, id.vars = "Diff", measure.vars = c("Home.Spread", "GBMfit"))
predict_comparison$variable <- ifelse(predict_comparison$variable == "Home.Spread", "Vegas odds","PPS odds")
ggplot(predict_comparison, aes(x = Diff, y = value)) + geom_point(aes(color = variable)) + geom_smooth(method = lm, aes(fill=variable, color = variable)) + xlab("Observed spread") + ylab("Predicted spread") + theme(legend.position = "bottom")


#get The predicted

postResample(pred = NBAOdds2016$GBMfit, obs = NBAOdds2016$Diff)
postResample(pred = NBAOdds2016$Home.Spread, obs = NBAOdds2016$Diff)

####Surface plot


For.predictions <- expand.grid(defAPPS = seq(from = min(NBAOdds$defAPPS), to = max(NBAOdds$defAPPS), length.out = 100), 
                    offAPPS =seq(from= min(NBAOdds$offAPPS),to = max(NBAOdds$offAPPS), length.out = 100))

For.predictions$Spread <- predict(modelGbmforcast, For.predictions)

wireframe(Spread ~  offAPPS + defAPPS, data = For.predictions, colorkey = TRUE, drape = TRUE, pretty = TRUE,scales = list(arrows = FALSE), screen = list(z = -220, x = -80))



#####################################################################################################################################################################################################
########################Pace Adjusted######################################################
########################################################################################


pace2012 <- readRDS("pacetables2012.rds")
pace2013 <- readRDS("pacetables2013.rds")
pace2014 <- readRDS("pacetables2014.rds")
pace2015 <- readRDS("pacetables2015.rds")
pace2016 <- readRDS("pacetables2016.rds")


Pace <- list(pace2012 = pace2012 ,pace2013 = pace2013, pace2014 = pace2014, pace2015 = pace2015, pace2015= pace2015, pace2016 = pace2016)



for (i in 1:nrow(NBAOdds)) {
  print(i)
  if (NBAOdds$Year[i] == 2016){
    NBAOdds$Pace[i]<- Pace$pace2016[rownames(Pace$pace2016) == NBAOdds$Home[i],colnames(Pace$pace2016) == NBAOdds$Away[i]]
  }
  if (NBAOdds$Year[i] == 2015){
    NBAOdds$Pace[i]<- Pace$pace2015[rownames(Pace$pace2015) == NBAOdds$Home[i],colnames(Pace$pace2015) == NBAOdds$Away[i]]
  }
  if (NBAOdds$Year[i] == 2014){
    NBAOdds$Pace[i]<- Pace$pace2014[rownames(Pace$pace2014) == NBAOdds$Home[i],colnames(Pace$pace2014) == NBAOdds$Away[i]]
  }
  if (NBAOdds$Year[i] == 2013){
    NBAOdds$Pace[i]<- Pace$pace2013[rownames(Pace$pace2013) == NBAOdds$Home[i],colnames(Pace$pace2013) == NBAOdds$Away[i]]
  }
  if (NBAOdds$Year[i] == 2012){
    NBAOdds$Pace[i]<- Pace$pace2012[rownames(Pace$pace2012) == NBAOdds$Home[i],colnames(Pace$pace2012) == NBAOdds$Away[i]]
  }
}

saveRDS(NBAOdds, "NBAOdds_final.rds")
NBAOdds <- readRDS("NBAOdds_final.rds")

NBAOdds2012_2015 <- filter(NBAOdds, Year != 2016)
NBAOdds2016 <- filter(NBAOdds, Year == 2016)


#####Forcast

####Caret version
library(caret)
ctrl <- trainControl(method = "repeatedcv", number=10, repeats=3)


grid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                    n.trees = seq(100, 1000, by = 50),
                    shrinkage = c(0.01, 0.1),
                    n.minobsinnode=c(1,5,10))


# train the GBM model
set.seed(7)
modelGbmforcast <- train(x = NBAOdds2012_2015[,14:16],y = NBAOdds2012_2015[,12], method = "gbm",  preProcess = c("center", "scale"), verbose = FALSE, trControl = ctrl, tuneGrid = grid)

gbmcaretforecast <- predict(modelGbmforcast, NBAOdds2016[,14:16])
####

#Add BRT fit to model

#Add caret fit to model
NBAOdds2016$GBMfit <- gbmcaretforecast


#Plot predictions
predict_comparison <- melt(NBAOdds2016, id.vars = "Diff", measure.vars = c("Home.Spread", "GBMfit"))
predict_comparison$variable <- ifelse(predict_comparison$variable == "Home.Spread", "Vegas odds","PPS odds")
ggplot(predict_comparison, aes(x = Diff, y = value)) + geom_point(aes(color = variable)) + geom_smooth(method = lm, aes(fill=variable, color = variable)) + xlab("Observed spread") + ylab("Predicted spread") + theme(legend.position = "bottom")


#get The predicted

postResample(pred = NBAOdds2016$GBMfit, obs = NBAOdds2016$Diff)
postResample(pred = NBAOdds2016$Home.Spread, obs = NBAOdds2016$Diff)

####Surface plot


For.predictions <- expand.grid(defAPPS = seq(from = min(NBAOdds$defAPPS), to = max(NBAOdds$defAPPS), length.out = 100), 
                               offAPPS =seq(from= min(NBAOdds$offAPPS),to = max(NBAOdds$offAPPS), length.out = 100),
                               Pace = mean(NBAOdds2016$Pace))

For.predictions$Spread <- predict(modelGbmforcast, For.predictions)

wireframe(Spread ~  offAPPS + defAPPS, data = For.predictions, colorkey = TRUE, drape = TRUE, pretty = TRUE,scales = list(arrows = FALSE), screen = list(z = -220, x = -80))

For.predictions <- expand.grid(defAPPS = seq(from = min(NBAOdds$defAPPS), to = max(NBAOdds$defAPPS), length.out = 50), 
                               offAPPS =seq(from= min(NBAOdds$offAPPS),to = max(NBAOdds$offAPPS), length.out = 50),
                               Pace = mean(NBAOdds2016$Pace))
For.predictions$Spread <- predict(modelGbmforcast, For.predictions)

For.predictions2 <- For.predictions
For.predictions2$Type <- c("Predicted")
For.predictions3 <- For.predictions2[seq(from =1, to = NROW(For.predictions), by = 100),]
For.predictions3$Spread <- 0
For.predictions3$Type <- c("Push")
For.predictions2 <- rbind(For.predictions2, For.predictions3)

#Test 1
wireframe(Spread ~  offAPPS + defAPPS, group = Type, data = For.predictions2, colorkey = TRUE, drape = TRUE, pretty = TRUE,scales = list(arrows = FALSE), screen = list(z = -220, x = -80), par.settings = list(regions=list(alpha=0.75)))
#Test 2
wireframe(Spread ~  offAPPS + defAPPS, group = Type, data = For.predictions2, colorkey = TRUE, drape = TRUE, pretty = TRUE,scales = list(arrows = FALSE), screen = list(z = -220, x = -60), par.settings = list(regions=list(alpha=0.85)))
#Test3
wireframe(Spread ~  offAPPS + defAPPS, group = Type, data = For.predictions2, colorkey = TRUE, drape = TRUE, pretty = TRUE,scales = list(arrows = FALSE), screen = list(z = -220, x = -100))
