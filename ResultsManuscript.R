library(SpatialBall)

data("season2017")

season2016 <- readRDS("shotDataTotal2016.rds")
season2015 <- readRDS("shotDataTotal2015.rds")
season2014 <- readRDS("shotDataTotal2014.rds")
season2013 <- readRDS("shotDataTotal2013.rds")
season2012 <- readRDS("shotDataTotal2012.rds")

#Number of shots study

sum(nrow(season2017), nrow(season2016),nrow(season2015), nrow(season2014), nrow(season2013), nrow(season2012))


#SpatialNetRating

Rank <- SpatialRating(season2017)
colnames(Rank) <- c("Team", "OffSBR", "DefSBR", "NetSBR")

stargazer::stargazer(Rank, summary = FALSE, style = "qje")



#Number of games for training and test set

trains <- readRDS("trains.rds")

train <- trains[[7]]

rm(trains)

tests <- readRDS("tests.rds")
param <- readRDS("parameters.rds")
test <- tests[[7]]

rm(tests)

nrow(train) + nrow(DF3)


####Graph, model
library(SpatialBall)
data(BRT)

For.predictions <- expand.grid(defAPPS = seq(from = min(train$defAPPS), to = max(train$defAPPS), length.out = 100), 
                               offAPPS =seq(from= min(train$offAPPS),to = max(train$offAPPS), length.out = 100))

For.predictions$Spread <- predict(BRT, For.predictions)

For.predictions2 <- For.predictions
For.predictions2$Type <- c("Predicted")
For.predictions3 <- For.predictions2[seq(from =1, to = NROW(For.predictions), by = 100),]
For.predictions3$Spread <- 0
For.predictions3$Type <- c("Push")
For.predictions2 <- rbind(For.predictions2, For.predictions3)

#Test 1
wireframe(Spread ~  offAPPS + defAPPS, group = Type, data = For.predictions2, colorkey = TRUE, drape = TRUE, pretty = TRUE,scales = list(arrows = FALSE), screen = list(z = -220, x = -80), par.settings = list(regions=list(alpha=0.75)))



###########################Accuracy, etc confussion matrix DF3 is the real test set
library(caret)

DF3 <- readRDS("DF3.rds")

ClassificationWin <- data.frame(Winner = ifelse(DF3$HomeRes < 0, "H", "V"), BRTWinner = ifelse(DF3$PredBRT < 0, "H", "V"), VegasWinner = ifelse(DF3$VegasPred < 0, "H", "V"))


confusionMatrix(ClassificationWin$BRTWinner, ClassificationWin$Winner)

confusionMatrix(ClassificationWin$VegasWinner, ClassificationWin$Winner)

(598 + 354)/nrow(DF3)
#####Rsquareds


postResample(DF3$PredBRT, DF3$HomeRes)

postResample(DF3$VegasPred, DF3$HomeRes)

#####Table best performance of model compared to vegas


TAT <- DF3
TAT$DifBRT <- abs(TAT$PredBRT - TAT$HomeRes)
TAT$DifVegas <- abs(TAT$VegasPred - TAT$HomeRes)
#Most Negative results in Compare are the best results for Vegas, the more Positive best results for us
TAT$Compare <- TAT$DifVegas - TAT$DifBRT

TATPlayoffs <- dplyr::filter(TAT, Type == "Playoffs")

#Best games for vegas
BestVegas <- head(dplyr::arrange(TATPlayoffs, Compare), 10)
BestVegas <- BestVegas[,-c(7,8,10)]
colnames(BestVegas) <- c("Date", "Visitor", "Visit PTS", "Home", "Home PTS", "Season", "Spread", "BRT Pred", "Vegas Pred", "DifBRT", "DifVegas", "Comparison")
stargazer::stargazer(BestVegas, summary = FALSE)
#best games for me
BestBRT <- head(dplyr::arrange(TATPlayoffs, desc(Compare)), 10)
BestBRT <- BestBRT[,-c(7,8,10)]
colnames(BestBRT) <- c("Date", "Visitor", "Visit PTS", "Home", "Home PTS", "Season", "Spread", "BRT Pred", "Vegas Pred", "DifBRT", "DifVegas", "Comparison")
stargazer::stargazer(BestBRT, summary = FALSE)

