# Boosted Regression Trees in R
################ note to reader #####################
# This is a cut-down version of a tutorial prepared by Jane Elith and John Leathwick, 
# to show how to use our code to fit boosted regression tree models. 
# Refer to the Word document for fuller explanations, and for the first section, which has been deleted. 
#####################################################

##################### Install our additional BRT functions 
# These are provided in a file such as ?brt.functions.R? and can be loaded into R using the source command, e.g.:

source("brt.functions.R")

# This assumes that the brt functions file is located in the working directory. 

# Or browse to it through the menu:

# Our functions load the gbm library automatically, but if you want to load it yourself use:

library(gbm)

# or through the menu:    Packages/load package?.


##################### Importing and checking example data
# Data stored in a text file are typically read in using the read.table or read.csv command, the help material for which can be accessed, for example, using the following command:

?read.csv 

# If reading in the example data used in our examples here, and which is supplied as a comma-delimited file ("model.data.csv") with a header, the appropriate command to read the data from a directory "brt" on your C drive, to R object that we will call ?model.data? would be: 

model.data <- read.csv("c:/brt/model.data.csv")

# Note that the data had one column with characters in it ? using the above syntax, this will have been automatically changed into a "factor" variable, which is how we want to model it. To stop that automatic conversion, look at the help file for use of as.is

# You can look at the data to check it:

model.data[1:3,]  

# The above is standard R syntax, asking for all columns and the first 3 rows. Alternatively you could have used the function "head"

# You can see that by default it prints the row number on the far left. The site number (in this case, simply 1 to 1000) is in column 1. Presence-absence data for Anguilla australis (Angaus) are in column 2. The environmental variables are in columns 3 to 14. This is the set of data used in all the figures and results in Elith, Leathwick & Hastie (2008) that rely on 1000 sites. 

##################### Fitting a model
# You need to decide what settings to use ? the article associated with this tutorial gives you information on what to use as rules of thumb. 
# These data have 1000 sites, comprising 202 presence records for the short-finned eel (the command sum(model.data$Angaus)will give you the total number of presences). As a first guess you could decide there are enough data to model interactions of reasonable complexity, and a lr of about 0.01 could be a reasonable starting point.
# To use our function that steps forward and identifies the optimal number of trees (nt) use this call:

angaus.tc5.lr01 <- gbm.step(data=model.data, 
    gbm.x = 3:14,
    gbm.y = 2,
    family = "bernoulli",
    tree.complexity = 5,
    learning.rate = 0.01,
    bag.fraction = 0.5)


set.seed(123)
set.seed(123)
BRT <- gbm.step(NBAOdds2012_2015, gbm.x = 14:15, gbm.y = 12, family = "gaussian", plot.main = TRUE, plot.folds = FALSE)
    


names(BRT)

# The above was a first guess at settings, using rules of thumb discussed in Elith et al. (2008). It made a model with only 650 trees, so our next step would be to reduce the lr  - e.g., try lr = 0.005, to aim for over 1000 trees ? i.e.:

set.seed(123)
BRT2 <- gbm.step(NBAOdds2012_2015, 
                gbm.x = 14:15, 
                gbm.y = 12, 
                family = "gaussian", 
                plot.main = TRUE, 
                plot.folds = FALSE,
                tree.complexity = 5,
    learning.rate = 0.005,
    bag.fraction = 0.5)







gbm.plot(BRT2, n.plots=2, write.title = F, plot.layout = c(1,2))
gbm.plot(BRT, n.plots=2, write.title = F, plot.layout = c(1,2))










# To make predictions to sites from the BRT model use Ridgeway's code:

library(gbm)
preds <- predict.gbm(BRT, NBAOdds2016, n.trees=BRT$gbm.call$best.trees, type="response")
NBAOdds2016$preds <- preds
preds2 <- predict.gbm(BRT2, NBAOdds2016, n.trees=BRT2$gbm.call$best.trees, type="response")


ggplot(NBAOdds2016, aes(x = preds, y = Diff)) + geom_point() + geom_smooth()
# or use our code with the defaults, which means it just makes predictions to the workspace:

gbm.predict.grids(angaus.tc5.lr005, eval.data, want.grids = F, sp.name = "preds")

# In both cases the predictions will be in a vector called preds, because that's what we named them.
# These are evaluation sites, and have observations in column 1 (named Angaus_obs). They are independent of the model building set and could be used for an independent evaluation. For example here is code for calculating the deviance and the AUC (area under the ROC curve):

calc.deviance(sqrt(mean(error^2)),calc.mean=T, family = "gaussian")
calc.deviance(NBAOdds2016$Diff,preds2,calc.mean=T, family = "gaussian")

calc.deviance(NBAOdds2016$Diff,NBAOdds2016$Home.Spread,calc.mean=T, family = "gaussian")

RMSE <- sqrt(mean((NBAOdds2016$Diff- preds)^2))

RMSEVEGAS <- sqrt(mean((NBAOdds2016$Diff- NBAOdds2016$Home.Spread)^2))


# Note that the calc.deviance function has different formulae for different distributions of data; the default is binomial, so we didn't specify it in the call

##################### Code for a figure like figure 2, Elith et al. (2008)

# One useful feature of prediction in gbm is you can predict to a varying number of trees.
# The full set of code here shows how to make one of the graphed lines from Fig. 2 in our paper, using a model of 5000 trees developed with gbm.fixed

angaus.5000 <- gbm.fixed(data=model.data, gbm.x = 3:14, gbm.y = 2, learning.rate = 0.005, tree.complexity = 5, n.trees = 5000)

tree.list <- seq(100, 5000, by = 100)

pred <- predict.gbm(angaus.5000, eval.data, n.trees = tree.list, "response")

# Note that the code above makes a matrix, with each column being the predictions from the model angaus.5000 to the number of trees specified by that element of tree.list ? for example, the predictions in column 5 are for tree.list[5] = 500 trees. 
# Now to calculate the deviance of all these results, and plot them:

angaus.pred.deviance <- rep(0,50)

for (i in 1:50) {
angaus.pred.deviance[i] <- calc.deviance(eval.data$Angaus_obs, pred[,i],calc.mean=T)
}

plot(tree.list,angaus.pred.deviance,ylim=c(0.7,1),xlim = c(-100,5000),type='l', xlab = "number of trees",ylab = "predictive deviance", cex.lab = 1.5) 


##################### Making grids
# Alternatively, you can predict to grids (i.e., to a whole map). To do this, export the grids for your variables of interest from a GIS program, as ascii grids (note - these must all have the same cell size and the same number of rows and columns). We provide our data as ascii grids, plus a mask, that has "1" for a river and nodata elsewhere. These are rasterized versions of the line data we usually work with. You can view them by importing them into a GIS program. You can alternatively view just the header by opening the ascii file a text reading program; the first six lines contain information you will need if you want to make a grid of predictions. 
# Import them into R:
grid.names <- c("mask.asc", "segsumt.asc", "segtseas.asc","seglowflow.asc","dsdist.asc","dsmaxslope.asc","usavgt.asc","usraindays.asc","usslope.asc", "usnative.asc", "dsdam.asc", "locsed.asc")

variable.names <- c("mask", names(model.data)[c(3:12,14)]) # here make sure the order is the same as above, if you're using different data

for(i in 1:length(grid.names)){
assign(variable.names[i],scan(grid.names[i], skip=6, na.string = "-9999"), pos=1)
}

# Make them into a data frame, adding a column for the Method ? we will predict the probability of catch using electric fishing:

preddat <- data.frame (SegSumT,USNative,DSDist,LocSed,DSMaxSlope,USSlope,USRainDays,USAvgT,SegTSeas,SegLowFlow,DSDam,rep("electric", length(SegSumT)))

names(preddat)[12] <- "Method"

preddat$Method <- factor(preddat$Method, levels = levels(model.data$Method))

# This data frame has 49000 rows, because there were 49000 cells in each grid. Whilst you could predict to all sites, for very large grids you might want to reduce it to the sites you are interested in:

preddat<- preddat[!is.na(mask),]

# You will now have have 8058 rows in the data.
gbm will predict to sites even if there is no data there, so in your own data make sure you mask out those sites with no data, or for which there are insufficient predictors. Our code will re-form these reduced data frames into a full grid, so long as you form the data frame first then reduce it (as above) and give the function a vector the original length of one of the scanned grids. This is how to make a grid (it also returns the predictions to the R workspace too):

gbm.predict.grids(angaus.tc5.lr005, preddat, want.grids = T, sp.name = "angaus_preds",pred.vec = rep(-9999,49000), filepath = "c:/brt/", num.col = 250, num.row = 196, xll = 0, yll = 0, cell.size = 100, no.data = -9999, plot=T) 

# The information from the header file is included here (highlighted in aqua), and the pred.vec argument is where you give information on the value to be NO DATA (here, -9999) and the number of grid cells in the original grids (here, 49000). It will make a map in R. It will also have made a file in the directory you specified ? here, it will be c:/brt/angaus_preds.csv (see yellow highlights). This could be read into in a GIS program.

##################### Dealing with large grids
# If you have very large files you can do the above in a loop. For example, pretending that these ascii files are large and that even with changes to the memory size in R you can't predict, our code can be used in a loop. Let's say we have to do it in 4 repetitions; you would do a first run to make an output with a header file, then do the rest in a loop:

# First detail how many rows you will have in each run; here we'll do ? at a time

rep.rows <- c(49,49,49,49)

# Then do the first run:

for(i in 1:length(grid.names)){
assign(variable.names[i],scan(grid.names[i], skip=6, na.string = "-9999", nlines = rep.rows[1]), pos=1)
}

preddat <- data.frame (SegSumT,USNative,DSDist,LocSed,DSMaxSlope,USSlope,USRainDays,USAvgT,SegTSeas,SegLowFlow,DSDam,rep("electric", length(SegSumT)))

names(preddat)[12] <- "Method"

preddat$Method <- factor(preddat$Method, levels = levels(model.data$Method))

preddat<- preddat[!is.na(mask),]

# This version of preddat (above) only has 2116 rows, and before we masked it it was 12250 rows. You can use our code similar to before; it will write the header file, plus the predictions to these 2116 rows. We highlight the new parts of the commands, for clarity

gbm.predict.grids(angaus.tc5.lr005, preddat, want.grids = T, sp.name = "angaus_preds2",pred.vec = rep(-9999,250 * rep.rows[1]), filepath = "c:/brt/", num.col = 250, num.row = 196, xll = 0, yll = 0, cell.size = 100, no.data = -9999, plot=T, full.grid = F, part.number = 1, part.row = rep.rows[1]) 

# Then do the rest in a loop:

for(i in 2:4){
for(j in 1:length(grid.names)){
assign(variable.names[j],scan(grid.names[j], skip=(6 + sum(rep.rows[1:(i-1)])), na.string = "-9999", nlines = rep.rows[i]), pos=1)
}

preddat <- data.frame (SegSumT,USNative,DSDist,LocSed,DSMaxSlope,USSlope,USRainDays,USAvgT,SegTSeas,SegLowFlow,DSDam,rep("electric", length(SegSumT)))
names(preddat)[12] <- "Method"
preddat$Method <- factor(preddat$Method, levels = levels(model.data$Method))
preddat<- preddat[!is.na(mask),]

gbm.predict.grids(angaus.tc5.lr005, preddat, want.grids = T, sp.name = "angaus_preds2",pred.vec = rep(-9999,250 * rep.rows[i]), filepath = "c:/brt/", num.col = 250, full.grid = F, part.number = i, part.row = rep.rows[i], header = F) 
}

# Note that the function is currently also saving the predictions in the R workspace ? for large grids you probably want to turn that off; use pred2R = F within the call above.



