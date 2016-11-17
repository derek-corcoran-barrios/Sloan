library(XML)

years <- c(2012:2016)
pacetable <- list()
webtable<- vector()
for(i in 1:length(years)){
  webtable[i] <- paste("http://www.espn.com/nba/hollinger/teamstats/_/sort/paceFactor/year/", years[i], sep = "")
  pacetable.temp <- readHTMLTable(webtable[i])[[1]]
  colnames(pacetable.temp) <- as.character(t(pacetable.temp[1,]))
  pacetable.temp <- pacetable.temp[-1,2:3]
  pacetable.temp$TEAM <- as.factor(as.character(pacetable.temp$TEAM))
  pacetable.temp$PACE <- as.numeric(as.character(pacetable.temp$PACE))
  pacetable.temp$YEAR <- years[i]
  pacetable[[i]] <- pacetable.temp
}

pacetable <- do.call("rbind", pacetable)

library(ggplot2)
ggplot(pacetable, aes(y = PACE, x=as.factor(YEAR))) + geom_boxplot()


pacetable$TEAM











#####

Offensive_teams <- as.character(unique(shotDataTotal2016$TEAM_NAME))

defenseve_names <- names(shotDatafDef2016)

df <- data.frame(matrix(ncol = 30, nrow = 30))
colnames(df) <- as.character(unique(shotDataTotal2016$TEAM_NAME))
rownames(df) <- names(shotDatafDef2016)

for (i in 1:length(Offensive_teams)) {
  for (j in 1:length(defenseve_names)){
    df[rownames(df) == defenseve_names[j],colnames(df) == Offensive_teams[i]] <- ShotComparison(OffTeam = Offensive_teams[i], DefTown = defenseve_names[j], SeasondataOff = shotDataTotal2016, SeasonDataDef = shotDatafDef2016, nbins = 30)$PPSAA
  }
}
