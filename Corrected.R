shotDataTotal2017<- readRDS("shotDataTotal2017.rds")
##Every shot that GSW was involved with
a <- dplyr::filter(shotDataTotal2017, HTM == "GSW" | VTM == "GSW")
##Shots taken by golden state divided by the other team's shots

nrow(dplyr::filter(a, TEAM_NAME == "GSW"))/nrow(dplyr::filter(a, TEAM_NAME != "GSW"))


unique(shotDataTotal2017$TEAM_NAME)
       
unique(as.character(shotDataTotal2017$HTM))

          
            

shotDataTotal2017$HTM <- gsub("DET", "Det", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("ATL", "Atl", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("CHI", "Chi", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("BOS", "Bos", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("CLE", "Cle", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("NOP", "NO", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("GSW", "GSW", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("ORL", "ORL", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("WAS", "Was", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("PHI", "Phi", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("BKN", "Bkn", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("UTA", "Uta", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("MIA", "Mia", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("CHA", "Cha", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("TOR", "Tor", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("IND", "Ind", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("HOU", "Hou", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("DEN", "Den", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("MEM", "Mem", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("NYK", "NY", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("MIL", "Mil", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("OKC", "Okc", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("SAS", "Sas", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("DAL", "Dal", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("PHX", "Pho", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("POR", "Por", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("LAC", "Lac", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("SAC", "Sac", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("LAL", "Lal", as.character(shotDataTotal2017$HTM))
shotDataTotal2017$HTM <- gsub("MIN" , "Min", as.character(shotDataTotal2017$HTM))



shotDataTotal2017$VTM <- gsub("DET", "Det", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("ATL", "Atl", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("CHI", "Chi", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("BOS", "Bos", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("CLE", "Cle", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("NOP", "NO", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("GSW", "GSW", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("ORL", "ORL", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("WAS", "Was", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("PHI", "Phi", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("BKN", "Bkn", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("UTA", "Uta", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("MIA", "Mia", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("CHA", "Cha", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("TOR", "Tor", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("IND", "Ind", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("HOU", "Hou", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("DEN", "Den", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("MEM", "Mem", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("NYK", "NY", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("MIL", "Mil", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("OKC", "Okc", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("SAS", "Sas", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("DAL", "Dal", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("PHX", "Pho", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("POR", "Por", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("LAC", "Lac", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("SAC", "Sac", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("LAL", "Lal", as.character(shotDataTotal2017$VTM))
shotDataTotal2017$VTM <- gsub("MIN" , "Min", as.character(shotDataTotal2017$VTM))

saveRDS(shotDataTotal2017, "shotDataTotal2017.rds")

Correction <- matrix(ncol=2, nrow=30)
teams <-unique(shotDataTotal2017$TEAM_NAME)

for(i in 1:length(teams)){
  Correction[i,1] <- teams[i]
  a <- dplyr::filter(shotDataTotal2017, HTM == teams[i] | VTM == teams[i])
  Correction[i,2] <- nrow(dplyr::filter(a, TEAM_NAME == teams[i]))/nrow(dplyr::filter(a, TEAM_NAME != teams[i]))
}

Correction <- as.data.frame(Correction)
colnames(Correction) <- c("Team", "Factor")
Correction$Factor <- as.numeric(as.character(Correction$Factor))

summary(Correction)
