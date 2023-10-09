#Calculating prevalence
library(tidyverse)

# prevalence calculation ----------------------------------------

mon_data <- read.csv("raw_data/monitoring_info/DataS1.csv")
unique(mon_data$season)

#use spring detection prevalence only
mon_data1 <- mon_data %>%
  filter(season == "t1.spring2014" | season == "t4.spring2015")
n_distinct(mon_data1$visitID)
n_distinct(mon_data1$surveyID)
n_distinct(mon_data1$bidID)

mon_data2 <- mon_data1 %>%
  dplyr::select(c("visitID", "surveyID", "bidID",  "American.Avocet","Dunlin","Greater.Yellowlegs","Least.Sandpiper","Lesser.Yellowlegs","Long.billed.Curlew","Long.billed.Dowitcher","Unidentified.Dowitcher","Unidentified.Yellowlegs","Western.Sandpiper"))%>%
  mutate(Yellowlegs = Greater.Yellowlegs+Lesser.Yellowlegs+Unidentified.Yellowlegs,
         Dowitcher = Long.billed.Dowitcher+Unidentified.Dowitcher,
  )

prevalence <- mon_data2 %>%
  group_by(surveyID)%>%
  summarise(Avocet=sum(American.Avocet),
            Dowitcher=sum(Dowitcher),
            Dunlin=sum(Dunlin),
            LBCurlew=sum(Long.billed.Curlew),
            WSandpiper=sum(Western.Sandpiper),
            Yellowlegs=sum(Yellowlegs),
            LSandpiper=sum(Least.Sandpiper))
 
prevalence_surveyID <- prevalence %>%
  summarise(Avocet=sum(Avocet>0)/length(surveyID),
            Dowitcher=sum(Dowitcher>0)/length(surveyID),
            Dunlin=sum(Dunlin>0)/length(surveyID),
            LBCurlew=sum(LBCurlew>0)/length(surveyID),
            WSandpiper=sum(WSandpiper>0)/length(surveyID),
            Yellowlegs=sum(Yellowlegs>0)/length(surveyID),
            LSandpiper=sum(LSandpiper>0)/length(surveyID)
  )
  

save(prevalence_surveyID, file = "data_outputs/prev_surveyID.RData")


