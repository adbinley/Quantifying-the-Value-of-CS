# Golden standard approach
# use the estimates from integrated model, adjusted for sensitivity and specificity, as the best knowledge of whether species are detected or not
#100 reps of the integrated model predictions, corrected for sensitivity and specificity using Bayes theorem 

library(rgdal)
library(raster)
library(tidyverse)
library(sf)

# use updated values and farm selection to calculate validation value at each budget

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

spec <- c("amav","dowi","dunl","lbcu","wesa","yleg","lesa")

spec_upper <- c("AMAV","DOWI","DUNL","LBCU","WESA","YLEG","LESA")

#note that these have been removed from the project for privacy reasons
fields <- readOGR(dsn = "raw_data/TNC_field_data", layer = "fields_4_allison_at_carleton")

#data on prevalence of species (see Calc_prevalence.R)
load("data_outputs/prev_surveyID.RData")

prevalence <- prevalence_surveyID

calc_values <- function(x){ifelse(x>thresh,prob.det.if.above,prob.det.if.below)}

################ both model #########################

mod1 <- "both"

both_results_list <- list()

for(a in 1:length(species)){
  
  species1 <- species[a]
  spec1 <- spec[a]
  spec_upper1 <- spec_upper[a]
  prev <- prevalence[,species1]
  
  q <- raster(paste0("raw_data/Robinson_model_outputs/eBird and TNC data for Allie/rf-prediction_AVG_",mod1,"_",species1,".tif"))
  
  kappa <- readRDS(paste0("raw_data/Robinson_model_outputs/Acc_Metrics_For_Allie/Acc_Metrics_For_Allie/",spec_upper1,"_",mod1,".rds"))%>%
    as.data.frame()
  sensitivity <- kappa$sensitivity
  specificity <- kappa$specificity
  threshold <- kappa$thresh
  
  #loop through each farm first, all 100 reps
  
  farm_vals_list <- list()
  
  for(b in 1:length(fields)){
    
    p <- fields[b,] 
    
    reps <- list()
    
    for(t in 1:length(threshold)){
      
      thresh <- threshold[t]
      
      conseq.table<-data.frame(c(1,0),c(0,0))
      rownames(conseq.table)<-c("select","don't select")
      colnames(conseq.table)<-c("detected","absent")
      
      sensi <- sensitivity[t]
      speci <- specificity[t]
      
      mon.acc<- data.frame(c(sensi, 1-sensi),
                           c(1-speci, speci))
      
      # "above threshold" counts as presence/detected here, "below threshold" is absence/ not detected
      # sensitivity = prob detected given that its above the kappa threshold (ie "predicted detected")
      # specificity = prob not detected given that its below the maximized kappa threshold (ie "predicted not detected")
      rownames(mon.acc)<-c("above threshold","below threshold")
      colnames(mon.acc)<-c("detected","absent")
      
      
      prob.det.if.above <- mon.acc["above threshold","detected"]*prev/((mon.acc["above threshold","detected"]*prev)+(mon.acc["above threshold","absent"]*(1-prev)))
      prob.det.if.above <- prob.det.if.above[1,1]
      
      prob.det.if.below <- mon.acc["below threshold","detected"]*prev/((mon.acc["below threshold","detected"]*prev)+(mon.acc["below threshold","absent"]*(1-prev)))
      prob.det.if.below <- prob.det.if.below[1,1]
      
      r <- calc(q,fun=calc_values)
      
      vals_exp <- raster::extract(r,p)[[1]]
      
      reps[[t]] <- vals_exp
      
    }
    
    reps1 <- do.call(rbind.data.frame, reps)
    
    farm_vals_list[[b]] <- reps1
    
  }
  
  #farm_vals_list is now a list of dataframes, each df is the 100 reps for each farm
  # dataframes have different numbers of columns depending on how many cells in each farm
  
  both_results_list[[a]] <- farm_vals_list
  # this is a list of the farm_vals_lists for each species
  
}

names(both_results_list) <- species

#100 reps of biodiversity value of each species on each farm, uncertainty captured by the 100 kappa, sensitivity and specificity values from Orin's paper

farm_values_both <- list()

for(c in 1:length(species)){
  
  s <- species[c]
  
  g <-  both_results_list[[s]] #each farm_vals_list
  
  farm_values <- list()
  
  for(d in 1:length(g)){ #for each farm
    
    farmx <- g[[d]] #100 reps of values for each cell in field x
    
    value <- rowSums(farmx)
    
    
    farm_values[[d]] <- value 
    
  }
  
  farm_values1 <- do.call(rbind.data.frame, farm_values)
  colnames(farm_values1) <- as.character(1:100)
  #species values1 is a df with all 207 farms (rows) and 100 rep values (cols)
  
  farm_values_both[[c]] <- farm_values1
  
}

names(farm_values_both) <- species

farm_values_both1 <- bind_rows(farm_values_both, .id = "species")


data <- fields@data

#### costs ####
farm_id <- rep(1:length(fields), length(species))
area <- rep(data$Shape_Area, length(species))
set.seed(1)
auction_cost <- rnorm(207,53.97,15.14)
auction_cost1 <- cbind("farm_id"=1:207, "cost_per_area"=auction_cost)

farm_values_both2 <- cbind(farm_id,area,farm_values_both1)
farm_values_both3 <- left_join(farm_values_both2,auction_cost1, copy=T)%>%
  mutate(total_cost = (area/4046.86)*cost_per_area)


save(farm_values_both3, file = "data_outputs/both_values_reps_SurveyID.RData")

#### validation data ####
load("data_outputs/both_values_reps_SurveyID.RData")

farm_vals_wide <- farm_values_both3

farm_vals_long <- pivot_longer(farm_values_both3,
                               cols = !c("farm_id", "area","cost_per_area","species","total_cost"),
                               names_to = "replicate",
                               values_to = "value")

#calculate value of selections based on straight model estimates, validated using the 100 reps of integrated model predictions corrected using Bayes theorem

##### TNC with penalty ####

load("data_outputs/tnc_selections_species_GS.RData")

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

#for each budget and species, need to sum the values for the selected farms across all reps

reps <- 1:100

species_values_list <- list()
species_values_reps <- list()

for(c in 1:length(species)){ #for each species
  
  spec <- species[c]
  selections_by_budget <- species_list_tnc[[c]]
  true_values <- farm_vals_wide %>%
    filter(species == spec)

  
  vals_per_budget <- list()
  
  for(a in 1:length(selections_by_budget)){ #and for each budget
    
    #selects 100 reps of 1 budget, farms selected by prioritization
    selections <- selections_by_budget[[a]]
    
    vals_per_rep_total <- list()
    # vals_each_farm <- list()
    
    for(b in 1:length(reps)){
      
      rep <- as.character(reps[b])
      #values for a specific species, budget, rep
      val_per_rep_and_farm <- true_values[which(true_values$farm_id %in% selections),rep] 
      vals_per_rep_sum <- sum(val_per_rep_and_farm) #summed across all selected farms
      
 #value for each farm
      vals_per_rep_total[[b]] <- vals_per_rep_sum #value of decision
      
    }
    
    vals_per_rep1 <- do.call(rbind.data.frame, vals_per_rep_total)
    vals_per_repv <- vals_per_rep1[,1] #vector of the total value for each rep at a given budget
    
    vals_per_budget[[a]] <- vals_per_repv #list of vectors (value at each rep, L100), at each budget
    
  }
  
  tnc_values_df <- do.call(rbind.data.frame, vals_per_budget)     
  names(tnc_values_df) <- as.character(1:100)    
  budget <- seq(150000,1500000,by = 50000)
  tnc_values_df1 <- cbind(budget,tnc_values_df)
  tnc_values_df2 <- pivot_longer(tnc_values_df1,
                                 cols = !budget,
                                 names_to = "replicate",
                                 values_to = "value") 
  
  tnc_summary <- tnc_values_df2 %>%
    group_by(budget)%>%
    summarise(mean = mean(value),
              stdev = sd(value))
  
  budget <- c(0,50000,100000)
  m <- rep(0,3)
  s <- rep(0,3)
  d <- cbind(budget, m,s)%>%
    data.frame()
  names(d) <- c("budget","mean","stdev")
  
  tnc_summary1 <- rbind(d,tnc_summary)
  
  species_values_list[[c]] <- tnc_summary1 
  species_values_reps[[c]] <- tnc_values_df2
  
}

names(species_values_list) <- species
names(species_values_reps) <- species

save(species_values_reps, file = "data_outputs/results/tnc_species_reps_GS.RData")
save(species_values_list, file = "data_outputs/results/tnc_species_summary_GS.RData")

##### TNC WITHOUT penalty ####

load("data_outputs/free_tnc_selections_species_GS.RData")

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

#for each budget and species, need to sum the values for the selected farms across all reps

reps <- 1:100

species_values_list <- list()
species_values_reps <- list()

for(c in 1:length(species)){ #for each species
  
  spec <- species[c]
  selections_by_budget <- species_list_tnc[[c]]
  true_values <- farm_vals_wide %>%
    filter(species == spec)
  
  
  vals_per_budget <- list()
  
  for(a in 1:length(selections_by_budget)){ #and for each budget
    
    #selects 100 reps of 1 budget, farms selected by prioritization
    selections <- selections_by_budget[[a]]
    
    vals_per_rep_total <- list()
    # vals_each_farm <- list()
    
    for(b in 1:length(reps)){
      
      rep <- as.character(reps[b])
      #values for a specific species, budget, rep
      val_per_rep_and_farm <- true_values[which(true_values$farm_id %in% selections),rep] 
      vals_per_rep_sum <- sum(val_per_rep_and_farm) #summed across all selected farms
      
      #list of total vals for a given budget, rep, species
      # vals_each_farm[[b]] <- val_per_rep_and_farm #value for each farm
      vals_per_rep_total[[b]] <- vals_per_rep_sum #value of decision
      
    }
    
    vals_per_rep1 <- do.call(rbind.data.frame, vals_per_rep_total)
    vals_per_repv <- vals_per_rep1[,1] #vector of the total value for each rep at a given budget
    
    vals_per_budget[[a]] <- vals_per_repv #list of vectors (value at each rep, L100), at each budget
    
  }
  
  tnc_values_df <- do.call(rbind.data.frame, vals_per_budget)     
  names(tnc_values_df) <- as.character(1:100)    
  budget <- seq(50000,1500000,by = 50000)
  tnc_values_df1 <- cbind(budget,tnc_values_df)
  tnc_values_df2 <- pivot_longer(tnc_values_df1,
                                 cols = !budget,
                                 names_to = "replicate",
                                 values_to = "value") 
  
  tnc_summary <- tnc_values_df2 %>%
    group_by(budget)%>%
    summarise(mean = mean(value),
              stdev = sd(value))
  
  budget <- 0
  m <- 0
  s <- 0
  d <- cbind(budget, m,s)%>%
    data.frame()
  names(d) <- c("budget","mean","stdev")
  
  tnc_summary1 <- rbind(d,tnc_summary)
  
  species_values_list[[c]] <- tnc_summary1 
  species_values_reps[[c]] <- tnc_values_df2
  
}

#beep()

names(species_values_list) <- species
names(species_values_reps) <- species

save(species_values_reps, file = "data_outputs/results/free_tnc_species_reps_GS.RData")
save(species_values_list, file = "data_outputs/results/free_tnc_species_summary_GS.RData")


##### GS WITHOUT penalty ####

load("data_outputs/both_free_selections_species_GS.RData")

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

#for each budget and species, need to sum the values for the selected farms across all reps

reps <- 1:100

species_values_list <- list()
species_values_reps <- list()

for(c in 1:length(species)){ #for each species
  
  spec <- species[c]
  selections_by_budget <- species_list_both[[c]]
  true_values <- farm_vals_wide %>%
    filter(species == spec)
  
  
  vals_per_budget <- list()
  
  for(a in 1:length(selections_by_budget)){ #and for each budget
    
    #selects 100 reps of 1 budget, farms selected by prioritization
    selections <- selections_by_budget[[a]]
    
    vals_per_rep_total <- list()
    # vals_each_farm <- list()
    
    for(b in 1:length(reps)){
      
      rep <- as.character(reps[b])
      #values for a specific species, budget, rep
      val_per_rep_and_farm <- true_values[which(true_values$farm_id %in% selections),rep] 
      vals_per_rep_sum <- sum(val_per_rep_and_farm) #summed across all selected farms
      
      #list of total vals for a given budget, rep, species
      # vals_each_farm[[b]] <- val_per_rep_and_farm #value for each farm
      vals_per_rep_total[[b]] <- vals_per_rep_sum #value of decision
      
    }
    
    vals_per_rep1 <- do.call(rbind.data.frame, vals_per_rep_total)
    vals_per_repv <- vals_per_rep1[,1] #vector of the total value for each rep at a given budget
    
    vals_per_budget[[a]] <- vals_per_repv #list of vectors (value at each rep, L100), at each budget
    
  }
  
  both_values_df <- do.call(rbind.data.frame, vals_per_budget)     
  names(both_values_df) <- as.character(1:100)    
  budget <- seq(50000,1500000,by = 50000)
  both_values_df1 <- cbind(budget,both_values_df)
  both_values_df2 <- pivot_longer(both_values_df1,
                                 cols = !budget,
                                 names_to = "replicate",
                                 values_to = "value") 
  
  both_summary <- both_values_df2 %>%
    group_by(budget)%>%
    summarise(mean = mean(value),
              stdev = sd(value))
  
  budget <- 0
  m <- 0
  s <- 0
  d <- cbind(budget, m,s)%>%
    data.frame()
  names(d) <- c("budget","mean","stdev")
  
  both_summary1 <- rbind(d,both_summary)
  
  species_values_list[[c]] <- both_summary1 
  species_values_reps[[c]] <- both_values_df2
  
}

#beep()

names(species_values_list) <- species
names(species_values_reps) <- species

save(species_values_reps, file = "data_outputs/results/free_both_species_reps_GS.RData")
save(species_values_list, file = "data_outputs/results/free_both_species_summary_GS.RData")

##### GS with penalty ####

load("data_outputs/both_penalty_selections_species_GS.RData")

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

#for each budget and species, need to sum the values for the selected farms across all reps

reps <- 1:100

species_values_list <- list()
species_values_reps <- list()

for(c in 1:length(species)){ #for each species
  
  spec <- species[c]
  selections_by_budget <- species_list_both[[c]]
  true_values <- farm_vals_wide %>%
    filter(species == spec)
  
  
  vals_per_budget <- list()
  
  for(a in 1:length(selections_by_budget)){ #and for each budget
    
    #selects 100 reps of 1 budget, farms selected by prioritization
    selections <- selections_by_budget[[a]]
    
    vals_per_rep_total <- list()
    
    for(b in 1:length(reps)){
      
      rep <- as.character(reps[b])
      #values for a specific species, budget, rep
      val_per_rep_and_farm <- true_values[which(true_values$farm_id %in% selections),rep] 
      vals_per_rep_sum <- sum(val_per_rep_and_farm) #summed across all selected farms
      
      vals_per_rep_total[[b]] <- vals_per_rep_sum #value of decision
      
    }
    
    vals_per_rep1 <- do.call(rbind.data.frame, vals_per_rep_total)
    vals_per_repv <- vals_per_rep1[,1] #vector of the total value for each rep at a given budget
    
    vals_per_budget[[a]] <- vals_per_repv #list of vectors (value at each rep, L100), at each budget
    
  }
  
  both_values_df <- do.call(rbind.data.frame, vals_per_budget)     
  names(both_values_df) <- as.character(1:100)    
  budget <- seq(150000,1500000,by = 50000)
  both_values_df1 <- cbind(budget,both_values_df)
  both_values_df2 <- pivot_longer(both_values_df1,
                                 cols = !budget,
                                 names_to = "replicate",
                                 values_to = "value") 
  
  both_summary <- both_values_df2 %>%
    group_by(budget)%>%
    summarise(mean = mean(value),
              stdev = sd(value))
  
  budget <- c(0,50000,100000)
  m <- rep(0,3)
  s <- rep(0,3)
  d <- cbind(budget, m,s)%>%
    data.frame()
  names(d) <- c("budget","mean","stdev")
  
  both_summary1 <- rbind(d,both_summary)
  
  species_values_list[[c]] <- both_summary1 
  species_values_reps[[c]] <- both_values_df2
  
}

beep()

names(species_values_list) <- species
names(species_values_reps) <- species

save(species_values_reps, file = "data_outputs/results/both_penalty_species_reps_GS.RData")
save(species_values_list, file = "data_outputs/results/both_penalty_species_summary_GS.RData")


####EBD####

load("data_outputs/ebd_selections_species_GS.RData")

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

#for each budget and species, need to sum the values for the selected farms across all reps

reps <- 1:100

species_values_list <- list()
species_values_reps <- list()

for(c in 1:length(species)){ #for each species
  
  spec <- species[c]
  selections_by_budget <- species_list_ebd[[c]]
  true_values <- farm_vals_wide %>%
    filter(species == spec)
  
  
  vals_per_budget <- list()
  
  for(a in 1:length(selections_by_budget)){ #and for each budget
    
    #selects 100 reps of 1 budget, farms selected by prioritization
    selections <- selections_by_budget[[a]]
    
    vals_per_rep_total <- list()
    # vals_each_farm <- list()
    
    for(b in 1:length(reps)){
      
      rep <- as.character(reps[b])
      #values for a specific species, budget, rep
      val_per_rep_and_farm <- true_values[which(true_values$farm_id %in% selections),rep] 
      vals_per_rep_sum <- sum(val_per_rep_and_farm) #summed across all selected farms
      
      #list of total vals for a given budget, rep, species
      # vals_each_farm[[b]] <- val_per_rep_and_farm #value for each farm
      vals_per_rep_total[[b]] <- vals_per_rep_sum #value of decision
      
    }
    
    vals_per_rep1 <- do.call(rbind.data.frame, vals_per_rep_total)
    vals_per_repv <- vals_per_rep1[,1] #vector of the total value for each rep at a given budget
    
    vals_per_budget[[a]] <- vals_per_repv #list of vectors (value at each rep, L100), at each budget
    
  }
  
  ebd_values_df <- do.call(rbind.data.frame, vals_per_budget)     
  names(ebd_values_df) <- as.character(1:100)    
  budget <- seq(50000,1500000,by = 50000)
  ebd_values_df1 <- cbind(budget,ebd_values_df)
  ebd_values_df2 <- pivot_longer(ebd_values_df1,
                                 cols = !budget,
                                 names_to = "replicate",
                                 values_to = "value") 
  
  ebd_summary <- ebd_values_df2 %>%
    group_by(budget)%>%
    summarise(mean = mean(value),
              stdev = sd(value))
  
  budget <- 0
  m <- 0
  s <- 0
  d <- cbind(budget, m,s)%>%
    data.frame()
  names(d) <- c("budget","mean","stdev")
  
  ebd_summary1 <- rbind(d,ebd_summary)
  
  species_values_list[[c]] <- ebd_summary1
  species_values_reps[[c]] <- ebd_values_df2
  
}

beep()

names(species_values_list) <- species
names(species_values_reps) <- species

save(species_values_reps, file = "data_outputs/results/ebd_species_reps_GS.RData")
save(species_values_list, file = "data_outputs/results/ebd_species_summary_GS.RData")

