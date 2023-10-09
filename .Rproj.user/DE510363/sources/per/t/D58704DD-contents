#GS1 - prioritize properties based on the EBD model outputs and TNC model outputs
#also try prioritization using integrated predictions for comparison, with and without monitoring penalty, plus TNC prioritization without penalty

library(prioritizr)
library(tidyverse)
library(rgdal)
library(raster)
library(sf)

# data preparation --------------------------------------------------------

####TNC with penalty####

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

spec <- c("amav","dowi","dunl","lbcu","wesa","yleg","lesa")

spec_upper <- c("AMAV","DOWI","DUNL","LBCU","WESA","YLEG","LESA")

#note that these have been removed from the project for privacy reasons
fields <- readOGR(dsn = "raw_data/TNC_field_data", layer = "fields_4_allison_at_carleton")


mod1 <- "tnc"

tnc_list <- list()

for(a in 1:length(species)){
  
  species1 <- species[a]
  spec1 <- spec[a]
  spec_upper1 <- spec_upper[a]
  
  q <- raster(paste0("raw_data/Robinson_model_outputs/eBird and TNC data for Allie/rf-prediction_AVG_",mod1,"_",species1,".tif"))
  
  farm_list <- list()
  
  for(b in 1:length(fields)){
    
    p <- fields[b,]
    
    vals_pred <- raster::extract(q,p)[[1]]
    
    farm_list[[b]] <- vals_pred
  
  }
  
  tnc_list[[a]] <- farm_list
  
}
  
names(tnc_list) <- species
save(tnc_list, file = "data_outputs/tnc_farm_pred_vals.RData")  

farm_values_tnc <- list()

for(c in 1:length(species)){
  
  s <- species[c]
  
  g <- tnc_list[[c]]
  
  sum_farm_values <- list()
  
  for(d in 1:207){ #for each farm
    
    farmx <- g[[d]] 
    
    value <- sum(farmx)
    
    sum_farm_values[[d]] <- value 
    
  }
  
  sum_farm_values1 <- unlist(sum_farm_values)
  
  farm_values_tnc[[c]] <- sum_farm_values1
  
}

names(farm_values_tnc) <- species

farm_values_tnc1 <- bind_rows(farm_values_tnc, .id = "species")

field_data <- fields@data

# costs 
farm_id <- 1:207
area <- field_data$Shape_Area
set.seed(1)
auction_cost <- rnorm(207,53.97,15.14)
auction_cost1 <- cbind("farm_id"=1:207, "cost_per_area"=auction_cost)

farm_values_tnc2 <- cbind(farm_id,area,farm_values_tnc1)
farm_values_tnc3 <- left_join(farm_values_tnc2,auction_cost1, copy=T)%>%
  mutate(total_cost = (area/4046.86)*cost_per_area)

save(farm_values_tnc3, file = "data_outputs/tnc_prioritization_data.RData")  

#### both ####

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

spec <- c("amav","dowi","dunl","lbcu","wesa","yleg","lesa")

spec_upper <- c("AMAV","DOWI","DUNL","LBCU","WESA","YLEG","LESA")

#note that these have been removed from the project for privacy reasons
fields <- readOGR(dsn = "raw_data/TNC_field_data", layer = "fields_4_allison_at_carleton")


mod1 <- "both"

both_list <- list()

for(a in 1:length(species)){
  
  species1 <- species[a]
  spec1 <- spec[a]
  spec_upper1 <- spec_upper[a]
  
  q <- raster(paste0("raw_data/Robinson_model_outputs/eBird and TNC data for Allie/rf-prediction_AVG_",mod1,"_",species1,".tif"))
  
  farm_list <- list()
  
  for(b in 1:length(fields)){
    
    p <- fields[b,]
    
    vals_pred <- raster::extract(q,p)[[1]]
    
    farm_list[[b]] <- vals_pred
    
  }
  
  both_list[[a]] <- farm_list
  
}

names(both_list) <- species
save(both_list, file = "data_outputs/both_farm_pred_vals.RData")  

farm_values_both <- list()

for(c in 1:length(species)){
  
  s <- species[c]
  
  g <- both_list[[c]]
  
  sum_farm_values <- list()
  
  for(d in 1:207){ #for each farm
    
    farmx <- g[[d]] 
    
    value <- sum(farmx)
    
    sum_farm_values[[d]] <- value 
    
  }
  
  sum_farm_values1 <- unlist(sum_farm_values)
  
  farm_values_both[[c]] <- sum_farm_values1
  
}

names(farm_values_both) <- species

farm_values_both1 <- bind_rows(farm_values_both, .id = "species")

field_data <- fields@data

# costs 
farm_id <- 1:207
area <- field_data$Shape_Area
set.seed(1)
auction_cost <- rnorm(207,53.97,15.14)
auction_cost1 <- cbind("farm_id"=1:207, "cost_per_area"=auction_cost)

farm_values_both2 <- cbind(farm_id,area,farm_values_both1)
farm_values_both3 <- left_join(farm_values_both2,auction_cost1, copy=T)%>%
  mutate(total_cost = (area/4046.86)*cost_per_area)

save(farm_values_both3, file = "data_outputs/GS_prioritization_data.RData")  


####EBD####

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

spec <- c("amav","dowi","dunl","lbcu","wesa","yleg","lesa")

spec_upper <- c("AMAV","DOWI","DUNL","LBCU","WESA","YLEG","LESA")

#note that these have been removed from the project for privacy reasons
fields <- readOGR(dsn = "raw_data/TNC_field_data", layer = "fields_4_allison_at_carleton")


mod1 <- "ebd"

ebd_list <- list()

for(a in 1:length(species)){
  
  species1 <- species[a]
  spec1 <- spec[a]
  spec_upper1 <- spec_upper[a]
  
  q <- raster(paste0("raw_data/Robinson_model_outputs/eBird and TNC data for Allie/rf-prediction_AVG_",mod1,"_",species1,".tif"))
  
  farm_list <- list()
  
  for(b in 1:length(fields)){
    
    p <- fields[b,]
    
    vals_pred <- raster::extract(q,p)[[1]]
    
    farm_list[[b]] <- vals_pred
    
  }
  
  ebd_list[[a]] <- farm_list
  
}

names(ebd_list) <- species
save(ebd_list, file = "data_outputs/ebd_farm_pred_vals.RData")  

farm_values_ebd <- list()

for(c in 1:length(species)){
  
  s <- species[c]
  
  g <- ebd_list[[c]]
  
  sum_farm_values <- list()
  
  for(d in 1:207){ #for each farm
    
    farmx <- g[[d]] 
    
    value <- sum(farmx)
    
    
    sum_farm_values[[d]] <- value 
    
  }
  
  sum_farm_values1 <- unlist(sum_farm_values)
  
  farm_values_ebd[[c]] <- sum_farm_values1
  
}

names(farm_values_ebd) <- species

farm_values_ebd1 <- bind_rows(farm_values_ebd, .id = "species")

field_data <- fields@data

# costs 
farm_id <- 1:207
area <- field_data$Shape_Area
set.seed(1)
auction_cost <- rnorm(207,53.97,15.14)
auction_cost1 <- cbind("farm_id"=1:207, "cost_per_area"=auction_cost)

farm_values_ebd2 <- cbind(farm_id,area,farm_values_ebd1)
farm_values_ebd3 <- left_join(farm_values_ebd2,auction_cost1, copy=T)%>%
  mutate(total_cost = (area/4046.86)*cost_per_area)

save(farm_values_ebd3, file = "data_outputs/ebd_prioritization_data.RData")   


# prioritizr --------------------------------------------------------------

#### TNC with penalty ####
load("data_outputs/tnc_prioritization_data.RData")

tnc_run <- farm_values_tnc3

names(tnc_run)[names(tnc_run) == 'farm_id'] <- 'id'
names(tnc_run)[names(tnc_run) == 'total_cost'] <- 'cost'

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

budget <- seq(150000,1500000,by = 50000)


species_list_tnc <- list()

for(c in 1:length(species)){
  
  selections_by_budget <- list()
  
  for(a in 1:length(budget)){
    
    cap <- budget[a]-121622  #monitoring penalty
    
    p_tnc <- prioritizr::problem(x=tnc_run, features=species[c], cost_column = "cost")%>%
      add_relative_targets(1) %>% #aiming for 100% to maximize all values
      add_min_shortfall_objective(budget=cap)%>% #will try to get the average feature representation to 100%, but might poorly represent some that are in high cost areas
      add_binary_decisions()%>%#binary selection
      add_default_solver(gap = 0, verbose = FALSE)
    
    s_tnc <- solve(p_tnc)
    
    selection <- s_tnc$id[s_tnc$solution_1==1]
    
    selections_by_budget[[a]] <- selection
  }
  
  names(selections_by_budget) <- as.character(budget)
  
  species_list_tnc[[c]] <- selections_by_budget
  
}

names(species_list_tnc) <- species


save(species_list_tnc, file = "data_outputs/tnc_selections_species_GS.RData")

#### TNC without penalty ####
load("data_outputs/tnc_prioritization_data.RData")

tnc_run <- farm_values_tnc3

names(tnc_run)[names(tnc_run) == 'farm_id'] <- 'id'
names(tnc_run)[names(tnc_run) == 'total_cost'] <- 'cost'

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

budget <- seq(50000,1500000,by = 50000)


species_list_tnc <- list()

for(c in 1:length(species)){
  
  selections_by_budget <- list()
  
  for(a in 1:length(budget)){
    
    cap <- budget[a] # no monitoring penalty
    
    p_tnc <- prioritizr::problem(x=tnc_run, features=species[c], cost_column = "cost")%>%
      add_relative_targets(1) %>% #aiming for 100% to maximize all values
      add_min_shortfall_objective(budget=cap)%>% #will try to get the average feature representation to 100%, but might poorly represent some that are in high cost areas
      add_binary_decisions()%>%#binary selection
      add_default_solver(gap = 0, verbose = FALSE)
    
    s_tnc <- solve(p_tnc)
    
    selection <- s_tnc$id[s_tnc$solution_1==1]
    
    selections_by_budget[[a]] <- selection
  }
  
  names(selections_by_budget) <- as.character(budget)
  
  species_list_tnc[[c]] <- selections_by_budget
  
}

names(species_list_tnc) <- species


save(species_list_tnc, file = "data_outputs/free_tnc_selections_species_GS.RData")

#### GS without penalty ####
load("data_outputs/GS_prioritization_data.RData")

both_run <- farm_values_both3

names(both_run)[names(both_run) == 'farm_id'] <- 'id'
names(both_run)[names(both_run) == 'total_cost'] <- 'cost'

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

budget <- seq(50000,1500000,by = 50000)


species_list_both <- list()

for(c in 1:length(species)){
  
  selections_by_budget <- list()
  
  for(a in 1:length(budget)){
    
    cap <- budget[a] #no monitoring penalty
    
    p_both <- prioritizr::problem(x=both_run, features=species[c], cost_column = "cost")%>%
      add_relative_targets(1) %>% #aiming for 100% to maximize all values
      add_min_shortfall_objective(budget=cap)%>% #will try to get the average feature representation to 100%, but might poorly represent some that are in high cost areas
      add_binary_decisions()%>%#binary selection
      add_default_solver(gap = 0, verbose = FALSE)
    
    s_both <- solve(p_both)
    
    selection <- s_both$id[s_both$solution_1==1]
    
    selections_by_budget[[a]] <- selection
  }
  
  names(selections_by_budget) <- as.character(budget)
  
  species_list_both[[c]] <- selections_by_budget
  
}

names(species_list_both) <- species


save(species_list_both, file = "data_outputs/both_free_selections_species_GS.RData")


#### GS with penalty ####
load("data_outputs/GS_prioritization_data.RData")

both_run <- farm_values_both3

names(both_run)[names(both_run) == 'farm_id'] <- 'id'
names(both_run)[names(both_run) == 'total_cost'] <- 'cost'

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

budget <- seq(150000,1500000,by = 50000)


species_list_both <- list()

for(c in 1:length(species)){
  
  selections_by_budget <- list()
  
  for(a in 1:length(budget)){
    
    cap <- budget[a]-121622  #monitoring penalty
    
    p_both <- prioritizr::problem(x=both_run, features=species[c], cost_column = "cost")%>%
      add_relative_targets(1) %>% #aiming for 100% to maximize all values
      add_min_shortfall_objective(budget=cap)%>% #will try to get the average feature representation to 100%, but might poorly represent some that are in high cost areas
      add_binary_decisions()%>%#binary selection
      add_default_solver(gap = 0, verbose = FALSE)
    
    s_both <- solve(p_both)
    
    selection <- s_both$id[s_both$solution_1==1]
    
    selections_by_budget[[a]] <- selection
  }
  
  names(selections_by_budget) <- as.character(budget)
  
  species_list_both[[c]] <- selections_by_budget
  
}

names(species_list_both) <- species


save(species_list_both, file = "data_outputs/both_penalty_selections_species_GS.RData")


#### EBD ####


load("data_outputs/ebd_prioritization_data.RData")

ebd_run <- farm_values_ebd3

names(ebd_run)[names(ebd_run) == 'farm_id'] <- 'id'
names(ebd_run)[names(ebd_run) == 'total_cost'] <- 'cost'


species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

budget <- seq(50000,1500000,by = 50000)


species_list_ebd <- list()

for(c in 1:length(species)){
  
  selections_by_budget <- list()
  
  for(a in 1:length(budget)){
    
    cap <- budget[a] 

      p_ebd <- prioritizr::problem(x=ebd_run, features=species[c], cost_column = "cost")%>%
        add_relative_targets(1) %>% #aiming for 100% to maximize all values
        add_min_shortfall_objective(budget=cap)%>% #will try to get the average feature representation to 100%, but might poorly represent some that are in high cost areas
        add_binary_decisions()%>%#binary selection
        add_default_solver(gap = 0, verbose = FALSE)
      
      s_ebd <- solve(p_ebd)
      
      selection <- s_ebd$id[s_ebd$solution_1==1]
      
      selections_by_budget[[a]] <- selection
    }
  
  names(selections_by_budget) <- as.character(budget)
  
  species_list_ebd[[c]] <- selections_by_budget
  
}

names(species_list_ebd) <- species

save(species_list_ebd, file = "data_outputs/ebd_selections_species_GS.RData")


# end
