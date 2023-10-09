library(tidyverse)
library(prioritizr)
library(scales)

# at budget of 400k, prioritizations using eBird data yield a biodiversity value of XX
# how much more would need to be spent if we used TNC data instead?


#### 1-prioritizations ####
# running prioritizations using TNC estimates at range of 400k to 1200k usd

#### TNC with penalty ####
load("data_outputs/tnc_prioritization_data.RData")

tnc_run <- farm_values_tnc3

names(tnc_run)[names(tnc_run) == 'farm_id'] <- 'id'
names(tnc_run)[names(tnc_run) == 'total_cost'] <- 'cost'

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

budget <- seq(400000,1200000,by = 10000)


species_list_tnc <- list()

for(c in 1:length(species)){
  
  selections_by_budget <- list()
  
  for(a in 1:length(budget)){
    
    cap <- budget[a]-121622 #monitoring penalty
    
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


save(species_list_tnc, file = "data_outputs/tnc_selections_species_GS_box.RData")

#### 2-validation ####

load("data_outputs/both_values_reps_SurveyID.RData")

farm_vals_wide <- farm_values_both3

farm_vals_long <- pivot_longer(farm_values_both3,
                               cols = !c("farm_id", "area","cost_per_area","species","total_cost"),
                               names_to = "replicate",
                               values_to = "value")

#calculate value of selections based on straight model estimates, validated using the integrated model predictions

##### TNC with penalty ####

load("data_outputs/tnc_selections_species_GS_box.RData")

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
  budget <- seq(400000,1200000,by = 10000)
  tnc_values_df1 <- cbind(budget,tnc_values_df)
  tnc_values_df2 <- pivot_longer(tnc_values_df1,
                                 cols = !budget,
                                 names_to = "replicate",
                                 values_to = "value") 
  
  tnc_summary <- tnc_values_df2 %>%
    group_by(budget)%>%
    summarise(mean = mean(value),
              stdev = sd(value))
  
  # budget <- c(0,50000)
  # m <- rep(0,2)
  # s <- rep(0,2)
  # d <- cbind(budget, m,s)%>%
  #   data.frame()
  # names(d) <- c("budget","mean","stdev")
  
  tnc_summary1 <- tnc_summary #rbind(d,tnc_summary)
  
  species_values_list[[c]] <- tnc_summary1 
  species_values_reps[[c]] <- tnc_values_df2
  
}

beep()

names(species_values_list) <- species
names(species_values_reps) <- species

save(species_values_reps, file = "data_outputs/tnc_species_reps_GS_box.RData")
save(species_values_list, file = "data_outputs/tnc_species_summary_GS_box.RData")

#### 3-results ####

#### contrasts approach ####

load("data_outputs/results/ebd_species_reps_GS.RData")
species_ebd_reps <- species_values_reps

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")


ebd_contrast_data_list <- list()

for (p in 1:length(species)){
  
  spec <- species[[p]]
  
  ebd_reps1 <- species_ebd_reps[[p]]
  # tnc_reps1 <- species_tnc_reps[[p]]
  
  ebd_reps1$model <- rep("eBird", length(ebd_reps1$budget))
  # tnc_reps1$model <- rep("TNC_penalty", length(tnc_reps1$budget))
  
  ebd_reps1$species <- rep(spec, length(ebd_reps1$budget))
  # tnc_reps1$species <- rep(spec, length(tnc_reps1$budget))
  
  
  ebd_contrast_data_list[[p]] <- ebd_reps1
  
}

#dataframe
ebd_contrast_data_species <- do.call(rbind.data.frame, ebd_contrast_data_list)%>%
  filter(budget == 400000)

save(ebd_contrast_data_species, file = "data_outputs/results/ebd_contrast_data_species.Rdata")

# tnc

load("data_outputs/tnc_species_reps_GS_box.RData")
species_tnc_reps <- species_values_reps
rm(species_values_reps)

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

tnc_contrast_data_list <- list()

for (p in 1:length(species)){
  
  spec <- species[[p]]
  
  # ebd_reps1 <- species_ebd_reps[[p]]
  tnc_reps1 <- species_tnc_reps[[p]]
  
  # ebd_reps1$model <- rep("eBird", length(ebd_reps1$budget))
  tnc_reps1$model <- rep("TNC_penalty", length(tnc_reps1$budget))
  
  # ebd_reps1$species <- rep(spec, length(ebd_reps1$budget))
  tnc_reps1$species <- rep(spec, length(tnc_reps1$budget))
  
  
  tnc_contrast_data_list[[p]] <- tnc_reps1
  
}

#dataframe
tnc_contrast_data_species <- do.call(rbind.data.frame, tnc_contrast_data_list)

save(tnc_contrast_data_species, file = "data_outputs/results/tnc_contrast_data_species.Rdata")

# contrasts

load("data_outputs/results/tnc_contrast_data_species.Rdata")
load("data_outputs/results/ebd_contrast_data_species.Rdata")

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

budget <- unique(tnc_contrast_data_species$budget)

species_contrast_list <- list()

for(s in 1:length(species)){
  
  spec <- species[s]
  
  tnc_df1 <- tnc_contrast_data_species %>%
    filter(species == spec)
  
  ebd_df1 <- ebd_contrast_data_species %>%
    filter(species == spec)
  
  budget_list <- list()

  for(b in 1:length(budget)){
    
    bud <- budget[b]
    
    tnc_df2 <- tnc_df1 %>%
      filter(budget == bud)
    
    contrast_df <- data.frame(
      contrast_value = ebd_df1$value - tnc_df2$value,
      contrast_percent = ((ebd_df1$value - tnc_df2$value)/tnc_df2$value)*100,
    replicate = tnc_df2$replicate,
    budget = bud
    )
    
    budget_list[[b]] <- contrast_df
  
  }
  
  #names(budget_list) <- budget
  
  budget_df <- bind_rows(budget_list)
  
  species_contrast_list[[s]] <- budget_df

}

names(species_contrast_list) <- species

species_contrast_df <- bind_rows(species_contrast_list, .id = "species")

save(species_contrast_df, file = "data_outputs/results/species_contrast_df.RData")

# data vis

load("data_outputs/results/species_contrast_df.RData")

contrasts_plot_cost_test <- species_contrast_df %>%
  group_by(species,budget)%>%
  summarise(mean_value = mean(contrast_value),
            hpdi_l = HPDI(contrast_value, prob=0.9)[1],
            hpdi_u = HPDI(contrast_value, prob=0.9)[2])

contrasts_plot_cost <- species_contrast_df %>%
  group_by(species,budget)%>%
  summarise(mean_value = mean(contrast_percent),
            hpdi_l = HPDI(contrast_percent, prob=0.9)[1],
            hpdi_u = HPDI(contrast_percent, prob=0.9)[2])

contrasts_plot_cost1 <- contrasts_plot_cost %>%
  group_by(species)%>%
  filter(budget<970000)%>%
  ungroup()

contrasts_plot_cost2 <- contrasts_plot_cost1 %>%
  mutate(cost = budget - 400000)

species_names <- c('Avocet'="Avocet",
                   'Dowitcher'="Dowitcher",
                   'Dunlin'="Dunlin",
                   'LBCurlew'="Long-Billed Curlew",
                   'LSandpiper'="Least Sandpiper",
                   'WSandpiper'="Western Sandpipier",
                   'Yellowlegs'="Yellowlegs")


#plot
cost_plot <- ggplot(contrasts_plot_cost2, aes(cost, mean_value))+
  #geom_point()+
  #geom_errorbar(aes(ymin=hpdi_l, ymax=hpdi_u), width=.2,
  #              position=position_dodge(.9))+
  geom_ribbon(aes(ymin=hpdi_l, ymax=hpdi_u), alpha=0.5)+
  geom_line(aes(y=mean_value))+
  theme_classic()+
  ylim(-15,25)+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        text = element_text(size = 25))+
  geom_hline(yintercept=0, linetype="dashed",col="blue")+
  xlab("Cost (USD)")+
  ylab("Expected Detections Prioritized (%)")+
  scale_x_continuous(labels = scales::comma, limits = c(0,130000))+
  facet_wrap(~species,
             labeller = labeller(species = species_names))
cost_plot

png("data_outputs/results/figures/contrast_plot_costs.png", height = 12, width = 12, units = "in",res=300)
cost_plot
dev.off()

