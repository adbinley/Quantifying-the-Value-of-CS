#GS 3 - results

# summary vis

#tnc
load("data_outputs/results/tnc_species_summary_GS.RData")
species_tnc_summary <- species_values_list
#tnc without penalty
load("data_outputs/results/free_tnc_species_summary_GS.RData")
species_free_tnc_summary <- species_values_list
#ebd
load("data_outputs/results/ebd_species_summary_GS.RData")
species_ebd_summary <- species_values_list
#GS with penalty
load("data_outputs/results/both_penalty_species_summary_GS.RData")
species_GS_summary <- species_values_list
#GS wihtout penalty
load("data_outputs/results/free_both_species_summary_GS.RData")
species_free_GS_summary <- species_values_list

rm(species_values_list)

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

plot_data_list <- list()

for (p in 1:length(species)){
  
  spec <- species[[p]]
  
  ebd_summary1 <- species_ebd_summary[[p]]
  tnc_summary1 <- species_tnc_summary[[p]]
  tnc_free_summary1 <- species_free_tnc_summary[[p]]
  GS_summary1 <- species_GS_summary[[p]]
  GS_free_summary1 <- species_free_GS_summary[[p]]
  # null_summary1 <- species_null_summary[[p]]
  
  ebd_summary1$model <- rep("eBird", length(ebd_summary1$budget))
  tnc_summary1$model <- rep("TNC_penalty", length(tnc_summary1$budget))
  tnc_free_summary1$model <- rep("TNC_free", length(tnc_free_summary1$budget))
  GS_summary1$model <- rep("GS_penalty", length(GS_summary1$budget))
  GS_free_summary1$model <- rep("GS_free", length(GS_free_summary1$budget))
  # null_summary1$model <- rep("Null", length(null_summary1$budget))
  
  ebd_summary1$species <- rep(spec, length(ebd_summary1$budget))
  tnc_summary1$species <- rep(spec, length(tnc_summary1$budget))
  tnc_free_summary1$species <- rep(spec, length(tnc_free_summary1$budget))
  GS_summary1$species <- rep(spec, length(GS_summary1$budget))
  GS_free_summary1$species <- rep(spec, length(GS_free_summary1$budget))
  # null_summary1$species <- rep(spec, length(null_summary1$budget))
  

  plot_data <- rbind(ebd_summary1,tnc_summary1,tnc_free_summary1,GS_summary1,GS_free_summary1) #null_summary1)
  
  plot_data_list[[p]] <- plot_data
  
}


plot_data_species <- do.call(rbind.data.frame, plot_data_list)
save(plot_data_species, file = "data_outputs/results/figures/plot_data_species_GS.RData")


p <- ggplot(plot_data_species, aes(budget,mean, color = model))+
  geom_ribbon(aes(ymin = mean-stdev, ymax=mean+stdev, fill=model), alpha=0.5) +
  geom_line(aes(y = mean, color=model))+
  theme_classic()+
  xlab("Budget")+
  ylab("Value")+
  facet_wrap(~species)
p

p <- ggplot(plot_data_species, aes(budget,mean, color = model))+
  # geom_ribbon(aes(ymin = mean-stdev, ymax=mean+stdev, fill=model), alpha=0.5) +
  geom_line(aes(y = mean, color=model))+
  theme_classic()+
  xlab("Budget")+
  ylab("Value")+
  facet_wrap(~species)
p

# png("species_result_plot_GS.png")
# p
# dev.off()



#### contrast data ####

load("data_outputs/results/tnc_species_reps_GS.RData")
species_tnc_reps <- species_values_reps
load("data_outputs/results/free_tnc_species_reps_GS.RData")
species_free_tnc_reps <- species_values_reps
load("data_outputs/results/ebd_species_reps_GS.RData")
species_ebd_reps <- species_values_reps
load("data_outputs/results/free_both_species_reps_GS.RData")
species_GS_free_reps <- species_values_reps
load("data_outputs/results/both_penalty_species_reps_GS.RData")
species_GS_penalty_reps <- species_values_reps

rm(species_values_reps)

species <- c("Avocet","Dowitcher","Dunlin","LBCurlew","WSandpiper","Yellowlegs","LSandpiper")

contrast_data_list <- list()

for (p in 1:length(species)){
  
  spec <- species[[p]]
  
  ebd_reps1 <- species_ebd_reps[[p]]
  tnc_reps1 <- species_tnc_reps[[p]]
  tnc_free_reps1 <- species_free_tnc_reps[[p]]
  GS_free_reps1 <- species_GS_free_reps[[p]]
  GS_penalty_reps1 <- species_GS_penalty_reps[[p]]
  
  ebd_reps1$model <- rep("eBird", length(ebd_reps1$budget))
  tnc_reps1$model <- rep("TNC_penalty", length(tnc_reps1$budget))
  tnc_free_reps1$model <- rep("TNC_free", length(tnc_free_reps1$budget))
  GS_free_reps1$model <- rep("GS_free", length(GS_free_reps1$budget))
  GS_penalty_reps1$model <- rep("GS_penalty", length(GS_penalty_reps1$budget))
  
  ebd_reps1$species <- rep(spec, length(ebd_reps1$budget))
  tnc_reps1$species <- rep(spec, length(tnc_reps1$budget))
  tnc_free_reps1$species <- rep(spec, length(tnc_free_reps1$budget))
  GS_free_reps1$species <- rep(spec, length(GS_free_reps1$budget))
  GS_penalty_reps1$species <- rep(spec, length(GS_penalty_reps1$budget))

  
  
  contrast_data <- rbind(ebd_reps1,tnc_reps1, tnc_free_reps1, GS_free_reps1,GS_penalty_reps1)
  
  contrast_data_list[[p]] <- contrast_data
  
}

#dataframe
contrast_data_species <- do.call(rbind.data.frame, contrast_data_list)

#main
contrasts_EBD_TNCp <- contrast_data_species %>%
    group_by(species, budget) %>%
    summarise(eBird_TNCp = (value[which(model=="eBird")]-value[which(model=="TNC_penalty")])/value[which(model=="TNC_penalty")]*100,
              eBird_TNCp_absolute = value[which(model=="eBird")]-value[which(model=="TNC_penalty")])

contrasts_GSf_eBird <- contrast_data_species %>%
  group_by(species, budget) %>%
  summarise(GSf_eBird = (value[which(model=="GS_free")]-value[which(model=="eBird")])/value[which(model=="eBird")]*100,
            GSf_eBird_absolute = (value[which(model=="GS_free")]-value[which(model=="eBird")]))

#supp
#if GS had a realistic penalty
contrasts_GSp_eBird <- contrast_data_species %>%
  group_by(species, budget) %>%
  summarise(GSp_eBird = (value[which(model=="GS_penalty")]-value[which(model=="eBird")])/value[which(model=="eBird")]*100,
            GSp_eBird_absolute = (value[which(model=="GS_penalty")]-value[which(model=="eBird")]))


#if TNC was free
contrasts_EBD_TNCf <- contrast_data_species %>%
  group_by(species, budget) %>%
  summarise(eBird_TNCf = (value[which(model=="eBird")]-value[which(model=="TNC_free")])/value[which(model=="TNC_free")]*100,
            eBird_TNCf_absolute = (value[which(model=="eBird")]-value[which(model=="TNC_free")]))


save(contrasts_EBD_TNCp, file = "data_outputs/results/contrasts_by_budget_EBD_TNCp.RData")
save(contrasts_EBD_TNCf, file = "data_outputs/results/contrasts_by_budget_EBD_TNCf.RData")
save(contrasts_GSf_eBird, file = "data_outputs/results/contrasts_by_budget_GSf_EBD.RData")
save(contrasts_GSp_eBird, file = "data_outputs/results/contrasts_by_budget_GSp_EBD.RData")



#### plotting ####

load("data_outputs/results/contrasts_by_budget_EBD_TNCp.RData")
load("data_outputs/results/contrasts_by_budget_EBD_TNCf.RData")
load("data_outputs/results/contrasts_by_budget_GSf_EBD.RData")
load("data_outputs/results/contrasts_by_budget_GSp_EBD.RData")

species_names <- c('Avocet'="Avocet",
                   'Dowitcher'="Dowitcher",
                   'Dunlin'="Dunlin",
                   'LBCurlew'="Long-Billed Curlew",
                   'LSandpiper'="Least Sandpiper",
                   'WSandpiper'="Western Sandpipier",
                   'Yellowlegs'="Yellowlegs")

#### main results ####
library(rethinking)
library(tidyverse)

#EBD-TNCp
contrasts_plot_EBD_TNCp <- contrasts_EBD_TNCp %>%
  group_by(species,budget)%>%
  summarise(mean_ebd_tnc = mean(eBird_TNCp),
            hpdi_l_ebd_tnc = HPDI(eBird_TNCp, prob=0.9)[1],
            hpdi_u_ebd_tnc = HPDI(eBird_TNCp, prob=0.9)[2])

p4 <- ggplot(contrasts_plot_EBD_TNCp, aes(budget,mean_ebd_tnc))+
  geom_ribbon(aes(ymin = hpdi_l_ebd_tnc, ymax=hpdi_u_ebd_tnc), alpha=0.5) +
  geom_line(aes(y = mean_ebd_tnc))+
  theme_classic()+
  #ylim(-15,55)+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        text = element_text(size = 20))+
  geom_hline(yintercept=0, linetype="dashed",col="blue")+
  xlab("Budget (USD)")+
  ylab("Expected Detections Prioritized (%)")+
  facet_wrap(~species,
             labeller = labeller(species = species_names))
p4

png("data_outputs/results/figures/contrast_plot_ebd_tncp.png", height = 12, width = 12, units = "in",res=300)
p4
dev.off()

#for supp, same comparison but absolute difference rather than percent difference

contrasts_plot_EBD_TNCp_abs <- contrasts_EBD_TNCp %>%
  group_by(species,budget)%>%
  summarise(mean_ebd_tnc = mean(eBird_TNCp_absolute),
            hpdi_l_ebd_tnc = HPDI(eBird_TNCp_absolute, prob=0.9)[1],
            hpdi_u_ebd_tnc = HPDI(eBird_TNCp_absolute, prob=0.9)[2])

p4.1 <- ggplot(contrasts_plot_EBD_TNCp_abs, aes(budget,mean_ebd_tnc))+
  geom_ribbon(aes(ymin = hpdi_l_ebd_tnc, ymax=hpdi_u_ebd_tnc), alpha=0.5) +
  geom_line(aes(y = mean_ebd_tnc))+
  theme_classic()+
  #ylim(-15,55)+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        text = element_text(size = 20))+
  geom_hline(yintercept=0, linetype="dashed",col="blue")+
  xlab("Budget (USD)")+
  ylab("Number of Expected Detections Prioritized")+
  facet_wrap(~species,
             labeller = labeller(species = species_names))
p4.1

png("data_outputs/results/figures/contrast_plot_ebd_tncp_abs.png", height = 12, width = 12, units = "in",res=300)
p4.1
dev.off()

#EBD-GSf
contrasts_plot_GSf_EBD <- contrasts_GSf_eBird %>%
  group_by(species,budget)%>%
  summarise(mean_ebd_GS = mean(GSf_eBird),
            hpdi_l_ebd_GS = HPDI(GSf_eBird, prob=0.9)[1],
            hpdi_u_ebd_GS = HPDI(GSf_eBird, prob=0.9)[2])

p5 <- ggplot(contrasts_plot_GSf_EBD, aes(budget,mean_ebd_GS))+
  geom_ribbon(aes(ymin = hpdi_l_ebd_GS, ymax=hpdi_u_ebd_GS), alpha=0.5) +
  geom_line(aes(y = mean_ebd_GS))+
  theme_classic()+
  #ylim(-15,55)+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        text = element_text(size = 20))+
  geom_hline(yintercept=0, linetype="dashed",col="blue")+
  xlab("Budget (USD)")+
  ylab("Expected Detections Prioritized (%)")+
  facet_wrap(~species,
             labeller = labeller(species = species_names))
p5

png("data_outputs/results/figures/contrast_plot_GSf_EBD.png", height = 12, width = 12, units = "in",res=300)
p5
dev.off()

#same but absolute values

contrasts_plot_GSf_EBD_abs <- contrasts_GSf_eBird %>%
  group_by(species,budget)%>%
  summarise(mean_ebd_GS = mean(GSf_eBird_absolute),
            hpdi_l_ebd_GS = HPDI(GSf_eBird_absolute, prob=0.9)[1],
            hpdi_u_ebd_GS = HPDI(GSf_eBird_absolute, prob=0.9)[2])

p5.1 <- ggplot(contrasts_plot_GSf_EBD_abs, aes(budget,mean_ebd_GS))+
  geom_ribbon(aes(ymin = hpdi_l_ebd_GS, ymax=hpdi_u_ebd_GS), alpha=0.5) +
  geom_line(aes(y = mean_ebd_GS))+
  theme_classic()+
  #ylim(-15,55)+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        text = element_text(size = 20))+
  geom_hline(yintercept=0, linetype="dashed",col="blue")+
  xlab("Budget (USD)")+
  ylab("Number of Expected Detections Prioritized")+
  facet_wrap(~species,
             labeller = labeller(species = species_names))
p5.1

png("data_outputs/results/figures/contrast_plot_GSf_EBD_abs.png", height = 12, width = 12, units = "in",res=300)
p5.1
dev.off()

#### supp results ####
contrasts_plot_GSp_EBD <- contrasts_GSp_eBird %>%
  group_by(species,budget)%>%
  summarise(mean_ebd_GS = mean(GSp_eBird),
            hpdi_l_ebd_GS = HPDI(GSp_eBird, prob=0.9)[1],
            hpdi_u_ebd_GS = HPDI(GSp_eBird, prob=0.9)[2])

p6 <- ggplot(contrasts_plot_GSp_EBD, aes(budget,mean_ebd_GS))+
  geom_ribbon(aes(ymin = hpdi_l_ebd_GS, ymax=hpdi_u_ebd_GS), alpha=0.5) +
  geom_line(aes(y = mean_ebd_GS))+
  theme_classic()+
  #ylim(-15,55)+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        text = element_text(size = 20))+
  geom_hline(yintercept=0, linetype="dashed",col="blue")+
  xlab("Budget (USD)")+
  ylab("Expected Detections Prioritized (%)")+
  facet_wrap(~species,
             labeller = labeller(species = species_names))
p6

png("data_outputs/results/figures/contrast_plot_GSp_EBD.png", height = 12, width = 12, units = "in",res=300)
p6
dev.off()

#same but absolute values

contrasts_plot_GSp_EBD_abs <- contrasts_GSp_eBird %>%
  group_by(species,budget)%>%
  summarise(mean_ebd_GS = mean(GSp_eBird_absolute),
            hpdi_l_ebd_GS = HPDI(GSp_eBird_absolute, prob=0.9)[1],
            hpdi_u_ebd_GS = HPDI(GSp_eBird_absolute, prob=0.9)[2])

p6.1 <- ggplot(contrasts_plot_GSp_EBD_abs, aes(budget,mean_ebd_GS))+
  geom_ribbon(aes(ymin = hpdi_l_ebd_GS, ymax=hpdi_u_ebd_GS), alpha=0.5) +
  geom_line(aes(y = mean_ebd_GS))+
  theme_classic()+
  #ylim(-15,55)+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        text = element_text(size = 20))+
  geom_hline(yintercept=0, linetype="dashed",col="blue")+
  xlab("Budget (USD)")+
  ylab("Number of Expected Detections Prioritized")+
  facet_wrap(~species,
             labeller = labeller(species = species_names))
p6.1

png("data_outputs/results/figures/contrast_plot_GSp_EBD_abs.png", height = 12, width = 12, units = "in",res=300)
p6.1
dev.off()

#EBD-TNCf

contrasts_plot_EBD_TNCf <- contrasts_EBD_TNCf %>%
  group_by(species,budget)%>%
  summarise(mean_ebd_tnc = mean(eBird_TNCf),
            hpdi_l_ebd_tnc = HPDI(eBird_TNCf, prob=0.9)[1],
            hpdi_u_ebd_tnc = HPDI(eBird_TNCf, prob=0.9)[2])

p7 <- ggplot(contrasts_plot_EBD_TNCf, aes(budget,mean_ebd_tnc))+
  geom_ribbon(aes(ymin = hpdi_l_ebd_tnc, ymax=hpdi_u_ebd_tnc), alpha=0.5) +
  geom_line(aes(y = mean_ebd_tnc))+
  theme_classic()+
  #ylim(-15,55)+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        text = element_text(size = 20))+
  geom_hline(yintercept=0, linetype="dashed",col="blue")+
  xlab("Budget (USD)")+
  ylab("Expected Detections Prioritized (%)")+
  facet_wrap(~species,
             labeller = labeller(species = species_names))
p7

png("data_outputs/results/figures/contrast_plot_ebd_tncf_abs.png", height = 12, width = 12, units = "in",res=300)
p7
dev.off()


# same but absolute values

contrasts_plot_EBD_TNCf_abs <- contrasts_EBD_TNCf %>%
  group_by(species,budget)%>%
  summarise(mean_ebd_tnc = mean(eBird_TNCf_absolute),
            hpdi_l_ebd_tnc = HPDI(eBird_TNCf_absolute, prob=0.9)[1],
            hpdi_u_ebd_tnc = HPDI(eBird_TNCf_absolute, prob=0.9)[2])

p7.1 <- ggplot(contrasts_plot_EBD_TNCf_abs, aes(budget,mean_ebd_tnc))+
  geom_ribbon(aes(ymin = hpdi_l_ebd_tnc, ymax=hpdi_u_ebd_tnc), alpha=0.5) +
  geom_line(aes(y = mean_ebd_tnc))+
  theme_classic()+
  #ylim(-15,55)+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        text = element_text(size = 20))+
  geom_hline(yintercept=0, linetype="dashed",col="blue")+
  xlab("Budget (USD)")+
  ylab("Number of Expected Detections Prioritized")+
  facet_wrap(~species,
             labeller = labeller(species = species_names))
p7.1

png("data_outputs/results/figures/contrast_plot_ebd_tncf_abs.png", height = 12, width = 12, units = "in",res=300)
p7.1
dev.off()
