#################################################
##############     MODAL SHIFT     ##############
##############  linear regression  ##############
#################################################

# Files needed :
  # EMP_drivers.xlsx
  # EMP_walkers.xlsx
  # 0_Functions.R

  # HIA_modal_shift_100replicate.rds : HIA outcomes of 100 replications for each scenario of modal shifts
  # modalshift_tot_km.rds : Total km walked with IC per scenario
  # modalshift_drive_time.rds : Drive time per scenario


# Files outputted :
  # HIA_modal_shift_100replicate.xlsx : HIA for each scenario of modal shift
  # plot_modalshift_cases_prevented.png : Total of prevented cases depending on different scenarios of modal shift
  # plot_modalshift_daly_prevented.png : DALY prevented
  # plot_modalshift_costs_saved.png : Saved medical costs
  # plot_modalshift_soc_costs_saved.png : : Saved intangible costs
  # plot_modalshift_morbidity_prevented.png : Chronic diseases prevented
  # plot_modalshift_mortality_prevented.png : Premature deaths prevented
  # plot_modalshift_morbi_mortality_prevented.png : Combined morbi-mortality graph
  # modalshift_tot_km_CO2_emit.xlsx : Total km walked per scenario with IC and CO2 emissions prevented per scenario with IC
  # modalshift_1km_value.xlsx : Economic value of 1 km walked per scenario
  # modalshift_1€_km_duration.xlsx : Distance and duration to save 1€ per scenario


################################################################################################################################
#                                                    1. LOAD PACKAGES                                                          #
################################################################################################################################
pacman :: p_load(
  rio,          # Data importation
  here,         # Localization of files 
  dplyr,        # Data management
  srvyr,        # Survey
  tidyr,        # Table - Data organization, extraction
  tidyverse,    # Data manipulation and visualization
  ggplot2,      # Plotting
  patchwork,    # Plots combining
  viridis       # Color palette
)



################################################################################################################################
#                                                     2. IMPORT DATA                                                           #
################################################################################################################################

# Import drivers dataset
emp_drive <- import(here("data_clean", "EMP_drivers.xlsx"))

#Import walkers dataset
emp_walk <- import(here("data_clean", "EMP_walkers.xlsx"))

# Import functions
source(here("0_Functions.R"))


################################################################################################################################
#                                                      3. PARAMETERS                                                           #
################################################################################################################################

# Import parameters
source(here("0_Parameters.R"))

# Diseases considered
dis_vec = c("cc", "dem", "bc", "cvd", "diab2", "mort")

# Driven distances shifted to walked (km)
dist_vec <- c(0.5, 1, 1.5, 2)

# Percentages of car trips shifted to walking
perc_vec <- c(0.1, 0.2, 0.3, 0.4, 0.5)




################################################################################################################################
#                                              4. HEALTH IMPACT ASSESSMENT                                                     #
################################################################################################################################

set.seed(123)

burden_tot <- data.frame()
for(dist in dist_vec){
  print(paste0("Distance = ", dist))
  drivers_dist <- emp_drive %>% 
    filter(!is.na(mdisttot_fin1) & mdisttot_fin1 <= dist)                             # Select the drivers under this distance
  
  burden_perc <- data.frame()
  for (perc in perc_vec){
    print(paste0("Share = ", perc))
    
    N=100                                                                               # N random samples of drivers of different shares
    burden_run <- data.frame()
    for(i in 1:N) {
      print(paste0("Run = ", i))
      sample_drivers <- drivers_dist %>% 
        slice_sample(prop = perc) %>% 
        rename(week_time = week_time_shift) %>% 
        mutate(run = i, percentage = perc, distance = dist)
      
      
      burden_dis <- data.frame()
      for(dis in dis_vec){
      burden_i <- burden_prevented_replicate(sample_drivers, dis, N=1, NULL) %>%        # HIA outcomes for 1 disease
        mutate(run = i,
               percentage = perc,
               distance = dist)
      burden_dis <- bind_rows(burden_dis, burden_i)                                     # HIA for all diseases (for 1 run)
      }
      burden_run <- bind_rows(burden_run, burden_dis)                                   # HIA for all runs (for 1 percentage)
    }
    burden_perc <- bind_rows(burden_perc, burden_run)                                   # HIA for all percentages (for 1 distance)
  }
  burden_tot <- bind_rows(burden_tot, burden_perc)                                      # HIA for all distances
}


# Export HIA outcomes of 100 replications for each scenario of modal shifts
export(burden_tot, here("output", "RDS", "HIA_modal_shift_100replicate.rds"))





##############################################################
#                          HIA FOR...                        #
##############################################################

# Load HIA outcomes of 100 replications for each scenario of modal shifts
burden_tot <- import(here("output", "RDS", "HIA_modal_shift_100replicate.rds"))


# GLOBAL
global_shift <- burden_tot %>% 
  group_by(distance, percentage) %>% 
  summarise(tot_cases = sum(tot_cases),
            tot_cases_se = sum(tot_cases_se),
            tot_daly = sum(tot_daly),
            tot_daly_se = sum(tot_daly_se),
            tot_medic_costs = sum(tot_medic_costs) * 1e-6,                                # in million €
            tot_medic_costs_se = sum(tot_medic_costs_se) * 1e-6,
            tot_soc_costs = sum(tot_daly * vsl * 1e-6),
            tot_soc_costs_se = sum(tot_daly_se * vsl * 1e-6))

  # Export HIA for each scenario
export(global_shift, here("output", "Tables", "Linear", "Modal shift", "HIA_modal_shift_100replicate.xlsx"))

  # IC per scenario
set.seed(123)
global_shift_IC <- data.frame()

for (dist in dist_vec) {
  for (perc in perc_vec) {
    scenario <- global_shift %>% 
      filter(distance == dist & percentage == perc)
    cases_IC <- as.data.frame(t(calc_replicate_IC(scenario, "tot_cases"))) %>% 
      rename(cases_low = "2.5%", cases_mean = "50%", cases_sup = "97.5%") %>% 
      mutate(distance = dist, percentage = perc)
    
    daly_IC <- as.data.frame(t(calc_replicate_IC(scenario, "tot_daly"))) %>% 
      rename(daly_low = "2.5%", daly_mean = "50%", daly_sup = "97.5%") %>% 
      mutate(distance = dist, percentage = perc)
    
    medic_costs_IC <- as.data.frame(t(calc_replicate_IC(scenario, "tot_medic_costs"))) %>% 
      rename(medic_costs_low = "2.5%", medic_costs_mean = "50%", medic_costs_sup = "97.5%") %>% 
      mutate(distance = dist, percentage = perc)
    
    scenario_IC <- left_join(cases_IC, daly_IC, by = c("distance", "percentage"))
    scenario_IC <- left_join(scenario_IC, medic_costs_IC, by = c("distance", "percentage"))
    global_shift_IC <- bind_rows(global_shift_IC, scenario_IC)
  }
}




# EACH DISEASE + MORTALITY
for (dis in dis_vec) {
  dis_shift <- burden_tot %>% 
  filter(disease == dis) %>% 
  group_by(distance, percentage) %>% 
  summarise(tot_cases = sum(tot_cases),
            tot_cases_se = sum(tot_cases_se),
            tot_daly = sum(tot_daly),
            tot_daly_se = sum(tot_daly_se),
            tot_medic_costs = sum(tot_medic_costs) / 1e6,
            tot_medic_costs_se = sum(tot_medic_costs_se) / 1e6)
  assign(paste0(dis, "_shift"), dis_shift)
}


# MORBIDITY
morbidity_shift <- burden_tot %>% 
  filter(disease != "mort") %>% 
  group_by(distance, percentage) %>% 
  summarise(tot_cases = sum(tot_cases),
            tot_cases_se = sum(tot_cases_se),
            tot_daly = sum(tot_daly),
            tot_daly_se = sum(tot_daly_se),
            tot_medic_costs = sum(tot_medic_costs) / 1e6,
            tot_medic_costs_se = sum(tot_medic_costs_se) / 1e6)



################################################################################################################################
#                                                       5. HEAT MAPS                                                           #
################################################################################################################################

##############################################################
#                            GLOBAL                          #
##############################################################
# All cases prevented per scenario
global_shift_cases <- ggplot(data = global_shift) +
  geom_tile(aes(x = distance, y = percentage*100, fill = tot_cases),
            color = "white") +
  scale_fill_viridis() +
  labs(x = "Distances of car trips shifted (km)",
       y = "Share shifted (%)", 
       title = "Prevented morbi-mortality cases depending on different scenarios of car trips shifted to walk trips",
       fill = "Number of cases") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.height = grid::unit(1, "cm"),
        legend.key.width = grid::unit(0.6, "cm"),
        
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(vjust = 0.2),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12, face = "bold"),
        
        plot.title = element_text(hjust = 0, size = 14, face = "bold")) +
  theme_minimal()
global_shift_cases


# DALY prevented per scenario

global_shift_daly <- ggplot(data = global_shift) +
  geom_tile(aes(x = distance, y = percentage*100, fill = tot_daly),
            color = "white") +
  scale_fill_viridis() +
  labs(x = "Distances of car trips shifted (km)",
       y = "Share shifted (%)", 
       title = "DALY prevented depending on different scenarios of car trips shifted to walk trips",
       fill = "Number of years") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.height = grid::unit(1, "cm"),
        legend.key.width = grid::unit(0.6, "cm"),
        
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(vjust = 0.2),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12, face = "bold"),
        
        plot.title = element_text(hjust = 0, size = 14, face = "bold")) +
  theme_minimal()
global_shift_daly



# Medical costs saved per scenario
global_shift_costs <- ggplot(data = global_shift) +
  geom_tile(aes(x = distance, y = percentage*100, fill = tot_medic_costs/1e3),                    # in billion €
            color = "white") +
  scale_fill_viridis() +
  labs(x = "Distances of car trips shifted (km)",
       y = "Share shifted (%)", 
       title = "Direct medical (tangible) costs savings depending on different scenarios of car trips shifted to walk trips",
       fill = "Costs (in billion €)")+
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.height = grid::unit(1, "cm"),
        legend.key.width = grid::unit(0.6, "cm"),
        
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(vjust = 0.2),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12, face = "bold"),
        
        plot.title = element_text(hjust = 0, size = 14, face = "bold")) +
  theme_minimal()
global_shift_costs


# Intangible costs saved per scenario
global_shift_soc_costs <- ggplot(data = global_shift) +
  geom_tile(aes(x = distance, y = percentage*100, fill = tot_soc_costs/1e3),                    # in billion €
            color = "white") +
  scale_fill_viridis() +
  labs(x = "Distances of car trips shifted (km)",
       y = "Share shifted (%)", 
       title = "Intangible costs savings depending on different scenarios of car trips shifted to walk trips",
       fill = "Costs (in billion €)")+
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.height = grid::unit(1, "cm"),
        legend.key.width = grid::unit(0.6, "cm"),
        
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(vjust = 0.2),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12, face = "bold"),
        
        plot.title = element_text(hjust = 0, size = 14, face = "bold")) +
  theme_minimal()
global_shift_soc_costs


# Export plots
ggsave(here("output", "Plots", "Linear", "Modal shift", "plot_modalshift_cases_prevented.png"), plot = global_shift_cases)
ggsave(here("output", "Plots", "Linear", "Modal shift", "plot_modalshift_daly_prevented.png"), plot = global_shift_daly)
ggsave(here("output", "Plots", "Linear", "Modal shift", "plot_modalshift_costs_saved.png"),plot = global_shift_costs)
ggsave(here("output", "Plots", "Linear", "Modal shift", "plot_modalshift_soc_costs_saved.png"),plot = global_shift_soc_costs)




##############################################################
#                        MORBIDITY                           #
##############################################################

# Chronic diseases prevented per scenario
morbidity_shift_cases <- ggplot(data = morbidity_shift) +
  geom_tile(aes(x = distance, y = percentage*100, fill = tot_cases)) +
  scale_fill_viridis() +
  labs(x = "Distances of car trips shifted (km)",
       y = "Share shifted (%)", 
       title = "Chronic diseases prevented",
       fill = "Number of cases") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.height = grid::unit(1, "cm"),
        legend.key.width = grid::unit(0.6, "cm"),
        
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(vjust = 0.2),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12, face = "bold"),
        
        plot.title = element_text(hjust = 0, size = 14, face = "bold")) +
  theme_minimal()
morbidity_shift_cases


# Export plot
ggsave(here("output", "Plots", "Linear", "Modal shift", "plot_modalshift_morbidity_prevented.png"), plot = morbidity_shift_cases)



##############################################################
#                        MORTALITY                           #
##############################################################

# Premature deaths prevented per scenario
mortality_shift_cases <- ggplot(data = mort_shift) +
  geom_tile(aes(x = distance, y = percentage*100, fill = tot_cases)) +
  scale_fill_viridis() +
  labs(x = "Distances of car trips shifted (km)",
       y = "Share shifted (%)", 
       title = "Premature deaths prevented",
       fill = "Number of cases") +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.height = grid::unit(1, "cm"),
        legend.key.width = grid::unit(0.6, "cm"),
        
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(vjust = 0.2),
        axis.ticks = element_line(linewidth = 0.4),
        axis.title = element_text(size = 12, face = "bold"),
        
        plot.title = element_text(hjust = 0, size = 14, face = "bold")) +
  theme_minimal()
mortality_shift_cases


# Export plot
ggsave(here("output", "Plots", "Linear", "Modal shift", "plot_modalshift_mortality_prevented.png"), plot = mortality_shift_cases)


##############################################################
#                     MORBI-MORTALITY                        #
##############################################################

# List of the morbidity and mortality graphs
morbi_mortality_shift_graph <- list(morbidity_shift_cases, mortality_shift_cases)

# Theme for the common graph
common_theme <- theme(
  axis.title = element_text(size = 10, face = "bold"),
  strip.text = element_text(size = 8),
  axis.text.y = element_text(size = 9),
  legend.key.height = grid::unit(1, "cm"),
  legend.key.width = grid::unit(0.6, "cm")
)


# Apply the common theme to each graph
morbi_mortality_shift_graph <- lapply(morbi_mortality_shift_graph, 
                                      function(p) p + theme_minimal() + common_theme +
                                        scale_fill_gradient2(limits = c(0, max(morbidity_shift[["tot_cases"]])),          # Caliber a common scale
                                                             low = "#440154FF",
                                                             mid = "#1F968BFF",
                                                             high = "#FDE725FF",
                                                             midpoint = mean(c(0, max(morbidity_shift[["tot_cases"]]))) ) +
                                        theme(legend.position = "none")
                                      )

# Make the legend appear once
morbi_mortality_shift_graph[[2]] <- morbi_mortality_shift_graph[[2]] + theme(legend.position = "right")

# Combine the graphs into one single figure
morbi_mortality_shift_cases <- wrap_plots(morbi_mortality_shift_graph, ncol = 2)

morbi_mortality_shift_cases


  # Export plot
ggsave(here("output", "Plots", "Linear", "Modal shift", "plot_modalshift_morbi_mortality_prevented.png"), plot = morbi_mortality_shift_cases)





################################################################################################################################
#                                                       6. DESCRIPTION                                                         #
################################################################################################################################


##############################################################
#                       DISTANCE DRIVEN                      #
##############################################################

# Total distance driven of all drivers for each scenario distance per year (in km)
km_driven <- data.frame()
for(dist in dist_vec) {
  drivers_dist <- emp_drive %>% 
    filter(!is.na(mdisttot_fin1) & mdisttot_fin1 <= dist)
  km_driven_dist <- drivers_dist %>% 
    filter(pond_jour != "NA") %>% 
    as_survey_design(ids = ident_ind, weights = pond_jour) %>% 
    summarise(tot_km = survey_total(mdisttot_fin1, na.rm = T) * 365.25 / 7,
              tot_mean = survey_mean(mdisttot_fin1, na.rm = T)) %>% 
    mutate(distance = dist)
  
  km_driven <- bind_rows(km_driven, km_driven_dist)
}





##############################################################
#      REPLICATIONS - DISTANCE SHIFTED & CO2 EMISSIONS       #
##############################################################

# Total km walked per scenario with IC and CO2 emissions prevented per scenario with IC per year
set.seed(123)
N=100
tot_km_CO2 <- data.frame()
tot_km_scenario <- data.frame()

for (dist in dist_vec) {
  print(paste0("Distance = ", dist))
  drivers_dist <- emp_drive %>% 
    filter(!is.na(mdisttot_fin1) & mdisttot_fin1 <= dist)                             # Select the drivers under this distance
  
  for(perc in perc_vec) {
    print(paste0("Share = ", perc))
    
    tot_km_drivers <- data.frame()                                                    # 1 dataframe per scenario

    for(i in 1:N) {
      print(i)
      tot_sample <- drivers_dist %>%                                                  # Distances shifted for a year for 1 scenario
        filter(pond_jour != "NA") %>% 
        slice_sample(prop = perc) %>% 
        as_survey_design(ids= ident_ind, weights = pond_jour) %>% 
        summarise(tot_km = survey_total(mdisttot_fin1, na.rm = T)*365.25/7)
        
      tot_km_drivers <- bind_rows(tot_km_drivers, tot_sample)
    }
    
    IC_km <- calc_replicate_IC(tot_km_drivers, "tot_km") / 1e6                                       # in million km
    tot_km_IC <- paste0(round(IC_km["50%"], 3), " (", round(IC_km["2.5%"], 3), " - ", round(IC_km["97.5%"], 3), ")")
    
    IC_km_Rubin <- calc_IC_Rubin (tot_km_drivers, "tot_km") / 1e6                                    # Rubin's rule
    tot_km_IC_Rubin <- paste0(round(IC_km_Rubin[2], 3), " (", round(IC_km_Rubin[1], 3), " - ", round(IC_km_Rubin[3], 3), ")")
    
    
    IC_mt <- IC_km * CO2_emit *1e-6                                                                  # CO2 emissions (in Mt CO2)
    tot_mt_IC <- paste0(round(IC_mt["50%"], 3), " (", round(IC_mt["2.5%"], 3), " - ", round(IC_mt["97.5%"], 3), ")")
    
    IC_mt_Rubin <- IC_km_Rubin* CO2_emit * 1e-6                                                      # Rubin's rule
    tot_mt_IC_Rubin <- paste0(round(IC_mt_Rubin[2], 3), " (", round(IC_mt_Rubin[1], 3), " - ", round(IC_mt_Rubin[3], 3), ")")
    
    
    tot_km_scenario <- bind_rows(tot_km_scenario, data.frame(
      distance = dist,
      percentage = perc,
      km = IC_km[["50%"]],
      km_low = IC_km[["2.5%"]],
      km_sup = IC_km[["97.5%"]], 
      Rubin_km = IC_km_Rubin[2],
      Rubin_km_low = IC_km_Rubin[1],
      Rubin_km_sup = IC_km_Rubin[3]))
    
    tot_km_CO2 <- bind_rows(tot_km_CO2, data.frame(
      distance = dist,
      percentage = paste0(perc*100, "%"),
      total_millions_km = tot_km_IC,
      Rubin_total_millions_km = tot_km_IC_Rubin,
      CO2_emissions_Mt = tot_mt_IC,
      Rubin_CO2_emissions_Mt = tot_mt_IC_Rubin))
    
  }
}


# Export replications - Total km walked per scenario
export(tot_km_scenario, here("output", "RDS", "modalshift_tot_km.rds"))

# Export replications - Total km walked per scenario with IC and CO2 emissions prevented per scenario with IC
export(tot_km_CO2, here("output", "Tables", "Linear", "Modal shift", "modalshift_tot_km_CO2_emit.xlsx"))







##############################################################
#                     TIME LOST BY WALKING                   #  TO BE CONTINUED
##############################################################

# Calculate time spent walking per scenario
walk_time_scenario <- data.frame(
  distance = tot_km_scenario[["distance"]],
  percentage = tot_km_scenario[["percentage"]],
  walk_time = tot_km_scenario[["km"]] / walk_speed,                                    # in hours
  walk_time_low = tot_km_scenario[["km_low"]] / walk_speed,
  walk_time_sup = tot_km_scenario[["km_sup"]] / walk_speed,
  Rubin_walk_time = tot_km_scenario[["Rubin_km"]] / walk_speed,
  Rubin_walk_time_low = tot_km_scenario[["Rubin_km_low"]] / walk_speed,
  Rubin_walk_time_sup = tot_km_scenario[["Rubin_km_sup"]] / walk_speed)




## Calculate time spent driving per scenario ---- TO BE CONTINUED
set.seed(123)
N=100

drive_time_scenario <- data.frame()

for (dist in dist_vec) {
  print(paste0("Distance = ", dist))
  drivers_dist <- emp_drive %>% 
    filter(!is.na(mdisttot_fin1) & mdisttot_fin1 <= dist)                              # Select the drivers under this distance
  
  for(perc in perc_vec) {
    print(paste0("Share = ", perc))
    
    tot_time_drivers <- data.frame()                                                   # 1 dataframe per scenario
    for(i in 1:N) {
      print(i)
      time_sample <- drivers_dist %>%                                                  # Time driven for a year (hours)
        filter(pond_jour != "NA") %>% 
        slice_sample(prop = perc) %>% 
        as_survey_design(ids= ident_ind, weights = pond_jour) %>% 
        summarise(tot_drive_time = survey_total(week_time_shift, na.rm = T)*365.25/(7*7*60))
      
      tot_time_drivers <- bind_rows(tot_time_drivers, time_sample)
    }
    
    IC_time <- calc_replicate_IC(tot_time_drivers, "tot_drive_time")                                             
    
    IC_time_Rubin <- calc_IC_Rubin (tot_time_drivers, "tot_drive_time")                                          # Rubin's rule
    
    drive_time_scenario <- bind_rows(drive_time_scenario, data.frame(                                            # in hours
      distance = dist, 
      percentage = perc,
      drive_time = IC_time[["50%"]],
      drive_time_low = IC_time[["2.5%"]],
      drive_time_sup = IC_time[["97.5%"]],
      Rubin_drive_time = IC_time_Rubin[2],
      Rubin_drive_time_low = IC_time_Rubin[1],
      Rubin_drive_time_sup = IC_time_Rubin[3]
    ))
  }
}
  # Export : time spent driving per scenario
export(drive_time_scenario, here("output", "RDS", "modalshift_drive_time.rds"))



  # Load time spent driving per scenario ----
drive_time_scenario <-  import(here("output", "RDS", "modalshift_drive_time.rds"))

# Calculate time lost by walking 
time_scenario <- left_join(walk_time_scenario, drive_time_scenario, by = c("distance", "percentage")) %>% 
  mutate(time_lost = walk_time - drive_time,
         time_lost_low = walk_time_low - drive_time_low,
         time_lost_sup = walk_time_sup - drive_time_sup,
         Rubin_time_lost = Rubin_walk_time - Rubin_drive_time,
         Rubin_time_lost_low = Rubin_walk_time_low - Rubin_drive_time_low,
         Rubin_time_lost_sup = Rubin_walk_time_sup - Rubin_drive_time_sup)





##############################################################
#                LIFE TIME GAINED BY WALKING                 #
##############################################################
# in days
time_scenario <- time_scenario %>% 
  mutate(time_saved = )

## Net time gained






##############################################################
#                  ECONOMIC UNIT VALUE (€)                   #
##############################################################


# Calculate economic value of 1 km walked per scenario
set.seed(123)
unit_value_scenario <- data.frame()

for(dist in dist_vec) {
  for (perc in perc_vec) {
    scenario_km <- tot_km_scenario %>%                                                 # Set parameters
      filter(distance == dist & percentage == perc)
    km <- scenario_km [["km"]]
    km_low <- scenario_km [["km_low"]]
    km_sup <- scenario_km [["km_sup"]]
    
    scenario_medic_costs <- global_shift_IC %>% 
      filter(distance == dist & percentage == perc)
    euro <- scenario_medic_costs [["medic_costs_mean"]]
    euro_low <- scenario_medic_costs [["medic_costs_low"]]
    euro_sup <- scenario_medic_costs [["medic_costs_sup"]]
    
    unit <- unit_value(km, km_low, km_sup, euro, euro_low, euro_sup, N=1000)
    unit_scenario <- as.data.frame(t(quantile(unit, probs = c(0.025, 0.5, 0.975)))) %>% 
      mutate(distance = dist, percentage = perc)
    
    unit_value_scenario <- bind_rows(unit_value_scenario, unit_scenario)
  }
}

  # Export : economic value of 1 km walked per scenario
export(unit_value_scenario, here("output", "Tables", "Linear", "Modal shift", "modalshift_1km_value.xlsx"))



# Calculate distance walked to save 1€ (km)
set.seed(123)
euro_unit_scenario <- data.frame()

for(dist in dist_vec) {
  for (perc in perc_vec) {
    scenario_km <- tot_km_scenario %>%                                             # Set parameters
      filter(distance == dist & percentage == perc)
    km <- scenario_km [["km"]]
    km_low <- scenario_km [["km_low"]]
    km_sup <- scenario_km [["km_sup"]]
    
    scenario_medic_costs <- global_shift_IC %>% 
      filter(distance == dist & percentage == perc)
    euro <- scenario_medic_costs [["medic_costs_mean"]]
    euro_low <- scenario_medic_costs [["medic_costs_low"]]
    euro_sup <- scenario_medic_costs [["medic_costs_sup"]]
    
    euro <- euro_unit(km, km_low, km_sup, euro, euro_low, euro_sup, N = 1000)
    euro_scenario <- as.data.frame(t(quantile(euro, probs = c(0.025, 0.5, 0.975)))) %>% 
      mutate(distance = dist, percentage = perc)
    
    euro_unit_scenario <- bind_rows(euro_unit_scenario, euro_scenario)
  }
}


# Calculate duration walked to save 1€ (min)
euro_unit_duration_scenario <- euro_unit_scenario %>% 
  mutate(
    min_2.5 = `2.5%` * 60 / walk_speed,
    min_50 = `50%` * 60 / walk_speed,
    min_97.5 = `97.5%` * 60 / walk_speed
  ) %>% 
  rename(km_2.5 = "2.5%",
         km_50 = "50%",
         km_97.5 = "97.5%") %>% 
  mutate(euro = 1)


# Export : Calculate distance and duration to save 1€ per scenario
export(euro_unit_duration_scenario, here("output", "Tables", "Linear", "Modal shift", "modalshift_1€_km_duration.xlsx"))









