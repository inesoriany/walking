#################################################
##############     MODAL SHIFT     ##############
##############  linear regression  ##############
#################################################

# Files needed :
  # EMP_drivers.xlsx
  # EMP_walkers.xlsx
  # 0_Functions.R

  # HIA_modal_shift_100replicate.rds : HIA outcomes of 100 replications for each scenario of modal shifts
  # modalshift_tot_km_CO2_emit.rds : Total km walked per scenario with IC and CO2 emissions prevented per scenario with IC
  # modalshift_tot_km_drivers.xlsx : Total km walked with IC per scenario
  


# Files outputted :
  # plot_modalshift_cases_prevented.tiff : Total of prevented cases depending on different scenarios of car trips shifted to walk trips
  # plot_modalshift_costs_saved.tiff : Saved medical costs depending on different scenarios of car trips shifted to walk trips
  # modalshift_tot_km_drivers.xlsx : Total km walked with IC per scenario
  # modalshift_CO2_prevented.xlsx : CO2 emissions prevented with IC per scenario
  # modalshift_unit_value.xlsx : Economic value of 1 km walked per scenario


################################################################################################################################
#                                                    1. LOAD PACKAGES                                                          #
################################################################################################################################
pacman :: p_load(
  rio,          # Data importation
  here,         # Localization of files 
  dplyr,        # Data management
  survey,       # Survey
  tidyr,        # Table - Data organization, extraction
  tidyverse     # Data manipulation and visualization
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
#                                                3. SETTING THE CONSTANTS                                                      #
################################################################################################################################

# Diseases 
dis_vec = c("cc", "dem", "bc", "cvd", "diab2", "mort")

# Driven distances shifted to walked (km)
dist_vec <- c(0.5, 1, 1.5, 2)

# Percentages of car trips shifted to walking
perc_vec <- c(0.1, 0.2, 0.3, 0.4, 0.5)

# CO2 emissions per km driven
CO2_emit <- 124                    # 124g CO2 per km



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
    
    N=100                                                                               # N random samples of drivers
    burden_run <- data.frame()
    for(i in 1:N) {
      print(paste0("Run = ", i))
      sample_drivers <- drivers_dist %>% 
        slice_sample(prop = perc) %>% 
        rename(week_time = week_time_shift)
      
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
            tot_medic_costs = sum(tot_medic_costs) /1e6,                                # in million €
            tot_medic_costs_se = sum(tot_medic_costs_se) / 1e6)

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
  geom_tile(aes(x = distance, y = percentage, fill = tot_cases)) +
  scale_fill_gradient(low = "brown2", 
                      high = "royalblue") +
  labs(x = "Distances of car trips shifted (km)",
       y = "Share shifted", 
       title = "Total of prevented cases depending on different scenarios of car trips shifted to walk trips",
       fill = "Number of all prevented cases")
global_shift_cases


# Medical costs saved per scenario
global_shift_costs <- ggplot(data = global_shift) +
  geom_tile(aes(x = distance, y = percentage, fill = tot_medic_costs)) +
  scale_fill_gradient(low = "brown2", 
                      high = "royalblue") +
  labs(x = "Distances of car trips shifted (km)",
       y = "Share shifted", 
       title = "Saved medical costs depending on different scenarios of car trips shifted to walk trips",
       fill = "Saved medical costs (in million €)")
global_shift_costs


# Export plots
ggsave(here("output", "Plots", "Linear", "Modal shift", "plot_modalshift_cases_prevented.tiff"), plot = global_shift_cases)
ggsave(here("output", "Plots", "Linear", "Modal shift", "plot_modalshift_costs_saved.tiff"),plot = global_shift_costs)




##############################################################
#                 EACH DISEASE + MORTALITY                   #
##############################################################

# Disease cases prevented per scenario
for(dis in dis_vec) {
  dis_shift_cases <- ggplot(data = get(paste0(dis, "_shift"))) +
    geom_tile(aes(x = distance, y = percentage, fill = tot_cases)) +
    scale_fill_gradient(low = "brown2", 
                        high = "royalblue") +
    labs(x = "Distances of car trips shifted",
         y = "Share shifted", 
         title = paste0("Total of ", dis, " prevented depending on different scenarios of car trips shifted to walk trips"),
         fill = "Number of all prevented cases")
  assign(paste0(dis, "_shift_cases"), dis_shift_cases)
  print(get(paste0(dis, "_shift_cases")))
}


# Medical costs saved for each disease per scenario
for(dis in dis_vec) {
  dis_shift_cases <- ggplot(data = get(paste0(dis, "_shift"))) +
    geom_tile(aes(x = distance, y = percentage, fill = tot_medic_costs)) +
    scale_fill_gradient(low = "brown2", 
                        high = "royalblue") +
    labs(x = "Distances of car trips shifted",
         y = "Share shifted", 
         title = paste0("Total of ", dis, " prevented depending on different scenarios of car trips shifted to walk trips"),
         fill = "Number of all prevented cases")
  assign(paste0(dis, "_shift_cases"), dis_shift_cases)
  print(get(paste0(dis, "_shift_cases")))
}





################################################################################################################################
#                                                       6. DESCRIPTION                                                         #
################################################################################################################################

##############################################################
#                       DISTANCE DRIVEN                      #
##############################################################

# Total distance driven of all drivers for each scenario distance (in km)
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
#        REPLICATIONS - DISTANCE SHIFTED & EMISSIONS         #
##############################################################

# Total km walked per scenario with IC and CO2 emissions prevented per scenario with IC
set.seed(123)
N=100
tot_table <- data.frame()

for (dist in dist_vec) {
  print(paste0("Distance = ", dist))
  drivers_dist <- emp_drive %>% 
    filter(!is.na(mdisttot_fin1) & mdisttot_fin1 <= dist)                             # Select the drivers under this distance
  
  for(perc in perc_vec) {
    print(paste0("Share = ", perc))
    
    tot_km_drivers <- data.frame()                                                    # Reset the dataframe 
    for(i in 1:N) {
      print(i)
      tot_sample <- drivers_dist %>% 
        filter(pond_jour != "NA") %>% 
        slice_sample(prop = perc) %>% 
        as_survey_design(ids= ident_ind, weights = pond_jour) %>% 
        summarise(tot_km = survey_total(mdisttot_fin1, na.rm = T)*365.25/7)
        
      tot_km_drivers <- bind_rows(tot_km_drivers, tot_sample)
    }
    IC_km <- calc_replicate_IC(tot_km_drivers, "tot_km") / 1e6                                       # in million km
    tot_km_IC <- paste0(round(IC_km["50%"], 3), " (", round(IC_km["2.5%"], 3), " - ", round(IC_km["97.5%"], 3), ")")
    
    IC_mt <- IC_km * 1e6 * CO2_emit / (1e6*1e6)                                                      # CO2 emissions (in Mt CO2)
    tot_mt_IC <- paste0(round(IC_mt["50%"], 3), " (", round(IC_mt["2.5%"], 3), " - ", round(IC_mt["97.5%"], 3), ")")
    
    tot_table <- bind_rows(tot_table, data.frame(
      distance = dist,
      percentage = paste0(perc*100, "%"),
      total_millions_km = tot_km_IC,
      CO2_emissions_Mt = tot_mt_IC))
  }
}

# Export replications - Total km walked per scenario with IC and CO2 emissions prevented per scenario with IC
export(tot_table, here("output", "RDS", "modalshift_tot_km_CO2_emit.rds"))


##############################################################
#                       DISTANCE SHIFTED                     #
##############################################################

# Import replications
tot_table <- import(here("output", "RDS", "modalshift_tot_km_CO2_emit.rds"))

# Contingency table gathering total km walked with IC per scenario
tot_km_drivers_scenario <- tot_table %>% 
  select(-CO2_emissions_Mt) %>% 
  pivot_wider(names_from = percentage, values_from = total_millions_km)

# Export : Total km walked per scenario with IC
export(tot_km_drivers_scenario, here("output", "Tables", "Linear", "Modal shift", "modalshift_tot_km_drivers.xlsx"))
 


##############################################################
#                          EMISSIONS                         #
##############################################################

# Import replications
tot_table <- import(here("output", "RDS", "modalshift_tot_km_CO2_emit.rds"))

# Contingency table gathering CO2 emissions prevented with IC per scenario
CO2_emissions_scenario <- tot_table %>% 
  select(-total_millions_km) %>% 
  pivot_wider(names_from = percentage, values_from = CO2_emissions_Mt)

# Export : CO2 emissions prevented with IC per scenario
export(CO2_emissions_scenario, here("output", "Tables", "Linear", "Modal shift", "modalshift_CO2_prevented.xlsx"))





##############################################################
#                  ECONOMIC UNIT VALUE (€)                   #
##############################################################

# Import : Total km walked per scenario with IC
tot_km_drivers_scenario <- import(here("output", "Tables", "Linear", "Modal shift", "modalshift_tot_km_drivers.xlsx"))


# Extract km, km_low and km_sup for each scenario
tot_km_drivers_IC <- data.frame()
for(perc in perc_vec){
  tot_km_perc_IC <- tot_km_drivers_scenario %>% 
    extract(paste0(perc*100, "%"), into = c("km", "km_low", "km_sup"),
            regex = "([0-9.]+) \\(([0-9.]+) - ([0-9.]+)\\)",
            convert = TRUE) %>%                                            # Convert in numeric
    mutate(percentage = perc)
  
  tot_km_drivers_IC <- bind_rows(tot_km_drivers_IC, tot_km_perc_IC) %>% 
    select(distance, percentage, km, km_low, km_sup)
}


# Calculate economic value of 1 km walked per scenario
set.seed(123)
unit_value_scenario <- data.frame()

for(dist in dist_vec) {
  for (perc in perc_vec) {
    scenario_km <- tot_km_drivers_IC %>%                                             # Set parameters
      filter(distance == dist & percentage == perc)
    km <- scenario_km %>% 
      pull(km)
    km_low <- scenario_km %>% 
      pull(km_low)
    km_sup <- scenario_km %>% 
      pull(km_sup)
    
    scenario_medic_costs <- global_shift_IC %>% 
      filter(distance == dist & percentage == perc)
    euro <- scenario_medic_costs %>% 
      pull(medic_costs_mean)
    euro_low <- scenario_medic_costs %>% 
      pull(medic_costs_low)
    euro_sup <- scenario_medic_costs %>% 
      pull(medic_costs_sup)
    
    unit <- unit_value(km, km_low, km_sup, euro, euro_low, euro_sup, N=1000)
    unit_scenario <- as.data.frame(t(quantile(unit, probs = c(0.025, 0.5, 0.975)))) %>% 
      mutate(distance = dist, perentage = perc)
    
    unit_value_scenario <- bind_rows(unit_value_scenario, unit_scenario)
  }
}

  # Export :economic value of 1 km walked per scenario
export(unit_value_scenario, here("output", "Tables", "Linear", "Modal shift", "modalshift_unit_value.xlsx"))



