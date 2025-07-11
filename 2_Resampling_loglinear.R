#################################################
############## MULTIPLE IMPUTATION ##############
############  Log linear regression  ############
#################################################

# Files needed :
  # EMP_walkers.xlsx
  # 0_Functions.R

  # logHIA_1000replicate.rds : results of HIA outputs for 1000 replications of all diseases
  # logHIA_1000replicate_age.rds : results of HIA outputs for 1000 replications per age of all diseases 


# Files outputted :
  # HIA_per_disease.xlsx : Prevented cases, DALY and costs per disease (IC from Monte-Carlo distribution)
  # HIA_per_disease_Rubin.xlsx : Prevented cases, DALY and costs per disease (IC from Rubin's rule)
  # HIA_global.xlsx : Prevented cases, DALY and costs for all morbi-mortality events (IC from Monte-Carlo distribution)
  # HIA_global_Rubin.xlsx : Prevented cases, DALY and costs for all morbi-mortality events (IC from Rubin's rule)
  # reduc_mortality_risk.xlsx : Reduction of mortality risk due to walking in 2019 (IC from Monte-Carlo distribution)
  # 1km_value_1000replicate.xlsx : Economic value of 1 km walked (medical costs) (in €)
  # 1km_soc_value_1000replicate.xlsx : Economic value of 1 km walked (intangible costs) (in €)
  # 1€_km_duration_1000replicate.xlsx : Distance and duration walked to save 1€ of medical costs
  # soc_1€_km_duration_1000replicate.xlsx : Distance and duration walked to save 1€ of intangible costs


################################################################################################################################
#                                                    1. LOAD PACKAGES                                                          #
################################################################################################################################

pacman :: p_load(
  rio,          # Data importation
  here,         # Localization of files 
  dplyr,        # Data management
  srvyr,        # Survey
  survey,
  ggplot2       # Data visualization
)


###############################################################################################################################
#                                                     2. IMPORT DATA                                                           #
################################################################################################################################

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



################################################################################################################################
#                                                     4. MONTE-CARLO                                                           #
################################################################################################################################
## N replications for HIA outcomes (based on random RR generated) per age and disease ----
health_replicate <- emp_walk

burden_replicate_age <- data.frame()

set.seed(123)                                                              # Fix a seed to guarantee reproducibility of simulations
for (dis in dis_vec) {
  burden_dis <- log_burden_prevented_replicate (health_replicate, dis, N = 1000, "age_grp.x")           # HIA for all runs of 1 disease
  burden_replicate_age <- bind_rows(burden_replicate_age, burden_dis)                                   # HIA for all diseases
}


  # Export the results of HIA outputs for all replications per age group
export(burden_replicate_age, here("output", "RDS", "Log linear", "2019", "logHIA_1000replicate_per_age.rds"))



## N replications for HIA outputs (based on random RR generated) per disease ----
burden_replicate <- data.frame()

set.seed(123)
for (dis in dis_vec) {
  burden_dis <- log_burden_prevented_replicate (health_replicate, dis, N = 1000, NULL)                    # HIA for all runs of 1 disease
  burden_replicate <- bind_rows(burden_replicate, burden_dis)                                         # HIA for all diseases
}

  # Export the results of HIA outputs for all replications
export(burden_replicate, here("output", "RDS", "Log linear", "2019", "logHIA_1000replicate.rds"))




################################################################################################################################
#                                                   5. INTERVAL OF CONFIDENCE                                                  #
################################################################################################################################

# IMPORT DATA (to avoid re-generating Monte-Carlo replications)
burden_replicate <- import(here("output", "RDS", "Log linear", "2019", "logHIA_1000replicate.rds"))


##############################################################
#                 HIA MORTALITY / MORBIDITY                  #
##############################################################

# HIA for mortality
burden_mortality <- burden_replicate %>% 
  filter(disease == "mort")

mortality <- burden_mortality %>% 
  group_by(run) %>% 
  summarise(tot_cases = sum(tot_cases),
            tot_cases_se = sum(tot_cases_se),
            tot_daly = sum(tot_daly),
            tot_daly_se = sum(tot_daly_se))


# HIA for morbidity
burden_morbidity <- burden_replicate %>% 
  filter(disease != "mort")

morbidity <- burden_morbidity %>% 
  group_by(run) %>% 
  summarise(tot_cases = sum(tot_cases),
            tot_cases_se = sum(tot_cases_se),
            tot_daly = sum(tot_daly),
            tot_daly_se = sum(tot_daly_se),
            tot_medic_costs = sum(tot_medic_costs),
            tot_medic_costs_se = sum(tot_medic_costs_se))



# IC and mean for each outcome for morta-morbidity
  # Mortality
set.seed(123)
calc_replicate_IC(mortality, "tot_cases")                       # Premature deaths prevented (distribution of Monte-Carlo replications)
calc_IC_Rubin(mortality, "tot_cases")                              # Rubin's rule

set.seed(123)
calc_replicate_IC(mortality, "tot_daly")                        # YLL prevented (distribution of Monte-Carlo replications)
calc_IC_Rubin(mortality, "tot_daly")                               # Rubin's rule



  # Morbidity 
set.seed(123)
calc_replicate_IC(morbidity, "tot_cases")                       # Chronic diseases prevented (distribution of Monte-Carlo replications)
calc_IC_Rubin(morbidity, "tot_cases")                              # Rubin's rule

set.seed(123)
calc_replicate_IC(morbidity, "tot_daly")                        # YLD prevented (distribution of Monte-Carlo replications)
calc_IC_Rubin(morbidity, "tot_daly")                               # Rubin's rule

set.seed(123)
calc_replicate_IC(morbidity, "tot_medic_costs")* 1e-6           # Medical costs prevented (in million €)
calc_IC_Rubin(morbidity, "tot_medic_costs")* 1e-6                  # Rubin's rule





##############################################################
#                      HIA PER DISEASE                       #
##############################################################

burden_disease <- burden_replicate %>% 
  group_by(run, disease) %>% 
  summarise(tot_cases = sum(tot_cases),
            tot_cases_se = sum(tot_cases_se),
            tot_daly = sum(tot_daly),
            tot_daly_se = sum(tot_daly_se),
            tot_medic_costs = sum(tot_medic_costs * 1e-6),                     # in million €   
            tot_medic_costs_se = sum(tot_medic_costs_se * 1e-6),
            tot_soc_costs = sum(tot_daly * vsl * 1e-6),
            tot_soc_costs_se = sum(tot_daly_se * vsl * 1e-6))


outcome_vec <- c("tot_cases", "tot_daly", "tot_medic_costs", "tot_soc_costs")



# IC from distribution of Monte-Carlo replications
set.seed(123)
HIA_disease <- HIA_burden_IC(burden_disease, dis_vec, outcome_vec, calc_replicate_IC)


  # Export results 
export(HIA_disease, here("output", "RDS", "Log linear", "2019", "HIA_disease_1000replicate.rds"))


  # OTHER VISUALIZATION
HIA_per_disease <- data.frame(disease = HIA_disease[["disease"]])
for (out in outcome_vec){
  HIA_per_disease <- HIA_per_disease %>% 
    mutate(!!sym(out) := paste0(HIA_disease[[paste0(out)]], " (", HIA_disease[[paste0(out, "_low")]], " - ", HIA_disease[[paste0(out, "_sup")]], " )"))
}

  # Export results 
export(HIA_per_disease, here("output", "Tables", "Log linear", "2019", "HIA_per_disease.xlsx"))




# IC - Rubin's rule
HIA_disease_Rubin <- HIA_burden_IC(burden_disease, dis_vec, outcome_vec, calc_IC_Rubin)

  # OTHER VISUALIZATION
HIA_per_disease_Rubin <- data.frame(disease = HIA_disease_Rubin[["disease"]])
for (out in outcome_vec){
  HIA_per_disease_Rubin <- HIA_per_disease_Rubin %>% 
    mutate(!!sym(out) := paste0(HIA_disease_Rubin[[paste0(out)]], " (", HIA_disease_Rubin[[paste0(out, "_low")]], " - ", HIA_disease_Rubin[[paste0(out, "_sup")]], " )"))
}

  # Export results 
export(HIA_per_disease_Rubin, here("output", "Tables", "Log linear", "2019", "HIA_per_disease_Rubin.xlsx"))



##############################################################
#                         HIA GLOBAL                         #
##############################################################

# Global
burden_global <- burden_replicate %>%
  group_by(run) %>%
  summarise(
    tot_cases = sum(tot_cases, na.rm = TRUE),
    tot_cases_se = sum(tot_cases_se, na.rm = TRUE),
    tot_daly = sum(tot_daly, na.rm = TRUE),
    tot_daly_se = sum(tot_daly_se, na.rm = TRUE),
    tot_medic_costs = sum(tot_medic_costs * 1e-6, na.rm = TRUE),                        # in million € 
    tot_medic_costs_se = sum(tot_medic_costs_se  * 1e-6, na.rm = TRUE),
    tot_soc_costs = sum(tot_daly * vsl * 1e-6),
    tot_soc_costs_se = sum(tot_daly_se * vsl * 1e-6)) %>% 
  mutate(disease = "all")



# IC from distribution of Monte-Carlo replications
set.seed(123)
HIA_global <- HIA_burden_IC(burden_global, "all", outcome_vec, calc_replicate_IC)

  # OTHER VISUALIZATION
HIA_all_disease <- data.frame(disease = HIA_global[["disease"]])
for (out in outcome_vec){
  HIA_all_disease <- HIA_all_disease %>% 
    mutate(!!sym(out) := paste0(HIA_global[[paste0(out)]], " (", HIA_global[[paste0(out, "_low")]], " - ", HIA_global[[paste0(out, "_sup")]], " )"))
}

  # Export results 
export(HIA_all_disease, here("output", "Tables", "Log linear", "2019", "HIA_global.xlsx"))



# IC - Rubin's rule
HIA_global_Rubin <- HIA_burden_IC(burden_global, "all", outcome_vec, calc_IC_Rubin)

  # OTHER VISUALIZATION
HIA_all_disease_Rubin <- data.frame(disease = HIA_global_Rubin[["disease"]])
for (out in outcome_vec){
  HIA_all_disease_Rubin <- HIA_all_disease_Rubin %>% 
    mutate(!!sym(out) := paste0(HIA_all_disease_Rubin[[paste0(out)]], " (", HIA_all_disease_Rubin[[paste0(out, "_low")]], " - ", HIA_all_disease_Rubin[[paste0(out, "_sup")]], " )"))
}

  # Export results 
export(HIA_all_disease_Rubin, here("output", "Tables", "Log linear", "2019", "HIA_global_Rubin.xlsx"))





##############################################################
#               ‰ REDUCTION IN MORTALITY RISK                #
##############################################################
## N replications for HIA outcomes (based on random RR generated) per age for mortality ----

set.seed(101)
N = 1000
reduc_mortality_risk <- data.frame()
for (i in 1:N) {
  print(i)
  burd_mort <- calc_HIA_replicate(health_replicate, "mort")                                        # HIA for death
  burden_mort <- burd_mort %>% 
    as_survey_design(ids = ident_ind, weights = pond_indc) %>% 
    summarise(mean_mort_reduction_risk = survey_mean(mort_reduction_risk, na.rm = T)) %>%          # mean of reduction of mortality risk
    mutate(run = i)
  reduc_mortality_risk <- bind_rows(reduc_mortality_risk, burden_mort)
}
# Export reduction in mortality risk for 1000 replications
export(reduc_mortality_risk, here("output", "RDS", "2019",  "reduc_mortality_risk_1000_rep.RDS"))


# IC
reduc_mortality_risk <- import(here("output", "RDS", "2019",  "reduc_mortality_risk_1000_rep.RDS"))

N = 1000
IC <-  calc_replicate_IC(reduc_mortality_risk, "mean_mort_reduction_risk")
reduc_mortality_risk_IC <- data.frame(
  reduc_mortality_risk = paste0(round(IC["50%"], 3), " (", round(IC["2.5%"], 3), " - ", round(IC["97.5%"],3),  ")"),
  N_replications = N
)

# Export reduction in mortality risk due to walking
export(reduc_mortality_risk_IC, here("output", "Tables", "Log linear", "2019", "reduc_mortality_risk.xlsx"))




################################################################################################################################
#                                                 6. ECONOMIC UNIT VALUE (€)                                                   #
################################################################################################################################

# Survey design ponderated by day
jour <- emp_walk %>% 
  filter(pond_jour != "NA") %>% 
  as_survey_design(ids = ident_ind,
                   weights = pond_jour,
                   strata = c(sexe, age_grp.x),
                   nest = TRUE)

# Total walked distance in 2019
km_total_2019 <- as.numeric(svytotal(~nbkm_walking, jour)) *365.25/7                              # Total km per year
km_total_2019_IC <- confint(svytotal(~nbkm_walking, jour) *365.25/7 )                             # Confidence interval


# Setting parameters 
km_low_2019 <- km_total_2019_IC[1, 1]
km_sup_2019 <- km_total_2019_IC[1, 2]

euro <- HIA_global[["tot_medic_costs"]] [[1]] * 1e6
euro_low <- HIA_global[["tot_medic_costs_low"]] [[1]] * 1e6
euro_sup <- HIA_global[["tot_medic_costs_sup"]] [[1]] * 1e6

soc_euro <- HIA_global[["tot_soc_costs"]] [[1]] * 1e6
soc_euro_low <- HIA_global[["tot_soc_costs_low"]] [[1]] * 1e6
soc_euro_sup <- HIA_global[["tot_soc_costs_sup"]] [[1]] * 1e6



##############################################################
#                        VALUE OF 1km                        #
##############################################################
## MEDICAL COSTS ----

# Calculate economic value of 1 km walked (medical costs) (in €)
set.seed(123)
unit_2019 <- unit_value(km_total_2019, km_low_2019, km_sup_2019, euro, euro_low, euro_sup, N=1000)
unit_value_2019 <- as.data.frame(t(quantile(unit_2019, probs = c(0.025, 0.5, 0.975)))) %>% 
  rename(euro_2.5 = "2.5%",
         euro_50 = "50%",
         euro_97.5 = "97.5%") %>% 
  mutate(km = 1)


# Export : economic value of 1 km walked per scenario
export(unit_value_2019, here("output", "Tables", "Log linear", "2019", "1km_value_1000replicate.xlsx"))



## SOCIAL COSTS ----

# Calculate economic value of 1 km walked (intangible costs) (in €)
set.seed(123)
unit_soc_2019 <- unit_value(km_total_2019, km_low_2019, km_sup_2019, soc_euro, soc_euro_low, soc_euro_sup, N=1000)
unit_soc_value_2019 <- as.data.frame(t(quantile(unit_soc_2019, probs = c(0.025, 0.5, 0.975)))) %>% 
  rename(soc_euro_2.5 = "2.5%",
         soc_euro_50 = "50%",
         soc_euro_97.5 = "97.5%") %>% 
  mutate(km = 1)


# Export : economic value of 1 km walked per scenario
export(unit_soc_value_2019, here("output", "Tables", "Log linear", "2019", "1km_soc_value_1000replicate.xlsx"))



##############################################################
#                           SAVE 1€                          #
##############################################################


## MEDICAL COSTS ----

# Calculate distance walked to save 1€ of medical costs (km)
set.seed(123)
euro_2019 <- euro_unit(km_total_2019, km_low_2019, km_sup_2019, euro, euro_low, euro_sup, N = 1000)
euro_unit_2019 <- as.data.frame(t(quantile(euro_2019, probs = c(0.025, 0.5, 0.975))))


# Calculate duration walked to save 1€ of medical costs (min)
euro_unit_duration_2019<- euro_unit_2019 %>% 
  mutate(
    min_2.5 = `2.5%` * 60 / walk_speed,
    min_50 = `50%` * 60 / walk_speed,
    min_97.5 = `97.5%` * 60 / walk_speed
  ) %>% 
  rename(km_2.5 = "2.5%",
         km_50 = "50%",
         km_97.5 = "97.5%") %>% 
  mutate(medic_costs = 1)



# Export : Calculate distance and duration to save 1€ of medical costs in 2019
export(euro_unit_duration_2019, here("output", "Tables", "Log linear", "2019", "1€_km_duration_1000replicate.xlsx"))




## SOCIAL COSTS ----

# Calculate distance walked to save 1€ of intangible costs (km)
set.seed(123)
soc_euro_2019 <- euro_unit(km_total_2019, km_low_2019, km_sup_2019, soc_euro, soc_euro_low, soc_euro_sup, N = 1000)
soc_euro_unit_2019 <- as.data.frame(t(quantile(soc_euro_2019, probs = c(0.025, 0.5, 0.975))))


# Calculate duration walked to save 1€ of intangible costs (min)
soc_euro_unit_duration_2019<- soc_euro_unit_2019 %>% 
  mutate(
    min_2.5 = `2.5%` * 60 / walk_speed,
    min_50 = `50%` * 60 / walk_speed,
    min_97.5 = `97.5%` * 60 / walk_speed
  ) %>% 
  rename(km_2.5 = "2.5%",
         km_50 = "50%",
         km_97.5 = "97.5%") %>% 
  mutate(soc_costs = 1)



# Export : Calculate distance and duration to save 1€ of medical costs in 2019
export(soc_euro_unit_duration_2019, here("output", "Tables", "Log linear", "2019", "soc_1€_km_duration_1000replicate.xlsx"))






################################################################################################################################
#                                               7. DESCRIPTION (optional)                                                      #
################################################################################################################################

# IMPORT DATA (to avoid re-generating Monte-Carlo replications)
burden_replicate_age <- import(here("output","RDS","Log linear", "2019", "logHIA_1000replicate_per_age.rds"))



# Mean value for each outcome per disease of random sampled value (no need)
burden_replicate_mean <- burden_replicate_age %>% 
  group_by(age_grp.x, disease) %>% 
  summarise(mean_run = (mean(run)),
            mean_cases = mean(tot_cases, na.rm = T),
            mean_cases_se = mean(tot_cases_se, na.rm = T),
            mean_daly = mean(tot_daly, na.rm = T),
            mean_daly_se = mean(tot_daly_se, na.rm = T),
            mean_costs = mean(tot_medic_costs, na.rm = T),
            mean_costs_se = mean(tot_medic_costs_se, na.rm = T)) %>% 
  mutate(disease = recode_factor(disease, 
                                 bc = "Breast cancer", 
                                 cc="Colon cancer" , 
                                 cvd ="CVD" , 
                                 dem ="Dementia",
                                 diab2 ="T2 Diabetes" , 
                                 dep = "Depression",
                                 mort ="Mortality")) 



# Median value for each outcome per disease of random sampled value (no need because we chose central values for this plot)
burden_replicate_median <- burden_replicate_age %>% 
  group_by(age_grp.x, disease) %>% 
  summarise(median_run = (median(run)),
            median_case = median(tot_cases, na.rm = T),
            median_case_se = median(tot_cases_se, na.rm = T),
            median_daly = median(tot_daly, na.rm = T),
            median_daly_se = median(tot_daly_se, na.rm = T),
            median_cost = median(tot_medic_costs, na.rm = T),
            median_cost_se = median(tot_medic_costs_se, na.rm = T)) %>% 
  mutate(disease = recode_factor(disease, 
                                 bc = "Breast cancer", 
                                 cc="Colon cancer" , 
                                 cvd ="CVD" , 
                                 dem ="Dementia",
                                 diab2 ="T2 Diabetes" , 
                                 dep = "Depression",
                                 mort ="Mortality")) 

# Plot : Median DALY prevented by walking in 2019 according to age group
median_daly_prevented <- ggplot(burden_replicate_median, aes(x = age_grp.x, y = median_daly, fill = disease)) +
  geom_bar(width = 0.7, position = "stack", stat = "identity")  +
  scale_fill_manual(values = c("Breast cancer" = "firebrick2",        
                               "Colon cancer" = "darkorange",
                               "CVD" = "gold" ,
                               "Dementia" = "pink" ,
                               "T2 Diabetes" = "palegreen3",
                               "Depression" = "slateblue",
                               "Mortality" = "steelblue")) +
  xlab("Age group") +
  ylab("Median DALY") +
  theme_minimal()
median_daly_prevented







