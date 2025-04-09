#################################################
############## MULTIPLE IMPUTATION ##############
#################################################

# Files needed :
  # EMP_walkers.xlsx
  # 0_Functions.R


# Files outputted :
  # HIA_replicate.rds : results of HIA outputs for 100 replications of all diseases
  # HIA_replicate_age.rds : results of HIA outputs for 100 replications per age of all diseases 
  # HIA_per_disease.xlsx : Prevented cases, DALY and costs per disease (IC from Monte-Carlo distribution)
  # HIA_per_disease_Rubin.xlsx : Prevented cases, DALY and costs per disease (IC from Rubin's rule)
  # reduc_mortality_risk.xlsx : Reduction of mortality risk due to walking in 2019 (IC from Monte-Carlo distribution)
  

################################################################################################################################
#                                                    1. LOAD PACKAGES                                                          #
################################################################################################################################

pacman :: p_load(
  rio,          # Data importation
  here,         # Localization of files 
  dplyr,        # Data management
  srvyr,        # Survey
  ggplot2       # Data visualization
)


###############################################################################################################################
#                                                     2. IMPORT DATA                                                           #
################################################################################################################################

emp_walk <- import(here("data_clean", "EMP_walkers.xlsx"))


# Import functions
source(here("0_Functions.R"))


################################################################################################################################
#                                                3. SETTING THE CONSTANTS                                                      #
################################################################################################################################

##############################################################
#                          Diseases                          #
##############################################################

dis_vec = c("cc", "dem", "bc", "cvd", "diab2", "mort")

# Colon cancer (Rojas-Rueda et al, 2013)
ref_cc_m <- 168*30.1/11.25                                  # Disease reference volume men (in min)
rr_cc_men_lb <-.67                                          # Disease relative risk for men (upper bound)
rr_cc_men <- .80                                            # Disease relative risk for men
rr_cc_men_ub <- .96                                         # Disease relative risk for men (lower bound)
ref_cc_w <- 168*30.9/11.25                                  # Disease reference volume women
rr_cc_women_lb <- .76                                       # Disease relative risk for women (upper bound)
rr_cc_women <-  .86                                         # Disease relative risk for women 
rr_cc_women_ub <-.98                                        # Disease relative risk for women (lower bound)

# Dementia (Rojas-Rueda et al, 2013)
ref_dem = ref_dem_m = ref_dem_w <- 168*33/11.25             # same for men and women
rr_dem_lb = rr_dem_men_lb =rr_dem_women_lb <-.6 
rr_dem = rr_dem_men = rr_dem_women <- .72
rr_dem_ub = rr_dem_men_ub =rr_dem_women_ub<-.86 

# Breast cancer (Rojas-Rueda et al, 2013)
ref_bc_w <- 60 
rr_bc_women_lb <- .92 
rr_bc_women <- .94
rr_bc_women_ub <-.97

ref_bc_m <- NA
rr_bc_men_ub <-NA
rr_bc_men <- NA
rr_bc_men_lb <- NA

# Cardiovascular disease (Rojas-Rueda et al, 2013)
ref_cvd = ref_cvd_m = ref_cvd_w <- 180                       # 3h per week of physical activity of moderate intensity
rr_cvd_lb = rr_cvd_men_lb =rr_cvd_women_lb<-.79
rr_cvd = rr_cvd_men = rr_cvd_women<- .84
rr_cvd_ub = rr_cvd_men_ub =rr_cvd_women_ub<-.90

# type 2 diabetes
ref_diab2 = ref_diab2_m =ref_diab2_w <- 168*10/11.25
rr_diab2_lb = rr_diab2_men_lb =rr_diab2_women_lb<-.75
rr_diab2 = rr_diab2_men = rr_diab2_women <- .83
rr_diab2_ub = rr_diab2_men_ub =rr_diab2_women_ub<- .91

# Depression (Pearce et al, 2022)
ref_dep = ref_dep_m = ref_dep_w <- 168                           # 168 minutes per week for walking
rr_dep_women_lb <- 0.7
rr_dep_women <-  0.75
rr_dep_women_ub <-  0.8

rr_dep_men_lb <- 0.76
rr_dep_men <- 0.8
rr_dep_men_ub <- 0.85

# Anxiety
ref_anx = ref_anx_m = ref_anx_w <- 
rr_anx_women_ub <- 
rr_anx_women <- 
rr_anx_women_lb <- 
  
  
  
  # mortality (Kelly et al.) 
ref_mort = ref_mort_m =ref_mort_w <- 168                         # 168 minutes per week for walking
rr_mort_lb=  rr_mort_men_lb =rr_mort_women_lb<-.83         
rr_mort = rr_mort_men=  rr_mort_women<- .89
rr_mort_ub =rr_mort_men_ub =rr_mort_women_ub <-.96

## Disability weights ----
cc_dw <- 0.09316752
cc_dw_lb <-0.07674739
cc_dw_ub <-0.1074471

dem_dw <-0.1518996
dem_dw_lb <-0.1250537
dem_dw_ub <-0.1758752

bc_dw <-0.06758792
bc_dw_lb <-0.05164746
bc_dw_ub <-0.083671

cvd_dw <-0.0526328
cvd_dw_lb <-0.04023609
cvd_dw_ub <-0.0645608

diab2_dw <- 0.06806817
diab2_dw_lb <-0.0504114
diab2_dw_ub <-0.08533913

mort_dw <- 1
mort_dw_lb <- 1
mort_dw_ub <- 1

## Medical costs ----
cc_cost <- 26716
dem_cost <- 22748
bc_cost <- 46968
cvd_cost <- 20938
diab2_cost <- 36514
mort_cost <- NA

## Value of a statistical life year for 2019 France ----
vsl <- 133000


################################################################################################################################
#                                                     4. MONTE-CARLO                                                           #
################################################################################################################################
 # Fix a seed to guarantee reproducibility of simulations


## N replications for HIA outcomes (based on random RR generated) per age and disease ----
health_replicate <- emp_walk

burden_replicate_age <- data.frame()

set.seed(123)
for (dis in dis_vec) {
  burden_dis <- burden_prevented_replicate (health_replicate, dis, N = 1000, "age_grp.x")           # HIA for all runs of 1 disease
  burden_replicate_age <- bind_rows(burden_replicate_age, burden_dis)                               # HIA for all diseases
}


  # Export the results of HIA outputs for all replications per age group
export(burden_replicate_age, here("output", "RDS", "HIA_1000replicate_per_age.rds"))



## N replications for HIA outputs (based on random RR generated) per disease ----
burden_replicate <- data.frame()

set.seed(123)
for (dis in dis_vec) {
  burden_dis <- burden_prevented_replicate (health_replicate, dis, N = 1000, NULL)                    # HIA for all runs of 1 disease
  burden_replicate <- bind_rows(burden_replicate, burden_dis)                                         # HIA for all diseases
}

  # Export the results of HIA outputs for all replications
export(burden_replicate, here("output", "RDS", "HIA_1000replicate.rds"))




################################################################################################################################
#                                                 5. INTERVAL OF CONFIDENCE                                                    #
################################################################################################################################

# IMPORT DATA (to avoid re-generating Monte-Carlo replications)
burden_replicate <- import(here("output", "RDS", "HIA_1000replicate.rds"))


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
calc_replicate_IC(morbidity, "tot_medic_costs")                 # Medical costs prevented (distribution of Monte-Carlo replications)
calc_IC_Rubin(morbidity, "tot_medic_costs")                        # Rubin's rule



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
HIA_per_disease <- HIA_burden_IC(burden_disease, dis_vec, outcome_vec, calc_replicate_IC)

  # Export results 
export(HIA_per_disease, here("output", "Tables", "HIA_per_disease.xlsx"))




# IC - Rubin's rule
HIA_per_disease_Rubin <- HIA_burden_IC(burden_disease, dis_vec, outcome_vec, calc_IC_Rubin)

  # Export results 
export(HIA_per_disease_Rubin, here("output", "Tables", "HIA_per_disease_Rubin.xlsx"))




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
export(reduc_mortality_risk, here("output", "RDS",  "reduc_mortality_risk_1000_rep.RDS"))


# IC
reduc_mortality_risk <- import(here("output", "RDS",  "reduc_mortality_risk_1000_rep.RDS"))

N = 1000
IC <-  calc_replicate_IC(reduc_mortality_risk, "mean_mort_reduction_risk")
reduc_mortality_risk_IC <- data.frame(
  reduc_mortality_risk = paste0(round(IC["50%"], 3), " (", round(IC["2.5%"], 3), " - ", round(IC["97.5%"],3),  ")"),
  N_replications = N
)

# Export reduction in mortality risk due to walking
export(reduc_mortality_risk_IC, here("output", "Tables", "reduc_mortality_risk.xlsx"))







################################################################################################################################
#                                                  6. DESCRIPTION (optional)                                                   #
################################################################################################################################

# IMPORT DATA (to avoid re-generating Monte-Carlo replications)
burden_replicate_age <- import(here("output","RDS", "HIA_1000replicate_per_age.rds"))


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









