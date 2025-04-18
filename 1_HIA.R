#################################################
############ HEALTH IMPACT ASSESSMENT ###########
############    linear regression     ###########
#################################################

# Files needed :
  # EMP_walkers.xlsx
  # 0_Functions.R


# Files outputted :
  # plot_prevented_cases.tiff : Diseases cases prevented per sex by walking in 2019 
  # HIA_walking_2019.xlsx : Cases, daly, costs (medical + social) prevented by walking in 2019
  # plot_deaths_prevented.tiff : Deaths prevented per sex by walking in 2019
  # plot_YLL_prevented.tiff : YLL prevented per sex by walking in 2019 
  


################################################################################################################################
#                                                    1. LOAD PACKAGES                                                          #
################################################################################################################################
pacman :: p_load(
  rio,          # Data importation
  here,         # Localization of files 
  dplyr,        # Data management
  survey        # Survey
)



################################################################################################################################
#                                                     2. IMPORT DATA                                                           #
################################################################################################################################

emp_walk <- import(here("data_clean", "EMP_walkers.xlsx"))

# Import functions
source(here("0_Functions.R"))



################################################################################################################################
#                                                3. SETTING THE CONSTANTS                                                      #
################################################################################################################################


## Walking speed ----
walk_speed <- 4.8  # km/h


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
#                                              4. HEALTH IMPACT ASSESSMENT                                                     #
################################################################################################################################



##############################################################
#                DISEASE REDUCTION RISK                      #
##############################################################

# Calculate the percentage of disease decrease in 2019 attributed to walking ----
health_walkers <- emp_walk
for (dis in dis_vec) {
  params <- dis_setting(dis)
  health_walkers <-  reduction_risk(health_walkers, dis, params$rr_women, params$rr_men, params$ref_women, params$ref_men)
}



# IC
  # Upper bound
health_walkers_ub <- emp_walk
for (dis in dis_vec) {
  params <- dis_setting(dis)
  health_walkers_ub <-  reduction_risk(health_walkers_ub, dis, params$rr_women_lb, params$rr_men_lb, params$ref_women, params$ref_men)
} # use RR lower bound because the decrease will be higher i.e. the person exposed (walking) is less likely to have the disease  

  # Lower bound
health_walkers_lb <- emp_walk
for (dis in dis_vec) {
  params <- dis_setting(dis)
  health_walkers_lb <-  reduction_risk(health_walkers_lb, dis, params$rr_women_ub, params$rr_men_ub, params$ref_women, params$ref_men)
}




# Calculate the reduced disease incidence in 2019 attributed to walking ----
for (dis in dis_vec) {
  dis_incidence_rate <- ifelse(dis=="mort", "mort_rate" , paste0(dis, "_incidence_rate"))
  dis_reduction_risk <- paste0(dis, "_reduction_risk")
  
  health_walkers <-  reduc_incidence(health_walkers, dis_incidence_rate, dis_reduction_risk, dis)
}

# IC
  # Upper bound
for (dis in dis_vec) {
  dis_incidence_rate <- ifelse(dis=="mort", "mort_rate" , paste0(dis, "_incidence_rate"))
  dis_reduction_risk <- paste0(dis, "_reduction_risk")
  
  health_walkers_ub <-  reduc_incidence(health_walkers_ub, dis_incidence_rate, dis_reduction_risk, dis)
}
  # Lower bound
for (dis in dis_vec) {
  dis_incidence_rate <- ifelse(dis=="mort", "mort_rate" , paste0(dis, "_incidence_rate"))
  dis_reduction_risk <- paste0(dis, "_reduction_risk")
  
  health_walkers_lb <-  reduc_incidence(health_walkers_lb, dis_incidence_rate, dis_reduction_risk, dis)
}




##############################################################
#                           DALY                             #
##############################################################
# Goal : To know the number of sick or death years prevented for each individual by walking

# Calculate DALY (Disability-Adjusted Life Years) prevented for each disease in 2019
for (dis in dis_vec) {
  health_walkers <- daly(health_walkers, dis)


  #Upper bound
  health_walkers_ub <- daly_IC(health_walkers_ub, dis, "ub")

  #Lower bound
  health_walkers_lb <- daly_IC(health_walkers_lb, dis, "lb")
}



##############################################################
#                   ECONOMIC IMPACT (1)                      #
##############################################################

## MEDICAL COSTS SAVED----

# Calculate the medical costs associated with the reduced disease incidence for each individual in 2019 
for (dis in dis_vec) {
  health_walkers <- medic_costs(health_walkers, dis)

# IC
# Upper bound
  health_walkers_ub <- medic_costs(health_walkers_ub, dis)

# Lower bound
  health_walkers_lb <- medic_costs(health_walkers_lb, dis)
}



##############################################################
#                      HIA OUTCOMES                          #     with cases, DALY and medical costs
##############################################################

# Survey design ----
surv_dis <- health_walkers %>% 
    as_survey_design(ids = ident_ind,
                     weights = pond_indc,
                     strata = c(sexe, age_grp.x, quartile_rev),           # by sex and age group
                     nest = TRUE)
  # IC
    # Upper bound
surv_dis_ub <- health_walkers_ub %>% 
  as_survey_design(ids = ident_ind,
                   weights = pond_indc,
                   strata = c(sexe, age_grp.x, quartile_rev),           
                   nest = TRUE)

    # Lower bound
surv_dis_lb <- health_walkers_lb %>% 
  as_survey_design(ids = ident_ind,
                   weights = pond_indc,
                   strata = c(sexe, age_grp.x, quartile_rev),      
                   nest = TRUE)


# Total of prevented cases, DALY and saved costs, for each disease in 2019
burden <- data.frame()
for (dis in dis_vec) {
  dis_burden <- burden_prevented(surv_dis, dis, NULL)
  burden <- bind_rows(burden, dis_burden) 
}

# IC
# Upper bound
burden_ub <- data.frame()
burden_lb <- data.frame()
for(dis in dis_vec) {
  dis_burden_ub <- burden_prevented(surv_dis_ub, dis, NULL)
  burden_ub <- bind_rows(burden_ub, dis_burden_ub) 
  
  # Lower bound
  dis_burden_lb <- burden_prevented(surv_dis_lb, dis, NULL)
  burden_lb <- bind_rows(burden_lb, dis_burden_lb) 
}


# Gather results with IC
burden_IC <- burden %>% 
  mutate(low_cases = burden_lb[,1], sup_cases = burden_ub[,1],
         low_daly = burden_lb[,3], sup_daly = burden_ub[,3],
         low_medic = burden_lb[,5], sup_medic = burden_ub[,5])



##############################################################
#                    ECONOMIC IMPACT (2)                     #
##############################################################


## SOCIAL COSTS (intangible)----
# Add social costs
burden_IC <- burden_IC %>% 
  mutate(social_cost = tot_daly*vsl,
         low_social_cost = low_daly*vsl,
         sup_social_cost = sup_daly*vsl)



# Export HIA : total of prevented cases, DALY and saved costs per disease
export(burden_IC, here("output", "Tables", "Linear", "HIA_walking_2019.xlsx"))




##############################################################
#                  RESULTS - VISUALIZATION                   #
##############################################################

## Total of prevented cases, DALY, medical costs per sex and age group ----
burden_sex_age <- data.frame()
burden_sex_age_ub <- data.frame()
burden_sex_age_lb <- data.frame()

for (dis in dis_vec) {
  dis_burden_sex_age <- burden_prevented(surv_dis, dis, c("age_grp.x", "sexe"))
  burden_sex_age <- bind_rows(burden_sex_age, dis_burden_sex_age) 
  
  # Upper bound
  dis_burden_sex_age_ub <- burden_prevented(surv_dis_ub, dis, c("age_grp.x", "sexe"))
  burden_sex_age_ub <- bind_rows(burden_sex_age_ub, dis_burden_sex_age_ub) 
  
  # Lower bound
  dis_burden_sex_age_lb <- burden_prevented(surv_dis_lb, dis, c("age_grp.x", "sexe"))
  burden_sex_age_lb <- bind_rows(burden_sex_age_lb, dis_burden_sex_age_lb) 
}

  # Combine values with IC
burden_sex_age_IC <- burden_sex_age %>% 
  mutate(low_cases = burden_sex_age_lb[,"tot_cases"], sup_cases = burden_sex_age_ub[,"tot_cases"],
         low_daly = burden_sex_age_lb[,"tot_daly"], sup_daly = burden_sex_age_ub[,"tot_daly"],
         low_medic_costs = burden_sex_age_lb[,"tot_medic_costs"], sup_medic_costs = burden_sex_age_ub[,"tot_medic_costs"])


  # Rename columns and variable
burden_sex_age_IC <-  burden_sex_age_IC %>% 
  mutate(disease = recode_factor(disease, 
                                 bc = "Breast cancer", 
                                 cc="Colon cancer" , 
                                 cvd ="CVD" , 
                                 dem ="Dementia",
                                 diab2 ="T2 Diabetes" , 
                                 dep = "Depression",
                                 mort ="Mortality")) %>% 
  rename(Sex = sexe)




## HIA according to sex ----
burden_sex_IC <- burden_sex_age_IC %>% 
  group_by(Sex, disease) %>% 
  summarise(tot_cases = sum(tot_cases, na.rm = T),
            tot_daly = sum(tot_daly, na.rm = T),
            tot_medic_costs = sum(tot_medic_costs, na.rm = T),
            low_cases = sum(low_cases, na.rm = T),
            sup_cases = sum(sup_cases, na.rm = T),
            low_daly = sum(low_daly, na.rm = T),
            sup_daly = sum(sup_daly, na.rm = T),
            low_medic_costs = sum(low_medic_costs, na.rm = T),
            sup_medic_costs = sum(sup_medic_costs, na.rm = T))


# Plot : Cases prevented by walking in 2019 according to sex 
cases_prevented <- ggplot(burden_sex_IC, aes(x = disease, y = tot_cases, ymin = low_cases, ymax = sup_cases, fill = Sex)) +
  geom_bar(width = 0.7, position = position_dodge2(.7), stat = "identity")  +
  geom_errorbar(position = position_dodge(.7), width = .25) +
  scale_fill_manual(values = c("Female" = "darkorange1",
                                "Male" = "chartreuse4")) +
  ylab ("Cases prevented") +
  xlab("Disease") +
  theme_minimal()
cases_prevented

  #Export plot
ggsave(here("output", "Plots", "Linear", "plot_cases_prevented.tiff"), plot = cases_prevented)




## HIA according to age group ----
burden_age_IC <- burden_sex_age_IC %>% 
  group_by(age_grp.x, disease) %>% 
  summarise(tot_cases = sum(tot_cases, na.rm = T),
            tot_daly = sum(tot_daly, na.rm = T),
            tot_medic_costs = sum(tot_medic_costs, na.rm = T),
            low_cases = sum(low_cases, na.rm = T),
            sup_cases = sum(sup_cases, na.rm = T),
            low_daly = sum(low_daly, na.rm = T),
            sup_daly = sum(sup_daly, na.rm = T),
            low_medic_costs = sum(low_medic_costs, na.rm = T),
            sup_medic_costs = sum(sup_medic_costs, na.rm = T))


# Plot : DALY prevented by walking in 2019 according to age group
daly_prevented <- ggplot(burden_age_IC, aes(x = age_grp.x, y = tot_daly, fill = disease)) +
  geom_bar(width = 0.7, position = "stack", stat = "identity")  +
  scale_fill_manual(values = c("Breast cancer" = "firebrick2",      
                               "Colon cancer" = "darkorange",
                               "CVD" = "gold" ,
                               "Dementia" = "pink" ,
                               "T2 Diabetes" = "palegreen3",
                               "Depression" = "slateblue",
                               "Mortality" = "steelblue")) +
  xlab("Age group") +
  ylab("DALY") +
  theme_minimal()
daly_prevented

  #Export plot
ggsave(here("output", "Plots", "Linear", "plot_DALY_prevented.tiff"), plot = daly_prevented)


##############################
## Per quartile of revenue (no need mais CODE A REECRIRE AVEC LA NOUVELLE FONCTION) ----

  # Combine values with IC


  # Rename columns and variables
prevented_dis_rev_IC <-  prevented_dis_rev_IC %>% 
  mutate(disease = recode_factor(disease, 
                                 bc = "Breast cancer", 
                                 cc="Colon cancer" , 
                                 cvd ="CVD" , 
                                 dem ="Dementia",
                                 diab2="T2 Diabetes" ,
                                 dep = "Depression",
                                 mort="Mortality")) %>%
  mutate(quartile_rev = recode_factor(quartile_rev,
                                      "1" = "Q1",
                                      "2" = "Q2",
                                      "3" = "Q3",
                                      "4" = "Q4")) %>% 
  rename(Revenue = quartile_rev)



  # Plot : Diseases cases prevented per revenue by walking in 2019 
prevented_cases_rev <- ggplot(prevented_dis_rev_IC, aes(x = disease, y = tot_cases, ymin = low, ymax = sup, fill = Revenue)) +
  geom_bar(width = 0.7, position = position_dodge(.9), stat = "identity")  +
  geom_errorbar(position = position_dodge(0.9), width = .25) +
  ylab ("Cases prevented") +
  xlab("Disease") +
  theme_minimal()
prevented_cases_rev
###################################







################################################################################################################################
#                                                       4. DESCRIPTION                                                         #
################################################################################################################################

##############################################################
#        Premature deaths prevented by walking in 2019       #
##############################################################

prevented_death <- burden_sex_age_IC %>% 
  filter(disease == "Mortality")


# Plot : Contribution of the different age groups to premature deaths prevented by walking in 2019
deaths_prevented <- ggplot(prevented_death, aes(x = age_grp.x, y = tot_cases, ymin = low_cases, ymax = sup_cases, fill = Sex)) +
  geom_bar(width = 0.7, position = position_dodge2(0.7), stat = "identity") +
  geom_errorbar(position = position_dodge(0.7), width = 0.25) +
  scale_fill_manual(values = c("Female" = "darkorange1",
                               "Male" = "chartreuse4")) +
  ylab("Premature deaths prevented") +
  xlab("Age group") +
  theme_minimal()
deaths_prevented

# Export plot
ggsave(here("output", "Plots", "Linear", "plot_deaths_prevented.tiff"), plot = deaths_prevented)




##############################################################
#               YLL prevented by walking in 2019             #
##############################################################


# Plot : Contribution of the different age groups to years of life lost (YLL) prevented by walking in 2019
YLL_prevented <- ggplot(prevented_death, aes(x = age_grp.x, y = tot_daly, ymin = low_daly, ymax = sup_daly, fill = Sex)) +
  geom_bar(width = 0.7, position = position_dodge2(0.7), stat = "identity") +
  geom_errorbar(position = position_dodge(0.7), width = 0.25) +
  scale_fill_manual(values = c("Female" = "darkorange1",
                               "Male" = "chartreuse4")) +
  ylab("YLL prevented") +
  xlab("Age group") +
  theme_minimal()
YLL_prevented

# Export plot : YLL prevented per sex by walking in 2019 
ggsave(here("output", "Plots", "Linear", "plot_YLL_prevented.tiff"), plot = YLL_prevented)






