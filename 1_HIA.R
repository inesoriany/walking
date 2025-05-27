#################################################
############ HEALTH IMPACT ASSESSMENT ###########
############    linear regression     ###########
#################################################

# Files needed :
  # EMP_walkers.xlsx
  # 0_Functions.R


# Files outputted :
  # plot_prevented_cases.png : Diseases cases prevented per sex by walking in 2019 
  # HIA_walking_2019.xlsx : Cases, daly, costs (medical + social) prevented by walking in 2019
  # plot_deaths_prevented.png : Deaths prevented per sex by walking in 2019
  # plot_YLL_prevented.png : YLL prevented per sex by walking in 2019 
  # 1€_km_duration.xlsx : Distance and duration that saved 1€ in 2019

  # plot_cases_step.png : Benefits it we walk 10 000 steps
  


################################################################################################################################
#                                                    1. LOAD PACKAGES                                                          #
################################################################################################################################
pacman :: p_load(
  rio,          # Data importation
  here,         # Localization of files 
  dplyr,        # Data management
  srvyr,        # Survey
  survey
)



################################################################################################################################
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
         low_medic_costs = burden_lb[,5], sup_medic_costs = burden_ub[,5])



##############################################################
#                    ECONOMIC IMPACT (2)                     #
##############################################################


## SOCIAL COSTS (intangible)----
# Add social costs
burden_IC <- burden_IC %>% 
  mutate(tot_soc_costs = tot_daly*vsl,
         low_soc_costs = low_daly*vsl,
         sup_soc_costs = sup_daly*vsl)

# Reorganize columns
burden_IC <- burden_IC %>% 
  select(disease,
         tot_cases, tot_cases_se, low_cases, sup_cases,
         tot_daly, tot_daly_se, low_daly, sup_daly,
         tot_medic_costs, tot_medic_costs_se, low_medic_costs, sup_medic_costs,
         tot_soc_costs, low_soc_costs, sup_soc_costs)



# Export HIA : total of prevented cases, DALY and saved costs per disease
export(burden_IC, here("output", "Tables", "Linear", "HIA_walking_2019.xlsx"))



################################################################################################################################
#                                                 5. RESULTS - VISUALIZATION                                                   #
################################################################################################################################

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

  # Export HIA per sex : total of prevented cases, DALY and saved costs per disease
export(burden_sex_IC, here("output", "Tables", "Linear", "HIA_sex_walking_2019.xlsx"))


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

  # Export plot
ggsave(here("output", "Plots", "Linear", "plot_cases_prevented.png"), plot = cases_prevented)




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
ggsave(here("output", "Plots", "Linear", "plot_DALY_prevented.png"), plot = daly_prevented)




##  HIA according to quartile of revenue ----
burden_rev <- data.frame()
burden_rev_ub <- data.frame()
burden_rev_lb <- data.frame()

for (dis in dis_vec) {
  dis_burden_rev <- burden_prevented(surv_dis, dis, c("quartile_rev"))
  burden_rev <- bind_rows(burden_rev, dis_burden_rev) 
  
  # Upper bound
  dis_burden_rev_ub <- burden_prevented(surv_dis_ub, dis, c("quartile_rev"))
  burden_rev_ub <- bind_rows(burden_rev_ub, dis_burden_rev_ub) 
  
  # Lower bound
  dis_burden_rev_lb <- burden_prevented(surv_dis_lb, dis, c("quartile_rev"))
  burden_rev_lb <- bind_rows(burden_rev_lb, dis_burden_rev_lb) 
}

# Combine values with IC
burden_rev_IC <- burden_rev %>% 
  mutate(low_cases = burden_rev_lb[,"tot_cases"], sup_cases = burden_rev_ub[,"tot_cases"],
         low_daly = burden_rev_lb[,"tot_daly"], sup_daly = burden_rev_ub[,"tot_daly"],
         low_medic_costs = burden_rev_lb[,"tot_medic_costs"], sup_medic_costs = burden_rev_ub[,"tot_medic_costs"])


  # Rename columns and variables
burden_rev_IC <-  burden_rev_IC %>% 
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
cases_prevented_rev <- ggplot(burden_rev_IC, aes(x = disease, y = tot_cases, ymin = low_cases, ymax = sup_cases, fill = Revenue)) +
  geom_bar(width = 0.7, position = position_dodge(.9), stat = "identity")  +
  geom_errorbar(position = position_dodge(0.9), width = .25) +
  ylab ("Cases prevented") +
  xlab("Disease") +
  theme_minimal()
cases_prevented_rev

#Export plot
ggsave(here("output", "Plots", "Linear", "plot_cases_per_rev.png"), plot = cases_prevented_rev)






################################################################################################################################
#                                                       6. DESCRIPTION                                                         #
################################################################################################################################

##############################################################
#              DALY prevented by walking in 2019             #
##############################################################

# Global
sum(burden_IC[["tot_daly"]])
sum(burden_IC[["low_daly"]])
sum(burden_IC[["sup_daly"]])

# YLD
morbidity_burden_IC <- burden_IC %>% 
  filter(disease != "mort")

sum(morbidity_burden_IC[["tot_daly"]])
sum(morbidity_burden_IC[["low_daly"]])
sum(morbidity_burden_IC[["sup_daly"]])

sum(morbidity_burden_IC[["tot_daly"]]) / sum(burden_IC[["tot_daly"]])                 # Proportion of contribution


# YLL 
mortality_burden_IC <- burden_IC %>% 
  filter(disease == "mort")

mortality_burden_IC[["tot_daly"]] / sum(burden_IC[["tot_daly"]])                      # Proportion of contribution



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
ggsave(here("output", "Plots", "Linear", "plot_deaths_prevented.png"), plot = deaths_prevented)




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
ggsave(here("output", "Plots", "Linear", "plot_YLL_prevented.png"), plot = YLL_prevented)




##############################################################
#                  ECONOMIC UNIT VALUE (€)                   #
##############################################################
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

euro <- sum(burden_IC[["tot_medic_costs"]])
euro_low <- sum(burden_IC[["low_medic_costs"]])
euro_sup <- sum(burden_IC[["sup_medic_costs"]])

soc_euro <- sum(burden_IC[["tot_soc_costs"]])
soc_euro_low <- sum(burden_IC[["low_soc_costs"]])
soc_euro_sup <- sum(burden_IC[["sup_soc_costs"]])



##################################
########   VALUE OF 1km   ########

## SOCIAL COSTS ----

# Calculate economic value of 1 km walked (intangible costs)
set.seed(123)
unit_soc_2019 <- unit_value(km_total_2019, km_low_2019, km_sup_2019, soc_euro, soc_euro_low, soc_euro_sup, N=1000)
unit_soc_value_2019 <- as.data.frame(t(quantile(unit_soc_2019, probs = c(0.025, 0.5, 0.975)))) %>% 
  rename(soc_cost_2.5 = "2.5%",
         soc_cost_50 = "50%",
         soc_cost_97.5 = "97.5%") %>% 
  mutate(km = 1)

    
# Export : economic value of 1 km walked per scenario
export(unit_soc_value_2019, here("output", "Tables", "Linear", "1km_soc_value.xlsx"))




##################################
#########     SAVE 1€    #########

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
export(euro_unit_duration_2019, here("output", "Tables", "Linear", "1€_km_duration.xlsx"))




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
export(soc_euro_unit_duration_2019, here("output", "Tables", "Linear", "soc_1€_km_duration.xlsx"))




##############################################################
#               CO2 emissions prevented in 2019              #
##############################################################

# If those trips were driven

mt_2019 <- km_total_2019 * CO2_emit * 1e-12                    # CO2 emissions (in Mt CO2)
mt_2019_low <- km_low_2019 * CO2_emit * 1e-12
mt_2019_sup <- km_sup_2019 * CO2_emit * 1e-12

mt_2019
mt_2019_low
mt_2019_sup





###########################################################################################################################################################################
###########################################################################################################################################################################
#                                                                       HIA - 10 000 STEPS                                                                                #
###########################################################################################################################################################################
###########################################################################################################################################################################


################################################################################################################################
#                                                     1. IMPORT DATA                                                           #
################################################################################################################################

emp_step <- import(here("data_clean", "EMP_walkers.xlsx"))



################################################################################################################################
#                                                       2. DATASET                                                             #
################################################################################################################################

# Week time spent if people walk 10 000 steps (equivalent to 8km) (in min)
week_time_step <- 8*7*60 / walk_speed

emp_step <- emp_step %>% 
  mutate(week_time = week_time_step)


################################################################################################################################
#                                               3. HEALTH IMPACT ASSESSMENT                                                    #
################################################################################################################################

health_step <- emp_step
health_step_ub <- emp_step
health_step_lb <- emp_step


for (dis in dis_vec) {
  params <- dis_setting(dis)
  
  # Percentage of disease decrease 
  health_step<-  reduction_risk(health_step, dis, params$rr_women, params$rr_men, params$ref_women, params$ref_men)
  health_step_ub <-  reduction_risk(health_step_ub, dis, params$rr_women_lb, params$rr_men_lb, params$ref_women, params$ref_men)
  health_step_lb <-  reduction_risk(health_step_lb, dis, params$rr_women_ub, params$rr_men_ub, params$ref_women, params$ref_men)
  
  # Reduced incidence
  dis_incidence_rate <- ifelse(dis=="mort", "mort_rate" , paste0(dis, "_incidence_rate"))
  dis_reduction_risk <- paste0(dis, "_reduction_risk")
    
  health_step <-  reduc_incidence(health_step, dis_incidence_rate, dis_reduction_risk, dis)
  health_step_ub <-  reduc_incidence(health_step_ub, dis_incidence_rate, dis_reduction_risk, dis)
  health_step_lb <-  reduc_incidence(health_step_lb, dis_incidence_rate, dis_reduction_risk, dis)
  
  # DALY prevented  
  health_step <- daly(health_step, dis)
  health_step_ub <- daly_IC(health_step_ub, dis, "ub")
  health_step_lb <- daly_IC(health_step_lb, dis, "lb")
  
  # Medical costs prevented
  health_step <- medic_costs(health_step, dis)
  health_step_ub <- medic_costs(health_step_ub, dis)
  health_step_lb <- medic_costs(health_step_lb, dis)
}



##############################################################
#                      HIA OUTCOMES                          #     with cases, DALY and medical costs
##############################################################

# Survey design ----
surv_dis_step <- health_step %>% 
  as_survey_design(ids = ident_ind,
                   weights = pond_indc,
                   strata = c(sexe, age_grp.x),           # by sex and age group
                   nest = TRUE)
# IC
# Upper bound
surv_dis_step_ub <- health_step_ub %>% 
  as_survey_design(ids = ident_ind,
                   weights = pond_indc,
                   strata = c(sexe, age_grp.x),           
                   nest = TRUE)

# Lower bound
surv_dis_step_lb <- health_step_lb %>% 
  as_survey_design(ids = ident_ind,
                   weights = pond_indc,
                   strata = c(sexe, age_grp.x),      
                   nest = TRUE)


# Total of prevented cases, DALY and saved costs, for each disease in 2019
burden_step <- data.frame()
burden_step_ub <- data.frame()
burden_step_lb <- data.frame()
for (dis in dis_vec) {
  dis_burden_step <- burden_prevented(surv_dis_step, dis, "sexe")
  burden_step <- bind_rows(burden_step, dis_burden_step) 
  
  dis_burden_step_ub <- burden_prevented(surv_dis_step_ub, dis, "sexe")
  burden_step_ub <- bind_rows(burden_step_ub, dis_burden_step_ub) 
  
  dis_burden_step_lb <- burden_prevented(surv_dis_step_lb, dis, "sexe")
  burden_step_lb <- bind_rows(burden_step_lb, dis_burden_step_lb) 
}


# Gather results with IC
burden_step_IC <- burden_step %>% 
  mutate(low_cases = burden_step_lb[,2], sup_cases = burden_step_ub[,2],
         low_daly = burden_step_lb[,4], sup_daly = burden_step_ub[,4],
         low_medic_costs = burden_step_lb[,6], sup_medic_costs = burden_step_ub[,6])



##############################################################
#                        SOCIAL IMPACT                       #
##############################################################


## SOCIAL COSTS (intangible)----
# Add social costs
burden_step_IC <- burden_step_IC %>% 
  mutate(tot_soc_costs = tot_daly*vsl,
         low_soc_costs = low_daly*vsl,
         sup_soc_costs = sup_daly*vsl)

# Reorganize columns
burden_step_IC <- burden_step_IC %>% 
  select(sexe,
         disease,
         tot_cases, tot_cases_se, low_cases, sup_cases,
         tot_daly, tot_daly_se, low_daly, sup_daly,
         tot_medic_costs, tot_medic_costs_se, low_medic_costs, sup_medic_costs,
         tot_soc_costs, low_soc_costs, sup_soc_costs) %>% 
  mutate(disease = recode_factor(disease, 
                                 bc = "Breast cancer", 
                                 cc="Colon cancer" , 
                                 cvd ="CVD" , 
                                 dem ="Dementia",
                                 diab2 ="T2 Diabetes" , 
                                 dep = "Depression",
                                 mort ="Mortality")) %>% 
  rename(Sex = sexe)



# Export HIA : total of prevented cases, DALY and saved costs per disease
export(burden_step_IC, here("output", "Tables", "Linear", "HIA_10000steps.xlsx"))



################################################################################################################################
#                                                      4. VISUALIZATION                                                        #
################################################################################################################################

# Plot : Cases prevented by walking in 2019 according to sex 
cases_prevented_step <- ggplot() +
  geom_bar(data = burden_sex_IC, 
           mapping = aes(x = disease, y = tot_cases, fill = Sex),
           width = 0.7,
           position = position_dodge2(0.7),
           stat = "identity") +
  
  geom_errorbar(data = burden_sex_IC,
                mapping = aes(x = disease, ymin = low_cases, ymax = sup_cases, group = Sex),
                position = position_dodge(0.7),
                width = 0.25) +
  
  scale_fill_manual(values = c("Female" = "darkorange1",
                               "Male" = "chartreuse4")) +
  
  
  geom_bar(data = burden_step_IC, 
           mapping = aes(x = disease, y = tot_cases, color = Sex),
           width = 0.7,
           position = position_dodge2(0.7),
           stat = "identity",
           fill = NA,
           linetype = "dashed") +
  
  scale_color_manual(values = c("Female" = "darkorange1",
                                "Male" = "chartreuse4")) +
  
  geom_errorbar(data = burden_step_IC,
                mapping = aes(x = disease, ymin = low_cases, ymax = sup_cases, group = Sex),
                position = position_dodge(0.7),
                width = 0.25,
                color = "black") +
  
  ylab("Cases prevented") +
  xlab("Disease") +
  theme_minimal()

cases_prevented_step


# Export plot
ggsave(here("output", "Plots", "Linear", "plot_cases_step.png"), plot = cases_prevented_step)


















