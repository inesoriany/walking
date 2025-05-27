#################################################
################## FUNCTIONS ####################
#################################################



################################################################################################################################
################################################################################################################################
#                                            1. HEALTH IMPACT ASSESSMENT                                                       #
################################################################################################################################
################################################################################################################################

##############################################################
#                    DISEASE PARAMETERS                      #
##############################################################

# FUNCTION dis_setting : Get the corresponding parameters for each disease
dis_setting = function (dis) {
  rr_women <-  get(paste0("rr_", dis, "_women"))
  rr_women_lb <-  get(paste0("rr_", dis, "_women_lb"))
  rr_women_ub <-  get(paste0("rr_", dis, "_women_ub"))
  rr_men <- get(paste0("rr_", dis, "_men"))
  rr_men_lb <-  get(paste0("rr_", dis, "_men_lb"))
  rr_men_ub <-  get(paste0("rr_", dis, "_men_ub"))
  ref_women <-  get(paste0("ref_", dis, "_w"))
  ref_men <- get(paste0("ref_", dis, "_m"))
  return(data.frame("rr_men" = rr_men, "rr_men_lb" = rr_men_lb, "rr_men_ub" = rr_men_ub,
                    "rr_women" = rr_women, "rr_women_lb" = rr_women_lb, "rr_women_ub" = rr_women_ub,
                    "ref_women" = ref_women, "ref_men" = ref_men))
}




##############################################################
#                DISEASE REDUCTION RISK                      #
##############################################################


## DISEASE REDUCTION PERCENTAGE ----
# FUNCTION reduction_risk : Calculate the disease risk reduction percentage for each individual with a linear regression
  # (% of decrease in disease risk comparing to the baseline : if people did not walk)

reduction_risk = function(data, dis, rr_women, rr_men, ref_women, ref_men) {
  data[[paste0(dis, "_reduction_risk")]] <- ifelse(                        # Calculate risk reduction percentage
    data$sexe == "Female",
    (1 - rr_women) * data$week_time / ref_women,                       # for women % of decrease for this disease risk
    (1 - rr_men) * data$week_time / ref_men)                           # for men % of decrease for this disease risk
  
  if(any(data$mort_reduction_risk > (1 - 0.45), na.rm = TRUE)) {           # Cap mortality at 55%
    data$mort_reduction_risk <- ifelse(data$mort_reduction_risk > (1 - 0.45), 1 - 0.45, data$mort_reduction_risk)
  }
  return(data)
}
  # To calculate the upper bound of reduction of the relative risk, use RR lower bound because the decrease will be higher i.e. the person exposed (walking) is less likely to have the disease 




# FUNCTION loglinear_reduction_risk : Calculate the disease risk reduction percentage for each individual with a log linear regression
# (% of decrease in disease risk comparing to the baseline : if people did not walk)

loglinear_reduction_risk = function(data, dis, rr_women, rr_men, ref_women, ref_men) {
  data[[paste0(dis, "_reduction_risk")]] <- ifelse(                        # Calculate risk reduction percentage
    data$sexe == "Female",
    1-(exp(log(rr_women) * data$week_time / ref_women)),                       # for women % of decrease for this disease risk
    1-(exp(log(rr_men) * data$week_time / ref_men)) )                          # for men % of decrease for this disease risk
  
  if(any(data$mort_reduction_risk > (1 - 0.45), na.rm = TRUE)) {           # Cap mortality at 55%
    data$mort_reduction_risk <- ifelse(data$mort_reduction_risk > (1 - 0.45), 1 - 0.45, data$mort_reduction_risk)
  }
  return(data)
}





## REDUCED DISEASE INCIDENCE ----
# FUNCTION reduc_incidence : Calculate the reduced disease incidence (number of prevented new cases)
reduc_incidence = function (data, incidence_rate, reduction_risk, dis) {
  for (i in 1:nrow(data)) {
    data[i, paste0(dis,"_reduc_incidence")] <- data[i, incidence_rate] * data[i, reduction_risk]
  }
  if (!is.na(data[i, paste0(dis,"_reduc_incidence")]) & data[i, paste0(dis,"_reduc_incidence")] > 0.40) {             
    data[i, paste0(dis,"_reduc_incidence")] <-  0.40                                    # cap reduction to 40%
  }
  return(data)
}




##############################################################
#                           DALY                             #
##############################################################
# Goal : To know the number of sick or death years prevented for each individual by walking


# FUNCTION daly : Calculate DALY (Disability-Adjusted Life Years) for each disease
daly = function(data, dis){
  data[[paste0(dis, "_daly")]] <- data$years_remaining * get((paste0(dis, "_dw"))) * data[[paste0(dis, "_reduc_incidence")]]
  return(data)
}



#IC - FUNCTION daly_IC : to calculate DALY for IC
daly_IC = function(data, dis, bound) {
  data[[paste0(dis, "_daly")]] <- data$years_remaining * get((paste0(dis, "_dw_", bound))) * data[[paste0(dis, "_reduc_incidence")]]
  return(data)
}



##############################################################
#                      ECONOMIC IMPACT                       #
##############################################################

## MEDICAL COSTS ----
# FUNCTION medic_costs : Calculate the medical costs associated with the reduced disease incidence for each individual
medic_costs = function(data, dis) {
  data [[paste0(dis, "_medic_costs")]] <- get(paste0(dis, "_cost")) * data[[paste0(dis, "_reduc_incidence")]]
  return(data)
}



##############################################################
#                        HIA OUTCOMES                        #
##############################################################
# FUNCTION burden_prevented : Total of prevented cases, DALY and saved costs, for each disease
burden_prevented = function(data, dis, group){
  
  dis_burden <- data %>% 
    group_by(across(all_of(group))) %>% 
    summarise(tot_cases = survey_total(!!sym(paste0(dis, "_reduc_incidence")), na.rm = TRUE),       # Total of prevented cases per disease
                tot_daly = survey_total(!!sym(paste0(dis, "_daly")), na.rm = TRUE),                 # Total of prevented DALY per disease
                tot_medic_costs = survey_total(!!sym(paste0(dis, "_medic_costs")), na.rm = TRUE)    # Total of saved medical costs per disease
      ) %>%             
    mutate(disease = dis)
    
  return(dis_burden)
}




################################################################################################################################
################################################################################################################################
#                                                  2. MULTIPLE IMPUTATION                                                      #
################################################################################################################################
################################################################################################################################


# FUNCTION generate_RR : Generate random RR values in a normal distribution based on existing RR and their IC (Monte-Carlo)
#set.seed()
generate_RR_distrib = function (RR, low, sup, N) {          # N : number of random values
  lRR <- log(RR)                                            # Conversion in log scale
  l_low <- log(low)
  l_sup <- log(sup)
  
  sd1 <- (lRR - l_low) / qnorm(1-0.05/2)
  sd2 <- (l_sup - lRR) / qnorm(1-0.05/2) 
  sd <- mean( c(sd1, sd2))                          # Estimation of standard deviation assuming symmetrical confidence intervals
  
  distr_RR <- exp(rnorm(N, lRR, sd))                     # Generation of log-normal distribution (random samples)

  distr_RR[distr_RR>1]=1                                 # just need to truncat values
  distr_RR[distr_RR<0]=0
  
  return(distr_RR)                                       # Return simulated RR value
}



# FUNCTION calc_HIA_replicate : Calculate HIA for each replicate
#set.seed()
calc_HIA_replicate = function(data, dis) {
  params <- dis_setting(dis)                                                            # Parameters of diseases
      
  # Generate RR values for women and men
  rr_women <-  generate_RR_distrib (RR = params$rr_women, params$rr_women_lb, params$rr_women_ub, N=1)
  if (dis %in% c("bc", "cc")) {                                                          # if disease is bc or cc
    rr_men <- generate_RR_distrib (RR = params$rr_men, params$rr_men_lb, params$rr_men_ub, N=1)
  } else {
    rr_men <- rr_women
  }    
  data <- reduction_risk(data, dis, rr_women, rr_men, params$ref_women, params$ref_men)   # Reduction disease risk percentage
      
  dis_incidence_rate <- ifelse(dis=="mort", "mort_rate" , paste0(dis, "_incidence_rate"))
  dis_reduction_risk <- paste0(dis, "_reduction_risk")
  data <-  reduc_incidence(data, dis_incidence_rate, dis_reduction_risk, dis)           # Reduced incidence
      
  data <- daly(data, dis)                                                               # DALY prevented         
      
  data <- medic_costs(data, dis)                                                        # Medical costs prevented
  
  return(data)
}



# FUNCTION burden_prevented_replicate : Total of prevented cases, DALY and saved costs, for each disease based on RR random values issued (Monte-Carlo)
burden_prevented_replicate = function(data, dis, N, group){                             # group : Column name (or names) used to group the results (e.g., "age", "sex")
  burden_dis <- data.frame()
  
  for (i in 1:N) {                                                                      # N replicates
    print(i)                                                                            # To track each run 
    print(paste0("~", dis))                                                             # To track each disease
    
    data_replicate <- calc_HIA_replicate(data, dis)                                     # Each i time, draw 1 random RR for men and women, for each disease
      
    burden_i <- data_replicate %>%                                                      # HIA outcomes based on generated RR values
      as_survey_design(ids = ident_ind,
                         weights = pond_indc) %>% 
      group_by(across(all_of(group))) %>% 
      summarise(tot_cases = survey_total(!!sym(paste0(dis, "_reduc_incidence")), na.rm = TRUE),       # Total of prevented cases per disease in this replicate
                  tot_daly = survey_total(!!sym(paste0(dis, "_daly")), na.rm = TRUE),                 # Total of prevented DALY per disease in this replicate
                  tot_medic_costs = survey_total(!!sym(paste0(dis, "_medic_costs")), na.rm = TRUE)    # Total of saved medical costs per disease in this replicate
      ) %>%             
      mutate(disease = dis, run = i)
    
    burden_dis <- bind_rows(burden_dis, burden_i)                                    # A table per disease : HIA results for each disease for all runs
  }
  return(burden_dis)
}



# FUNCTION calc_replicate_IC : Calculate interval of confidence by combining replications obtained with generated RR samples to generate a posterior distribution for each outcome
#set.seed()
calc_replicate_IC = function(data, outcome){
  vec = c()
  se_name = paste0(outcome, "_se")
  
  for (i in 1:nrow(data)){
    sam = rnorm(n=200, mean = as.numeric(data[i,outcome]), sd = as.numeric(data[i,se_name]) ) # Generation of samples : uncertainty estimation
    vec = c(vec, sam)
  }
  IC = quantile(vec, probs = c(0.025, 0.5, 0.975))
  return(IC)  
}




# FUNCTION calc_IC_Rubin : Calculate interval of confidence with using Rubin's rules
calc_IC_Rubin = function(data, outcome){
  zq <- qnorm(1-0.05/2)
  se_name = paste0(outcome, "_se")
  
  theta =  sum(data[,outcome])/nrow(data)                             # Pooled mean differences
  V_w = sum((data[,se_name] )^2)/nrow(data)                           # Within imputation variance
  V_b = sum((data[,outcome] - theta )^2) / (nrow(data)-1)             # Between imputation variance
  
  V_tot = V_w + V_b +  V_b / (nrow(data))                             # Total variance    
  
  IC = (c(theta-zq*sqrt(V_tot), theta,theta+zq*sqrt(V_tot)))     # Confidence interval
  return(IC)  
}



# FUNCTION HIA_burden_IC : Get a table with HIA outcomes and IC 
#set.seed() if use calc_replicate_IC
HIA_burden_IC = function(data, dis_vec, outcome_vec, IC_func ) {
  HIA_burden <- data.frame() 
  for (dis in dis_vec) {
    data_dis <- data %>% 
      filter(disease == dis)                                             # Filter for the corresponding disease
    
    HIA_dis <- data.frame(disease = dis)                                 # 1 line = 1 disease
    
    for (out in outcome_vec) {
      IC <- IC_func(data_dis, out)                                       # Calculate IC for the corresponding outcome with the chosen method
      
      HIA_dis <- HIA_dis %>%                                             # All outcomes for 1 disease
        mutate(!!sym(out) := round(IC[2], 3),
               !!sym(paste0(out, "_low")) := round(IC[1], 3),
               !!sym(paste0(out, "_sup")) := round(IC[3], 3))
    }
    HIA_burden <- bind_rows(HIA_burden, HIA_dis)                         # Gather all outcomes for all diseases
  }
  return(HIA_burden)
}



################################################################################################################################
################################################################################################################################
#                                                  3. ECONOMIC UNIT VALUE                                                      #
################################################################################################################################
################################################################################################################################

# FUNCTION unit_value : Calculate the economic value of 1 km walked
unit_value = function(km, km_low, km_sup, euro, euro_low, euro_sup, N = 1000) {
  km_sd1 <- (km - km_low) / qnorm(1-0.05/2)
  km_sd2 <- (km_sup - km) / qnorm(1-0.05/2)
  km_sd <- mean(c(km_sd1, km_sd2))
  distr_km <- rnorm(N, km, km_sd)
  
  euro_sd1 <- (euro - euro_low) / qnorm(1-0.05/2)
  euro_sd2 <- (euro_sup - euro) / qnorm(1-0.05/2)
  euro_sd <- mean(c(euro_sd1, euro_sd2))
  distr_euro <- rnorm(N, euro, euro_sd)
  
  distr_unit <- distr_euro / distr_km
  
  return(distr_unit)
}


# FUNCTION euro_value : Calculate distance walked to save 1€
euro_unit = function(km, km_low, km_sup, euro, euro_low, euro_sup, N = 1000) {
  km_sd1 <- (km - km_low) / qnorm(1-0.05/2)
  km_sd2 <- (km_sup - km) / qnorm(1-0.05/2)
  km_sd <- mean(c(km_sd1, km_sd2))
  distr_km <- rnorm(N, km, km_sd)
  
  euro_sd1 <- (euro - euro_low) / qnorm(1-0.05/2)
  euro_sd2 <- (euro_sup - euro) / qnorm(1-0.05/2)
  euro_sd <- mean(c(euro_sd1, euro_sd2))
  distr_euro <- rnorm(N, euro, euro_sd)
  
  distr_unit <- distr_km / distr_euro
  
  return(distr_unit)
}
 




