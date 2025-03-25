#################################################
################## FUNCTIONS ####################
#################################################


################################################################################################################################
################################################################################################################################
#                                            1. HEALTH IMPACT ASSESSMENT                                                       #
################################################################################################################################
################################################################################################################################


##############################################################
#                DISEASE REDUCTION RISK                      #
##############################################################

## DISEASE REDUCTION PERCENTAGE ----
# FUNCTION reduction_risk : Calculate the disease risk reduction percentage for each individual 
  # (% of decrease of the risk comparing to the baseline)

reduction_risk = function(data, dis) {
  rr_women = get(paste0("rr_", dis, "_women"))
  rr_men = get(paste0("rr_", dis, "_men"))
  exp_ref_women = get(paste0("ref_", dis, "_w"))
  exp_ref_men = get(paste0("ref_", dis, "_m"))
  
  data[[paste0(dis, "_reduction_risk")]] <- ifelse(                        # Calculate risk reduction percentage
    data$sexe == "Female",
    (1 - rr_women) * data$week_time / exp_ref_women,                       # for women % of decrease for this disease risk
    (1 - rr_men) * data$week_time / exp_ref_men)                           # for men % of decrease for this disease risk
  
  if(any(data$mort_reduction_risk > (1 - 0.45), na.rm = TRUE)) {           # Cap mortality at 55%
    data$mort_reduction_risk <- ifelse(data$mort_reduction_risk > (1 - 0.45), 1 - 0.45, data$mort_reduction_risk)
  }
  return(data)
}


# IC - FUNCTION reduction_risk_IC : Calculate the disease risk reduction percentage for each bound for each individual
reduction_risk_IC = function(data, dis, bound) {
  rr_women_b = get(paste0("rr_", dis, "_women_", bound))
  rr_men_b = get(paste0("rr_", dis, "_men_", bound))
  exp_ref_women = get(paste0("ref_", dis, "_w"))
  exp_ref_men = get(paste0("ref_", dis, "_m"))
  
  data[[paste0(dis, "_reduction_risk")]] <- ifelse(                         # Calculate risk reduction percentage
    data$sexe == "Female",
    (1 - rr_women_b) * data$week_time / exp_ref_women,                       # for women % of decrease for this disease risk
    (1 - rr_men_b) * data$week_time / exp_ref_men)                           # for men % of decrease for this disease risk
  
  if(any(data$mort_reduction_risk > (1 - 0.45), na.rm = TRUE)) { 
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
daly = function(data){
  for (dis in dis_vec) {
    data[[paste0(dis, "_daly")]] <-
    data$years_remaining * get((paste0(dis, "_dw"))) * data[[paste0(dis, "_reduc_incidence")]]
  }
  return(data)
}



#IC - FUNCTION daly_IC : to calculate DALY for IC
daly_IC = function(data,bound) {
  for (dis in dis_vec) {
    data[[paste0(dis, "_daly")]] <- data$years_remaining * get((paste0(dis, "_dw_", bound))) * data[[paste0(dis, "_reduc_incidence")]]
  }
  return(data)
}



##############################################################
#                      ECONOMIC IMPACT                       #
##############################################################

## MEDICAL COSTS ----
# FUNCTION medic_costs : Calculate the medical costs associated with the reduced disease incidence for each individual
medic_costs = function(data, dis) {
  for (dis in dis_vec) {
    data [[paste0(dis, "_medic_costs")]] <-
      get(paste0(dis, "_cost")) * data[[paste0(dis, "_reduc_incidence")]]
  }
  return(data)
}


## SOCIAL COSTS (intangible)----
# FUNCTION burden : Total of prevented cases, DALY and saved costs, for each disease
calc_burden = function(survey_design){
  burden <- data.frame()
  
  for (dis in dis_vec) {
    dis_burden <- survey_design %>% 
      summarise(tot_cases = survey_total(!!sym(paste0(dis, "_reduc_incidence")), na.rm = TRUE),     # Total of prevented cases per disease
                tot_daly = survey_total(!!sym(paste0(dis, "_daly")), na.rm = TRUE),                 # Total of prevented DALY per disease
                tot_medic_costs = survey_total(!!sym(paste0(dis, "_medic_costs")), na.rm = TRUE)    # Total of saved medical costs per disease
      ) %>%             
      mutate(disease = dis)
    
    burden <- bind_rows(burden, dis_burden) 
  }
  return(burden)
}






