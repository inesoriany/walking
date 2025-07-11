#################################################
########     SENSITIVITY ANALYSIS         #######
#################################################


# Files needed :
  # EMP_walkers.xlsx
  # 0_Functions.R
  # HIA_disease_1000replicate.rds : Main analysis
  # HIA_disease_1000replicate.rds : Linear regression



# Modified parameters - files outputted :
  # Walking speed 5.3 km/h                     ~ HIA_walking_speed.xlsx
  # Linear regression


# Sensitivity analysis - file outputted :
  # sensitivity_analysis.xlsx
  # plot_sensitivity.png





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



################################################################################################################################
#                                                     2. IMPORT DATA                                                           #
################################################################################################################################

emp_walk <- import(here("data_clean", "EMP_walkers.xlsx"))

# Import functions
source(here("0_Functions.R"))


# Import main analysis
HIA_main <- import(here("output", "RDS", "Log linear", "2019", "HIA_disease_1000replicate.rds"))

# Import linear regression
HIA_linear <- import(here("output", "RDS", "Linear", "2019", "HIA_disease_1000replicate.rds"))





################################################################################################################################
################################################################################################################################
#                                                         WALKING SPEED                                                        #
################################################################################################################################
################################################################################################################################

################################################################################################################################
#                                                        3. PARAMETERS                                                         #
################################################################################################################################


# Set parameters by default
source(here("0_Parameters.R"))

# Diseases considered
dis_vec = c("cc", "dem", "bc", "cvd", "diab2", "mort")

# Modified parameters
  # WALKING SPEED (Compendium, 2011) ----
  walk_speed <- 5.3  # km/h



################################################################################################################################
#                                                     4. MODIFY DATASET                                                        #
################################################################################################################################
emp_walk <- emp_walk %>% 
  mutate(week_time = 7*nbkm_walking*60/walk_speed)



################################################################################################################################
#                                          5. HEALTH IMPACT ASSESSMENT (Monte-Carlo)                                           #
################################################################################################################################

health_speed_replicate <- emp_walk

## N replications for HIA outputs (based on random RR generated) per disease ----
burden_speed_replicate <- data.frame()

set.seed(123)
for (dis in dis_vec) {
  burden_dis <- log_burden_prevented_replicate (health_speed_replicate, dis, N = 1000, NULL)                    # HIA for all runs of 1 disease
  burden_speed_replicate <- bind_rows(burden_replicate, burden_dis)                                         # HIA for all diseases
}

# Export the results of HIA outputs for all replications
export(burden_speed_replicate, here("output", "RDS", "Log linear", "Sensitivity analysis", "logHIA_speed_1000replicate.rds"))


################################################################################################################################
#                                                   6. INTERVAL OF CONFIDENCE                                                  #
################################################################################################################################

# IMPORT DATA (to avoid re-generating Monte-Carlo replications)
burden_speed_replicate <- import(here("output", "RDS", "Log linear", "Sensitivity analysis", "logHIA_speed_1000replicate.rds"))


##############################################################
#                      HIA PER DISEASE                       #
##############################################################

burden_speed <- burden_speed_replicate %>% 
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
HIA_speed <- HIA_burden_IC(burden_speed, dis_vec, outcome_vec, calc_replicate_IC)


  # Export results 
export(HIA_speed, here("output", "Tables", "Log linear", "Sensitivity analysis", "HIA_speed_disease.xlsx"))






################################################################################################################################
################################################################################################################################
#                                                     SENSITIVITY ANALYSIS                                                     #
################################################################################################################################
################################################################################################################################

# IMPORT DATA of HIA with the modified walking speed
HIA_speed <- import(here("output", "Tables", "Log linear", "Sensitivity analysis", "HIA_speed_disease.xlsx"))



# Gather all the results in a table
HIA_main <- HIA_main %>% 
  mutate(analysis = "main") %>% 
  select(analysis, disease, tot_cases, tot_cases_low, tot_cases_sup, tot_daly, tot_daly_low, tot_daly_sup) 

HIA_linear <- HIA_linear %>% 
  mutate(analysis = "sc1") %>% 
  mutate(shift_cases = (tot_cases - HIA_main[["tot_cases"]])*100 / HIA_main[["tot_cases"]]) %>%      # Percentage of shift
  mutate(shift_daly = (tot_daly - HIA_main[["tot_daly"]])*100 / HIA_main[["tot_daly"]]) %>% 
  select(analysis, disease, tot_cases, tot_cases_low, tot_cases_sup, shift_cases, tot_daly, tot_daly_low, tot_daly_sup, shift_daly)

HIA_speed <- HIA_speed %>% 
  mutate(analysis = "sc2") %>% 
  mutate(shift_cases = (tot_cases - HIA_main[["tot_cases"]])*100 / HIA_main[["tot_cases"]]) %>%      # Percentage of shift
  mutate(shift_daly = (tot_daly - HIA_main[["tot_daly"]])*100 / HIA_main[["tot_daly"]]) %>% 
  select(analysis, disease, tot_cases, tot_cases_low, tot_cases_sup, shift_cases, tot_daly, tot_daly_low, tot_daly_sup, shift_daly)

all_data <- bind_rows(HIA_main, HIA_linear, HIA_speed)

# Cases
cases_data <- all_data %>%
  transmute(analysis, disease,
            outcome = "tot_cases",
            value = tot_cases,
            low = tot_cases_low,
            sup = tot_cases_sup,
            shift = shift_cases)

# DALY
daly_data <- all_data %>%
  transmute(analysis, disease,
            outcome = "tot_daly",
            value = tot_daly,
            low = tot_daly_low,
            sup = tot_daly_sup,
            shift = shift_daly)

# All data
sensi_data <- bind_rows(cases_data, daly_data)


  # Export sensitivity analysis
export(sensi_data, here("output", "Tables", "Log linear", "Sensitivity analysis", "sensitivity_analysis.xlsx"))




# Plot : Sensitivity analysis of the number health events prevented under different assumptions

labels_analysis <- c(
  "main" = "Main Analysis",
  "sc1"  = "Linear DRF regression",
  "sc2" = "Walking speed 5.3 km/h"
)

labels_disease <- c(
  "bc" = "Breast cancer",        
  "cc" = "Colon cancer",
  "cvd" = "CVD",
  "dem" = "Dementia",
  "diab2" = "T2 Diabetes",
  "dep" = "Depression",
  "mort" = "Mortality"
)



plot_sensi <- ggplot(sensi_data) +
  geom_point(aes(x = value,
                 y = disease,
                 color = factor(disease)),
             position = position_dodge(width = 0.5),
             size = 2.5,
             shape = 18) +
  geom_segment(aes(x = low,
                   xend = sup,
                   y = disease,
                   yend = disease,
                   color = factor(disease)),
               position = position_dodge(width = 0.5),
               linewidth = 0.3) +
  facet_grid(
    rows = vars(analysis),
    cols = vars(outcome),
    labeller = labeller(
      analysis = labels_analysis,
      outcome = c("tot_cases" = "Prevented cases", "tot_daly" = "Prevented DALYs")
    ),
    scales = "free_x"
  ) +
  scale_color_manual(
    values = c(
      "bc" = "firebrick2",
      "cc" = "darkorange",
      "cvd" = "gold",
      "dem" = "pink",
      "diab2" = "palegreen3",
      "dep" = "slateblue",
      "mort" = "steelblue"
    ),
    labels = labels_disease
  ) +
  labs(
    title = "",
    y = NULL,
    x = NULL,
    color = "Disease"
  ) +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0, size = 9),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 7),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(labels = scales::label_comma()) +
  geom_vline(data = data.frame(outcome = c("tot_cases", "tot_daly"),
                               xintercept = c(10000, 50000)),
             aes(xintercept = xintercept),
             linetype = "dashed", color = "black", linewidth = 0.3) +
  guides(color = guide_legend(title = NULL))


plot_sensi 

  # Export 
ggsave(here("output", "Plots", "Log linear", "Sensitivity analysis", "plot_sensitivity.png"))







