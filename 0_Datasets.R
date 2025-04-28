###############################################
############ CREATING DATASETS ################
###############################################

# Files needed :
  # emp_dataset_km_bike_and_car_and_walk_individual.csv
  # out_merged.csv
  # INSEE_2019.RDS


# Files outputted :
  # EMP_walkers.xlsx
  # EMP_drivers.xlsx


################################################################################################################################
################################################################################################################################
#                                                      WALKERS DATABASE                                                        #
################################################################################################################################
################################################################################################################################


################################################################################################################################
#                                                    1. LOAD PACKAGES                                                          #
################################################################################################################################

pacman :: p_load(
  rio,          # Data importation
  here,         # Localization of files 
  dplyr,        # Data manipulation
  forcats,      # Factor conversion
  epikit,       # Age categories creation
  janitor,      # De-duplication
  survey        # Survey management
)


################################################################################################################################
#                                                     2. IMPORT DATA                                                           #
################################################################################################################################

# EMP 2019 : distances for bike, cars, walking
emp <- import(here("data", "emp_dataset_km_bike_and_car_and_walk_individual.csv")) 

# Diseases and mortality data
diseases <- import(here("data", "out_merged.csv"))

# INSEE data
insee <- import(here("data", "INSEE_2019.RDS"))


################################################################################################################################
#                                                    3. SETTING THE CONSTANTS                                                  #
################################################################################################################################
walk_speed <- 4.8  # km/h



################################################################################################################################
#                                  4. CREATION OF SUBSET OF EMP SUBSET WITH ONLY VARIABLES NEEDED                              #
################################################################################################################################

# Creation of subset uniting variables of EMP and calculations
emp_subset <- emp %>% 
  select(
    ident_ind,
    sexe,
    age,
    quartile_rev,
    pond_indc,
    pond_jour,
    nbkm_walking_lower,
    nbkm_walking_upper,
    mdisttot_fin1
  )

# Week time spent walking (min)
emp_subset <- emp_subset %>% 
  mutate(week_time = 7*nbkm_walking_lower*60/walk_speed) %>% 
  mutate(week_time_upper = 7*nbkm_walking_upper*60/walk_speed)

# Re-write nbkm_walking, easier to be used in functions
emp_subset <- emp_subset %>%
  rename(nbkm_walking = nbkm_walking_lower) %>% 

# Re-write sexe as female and male and convert as factors
  mutate(sexe = as.character(sexe)) %>%                                 # Conversion in character for function to work well
  mutate(sexe = fct_recode(sexe, "male" = "1", "female" = "2")) %>%     # Replacing
  rename(sex = sexe) 


# Create age categories
emp_subset <- emp_subset %>% 
  mutate(
    age_grp.x = age_categories(
      age,
      lower = 0,
      upper = 110,
      by = 5)
    ) %>% 
  mutate(age_grp.x = as.character(age_grp.x))


# Add population counts per sex
emp_subset <- emp_subset %>% 
  left_join(
      diseases %>% select(pop_age_grp, sex, age_grp.x),    # Matching columns
      by = c("sex", "age_grp.x")                           # Fill the variables depending on sex and age group
    ) %>% 
  rename(pop_age_sex = pop_age_grp)


# Add diseases incidences
emp_subset <- emp_subset %>% 
  left_join(
    diseases %>% select(cc_incidence, dem_incidence, bc_incidence, cvd_incidence, diab2_incidence, sex, age_grp.x),    # Matching columns
    by = c("sex", "age_grp.x")                                                                                         # Fill the variables depending on sex and age group
  ) 

# Add mortality rates
emp_subset <- emp_subset %>% 
  rename(sexe = sex) %>%                                                      # Rename to match INSEE sexe columns
  mutate(sexe = as.factor(sexe)) %>% 
  mutate(sexe = fct_recode(sexe, "Male" = "male", "Female" = "female")) %>%   # Rename to match INSEE sexe
  mutate(sexe = fct_relevel(sexe, "Male", "Female"))


emp_subset <- emp_subset %>% 
  left_join(
    insee %>% select(MR, sexe, age),       # Matching columns
    by = c("sexe", "age")                  # Fill the variables depending on sexe and age
  ) %>% 
  rename(mort_rate = MR)


# Calculate incidence rates
dis <- c("cc_incidence", "dem_incidence", "bc_incidence", "cvd_incidence", "diab2_incidence")

for (i in 1:nrow(emp_subset)) {
  for(j in dis) {
    if (!is.na(emp_subset[i, "pop_age_sex"])) {
      emp_subset[i, paste0(j, "_rate")] <- emp_subset[i, j] / emp_subset[i, "pop_age_sex"]
    } else {
      emp_subset[i, paste0(j, "_rate")] <- NA        
      }
  }
}


# Add life-expectancy for each sex
for (i in 1:nrow(emp_subset)) {
  emp_subset[i, "life_exp"] = ifelse (
    emp_subset$sexe[i]== "Female", 
    85.99324,                            # Life expectancy for women = 85.99324
    79.59503                             # Life expectancy for men = 79.59503
    )  
  }

# Add the years of life remaining, potentially affected by diseases or premature death
for (i in 1:nrow(emp_subset)) {
  emp_subset[i,"years_remaining"] = ifelse(
    emp_subset$life_exp[i]-emp_subset$age[i] >=0,
    emp_subset$life_exp[i]-emp_subset$age[i],
    0                                                   # No negatives for individuals above life expectancy
    )
}

# Only keep ages 20-89 years 
emp_subset <-  emp_subset %>% 
  filter(age >= 20 & age <90)


# Remove column mdisttot_fin1 (not useful for walkers)
emp_walkers <- emp_subset %>% 
  select(-mdisttot_fin1)


################################################################################################################################
#                                                    5. EXPORT EMP SUBSET                                                      #
################################################################################################################################

export(emp_walkers, here("data_clean", "EMP_walkers.xlsx"))






################################################################################################################################
################################################################################################################################
#                                                      DRIVERS DATABASE                                                        #
################################################################################################################################
################################################################################################################################

################################################################################################################################
#                                  4. CREATION OF SUBSET OF EMP SUBSET WITH ONLY VARIABLES NEEDED                              #
################################################################################################################################

# Selecting only variables of interests for drivers / removing non-relevant variables 
emp_drivers <- emp_subset %>% 
  select(-nbkm_walking,
         -nbkm_walking_upper,
         -week_time,
         -week_time_upper)


# Week time spent walking if those car distances were walked (min)
emp_drivers <- emp_drivers %>% 
  mutate(week_time_shift = 7*mdisttot_fin1*60 / walk_speed)



################################################################################################################################
#                                                    5. EXPORT EMP SUBSET                                                      #
################################################################################################################################

export(emp_drivers, here("data_clean", "EMP_drivers.xlsx"))











