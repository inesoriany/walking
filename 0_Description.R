###############################################
################ DESCRIPTION #################
##############################################

# GOALS : Description
  # Mean walking distance by age group and sex
  # Proportion of people by distance walked


# Files needed :
  # EMP_walkers.xlsx


### 1. LOAD PACKAGES ----

pacman :: p_load(
  rio,          # Data importation
  here,         # Localization of files 
  dplyr,        # Data manipulation
  epikit,       # Age categories creation
  survey,       # Survey management
  srvyr,        # Survey management
  ggplot2       # Data visualization
)

### 2. IMPORT DATA ----

# EMP 2019 subset for walkers
emp_walkers <- import(here("data_clean", "EMP_walkers.xlsx"))

# EMP 2019 : distances for bike, cars, walking
emp <- import(here("data", "emp_dataset_km_bike_and_car_and_walk_individual.csv")) 


### 3. DESCRIPTION ----

# Total population
emp_20_89 <-  emp %>% 
  filter(age >= 20 & age <90)

pop_tot <- sum(emp_20_89$pond_indc)
pop_tot


################################################################################################################################
#                                                          WALKING                                                             #
################################################################################################################################


# Creation of walking distances categories (in case but for now no need)
emp_walkers <- emp_walkers %>% 
  mutate(dist_grp = case_when(
    nbkm_walking < 1                           ~  "0-1 km",
    nbkm_walking >= 1 & nbkm_walking < 2       ~  "1-2 km",
    nbkm_walking >= 2 & nbkm_walking < 5       ~  "2-5 km",
    nbkm_walking >= 5 & nbkm_walking < 10      ~  "5-10 km",
    nbkm_walking >= 10                         ~  "10 km +" 
  ))%>% 
  mutate(dist_grp = as.factor(dist_grp))

    
# Survey design ponderated by day
ponderation_jour <- emp_walkers %>% 
  filter(pond_jour != "NA")

jour <- ponderation_jour %>% 
  as_survey_design(ids = ident_ind,
                   weights = pond_jour,
                   strata = c(sexe, age_grp.x),
                   nest = TRUE)

# Survey design ponderated by individual (for a whole week)
pond_ind <- emp_walkers %>% 
  filter (pond_indc != "NA")

indiv <- pond_ind %>% 
  as_survey_design(ids = ident_ind,
                   weights = pond_indc,
                   strata = c(sexe, age_grp.x),
                   nest = TRUE)


## Total walked distance per day in 2019
km_total_day <- svytotal(~nbkm_walking, jour)                   # Total km per day
km_total_day

km_total_2019 <- km_total_day*365.25/7                          # Total km per year
km_total_2019_IC <- confint(km_total_2019)                      # Confidence interval

# Total walking distances per day, by age group
svyby(~nbkm_walking, by = ~age_grp.x, jour, svytotal, na.rm = T)  


## Mean km walked per day
km_mean <- svymean(~nbkm_walking, jour, na.rm = TRUE)         # Mean km per day
km_mean

km_mean_IC <- confint(km_mean)
km_mean_IC


# Mean walking distances per day, by age group
svyby(~nbkm_walking, by = ~age_grp.x, jour, svymean, na.rm = T)

# Mean walking distances per day, per age group and per sex
mean_distance_people <- svyby(~nbkm_walking, by = ~sexe + age_grp.x, jour, svymean, na.rm = T)




## Plot : Mean walking distance by age group and sex
mean_distance_people <- jour %>% 
  group_by(sexe , age_grp.x) %>% 
  summarise(mean_distance = survey_mean(nbkm_walking, na.rm = TRUE))

zq <- qnorm(1-0.05/2)      # Level of confidence at 95%

mean_km_walkers = ggplot(mean_distance_people, aes(x = age_grp.x, y = mean_distance,
                                              ymin = mean_distance - zq*mean_distance_se, ymax = mean_distance + zq*mean_distance_se,
                                              fill = sexe)) +
  geom_col(width = 0.7, position = position_dodge2(0.4)) +
  geom_errorbar(position = position_dodge(.7), width = .25) + 
  labs(title = "Mean distance walked by age group and sex",
       x = "Age group",
         y = "Mean distance walked (km per day)")
plot(mean_km_walkers)

  # Export plot
ggsave(here("output", "Plot", "plot_mean_km_walkers.tiff"), plot = mean_km_walkers)





## Plot : Proportion of people by distance walked
proportion_km <- indiv %>% 
  group_by(dist_grp = factor(dist_grp, levels = c("0-1 km", "1-2 km", "2-5 km", "5-10 km", "10 km +"))) %>%   # Put distance range in the right order 
  summarise(total_pondere = survey_total (1, na.rm = TRUE)) %>%         # Sum of ponderation of individual for each distance group
  mutate(proportion = total_pondere / sum(total_pondere))               # Proportion

ggplot(proportion_km, aes(x = dist_grp, y = proportion)) +
  geom_col() +
  labs(title = "Proportion of people by distance walked",
       x = "Distance range",
       y = "Proportion")
 



## Mortality rates distribution per sex and age group
mortality <- indiv %>% 
  group_by(sexe, age_grp.x) %>% 
  summarise(mort_rate_mean = survey_mean(mort_rate, proportion = TRUE, na.rm = TRUE))

mean_mortality_rates <- ggplot(mortality, aes(x = age_grp.x, y = mort_rate_mean, fill = sexe)) +
  geom_bar(width = 0.7, position = position_dodge2(0.7), stat = "identity") +
  scale_fill_manual(values = c("Female" = "darkorange1",
                               "Male" = "chartreuse4")) +
  labs (y = "Mean mortality rate",
        x ="Age group") +
  theme_minimal()

  # Export plot
ggsave(here("output", "Plot", "plot_mean_mortality_rates.tiff"), plot = mean_mortality_rates)



################################################################################################################################
#                                                          DRIVING                                                             #
################################################################################################################################


# Proportion of the French adult population reporting any short (<5km) car trip in the past day
# mean distance driven (km) in the past day among those reporting any car trip according to sex and age


