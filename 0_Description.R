###############################################
################ DESCRIPTION #################
##############################################

# GOALS : Description
  # WALKING : 
    # Mean walking distance by age group and sex
    # Proportion of people by distance walked
    # Rate of deadly accidents
  # DRIVING :
    # Proportion of people reporting any short (<2km) car trip
    # Mean length of short car travel <2km 
    # Distribution of people reporting any short trips (0.5 - 1 - 1.5 - 2km) (mutually exclusive)

# Files needed :
  # EMP_walkers.xlsx


################################################################################################################################
#                                                    1. LOAD PACKAGES                                                          #
################################################################################################################################

pacman :: p_load(
  rio,          # Data importation
  here,         # Localization of files 
  dplyr,        # Data manipulation
  epikit,       # Age categories creation
  survey,       # Survey management
  srvyr,        # Survey management
  ggplot2       # Data visualization
)


################################################################################################################################
#                                                     2. IMPORT DATA                                                           #
################################################################################################################################

# EMP 2019 : distances for bike, cars, walking
emp <- import(here("data", "emp_dataset_km_bike_and_car_and_walk_individual.csv")) 

# EMP 2019 subset for walkers
emp_walkers <- import(here("data_clean", "EMP_walkers.xlsx"))

# EMP 2019 subset for walkers
emp_drivers <- import(here("data_clean", "EMP_drivers.xlsx"))



################################################################################################################################
################################################################################################################################
#                                                      3. DESCRIPTION                                                          #
################################################################################################################################
################################################################################################################################


# Total population
emp_20_89 <-  emp %>% 
  filter(age >= 20 & age <90)

pop_tot <- sum(emp_20_89$pond_indc)
pop_tot


##############################################################
#                        SURVEY DESIGNS                      #
##############################################################

# Survey design ponderated by day
jour <- emp_walkers %>% 
  filter(pond_jour != "NA") %>% 
  as_survey_design(ids = ident_ind,
                   weights = pond_jour,
                   strata = c(sexe, age_grp.x),
                   nest = TRUE)

# Survey design ponderated by individual
indiv <- emp_walkers %>% 
  filter (pond_indc != "NA") %>% 
  as_survey_design(ids = ident_ind,
                   weights = pond_indc,
                   strata = c(sexe, age_grp.x),
                   nest = TRUE)



################################################################################################################################
#                                                           HEALTH                                                             #
################################################################################################################################

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
plot(mean_mortality_rates)

# Export plot
ggsave(here("output", "Plots", "Description", "plot_mean_mortality_rates.tiff"), plot = mean_mortality_rates)




################################################################################################################################
#                                                          WALKING                                                             #
################################################################################################################################



##############################################################
#                  TOTAL WALKED DISTANCE                     #
##############################################################

## Total walked distance in 2019
km_total_2019 <- as.numeric(svytotal(~nbkm_walking, jour)) *365.25/7                              # Total km per year
km_total_2019_IC <- as.numeric(confint(svytotal(~nbkm_walking, jour) *365.25/7 ))                 # Confidence interval


## Total walked distance per day in 2019
km_total_day <- svytotal(~nbkm_walking, jour)                   # Total km per day
km_total_day


# Total walking distances per day, by age group
svyby(~nbkm_walking, by = ~age_grp.x, jour, svytotal, na.rm = T)  



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


## Plot : Proportion of people by distance walked
proportion_km <- emp_walkers %>% 
  filter (pond_indc != "NA") %>% 
  as_survey_design(ids = ident_ind,
                   weights = pond_indc,
                   strata = c(sexe, age_grp.x),
                   nest = TRUE) %>% 
  group_by(dist_grp = factor(dist_grp, levels = c("0-1 km", "1-2 km", "2-5 km", "5-10 km", "10 km +"))) %>%   # Put distance range in the right order 
  summarise(total_pondere = survey_total (1, na.rm = TRUE)) %>%         # Sum of ponderation of individual for each distance group
  mutate(proportion = total_pondere / sum(total_pondere))               # Proportion

prop_walkers_km <- ggplot(proportion_km, aes(x = dist_grp, y = proportion)) +
  geom_col() +
  labs(title = "Proportion of people by distance walked",
       x = "Distance range",
       y = "Proportion") +
  theme_minimal()

# Export plot
ggsave(here("output", "Plots", "Description", "plot_prop_walkers_km.tiff"), plot = prop_walkers_km)


##############################################################
#                    MEAN WALKED DISTANCE                    #
##############################################################
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
ggsave(here("output", "Plots", "Description", "plot_mean_km_walkers.tiff"), plot = mean_km_walkers)




##############################################################
#                  RATE OF DEADLY ACCIDENTS                  #
##############################################################

# Rate of deadly accidents per km from 2019 levels
deaths_per_km_walked <- 483 / km_total_2019                           # Number of dead walkers per km in 2019 (ONISR 2020 - Bilan 2019)
deaths_per_km_walked






################################################################################################################################
#                                                          DRIVING                                                             #
################################################################################################################################

##############################################################
#                     SHORT TRIPS (<2km)                     #
##############################################################

# French adult reporting any short (<2km) car trip in the past day according to sex and age
drivers_2km <- emp_drivers %>% 
  filter(pond_jour != "NA", mdisttot_fin1 > 0) %>% 
  as_survey_design(ids = ident_ind,
                   weights = pond_jour) %>% 
  group_by(sexe, age_grp.x) %>% 
  summarise(total = survey_total(mdisttot_fin1 <= 2, na.rm = TRUE)) %>% 
  rename(Sex = sexe)

zq <- qnorm(1-0.05/2)

nb_drivers_2km <- ggplot(drivers_2km, aes(x = age_grp.x, y = total,
                                            ymin = total - zq*total_se, ymax = total + zq*total_se, fill = Sex)) +
  geom_col(width = 0.7, position = position_dodge2(0.4))+
  geom_errorbar(position = position_dodge(0.7), width = 0.25) +
  ylab ("Number of drivers driving <2km in the past day") +
  xlab("Age group") +
  theme_minimal()
plot(nb_drivers_2km)




# Proportion of the French adult population reporting any short (<2km) car trip in the past day according to sex and age
mean_drivers_2km <- emp_drivers %>% 
  filter(pond_jour != "NA", mdisttot_fin1 > 0) %>% 
  as_survey_design(ids = ident_ind,
                   weights = pond_jour) %>% 
  group_by(sexe, age_grp.x) %>% 
  summarise(perc = 100 * survey_mean(mdisttot_fin1 <= 2, na.rm = TRUE)) %>% 
  rename(Sex = sexe)

zq <- qnorm(1-0.05/2)

perc_drivers_2km <- ggplot(mean_drivers_2km, aes(x = age_grp.x, y = perc,
                                            ymin = perc - zq*perc_se, ymax = perc + zq*perc_se, fill = Sex)) +
  geom_col(width = 0.7, position = position_dodge2(0.4))+
  geom_errorbar(position = position_dodge(0.7), width = 0.25) +
  ylab ("% driving <2km in the past day") +
  xlab("Age group") +
  theme_minimal()
plot(perc_drivers_2km)

  # Export plot
ggsave(here("output", "Plots", "Description", "plot_drivers_2km.tiff"), plot = perc_drivers_2km)




# Distribution of people reporting any short trips (mutually exclusive)
short_trips <- emp_drivers %>%
  as_survey_design(ids = ident_ind,
                   weights = pond_indc) %>% 
  mutate(class_dist = case_when(
    mdisttot_fin1 <= 0.5                           ~ "≤0.5km",                                 
    mdisttot_fin1 > 0.5 & mdisttot_fin1 <= 1       ~ "0.5–1km",  
    mdisttot_fin1 > 1   & mdisttot_fin1 <= 1.5     ~ "1–1.5km",   
    mdisttot_fin1 > 1.5 & mdisttot_fin1 <= 2       ~ "1.5–2km",   
  )) %>%
  group_by(class_dist) %>%
  summarise(tot_drivers = survey_total(mdisttot_fin1))


  # Plot : Distribution of people reporting any short trips (mutually exclusive)
zq <- qnorm(1-0.05/2)
short_trips_2km <- ggplot(short_trips, aes(x = class_dist, y = tot_drivers,
                                               ymin = tot_drivers - zq*tot_drivers_se, ymax = tot_drivers + zq*tot_drivers_se)) +
  geom_col(width = 0.9) +
  geom_errorbar(position = position_dodge(0.7), width = 0.25) +
  ylab ("Number of drivers") +
  xlab("Distance (km)") +
  theme_minimal() 
plot(short_trips_2km)

ggsave(here("output", "Plots", "Description", "plot_drivers_shorttrips.tiff"), plot = short_trips_2km)







##############################################################
#                MEAN DRIVEN DISTANCE (<2km)                 #
##############################################################

# Mean distance driven (km) in the past day among those reporting short car trips <2km according to sex and age
mean_drivers_2km <- emp_drivers %>% 
  filter(pond_jour !="NA", mdisttot_fin1 > 0, mdisttot_fin1 <= 2) %>% 
  as_survey_design(ids = ident_ind, 
                   weights = pond_jour) %>% 
  group_by(sexe, age_grp.x) %>% 
  summarise(day_mean = survey_mean(mdisttot_fin1, na.rm = TRUE)) %>% 
  rename(Sex = sexe)


mean_km_drivers_2km <- ggplot(mean_drivers_2km, aes(x = age_grp.x, y = day_mean,
                                            ymin = day_mean - zq*day_mean_se, ymax = day_mean + zq*day_mean_se, fill = Sex)) +
  geom_col(width = 0.7, position = position_dodge2(0.4)) +
  geom_errorbar(position = position_dodge(0.7), width = 0.25) +
  ylab ("Mean length of short car travel <2km (km)") +
  xlab("Age group") +
  theme_minimal() 
plot(mean_km_drivers_2km)

  # Export plot
ggsave(here("output", "Plots", "Description", "plot_mean_drivers_2km.tiff"), plot = mean_km_drivers_2km)


