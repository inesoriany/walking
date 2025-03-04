###############################################
################ DESCRIPTION #################
##############################################

# Objectifs : Réaliser une description 
  # distances parcourues par âge et par sexe
  # proportion de la marche dans les modes de transports par âge et par sexe

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
#                                                         TRANSPORT                                                            #
################################################################################################################################

# Distribution of modal share of walking and cars depending on distance (part des différents modes selon la distance)
  # On peut retrouver le nombre de personnes par tranche de distance avec l'étude d'Aurélien Bigo

################################################################################################################################
#                                                          WALKING                                                             #
################################################################################################################################

# Total walkers 
pop_walkers <- sum(emp_walkers$pond_indc)
pop_walkers                                 # Everyone walks

# Creation of walking distances categories
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


# Total walked distance per day in 2019
km_total_day <- svytotal(~nbkm_walking, jour)        # Total km per day
km_total_day

km_total <- km_total_day*365.25                       # Total km per year
km_total_IC <- confint(km_total)                      # Confidence interval

svyby(~nbkm_walking, by = ~age_grp.x, jour, svytotal, na.rm = T)  # Total walking distances per day, per age group


# Mean km walked per day
km_mean <- svymean(~nbkm_walking, jour, na.rm = TRUE)         # Mean km per day
km_mean

km_mean_IC <- confint(km_mean)
km_mean_IC


# Mean walking distances per day, per age group
svyby(~nbkm_walking, by = ~age_grp.x, jour, svymean, na.rm = T)

# Mean walking distances per day, per age group and per sex
mean_distance_people <- svyby(~nbkm_walking, by = ~sexe + age_grp.x, jour, svymean, na.rm = T)


# Walking distance depending on age group and sex
mean_distance_people <- jour %>% 
  group_by(sexe , age_grp.x) %>% 
  summarise(mean_distance = survey_mean(nbkm_walking, na.rm = TRUE))

zq <- qnorm(1-0.05/2)

km_walkers = ggplot(mean_distance_people, aes(x = age_grp.x, y = mean_distance,
                                              ymin = mean_distance - zq*mean_distance_se, ymax = mean_distance + zq*mean_distance_se,
                                              fill = sexe)) +
  geom_col(width = 0.7, position = position_dodge2(0.4)) +
  geom_errorbar(position = position_dodge(.7), width = .25) + 
  ylab("Mean distance walked (km per day)") +
  xlab("Age group")
plot(km_walkers)


#  

emp_subset %>%                                
  tabyl(age_grp.x,sexe) %>%                   # cross-tabulate counts of two columns
  adorn_totals(where = "row") %>%             # add a total row
  adorn_percentages(denominator = "col") %>%  # convert to proportions with column denominator
  adorn_pct_formatting() %>%                  # convert proportions to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Age group",
    col_name = "Sexe")

# Distribution of mortality rates












