#################################################
#################  PARAMETERS ###################
#################################################




## WALKING SPEED (Barban et al, 2022) ----
walk_speed <- 4.8  # km/h

## DRIVING SPEED (Kahlmeier, Götschi et al, 2017) ----
paris_car_speed <- 31  # km/h 
urban_car_speed <-  32
rural_car_speed <- 60 





##############################################################
#                          Diseases                          #
##############################################################

## RELATIVE RISKS ----

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



# mortality (Kelly et al.) 
ref_mort = ref_mort_m =ref_mort_w <- 168                         # 168 minutes per week for walking
rr_mort_lb=  rr_mort_men_lb =rr_mort_women_lb<-.83         
rr_mort = rr_mort_men=  rr_mort_women<- .89
rr_mort_ub =rr_mort_men_ub =rr_mort_women_ub <-.96



## DISABILITY WEIGHTS ----
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



## MEDICAL COSTS ----
cc_cost <- 26716
dem_cost <- 22748
bc_cost <- 46968
cvd_cost <- 20938
diab2_cost <- 36514
mort_cost <- NA



##############################################################
#                       Social cost                          #
##############################################################

## VALUE OF A STATISTICAL LIFE YEAR FOR 2019 FRANCE ----
vsl <- 133000



##############################################################
#                      CO2 emissions                         #
##############################################################

# CO2 emissions per km driven
CO2_emit <- 124                    # 124g CO2 per km








