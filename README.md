# Health benefits of walking

**Objectives :**

Using individual data from a nationally representative mobility survey, we conducted a burden of disease analysis to assess the incidence of diseases (morbidity) and the number of deaths prevented by walking in 2019.

We assessed the corresponding DALY prevented, direct medical cost savings and the intangible costs prevented based on the value of a statistical life year.

Lastly, based on individual simulations, we assessed the likely additional benefits of shifting scenarios of short (\<2km) car trips to cycling.

**Health impact assessment :**

-   **0_Datasets** : Creation of walkers and drivers datasets

-   **0_Description** : Description of the French 2019 walking levels by age and sex

-   **0_Functions** : All functions used

-   **0_Parameters** : Setting mortality, morbidity parameters and other constants (speed, value of a statistical life year, CO2 emissions)

-   **1_HIA** :

    -   Calculate the disease risk reduction assuming a linear relationship and the corresponding reduced incidence

    -   Calculate DALY (Disability-Adjusted Life Years) prevented for each disease

    -   Calculate the medical costs saved associated to each disease

    -   Calculate the social costs saved based on the value of a statistical life year

-   **1_HIA_loglinear** : same but assume a log linear regression

-   **2_Resampling** :

    -   Monte-Carlo : Generate 1000 RR (following a normal distribution based on RR central values and the lower and upper bounds of the 95% confidence interval provided in the litterature) and calculate the outcomes for each individual. Then per run (RR), calculate the total outcomes for the national population.

    -   Confidence interval :

        -   For each outcome, generate 200 random values of that outcome for each run (following a normal distribution based on the outcome observed for this run and standard deviation based on standard error for this outcome, for the same run). Then, take the quantiles of all those random values (200x1000 values for 1 disease). We get an estimation of the mean and interval confidence for the disease of interest.

        -   Rubin's rule : Combination of the mean-value and the standard-value for each outcome for each run using the Rubin's rule.

-   **3_Modal_shift** : Assess the potential additional public health benefits associated with modal shift scenarios in which a share (10%, 20%, 30%, 40%, 50%) of short car trips (500m, 1km, 1,5 km, 2km) would be shifted to walking.
