README
================
8/9/2021

## Article information

Title: A delayed effect of the aquatic parasite Margaritifera laevis on
the growth of the salmonid host fish Oncorhynchus masou masou

Author: Ooue et al.

Journal: Limnology

DOI: <https://doi.org/10.1007/s10201-017-0514-2>

## File description

-   `figure_dominance.R` script for the relationship between dominance
    and condition factor
-   `figure_growth.R` script for the relationship between condition
    factor and dominance
-   `inits_model_aqua.R` script for running a jags model
-   `model_aqua` script for a jags model
-   `data_fmt/`
    -   `data_aquarium_fmt.csv` experimental data
        -   ID: individual ID
        -   col: VIE color
        -   sex: sex
        -   infection: infection status of the individual. One if
            infected otherwise zero
        -   treatment: treatment code. P: infection treatment, C:
            control with no infection
        -   aquarium: aquarium tank ID
        -   L0: fork length at day 0 (mm)
        -   L50: fork length at day 50 (mm)
        -   L70: fork length at day 70 (mm)
        -   W0: wet weight at day 0 (g)
        -   W50: wet weight at day 50 (g)
        -   W70: wet weight at day 70 (g)
        -   dominance: dominance status. One if dominant otherwise
            subordinate
    -   `2017-01-18W50re.csv` JAGS estimates at day 50
    -   `2017-01-18W70re.csv` JAGS estimates at day 70
