####----TESTING----####

## raw estimate variable: employment_civilian_labor_force_employed
## se formula: margin of error / 1.645
## cv formula: se / estimate * 100
## estimate: 186469
## moe: 2128
## expected se = 2128 / 1.645 = 1293.617
## expected cv = 1293.617 / 186469 * 100 = 0.6937437 = ~ .694


test_that(
  "cv calculation for employment_civilian_labor_force_employed is correct",
  {
    expected_employed_labor_force_cv = .694

    expect_equal(
    round(df %>%
      dplyr::filter(NAME == "Mercer County, New Jersey") %>%
      dplyr::select(matches("employment_civilian_labor_force_employed")) %>%
      dplyr::pull(employment_civilian_labor_force_employed_cv), 3),
    expected_employed_labor_force_cv)})

## sum: age_under_5_years
## se_formula: sqrt(sum of squared standard errors)
##  sqrt(se(sex_by_age_female_under_5_years)^2 + se(sex_by_age_male_under_5_years)^2)
##  sqrt((3 / 1.645)^2 + (60 / 1.645) ^2)
## estimate: 21160
## moe: 2128
## expected se = 36.51973
## expected cv = 36.51973 / 21160 * 100 = 0.1725885 = ~ .173

expected_age_under_5_years_cv = .17
test_that(
  "cv calculation for employment_civilian_labor_force_employed is correct",
  { expect_equal(
    round(df %>%
            dplyr::filter(NAME == "Mercer County, New Jersey") %>%
            dplyr::select(matches("age_under_5_years")) %>%
            dplyr::pull(age_under_5_years_cv), 2),
    expected_age_under_5_years_cv)})

## percent simple: snap_received_percent
## se formula:
##  (1 / estimate_denominator) * sqrt( se(numerator)^2     - ((numerator^2      /  denominator^2)   * (se(denominator)^2) ))
##  (1 / snap_universe)        * sqrt( se(snap_received_M)^2 - ((snap_received^2  /  snap_universe^2) * (se(snap_universe_M)^2) ))
##  (1 / 139549)               * sqrt( (1067/1.645)^2      - ((12623^2          /  139549^2)        * (830 / 1.645)^2) )
## expected se: 0.004659553
## expected cv: 0.004659553 / .0905 * 100 = 5.148677 = ~ 5.15

## note - due to rounding, the calculated cv is not exactly 5.15
expected_snap_received_percent_cv = 5
test_that(
  "cv calculation for snap_received_percent is correct",
  { expect_equal(
    round(df %>%
            dplyr::filter(NAME == "Mercer County, New Jersey") %>%
            dplyr::select(matches("snap")) %>%
            dplyr::pull(snap_received_percent_cv), 0),
    expected_snap_received_percent_cv)})

## percent numerator sum: disability_percent
## disability_count_variables = c(
## sex_by_age_by_disability_status_male_under_5_years_with_a_disability, sex_by_age_by_disability_status_male_5_17_years_with_a_disability, sex_by_age_by_disability_status_male_18_34_years_with_a_disability, sex_by_age_by_disability_status_male_35_64_years_with_a_disability, sex_by_age_by_disability_status_male_65_74_years_with_a_disability, sex_by_age_by_disability_status_male_75_years_over_with_a_disability, sex_by_age_by_disability_status_female_under_5_years_with_a_disability, sex_by_age_by_disability_status_female_5_17_years_with_a_disability, sex_by_age_by_disability_status_female_18_34_years_with_a_disability, sex_by_age_by_disability_status_female_35_64_years_with_a_disability, sex_by_age_by_disability_status_female_65_74_years_with_a_disability, sex_by_age_by_disability_status_female_75_years_over_with_a_disability)
## se formula:
##  (1 / estimate_denominator) * sqrt( se(numerator)^2 - ((numerator^2 / denominator^2) * (se(denominator)^2) ))
##
##  (1 / sex_by_age_by_disability_status_universe)  *
##    sqrt(
##      se_sum(disability_count_variables_M)^2 -
##      ((sum(disability_count_variables)^2 / sex_by_age_by_disability_status_universe^2) * (se(sex_by_age_by_disability_status_universe_M)^2)
##
##  square root of the summed standard errors
##
##  (1 / 378850) *
##    sqrt(
##      se_sum(disability_count_variables_M)^2 -
##      ((sum(disability_count_variables)^2 / 378850^2) * (se_simple(189)^2)))
##
## expected se:
## expected cv:

## percent variables where the denominator does not have an MOE

disability_count_variables = df %>%
  dplyr::filter(NAME == "Mercer County, New Jersey") %>%
  dplyr::select(c(matches("with_a_disability"), -matches("_M$|cv$"))) %>%
  colnames

disability_count_variables_M = df %>%
  dplyr::filter(NAME == "Mercer County, New Jersey") %>%
  dplyr::select(c(matches("with_a_disability.*_M$"))) %>%
  colnames

df %>%
  dplyr::filter(NAME == "Mercer County, New Jersey") %>%
  dplyr::transmute(
    t1 = 1 / 378850,
    radical1 = se_sum(purrr::map(disability_count_variables_M, ~ df %>% dplyr::filter(NAME == "Mercer County, New Jersey") %>% dplyr::pull(.x))) ^2,
    radical2a = rowSums(dplyr::select(., disability_count_variables))^2 / 378850^2,
    radical2b = se_simple(189)^2,
    expected_se = t1 * sqrt( (radical1 - (radical2a * radical2b) )),
    expected_cv = expected_se / disability_percent * 100, ## 2.56
    actual_cv = disability_percent_cv) ## 16349


df %>%
  dplyr::select(matches("cv$")) %>%
  tidyr::pivot_longer(everything()) %>%
  dplyr::arrange(desc(value)) %>%
  dplyr::filter(value > 100) %>% dplyr::pull(name)

985044

df$disability_percent
    expected_value = (1 / 378850) * # 0.000002639567
     sqrt(
       se_sum(purrr::map(disability_count_variables_M, ~ df %>% dplyr::filter(NAME == "Mercer County, New Jersey") %>% dplyr::pull(.x))) ^2 - # 985044
       ((rowSums(dplyr::select(., disability_count_variables))^2 / 378850^2) * (se_simple(189)^2)))) # .0105 * 13200.5

0.000002639567 * sqrt((985044 - (.0105 * 13200.5)))

## percent no moe denominator: race_nonhispanic_white_alone_percent

variables_with_cvs = df %>%
  dplyr::select(matches("cv$")) %>%
  colnames %>%
  stringr::str_remove_all("_cv")

all_variables = df %>%
  dplyr::select(-c(
    matches("cv$|_M$"),
    any_of(codebook %>% dplyr::filter(variable_type == "Metadata") %>% dplyr::pull(calculated_variable)))) %>%
  colnames

## there are no spontaneously-generated cv-variables
variables_with_cvs[!variables_with_cvs %in% all_variables]

## all variables (except metadata variables) should have a cv, except for the following
  ## area variables (3) - these are based on geometries
  ## population density (1) - this is based on geometry and total population
  ## total population, total race, total sex by age (3) - these do not have MOEs
all_variables[!all_variables %in% variables_with_cvs]


