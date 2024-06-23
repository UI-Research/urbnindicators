####----Load Test Data----####

## Statistics for NJ Counties
df = urbnindicators::compile_acs_data(
  variables = urbnindicators::list_acs_variables(year = "2022"),
  years = 2022,
  geography = "county",
  states = "NJ",
  counties = NULL,
  retain_moes = TRUE,
  spatial = FALSE)

codebook = attr(df, "codebook")

####----TESTING----####

codebook1 %>% View()

## raw estimate variable: employment_civilian_labor_force_employed
## se formula: margin of error / 1.645
## cv formula: se / estimate * 100
## estimate: 186469
## moe: 2128
## expected se = 2128 / 1.645 = 1293.617
## expected cv = 1293.617 / 186469 * 100 = 0.6937437 = ~ .694

expected_employed_labor_force_cv = .694
test_that(
  "cv calculation for employment_civilian_labor_force_employed is correct",
  { expect_equal(
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

expected_age_under_5_years_cv = .173
test_that(
  "cv calculation for employment_civilian_labor_force_employed is correct",
  { expect_equal(
    round(df %>%
            dplyr::filter(NAME == "Mercer County, New Jersey") %>%
            dplyr::select(matches("age_under_5_years")) %>%
            dplyr::pull(age_under_5_years_cv), 3),
    expected_age_under_5_years_cv)})

## percent simple: snap_received_percent
## se formula:
##  (1 / estimate_denominator) * sqrt( se(numerator)^2     - ((numerator^2      /  denominator^2)   * (se(denominator)^2) ))
##  (1 / snap_universe)        * sqrt( se(snap_received)^2 - ((snap_received^2  /  snap_universe^2) * (se(snap_universe)^2) )))
##  (1 / 139549)               * sqrt( (1067/1.645)^2      - ((12623^2          /  139549^2)        * (830 / 1.645)^2) )
## expected se: 0.004659553
## expected cv: 0.004659553 / .0905 * 100 = 5.148677 = ~ 5.14

## percent numerator sum: disability_percent
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

variables_with_cvs[!variables_with_cvs %in% all_variables]
all_variables[!all_variables %in% variables_with_cvs]
