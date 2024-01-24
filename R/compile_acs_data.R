#' A modified division operation that returns zero when the denominator is zero
#' rather than returning NA. Otherwise returns the dividend.
#'
#' @param x A numeric scalar.
#' @param y A numeric scalar.
#' @returns The traditional dividend in all cases except where `y` == 0, in which
#'    case it returns 0.
#' @examples
#' safe_divide(1, 2) ## .5
#' safe_divide(3, 0) ## 0
#' @export
safe_divide = function(x, y) { dplyr::if_else(y == 0, 0, x / y) }

#' Calculate constructs frequently used in social sciences research while leveraging
#' [tidycensus::get_acs()] to acquire raw estimates from the Census Bureau API.
#'
#' @param variables A named vector of ACS variables such as that returned from
#'    [urbnindicators::list_acs_variables()].
#' @param years A character vector (or coercible to the same) comprising one or more
#'    four-digit years for which to pull five-year American Community Survey estimates.
#' @param geography A geography type that is accepted by [tidycensus::get_acs()], e.g.,
#'    "tract", "county", "state", among others.
#' @param states A vector of one or more state names, abbreviations, or codes as
#'    accepted by [tidycensus::get_acs()].
#' @param counties A vector of one or more county names, abbreviations, or codes as
#'    accepted by [tidycensus::get_acs()]. NOTE: this parameter is not currently supported
#'    and must be set to NULL (as it is by default).
#' @param retain_moes Boolean. Include margins of error (MOE) in the returned dataframe,
#'    or omit them?
#' @seealso [tidycensus::get_acs()], which this function wraps.
#' @returns A dataframe containing the requested `variables`, their MOEs (optionally),
#'    a series of derived variables, such as percentages, and the year of the data.
#'    Returned data are formatted wide.
#' @examples
#' acs_variables = list_acs_variables(year = "2022")
#' compile_acs_data(
#'   variables = acs_variables,
#'   years = c(2021, 2022),
#'   geography = "county",
#'   states = "NJ",
#'   counties = NULL,
#'   retain_moes = TRUE)
#' @export
#' @importFrom magrittr %>%
compile_acs_data = function(
    variables = NULL,
    years = c(2022),
    geography = "county",
    states = NULL,
    counties = NULL,
    retain_moes = FALSE) {

warning(
"Variable names and geographies for ACS data products can change between years.
Changes to geographies are particularly significant across decades (e.g., from 2019 to 2020), but these changes can occur in any year.
Users should ensure that the logic embedded in this function--which was developed around five-year ACS estimates for 2017-2021--remains accurate for their use cases.
Evaluation of measures and geographies over time should be thoroughly quality checked.\n")

  ## default values for the variables and states arguments.
  if (length(variables) == 0) { variables = list_acs_variables(year = years[1]) }
  if (length(states) == 0) { states =  tigris::fips_codes %>% dplyr::filter(!state %in% c("PR", "UM", "VI", "GU", "AS", "MP")) %>% dplyr::pull(state) %>% unique() }

  super_state_geographies = c(
    "us", "region", "division", "metropolitan/micropolitan statistical area", "metropolitan statistical area/micropolitan statistical area",
    "cbsa", "urban area", "zip code tabulation area", "zcta")

  ## some geographies are not available by state and can only be returned nationally
  if (geography %in% super_state_geographies) {
    df = purrr::map_dfr(
      ## when year is a vector with length > 1 (i.e., there are multiple years)
      ## loop over each iterm in the vector (and this approach also works for a single year)
      years,
      ~ tidycensus::get_acs(
          geography = geography,
          variables = variables,
          year = as.numeric(.x),
          survey = "acs5",
          output = "wide") %>%
        dplyr::mutate(data_source_year = .x))
  } else {
    ## for those geographies that can (or must) be returned by state:
    ## a tidycensus::get_acs() call using map_dfr to iteratively make calls for data from each state
    ## and then combine the resulting dataframes together into a single dataframe
    df = purrr::map_dfr(
      states,
      function (state) {
        purrr::map_dfr(
          ## when year is a vector with length > 1 (i.e., there are multiple years)
          ## loop over each item in the vector (and this approach also works for a single year)
          years,
          ~ tidycensus::get_acs(
              geography = geography,
              variables = variables,
              year = as.numeric(.x),
              state = state,
              county = counties,
              survey = "acs5",
              output = "wide") %>%
            dplyr::mutate(data_source_year = .x))})}

  if (retain_moes == TRUE) { moes = df %>% dplyr::select(GEOID, data_source_year, dplyr::matches("_M$")) }

  df %>%
    ## drop margin of error variables for calculations since these only relate to raw
    ## variables. these are joined back to the dataframe at the end of this process
    ## if retain_moes == T
    dplyr::select(-dplyr::matches("_M$")) %>%
    dplyr::rename_with(~ stringr::str_remove(.x, "_E$")) %>% ## removing "_E" (for "Estimate") from column names
    dplyr::mutate(
      ####----INCOME, POVERTY, FINANCIAL ASSISTANCE----####
      snap_received_percent = safe_divide(snap_received, snap_universe),
      public_assistance_received_percent = safe_divide(public_assistance_received, public_assistance_universe),
      dplyr::across(
        .cols = dplyr::matches("federal_poverty_limit.*below"),
        .fns = ~ safe_divide(.x, get( dplyr::cur_column() %>% stringr::str_replace("below", "universe") )),
        .names = "{.col}_percent"),
      cost_burdened_30percentormore_allincomes_percent = safe_divide(
        ## numerator -- all households where gross rent is 30% or more of household income
        rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*(30_0|35_0|40_0|50_0).*percent"))),
        ## denominator -- all households with computed rent shares
        rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*([0-9]$|100000_more$)"))) - rowSums(dplyr::select(., dplyr::matches("household_income.*not_computed")))),
      cost_burdened_50percentormore_allincomes_percent = safe_divide(
        ## numerator -- all households where gross rent is 50% or more of household income
        rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*50_0.*percent"))),
        ## denominator -- all households with computed rent shares
        rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*([0-9]$|100000_more$)"))) - rowSums(dplyr::select(., dplyr::matches("household_income.*not_computed")))),
      cost_burdened_30percentormore_incomeslessthan35000_percent = safe_divide(
        ## numerator -- all households where gross rent is 30% or more of household income
        rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*(10000_|19999|34999).*(30_0|35_0|40_0|50_0).*percent"))),
        ## denominator -- all households whose household incomes are $34,999 or less
        rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*(10000|19999|34999)$")))),
      cost_burdened_50percentormore_incomeslessthan35000_percent = safe_divide(
        ## numerator -- all households where gross rent is 50% or more of household income
        rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*(10000_|19999|34999).*50_0.*percent"))),
        ## denominator -- all households whose household incomes are $34,999 or less
        rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*(10000|19999|34999)$")))),
      cost_burdened_30percentormore_incomeslessthan50000_percent = safe_divide(
        ## numerator -- all households where gross rent is 30% or more of household income
        rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*(10000_|19999|34999|49999).*(30_0|35_0|40_0|50_0).*percent"))),
        ## denominator -- all households whose household incomes are $50,000 or less
        rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*(10000|19999|34999|49999)$")))),
      cost_burdened_50percentormore_incomeslessthan50000_percent = safe_divide(
        ## numerator -- all households where gross rent is 50% or more of household income
        rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*(10000_|19999|34999|49999).*50_0.*percent"))),
        ## denominator -- all households whose household incomes are $50,000 or less
        rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*(10000|19999|34999|49999)$")))),

      ####----RACE/ETHNICITY----####
      dplyr::across(
        .cols = dplyr::matches("^race_(nonhispanic|hispanic)"),
        .fns = ~ safe_divide(.x, race_universe),
        .names = "{.col}_percent"),
      race_personofcolor_percent = 1 - race_nonhispanic_white_alone_percent,

      ####----SEX----####
      sex_female_percent = safe_divide(sex_by_age_female, sex_by_age_universe),
      sex_male_percent = safe_divide(sex_by_age_male, sex_by_age_universe),

      ####----AGE----####
      dplyr::across(
        .cols = dplyr::matches("sex_by_age_female_.*years"),
        .fns = ~ .x + get( dplyr::cur_column() %>% stringr::str_replace("female", "male")),
        .names =  "{.col}_male"),
      dplyr::across(
        .cols = dplyr::matches("sex_by_age_female_.*_male"),
        .fns = ~ safe_divide( .x + get( dplyr::cur_column() %>% stringr::str_replace("female", "male") %>% stringr::str_remove("_male$")), sex_by_age_universe),
        .names =  "{.col}_percent")) %>%

    ## adding a new mutate call because rowSums is unable to access variables
    ## created within the same mutate call
    dplyr::mutate(
      sex_by_age_female_under_18_male = safe_divide(
        rowSums(
          dplyr::select(.,
                 sex_by_age_female_under_5_years_male,
                 sex_by_age_female_5_9_years_male,
                 sex_by_age_female_10_14_years_male,
                 sex_by_age_female_15_17_years_male)),
        sex_by_age_universe),
      sex_by_age_female_over_64_male = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("sex_by_age_female_(6(5|7)|7|8).*male"))),
        sex_by_age_universe),

      ####----DISABILITY----####
      disability_percent = safe_divide(rowSums(dplyr::select(., dplyr::matches("with_a_disability"))), sex_by_age_by_disability_status_universe),

      ####----HOUSING----####
      tenure_renteroccupied_percent = safe_divide(tenure_renter_occupied, tenure_universe),
      tenure_owneroccupied_percent = safe_divide(tenure_owner_occupied, tenure_universe),
      dplyr::across(
        .cols = dplyr::matches("tenure.*householder.*occupied"),
        .fns = ~ safe_divide(.x, get( dplyr::cur_column() %>% stringr::str_replace("(renter|owner)_occupied", "universe") )),
        .names = "{.col}_percent"),

      ## units in structure
      ## (sums)
      dplyr::across( ## summing renter- and owner-occupied estimates
        .cols = dplyr::matches("tenure.*renter_occupied"),
        .fns = ~ .x + get( dplyr::cur_column() %>% stringr::str_replace("renter", "owner")),
        .names = "{.col}_owner_occupied"),
      ## (percentages)
      dplyr::across(
        .cols = c(dplyr::matches("tenure.*renter.*owner"), -tenure_by_units_in_structure_renter_occupied_housing_units_owner_occupied),
        .fns = ~ safe_divide(.x, tenure_by_units_in_structure_renter_occupied_housing_units_owner_occupied),
        .names = "{.col}_percent"),
      ## renter-occupied units in structure (percentages)
      dplyr::across(
        .cols = c(dplyr::matches("tenure.*renter_occupied"), -dplyr::matches("owner|units$")),
        .fns = ~ safe_divide(.x, tenure_by_units_in_structure_renter_occupied_housing_units),
        .names = "{.col}_percent"),
      ## owner-occupied units in structure (percentages)
      dplyr::across(
        .cols = c(dplyr::matches("tenure.*owner_occupied"), -dplyr::matches("renter|units$")),
        .fns = ~ safe_divide(.x, tenure_by_units_in_structure_owner_occupied_housing_units),
        .names = "{.col}_percent"),

      ## overcrowding
      overcrowding_morethan1_ppr_alltenures_percent = safe_divide( ## ppr stands for "people per room"
        rowSums(dplyr::select(., dplyr::matches("tenure_by_occupants_per_room.*(1_01|1_51|2_01)"))),
        tenure_by_occupants_per_room_universe),
      overcrowding_morethan1_ppr_renteroccupied_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("tenure_by_occupants_per_room_renter.*(1_01|1_51|2_01)"))),
        tenure_by_occupants_per_room_renter_occupied),

      ## year built
      dplyr::across(
        .cols = dplyr::matches("year_structure_built_built_[0-9].*"),
        .fns = ~ safe_divide(.x, get( dplyr::cur_column() %>% stringr::str_replace("[0-9].*", "universe") %>% stringr::str_replace("built_", "") )),
        .names = "{.col}_percent"),
      year_structure_built_built_since_1940_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("year_structure_built_built_(19[4-9]|2).*"))),
        year_structure_built_universe),
      year_structure_built_built_since_1950_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("year_structure_built_built_(19[5-9]|2).*"))),
        year_structure_built_universe),
      year_structure_built_built_since_1960_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("year_structure_built_built_(19[6-9]|2).*"))),
        year_structure_built_universe),
      year_structure_built_built_since_1970_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("year_structure_built_built_(19[7-9]|2).*"))),
        year_structure_built_universe),
      year_structure_built_built_since_1980_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("year_structure_built_built_(19[8-9]|2).*"))),
        year_structure_built_universe),
      year_structure_built_built_since_1990_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("year_structure_built_built_(19[9]|2).*"))),
        year_structure_built_universe),
      year_structure_built_built_since_2000_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("year_structure_built_built_(200|201|202).*"))),
        year_structure_built_universe),
      year_structure_built_built_since_2010_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("year_structure_built_built_(201|202).*"))),
        year_structure_built_universe),
      year_structure_built_built_since_2020_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("year_structure_built_built_202.*"))),
        year_structure_built_universe),

      ####----TRANSPORTATION----####
      ## Note: means_transportation_work_public_transportation_excluding_taxicab is a measure of conventional "public transportation"
      dplyr::across(
        .cols = c(dplyr::matches("means_transportation"), -dplyr::matches("universe|worked_from_home")),
        .fns = ~ safe_divide(.x, (means_transportation_work_universe - means_transportation_work_worked_from_home)),
        .names = "{.col}_percent"), ## the denominator here does not include people who worked from home
      means_transportation_work_bicycle_walked_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("means_transportation_work_(bicycle|walked)$"))),
        (means_transportation_work_universe - means_transportation_work_worked_from_home)),
      means_transportation_work_motor_vehicle_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("means_transportation_work_(car_truck_van|taxicab|motorcycle)$"))),
        (means_transportation_work_universe - means_transportation_work_worked_from_home)),

      ####----EDUCATION----####
      educational_attainment_highschool_none_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("educational_attainment.*(no_schooling|nursery|kindergarten|_[0-8]th_grade)"))),
        educational_attainment_population_25_years_over_universe),
      educational_attainment_highschool_nodiploma_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("educational_attainment.*(9th|10th|11th|12th)"))),
        educational_attainment_population_25_years_over_universe),
      educational_attainment_ged_percent = safe_divide(
        educational_attainment_population_25_years_over_ged_alternative_credential,
        educational_attainment_population_25_years_over_universe),
      educational_attainment_highschool_diploma_percent = safe_divide(
        educational_attainment_population_25_years_over_regular_high_school_diploma,
        educational_attainment_population_25_years_over_universe),
      educational_attainment_college_some_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("educational_attainment.*some_college"))),
        educational_attainment_population_25_years_over_universe),
      educational_attainment_degree_associate_percent = safe_divide(
        educational_attainment_population_25_years_over_associates_degree,
        educational_attainment_population_25_years_over_universe),
      educational_attainment_degree_bachelors_percent = safe_divide(
        educational_attainment_population_25_years_over_bachelors_degree,
        educational_attainment_population_25_years_over_universe),
      educational_attainment_degree_morethanbachelors_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("educational_attainment.*(masters|professional|doctorate)"))),
        educational_attainment_population_25_years_over_universe),
      educational_enrollment_grades_1thru12_percent = safe_divide(
        school_enrollment_universe - rowSums(dplyr::select(., dplyr::matches("school_enrollment.*[^(_universe)]"))),
        school_enrollment_universe),
      dplyr::across(
        .cols = dplyr::matches("school_enrollment.*[^(_universe)]"),
        .fns = ~ safe_divide(.x, school_enrollment_universe),
        .names = "{.col}_percent"),

      ####----PLACE OF BIRTH/LANGUAGE----####
      nativity_native_born_percent = safe_divide(
        nativity_by_language_spoken_at_home_by_ability_speak_english_population_5_years_over_native,
        nativity_by_language_spoken_at_home_by_ability_speak_english_population_5_years_over_universe),
      nativity_foreign_born_percent = safe_divide(
        nativity_by_language_spoken_at_home_by_ability_speak_english_population_5_years_over_foreign_born,
        nativity_by_language_spoken_at_home_by_ability_speak_english_population_5_years_over_universe),
      ability_speak_english_very_well_better_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("nativity.*(only_english|english_very_well)"))),
        nativity_by_language_spoken_at_home_by_ability_speak_english_population_5_years_over_universe),
      ability_speak_english_less_than_very_well_percent = 1 - ability_speak_english_very_well_better_percent,

      ####----EMPLOYMENT----####
      employment_civilian_labor_force_percent = safe_divide(employment_civilian_labor_force_employed, employment_civilian_labor_force_universe),

      ####----HEALTH INSURANCE----####
      health_insurance_coverage_status_covered_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("health_insurance_coverage_status_type_by_employment_status.*with_health_insurance_coverage$"))),
        health_insurance_coverage_status_type_by_employment_status_universe),
      health_insurance_coverage_status_notcovered_percent = 1 - health_insurance_coverage_status_covered_percent,
      health_insurance_coverage_status_covered_employed_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("health_insurance_coverage_status_type_by_employment_status.*_employed.*with_health_insurance_coverage$"))),
        health_insurance_coverage_status_type_by_employment_status_in_labor_force), ## denominator is only people in the labor force
      health_insurance_coverage_status_covered_unemployed_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("health_insurance_coverage_status_type_by_employment_status.*_unemployed.*with_health_insurance_coverage$"))),
        health_insurance_coverage_status_type_by_employment_status_in_labor_force)) %>% ## denominator is only people in the labor force

    ## these variable names end in "percent", but they're actually count estimates
    dplyr::rename_with(.cols = dplyr::matches("household_income.*percent$"), .fn = ~ paste0(., "_count_estimate")) %>%

    ## add back MOEs if retain_moes == T
    { if (retain_moes == TRUE) dplyr::left_join(., moes, by = c("GEOID", "data_source_year")) else . }
}


utils::globalVariables(c(
  "state", "GEOID", "data_source_year", "snap_received", "snap_universe",
  "public_assistance_received", "public_assistance_universe", ".",
  "race_nonhispanic_white_alone_percent", "sex_by_age_female", "sex_by_age_universe",
  "sex_by_age_male", "sex_by_age_female_under_5_years_male", "sex_by_age_female_10_14_years_male",
  "sex_by_age_female_5_9_years_male", "sex_by_age_female_10_14_years_male",
  "sex_by_age_female_15_17_years_male", "sex_by_age_by_disability_status_universe",
  "tenure_renter_occupied", "tenure_universe", "tenure_owner_occupied",
  "tenure_by_units_in_structure_renter_occupied_housing_units_owner_occupied",
  "tenure_by_occupants_per_room_universe", "tenure_by_occupants_per_room_renter_occupied",
  "year_structure_built_universe", "means_transportation_work_universe", "means_transportation_work_universe",
  "means_transportation_work_worked_from_home", "educational_attainment_population_25_years_over_universe",
  "educational_attainment_population_25_years_over_ged_alternative_credential",
  "educational_attainment_population_25_years_over_regular_high_school_diploma",
  "educational_attainment_population_25_years_over_associates_degree",
  "educational_attainment_population_25_years_over_bachelors_degree", "school_enrollment_universe",
  "nativity_by_language_spoken_at_home_by_ability_speak_english_population_5_years_over_native",
  "nativity_by_language_spoken_at_home_by_ability_speak_english_population_5_years_over_universe",
  "nativity_by_language_spoken_at_home_by_ability_speak_english_population_5_years_over_foreign_born",
  "ability_speak_english_very_well_better_percent", "employment_civilian_labor_force_employed",
  "employment_civilian_labor_force_universe", "health_insurance_coverage_status_type_by_employment_status_universe",
  "health_insurance_coverage_status_covered_percent",
  "health_insurance_coverage_status_type_by_employment_status_in_labor_force"))
