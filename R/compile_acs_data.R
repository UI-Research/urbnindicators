#' @title Division without NAs
#' @description Return 0 when the divisor is 0.
#' @details A modified division operation that returns zero when the divisor is zero
#'    rather than returning NA. Otherwise returns the quotient.
#' @param x A numeric scalar.
#' @param y A numeric scalar.
#' @returns The traditional dividend in all cases except where \code{y == 0}, in which
#'    case it returns 0.
#' @examples
#' safe_divide(1, 2)
#' safe_divide(3, 0)
#' @export
safe_divide = function(x, y) { dplyr::if_else(y == 0, 0, x / y) }

#' @title Calculate ACS measures
#' @description Calculates derived ACS indicators.
#' @details An internal function used to calculate indicators and derived variable
#' definitions for the codebook.
#' @param .data The dataset returned from \code{compile_acs_data()}.
#' @returns A modified dataframe that includes newly calculated indicators.
#' @examples
#' \dontrun{
#' df = compile_acs_data(
#'   variables = list_acs_variables(year = 2022),
#'   years = c(2022),
#'   geography = "county",
#'   states = "NJ",
#'   counties = NULL,
#'   spatial = FALSE)
#' internal_compute_acs_variables(.data = df)
#' }
#' @importFrom magrittr %>%
internal_compute_acs_variables = function(.data) {
  .data %>%
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
        .cols = dplyr::matches("^race_nonhispanic|^race_hispanic"),
        .fns = ~ safe_divide(.x, race_universe),
        .names = "{.col}_percent"),
      race_personofcolor_percent = 1 - race_nonhispanic_white_alone_percent,

      ####----SEX----####
      sex_female_percent = safe_divide(sex_by_age_female, sex_by_age_universe),
      sex_male_percent = safe_divide(sex_by_age_male, sex_by_age_universe),

      ####----AGE----####
      # creating combined, male and female counts by age group named, e.g., age_15_17_years
      dplyr::across(
        .cols = dplyr::matches("sex_by_age_female_.*years$"),
        .fns = ~ .x + get( dplyr::cur_column() %>% stringr::str_replace("female", "male")),
        .names =  "{stringr::str_replace(string = .col, pattern = 'sex_by_age_female_', replacement = 'age_')}"),
      dplyr::across(
        .cols = dplyr::matches("^age.*years$"),
        .fns = ~ safe_divide(.x, sex_by_age_universe),
        .names =  "{.col}_percent")) %>%

    ## adding a new mutate call because rowSums is unable to access variables
    ## created within the same mutate call
    dplyr::mutate(
      age_under_18_percent = safe_divide(
        rowSums(dplyr::select(., age_under_5_years, age_5_9_years, age_10_14_years, age_15_17_years)),
        sex_by_age_universe),
      age_over_64_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("age_(6(5|7)|7|8).*_years$"))),
        sex_by_age_universe),

      ####----DISABILITY----####
      disability_percent = safe_divide(rowSums(dplyr::select(., dplyr::matches("with_a_disability"))), sex_by_age_by_disability_status_universe),

      ####----HOUSING----####
      ## tenure
      ## (percentages)
      dplyr::across(
        .cols = dplyr::matches("tenure_renter_occupied|tenure_owner_occupied"),
        .fns = ~ safe_divide(.x, tenure_universe),
        .names = "{.col}_percent"),

      ## tenure by race
      ## (sums)
      dplyr::across(
        .cols = dplyr::matches("tenure_.*_householder_renter_occupied"),
        .fns = ~ .x + get( dplyr::cur_column() %>% stringr::str_replace("renter", "owner")),
        .names = "{stringr::str_replace_all(string = .col, pattern = 'renter_occupied', replacement = 'renter_owner_occupied')}"),

      ## tenure by race, renter-occupied
      ## (percentages)
      dplyr::across(
        .cols = dplyr::matches("tenure.*householder_renter_occupied"),
        .fns = ~ safe_divide(.x, get( dplyr::cur_column() %>% stringr::str_replace("renter", "renter_owner") )),
        .names = "{.col}_percent"),

      ## tenure by race, owner-occupied
      ## (percentages)
      dplyr::across(
        .cols = dplyr::matches("tenure.*householder_owner_occupied"),
        .fns = ~ safe_divide(.x, get( dplyr::cur_column() %>% stringr::str_replace("owner", "renter_owner") )),
        .names = "{.col}_percent"),

      ## units in structure, both tenures
      ## (sums)
      dplyr::across( ## summing renter- and owner-occupied estimates
        .cols = c(dplyr::matches("tenure_by_units.*renter_occupied_housing_units"), -dplyr::matches("owner")),
        .fns = ~ .x + get( dplyr::cur_column() %>% stringr::str_replace("renter", "owner")),
        .names = "{stringr::str_replace_all(string = .col, pattern = 'renter_occupied_housing_units', replacement = 'renter_owner_occupied_housing_units')}"),
      ## units in structure, both tenures
      ## (percentages)
      dplyr::across(
        .cols = dplyr::matches("tenure_by_units_in_structure_renter_owner_occupied_housing_units_"),
        .fns = ~ safe_divide(.x, tenure_by_units_in_structure_renter_owner_occupied_housing_units),
        .names = "{.col}_percent"),
      ## renter-occupied units in structure
      ## (percentages)
      dplyr::across(
        .cols = dplyr::matches("tenure_by_units_in_structure_renter_occupied_housing_units_"),
        .fns = ~ safe_divide(.x, tenure_by_units_in_structure_renter_occupied_housing_units),
        .names = "{.col}_percent"),
      ## owner-occupied units in structure
      ## (percentages)
      dplyr::across(
        .cols = dplyr::matches("tenure_by_units_in_structure_owner_occupied_housing_units_"),
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
      year_structure_built_built_before_1960_percent = 1 - year_structure_built_built_since_1960_percent, ## a common proxy for relative exposure to lead-based paint

      ####----TRANSPORTATION----####
      ## Note: means_transportation_work_public_transportation_excluding_taxicab is a measure of conventional "public transportation"
      dplyr::across(
        .cols = c(dplyr::matches("means_transportation"), -dplyr::matches("universe|worked_from_home"), -means_transportation_work_worked_from_home),
        .fns = ~ safe_divide(.x, (means_transportation_work_universe - means_transportation_work_worked_from_home)),
        .names = "{.col}_percent"), ## the denominator here does not include people who worked from home
      means_transportation_work_worked_from_home_percent = safe_divide(
        means_transportation_work_worked_from_home, means_transportation_work_universe),
      means_transportation_work_bicycle_walked_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("means_transportation_work_(bicycle|walked)$"))),
        (means_transportation_work_universe - means_transportation_work_worked_from_home)),
      means_transportation_work_motor_vehicle_percent = safe_divide(
        rowSums(dplyr::select(., dplyr::matches("means_transportation_work_(car_truck_van|taxicab|motorcycle)$"))),
        (means_transportation_work_universe - means_transportation_work_worked_from_home)),
      dplyr::across(
        .cols = c(dplyr::matches("travel_time_work"), -travel_time_work_universe),
        .fns = ~ safe_divide(.x, travel_time_work_universe),
        .names = "{.col}_percent"),

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
        health_insurance_coverage_status_type_by_employment_status_in_labor_force)) ## denominator is only people in the labor force
}

#' @title Analysis-ready social science measures
#' @description Construct measures frequently used in social sciences
#'    research, leveraging \code{tidycensus::get_acs()} to acquire raw estimates from
#'    the Census Bureau API.
#' @param variables A named vector of ACS variables such as that returned from
#'    \code{urbnindicators::list_acs_variables()}.
#' @param years A character vector (or coercible to the same) comprising one or more
#'    four-digit years for which to pull five-year American Community Survey estimates.
#' @param geography A geography type that is accepted by \code{tidycensus::get_acs()}, e.g.,
#'    "tract", "county", "state", among others. Geographies below the tract level are not
#'    supported.
#' @param states A vector of one or more state names, abbreviations, or codes as
#'    accepted by \code{tidycensus::get_acs()\code{.
#' @param counties A vector of one or more county names, abbreviations, or codes as
#'    accepted by \code{tidycensus::get_acs()}. NOTE: this parameter is not currently supported
#'    and must be set to NULL (as it is by default).
#' @param spatial Boolean. Return a simple features (sf), spatially-enabled dataframe?
#' @seealso \code{tidycensus::get_acs()}, which this function wraps.
#' @returns A dataframe containing the requested \code{variables}, their MOEs (optionally),
#'    a series of derived variables, such as percentages, and the year of the data.
#'    Returned data are formatted wide. A codebook generated with \code{generate_codebook()}
#'    is attached and can be accessed via \code{compile_acs_data() %>% attr("codebook")}.
#' @examples
#' \dontrun{
#' acs_variables = list_acs_variables(year = "2022")
#' df = compile_acs_data(
#'   variables = acs_variables,
#'   years = c(2021, 2022),
#'   geography = "county",
#'   states = "NJ",
#'   counties = NULL,
#'   spatial = FALSE)
#'   }
#' @export
#' @importFrom magrittr %>%

compile_acs_data = function(
    variables = NULL,
    years = c(2022),
    geography = "county",
    states = NULL,
    counties = NULL,
    spatial = FALSE) {

  options(tigris_use_cache = FALSE)

warning(
"Variable names and geographies for ACS data products can change between years.
Changes to geographies are particularly significant across decades
(e.g., from 2019 to 2020), but these changes can occur in any year.
Users should ensure that the logic embedded in this function--
which was developed around five-year ACS estimates for 2017-2021--
remains accurate for their use cases. Evaluation of measures and
geographies over time should be thoroughly quality checked.\n")

  ## default values for the variables and states arguments.
  if (length(variables) == 0) { variables = list_acs_variables(year = years[1]) }
  if (length(states) == 0) { states =  tigris::fips_codes %>%
    dplyr::filter(!state %in% c("PR", "UM", "VI", "GU", "AS", "MP")) %>%
    dplyr::pull(state) %>% unique() }

  ## warning about inter-decadal tract geometry changes
  if ( (max(years) >= 2020) & (min(years) < 2020) & (geography == "tract") ) {
    warning("Requested years span the year 2020, which is when the Census Bureau re-configures
      census tract boundaries. It is not valid to compare census tract-level statistics for years before 2020 to
      statistics from 2020 and after; use a crosswalk, such as those provided by NHGIS, to interpolate values.
      A future version of urbnindicators may address this issue automatically.") }

  ## tracts and larger are supported
  if ((geography %>% tolower) %in% c("block", "block group")) {
    stop("Block and block group geographies are not supported at this time.") }

  super_state_geographies = c(
    "us", "region", "division", "metropolitan/micropolitan statistical area",
    "metropolitan statistical area/micropolitan statistical area",
    "cbsa", "urban area", "zip code tabulation area", "zcta")

  ## download corresponding geometries from tigris
  ## these will be joined to the data to calculate population density
  ## (and optionally retained in the final output)
  suppressMessages({ suppressWarnings({
      geometries = purrr::map_dfr(
        years,
        function(year) {
          switch(
            geography,
            "us" = tigris::nation(year = year) %>%
              dplyr::mutate(
                GEOID = "1",
                ALAND = 9161555541118, ## sum of ALAND from tigris::states(year = 2022, cb = TRUE)
                AWATER = 711492860209), ## sum of AWATER from tigris::states(year = 2022, cb = TRUE)
            "region" = tigris::regions(year = year),
            "division" = tigris::divisions(year = year),
            "state" = tigris::states(year = year, cb = TRUE),
            "county" = purrr::map_dfr(states, ~ tigris::counties(state = .x, cb = TRUE, year = year)),
            "county subdivision" = purrr::map_dfr(states, ~ tigris::county_subdivisions(state = .x, cb = TRUE, year = year)),
            "tract" = purrr::map_dfr(states, ~ tigris::tracts(state = .x, cb = TRUE, year = year)),
            "place" = purrr::map_dfr(states, ~ tigris::places(state = .x, cb = TRUE, year = year)),
            "alaska native regional corporation" = tigris::alaska_native_regional_corporations(cb = TRUE, year = year),
            "american indian area/alaska native area/hawaiian home land" = tigris::native_areas(cb = TRUE, year = year),
            "american indian area/alaska native area (reservation of statistical entity only)" = tigris::native_areas(cb = TRUE, year = year),
            "american indian area (off reservation trust land only)/hawaiian home land" = tigris::native_areas(cb = TRUE, year = year),
            "metropolitan/micropolitan statistical area" = tigris::core_based_statistical_areas(cb = TRUE, year = year),
            "metropolitan statistical area/micropolitan statistical area" = tigris::core_based_statistical_areas(cb = TRUE, year = year),
            "cbsa" = tigris::core_based_statistical_areas(cb = TRUE, year = year),
            "combined statistical area" = tigris::combined_statistical_areas(cb = TRUE, year = year),
            "new england city and town area" = tigris::new_england(cb = TRUE, year = year, type = "NECTA")) %>%
            dplyr::transmute(
              area_land_sq_kilometer = ALAND / 1000000,
              area_water_sq_kilometer = AWATER / 1000000,
              area_land_water_sq_kilometer = area_land_sq_kilometer + area_water_sq_kilometer,
              GEOID = GEOID,
              data_source_year = year) })
    })})

  suppressMessages({ suppressWarnings({
    ## some geographies are not available by state and can only be returned nationally
    if (geography %in% super_state_geographies) {
      df_raw_estimates = purrr::map_dfr(
        ## when year is a vector with length > 1 (i.e., there are multiple years)
        ## loop over each item in the vector (and this approach also works for a single year)
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
      df_raw_estimates = purrr::map_dfr(
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

    moes = df_raw_estimates %>% dplyr::select(GEOID, data_source_year, dplyr::matches("_M$"))
  })})

  df_calculated_estimates = df_raw_estimates %>%
    ## drop margin of error variables for calculations since these only relate to raw
    ## variables.
    dplyr::select(-dplyr::matches("_M$")) %>%
    dplyr::rename_with(~ stringr::str_remove(.x, "_E$")) %>% ## removing "_E" (for "Estimate") from column names
    internal_compute_acs_variables() %>%
    ## these variable names end in "percent", but they're actually count estimates
    dplyr::rename_with(.cols = dplyr::matches("household_income.*percent$"), .fn = ~ paste0(., "_count_estimate")) %>%

    ## ensure the vintage of the data and the GEOID for each observation are the first columns
    dplyr::select(data_source_year, GEOID, dplyr::everything()) %>%

    ## join geometries, calculate population density, drop geometry attribute if spatial == FALSE
    dplyr::right_join(geometries, by = c("GEOID", "data_source_year"), relationship = "one-to-one") %>%
    dplyr::mutate(population_density_land_sq_kilometer = safe_divide(total_population_universe, area_land_sq_kilometer)) %>%
    {if (spatial == FALSE) sf::st_drop_geometry(.) else st_as_sf(.) } %>%
    dplyr::left_join(., moes, by = c("GEOID", "data_source_year"))

  ## attach the codebook as an attribute named "codebook" to the returned dataset
  attr(df_calculated_estimates, "codebook") = generate_codebook(.data = df_calculated_estimates)

  return(df_calculated_estimates)
}

utils::globalVariables(c(
  "ALAND", "AWATER", "area_land_sq_kilometer", "area_water_sq_kilometer", "total_population_universe",
  "state", "GEOID", "data_source_year", "snap_received", "snap_universe",
  "public_assistance_received", "public_assistance_universe", ".",
  "race_nonhispanic_white_alone_percent", "sex_by_age_female", "sex_by_age_universe",
  "sex_by_age_male", "sex_by_age_female_under_5_years_male", "sex_by_age_female_10_14_years_male",
  "sex_by_age_female_5_9_years_male", "sex_by_age_female_10_14_years_male",
  "sex_by_age_female_15_17_years_male", "sex_by_age_by_disability_status_universe",
  "age_under_5_years", "age_5_9_years", "age_10_14_years", "age_15_17_years",
  "tenure_renter_occupied", "tenure_universe", "tenure_owner_occupied",
  "tenure_by_units_in_structure_renter_occupied_housing_units_owner_occupied",
  "tenure_by_occupants_per_room_universe", "tenure_by_occupants_per_room_renter_occupied",
  "year_structure_built_universe", "means_transportation_work_universe", "means_transportation_work_universe",
  "means_transportation_work_worked_from_home", "educational_attainment_population_25_years_over_universe",
  "year_structure_built_built_since_1960_percent", "travel_time_work_universe",
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
