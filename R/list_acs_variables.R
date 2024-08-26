#' @title Easily rename ACS variables
#' @description Given the name of an ACS variable (or a string that matches one or more such variables),
#' generate a named character vector of original variable names and more meaningful names.
#' @param variable_name A named vector (intended for use with named ACS variables).
#' @param census_codebook An object returned from \code{tidycensus::load_variables()}.
#' @returns A named character vector containing the variables that matched
#'    \code{variable_name} from the \code{census_codebook}, with semantically-meaningful names
#'    derived from metadata fields contained in \code{census_codebook}.
#' @examples
#' \dontrun{
#' codebook = tidycensus::load_variables(dataset = "acs5", year = 2022)
#' select_variables_by_name("B16005_", census_codebook = codebook)
#' }
#' @export
#' @importFrom magrittr %>%
select_variables_by_name = function(variable_name, census_codebook) {

  variables = census_codebook %>%
    dplyr::filter(stringr::str_detect(name, !!variable_name)) %>%
    dplyr::mutate(
      ## string-cleaning syntax to generate relatively easy-to-read and meaningful
      ## names based on the "Concept" definition for each variable
      clean_names = paste0(
        concept, "_", # e.g., "tenure_white_alone_householder"
        label) %>% # e.g., "owner_occupied"
        stringr::str_to_lower() %>%
        stringr::str_remove_all("\\(|\\)") %>%
        stringr::str_replace_all(c(
          "\\!\\!" = "_",
          "\\:" = "_",
          "estimate_total" = "",
          "'" = "",
          '"' = "",
          "," = "",
          "_and_" = "_",
          " to " = "_",
          " " = "_",
          "_{2,}" = "_",
          "_$" = "",
          "\\." = "_",
          "__" =  "_",
          "-" = "_",
          "_$" = "",
          "_and_" = "_",
          "_or_" = "_",
          "_the_" = "_",
          "_for_" = "_",
          "_of_" = "_",
          "__" = "_",
          "native_hawaiian_other_pacific_islander" = "nhpi",
          "hispanic_latino" = "hispanic",
          "american_indian_alaska_native" = "aian",
          "black_african_american" = "black",
          "household_income_by_gross_rent_as_a_percentage_of_household_income_in_the_past_12_months" =
            "household_income_by_gross_rent_as_a_percentage_of_household_income")),
      clean_names = dplyr::if_else(
        label %in% c("Estimate!!Total:", "Estimate!!Total"),
        paste0(clean_names, "_universe_"),
        paste0(clean_names, "_")))

  selected_variables = variables %>% dplyr::pull(name)
  names(selected_variables) = variables %>% dplyr::pull(clean_names)

  return(selected_variables)
}

#' @title Easily filter ACS variables
#' @description Filter the the results of \code{select_variables_by_name()} based on their `match_type` relative
#' to `match_string`.
#' @param variable_vector A named vector (intended for use with named ACS variables).
#' @param match_string A string on which to filter (or not filter) elements in `variable_vector`.
#' @param match_type Whether to include (`match_type = "positive"`) or exclude
#'    (`match_type = "negative"`) matching elements.
#' @returns The elements from `variable_vector` that do/don't match `match_string`.
#' @examples
#' \dontrun{
#' codebook = tidycensus::load_variables(dataset = "acs5", year = 2022)
#' selected_variables = select_variables_by_name("B16005_", census_codebook = codebook)
#' filter_variables(
#'   variable_vector = selected_variables,
#'   match_string = "universe_$|native_$|foreign_born_$|only|very_well",
#'   match_type = "positive")
#' }
#' @export
filter_variables = function(variable_vector, match_string, match_type = "positive") {
  if (match_type == "positive") {
    variable_vector[stringr::str_detect(names(variable_vector), match_string)] }
  else if (match_type == "negative") {
    variable_vector[!stringr::str_detect(names(variable_vector), match_string)] }
}

#' @title Return ACS variables codes and names
#' @description Generate meaningful names for ACS variable codes based on their metadata
#'    and return these as a vector, along with their semantic names. Intended for
#'    use with [compile_acs_data()].
#' @param year The year for which variable names should be selected.
#' @returns A named vector of variable codes (as specified in the Census Bureau's API)
#'    with semantically-meaningful names (e.g., "race_black_alone_nonhispanic").
#' @examples
#' list_acs_variables(year = "2022") %>% head()
#' @export
list_acs_variables = function(year = "2022") {

  ## hard-coding this as variable metadata from 2023 for the time being because
  ## variable metadata follows a different naming convention (issue IDed in 2013;
  ## possibly other years as well), but the variables are (seemingly) the same
  ## underlying variables
  census_codebook = tidycensus::load_variables(year = 2022, dataset = "acs5")

  select_variables = purrr::partial(select_variables_by_name, census_codebook = census_codebook)

  ####----ACS Variables by Concept----####
  variables = c(

    ####----POPULATION----####
      # always use table-specific denominators for calculations
      # this is only for total population and population density
      total_population_universe_ = "B01003_001",

    ####----INCOME, POVERTY, FINANCIAL ASSISTANCE---####
      ## PUBLIC ASSISTANCE INCOME OR FOOD STAMPS/SNAP IN THE PAST 12 MONTHS FOR HOUSEHOLDS
      public_assistance_universe_ = "B19058_001",
      public_assistance_received_ = "B19058_002",

      ## RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY POVERTY STATUS IN THE PAST 12 MONTHS FOR HOUSEHOLDS
      snap_universe_ = "B22003_001",
      snap_received_ = "B22003_002",

      ## HOUSEHOLD INCOME QUINTILE UPPER LIMITS
      household_income_quintile_upper_limit_1_ = "B19080_001",
      household_income_quintile_upper_limit_2_ = "B19080_002",
      household_income_quintile_upper_limit_3_ = "B19080_003",
      household_income_quintile_upper_limit_4_ = "B19080_004",
      household_income_quintile_upper_limit_5_ = "B19080_005", ## lower limit of top five percent

      ## MEAN HOUSEHOLD INCOME OF QUINTILES
      household_income_quintile_mean_1_ = "B19081_001",
      household_income_quintile_mean_2_ = "B19081_002",
      household_income_quintile_mean_3_ = "B19081_003",
      household_income_quintile_mean_4_ = "B19081_004",
      household_income_quintile_mean_5_ = "B19081_005",
      household_income_quintile_mean_6_ = "B19081_006", ## top five percent

      ## SHARES OF AGGREGATE HOUSEHOLD INCOME BY QUINTILE
      household_income_quintile_share_aggregate_1_ = "B19082_001",
      household_income_quintile_share_aggregate_2_ = "B19082_002",
      household_income_quintile_share_aggregate_3_ = "B19082_003",
      household_income_quintile_share_aggregate_4_ = "B19082_004",
      household_income_quintile_share_aggregate_5_ = "B19082_005",
      household_income_quintile_share_aggregate_6_ = "B19082_006", ## top five percent

      ## GINI INDEX OF INCOME INEQUALITY
      gini_index_ = "B19083_001",

      ## MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN INFLATION-ADJUSTED DOLLARS)
      median_household_income_universe_allraces_ = "B19013_001",
      median_household_income_white_alone_ = "B19013A_001",
      median_household_income_white_alone_nonhispanic_ = "B19013H_001",
      median_household_income_black_alone_ = "B19013B_001",
      median_household_income_aian_alone_ = "B19013C_001",
      median_household_income_asian_alone_ = "B19013D_001",
      median_household_income_nhpi_ = "B19013E_001",
      median_household_income_otherrace_alone_ = "B19013F_001",
      median_household_income_twoormore_ = "B19013G_001",
      median_household_income_hispanic_ = "B19013I_001",

      ## POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY HOUSEHOLD TYPE BY TENURE
      federal_poverty_limit_universe_allraces_ = "B17020_001",
      federal_poverty_limit_universe_aian_alone_ = "B17020C_001",
      federal_poverty_limit_universe_asian_alone_ = "B17020D_001",
      federal_poverty_limit_universe_black_alone_ = "B17020B_001",
      federal_poverty_limit_universe_hispanic_ = "B17020I_001",
      federal_poverty_limit_universe_nhpi_alone_ = "B17020E_001",
      federal_poverty_limit_universe_otherrace_alone_ = "B17020F_001",
      federal_poverty_limit_universe_twoormore_ = "B17020G_001",
      #federal_poverty_limit_universe_white_alone_ = "B17020A_001", # rarely used - this includes people who are white and hispanic
      federal_poverty_limit_universe_white_alone_nonhispanic_ = "B17020H_001",

      federal_poverty_limit_below_allraces_ = "B17020_002",
      federal_poverty_limit_below_aian_alone_ = "B17020C_002",
      federal_poverty_limit_below_asian_alone_ = "B17020D_002",
      federal_poverty_limit_below_black_alone_ = "B17020B_002",
      federal_poverty_limit_below_hispanic_ = "B17020I_002",
      federal_poverty_limit_below_nhpi_alone_ = "B17020E_002",
      federal_poverty_limit_below_otherrace_alone_ = "B17020F_002",
      federal_poverty_limit_below_twoormore_ = "B17020G_002",
      #federal_poverty_limit_below_white_alone_ = "B17020_002", # rarely used -- this includes people who are white and hispanic
      federal_poverty_limit_below_white_alone_nonhispanic_ = "B17020H_002",

    ####----RACE AND ETHNICITY----####
      race_universe_ = "B03002_001",
      race_nonhispanic_allraces_ = "B03002_002",
      race_nonhispanic_white_alone_ = "B03002_003",
      race_nonhispanic_black_alone_ = "B03002_004",
      race_nonhispanic_aian_alone_ = "B03002_005",
      race_nonhispanic_asian_alone_ = "B03002_006",
      race_nonhispanic_nhpi_alone_ = "B03002_007",
      race_nonhispanic_otherrace_alone_ = "B03002_008",
      race_nonhispanic_twoormore_ = "B03002_009", ## this includes "some other race"
      race_nonhispanic_twoormore_includingotherrace_ = "B03002_010", # "some other race" could include, for example, "Egyptian", "Irish", "Middle Eastern"
      race_nonhispanic_twoormore_excludingotherrace_ = "B03002_011",
      race_hispanic_allraces_ = "B03002_012",
      race_hispanic_white_alone_ = "B03002_013",
      race_hispanic_black_alone_ = "B03002_014",
      race_hispanic_aian_alone_ = "B03002_015",
      race_hispanic_asian_alone_ = "B03002_016",
      race_hispanic_nhpi_alone_ = "B03002_017",
      race_hispanic_otherrace_alone_ = "B03002_018",
      race_hispanic_twoormore_ = "B03002_019",
      race_hispanic_twoormore_includingotherrace_ = "B03002_020",
      race_hispanic_twoormore_excludingotherrace_ = "B03002_021",

    ####----SEX AND AGE----####
      ## these domains are captured by the table of estimates on disability (below)
      ## but because that table reflects more detail (includes disability status), the
      ## universe estimates are smaller than those for the estimates in this table
      select_variables(variable_name = "B01001_"),

    ####----DISABILITY----####
      ## SEX BY AGE BY DISABILITY STATUS
      select_variables(variable_name = "B18101_"),
      ## race-specific tables, just remove the "_", a la: select_variables(variable_name = "B18101")

    ####----HOUSING---####
      ## TENURE (by race)
      select_variables("B25003"),

      ## TENURE BY OCCUPANTS PER ROOM
      select_variables("B25014"),

      ## TENURE BY UNITS IN STRUCTURE
      select_variables("B25032"),

      ## HOUSING STOCK AGE
      select_variables("B25034"),

      ## HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
      ## (AKA cost burden)
      select_variables(variable_name = "B25074"),

      ## TENURE BY HOUSING COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
      ## (AKA tenure by cost burden)
      select_variables(variable_name = "B25106"),

      ## MEDIAN MONTHLY HOUSING COSTS (DOLLARS)
      housing_cost_monthly_median_ = "B25105_001",

      ## MEDIAN ANNUAL HOUSEHOLD INCOME BY TENURE
      median_household_income_in_past_12_months_by_tenure_universe_ = "B25119_001",
      median_household_income_in_past_12_months_by_tenure_owner_occupied_ = "B25119_002",
      median_household_income_in_past_12_months_by_tenure_renter_occupied_ = "B25119_003",

      ## MORTGAGE STATUS
      mortgage_status_universe_ = "B25081_001",
      mortgage_status_housing_units_with_mortgage_ = "B25081_002",

    ####----TRANSPORTATION----####
      ## MEANS OF TRANSPORTATION TO WORK
      select_variables("B08301_"),

      ## TRAVEL TIME TO WORK
      select_variables("B08303_"),

      ## TENURE BY VEHICLES AVAILABLE
      select_variables(variable_name = "B25044_"),

    ####----EDUCATION----####
      ## EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER
      select_variables("B15003"),

      ## SCHOOL ENROLLMENT BY DETAILED  LEVEL OF SCHOOL FOR THE POPULATION 3 YEARS AND OVER
      school_enrollment_universe_ = "B14007_001",
      school_enrollment_prek_ = "B14007_003",
      school_enrollment_kindergarten_ = "B14007_004",
      school_enrollment_undergraduate_ = "B14007_017",
      school_enrollment_graduate_ = "B14007_018",
      school_enrollment_notenrolled_ = "B14007_019",

    ####----NATIVITY AND LANGUAGE----####
      ## NATIVITY BY LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
      select_variables("B16005_") %>% filter_variables("universe_$|native_$|foreign_born_$|only|very_well"),

    ####----EMPLOYMENT----####
      ## EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER
      employment_civilian_labor_force_universe_ = "B23025_003",
      employment_civilian_labor_force_employed_ = "B23025_004",

    ####----HOUSEHOLD COMPOSITION----####
      ## AVERAGE HOUSEHOLD SIZE OF OCCUPIED HOUSING UNITS BY TENURE
      household_size_average_ = "B25010_001",
      household_size_average_owneroccupied_ = "B25010_002",
      household_size_average_renteroccupied_ = "B25010_003",

    ####----HEALTH INSURANCE----####
      ## HEALTH INSURANCE COVERAGE STATUS AND TYPE BY EMPLOYMENT STATUS
      select_variables("B27011"),

    ####----DIGITAL INFRASTRUCTURE ACCESS----####
      ## PRESENCE AND TYPES OF INTERNET SUBSCRIPTIONS IN HOUSEHOLD
      internet_subscription_household_universe_ = "B28002_001",
      internet_subscription_household_with_subscription_ = "B28002_002",
      internet_subscription_household_with_subscription_broadband_ = "B28002_004",

      ## TYPES OF COMPUTERS IN HOUSEHOLD
      types_of_computing_devices_household_universe_ = "B28001_001",
      types_of_computing_devices_household_with_any_device_ = "B28001_002",
      types_of_computing_devices_household_with_computer_laptop_ = "B28001_003",
      types_of_computing_devices_household_with_smartphone_ = "B28001_005"
  )

  ## Check that all variables are in the Census codebook
  if (length(variables[!variables %in% census_codebook$name]) != 0) {
    stop(paste0(
      "The following variables don't exist in the selected dataset for the selected year: ",
      variables[!variables %in% census_codebook$name])) }

  return(variables)
}

utils::globalVariables(c("name", "concept", "label", "clean_names"))
