# message("Update test data prior to testing, as needed.")
# df = compile_acs_data(
#   variables = NULL,
#   years = c(2022),
#   geography = "tract",
#   states = c("TX"),
#   counties = NULL,
#   spatial = FALSE)
#
# codebook = attr(df, "codebook")
#
# saveRDS(object = df, file = file.path("inst", "test-data", "test_data_2025-05-13.rds"))
# saveRDS(codebook, file = file.path("inst", "test-data", "codebook_2025-05-13.rds"))

####----Tests----####
# All percentages have no values greater than one and no values less than zero
testthat::test_that(
  "All percentages have no values greater than one and no values less than zero",
  {
    ## Statistics for CA and TX tracts
    df = readRDS(system.file("test-data", "test_data_2025-05-13.rds", package = "urbnindicators"))

    percentage_outliers_maxima = df %>%
      dplyr::select(dplyr::matches("percent$")) %>%
      dplyr::summarise(dplyr::across(.cols = dplyr::where(is.numeric), ~ max(.x, na.rm = T))) %>%
      tidyr::pivot_longer(cols = everything()) %>%
      dplyr::arrange(desc(value)) %>%
      dplyr::filter(value > 1) %>%
      nrow

    percentage_outliers_minima = df %>%
      dplyr::select(dplyr::matches("percent$")) %>%
      dplyr::summarise(dplyr::across(.cols = where(is.numeric), ~ min(.x, na.rm = T))) %>%
      tidyr::pivot_longer(cols = everything()) %>%
      dplyr::arrange(desc(value)) %>%
      dplyr::filter(value < 0) %>%
      nrow

    testthat::expect_equal(percentage_outliers_maxima, 0)
    testthat::expect_equal(percentage_outliers_minima, 0) } )

# All measures have meaningful values: maximum and mean greater than zero,
# all values are not missing, and there are at least two distinct values per measure
testthat::test_that(
  "All measures have meaningful values",
  {
    ## Statistics for CA and TX Tracts
    df = readRDS(system.file("test-data", "test_data_2025-05-13.rds", package = "urbnindicators"))

    summary_statistics = df %>%
      dplyr::select(GEOID, matches("percent$")) %>%
      tidyr::pivot_longer(-GEOID) %>%
      dplyr::group_by(name) %>%
      dplyr::summarise(
        count_total  = dplyr::n(),
        count_na = sum(is.na(value)),
        min = min(value, na.rm = T),
        max = max(value, na.rm = T),
        mean = mean(value, na.rm = T),
        distinct_values = dplyr::n_distinct(value, na.rm = T))

    purrr::map_dbl(summary_statistics$max, ~testthat::expect_gt(.x, 0) )
    purrr::map_dbl(summary_statistics$mean, ~testthat::expect_gt(.x, 0) )
    purrr::map2_dbl(summary_statistics$count_na, summary_statistics$count_total, ~ testthat::expect_lt(.x, .y) )
    purrr::map_dbl(summary_statistics$min, ~testthat::expect_gte(.x, 0) ) })

# No percentage measure has exactly the same values (for every observation) as
# that of another percentage measure (Note: this is implemented with tract data. This does not
# inherently apply to other levels of geographic observation, nor for subsets of the US.)
testthat::test_that(
  "All percentages are distinct",
  {
    ## Statistics for CA and TX Tracts
    df = readRDS(system.file("test-data", "test_data_2025-05-13.rds", package = "urbnindicators"))

    duplicates = purrr::map_dfr(
      colnames(df %>% dplyr::select(dplyr::matches("percent$"))),
      function(colname1) {
        purrr::map_dfr(
          colnames(df %>% dplyr::select(-dplyr::matches("_M$|universe"))),
          function(colname2) {
            if ((colname1 != colname2) & (identical(df[[colname1]], df[[colname2]]))) {
              data.frame(variable_one = colname1, variable_two = colname2)}})})

    distinct_duplicates = duplicates %>%
      dplyr::mutate(
        combined_variables = dplyr::if_else(
          variable_one > variable_two,
          paste(variable_one, variable_two),
          paste(variable_two, variable_one))) %>%
      dplyr::distinct(combined_variables, .keep_all = T) %>%
      # this variable intentionally has a single duplicate (differently-named) variable
      # for consistency across different concepts
      dplyr::filter(!stringr::str_detect(variable_one, "year_structure_built.*later"))

    testthat::expect_equal(nrow(distinct_duplicates), 0) } )

####----SPECIFIC VARIABLES----####

## Guiding notes:
##    Derived variables that are explicitly defined, i.e., where all variables used
##    in the calculation are selected by name, not by string-matching, do not need to
##    be code reviewed.
##
##    All other derived variables should be code reviewed to ensure that only appropriate
##    variables are used in the calculation.

testthat::test_that(
  "All derived variables are calculated using the intended variables",
  {
    ## Statistics for CA and TX tracts
    df = readRDS(system.file("test-data", "test_data_2025-05-13.rds", package = "urbnindicators")) %>%
      dplyr::select(-matches("_M$|_CV$|_SE$"))


    ### Federal Poverty Limit Below
    ### Note that this calculates race-specific rates (as intended)

    # dplyr::across(
    #   .cols = dplyr::matches("federal_poverty_limit.*below"),
    #   .fns = ~ safe_divide(.x, get( dplyr::cur_column() %>% stringr::str_replace("below", "universe"))),
    #   .names = "{.col}_percent")

    ## numerators
    df %>%
      dplyr::select(c(dplyr::matches("federal_poverty_limit.*below"), -dplyr::matches("percent|universe"))) %>%
      colnames()

    ## denominator
    df %>%
      dplyr::select(c(dplyr::matches("federal_poverty_limit.*below"), -dplyr::matches("percent|universe"))) %>%
      dplyr::rename_with(
        .cols = dplyr::everything(),
        .fn = ~ stringr::str_replace(.x, "below", "universe"))

    ### cost_burdened_30percentormore_allincomes_percent
    # cost_burdened_30percentormore_allincomes_percent = safe_divide(
    #   ## numerator -- all households where gross rent is 30% or more of household income
    #   rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*(30_0|35_0|40_0|50_0).*percent"))),
    #   ## denominator -- all households with computed rent shares
    #   rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*([0-9]$|100000_more$)"))) -
    #     rowSums(dplyr::select(., dplyr::matches("household_income.*not_computed"))))

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("household_income_by_gross_rent.*(30_0|35_0|40_0|50_0).*percent")) %>%
      colnames()

    ## denominator
    df %>%
      dplyr::select(dplyr::matches("household_income_by_gross_rent.*([0-9]$|100000_more$)")) %>%
      colnames()

    ## exclude (not computed)
    df %>%
      dplyr::select(dplyr::matches("household_income.*not_computed")) %>%
      colnames()

    ### cost_burdened_50percentormore_allincomes_percent
    # cost_burdened_50percentormore_allincomes_percent = safe_divide(
    #   ## numerator -- all households where gross rent is 50% or more of household income
    #   rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*50_0.*percent"))),
    #   ## denominator -- all households with computed rent shares
    #   rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*([0-9]$|100000_more$)"))) -
    #     rowSums(dplyr::select(., dplyr::matches("household_income.*not_computed"))))

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("household_income_by_gross_rent.*50_0.*percent")) %>%
      colnames()

    ## denominator
    df %>%
      dplyr::select(dplyr::matches("household_income_by_gross_rent.*([0-9]$|100000_more$)")) %>%
      colnames()

    ## exclude (not computed)
    df %>%
      dplyr::select(dplyr::matches("household_income.*not_computed")) %>%
      colnames()

    ### cost_burdened_30percentormore_incomeslessthan35000_percent
    # cost_burdened_30percentormore_incomeslessthan35000_percent = safe_divide(
    #   ## numerator -- all households where gross rent is 30% or more of household income
    #   rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*(10000_|19999|34999).*(30_0|35_0|40_0|50_0).*percent"))),
    #   ## denominator -- all households whose household incomes are $34,999 or less
    #   rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*(10000|19999|34999)$"))))

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("household_income_by_gross_rent.*(10000_|19999|34999).*(30_0|35_0|40_0|50_0).*percent")) %>%
      colnames()

    ## denominator
    df %>%
      dplyr::select(dplyr::matches("household_income_by_gross_rent.*(10000|19999|34999)$")) %>%
      colnames()

    ### cost_burdened_50percentormore_incomeslessthan35000_percent
    # cost_burdened_50percentormore_incomeslessthan35000_percent = safe_divide(
    #   ## numerator -- all households where gross rent is 50% or more of household income
    #   rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*(10000_|19999|34999).*50_0.*percent"))),
    #   ## denominator -- all households whose household incomes are $34,999 or less
    #   rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*(10000|19999|34999)$"))))

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("household_income_by_gross_rent.*(10000_|19999|34999).*50_0.*percent")) %>%
      colnames()

    ## denominator
    df %>%
      dplyr::select(dplyr::matches("household_income_by_gross_rent.*(10000|19999|34999)$")) %>%
      colnames()

    ### cost_burdened_30percentormore_incomeslessthan50000_percent
    # cost_burdened_30percentormore_incomeslessthan50000_percent = safe_divide(
    #   ## numerator -- all households where gross rent is 30% or more of household income
    #   rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*(10000_|19999|34999|49999).*(30_0|35_0|40_0|50_0).*percent"))),
    #   ## denominator -- all households whose household incomes are $49,999 or less
    #   rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*(10000|19999|34999|49999)$"))))

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("household_income_by_gross_rent.*(10000_|19999|34999|49999).*(30_0|35_0|40_0|50_0).*percent")) %>%
      colnames()

    ## denominator
    df %>%
      dplyr::select(dplyr::matches("household_income_by_gross_rent.*(10000|19999|34999|49999)$")) %>%
      colnames()

    ### cost_burdened_50percentormore_incomeslessthan50000_percent
    # cost_burdened_50percentormore_incomeslessthan50000_percent = safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*(10000_|19999|34999|49999).*50_0.*percent"))),
    #   rowSums(dplyr::select(., dplyr::matches("household_income_by_gross_rent.*(10000|19999|34999|49999)$"))))

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("household_income_by_gross_rent.*(10000_|19999|34999|49999).*50_0.*percent")) %>%
      colnames()

    ## denominator
    df %>%
      dplyr::select(dplyr::matches("household_income_by_gross_rent.*(10000|19999|34999|49999)$")) %>%
      colnames()

    ### race_nonhispanic_and_race_hispanic_percent
    # dplyr::across(
    #   .cols = dplyr::matches("^race_nonhispanic|^race_hispanic"),
    #   .fns = ~ safe_divide(.x, race_universe),
    #   .names = "{.col}_percent")

    ## numerator (race_nonhispanic and race_hispanic columns)
    df %>%
      dplyr::select(dplyr::matches("^race_nonhispanic|^race_hispanic")) %>%
      colnames()

    ## denominator (race_universe)
    "race_universe"

    ### age_total_years
    # dplyr::across(
    #   .cols = dplyr::matches("sex_by_age_female_.*years($|_over$)"),
    #   .fns = ~ .x + get(dplyr::cur_column() %>% stringr::str_replace("female", "male")),
    #   .names = "{stringr::str_replace(string = .col, pattern = 'sex_by_age_female_', replacement = 'age_')}")

    ## female age variables
    df %>%
      dplyr::select(dplyr::matches("sex_by_age_female_.*years($|_over$)")) %>%
      colnames()

    ## corresponding male age variables
    df %>%
      dplyr::select(dplyr::matches("sex_by_age_male_.*years($|_over$)")) %>%
      colnames()

    ### age_years_percent
    # dplyr::across(
    #   .cols = dplyr::matches("^age.*years($|_over$)"),
    #   .fns = ~ safe_divide(.x, sex_by_age_universe),
    #   .names = "{.col}_percent")

    ## numerator (age variables)
    df %>%
      dplyr::select(dplyr::matches("^age.*years($|_over$)")) %>%
      colnames()

    ## denominator (sex_by_age_universe)
    "sex_by_age_universe"

    ### age_65_to_87_years_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("age_(6(5|7)|7|8).*_years($|_over$)"))),
    #   sex_by_age_universe)

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("age_(6(5|7)|7|8).*_years($|_over$)")) %>%
      colnames()

    ## denominator (sex_by_age_universe)
    "sex_by_age_universe"

    ### with_a_disability_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("with_a_disability"))),
    #   sex_by_age_by_disability_status_universe)

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("with_a_disability")) %>%
      colnames()

    ## denominator (sex_by_age_by_disability_status_universe)
    "sex_by_age_by_disability_status_universe"

    ### tenure_renter_and_owner_occupied_percent
    # dplyr::across(
    #   .cols = dplyr::matches("^tenure_renter_occupied|^tenure_owner_occupied"),
    #   .fns = ~ safe_divide(.x, tenure_universe),
    #   .names = "{.col}_percent")

    ## numerator (renter and owner occupied tenure columns)
    df %>%
      dplyr::select(dplyr::matches("^tenure_renter_occupied|^tenure_owner_occupied")) %>%
      colnames()

    ## denominator (tenure_universe)
    "tenure_universe"

    ### tenure_renter_owner_occupied
    # dplyr::across(
    #   .cols = dplyr::matches("tenure_.*_householder_renter_occupied"),
    #   .fns = ~ .x + get(dplyr::cur_column() %>% stringr::str_replace("renter", "owner")),
    #   .names = "{stringr::str_replace_all(string = .col, pattern = 'renter_occupied', replacement = 'renter_owner_occupied')}")

    ## renter occupied columns
    df %>%
      dplyr::select(dplyr::matches("tenure_.*_householder_renter_occupied")) %>%
      colnames()

    ## corresponding owner occupied columns
    df %>%
      dplyr::select(dplyr::matches("tenure_.*_householder_owner_occupied")) %>%
      colnames()

    ### tenure_householder_renter_occupied_percent
    # dplyr::across(
    #   .cols = dplyr::matches("tenure.*householder_renter_occupied"),
    #   .fns = ~ safe_divide(.x, get(dplyr::cur_column() %>% stringr::str_replace("renter", "renter_owner"))),
    #   .names = "{.col}_percent")

    ## numerator (renter occupied tenure columns)
    df %>%
      dplyr::select(dplyr::matches("tenure.*householder_renter_occupied")) %>%
      colnames()

    ## denominator (corresponding renter_owner tenure columns)
    df %>%
      dplyr::select(dplyr::matches("tenure.*householder_renter_owner")) %>%
      colnames()

    ### tenure_householder_owner_occupied_percent
    # dplyr::across(
    #   .cols = dplyr::matches("tenure.*householder_owner_occupied"),
    #   .fns = ~ safe_divide(.x, get(dplyr::cur_column() %>% stringr::str_replace("owner", "renter_owner"))),
    #   .names = "{.col}_percent")

    ## numerator (owner occupied tenure columns)
    df %>%
      dplyr::select(dplyr::matches("tenure.*householder_owner_occupied")) %>%
      colnames()

    ## denominator (renter_owner universe columns)
    df %>%
      dplyr::select(dplyr::matches("tenure.*householder_renter_owner")) %>%
      colnames()

    ### units_in_structure_percent
    # dplyr::across(
    #   .cols = c(dplyr::matches("^units_in_structure"), -dplyr::matches("universe|householder")),
    #   .fns = ~ safe_divide(.x, units_in_structure_universe),
    #   .names = "{.col}_percent")

    ## numerator (units_in_structure columns excluding universe and householder)
    df %>%
      dplyr::select(c(dplyr::matches("^units_in_structure"), -dplyr::matches("universe|householder"))) %>%
      colnames()

    ## denominator (units_in_structure_universe)
    "units_in_structure_universe"

    ### tenure_by_units_renter_owner_occupied_housing_units
    # dplyr::across(
    #   .cols = c(dplyr::matches("tenure_by_units.*renter_occupied_housing_units"), -dplyr::matches("owner")),
    #   .fns = ~ .x + get(dplyr::cur_column() %>% stringr::str_replace("renter", "owner")),
    #   .names = "{stringr::str_replace_all(string = .col, pattern = 'renter_occupied_housing_units', replacement = 'renter_owner_occupied_housing_units')}")

    ## renter occupied housing units columns
    df %>%
      dplyr::select(c(dplyr::matches("tenure_by_units.*renter_occupied_housing_units"), -dplyr::matches("owner"))) %>%
      colnames()

    ## owner occupied housing units columns corresponding to renter occupied
    df %>%
      dplyr::select(dplyr::matches("tenure_by_units.*owner_occupied_housing_units")) %>%
      colnames()

    ### tenure_by_units_in_structure_renter_owner_occupied_housing_units_percent
    # dplyr::across(
    #   .cols = dplyr::matches("tenure_by_units_in_structure_renter_owner_occupied_housing_units_"),
    #   .fns = ~ safe_divide(.x, tenure_by_units_in_structure_renter_owner_occupied_housing_units),
    #   .names = "{.col}_percent")

    ## numerator (renter_owner_occupied housing units by units in structure)
    df %>%
      dplyr::select(dplyr::matches("tenure_by_units_in_structure_renter_owner_occupied_housing_units_")) %>%
      colnames()

    ## denominator (total tenure_by_units_in_structure_renter_owner_occupied_housing_units)
    "tenure_by_units_in_structure_renter_owner_occupied_housing_units"

    ### tenure_by_units_in_structure_renter_occupied_housing_units_percent
    # dplyr::across(
    #   .cols = dplyr::matches("tenure_by_units_in_structure_renter_occupied_housing_units_"),
    #   .fns = ~ safe_divide(.x, tenure_by_units_in_structure_renter_occupied_housing_units),
    #   .names = "{.col}_percent")

    ## numerator (renter occupied housing units by units in structure)
    df %>%
      dplyr::select(dplyr::matches("tenure_by_units_in_structure_renter_occupied_housing_units_")) %>%
      colnames()

    ## denominator (total tenure_by_units_in_structure_renter_occupied_housing_units)
    "tenure_by_units_in_structure_renter_occupied_housing_units"

    ### tenure_by_units_in_structure_owner_occupied_housing_units_percent
    # dplyr::across(
    #   .cols = dplyr::matches("tenure_by_units_in_structure_owner_occupied_housing_units_"),
    #   .fns = ~ safe_divide(.x, tenure_by_units_in_structure_owner_occupied_housing_units),
    #   .names = "{.col}_percent")

    ## numerator (owner occupied housing units by units in structure)
    df %>%
      dplyr::select(dplyr::matches("tenure_by_units_in_structure_owner_occupied_housing_units_")) %>%
      colnames()

    ## denominator (total tenure_by_units_in_structure_owner_occupied_housing_units)
    "tenure_by_units_in_structure_owner_occupied_housing_units"

    ### tenure_by_occupants_per_room_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("tenure_by_occupants_per_room.*(1_01|1_51|2_01)"))),
    #   tenure_by_occupants_per_room_universe)

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("tenure_by_occupants_per_room.*(1_01|1_51|2_01)")) %>%
      colnames()

    ## denominator (tenure_by_occupants_per_room_universe)
    "tenure_by_occupants_per_room_universe"

    ### tenure_by_occupants_per_room_renter_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("tenure_by_occupants_per_room_renter.*(1_01|1_51|2_01)"))),
    #   tenure_by_occupants_per_room_renter_occupied)

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("tenure_by_occupants_per_room_renter.*(1_01|1_51|2_01)")) %>%
      colnames()

    ## denominator (tenure_by_occupants_per_room_renter_occupied)
    "tenure_by_occupants_per_room_renter_occupied"

    ### year_structure_built_percent
    # dplyr::across(
    #   .cols = dplyr::matches("year_structure_built_built_[0-9].*"),
    #   .fns = ~ safe_divide(.x, get(dplyr::cur_column() %>%
    #     stringr::str_replace("[0-9].*", "universe") %>%
    #     stringr::str_replace("built_", ""))),
    #   .names = "{.col}_percent")

    ## numerator (year_structure_built_built_ columns)
    df %>%
      dplyr::select(dplyr::matches("year_structure_built_built_[0-9].*")) %>%
      colnames()

    ## denominator (corresponding universe columns transformed from built year)
    df %>%
      dplyr::select(dplyr::matches("year_structure_built_universe")) %>%
      colnames()

    ### year_structure_built_1940s_to_2000s_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("year_structure_built_built_(19[4-9]|2).*"))),
    #   year_structure_built_universe)

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("year_structure_built_built_(19[4-9]|2).*")) %>%
      colnames()

    ## denominator (year_structure_built_universe)
    "year_structure_built_universe"

    ### year_structure_built_1950s_to_present_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("year_structure_built_built_(19[5-9]|2).*"))),
    #   year_structure_built_universe)

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("year_structure_built_built_(19[5-9]|2).*")) %>%
      colnames()

    ## denominator (year_structure_built_universe)
    "year_structure_built_universe"

    ### year_structure_built_1960s_to_present_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("year_structure_built_built_(19[6-9]|2).*"))),
    #   year_structure_built_universe)

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("year_structure_built_built_(19[6-9]|2).*")) %>%
      colnames()

    ## denominator (year_structure_built_universe)
    "year_structure_built_universe"

    ### year_structure_built_1970s_to_present_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("year_structure_built_built_(19[7-9]|2).*"))),
    #   year_structure_built_universe)

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("year_structure_built_built_(19[7-9]|2).*")) %>%
      colnames()

    ## denominator (year_structure_built_universe)
    "year_structure_built_universe"

    ### year_structure_built_1980s_to_present_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("year_structure_built_built_(19[8-9]|2).*"))),
    #   year_structure_built_universe)

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("year_structure_built_built_(19[8-9]|2).*")) %>%
      colnames()

    ## denominator (year_structure_built_universe)
    "year_structure_built_universe"

    ### year_structure_built_1990s_to_present_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("year_structure_built_built_(19[9]|2).*"))),
    #   year_structure_built_universe)

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("year_structure_built_built_(19[9]|2).*")) %>%
      colnames()

    ## denominator (year_structure_built_universe)
    "year_structure_built_universe"

    ### year_structure_built_2000s_to_present_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("year_structure_built_built_(200|201|202).*"))),
    #   year_structure_built_universe)

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("year_structure_built_built_(200|201|202).*")) %>%
      colnames()

    ## denominator (year_structure_built_universe)
    "year_structure_built_universe"

    ### year_structure_built_2010s_to_present_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("year_structure_built_built_(201|202).*"))),
    #   year_structure_built_universe)

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("year_structure_built_built_(201|202).*")) %>%
      colnames()

    ## denominator (year_structure_built_universe)
    "year_structure_built_universe"

    ### year_structure_built_2020s_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("year_structure_built_built_202.*"))),
    #   year_structure_built_universe)

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("year_structure_built_built_202.*")) %>%
      colnames()

    ## denominator (year_structure_built_universe)
    "year_structure_built_universe"

    ### means_transportation_percent
    # dplyr::across(
    #   .cols = c(dplyr::matches("means_transportation"), -dplyr::matches("universe|worked_from_home")),
    #   .fns = ~ safe_divide(.x, (means_transportation_work_universe - means_transportation_work_worked_from_home)),
    #   .names = "{.col}_percent")

    ## numerator (means_transportation columns excluding universe and worked_from_home)
    df %>%
      dplyr::select(c(dplyr::matches("means_transportation"), -dplyr::matches("universe|worked_from_home"))) %>%
      colnames()

    ## denominator (means_transportation_work_universe minus worked_from_home)
    c("means_transportation_work_universe", "means_transportation_work_worked_from_home")

    ### means_transportation_bicycle_walked_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("means_transportation_work_(bicycle|walked)$"))),
    #   (means_transportation_work_universe - means_transportation_work_worked_from_home))

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("means_transportation_work_(bicycle|walked)$")) %>%
      colnames()

    ## denominator (means_transportation_work_universe minus means_transportation_work_worked_from_home)
    c("means_transportation_work_universe", "means_transportation_work_worked_from_home")


    ### means_transportation_work_motor_vehicle_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("means_transportation_work_(car_truck_van|taxicab|motorcycle)$"))),
    #   (means_transportation_work_universe - means_transportation_work_worked_from_home))

    ## numerator
    df %>%
      dplyr::select(dplyr::matches("means_transportation_work_(car_truck_van|taxicab|motorcycle)$")) %>%
      colnames()

    ## denominator (means_transportation_work_universe minus means_transportation_work_worked_from_home)
    c("means_transportation_work_universe", "means_transportation_work_worked_from_home")


    ### travel_time_work_percent
    # dplyr::across(
    #   .cols = c(dplyr::matches("travel_time_work"), -travel_time_work_universe),
    #   .fns = ~ safe_divide(.x, travel_time_work_universe),
    #   .names = "{.col}_percent")

    ## numerator (travel_time_work-related columns excluding universe)
    df %>%
      dplyr::select(c(dplyr::matches("travel_time_work"), -travel_time_work_universe)) %>%
      colnames()

    ## denominator (travel_time_work_universe)
    "travel_time_work_universe"


    ### educational_attainment_no_schooling_to_8th_grade_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("educational_attainment.*(no_schooling|nursery|kindergarten|_[0-8]th_grade)"))),
    #   educational_attainment_population_25_years_over_universe)

    ## numerator (educational attainment levels from no schooling to 8th grade)
    df %>%
      dplyr::select(dplyr::matches("educational_attainment.*(no_schooling|nursery|kindergarten|_[0-8]th_grade)")) %>%
      colnames()

    ## denominator (population 25 years and over universe)
    "educational_attainment_population_25_years_over_universe"


    ### educational_attainment_9th_to_12th_grade_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("educational_attainment.*(9th|10th|11th|12th)"))),
    #   educational_attainment_population_25_years_over_universe)

    ## numerator (educational attainment levels 9th through 12th grade)
    df %>%
      dplyr::select(dplyr::matches("educational_attainment.*(9th|10th|11th|12th)")) %>%
      colnames()

    ## denominator (population 25 years and over universe)
    "educational_attainment_population_25_years_over_universe"


    ### educational_attainment_some_college_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("educational_attainment.*some_college"))),
    #   educational_attainment_population_25_years_over_universe)

    ## numerator (educational attainment: some college)
    df %>%
      dplyr::select(dplyr::matches("educational_attainment.*some_college")) %>%
      colnames()

    ## denominator (population 25 years and over universe)
    "educational_attainment_population_25_years_over_universe"


    ### educational_attainment_advanced_degrees_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("educational_attainment.*(masters|professional|doctorate)"))),
    #   educational_attainment_population_25_years_over_universe)

    ## numerator (educational attainment: masters, professional, doctorate)
    df %>%
      dplyr::select(dplyr::matches("educational_attainment.*(masters|professional|doctorate)")) %>%
      colnames()

    ## denominator (population 25 years and over universe)
    "educational_attainment_population_25_years_over_universe"


    ### school_enrollment_non_universe_percent
    # safe_divide(
    #   school_enrollment_universe - rowSums(dplyr::select(., dplyr::matches("school_enrollment.*[^(_universe)]"))),
    #   school_enrollment_universe)

    ## numerator (school enrollment universe minus selected school_enrollment categories)
    df %>%
      dplyr::select(dplyr::matches("school_enrollment.*[^(_universe)]")) %>%
      colnames()

    ## denominator (school_enrollment_universe)
    "school_enrollment_universe"


    ### school_enrollment_percent
    # dplyr::across(
    #   .cols = dplyr::matches("school_enrollment.*[^(_universe)]"),
    #   .fns = ~ safe_divide(.x, school_enrollment_universe),
    #   .names = "{.col}_percent")

    ## numerator (school enrollment categories excluding universe)
    df %>%
      dplyr::select(dplyr::matches("school_enrollment.*[^(_universe)]")) %>%
      colnames()

    ## denominator (school_enrollment_universe)
    "school_enrollment_universe"

    ### nativity_english_proficiency_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("nativity.*(only_english|english_very_well)"))),
    #   nativity_by_language_spoken_at_home_by_ability_speak_english_population_5_years_over_universe)

    ## numerator (nativity categories: only English or English very well)
    df %>%
      dplyr::select(dplyr::matches("nativity.*(only_english|english_very_well)")) %>%
      colnames()

    ## denominator (population 5 years and over universe by language spoken and English ability)
    "nativity_by_language_spoken_at_home_by_ability_speak_english_population_5_years_over_universe"

    ### health_insurance_coverage_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("health_insurance_coverage_status_type_by_employment_status.*with_health_insurance_coverage$"))),
    #   health_insurance_coverage_status_type_by_employment_status_universe)

    ## numerator (with health insurance coverage columns by employment status)
    df %>%
      dplyr::select(dplyr::matches("health_insurance_coverage_status_type_by_employment_status.*with_health_insurance_coverage$")) %>%
      colnames()

    ## denominator (health insurance coverage universe by employment status)
    "health_insurance_coverage_status_type_by_employment_status_universe"

    ### health_insurance_coverage_employed_in_labor_force_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("health_insurance_coverage_status_type_by_employment_status.*_employed.*with_health_insurance_coverage$"))),
    #   health_insurance_coverage_status_type_by_employment_status_in_labor_force)

    ## numerator (employed with health insurance coverage by employment status)
    df %>%
      dplyr::select(dplyr::matches("health_insurance_coverage_status_type_by_employment_status.*_employed.*with_health_insurance_coverage$")) %>%
      colnames()

    ## denominator (in labor force by health insurance coverage status and employment status)
    "health_insurance_coverage_status_type_by_employment_status_in_labor_force"

    ### health_insurance_coverage_unemployed_in_labor_force_percent
    # safe_divide(
    #   rowSums(dplyr::select(., dplyr::matches("health_insurance_coverage_status_type_by_employment_status.*_unemployed.*with_health_insurance_coverage$"))),
    #   health_insurance_coverage_status_type_by_employment_status_in_labor_force)

    ## numerator (unemployed with health insurance coverage by employment status)
    df %>%
      dplyr::select(dplyr::matches("health_insurance_coverage_status_type_by_employment_status.*_unemployed.*with_health_insurance_coverage$")) %>%
      colnames()

    ## denominator (in labor force by health insurance coverage status and employment status)
    "health_insurance_coverage_status_type_by_employment_status_in_labor_force"
  } )



