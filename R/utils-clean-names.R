#' @importFrom magrittr %>%

## Clean raw ACS variable names into human-readable snake_case names.
##
## Takes a data frame with `concept`, `label`, and `name` columns (as returned
## by `tidycensus::load_variables()`) and returns it with a `clean_names` column
## appended.  The cleaning logic is shared by `select_variables_by_name()` and
## `generate_codebook()`.
clean_acs_names = function(variables_df) {
  variables_df %>%
    dplyr::mutate(
      clean_names = paste0(
        concept, "_",
        label) %>%
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
          "__" = "_",
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
            "household_income_by_gross_rent_as_a_percentage_of_household_income",
          "_percent$" = "_pct")),
      clean_names = dplyr::if_else(
        label %in% c("Estimate!!Total:", "Estimate!!Total"),
        paste0(clean_names, "_universe_"),
        paste0(clean_names, "_")))
}
