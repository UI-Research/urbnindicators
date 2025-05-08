#' @title Pretty-ify variable names
#' @description Variable names are lower-case and underscore-separated. This converts
#' them to title case and replaces underscores with spaces. It also makes other adjustments
#' to facilitate reading and interpretation of variables in the context of, for example,
#' plots and tables.
#' @param .data A data.frame--or something coercible thereto--or a character vector containing
#' variables/variable names that will be converted to more publication-appropriate formats.
#' @examples
#' \dontrun{
#' # df = compile_acs_data(
#   variables = list_acs_variables(year = 2022),
#   years = c(2022),
#   geography = "county",
#   states = "NJ",
#   counties = NULL,
#   spatial = FALSE)
#'
#' df %>% make_pretty_names()
#' }
#' @export
#' @importFrom magrittr %>%

make_pretty_names = function(.data) {

  names_mapping = c(
    "_" = " ",
    "universe" = "(universe)",
    "quintile upper limit" = "(quintile)",
    "quintile mean" = "(quintile, mean)",
    "quintile share aggregate" = "(quintile, share total)",
    "allraces" = "all races",
    "twoormore" = "two or more",
    "includingotherrace" = "including other race",
    "excludingotherrace" = "excluding other race",
    "0 50 less" = ".5 or fewer",
    "0 51 1 00" = ".51 to 1",
    "1 00 less" = "1 or fewer",
    "1 51 2 00" = "1.51-2",
    "2 01 more" = "more than 2",
    "1 01 more" = "more than 1",
    "morethan1" = "more than 1",
    "ppr" = "person per room",
    "built built" = "built",
    "etc" = "",
    "2020 later" = "2020 or later",
    "1939 earlier" = "1939 or earlier",
    "household income by gross rent as a percentage household income in past 12 months" =
      "income by rent as a share of income",
    "housing cost monthly median" = "median monthly housing cost",
    "median household income in past 12 months" = "median household income",
    "means transportation work" = "commute mode",
    "travel time work" = "commute time",
    "minutes" = "",
    " population 25 years over" = " 25+",
    "notenrolled" = "not enrolled",
    "morethanbachelors" = "more than bachelors",
    "nativity by language spoken at home by ability speak english population 5 years over" =
      "nativity by primary language by english proficiency 5+ years",
    "very well better" = "very well or better",
    "employment civilian labor force" = "labor force",
    "health insurance coverage status type by employment status" =
      "health insurance by employment",
    "notcovered" = "not covered",
    "internet subscription household" = "internet access",
    "types of computing devices household" = "computer access",
    "owneroccupied" = "owner-occupied",
    "renteroccupied" = "renter-occuiped",
    "owner occupied" = "owner-occupied",
    "renter occupied" = "renter-occupied",
    "percentormore" = " percent or more",
    "percentage|percent" = "%",
    "%$" = "(%)",
    "sq kilometer" = "(sq. km)")

  capitalization_spacing_other_fixes = c(
    "In " = "in ",
    "By " = "by ",
    "With " = "with ",
    "Not " = "not ",
    "Of " = "of ",
    "To " = "to ",
    "For " = "for ",
    "As " = "as ",
    "A " = "a ",
    "Or " = "or ",
    "More " = "more ",
    "Fewer " = "fewer ",
    "Than " = "than ",
    "Below " = "below ",
    " Rv" = " RV",
    "Aian" = "AIAN",
    "Nhpi" = "NHPI",
    "Incomeslessthan" = "Income less than ",
    "Allincomes" = "All Incomes",
    "Snap " = "SNAP ",
    "1 01 1 50" = "1.01-1.50",
    "5 17" = "5-17",
    "18 34" = "18-34",
    "35 64" = "35-64",
    "65 74" = "65-74",
    "5 9" = "5-9",
    "10 14" = "10-14",
    "15 17" = "15-17",
    "18 19" = "18-19",
    "22 24" = "22-24",
    "25 29" = "25-29",
    "30 34" = "30-34",
    "35 39" = "35-39",
    "40 44" = "40-44",
    "45 49" = "45-49",
    "50 54" = "50-54",
    "55 59" = "55-59",
    "60 61" = "60-61",
    "62 64" = "62-64",
    "65 66" = "65-66",
    "67 69" = "67-69",
    "70 74" = "70-74",
    "75 79" = "75-79",
    "80 84" = "80-84",
    "3 4" = "3-4",
    "5 9" = "5-9",
    "10 19" = "10-19",
    "20 49" = "20-49",
    "2010 2019" = "2010-2019",
    "2000 2009" = "2000-2009",
    "1990 1999" = "1990-1999",
    "1980 1989" = "1980-1989",
    "1970 1979" = "1970-1979",
    "1960 1969" = "1960-1969",
    "1950 1959" = "1950-1959",
    "1940 1949" = "1940-1949",
    "20 0" = "20.0",
    "24 9" = "24.9",
    "25 0" = "25.0",
    "29 9" = "29.9",
    "30 0" = "30.0",
    "34 9" = "34.9",
    "35 0" = "35.0",
    "39 9" = "39.9",
    "40 0" = "40.0",
    "49 9" = "49.9",
    "50 0" = "50.0",
    "20 29" = "20-29",
    "10 14" = "10-14",
    "15 19" = "15-19",
    "20 24" = "20-24",
    "25 29" = "25-29",
    "30 34" = "30-34",
    "35 39" = "35-39",
    "40 44" = "40-44",
    "45 59" = "45-59",
    "60 89" = "60-89"
    )

  if (! (is.data.frame(.data) | is.character(.data))) {
    stop("The `.data` argument must be either a data.frame (or coercible thereto)
         or a character vector.")
  }

  if (is.character(.data)) {
    result = .data %>%
      stringr::str_replace_all(names_mapping) %>%
      stringr::str_squish() %>%
      stringr::str_trim() %>%
      stringr::str_to_title() %>%
      stringr::str_replace_all(capitalization_spacing_other_fixes)
  }

  if (is.data.frame(.data)) {
    result = .data %>%
      dplyr::rename_with(
        .fn = ~ stringr::str_replace_all(.x, names_mapping) %>%
          stringr::str_squish() %>%
          stringr::str_trim() %>%
          stringr::str_to_title() %>%
          stringr::str_replace_all(capitalization_spacing_other_fixes))
  }

  return(result)
}


