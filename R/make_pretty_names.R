#' @title Pretty-ify variable names for charting
#' @description Convert variable names into versions appropriate for charting or
#' other forms of publication.
#' @param .data A data.frame--or something coercible thereto--or a character vector containing
#' variables/variable names that will be converted more publication-appropriate formats.
#' @param .case Capitalization scheme of resulting variable names. One of "title", "sentence", or "upper".
#' @examples
#' \dontrun{
#' "race_personofcolor_percent" %>% make_pretty_names()
#' }
#' @export
#' @importFrom magrittr %>%
make_pretty_names = function(.data, .case = "title") {

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
    "personofcolor" = "person of color",
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
      "nativity by primary language by english proficiency 5+",
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


  if (! (is.data.frame(.data) | is.character(.data))) {
    stop("The `.data` argument must be either a data.frame, or coercible thereto,
         or a character vector.")
  }

  if (is.character(.data)) {
    result = .data %>%
      stringr::str_replace_all(names_mapping) %>%
      stringr::str_squish() %>%
      stringr::str_trim()
  }

  if (is.data.frame(.data)) {
    result = .data %>%
      dplyr::rename_with(
        .fn = ~ stringr::str_replace_all(.x, names_mapping) %>%
          stringr::str_squish() %>%
          stringr::str_trim())
  }

  output = switch(
    .case,
    title = dplyr::rename_with(result, stringr::str_to_title),
    sentence = dplyr::rename_with(result, stringr::str_to_sentence),
    upper = dplyr::rename_withs(result, stringr::str_to_upper))

  return(output)
}
