#' @title Calculate coefficients of variation (CoV)
#' @details Create CoVs for all ACS estimates and derived indicators
#' @param .data The dataset returned from `compile_acs_data()`.
#' @returns A modified dataframe that includes newly calculated indicators.
#' @examples
#' \dontrun{
#' df = compile_acs_data(
#'   variables = list_acs_variables(year = 2022),
#'   years = c(2022),
#'   geography = "county",
#'   states = "NJ",
#'   counties = NULL,
#'   retain_moes = TRUE,
#'   spatial = FALSE)
#' internal_compute_acs_variables(.data = df)
#' }
#' @importFrom magrittr %>%

calculae_covs = function() {

  df = compile_acs_data(
    variables = list_acs_variables(year = 2022),
    years = c(2022),
    geography = "county",
    states = "NJ",
    counties = NULL,
    retain_moes = TRUE,
    spatial = FALSE)

  codebook = df %>%
    attr("codebook")


  extract_definition_terms = function(.definition, .type) {
    .definition %>%
      stringr::str_extract(paste0(.type, " = .*\\.")) %>%
      stringr::str_remove_all("Numerator = |Denominator = |\\.") %>%
      stringr::str_remove_all("(\\(.*\\))?") %>%
      stringr::str_trim() %>% stringr::str_squish()
  }

  percent_codebook = codebook %>%
    dplyr::filter(variable_type == "Percent") %>%
    dplyr::mutate(
      numerator = extract_definition_terms(definition, .type = "Numerator"),
      denominator = extract_definition_terms(definition, .type = "Denominator"))

  df %>%
    dplyr::transmute(
      dplyr::across(
        .cols = any_of(percent_codebook$calculated_variable[1:10]),
        .fns = function(x) {

          current_column = dplyr::cur_column()
          tidycensus::moe_prop(
            num = get(percent_codebook %>% dplyr::filter(calculated_variable == current_column) %>% dplyr::pull(numerator)),
            denom = get(percent_codebook %>% dplyr::filter(calculated_variable == current_column) %>% dplyr::pull(denominator)),
            moe_num = get(percent_codebook %>% dplyr::filter(calculated_variable == current_column) %>% dplyr::pull(numerator) %>% paste0("_M")),
            moe_denom = get(percent_codebook %>% dplyr::filter(calculated_variable == current_column) %>% dplyr::pull(denominator) %>% paste0("_M")))
          },
        .names = "{.col}_cov"))

  df %>%
    #dplyr::select(dplyr::matches("year_structure_built_universe")) %>%
    dplyr::transmute(
      test = tidycensus::moe_prop(
        num = .data[[numerator]],
        denom = .data[[denominator]],
        moe_num = get(paste0(numerator, "_M")),
        moe_denom = get(paste0(denominator, "_M"))))

      dplyr::across(
        .cols = -matches("$_M"),
        .fns = ~
        ),
        .names = "{.col}_cov",
    ))

  df %>%
    dplyr::select(dplyr::matches("year_structure_built_built_since_1940_percent"))
    dplyr::select(dplyr::matches("percent")) %>%
    colnames

  select(-matches(""))
}
