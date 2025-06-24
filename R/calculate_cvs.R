#' @importFrom magrittr %>%

#' @title Calculate a simple standard error
#' @details Create a standard error at the 90% level from a 90% margin of error
#' @param moe A margin of error, or a vector thereof
#' @returns A 90% standard error
se_simple = function(moe) {
  se = purrr::map_dbl(moe,  ~ .x / 1.645)
  return(se)
}

#' @title Calculate a pooled standard error for a summed or subtracted estimate
#' @details For an estimate derived by adding or subtracting multiple estimates, calculate the pooled standard error
#' @param ... The unquoted names of the margin of error and corresponding estimate variables to be added or subtracted
#' @returns A pooled 90% standard error
se_sum = function(...) {
  dots = list(...)

  moes = dots[[1]] %>%
    as.data.frame() %>%
    stats::setNames(paste0("x", seq_along(.))) %>%
    dplyr::mutate(
      observation = dplyr::row_number(),
      type = "moe")
  estimates = dots[[2]] %>%
    as.data.frame() %>%
    stats::setNames(paste0("x", seq_along(.))) %>%
    dplyr::mutate(
      observation = dplyr::row_number(),
      type = "estimate")

  data_long = rbind(moes, estimates) %>%
    janitor::clean_names() %>%
    tidyr::pivot_longer(
      cols = -c(observation, type),
      names_to = "name",
      values_to = "value")  %>%
    tidyr::pivot_wider(
      names_from = type,
      values_from = value)

  ## if there are multiple zero-estimate observations that are summed,
  ## Census recommends taking only the largest MOE corresponding to these estimates:
  se =
    dplyr::bind_rows(
      data_long %>% dplyr::filter(estimate != 0),
      data_long %>%
        dplyr::filter(estimate == 0) %>%
        #AS: you could also use dplyr::slice_max() after
        #- group_by() to avoid having to arrange first
        dplyr::arrange(dplyr::desc(moe)) %>%
        dplyr::group_by(observation) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()) %>%
    dplyr::group_split(observation) %>%
    purrr::map(~ .x %>% dplyr::pull(moe)) %>%
    purrr::map(se_simple) %>%
    purrr::map(~ .x ^2) %>%
    purrr::map(sum) %>%
    purrr::map_dbl(sqrt)

  return(se)
}

#' @title Calculate a pooled standard error for a proportion or ratio
#' @details For an estimate derived using division--whether the resulting estimate is a proportion or a ratio--calculate the pooled standard error. While there are convenience parameters that support both SEs and MOEs for the numerator and denominator, only one of these--either SEs or MOEs--can be supplied--the other must be left NULL.
#' @param estimate_numerator The estimate of the numerator
#' @param estimate_denominator The estimate of the denominator
#' @param moe_numerator The margin of error of the numerator
#' @param moe_denominator The margin of error of the denominator
#' @param se_numerator The standard error of the numerator
#' @param se_denominator The standard error of the denominator
#' @param type The type of estimate being calculated, either "proportion" or "ratio"
#' @returns A pooled 90% standard error
se_proportion_ratio = function(
    estimate_numerator,
    estimate_denominator,
    moe_numerator = NULL,
    moe_denominator = NULL,
    se_numerator = NULL,
    se_denominator = NULL,
    type = "proportion") {

  if (all(is.null(c(moe_numerator, se_numerator))) | all(is.na(c(moe_denominator, se_denominator)))) {
    stop("A margin of error or standard error must be provided for both the numerator and the denominator.")
  }

  if( ((!is.null(moe_numerator) & !is.null(se_numerator))) | (!is.null(moe_denominator) & !is.null(se_denominator))) {
    stop("Only one of a margin of error or a standard error can be provided for the numerator and denominator.")
  }

  if (!is.null(moe_numerator)) { se_numerator = purrr::map_dbl(moe_numerator, se_simple) }
  if (!is.null(moe_denominator)) { se_denominator = purrr::map_dbl(moe_denominator, se_simple) }

  ## squared standard error of the numerator
  radical_term_one = se_numerator %>% `^`(2)

  ## squared numerator over the squared denominator times the squared standard error of the denominator
  radical_term_two = ( (estimate_numerator ^ 2) / (estimate_denominator ^ 2) ) *
    (se_denominator %>% `^`(2))

  ## if the value under the radical is negative, use the formula for ratio standard errors
  ## i.e., add the radical terms, rather than subtract them
  se = dplyr::if_else(
    radical_term_one < radical_term_two | type == "ratio",
    ((1 / estimate_denominator) * sqrt( radical_term_one + radical_term_two )),
    ((1 / estimate_denominator) * sqrt( radical_term_one - radical_term_two )))

  return(se)
}

#' @title Calculate a coefficient of variation
#' @details Return a coefficient of variation at the 90% level
#' @param estimate The estimate
#' @param se The standard error
#' @returns A coefficient of variation at the 90% level
cv = function(estimate, se) {
  cv = se / estimate * 100

  ## when the estimate is zero, this produces an infinite value
  ## replacing this with an NA value
  cv = dplyr::if_else(is.infinite(cv), NA, cv)

  return(cv)
}

## helper function -- pull out the numerator and denominator from the definition
extract_definition_terms = function(.definition, .type) {
  .definition %>%
    stringr::str_extract_all(paste0(.type, " .*?\\.")) %>%
    stringr::str_remove_all("Numerator = |Denominator = |Sum of: |\\.") %>%
    stringr::str_remove_all("\\(.*?\\)") %>%
    stringr::str_trim() %>% stringr::str_squish() %>%
    stringr::str_replace_all(c(" ," = ",", " -" = ","))
}

#' @title Calculate coefficients of variation
#' @details Create CVs for all ACS estimates and derived indicators
#' @param .df The dataset returned from \code{compile_acs_data()}.
#'  The argument to this parameter must have an attribute named `codebook` (as is)
#'  true of results from \code{compile_acs_data()}.
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
#' cvs = calculate_cvs(df) %>%
#'  dplyr::select(matches("_CV$"))
#' }

calculate_cvs = function(.df) {
  warning("Coefficients of variation and related, calculated measures of error such as margins of error and standard errors are experimental features and should be used with great caution. Such measures are respectively suffixed with `_CV`, `_M`, and `_SE`.")

  ## the codebook attached to the default compile_acs_data() return
  codebook = .df %>% attr("codebook")

  ## modified codebook prepared for calculating CVs
  codebook1 = codebook %>%
    dplyr::distinct(calculated_variable, .keep_all = TRUE) %>%
    dplyr::filter(!stringr::str_detect(calculated_variable, "_M$")) %>%
    dplyr::mutate(
      numerator_count = dplyr::case_when(
        definition == "This is a raw ACS estimate." ~ NA_real_,
        #AS: Is there a reason the condition here just checks for "Metadata" but for
        # - the denominator_count it checks for metatada and sum? It strikes me that
        # - the conditions should be parallel?
        variable_type %in% c("Metadata") ~ NA_real_,
        #AS: I might suggest pulling this logic into a helper function or at least adding
        # - a comment explaining what it's doing.
        TRUE ~ stringr::str_extract(definition, "Numerator = .*\\.") %>% stringr::str_count(",") + 1),
      denominator_count = dplyr::case_when(
        definition == "This is a raw ACS estimate." ~ NA_real_,
        variable_type %in% c("Metadata", "Sum") ~ NA_real_,
        TRUE ~ stringr::str_extract(definition, "Denominator = .*\\.") %>% stringr::str_count(",|-") + 1),
      numerator = dplyr::case_when(
        definition == "This is a raw ACS estimate." ~ NA_character_,
        variable_type %in% c("Metadata") ~ NA_character_,
        variable_type == "Sum" ~ extract_definition_terms(definition, .type = "Sum"),
        TRUE ~ extract_definition_terms(definition, .type = "Numerator")),
      denominator = dplyr::case_when(
        definition == "This is a raw ACS estimate." ~ NA_character_,
        variable_type %in% c("Metadata", "Sum") ~ NA_character_,
        TRUE ~ extract_definition_terms(definition, .type = "Denominator")),
      ## these variables do not have associated MOEs when returned from the Census API
      no_moe_flag = dplyr::case_when(
        #AS: It seems to me that the use of controlled estimates is a bit more nuanced than
        # - applying a rule to at the variable level. For example, it seems that these variables
        # - are controlled only at some geographic levels - for example, the following call:
        # - get_acs(geography = "county", variables = c("B01001_001", "B01001_002"), state = "CO")
        # - returns NA MOEs for some but not all counties. And when I change geography to "tract"
        # - all of the MOEs are non-NA. It seems to me that a better approach would involve
        # - testing for NA values in the MOE column. I'd also flag that I believe there are
        # - other controlled variables, such as group quarters per the link below. I've also
        # - seen some mention of household controlled estimates but haven't been able to find them.
        # - https://www.census.gov/programs-surveys/acs/technical-documentation/user-notes/2024-02.html
        # - I might suggest defining a controlled_variables object with this vector and using
        # - throughout instead of hard-coding in multiple places
        # - I would also recommend checking out footnote 9 on p 11 of this doc for more on controlled
        # - estimates and geography:
        # - https://www2.census.gov/programs-surveys/acs/replicate_estimates/2023/documentation/5-year/2019-2023_Variance_Replicate_Table_Documentation.pdf
        denominator %in% c("sex_by_age_universe", "race_universe", "total_population_universe") ~ 1,
        TRUE ~ 0),
      #AS: noting that for the test data this variable is always equal to 0
      # - flagging to confirm that is intended
      calculated_variable_dependency_flag = dplyr::if_else(
        stringr::str_detect(definition, "calculated variable"), 1, 0),
      moe_type = dplyr::case_when(
        definition == "This is a raw ACS estimate." ~ "raw",
        stringr::str_detect(definition, "minus") ~ "minus",
        stringr::str_detect(definition, "sum") & variable_type == "Sum" ~ "sum",
        numerator_count == 1 & denominator_count == 1 ~ "simple percent",
        numerator_count == 1 & denominator_count > 1 ~ "complex denominator percent",
        numerator_count > 1 & denominator_count == 1 ~ "complex numerator percent",
        numerator_count > 1 & denominator_count > 1 ~ "complex numerator and complex denominator percent",
        TRUE ~ "unspecified"),
      variable_class = dplyr::case_when(
        #AS: Not urgent, but I think you could simplify these conditions using the
        # - moe_type var you just created
        ## for example: snap_received_percent
        variable_type == "Percent" & numerator_count == 1 & denominator_count == 1 &
          calculated_variable_dependency_flag == 0 & no_moe_flag == 0 ~ "simple percent, no calculated variables",
        ## for example: disability_percent
        variable_type == "Percent" & numerator_count > 1 & denominator_count == 1 &
          calculated_variable_dependency_flag == 0 & no_moe_flag == 0 ~ "numerator sum percent",
        ## for example: means_transportation_work_bicycle_percent
        variable_type == "Percent" & numerator_count == 1 & denominator_count > 1 &
          calculated_variable_dependency_flag == 0 & no_moe_flag == 0 ~ "denominator sum percent",
        ## for example: cost_burdened_30percentormore_allincomes_percent
        variable_type == "Percent" & numerator_count > 1 & denominator_count > 1 &
          calculated_variable_dependency_flag == 0 & no_moe_flag == 0 ~ "numerator and denominator sum percent",
        ## for example: age_10_14_years
        variable_type == "Sum" & calculated_variable_dependency_flag == 0 & no_moe_flag == 0 ~ "sum",
        ## percents where there's no MOE for the denominator
        ## for example: sex_female_percent
        no_moe_flag == 1 & variable_type == "Percent" ~ "no MOE denominator",
        calculated_variable %in% c("total_population_universe", "sex_by_age_universe", "race_universe") ~ "no MOE count",
        ## basic SE calculations with se_simple
        ## for example: snap_universe
        stringr::str_detect(definition, "This is a raw ACS estimate.") ~ "raw",
        moe_type == "minus" ~ "one minus percentage",
        TRUE ~ "non-ACS variable"))

  ## all variables for which to calculate CVs
  cv_variables = codebook1 %>%
    dplyr::filter(!variable_class %in% c("non-ACS variable", "no MOE count")) %>%
    dplyr::pull(calculated_variable)

  ## a named list of of variables, with each list element named by the
  ## variable class it encompasses
  variable_class_groups = codebook1 %>%
    dplyr::filter(calculated_variable %in% cv_variables) %>%
    dplyr::group_split(variable_class)
  variable_class_names = variable_class_groups %>%
    purrr::map_chr(~ .x %>% dplyr::pull(variable_class) %>% unique())
  variable_classes = variable_class_groups %>%
    purrr::map(~ .x %>% dplyr::pull(calculated_variable)) %>%
    stats::setNames(variable_class_names)

  ## derived sum variables - calculate MOEs that can be used in subsequent calculations
  ## for example: age_10_14_years
  df_cvs1 = .df %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::any_of(variable_classes$sum),
        .fns = function(x) {
          current_column = dplyr::cur_column()

          numerator_estimate_variables = codebook1 %>%
            dplyr::filter(calculated_variable == current_column) %>%
            dplyr::pull(numerator) %>%
            stringr::str_remove_all("_count_estimate") %>%
            stringr::str_split(", ") %>%
            unlist()

          numerator_moe_variables = numerator_estimate_variables %>%
            paste0("_M")

          #AS: flagging that you're calculating SEs not MOEs here for clarity
          # - in line 327 below, I believe you're dividing the SEs you
          # - just calculated here by 1.645, which is incorrect
          moe = se_sum(
            purrr::map(numerator_moe_variables, ~ .df %>% dplyr::pull(.x)),
            purrr::map(numerator_estimate_variables, ~ .df %>% dplyr::pull(.x))) * 1.645

          return(moe) },
        .names = "{.col}_M"),

      ## count variables
      ## raw ACS variables
      dplyr::across(
        .cols = dplyr::any_of(cv_variables),
        .fns = function(x) {

          current_column = dplyr::cur_column()
          ## this effectively returns error calculations for the underlying variable
          ## under the assumption that the `1 - ` operation has no effect on the calculation
          ## of error
          if (current_column %in% variable_classes[["one minus percentage"]]) {
            current_column = codebook1 %>%
              dplyr::filter(calculated_variable == current_column) %>%
              dplyr::pull(definition) %>%
              stringr::str_remove_all("One minus |\\.") }

          ## variable metadata from the codebook
          codebook_variable = codebook1 %>%
            dplyr::filter(calculated_variable == current_column) %>%
            dplyr::mutate(
              dplyr::across(
                .cols = c(numerator, denominator),
                .fns = ~ .x %>%
                  stringr::str_remove_all("_count_estimate") %>%
                  stringr::str_split(", ")))
          numerator_estimate_variables = codebook_variable %>%
            dplyr::pull(numerator) %>% unlist()
          numerator_moe_variables = numerator_estimate_variables %>%
            stringr::str_c("_M")
          denominator_estimate_variables = codebook_variable %>%
            dplyr::pull(denominator) %>% unlist()
          denominator_moe_variables = denominator_estimate_variables %>%
            stringr::str_c("_M")

          ## for variables where we already have an MOE, this is simple:
          #AS: As mentioned in the comment on line 284 above, I think you don't
          # - need to apply se_simple() to variable_classes$sum since those are
          # - already a SE. I think you could add another condition to the logic
          # - below that just pulls the column for variable_classes$sum
          if (current_column %in% c(variable_classes$raw, variable_classes$sum) %>% stringr::str_remove("_count_estimate")) {
            SE = se_simple(get(dplyr::cur_column() %>% paste0("_M"))) }

          ## for percent variables with a denominator that doesn't have an MOE
          ## these are directly assigned the CV of the numerator and then we back-calculate
          ## values for the SE and MOE from the CV

          #AS: It strikes me as more straightforward to just directly calculate the
          # - MOE of the proportion/percentage using the Census formula and setting 0
          # - for MOE[Y], calculating the SE from the MOE[P] and then calculating
          # - the CV using the SE of the proportion/percent
          # - Additionally, I don't think this calculation is correct because it
          # - doesn't account for the 1/Y term in the calculation of MOE[P]
          if (current_column %in% variable_classes[["no MOE denominator"]]) {
            estimate_numerator = purrr::map(numerator_estimate_variables, ~ .df %>% dplyr::pull(.x))
            estimate_moe = purrr::map(numerator_moe_variables, ~ .df %>% dplyr::pull(.x))
            ## in the case that there are multiple numerator variables, we need to
            ## calculate the pooled SE of the numerator -- otherwise we just directly
            ## calculate the SE
            if (length(numerator_estimate_variables) > 1) {
              se_numerator = se_sum(
                estimate_moe,
                estimate_numerator) } else {
              se_numerator = se_simple(estimate_moe %>% unlist) }

            ## this is a CV -- we rename these accordingly in a following step
            ## this is consolidated here for brevity
            SE = cv(
              estimate = purrr::pmap_dbl(estimate_numerator, sum),
              se = se_numerator) }

          ## for simple percent variables with one numerator, one denominator:
          if (current_column %in% variable_classes[["simple percent, no calculated variables"]]) {
            SE = se_proportion_ratio(
              estimate_numerator = get(numerator_estimate_variables),
              estimate_denominator = get(denominator_estimate_variables),
              moe_numerator = get(numerator_moe_variables),
              moe_denominator = get(denominator_moe_variables)) }

          ## for percents with summed/subtracted numerators, one denominator
          if (current_column %in% variable_classes[["numerator sum percent"]]) {
            SE = se_proportion_ratio(
              estimate_numerator = rowSums(dplyr::select(., dplyr::all_of(numerator_estimate_variables))),
              estimate_denominator = get(denominator_estimate_variables),
              se_numerator = se_sum(
                purrr::map(numerator_moe_variables, ~ .df %>% dplyr::pull(.x)),
                purrr::map(numerator_estimate_variables, ~ .df %>% dplyr::pull(.x))),
              se_denominator = se_simple(
                purrr::map(denominator_moe_variables, ~ .df %>% dplyr::pull(.x)) %>% unlist())) }

          ## for percents with one numerator, summed/subtracted denominators
          if (current_column %in% variable_classes[["denominator sum percent"]]) {
            SE = se_proportion_ratio(
              estimate_numerator = get(numerator_estimate_variables),
              estimate_denominator = rowSums(dplyr::select(., dplyr::all_of(denominator_estimate_variables))),
              se_numerator = se_simple(
                purrr::map(numerator_moe_variables, ~ .df %>% dplyr::pull(.x)) %>% unlist()),
              se_denominator = se_sum(
                purrr::map(denominator_moe_variables, ~ .df %>% dplyr::pull(.x)),
                purrr::map(denominator_estimate_variables, ~ .df %>% dplyr::pull(.x)))) }

          ## for percents with summed numerators and summed denominators
          if (current_column %in% variable_classes[["numerator and denominator sum percent"]]) {
            SE = se_proportion_ratio(
              estimate_numerator = rowSums(dplyr::select(., dplyr::all_of(numerator_estimate_variables))),
              estimate_denominator = rowSums(dplyr::select(., dplyr::all_of(denominator_estimate_variables))),
              se_numerator = se_sum(
                purrr::map(numerator_moe_variables, ~ .df %>% dplyr::pull(.x)),
                purrr::map(numerator_estimate_variables, ~ .df %>% dplyr::pull(.x))),
              se_denominator = se_sum(
                purrr::map(denominator_moe_variables, ~ .df %>% dplyr::pull(.x)),
                purrr::map(denominator_estimate_variables, ~ .df %>% dplyr::pull(.x)))) }

          return(SE) },
        .names = "{.col}_SE")) %>%
    ## these variables are initially (above) named as if they're SEs, but in reality
    ## they're CVs, so we rename accordingly
    dplyr::rename_with(
      .cols = variable_classes[["no MOE denominator"]] %>% stringr::str_c("_SE"),
      .fn = ~ .x %>% stringr::str_replace("_SE$", "_CV")) %>%
    dplyr::mutate(
      ## create coefficients of variation from standard errors
      dplyr::across(
        .cols = dplyr::matches("_SE$"),
        .fns = ~ cv(
          estimate = get(dplyr::cur_column() %>% stringr::str_remove("_SE$")),
          se = .x),
        .names = "{.col %>% stringr::str_remove('_SE')}_CV"))

  moe_variables = df_cvs1 %>%
    dplyr::select(dplyr::matches("_M$")) %>%
    colnames() %>%
    stringr::str_remove("_M$")
  se_variables = df_cvs1 %>%
    dplyr::select(dplyr::matches("_SE$")) %>%
    colnames() %>%
    stringr::str_remove("_SE$")

  #AS: I think a comment here would be helpful to explain the conditions
  # - in which we'd expect a variable to have a SE but not MOE
  df_cvs = df_cvs1 %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(se_variables[!se_variables %in% moe_variables] %>% stringr::str_c("_SE")),
        .fns = ~ .x * 1.645,
        .names = "{.col %>% stringr::str_remove('_SE$')}_M"))

  # moe_variables = df_cvs %>% select(matches("_M$")) %>% colnames() %>% str_remove("_M$")
  # se_variables = df_cvs %>% select(matches("_SE$")) %>% colnames() %>% str_remove("_SE$")

  # # ## should be none
  # se_variables[!se_variables %in% moe_variables]
  # # ## should be three--the controlled variables that don't have margins of error
  # moe_variables[!moe_variables %in% se_variables]

  return(df_cvs)
}

utils::globalVariables(c(
  "calculated_variable", "numerator_variable_count", "denominator_variable_count",
  "no_moe_flag", "calculated_variable_dependency_flag", "numerator", "denominator",
  "observation", "type", "estimate", "moe", "variable_class"))
