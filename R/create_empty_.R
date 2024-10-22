create_empty_company_technology_deviation_tms <- function(scenario_select) {
  empty_company_technology_deviation_tms <- tibble::tibble(
    sector = character(),
    technology = character(),
    year = integer(),
    region = character(),
    scenario_source = character(),
    name_abcd = character(),
    projected = numeric(),
    !!paste0("target_", scenario_select) := numeric(),
    total_tech_deviation = numeric(),
    direction = character(),
    activity_unit = character()
  )
}

create_empty_company_alignment <- function() {
  empty_company_alignment <- tibble::tibble(
    name_abcd = character(),
    sector = character(),
    activity_unit = character(),
    region = character(),
    scenario_source = character(),
    scenario = character(),
    year = integer(),
    direction = character(),
    total_deviation = numeric(),
    alignment_metric = numeric()
  )
}

create_empty_aggregated_alignment <- function() {
  empty_aggregated_alignment <- list(
    company = tibble::tibble(
      name_abcd = character(),
      sector = character(),
      activity_unit = character(),
      region = character(),
      scenario_source = character(),
      scenario = character(),
      year = integer(),
      direction = character(),
      total_deviation = numeric(),
      alignment_metric = numeric(),
      loan_size_outstanding_currency = character(),
      loan_size_outstanding = numeric(),
      exposure_weight = numeric()
    ),
    aggregate = tibble::tibble(
      scenario = character(),
      region = character(),
      sector = character(),
      year = integer(),
      direction = character(),
      n_companies = integer(),
      n_companies_aligned = integer(),
      share_companies_aligned = numeric(),
      exposure_weighted_net_alignment = numeric(),
      sum_loan_size_outstanding = numeric(),
      sum_exposure_companies_aligned = numeric(),
      share_exposure_aligned = numeric()
    )
  )
}
