#' run_pacta
#'
#' @param config either a single string defining the path to a config YML file
#'   or a list object that contains the appropriate config params
#'
#' @return `NULL` (called for side effects)
#'
#' @importFrom rlang :=
#'
#' @noRd

run_pacta <- function(config) {
  config <- load_config(config)

  dir_prepared_abcd <- get_dir_prepared_abcd(config)
  dir_prioritized_loanbooks_and_diagnostics <- get_dir_prioritized_loanbooks_and_diagnostics(config)

  path_scenario_tms <- get_path_scenario_tms(config)
  path_scenario_sda <- get_path_scenario_sda(config)

  path_abcd <- file.path(dir_prepared_abcd, "abcd_final.csv")

  dir_analysis <- get_dir_analysis(config)
  dir_analysis_standard <- file.path(dir_analysis, "standard")

  dir.create(dir_analysis_standard, recursive = TRUE, showWarnings = FALSE)

  scenario_source_input <- get_scenario_source(config)
  scenario_select <- get_scenario_select(config)
  region_select <- get_region_select(config)
  start_year <- get_start_year(config)
  time_frame_select <- get_time_frame(config)

  by_group <- get_by_group(config)
  by_group <- check_and_prepare_by_group(by_group)
  by_group_ext <- if (is.null(by_group)) { "_meta" } else { paste0("_", by_group) }

  # validate config values ----

  assert_length(dir_prepared_abcd, 1L)
  assert_inherits(dir_prepared_abcd, "character")
  assert_dir_exists(dir_prepared_abcd, desc = "Output - prepare ABCD")
  assert_file_exists(file.path(dir_prepared_abcd, "abcd_final.csv"), desc = "ABCD final")

  assert_length(path_scenario_tms, 1L)
  assert_inherits(path_scenario_tms, "character")
  assert_file_exists(path_scenario_tms, desc = "Input - Scenario: target market share (TMS)")

  assert_length(path_scenario_sda, 1L)
  assert_inherits(path_scenario_sda, "character")
  assert_file_exists(path_scenario_sda, desc = "Input - Scenario: sectoral decarbonization approach (SDA)")

  assert_length(scenario_source_input, 1L)
  assert_inherits(scenario_source_input, "character")

  assert_length(scenario_select, 1L)
  assert_inherits(scenario_select, "character")

  assert_length(region_select, 1L)
  assert_inherits(region_select, "character")

  assert_length(start_year, 1L)
  assert_inherits(start_year, "integer")

  assert_length(time_frame_select, 1L)
  assert_inherits(time_frame_select, "integer")

  # load input data----
  # if demo data is expected, use the region_isos_demo data set from r2dii.data
  if (scenario_source_input %in% unique(r2dii.data::region_isos_demo$source)) {
    region_isos_select <- r2dii.data::region_isos_demo %>%
      dplyr::filter(
        .data[["source"]] == .env[["scenario_source_input"]],
        .data[["region"]] %in% .env[["region_select"]]
      )
  } else {
    region_isos_select <- r2dii.data::region_isos %>%
      dplyr::filter(
        .data[["source"]] == .env[["scenario_source_input"]],
        .data[["region"]] %in% .env[["region_select"]]
      )
  }

  scenario_input_tms <- readr::read_csv(
    path_scenario_tms,
    col_types = col_types_scenario_tms,
    col_select = dplyr::all_of(col_select_scenario_tms)
  )

  scenario_input_sda <- readr::read_csv(
    path_scenario_sda,
    col_types = col_types_scenario_sda,
    col_select = dplyr::all_of(col_select_scenario_sda)
  )

  abcd <- readr::read_csv(
    path_abcd,
    col_select = dplyr::all_of(cols_abcd),
    col_types = col_types_abcd_final
  )

  # validate input data ----
  validate_input_run_pacta(
    scenario_data_tms = scenario_input_tms,
    scenario_data_sda = scenario_input_sda,
    start_year = start_year
  )

  # read matched and prioritized loan book----
  list_matched_prioritized <- list.files(path = dir_prioritized_loanbooks_and_diagnostics, pattern = "^matched_prio_.*csv$")
  assert_any_file_exists(list_matched_prioritized, dir_prioritized_loanbooks_and_diagnostics, "dir_prioritized_loanbooks_and_diagnostics", "matched prioritized loan book CSVs")

  matched_prioritized <- readr::read_csv(
    file = file.path(dir_prioritized_loanbooks_and_diagnostics, list_matched_prioritized),
    col_types = col_types_matched_prioritized,
    col_select = dplyr::all_of(c(by_group, col_select_matched_prioritized))
  )

  # add helper column to facilitate calculation of meta results----
  if (is.null(by_group)) {
    by_group <- "meta"
    matched_prioritized <- matched_prioritized %>%
      dplyr::mutate(meta = "meta")
  }

  # remove non standard columns from matched_prioritzed when calling r2dii.analysis
  matched_prio_non_standard_cols <- names(matched_prioritized)[!names(matched_prioritized) %in% col_standard_matched_prioritized]

  # generate all P4B outputs----
  unique_loanbook_groups_matched <- unique(matched_prioritized[[by_group]])

  ## generate SDA outputs----
  results_sda_total <- NULL

  # generate SDA results for each individual loan book, including the meta loan book
  for (i in unique_loanbook_groups_matched) {
    matched_i <- matched_prioritized %>%
      dplyr::filter(.data[[by_group]] == i) %>%
      dplyr::select(-dplyr::all_of(matched_prio_non_standard_cols))

    results_sda_i <- matched_i %>%
      r2dii.analysis::target_sda(
        abcd = abcd,
        co2_intensity_scenario = scenario_input_sda,
        region_isos = region_isos_select
      ) %>%
      dplyr::mutate("{by_group}" := .env[["i"]])

    results_sda_total <- results_sda_total %>%
      dplyr::bind_rows(results_sda_i)
  }

  # write SDA results to csv
  results_sda_total %>%
    readr::write_csv(
      file.path(dir_analysis_standard, paste0("sda_results", by_group_ext, ".csv")),
      na = ""
    )


  ## generate TMS outputs----

  results_tms_total <- NULL

  # generate TMS results for each individual loan book, including the meta loan book
  for (i in unique_loanbook_groups_matched) {
    matched_i <- matched_prioritized %>%
      dplyr::filter(.data[[by_group]] == i) %>%
      dplyr::select(-dplyr::all_of(matched_prio_non_standard_cols))

    results_tms_i <- matched_i %>%
      r2dii.analysis::target_market_share(
        abcd = abcd,
        scenario = scenario_input_tms,
        region_isos = region_isos_select
      ) %>%
      dplyr::mutate("{by_group}" := .env[["i"]])

    results_tms_total <- results_tms_total %>%
      dplyr::bind_rows(results_tms_i)
  }

  # write TMS results to csv
  results_tms_total %>%
    readr::write_csv(
      file.path(dir_analysis_standard, paste0("tms_results", by_group_ext, ".csv")),
      na = ""
    )

  # generate P4B plots----

  ## retrieve set of unique groups to loop over----
  unique_groups_tms <- unique(results_tms_total[[by_group]])
  unique_groups_sda <- unique(results_sda_total[[by_group]])


  ## run automatic result generation ----------

  generate_individual_outputs_in_groups <-
    function(sector_select, target_type) {
      if (target_type == "tms") {
        results <- results_tms_total
        unique_groups <- unique_groups_tms
        metric_col <- "metric"
      } else {
        results <- results_sda_total
        unique_groups <- unique_groups_sda
        metric_col <- "emission_factor_metric"
      }

      for (unique_group_i in unique_groups) {
        available_rows <-
          results %>%
          dplyr::filter(
            .data[[by_group]] == .env[["unique_group_i"]],
            .data[["scenario_source"]] == .env[["scenario_source_input"]],
            grepl(.env[["scenario_select"]], .data[[metric_col]]),
            .data[["region"]] == .env[["region_select"]],
            .data[["sector"]] == .env[["sector_select"]]
          ) %>%
          nrow()

        if (available_rows > 0) {
          generate_individual_outputs(
            data = results,
            matched_prioritized = matched_prioritized,
            output_directory = dir_analysis_standard,
            target_type = target_type,
            by_group = by_group,
            by_group_value = unique_group_i,
            scenario_source = scenario_source_input,
            scenario = scenario_select,
            region = region_select,
            sector = sector_select,
            start_year = start_year,
            time_horizon = time_frame_select
          )
        }
      }
    }

  generate_individual_outputs_in_groups("automotive", "tms")
  generate_individual_outputs_in_groups("coal", "tms")
  generate_individual_outputs_in_groups("oil and gas", "tms")
  generate_individual_outputs_in_groups("power", "tms")
  generate_individual_outputs_in_groups("aviation", "sda")
  generate_individual_outputs_in_groups("cement", "sda")
  generate_individual_outputs_in_groups("steel", "sda")
}

validate_input_run_pacta <- function(scenario_data_tms,
                                     scenario_data_sda,
                                     start_year) {
  # consistency check
  if (!min(scenario_data_tms$year) == start_year) {
    cli::cli_abort(
      message = c(
        x = "required {.arg start_year} for running pacta is not the initial year found in {.arg scenario_data_tms}",
        i = "You provided: {.arg start_year} = {start_year}",
        i = "Initial year in {.arg scenario_data_tms} is: {min(scenario_data_tms$year)}",
        i = "Please ensure that your input data sets and parameter settings are consistent."
      )
    )
  }

  if (!min(scenario_data_sda$year) == start_year) {
    cli::cli_abort(
      message = c(
        x = "required {.arg start_year} for running pacta is not the initial year found in {.arg scenario_data_sda}",
        i = "You provided: {.arg start_year} = {start_year}",
        i = "Initial year in {.arg scenario_data_sda} is: {min(scenario_data_sda$year)}",
        i = "Please ensure that your input data sets and parameter settings are consistent."
      )
    )
  }

  invisible()
}

