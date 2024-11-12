run_aggregate_alignment_metric <- function(config) {
  config <- load_config(config)

  dir_prepared_abcd <- get_dir_prepared_abcd(config)
  dir_prio_diagnostics <- get_dir_prioritized_loanbooks_and_diagnostics(config)
  dir_analysis <- get_dir_analysis(config)
  dir_analysis_aggregated <- file.path(dir_analysis, "aggregated")

  dir.create(dir_analysis_aggregated, recursive = TRUE)

  path_scenario_tms <- get_path_scenario_tms(config)
  path_scenario_sda <- get_path_scenario_sda(config)

  scenario_source_input <- get_scenario_source(config)
  scenario_select <- get_scenario_select(config)
  region_select <- get_region_select(config)
  start_year <- get_start_year(config)
  time_frame <- get_time_frame(config)

  by_group <- get_by_group(config)
  by_group <- check_and_prepare_by_group(by_group)

  # load input data----
  region_isos_select <- r2dii.data::region_isos %>%
    dplyr::filter(
      .data[["source"]] == .env[["scenario_source_input"]],
      .data[["region"]] %in% .env[["region_select"]]
    )

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
    file.path(dir_prepared_abcd, "abcd_final.csv"),
    col_types = col_types_abcd_final,
    col_select = dplyr::all_of(cols_abcd)
  )

  # read matched and prioritized loan book----
  list_matched_prioritized <- list.files(path = dir_prio_diagnostics, pattern = "^matched_prio_.*csv$")
  stop_if_no_files_found(list_matched_prioritized, dir_prio_diagnostics, "dir_prio_diagnostics", "matched prioritized loan book CSVs")

  matched_prioritized <- readr::read_csv(
    file = file.path(dir_prio_diagnostics, list_matched_prioritized),
    col_types = col_types_matched_prioritized,
    col_select = dplyr::all_of(c(by_group, col_select_matched_prioritized))
  )

  # aggregate P4B alignment----

  ## set specifications----

  # for the calculation of the aggregate company alignment metric, we do not force companies
  # to enter a new market to build out hydro power or nuclear power, as this may
  # not be feasible for political and/or geographic reasons.
  # in the power sector, only renewables continues to follow the SMSP logic
  increasing_or_decreasing_aggregate_alignment <- r2dii.data::increasing_or_decreasing %>%
    dplyr::mutate(
      increasing_or_decreasing = dplyr::if_else(
        .data[["technology"]] %in% c("hydrocap", "nuclearcap"),
        "decreasing",
        .data[["increasing_or_decreasing"]]
      )
    )

  # define if technologies should be treated as build out or phase down in the
  # aggregation
  technology_direction <- scenario_input_tms %>%
    dplyr::filter(.data[["year"]] %in% c(.env[["start_year"]], .env[["start_year"]] + .env[["time_frame"]])) %>%
    dplyr::distinct(
      .data[["scenario_source"]],
      .data[["scenario"]],
      .data[["sector"]],
      .data[["technology"]],
      .data[["region"]]
    ) %>%
    dplyr::inner_join(r2dii.data::increasing_or_decreasing, by = c("sector", "technology")) %>%
    dplyr::mutate(
      directional_dummy = dplyr::if_else(
        .data[["increasing_or_decreasing"]] == "increasing",
        1,
        -1
      )
    ) %>%
    dplyr::select(-"increasing_or_decreasing")

  # remove non standard columns from matched_prioritzed when calling r2dii.analysis
  # TODO: check if this needs to be adjusted to remove other by_group columns
  matched_prio_non_standard_cols <- names(matched_prioritized)[!names(matched_prioritized) %in% col_standard_matched_prioritized]

  # only calculate net aggregated aligment results if the selected scenario has a trajectory for at least one sector in the matched_prioritzed loan book
  sectors_in_matched_prioritized <- matched_prioritized %>%
    dplyr::pull(.data[["sector"]]) %>%
    unique()

  sectors_in_scenario_tms <- scenario_input_tms %>%
    dplyr::filter(.data[["scenario_source"]] == .env[["scenario_source_input"]]) %>%
    dplyr::pull(.data[["sector"]]) %>%
    unique()

  sectors_in_scenario_sda <- scenario_input_sda %>%
    dplyr::filter(.data[["scenario_source"]] == .env[["scenario_source_input"]]) %>%
    dplyr::pull(.data[["sector"]]) %>%
    unique()

  ## prepare TMS company level P4B results for aggregation----
  if (
    length(dplyr::intersect(sectors_in_scenario_tms, sectors_in_matched_prioritized)) > 0
  ) {
  tms_result_for_aggregation <- r2dii.analysis::target_market_share(
    data = matched_prioritized %>%
      dplyr::select(-dplyr::all_of(matched_prio_non_standard_cols)),
    abcd = abcd,
    scenario = scenario_input_tms,
    region_isos = region_isos_select,
    by_company = TRUE,
    weight_production = FALSE,
    increasing_or_decreasing = increasing_or_decreasing_aggregate_alignment
  )

  ## aggregate TMS P4B results to company level alignment metric----
  # calculate aggregation for the loan book

  company_technology_deviation_tms <- tms_result_for_aggregation %>%
    calculate_company_tech_deviation(
      technology_direction = technology_direction,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      # bridge_tech = "gascap",
      time_frame = time_frame
    )

  company_alignment_net_tms <- company_technology_deviation_tms %>%
    calculate_company_aggregate_alignment_tms(
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      level = "net"
    )

  company_alignment_bo_po_tms <- company_technology_deviation_tms %>%
    calculate_company_aggregate_alignment_tms(
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      level = "bo_po"
    )

  company_technology_deviation_tms %>%
    readr::write_csv(
      file.path(dir_analysis_aggregated, "company_technology_deviation_tms.csv"),
      na = ""
    )

  company_alignment_net_tms %>%
    readr::write_csv(
      file.path(dir_analysis_aggregated, "company_alignment_net_tms.csv"),
      na = ""
    )

  company_alignment_bo_po_tms %>%
    readr::write_csv(
      file.path(dir_analysis_aggregated, "company_alignment_bo_po_tms.csv"),
      na = ""
    )
  } else {
    message(
      paste0(
        "None of the sectors available for target market share analysis in the ",
        scenario_select,
        " scenario of ",
        scenario_source_input,
        " are present in the matched and prioritized loan book. Calculation of aggregated TMS results not possible. Skipping!"
      )
    )

    company_technology_deviation_tms <- NULL
    company_alignment_net_tms <- NULL
    company_alignment_bo_po_tms <- NULL
  }

  ## prepare SDA company level P4B results for aggregation----
  if (
    length(dplyr::intersect(sectors_in_scenario_sda, sectors_in_matched_prioritized)) > 0
  ) {
    sda_result_for_aggregation <- r2dii.analysis::target_sda(
      data = matched_prioritized %>%
        dplyr::select(-dplyr::all_of(matched_prio_non_standard_cols)),
      abcd = abcd,
      co2_intensity_scenario = scenario_input_sda,
      by_company = TRUE,
      region_isos = region_isos_select
    )

    sda_result_for_aggregation <- sda_result_for_aggregation %>%
      dplyr::filter(.data[["year"]] >= .env[["start_year"]])

    ## aggregate SDA P4B results to company level alignment metric----
    company_alignment_net_sda <- sda_result_for_aggregation %>%
      calculate_company_aggregate_alignment_sda(
        scenario_source = scenario_source_input,
        scenario = scenario_select,
        time_frame = time_frame
      )

    company_alignment_net_sda %>%
      readr::write_csv(
        file.path(dir_analysis_aggregated, "company_alignment_net_sda.csv"),
        na = ""
      )
  } else {
    message(
      paste0(
        "None of the sectors available for SDA analysis in the ",
        scenario_select,
        " scenario of ",
        scenario_source_input,
        " are present in the matched and prioritized loan book. Calculation of aggregated SDA results not possible. Skipping!"
      )
    )

    company_alignment_net_sda <- NULL
  }


  ## calculate sector and loan book level aggregate alignment based on company exposures in loan book----

  # the company level aggregate alignment metrics are then joined with the matched
  # loan book to derive some high level summary statistics on the loan book level
  company_alignment_net <- dplyr::bind_rows(company_alignment_net_tms, company_alignment_net_sda)

  # show exposures (n companies and loan size) by alignment with given scenario
  write_alignment_metric_to_csv <- function(data,
                                            output_dir,
                                            level,
                                            .by = NULL) {
    if (is.null(.by)) {
      by_group <- ""
    } else {
      by_group <- glue::glue("_by_{paste(.by, collapse = \"_\")}")
    }

    data[["company"]] %>%
      readr::write_csv(
        file = file.path(output_dir, glue::glue("company_exposure_{level}_aggregate_alignment{by_group}.csv")),
        na = ""
      )

    data[["aggregate"]] %>%
      readr::write_csv(
        file = file.path(output_dir, glue::glue("loanbook_exposure_{level}_aggregate_alignment{by_group}.csv")),
        na = ""
      )

  }

  # net
  if (!is.null(company_alignment_net)) {
    if (nrow(company_alignment_net) > 0) {
      aggregated_alignment_net <- company_alignment_net %>%
        aggregate_alignment_loanbook_exposure(
          matched = matched_prioritized,
          level = "net",
          .by = by_group
        )

      write_alignment_metric_to_csv(
        data = aggregated_alignment_net,
        output_dir = dir_analysis_aggregated,
        level = "net",
        .by = by_group
      )
    }
  }

  # buildout / phaseout
  if (!is.null(company_alignment_bo_po_tms)) {
    if (nrow(company_alignment_bo_po_tms) > 0) {
      aggregated_alignment_bo_po <- company_alignment_bo_po_tms %>%
        aggregate_alignment_loanbook_exposure(
          matched = matched_prioritized,
          level = "bo_po",
          .by = by_group
        )

      write_alignment_metric_to_csv(
        data = aggregated_alignment_bo_po,
        output_dir = dir_analysis_aggregated,
        level = "bo_po",
        .by = by_group
      )
    }
  }

}
