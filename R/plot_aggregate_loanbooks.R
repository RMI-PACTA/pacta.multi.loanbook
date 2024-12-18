#' plot_aggregate_loanbooks
#'
#' @param config either a single string defining the path to a config YML file
#'   or a list object that contains the appropriate config params
#'
#' @return `NULL` (called for side effects)
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data .env
#'
#' @noRd

plot_aggregate_loanbooks <- function(config) {
  config <- load_config(config)

  # paths
  analysis_dir <- get_dir_analysis(config)
  analysis_aggregated_dir <- file.path(analysis_dir, "aggregated")

  # project parameters
  scenario_source_input <- get_scenario_source(config)
  scenario_select <- get_scenario_select(config)
  region_select <- get_region_select(config)
  start_year <- get_start_year(config)
  time_frame_select <- get_time_frame(config)

  by_group <- get_by_group(config)
  by_group <- check_and_prepare_by_group(by_group)

  # validate config values ----

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

  # load required data----

  if (is.null(by_group)) {
    file_by_group <- ""
  } else {
    file_by_group <- glue::glue("_by_{paste(by_group, collapse = \"_\")}")
  }

  ## company level results----
  # expected columns company_aggregated_alignment_* files
  col_types_company_aggregated_alignment <- readr::cols(
    name_abcd = "c",
    sector = "c",
    activity_unit = "c",
    region = "c",
    scenario_source = "c",
    scenario = "c",
    year = "i",
    direction = "c",
    total_deviation = "n",
    alignment_metric = "n",
    loan_size_outstanding_currency = "c",
    loan_size_outstanding = "n",
    exposure_weight = "n",
    .default = "c"
  )
  col_select_company_aggregated_alignment <- c(by_group, names(col_types_company_aggregated_alignment[["cols"]]))

  path_company_exposure_net_aggregate_alignment <- file.path(analysis_aggregated_dir, glue::glue("company_exposure_net_aggregate_alignment{file_by_group}.csv"))
  if (file.exists(path_company_exposure_net_aggregate_alignment)) {
    company_aggregated_alignment_net <-
      readr::read_csv(
        file = path_company_exposure_net_aggregate_alignment,
        col_types = col_types_company_aggregated_alignment,
        col_select = dplyr::all_of(col_select_company_aggregated_alignment)
      )
  } else {
    company_aggregated_alignment_net <- NULL
  }

  patch_company_exposure_bo_po_aggregate_alignment <- file.path(analysis_aggregated_dir, glue::glue("company_exposure_bo_po_aggregate_alignment{file_by_group}.csv"))
  if (file.exists(patch_company_exposure_bo_po_aggregate_alignment)) {
    company_aggregated_alignment_bo_po <-
      readr::read_csv(
        file = patch_company_exposure_bo_po_aggregate_alignment,
        col_types = col_types_company_aggregated_alignment,
        col_select = dplyr::all_of(col_select_company_aggregated_alignment)
      )
  } else {
    company_aggregated_alignment_bo_po <- NULL
  }

  ## loanbook level results----
  path_loanbook_exposure_bo_po_aggregate_alignment <- file.path(analysis_aggregated_dir, glue::glue("loanbook_exposure_bo_po_aggregate_alignment{file_by_group}.csv"))
  if (file.exists(path_loanbook_exposure_bo_po_aggregate_alignment)) {
    loanbook_exposure_aggregated_alignment_bo_po <-
      readr::read_csv(
        file = path_loanbook_exposure_bo_po_aggregate_alignment,
        col_types = readr::cols(
          scenario = "c",
          region = "c",
          sector = "c",
          year = "i",
          direction = "c",
          n_companies = "i",
          n_companies_aligned = "i",
          share_companies_aligned = "n",
          exposure_weighted_net_alignment = "n",
          .default = "c"
        )
      )
  } else {
    loanbook_exposure_aggregated_alignment_bo_po <- NULL
  }

  path_loanbook_exposure_net_aggregate_alignment <- file.path(analysis_aggregated_dir, glue::glue("loanbook_exposure_net_aggregate_alignment{file_by_group}.csv"))
  if (file.exists(path_loanbook_exposure_net_aggregate_alignment)) {
    loanbook_exposure_aggregated_alignment_net <-
      readr::read_csv(
        file = path_loanbook_exposure_net_aggregate_alignment,
        col_types = readr::cols(
          scenario = "c",
          region = "c",
          sector = "c",
          year = "i",
          direction = "c",
          n_companies = "i",
          n_companies_aligned = "i",
          share_companies_aligned = "n",
          exposure_weighted_net_alignment = "n",
          sum_loan_size_outstanding = "n",
          sum_exposure_companies_aligned = "n",
          share_exposure_aligned = "n",
          .default = "c"
        )
      )
  } else {
    loanbook_exposure_aggregated_alignment_net <- NULL
  }

  # generate plots for system-wide analysis----
  ### sankey plot----
  # Plot sankey plot of financial flows scenario alignment - examples
  if (!is.null(company_aggregated_alignment_net)) {
    data_sankey_sector <- prep_sankey(
      company_aggregated_alignment_net,
      region = "global",
      year = start_year + time_frame_select,
      group_var = by_group,
      middle_node = "sector"
    )

    if (is.null(by_group)) {
      output_file_sankey_sector <- "sankey_sector"
    } else {
      output_file_sankey_sector <- glue::glue("sankey_sector_{by_group}")
    }

    data_sankey_sector %>%
      readr::write_csv(
        file = file.path(
          analysis_aggregated_dir,
          glue::glue("data_{output_file_sankey_sector}.csv")
        ),
        na = ""
      )

    p_sankey <- plot_sankey(
      data = data_sankey_sector,
      y_axis = "loan_size_outstanding",
      initial_node = by_group,
      middle_node = "sector"
    )

    ggplot2::ggsave(
      plot = p_sankey,
      filename = glue::glue("plot_{output_file_sankey_sector}.png"),
      path = analysis_aggregated_dir,
      width = 8,
      height = 5,
      dpi = 300,
      units = "in",
    )

  } else {
    cli::cli_warn("Sankey plot (by sector) cannot be generated. Skipping!")
  }

  ### scatter plot alignment by exposure and sector comparison----
  year_scatter_alignment_exposure <- start_year + time_frame_select
  region_scatter_alignment_exposure <- region_select
  currency <- unique(company_aggregated_alignment_net[["loan_size_outstanding_currency"]])

  if (!is.null(loanbook_exposure_aggregated_alignment_net)) {
    if (
      nrow(loanbook_exposure_aggregated_alignment_net) > 0
    ) {
      data_scatter_alignment_exposure <- loanbook_exposure_aggregated_alignment_net %>%
        prep_scatter_alignment_exposure(
          year = year_scatter_alignment_exposure,
          region = region_scatter_alignment_exposure,
          scenario = scenario_select,
          group_var = by_group,
          exclude_groups = "benchmark"
        )

      if (is.null(by_group)) {
        output_file_alignment_exposure <- "scatter_alignment_exposure"
      } else {
        output_file_alignment_exposure <- glue::glue("scatter_alignment_exposure_{by_group}")
      }

      data_scatter_alignment_exposure %>%
        readr::write_csv(
          file = file.path(
            analysis_aggregated_dir,
            glue::glue("data_{output_file_alignment_exposure}.csv")
          ),
          na = ""
        )

      plot_scatter_alignment_exposure <- data_scatter_alignment_exposure %>%
        plot_scatter_alignment_exposure(
          floor_outliers = -1,
          cap_outliers = 1,
          group_var = by_group,
          currency = currency
        )

      ggplot2::ggsave(
        filename = glue::glue("plot_{output_file_alignment_exposure}.png"),
        path = analysis_aggregated_dir,
        width = 8,
        height = 5,
        dpi = 300,
        units = "in",
      )
    }
  } else {
    cli::cli_warn("Scatter plot exposure by alignment cannot be generated. Skipping!")
  }

  ### scatter plot for group level comparison----
  year_scatter <- start_year + time_frame_select
  region_scatter <- region_select
  data_level_group <- "group_var"
  # automotive
  sector_scatter <- "automotive"
  if (
    !is.null(loanbook_exposure_aggregated_alignment_bo_po) &
      !is.null(loanbook_exposure_aggregated_alignment_net)
  ) {
    if (
      nrow(loanbook_exposure_aggregated_alignment_bo_po) > 0 &
        nrow(loanbook_exposure_aggregated_alignment_net) > 0
    ) {
      data_scatter_automotive_group <- prep_scatter(
        loanbook_exposure_aggregated_alignment_bo_po,
        loanbook_exposure_aggregated_alignment_net,
        year = year_scatter,
        sector = sector_scatter,
        region = region_scatter,
        group_var = by_group,
        data_level = data_level_group
      )

      if (is.null(by_group)) {
        output_file_scatter_sector <- glue::glue("scatter_{sector_scatter}")
      } else {
        output_file_scatter_sector <- glue::glue("scatter_{sector_scatter}_by_{by_group}")
      }

      data_scatter_automotive_group %>%
        readr::write_csv(
          file = file.path(
            analysis_aggregated_dir,
            glue::glue("data_{output_file_scatter_sector}.csv")
          ),
          na = ""
        )

      plot_scatter(
        data_scatter_automotive_group,
        data_level = data_level_group,
        year = year_scatter,
        sector = sector_scatter,
        region = region_scatter,
        scenario_source = scenario_source_input,
        scenario = scenario_select
      )
      ggplot2::ggsave(
        filename = glue::glue("plot_{output_file_scatter_sector}.png"),
        path = analysis_aggregated_dir,
        width = 8,
        height = 5
      )
    }
  } else {
    cli::cli_warn("Scatter plot BO/PO cannot be generated. Skipping!")
  }

  # power
  sector_scatter <- "power"
  if (
    !is.null(loanbook_exposure_aggregated_alignment_bo_po) &
      !is.null(loanbook_exposure_aggregated_alignment_net)
  ) {
    if (
      nrow(loanbook_exposure_aggregated_alignment_bo_po) > 0 &
        nrow(loanbook_exposure_aggregated_alignment_net) > 0
    ) {
      data_scatter_power_group <- prep_scatter(
        loanbook_exposure_aggregated_alignment_bo_po,
        loanbook_exposure_aggregated_alignment_net,
        year = year_scatter,
        sector = sector_scatter,
        region = region_scatter,
        group_var = by_group,
        data_level = data_level_group
      )

      if (is.null(by_group)) {
        output_file_scatter_sector <- glue::glue("scatter_{sector_scatter}")
      } else {
        output_file_scatter_sector <- glue::glue("scatter_{sector_scatter}_by_{by_group}")
      }

      data_scatter_power_group %>%
        readr::write_csv(
          file = file.path(
            analysis_aggregated_dir,
            glue::glue("data_{output_file_scatter_sector}.csv")
          ),
          na = ""
        )

      plot_scatter(
        data_scatter_power_group,
        data_level = data_level_group,
        year = year_scatter,
        sector = sector_scatter,
        region = region_scatter,
        scenario_source = scenario_source_input,
        scenario = scenario_select
      )

      ggplot2::ggsave(
        filename = glue::glue("plot_{output_file_scatter_sector}.png"),
        path = analysis_aggregated_dir,
        width = 8,
        height = 5
      )
    }
  } else {
    cli::cli_warn("Scatter plot BO/PO cannot be generated. Skipping!")
  }

  # group level plots ----
  # create sub directories for each relevant group.

  if (
    length(by_group) == 1 &
      !is.null(loanbook_exposure_aggregated_alignment_net)
  ) {
    dirs_for_by_group <- loanbook_exposure_aggregated_alignment_net %>%
      dplyr::filter(
        !grepl("benchmark_corporate_economy_", !!rlang::sym(by_group))
      ) %>%
      dplyr::pull(!!rlang::sym(by_group)) %>%
      unique()

    for (i in dirs_for_by_group) {
      dir.create(file.path(analysis_aggregated_dir, i), recursive = TRUE, showWarnings = FALSE)
    }
  }

  ### scatter plot for company level comparison----

  # all excluding outliers
  # for all companies per group, not all companies across groups

  # company level, excluding outliers
  year_scatter <- start_year + time_frame_select
  region_scatter <- region_select
  data_level_company <- "company"

  # automotive
  sector_scatter <- "automotive"

  if (
    length(by_group) == 1 &
      !is.null(company_aggregated_alignment_bo_po)
  ) {
    unique_by_group <- company_aggregated_alignment_bo_po %>%
      dplyr::filter(
        .data[["sector"]] == .env[["sector_scatter"]],
        !grepl("benchmark_corporate_economy_", !!rlang::sym(by_group))
      ) %>%
      dplyr::pull(!!rlang::sym(by_group)) %>%
      unique()

    for (i in unique_by_group) {
      data_scatter_automotive_company_i <- prep_scatter(
        company_aggregated_alignment_bo_po,
        company_aggregated_alignment_net,
        year = year_scatter,
        sector = sector_scatter,
        region = region_scatter,
        group_var = by_group,
        groups_to_plot = i,
        data_level = data_level_company
      )

      if (nrow(data_scatter_automotive_company_i) > 0) {
        data_scatter_automotive_company_i %>%
          readr::write_csv(
            file = file.path(
              analysis_aggregated_dir,
              i,
              glue::glue("data_scatter_automotive_company_by_{by_group}_{i}.csv")
            ),
            na = ""
          )

        plot_scatter(
          data_scatter_automotive_company_i,
          data_level = data_level_company,
          year = year_scatter,
          sector = sector_scatter,
          region = region_scatter,
          scenario_source = scenario_source_input,
          scenario = scenario_select,
          cap_outliers = 2,
          floor_outliers = -2
        )

        ggplot2::ggsave(
          filename = glue::glue("plot_scatter_automotive_company_by_{by_group}_{i}.png"),
          path = file.path(analysis_aggregated_dir, i),
          width = 8,
          height = 5
        )
      } else {
        next()
      }
    }
  } else {
    cli::cli_warn("Scatter plot BO/PO cannot be generated at company level. Skipping!")
  }

  # power
  sector_scatter <- "power"

  if (
    length(by_group) == 1 &
      !is.null(company_aggregated_alignment_bo_po)
  ) {
    unique_by_group <- company_aggregated_alignment_bo_po %>%
      dplyr::filter(
        .data[["sector"]] == .env[["sector_scatter"]],
        !grepl("benchmark_corporate_economy_", !!rlang::sym(by_group))
      ) %>%
      dplyr::pull(!!rlang::sym(by_group)) %>%
      unique()

    for (i in unique_by_group) {
      data_scatter_power_company_i <- prep_scatter(
        company_aggregated_alignment_bo_po,
        company_aggregated_alignment_net,
        year = year_scatter,
        sector = sector_scatter,
        region = region_scatter,
        group_var = by_group,
        groups_to_plot = i,
        data_level = data_level_company
      )

      if (nrow(data_scatter_power_company_i) > 0) {
        data_scatter_power_company_i %>%
          readr::write_csv(
            file = file.path(
              analysis_aggregated_dir,
              i,
              glue::glue("data_scatter_power_company_by_{by_group}_{i}.csv")
            ),
            na = ""
          )

        plot_scatter(
          data_scatter_power_company_i,
          data_level = data_level_company,
          year = year_scatter,
          sector = sector_scatter,
          region = region_scatter,
          scenario_source = scenario_source_input,
          scenario = scenario_select,
          cap_outliers = 2,
          floor_outliers = -2
        )

        ggplot2::ggsave(
          filename = glue::glue("plot_scatter_power_company_by_{by_group}_{i}.png"),
          path = file.path(analysis_aggregated_dir, i),
          width = 8,
          height = 5
        )
      } else {
        next()
      }
    }
  } else {
    cli::cli_warn("Scatter plot BO/PO cannot be generated at company level. Skipping!")
  }
}
