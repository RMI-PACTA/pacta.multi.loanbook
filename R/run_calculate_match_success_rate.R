run_calculate_match_success_rate <- function(config) {
  config <- load_config(config)

  # input/output paths for match success rate
  dir_input <- get_dir_input(config)
  dir_loanbooks <- get_dir_loanbooks(config)
  dir_prioritized_loanbooks_and_diagnostics <- get_dir_prioritized_loanbooks_and_diagnostics(config)

  matching_use_manual_sector_classification <- get_use_manual_sector_classification(config)
  if (matching_use_manual_sector_classification) {
    path_manual_sector_classification <- get_path_manual_sector_classification(config)
  }

  by_group <- get_by_group(config)
  by_group <- check_and_prepare_by_group(by_group)
  by_group_ext <- if (is.null(by_group)) { "_meta" } else { paste0("_", by_group) }

  match_success_rate_plot_width <- get_match_plot_width(config)
  match_success_rate_plot_height <- get_match_plot_height(config)
  match_success_rate_plot_units <- get_match_plot_units(config)
  match_success_rate_plot_resolution <- get_match_plot_resolution(config)

  # validate config values----
  assert_length(dir_loanbooks, 1L)
  assert_inherits(dir_loanbooks, "character")
  assert_dir_exists(dir_loanbooks, desc = "Input - loanbooks")

  assert_length(dir_prioritized_loanbooks_and_diagnostics, 1L)
  assert_inherits(dir_prioritized_loanbooks_and_diagnostics, "character")
  assert_dir_exists(dir_prioritized_loanbooks_and_diagnostics, desc = "Output - Prioritized loanbooks and diagnostics")

  assert_length(matching_use_manual_sector_classification, 1L)
  assert_inherits(matching_use_manual_sector_classification, "logical")

  # path to manual sector classification only required if boolean TRUE
  if (matching_use_manual_sector_classification) {
    assert_length(path_manual_sector_classification, 1L)
    assert_inherits(path_manual_sector_classification, "character")
    assert_file_exists(path_manual_sector_classification, desc = "Manual sector classification")
  }

  assert_length(match_success_rate_plot_width, 1L)
  assert_inherits(match_success_rate_plot_width, "integer")

  assert_length(match_success_rate_plot_height, 1L)
  assert_inherits(match_success_rate_plot_height, "integer")

  assert_length(match_success_rate_plot_units, 1L)
  assert_inherits(match_success_rate_plot_units, "character")

  assert_length(match_success_rate_plot_resolution, 1L)
  assert_inherits(match_success_rate_plot_resolution, "integer")

  # load data----

  ## load raw loan books----
  list_raw <- list.files(dir_loanbooks)[grepl("csv$", list.files(dir_loanbooks))]
  assert_any_file_exists(list_raw, dir_loanbooks, "dir_loanbooks", "raw loan book CSVs")

  if (is.null(by_group) || by_group != "group_id") {
    raw_lbk <- readr::read_csv(
      file = file.path(dir_loanbooks, list_raw),
      col_types = col_types_raw,
      col_select = dplyr::all_of(c(by_group, col_select_raw)),
      id = "group_id"
    )
  } else {
    raw_lbk <- readr::read_csv(
      file = file.path(dir_loanbooks, list_raw),
      col_types = col_types_raw,
      col_select = dplyr::all_of(c(col_select_raw)),
      id = "group_id"
    )
  }

  raw_lbk <- raw_lbk %>%
    dplyr::mutate(
      group_id = basename(.data[["group_id"]]),
      group_id = sub("[.]csv$", "", .data[["group_id"]])
    )

  ## load matched prioritized loan books----
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
    raw_lbk <- raw_lbk %>%
      dplyr::mutate(meta = "meta")
    matched_prioritized <- matched_prioritized %>%
      dplyr::mutate(meta = "meta")
  }

  ## load classification system----
  if (matching_use_manual_sector_classification) {
    sector_classification_system <- readr::read_csv(
      file = path_manual_sector_classification,
      col_types = col_types_sector_classification,
      col_select = dplyr::all_of(col_select_sector_classification)
    )
  } else {
    sector_classifications_used <- unique(raw_lbk[["sector_classification_system"]])

    if (length(sector_classifications_used) != 1) {
      cli::cli_abort(
        message = c(
          "x" = "Number of sector classification systems across all loan books is > 1.",
          "i" = "You can only use one sector classification at the same time.",
          "i" = "Your raw loan books use {length(sector_classifications_used)} different types of sector classifications."
        )
      )
    }

    sector_classification_system <- r2dii.data::sector_classifications %>%
      dplyr::filter(.data[["code_system"]] == .env[["sector_classifications_used"]])
  }

  ## remove misclassified loans----
  # optional: manually exclude loans from the match success calculation
  # this is intended to allow excluding loans that are misclassified as in scope,
  # but research shows that the company is not actually in scope
  if (file.exists(file.path(dir_input, "loans_to_remove.csv"))) {
    loans_to_remove <- readr::read_csv(
      file = file.path(dir_input, "loans_to_remove.csv"),
      col_types = readr::cols_only(
        id_loan = "c",
        group_id = "c"
      )
    )
  } else {
    loans_to_remove <- NULL
  }

  lbk_match_success_rate <- calculate_match_success_rate(
    raw_lbk = raw_lbk,
    matched_prioritized = matched_prioritized,
    sector_classification_system = sector_classification_system,
    misclassfied_loans = loans_to_remove,
    by_group = by_group
  )

  # write to csv
  lbk_match_success_rate %>%
    readr::write_csv(
      file = file.path(dir_prioritized_loanbooks_and_diagnostics, paste0("lbk_match_success_rate", by_group_ext, ".csv")),
      na = ""
    )

  # prepare match success data for plotting----
  data_lbk_match_success_rate <- prep_match_success_rate(
    data = lbk_match_success_rate,
    by_group = by_group
  )

  # plot match success rate----
  plot_match_success_currency <- unique(raw_lbk[["loan_size_outstanding_currency"]])

  ## plot relative match success rates for individual loan books----
  plot_match_success_rate_rel_n_ind <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      metric_type = "relative",
      match_success_type = "n",
      currency = plot_match_success_currency,
      by_group = by_group
    )

  ggplot2::ggsave(
    filename = file.path(dir_prioritized_loanbooks_and_diagnostics, paste0("plot_match_success_rate_rel_n", by_group_ext, ".png")),
    plot = plot_match_success_rate_rel_n_ind,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  plot_match_success_rate_rel_outstanding_ind <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      metric_type = "relative",
      match_success_type = "outstanding",
      currency = plot_match_success_currency,
      by_group = by_group
    )

  ggplot2::ggsave(
    filename = file.path(dir_prioritized_loanbooks_and_diagnostics, paste0("plot_match_success_rate_rel_outstanding", by_group_ext, ".png")),
    plot = plot_match_success_rate_rel_outstanding_ind,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  plot_match_success_rate_rel_credit_limit_ind <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      metric_type = "relative",
      match_success_type = "credit_limit",
      currency = plot_match_success_currency,
      by_group = by_group
    )

  ggplot2::ggsave(
    filename = file.path(dir_prioritized_loanbooks_and_diagnostics, paste0("plot_match_success_rate_rel_credit_limit", by_group_ext, ".png")),
    plot = plot_match_success_rate_rel_credit_limit_ind,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  ## plot absolute match success rates for individual loan books----
  plot_match_success_rate_abs_n_ind <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      metric_type = "absolute",
      match_success_type = "n",
      currency = plot_match_success_currency,
      by_group = by_group
    )

  ggplot2::ggsave(
    filename = file.path(dir_prioritized_loanbooks_and_diagnostics, paste0("plot_match_success_rate_abs_n", by_group_ext, ".png")),
    plot = plot_match_success_rate_abs_n_ind,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  plot_match_success_rate_abs_outstanding_ind <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      metric_type = "absolute",
      match_success_type = "outstanding",
      currency = plot_match_success_currency,
      by_group = by_group
    )

  ggplot2::ggsave(
    filename = file.path(dir_prioritized_loanbooks_and_diagnostics, paste0("plot_match_success_rate_abs_outstanding", by_group_ext, ".png")),
    plot = plot_match_success_rate_abs_outstanding_ind,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  plot_match_success_rate_abs_credit_limit_ind <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      metric_type = "absolute",
      match_success_type = "credit_limit",
      currency = plot_match_success_currency,
      by_group = by_group
    )

  ggplot2::ggsave(
    filename = file.path(dir_prioritized_loanbooks_and_diagnostics, paste0("plot_match_success_rate_abs_credit_limit", by_group_ext, ".png")),
    plot = plot_match_success_rate_abs_credit_limit_ind,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )
}

calculate_match_success_rate <- function(raw_lbk,
                                         matched_prioritized,
                                         sector_classification_system,
                                         misclassfied_loans = NULL,
                                         by_group) {
  # combine data----
  # add sectors to raw loan books
  raw_lbk_with_sectors <- add_sectors_to_raw_lbk(
    raw_lbk = raw_lbk,
    sector_classification_system = sector_classification_system
  )

  lbk_match_success <- combine_raw_and_matched_loan_books(
    raw_lbk_with_sectors = raw_lbk_with_sectors,
    matched_prioritized = matched_prioritized,
    by_group = by_group
  )

  ## remove misclassified loans----
  # optional: manually exclude loans from the match success calculation
  # this is intended to allow excluding loans that are misclassified as in scope,
  # but research shows that the company is not actually in scope
  if (!is.null(misclassfied_loans)) {
    lbk_match_success <- dplyr::anti_join(
      x = lbk_match_success,
      y = misclassfied_loans,
      by = c("id_loan", "group_id")
    )
  }

  # calculate match success rate----
  lbk_match_success_rate <- add_match_success_rate(
    lbk_match_success,
    by_group = by_group
  )
}

add_sectors_to_raw_lbk <- function(raw_lbk, sector_classification_system) {

  raw_lbk_with_sectors <- dplyr::left_join(
    x = raw_lbk,
    y = sector_classification_system,
    by = c(
      "sector_classification_system" = "code_system",
      "sector_classification_direct_loantaker" = "code"
    )
  )

  raw_lbk_with_sectors <- raw_lbk_with_sectors %>%
    dplyr::mutate(
      sector = dplyr::if_else(
        is.na(.data[["sector"]]),
        "not in scope",
        .data[["sector"]]
      ),
      borderline = dplyr::if_else(
        is.na(.data[["sector"]]),
        FALSE,
        .data[["borderline"]]
      )
    )
}

combine_raw_and_matched_loan_books <- function(raw_lbk_with_sectors,
                                               matched_prioritized,
                                               by_group) {
  # join raw and matched loan books, matching on all common columns, but using the
  # financial sector from the raw loan book to match the production sector.
  # this simulates matching with the option by_sector = TRUE
  all_sectors <- unique(r2dii.data::sector_classifications$sector)

  # recreate initial id_loan format for joining
  matched_prioritized <- matched_prioritized %>%
    dplyr::select(-"sector") %>%
    dplyr::mutate(
      id_loan = gsub(paste0("_", .data[["group_id"]], collapse = "|"), "", .data[["id_loan"]])
    ) %>%
    dplyr::mutate(
      id_loan = gsub(paste0("_", .env$all_sectors, collapse="|"), "", .data[["id_loan"]])
    )

  if (by_group != "group_id") {
    matched_prioritized <- matched_prioritized %>%
      dplyr::select(-"group_id")
  }

  # use left_join so that unmatched loans are properly accounted for
  lbk_match_success <- dplyr::left_join(
    x = raw_lbk_with_sectors,
    y = matched_prioritized,
    by = c(
      "id_direct_loantaker",
      "name_direct_loantaker",
      "id_ultimate_parent",
      "name_ultimate_parent",
      "loan_size_outstanding",
      "loan_size_outstanding_currency",
      "loan_size_credit_limit",
      "loan_size_credit_limit_currency",
      "sector_classification_system",
      "sector_classification_direct_loantaker",
      "lei_direct_loantaker",
      "isin_direct_loantaker",
      "id_loan",
      by_group,
      "sector" = "sector_abcd",
      "borderline"
    )
  )

  lbk_match_success <- lbk_match_success %>%
    dplyr::mutate(
      matched = dplyr::case_when(
        .data[["score"]] == 1 ~ "Matched",
        is.na(.data[["score"]]) ~ "Not matched",
        TRUE ~ "Not matched"
      ),
      # unmatched borderline loans are considered not in scope, as they would
      # otherwise increase the potential exposure wrongly and artificially without
      # there being a realistic way to match that exposure
      sector = dplyr::case_when(
        .data[["borderline"]] & .data[["matched"]] == "Not matched" ~ "not in scope",
        TRUE ~ .data[["sector"]]
      )
    )
}

add_match_success_rate <- function(data,
                                   by_group) {
  data <- data %>%
    dplyr::mutate(
      total_n = dplyr::n(),
      total_outstanding = sum(.data[["loan_size_outstanding"]], na.rm = TRUE),
      total_credit_limit = sum(.data[["loan_size_credit_limit"]], na.rm = TRUE),
      .by = dplyr::all_of(c(by_group, "sector"))
    ) %>%
    dplyr::summarise(
      match_n = dplyr::n(),
      match_outstanding = sum(.data[["loan_size_outstanding"]], na.rm = TRUE),
      match_credit_limit = sum(.data[["loan_size_credit_limit"]], na.rm = TRUE),
      .by = dplyr::all_of(c(by_group, "sector", "matched", "total_n", "total_outstanding", "total_credit_limit"))
    ) %>%
    dplyr::mutate(
      match_success_rate_rel = .data[["match_n"]] / .data[["total_n"]],
      match_success_outstanding_rel = .data[["match_outstanding"]] / .data[["total_outstanding"]],
      match_success_credit_limit_rel = .data[["match_credit_limit"]] / .data[["total_credit_limit"]]
    )

  data <- data %>%
    dplyr::select(
      dplyr::all_of(
        c(
          by_group,
          "sector",
          "matched",
          "match_n",
          "total_n",
          "match_success_rate_rel",
          "match_outstanding",
          "total_outstanding",
          "match_success_outstanding_rel",
          "match_credit_limit",
          "total_credit_limit",
          "match_success_credit_limit_rel"
        )
      )
    ) %>%
    dplyr::arrange(
      .data[[by_group]],
      .data[["sector"]],
      .data[["matched"]]
    )
}

prep_match_success_rate <- function(data,
                                    by_group) {
  # prepare match success data for plotting----
  data <- data %>%
    dplyr::select(
      -dplyr::starts_with("total")
    ) %>%
    tidyr::pivot_longer(
      cols = -c(
        dplyr::all_of(by_group),
        "sector",
        "matched"
      ),
      names_to = "match_success_type",
      values_to = "match_success_rate"
    ) %>%
    dplyr::mutate(
      metric_type = dplyr::if_else(
        grepl("_rel$", .data[["match_success_type"]]),
        "relative",
        "absolute"
      ),
      match_success_type = dplyr::case_when(
        grepl("outstanding", .data[["match_success_type"]]) ~ "outstanding",
        grepl("credit_limit", .data[["match_success_type"]]) ~ "credit_limit",
        TRUE ~ "n"
      )
    )
}

