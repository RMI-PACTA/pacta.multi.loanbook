#' @param data_alignment data.frame. Holds aggregated alignment metrics per
#'   company for tms sectors. Must contain columns: `"name_abcd"`,
#'   `"sector"` and any column implied by `group_var`.
#' @param region Character. Region to filter `data_alignment` data frame on.
#' @param year Integer. Year on which `data_alignment` should be filtered.
#' @param group_var Character. Vector of length 1. Variable to group by.
#' @param middle_node Character. Column specifying the middle nodes to be
#'   plotted in sankey plot. Must be present in `data_alignment`.
#'
#' @return data.frame
#'
#' @rdname plot_sankey
#'
#' @noRd

prep_sankey <- function(data_alignment,
                        region,
                        year,
                        group_var,
                        middle_node) {
  if (!is.null(group_var)) {
    if (!inherits(group_var, "character")) {
      cli::cli_abort("{.arg group_var} must be of class {.cls character}")
    }
    if (!length(group_var) == 1) {
      cli::cli_abort("{.arg group_var} must be of length 1")
    }
  } else {
    data_alignment <- data_alignment %>%
      dplyr::mutate(aggregate_loan_book = "Aggregate loan book")
    group_var <- "aggregate_loan_book"
  }

  check_prep_sankey(
    data_alignment = data_alignment,
    region = region,
    year = year,
    group_var = group_var,
    middle_node = middle_node
  )

  data_alignment <- data_alignment %>%
    dplyr::filter(
      .data[["region"]] == .env[["region"]],
      .data[["year"]] == .env[["year"]]
    )

  data_out <- data_alignment %>%
    dplyr::mutate(
      is_aligned = dplyr::case_when(
        alignment_metric >= 0 ~ "Aligned",
        alignment_metric < 0 ~ "Not aligned",
        TRUE ~ "Unknown"
      ),
      middle_node = tools::toTitleCase(!!rlang::sym(middle_node))
    ) %>%
    dplyr::select(
      dplyr::all_of(
        c(
          group_var,
          "middle_node",
          "is_aligned",
          "loan_size_outstanding",
          "loan_size_outstanding_currency"
        )
      )
    ) %>%
    dplyr::summarise(
      loan_size_outstanding = sum(.data[["loan_size_outstanding"]], na.rm = TRUE),
      .by = c(group_var, "middle_node", "is_aligned", "loan_size_outstanding_currency")
    ) %>%
    dplyr::arrange(!!rlang::sym(group_var), .data[["is_aligned"]]) %>%
    dplyr::mutate(
      # y_axis = .data$loan_size_outstanding,
      initial_node = .data[[group_var]],
      end_node = .data$is_aligned#,
      # stratum = .data$is_aligned
    ) %>%
    dplyr::select(
      dplyr::all_of(
        c(
          "loan_size_outstanding",
          # "y_axis",
          "initial_node",
          "middle_node",
          "end_node",
          "is_aligned",
          # "stratum",
          currency = "loan_size_outstanding_currency"
        )
      )
    )

  data_out
}

check_prep_sankey <- function(data_alignment,
                              region,
                              year,
                              group_var,
                              middle_node) {
  names_all <- c(group_var, "name_abcd", "sector")
  names_aggergate <- c("region", "year")
  assert_no_missing_names(data_alignment, c(names_all, names_aggergate))
  if (!(region %in% unique(data_alignment$region))) {
    cli::cli_abort(c(
      x = "{.arg region} value not found in {.var data_alignment} dataset",
      i = "{cli::qty(length(unique(data_alignment$region)))}region{?s} in
           {.var data_alignment} {?is/are}:
          {.val {as.character(unique(data_alignment$region))}}",
      i = "the value{?s} in {.arg region} {?is/are}: {.val {region}}"
    ))
  }
  if (!(year %in% unique(data_alignment$year))) {
    cli::cli_abort(c(
      x = "{.arg year} value not found in {.var data_alignment} dataset",
      i = "{cli::qty(length(unique(data_alignment$year)))}year{?s} in
           {.var data_alignment} {?is/are}: {.val {unique(data_alignment$year)}}",
      i = "the value{?s} in {.arg year} {?is/are}: {.val {unique(year)}}"
    ))
  }
  assert_middle_node_column_exists(data_alignment, middle_node, env = list(data = substitute(data_alignment)))
}

assert_middle_node_column_exists <- function(data, name, env = parent.frame()) {
  .data <- deparse1(substitute(data, env = env))

  if (!(name %in% names(data))) {
    cli::cli_abort(c(
      x = "column name you passed as one of the middle nodes not found in {.var {(.data)}}",
      i = "{cli::qty(length(names(data)))}column name{?s} in
           {.var {(.data)}} {?is/are}: {.val {names(data)}}",
      i = "you asked to use column named: {.val {name}}"
    ))
  }
}
