#' Make a sankey plot
#'
#' @param data data.frame. Should have the same format as output of
#'   `prep_sankey()` and contain columns: `"y_axis"`, `"initial_node"`,
#'   `"middle_node"`, `"end_node"`, `"stratum"`, `"currency"`.
#' @param y_axis Character. Vector of length 1. Variable to determine the
#'   vertical size of the ribbons, e.g. `"loan_size_outstanding"`.
#' @param initial_node Character. Vector of length 1. Variable to determine the
#'   initial node of the sankey chart. Usually, this will be the groups by which
#'   the loan books are aggregated.
#' @param middle_node Character. Vector of length 1. Variable to determine the
#'   middle node of the sankey chart. Usually, this will be the PACTA sectors.
#' @param end_node Character. Vector of length 1. Variable to determine the
#'   end node of the sankey chart. Usually, this will be a binary indicator of
#'   alignment.
#' @param stratum  Character. Vector of length 1. Variable to determine the
#'   grouping and fill of the ribbons of the sankey chart. Usually, this will be
#'   a binary indicator of alignment.
#'
#' @return NULL
#'
#' @noRd

plot_sankey <- function(data,
                        y_axis,
                        initial_node,
                        middle_node = "sector",
                        end_node = "is_aligned",
                        stratum = "is_aligned") {
  # since the initial node is the loan book aggregation, NULL grouping corresponds to the aggregate loan book
  if (is.null(initial_node)) {
    initial_node <- "aggregate_loan_book"
  }

  currency <- unique(data[["currency"]])

  p <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      axis1 = .data[["initial_node"]],
      axis2 = .data[["middle_node"]],
      axis3 = .data[["end_node"]],
      y = .data[["y_axis"]]
    )
  ) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::ylab(glue::glue("Financial exposure (in {currency})")) +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = .data[["stratum"]])) +
    ggplot2::scale_fill_manual(
      values = c("Aligned" = "green4", "Not aligned" = "red3", "Unknown" = "gray40")
    ) +
    ggalluvial::geom_stratum(fill = "gray85", color = "gray50") +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = ggplot2::after_stat(stratum)),
      stat = ggalluvial::StatStratum, size = 4, direction = "y", nudge_x = .3
    ) +
    r2dii.plot::theme_2dii() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::ggtitle(
      "Sankey chart of counterparty alignment by financial exposure",
      paste0("stratified by counterpaty alignment and ", middle_node)
    )

  p
}
