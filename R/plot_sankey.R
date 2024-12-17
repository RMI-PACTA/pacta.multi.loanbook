#' Make a sankey plot
#'
#' @param data data.frame. Should have the same format as output of
#'   `prep_sankey()` and contain columns: `"middle_node"`, optionally
#'   `"middle_node2"`, `"is_aligned"`, `"loan_size_outstanding"`, and any column
#'   implied by `group_var`.
#' @param y_axis Character. Vector of length 1. Variable to group by.
#' @param initial_node Logical. Flag indicating if node labels should
#'   be converted into better looking capitalised form.
#' @param middle_node Character. Path where the output in png format should be
#'   saved
#' @param end_node Logical. Flag indicating if nodes order should
#'   be determined by an algorithm (in case of big datasets often results in a
#'   better looking plot) or should they be ordered based on data.
#' @param stratum Character. File name of the output.
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
  data <- data %>%
    dplyr::mutate(
      y_axis = .data[[y_axis]],
      initial_node = .data[[initial_node]],
      middle_node = .data[[middle_node]],
      end_node = .data[[end_node]],
      stratum = .data[[stratum]]
    ) %>%
    dplyr::select(!dplyr::all_of(c(y_axis, initial_node, middle_node, end_node, stratum)))

  p <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      axis1 = .data[["initial_node"]],
      axis2 = .data[["middle_node"]],
      axis3 = .data[["end_node"]],
      y = .data[["y_axis"]]
    )
  ) +
    ggplot2::scale_x_discrete(
      limits = c(initial_node, middle_node, end_node), expand = c(.2, .05)
    ) +
    ggplot2::xlab("Counterparty alignment") +
    ggplot2::ylab("Financial exposure") +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = .data[["stratum"]])) +
    ggplot2::scale_fill_manual(
      values = c("Aligned" = "green4", "Not aligned" = "red3", "Unknown" = "gray")#,
      # labels = c("Aligned", "Not aligned", "Unknown")
    ) +
    ggalluvial::geom_stratum() +
    # ggplot2::geom_text(
    #   stat = ggalluvial::StatStratum,
    #   ggplot2::aes(label = ggplot2::after_stat(stratum))
    # ) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = ggplot2::after_stat(stratum)),
      stat = ggalluvial::StatStratum, size = 4, direction = "y", nudge_x = .5
    ) +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle("Sankey chart of counterparty alignment by financial exposure",
                     "stratified by counterpaty alignment and sector")

  p
}

