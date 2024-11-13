beautify_scenario_label <- function(label) {
  out <- toupper(label)
  out <- r2dii.plot::to_title(out)
  out
}


#' Check if a named object contains expected names
#'
#' Based on fgeo.tool::abort_if_missing_names()
#'
#' @param x A named object.
#' @param expected_names String; expected names of `x`.
#'
#' @return Invisible `x`, or an error with informative message.
#'
#' @examples
#' x <- c(a = 1)
#' assert_no_missing_names(x, "a")
#' try(assert_no_missing_names(x, "bad"))
#'
#' @noRd

assert_no_missing_names <- function(data, expected_names) {
  if (!rlang::is_named(data)) {
    cli::cli_abort(
      message = c(x = "{.arg data} must be named"),
      .envir = rlang::env_parent()
    )
  }

  if (!is.character(expected_names)) {
    cli::cli_abort(
      message = c(x = "{.arg expected_names} must be of type {.cls character}"),
      .envir = rlang::env_parent()
    )
  }

  if (!all(unique(expected_names) %in% names(data))) {
    missing_names <- sort(setdiff(expected_names, names(data)))
    cli::cli_abort(
      c(
        x = "{.arg data} must have all the expected names",
        i = "Missing names: {.val {missing_names}}"
      ),
      class = "missing_names"
    )
  }

  invisible(data)
}


assert_no_unknown_values <- function(value, data, column) {
  if (is.null(value)) {
    return(invisible(value))
  }

  .value <- deparse1(substitute(value))
  .data <- deparse1(substitute(data))

  valid <- unique(data[[column]])
  if (!all(value %in% valid)) {
    msg <- c(
      x = "Each value of {.arg {(.value)}} must be one of these: {.val {value}}",
      i = "You passed: {.val {value}}",
      i = "Do you need to see valid values in this dataset?: {.arg {(.data)}}"
    )
    cli::cli_abort(msg, class = "unknown_value")
  }

  invisible(value)
}
