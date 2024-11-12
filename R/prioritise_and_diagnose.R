#' Prioritise and diagnose the loan book data sets used in the PACTA for Supervisors analysis
#'
#' @description
#' `prioritise_and_diagnose()` runs the necessary steps to prioritise the matched
#' loan books and diagnose both the match success rate and the coverage of the
#' real economy assets by the matched loan books.
#' Parameters for all steps are read from a `config.yml` file. The function is
#' called for its side effects and writes the prepared and diagnosed data sets
#' to the directory `output/prioritise_and_diagnose`, where `output` is the
#' output directory  specified in the `config.yml`.
#'
#' `prioritise_and_diagnose()` and `prioritize_and_diagnose()` are synonyms.
#'
#' @param config either a path to a config.yml file or a list of parameters
#'
#' @return NULL
#' @export
#'
#' @examples
#' # TODO
prioritise_and_diagnose <- function(config) {
  config <- load_config(config)

  dir_prioritized_loanbooks_and_diagnostics <- get_dir_prioritized_loanbooks_and_diagnostics(config)

  assert_length(dir_prioritized_loanbooks_and_diagnostics, 1L)
  assert_inherits(dir_prioritized_loanbooks_and_diagnostics, "character")

  if (dir.exists(dir_prioritized_loanbooks_and_diagnostics)) {
    ask_for_permission(
      "The output directory defined by the {.var dir_prioritized_loanbooks_and_diagnostics} parameter in your config already exists.\n
      {.path {dir_prioritized_loanbooks_and_diagnostics}}\n
      Would you like to delete it and replace it with the output of the current run?"
    )
    unlink(dir_prioritized_loanbooks_and_diagnostics, recursive = TRUE)
  }
  dir.create(dir_prioritized_loanbooks_and_diagnostics, recursive = TRUE, showWarnings = FALSE)

  run_match_prioritize(config)
  run_calculate_match_success_rate(config)
  run_calculate_loanbook_coverage(config)

  write_manifest(
    config = config,
    path = file.path(dir_prioritized_loanbooks_and_diagnostics, "manifest.yml"),
    prior_input_paths = c(
      get_dir_prepared_abcd(config),
      get_dir_matched_loanbooks(config)
    )
  )
}

#' @rdname prioritise_and_diagnose
#' @export
prioritize_and_diagnose <- prioritise_and_diagnose
