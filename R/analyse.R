#' Analyse the loan book data sets used in the PACTA for Supervisors analysis
#'
#' @description
#' `analyse()` runs the necessary steps to analyse the matched loan books,
#' producing both the outputs of the standard PACTA analysis and the outputs of
#' the net aggregated alignment metric, including tables and plots.
#' Parameters for all steps are read from a `config.yml` file. The function is
#' called for its side effects and writes the prepared and diagnosed data sets
#' to the directory `output/analysis`, where `output` is the
#' output directory  specified in the `config.yml`.
#'
#' `analyse()` and `analyze()` are synonyms.
#'
#' @param config either a path to a config.yml file or a list of parameters
#'
#' @return NULL
#'
#' @export

analyse <- function(config) {
  config <- load_config(config)

  dir_analysis <- get_dir_analysis(config)

  assert_length(dir_analysis, 1L)
  assert_inherits(dir_analysis, "character")

  if (dir.exists(dir_analysis)) {
    ask_for_permission(
      "The output directory defined by the {.var dir_analysis} parameter in your config already exists.\n
      {.path {dir_analysis}}\n
      Would you like to delete it and replace it with the output of the current run?"
    )
    unlink(dir_analysis, recursive = TRUE)
  }
  dir.create(dir_analysis, recursive = TRUE, showWarnings = FALSE)

  run_pacta(config)
  run_aggregate_alignment_metric(config)
  plot_aggregate_loanbooks(config)

  write_manifest(
    config = config,
    path = file.path(dir_analysis, "manifest.yml"),
    prior_input_paths = c(
      get_dir_prepared_abcd(config),
      get_dir_matched_loanbooks(config),
      get_dir_prioritized_loanbooks_and_diagnostics(config)
    )
  )
}

#' @rdname analyse
#' @export
analyze <- analyse
