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
#' @export
#'
#' @examples
#' # TODO
analyse <- function(config) {
  config <- load_config(config)

  output_analysis_dir <- get_output_analysis_dir(config)

  assert_length(output_analysis_dir, 1L)
  assert_inherits(output_analysis_dir, "character")

  if (dir.exists(output_analysis_dir)) {
    ask_for_permission(
      "The output directory defined by the {.var dir_analysis} parameter in your config already exists.\n
      {.path {output_analysis_dir}}\n
      Would you like to delete it and replace it with the output of the current run?"
    )
    unlink(output_analysis_dir, recursive = TRUE)
  }
  dir.create(output_analysis_dir, recursive = TRUE, showWarnings = FALSE)

  run_pacta(config)
  run_aggregate_alignment_metric(config)
  plot_aggregate_loanbooks(config)
  
  write_manifest(
    config = config,
    path = file.path(output_analysis_dir, "manifest.yml"),
    prior_input_paths = c(
      get_output_prepare_dir(config),
      get_output_matched_loanbooks_dir(config),
      get_output_prio_diagnostics_dir(config)
    )
  )
}

#' @rdname analyse
#' @export
analyze <- analyse
