#' Prepare input data sets for PACTA for Supervisors analysis
#'
#' @description
#' `prepare_abcd()` runs the necessary steps to prepare the input data sets for
#' the PACTA for Supervisors analysis. Specifically it prepares the abcd_final
#' data set by removing inactive companies if desired. And it allows preparing
#' the ratios by which the exposures to counterparties are split along the sectors.
#' Parameters for both steps are read from a `config.yml` file. The function is
#' called for its side effects and writes the prepared data sets to the directory
#' `output/prepare`, where `output` is the output directory specified in the
#' `config.yml`.
#'
#' @param config either a path to a config.yml file or a list of parameters
#'
#' @return NULL
#' @export
#'
#' @examples
#' # TODO
prepare_abcd <- function(config) {
  config <- load_config(config)

  dir_prepared_abcd <- get_dir_prepared_abcd(config)

  assert_length(dir_prepared_abcd, 1L)
  assert_inherits(dir_prepared_abcd, "character")

  if (dir.exists(dir_prepared_abcd)) {
    ask_for_permission(
      "The output directory defined by the {.var dir_prepared_abcd} parameter in your config already exists.\n
      {.path {dir_prepared_abcd}}\n
      Would you like to delete it and replace it with the output of the current run?"
    )
    unlink(dir_prepared_abcd, recursive = TRUE)
  }
  dir.create(dir_prepared_abcd, recursive = TRUE, showWarnings = FALSE)

  remove_inactive_companies(config)

  if (get_apply_sector_split(config)) {
    prepare_sector_split(config)
  }

  write_manifest(
    config = config,
    path = file.path(dir_prepared_abcd, "manifest.yml"),
    prior_input_paths = NULL
  )
}
