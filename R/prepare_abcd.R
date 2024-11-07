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

  output_prepare_dir <- get_output_prepare_dir(config)

  stop_if_not_length(output_prepare_dir, 1L)
  stop_if_not_inherits(output_prepare_dir, "character")

  if (dir.exists(output_prepare_dir)) {
    warning("Output directory `dir_prepared_abcd` already exists. The existing directory will be removed and replaced with the output of the current run.")
    unlink(output_prepare_dir, recursive = TRUE)
  }
  dir.create(output_prepare_dir, recursive = TRUE, showWarnings = FALSE)

  write_manifest(config, file.path(output_prepare_dir, "manifest.yml"))
  
  remove_inactive_companies(config)
  
  if (get_apply_sector_split(config)) {
    prepare_sector_split(config)
  }
}
