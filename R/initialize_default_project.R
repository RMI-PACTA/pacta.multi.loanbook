#' Initialize a project directory and its config file
#'
#' @param path an absolute or relative path pointing to the location you would
#'   like the project directory to be created
#'
#' @returns NULL
#'
#' @export

initialize_default_project <- function(path = "project") {
  if (dir.exists(path)) {
    cli::cli_abort("The path {.path {normalizePath(path)}} already exists. Cannot overwrite an existing path.")
  }

  dir.create(path)

  default_cfg <- yaml::read_yaml(file = system.file("default_config.yml", package = "pacta.multi.loanbook"))

  input_dir <- file.path(path, "input")
  dir.create(input_dir)
  default_cfg$default$directories$dir_input <- normalizePath(input_dir)

  prepared_abcd_dir <- file.path(path, "prepared_abcd")
  dir.create(prepared_abcd_dir)
  default_cfg$default$directories$dir_prepared_abcd <-
    normalizePath(prepared_abcd_dir)

  matched_loanbooks_dir <- file.path(path, "matched_loanbooks")
  dir.create(matched_loanbooks_dir)
  default_cfg$default$directories$dir_matched_loanbooks <-
    normalizePath(matched_loanbooks_dir)

  prioritized_loanbooks_and_diagnostics_dir <- file.path(path, "prioritized_loanbooks_and_diagnostics")
  dir.create(prioritized_loanbooks_and_diagnostics_dir)
  default_cfg$default$directories$dir_prioritized_loanbooks_and_diagnostics <-
    normalizePath(prioritized_loanbooks_and_diagnostics_dir)

  analysis_dir <- file.path(path, "analysis")
  dir.create(analysis_dir)
  default_cfg$default$directories$dir_analysis <-
    normalizePath(analysis_dir)

  yaml::write_yaml(
    x = default_cfg,
    file = file.path(path, "config.yml"),
    handlers = list(
      logical = function(x) `class<-`(as.character(as.logical(x)), "verbatim"),
      NULL = function(x) `class<-`("NULL", "verbatim"),
      character = function(x) `attr<-`(x, "quoted", TRUE)
    )
  )

  cli::cli_inform(c(
    "Project directory has been initialized here: {.path {normalizePath(path)}}",
    "You should review and edit if necessary your config file here: {.file {normalizePath(file.path(path, 'config.yml'))}}"
  ))
}
