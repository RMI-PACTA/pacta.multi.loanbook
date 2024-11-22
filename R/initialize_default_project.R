#' Initialise a project directory and its config file
#'
#' @param path an absolute or relative path pointing to the location you would
#'   like the project directory to be created
#'
#' @returns NULL
#'
#' @export

initialise_default_project <- function(path = "project") {
  if (dir.exists(path)) {
    cli::cli_abort(c(
      x = "Cannot overwrite an existing path.",
      i = "The specified path already exists: {.path {normalizePath(path)}}"
    ))
  }

  dir.create(path)
  path <- normalizePath(path)

  default_cfg <- yaml::read_yaml(file = system.file("default_config.yml", package = "pacta.multi.loanbook"))

  default_cfg$default$directories$dir_input <- file.path(path, "input")
  dir.create(default_cfg$default$directories$dir_input)

  default_cfg$default$directories$dir_prepared_abcd <-
    file.path(path, "prepared_abcd")

  default_cfg$default$directories$dir_matched_loanbooks <-
    file.path(path, "matched_loanbooks")

  default_cfg$default$directories$dir_prioritized_loanbooks_and_diagnostics <-
    file.path(path, "prioritized_loanbooks_and_diagnostics")

  default_cfg$default$directories$dir_analysis <-
    file.path(path, "analysis")

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
    i = "Project directory has been initialised here: {.path {path}}",
    i = "Put your input files in the directory here: {.path {default_cfg$default$directories$dir_input}}",
    i = "You should review and edit if necessary your config file here: {.file {file.path(path, 'config.yml')}}"
  ))
}


#' @rdname initialise_default_project
#' @export
initialize_default_project <- initialise_default_project
