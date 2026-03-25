# Initialise a project directory and its config file

`initialise_default_project()` sets up a default project directory,
including a project directory, a default `config.yml` configuration
file, an `input` sub-directory, and a `loanbooks` sub-directory.

## Usage

``` r
initialise_default_project(path = "project")

initialize_default_project(path = "project")
```

## Arguments

- path:

  an absolute or relative path pointing to the location you would like
  the project directory to be created

## Value

`initialise_default_project()` returns `NULL` invisibly. The function is
called for its side effects of creating a default project directory at
the specified path.

## Examples

``` r
if (FALSE) { # \dontrun{
project_dir <- "path/to/project"
initialise_default_project(project_dir)
} # }
```
