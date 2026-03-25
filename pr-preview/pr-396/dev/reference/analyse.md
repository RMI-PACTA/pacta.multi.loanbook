# Analyse the loan book data sets used in the PACTA for Supervisors analysis

`analyse()` runs the necessary steps to analyse the matched loan books,
producing both the outputs of the standard PACTA analysis and the
outputs of the net aggregated alignment metric, including tables and
plots. Parameters for all steps are read from a `config.yml` file. The
function is called for its side effects and writes the prepared and
diagnosed data sets in the directory specified by `dir_analysis` in the
`config.yml`.

`analyse()` and `analyze()` are synonyms.

## Usage

``` r
analyse(config)

analyze(config)
```

## Arguments

- config:

  either a path to a config.yml file or a list of parameters

## Value

`analyse()` returns `NULL` invisibly. The function is called for its
side effects and writes the prepared and diagnosed data sets in the
directory specified by `dir_analysis` in the `config.yml`.

## Examples

``` r
if (FALSE) { # \dontrun{
config <- "path/to/config.yml"
analyse(config)
} # }
```
