# Prioritise and diagnose the loan book data sets used in the PACTA for Supervisors analysis

`prioritise_and_diagnose()` runs the necessary steps to prioritise the
matched loan books and diagnose both the match success rate and the
coverage of the real economy assets by the matched loan books.
Parameters for all steps are read from a `config.yml` file. The function
is called for its side effects and writes the prepared and diagnosed
data sets in the directory specified by
`dir_prioritized_loanbooks_and_diagnostics` in the `config.yml`.

`prioritise_and_diagnose()` and `prioritize_and_diagnose()` are
synonyms.

## Usage

``` r
prioritise_and_diagnose(config)

prioritize_and_diagnose(config)
```

## Arguments

- config:

  either a path to a config.yml file or a list of parameters

## Value

`prioritise_and_diagnose()` returns `NULL` invisibly. The function is
called for its side effects and writes the prepared and diagnosed data
sets in the directory specified by
`dir_prioritized_loanbooks_and_diagnostics` in the `config.yml`.

## Examples

``` r
if (FALSE) { # \dontrun{
config <- "path/to/config.yml"
prioritise_and_diagnose(config)
} # }
```
