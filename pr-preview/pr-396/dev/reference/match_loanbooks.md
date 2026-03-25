# Match raw input loan books with ABCD for PACTA for Supervisors analysis

`match_loanbooks()` runs the necessary steps to match the raw input loan
books with the asset based company data (ABCD) used in the PACTA for
Supervisors analysis. Specifically, it prepares matched loan books based
on name matching or direct identifiers, depending on the configuration.
The output matched loan books need to be manually validated for further
processing. Parameters for the matching step are read from a
`config.yml` file and follow the options available in
[`r2dii.match::match_name`](https://rmi-pacta.github.io/r2dii.match/reference/match_name.html).
The function is called for its side effects and writes the prepared data
sets to the directory specified by `dir_matched_loanbooks` in the
`config.yml`.

## Usage

``` r
match_loanbooks(config)
```

## Arguments

- config:

  either a path to a config.yml file or a list of parameters

## Value

`match_loanbooks()` returns `NULL` invisibly. The function is called for
its side effects and writes the prepared data sets to the directory
specified by `dir_matched_loanbooks` in the `config.yml`.

## Examples

``` r
if (FALSE) { # \dontrun{
config <- "path/to/config.yml"
match_loanbooks(config)
} # }
```
