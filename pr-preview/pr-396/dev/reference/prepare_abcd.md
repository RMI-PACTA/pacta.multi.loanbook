# Prepare input data sets for PACTA for Supervisors analysis

`prepare_abcd()` runs the necessary steps to prepare the input data sets
for the PACTA for Supervisors analysis. Specifically it prepares the
abcd_final data set by removing inactive companies if desired. And it
allows preparing the ratios by which the exposures to counterparties are
split along the sectors. Parameters for both steps are read from a
`config.yml` file. The function is called for its side effects and
writes the prepared data sets in the directory specified by
`dir_prepared_abcd` in the `config.yml`.

## Usage

``` r
prepare_abcd(config)
```

## Arguments

- config:

  either a path to a config.yml file or a list of parameters

## Value

`prepare_abcd()` returns `NULL` invisibly. The function is called for
its side effects and writes the prepared data sets in the directory
specified by `dir_prepared_abcd` in the `config.yml`.

## Examples

``` r
if (FALSE) { # \dontrun{
config <- "path/to/config.yml"
prepare_abcd(config)
} # }
```
