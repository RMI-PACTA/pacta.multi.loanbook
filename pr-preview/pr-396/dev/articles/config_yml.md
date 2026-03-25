# Configuring the application

## Intro

Most of the functions in this package require a path to a `config.yml`
file as input. This structure allows for an easily reviewed file that
contains all relevant parameters that should be / were used for a given
multi-loanbook analysis. Below is a full documentation of each option.

## Preface

The config file is separated into a few top-level “sections” that
contain contextually similar options. The top-level sections will be
documented as well below, but note that the top-level sections
themselves never have a value directly associated with them.

Also note that the config file must have the top-level section
`default`. This is related to a feature of the `yaml` package which
facilitates having and targeting different config sets for different
purposes. Technically, one could leverage this for use with
pacta.multi.loanbook, but it is not recommended.

## Options

### directories:

The `directories` section contains options to define locally accessible
paths where input and output data should be found or saved. A full
example `directories` section might look like:

``` yaml
  directories:
    dir_input: "~/Desktop/test/input"
    dir_prepared_abcd: "~/Desktop/test/prepared_abcd"
    dir_matched_loanbooks: "~/Desktop/test/matched_loanbooks"
    dir_prioritized_loanbooks_and_diagnostics: "~/Desktop/test/prioritized_loanbooks_and_diagnostics"
    dir_analysis: "~/Desktop/test/analysis"
```

##### dir_input

`dir_input` is a path to a directory that contains all input data to be
used. Input data is any data set that must be produced or obtained
externally by the user and that is not the output of any of the
functions in this package. This includes files only needed optionally.
It must be a single string/character value, and it must refer to a
valid, accessible, local directory. As an example:

``` yaml
    dir_input: "~/Desktop/test/input"
```

##### dir_prepared_abcd

`dir_prepared_abcd` is a path to a directory where the outputs of the
function
[`prepare_abcd()`](https://rmi-pacta.github.io/pacta.multi.loanbook/dev/reference/prepare_abcd.md)
should be saved. It must be a single string/character value, and it must
refer to a valid, accessible, local directory. As an example:

``` yaml
    dir_prepared_abcd: "~/Desktop/test/prepared_abcd"
```

##### dir_matched_loanbooks

`dir_matched_loanbooks` is a path to a directory where the outputs of
the function
[`match_loanbooks()`](https://rmi-pacta.github.io/pacta.multi.loanbook/dev/reference/match_loanbooks.md)
should be saved. It must be a single string/character value, and it must
refer to a valid, accessible, local directory. As an example:

``` yaml
    dir_matched_loanbooks: "~/Desktop/test/matched_loanbooks"
```

##### dir_prioritized_loanbooks_and_diagnostics

`dir_prioritized_loanbooks_and_diagnostics` is a path to a directory
where the outputs of the function
[`prioritise_and_diagnose()`](https://rmi-pacta.github.io/pacta.multi.loanbook/dev/reference/prioritise_and_diagnose.md)
should be saved. It must be a single string/character value, and it must
refer to a valid, accessible, local directory. As an example:

``` yaml
    dir_prioritized_loanbooks_and_diagnostics: "~/Desktop/test/prioritized_loanbooks_and_diagnostics"
```

##### dir_analysis

`dir_analysis` is a path to a directory where the outputs of the
function
[`analyse()`](https://rmi-pacta.github.io/pacta.multi.loanbook/dev/reference/analyse.md)
should be saved. It must be a single string/character value, and it must
refer to a valid, accessible, local directory. As an example:

``` yaml
    dir_analysis: "~/Desktop/test/analysis"
```

### file_names:

The `file_names` section contains options to define the file names of
locally accessible files found in the directories defined in the
`directories` section. The directories and file names are defined
separately to allow for flexibility in where and how your input and
output files are stored. A full example `file_names` section might look
like:

``` yaml
  file_names:
    filename_scenario_tms: "scenarios_2022_p4b.csv"
    filename_scenario_sda: "scenarios_2022_ei_p4b.csv"
    filename_abcd: "2023-02-17_AI_RMI_PACTA for Banks Free dataset_EO_2022Q4.xlsx"
    sheet_abcd: "Company Indicators - PACTA Comp"
```

##### filename_scenario_tms

`filename_scenario_tms` is the filename of the file that contains
production based scenario data. The file specified by
`filename_scenario_tms` must exist in the directory specified by the
`dir_input` parameter. It must be a single string/character value, and
it must refer to a valid, accessible, local file. As an example:

``` yaml
    filename_scenario_tms: "scenarios_2022_p4b.csv
```

##### filename_scenario_sda

`filename_scenario_sda` is the filename of the file that contains
emission intensity based scenario data. The file specified by
`filename_scenario_sda` must exist in the directory specified by the
`dir_input` parameter. It must be a single string/character value, and
it must refer to a valid, accessible, local file. As an example:

``` yaml
    filename_scenario_sda: "scenarios_2022_ei_p4b.csv"
```

##### filename_abcd

`filename_abcd` is the filename of the file in the directory defined by
`dir_input` that contains asset based company data, including production
values and physical emission intensity values. It must be a single
string/character value, and it must refer to a valid, accessible, local
file. As an example:

``` yaml
    filename_abcd: "2023-02-17_AI_RMI_PACTA for Banks Free dataset_EO_2022Q4.xlsx"
```

##### sheet_abcd

`sheet_abcd` is the name of the sheet that contains asset based company
data in the file defined by `filename_abcd` and stored in the directory
defined by `dir_input`. It must be a single string/character value, and
it must refer to a valid, accessible, sheet name in the appropriate
file. As an example:

``` yaml
    sheet_abcd: "Company Indicators - PACTA Comp"
```

### project_parameters:

A full example `project_parameters` section might look like:

``` yaml
  project_parameters:
    scenario_source: "weo_2022"
    scenario_select: "nze_2050"
    region_select: "global"
    start_year: 2022
    time_frame: 5
    by_group: "group_id"
```

##### scenario_source

`scenario_source` is an identifier of the scenario source to be used. It
must be a single string/character value, and it must refer to a valid,
accessible, scenario source identifier contained in the scenario data
file/s defined by `filename_scenario_tms` and `filename_scenario_sda`.
Valid values typically look like `"weo_2023"` or `"geco_2022"`. As an
example:

``` yaml
    scenario_source: "weo_2022"
```

##### scenario_select

`scenario_select` is an identifier of the scenario to be used. It must
be a single string/character value, and it must refer to a valid,
accessible, scenario identifier corresponding to the `scenario_source`
and contained in the scenario data file/s defined by
`filename_scenario_tms` and `filename_scenario_sda`. Valid values
typically look like `"nze_2050"`, `"aps"` or `"steps"`. As an example:

``` yaml
    scenario_select: "nze_2050"
```

##### region_select

`region_select` is an identifier of the region to be used. It must be a
single string/character value, and it must refer to a valid, accessible,
region identifier contained in the
[`r2dii.data``::``region_isos`](https://rmi-pacta.github.io/r2dii.data/reference/region_isos_demo.html)
dataset where it must be listed as a region available for the
`scenario_source`. Valid values typically look like `"global"` or
`"advanced economies"`. As an example:

``` yaml
    region_select: "global"
```

##### start_year

`start_year` is the start year of the analysis. Normally, the start year
should correspond with year of the publication of the scenario in use.
It must be a single numeric value, and it must refer to a valid,
accessible, year contained in the scenario data file/s defined by
`filename_scenario_tms` and `filename_scenario_sda`. Valid values
typically look like `2022` or `2023` (note that this value should *not*
be wrapped in quotes). As an example:

``` yaml
    start_year: 2022
```

##### time_frame

`time_frame` is the number of years (starting from the `start_year`)
that the analysis covers, defining the time frame. It must be a single
numeric value, and it must define a valid, accessible, time frame
covered by the scenario data file/s defined by `filename_scenario_tms`
and `filename_scenario_sda`. Valid values typically look like `5` or `6`
(note that this value should *not* be wrapped in quotes). As an example:

``` yaml
    time_frame: 5
```

##### by_group

`by_group` allows specifying the level of disaggregation to be used in
the analysis. It determines the variable along which the loan books are
grouped and thus the dimension by which to compare the PACTA
calculations. For example, one may want to calculate system-wide results
without disaggregation, using `NULL` or one may want to analyse
alignment along bank specific traits, such as `"group_id"` or
`"bank_type"`. It can be `NULL` or a character vector of length 1. If it
is not `NULL`, the indicated name must be a variable that is provided in
the input loan books and it must be complete (`"group_id"` is
automatically created when reading in the loan books, so the user does
not have to add it to the raw loan books). If the provided character
string is `"NULL"`, it will be treated as `NULL`. As an example:

``` yaml
    by_group: "group_id"
```

### sector_split:

A full example `sector_split` section might look like:

``` yaml
  sector_split:
    apply_sector_split: TRUE
    filename_split_company_id: "split_company_ids.csv"
    filename_advanced_company_indicators: "2024-02-14_AI_2023Q4_RMI-Company-Indicators.xlsx"
    sheet_advanced_company_indicators: "Company Activities"
```

##### apply_sector_split

`apply_sector_split` It must be a single logical value (either `TRUE` or
`FALSE`). As an example:

``` yaml
    apply_sector_split: TRUE
```

##### filename_split_company_id

`filename_split_company_id` is the filename of the CSV file that
contains the split company ID data. The file specified by
`filename_split_company_id` must exist in the directory specified by the
`dir_input` parameter. It must be a single string/character value, and
it must refer to a valid, accessible, local file. As an example:

``` yaml
    filename_split_company_id: "split_company_ids.csv"
```

##### filename_advanced_company_indicators

`filename_advanced_company_indicators` is the filename of the XLSX file
that contains the Advanced Company Indicators. The file specified by
`filename_advanced_company_indicators` must exist in the directory
specified by the `dir_input` parameter. It must be a single
string/character value, and it must refer to a valid, accessible, local
file. As an example:

``` yaml
    filename_advanced_company_indicators: "2024-02-14_AI_2023Q4_RMI-Company-Indicators.xlsx"
```

##### sheet_advanced_company_indicators

`sheet_advanced_company_indicators` is the name of the sheet that
contains asset based company production data in the file defined by
`filename_advanced_company_indicators` and stored in the directory
defined by `dir_input`. It must be a single string/character value, and
it must refer to a valid, accessible, sheet name in the appropriate
file. As an example:

``` yaml
    sheet_advanced_company_indicators: "Company Activities"
```

### matching:

A full example `matching` section might look like:

``` yaml
  matching:
    params_match_name:
      by_sector: TRUE
      min_score: 0.9
      method: "jw"
      p: 0.1
      overwrite: NULL
      join_id: NULL
    manual_sector_classification:
      use_manual_sector_classification: FALSE
      filename_manual_sector_classification: "manual_sector_classification.csv"
```

#### params_match_name:

A full example `params_match_name` section might look like:

``` yaml
    params_match_name:
      by_sector: TRUE
      min_score: 0.9
      method: "jw"
      p: 0.1
      overwrite: NULL
      join_id: NULL
```

##### by_sector

`by_sector`. It must be a single logical value (either `TRUE` or
`FALSE`). Further explanation of this argument can be found in the
documentation for
[`r2dii.match``::``match_name``()`](https://rmi-pacta.github.io/r2dii.match/reference/match_name.html).
As an example:

``` yaml
      by_sector: TRUE
```

##### min_score

`min_score` is a number between 0-1, to set the minimum score threshold.
A score of 1 is a perfect match. It must be a single numeric value.
Valid values typically look like `0.7` or `0.9` (note that this value
should *not* be wrapped in quotes). Further explanation of this argument
can be found in the documentation for
[`r2dii.match``::``match_name``()`](https://rmi-pacta.github.io/r2dii.match/reference/match_name.html).
As an example:

``` yaml
      min_score: 0.9
```

##### method

`method` is the method for distance calculation. It must be a single
string/character value, and it must refer to a valid method identifier,
one of `"osa"`, `"lv"`, `"dl"`, `"hamming"`, `"lcs"`, `"qgram"`,
`"cosine"`, `"jaccard"`, `"jw"`, `"soundex"`. Further explanation of
this argument can be found in the documentation for
[`r2dii.match``::``match_name``()`](https://rmi-pacta.github.io/r2dii.match/reference/match_name.html)
and
[`stringdist``::``stringdist``-``metrics`](https://rdrr.io/cran/stringdist/man/stringdist-metrics.html).
As an example:

``` yaml
      method: "jw"
```

##### p

`p` is the prefix factor for Jaro-Winkler distance. The valid range for
p is 0 \<= p \<= 0.25. If p=0 (default), the Jaro-distance is returned.
Applies only to method=‘jw’. It must be a single numeric value. Valid
values typically look like `0.1` or `0.2` (note that this value should
*not* be wrapped in quotes). Further explanation of this argument can be
found in the documentation for
[`r2dii.match``::``match_name``()`](https://rmi-pacta.github.io/r2dii.match/reference/match_name.html).
As an example:

``` yaml
      p: 0.1
```

##### overwrite

`overwrite`. Further explanation of this argument can be found in the
documentation for
[`r2dii.match``::``match_name``()`](https://rmi-pacta.github.io/r2dii.match/reference/match_name.html).
As an example:

``` yaml
      overwrite: NULL
```

##### join_id

`join_id` is an optional parameter that allows defining by which
variable to match the loans to the the companies in the `abcd`. Its
intended use case is join based on unambiguous identifiers, such as the
`lei`, where such data is available. It can be `NULL` to use standard
name matching when no common identifiers are given. Must be a join
specification which is internally passed to
[`dplyr::inner_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html).
If it is an unnamed character/string vector, the values are assumed to
refer to identically named join columns. If it is a named character
vector, the names are used as the join columns in the `loanbook` and the
values are used as the join columns in the `abcd`. Further explanation
of this argument can be found in the documentation for
[`r2dii.match``::``match_name``()`](https://rmi-pacta.github.io/r2dii.match/reference/match_name.html).
As an example:

``` yaml
      join_id: c(lei_direct_loantaker = "lei")
```

#### manual_sector_classification:

A full example `manual_sector_classification` section might look like:

``` yaml
    manual_sector_classification:
      use_manual_sector_classification: FALSE
      filename_manual_sector_classification: "manual_sector_classification.csv"
```

##### use_manual_sector_classification

`use_manual_sector_classification` determines if the matching should use
an internally provided sector classification system or if it should use
one provided by the user instead. Internal sector classification systems
are given in
[`r2dii.data::sector_classifications`](https://rmi-pacta.github.io/r2dii.data/reference/sector_classifications.html) -
see also additional [documentation in
`r2dii.data`](https://rmi-pacta.github.io/r2dii.data/reference/sector_classifications.html).
The function will automatically attempt to use one of the sector
classification systems, based on the inputs in the raw loan book files.
If an externally prepared sector classification system is to be used,
for example because the loans are classified using a system that is not
provided in r2dii.data out of the box, the data must be prepared
following the same structure as found in
[`r2dii.data::sector_classifications`](https://rmi-pacta.github.io/r2dii.data/reference/sector_classifications.html).
It must be a single logical value (either `TRUE` or `FALSE`). As an
example:

``` yaml
      use_manual_sector_classification: FALSE
```

##### filename_manual_sector_classification

`filename_manual_sector_classification` is the filename of the CSV that
contains the manual sector classification data. The file specified by
`filename_manual_sector_classification` must exist in the directory
specified by the `dir_input` parameter. It must be a single
string/character value, and it must refer to a valid, accessible, local
file. As an example:

``` yaml
      filename_manual_sector_classification: "manual_sector_classification.csv"
```

### match_prioritize:

A full example `match_prioritize` section might look like:

``` yaml
  match_prioritize:
    priority: NULL
```

##### priority

`priority` indicates the level of matching that should be prioritized
when a loan can be matched at multiple levels. It must be a single
string/character value or `NULL`, and it must refer to a valid,
accessible, local file. Further explanation of this argument can be
found in the documentation for
[`r2dii.match``::``priortize``()`](https://rmi-pacta.github.io/r2dii.match/reference/prioritize.html).
As an example:

``` yaml
    priority: NULL
```

### prepare_abcd:

A full example `prepare_abcd` section might look like:

``` yaml
  prepare_abcd:
    remove_inactive_companies: TRUE
```

##### remove_inactive_companies

`remove_inactive_companies` determines if inactive companies should be
removed from the abcd dataset or not. “Companies” here refers to
company-sector combinations and “inactive” characterizes such
company-sector combinations that are inactive at the end of the time
frame analysed. When focusing forward looking analysis on exposures in
the end year, such inactive companies may not produce meaningful
results. It must be a single logical value (either `TRUE` or `FALSE`).
As an example:

``` yaml
    remove_inactive_companies: TRUE
```

### match_success_rate:

A full example `match_success_rate` section might look like:

``` yaml
  match_success_rate:
    plot_width: 12
    plot_height: 8
    plot_units: "in"
    plot_resolution: 300
```

##### plot_width

`plot_width` is the desired width of the XXX output plot in units
defined by `plot_units`. It must be a single numeric value. Valid values
typically look like `10` or `12` (note that this value should *not* be
wrapped in quotes). As an example:

``` yaml
    plot_width: 12
```

##### plot_height

`plot_height` is the desired height of the XXX output plot in units
defined by `plot_units`. It must be a single numeric value. Valid values
typically look like `6` or `8` (note that this value should *not* be
wrapped in quotes). As an example:

``` yaml
    plot_height: 8
```

##### plot_units

`plot_units` is the desired units to express the dimensions of the XXX
output plot in `plot_width` and `plot_height`. It must be a single
string/character value, and it must refer to a valid unit identifier.
Valid values typically look like `"in"` or `"px"`. As an example:

``` yaml
    plot_units: "in"
```

##### plot_resolution

`plot_resolution` is the desired resolution of the XXX output plot in
dpi. It must be a single numeric value. Valid values typically look like
`72` or `300` (note that this value should *not* be wrapped in quotes).
As an example:

``` yaml
    plot_resolution: 300
```
