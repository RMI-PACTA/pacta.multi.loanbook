#' Match raw input loan books with ABCD for PACTA for Supervisors analysis
#'
#' @description
#' `match_loanbooks()` runs the necessary steps to match the raw input loan
#' books with the asset based company data (ABCD) used in the PACTA for
#' Supervisors analysis. Specifically, it prepares matched loan books based on
#' name matching or direct identifiers, depending on the configuration. The
#' output matched loan books need to be manually validated for further
#' processing. Parameters for the matching step are read from a `config.yml`
#' file and follow the options available in `r2dii.match::match_name`. The
#' function is called for its side effects and writes the prepared data sets to
#' the directory `output/match`, where `output` is the output directory
#' specified in the `config.yml`.
#'
#' @param config either a path to a config.yml file or a list of parameters
#'
#' @return NULL
#'
#' @export

match_loanbooks <- function(config) {
  config <- load_config(config)

  # input paths for match_loanbooks
  dir_loanbooks <- get_dir_loanbooks(config)
  dir_prepared_abcd <- get_dir_prepared_abcd(config)

  # output path for match_loanbooks
  dir_matched_loanbooks <- get_dir_matched_loanbooks(config)

  if (dir.exists(dir_matched_loanbooks)) {
    ask_for_permission(
      "The output directory defined by the {.var dir_matched_loanbooks} parameter in your config already exists.\n
      {.path {dir_matched_loanbooks}}\n
      Would you like to delete it and replace it with the output of the current run?"
    )
    unlink(dir_matched_loanbooks, recursive = TRUE)
  }
  dir.create(dir_matched_loanbooks, recursive = TRUE, showWarnings = FALSE)

  matching_by_sector <- get_match_by_sector(config)
  matching_min_score <- get_match_min_score(config)
  matching_method <- get_match_method(config)
  # argument p only applies for Jaro-Winkler method
  if (matching_method == "jw") {matching_p <- get_match_p(config)}
  matching_overwrite <- get_match_overwrite(config)
  matching_join_id <- get_match_join_id(config)

  matching_use_manual_sector_classification <- get_use_manual_sector_classification(config)
  if (matching_use_manual_sector_classification) {
    path_manual_sector_classification <- get_path_manual_sector_classification(config)
  }

  # validate config values----
  assert_length(dir_loanbooks, 1L)
  assert_inherits(dir_loanbooks, "character")
  assert_dir_exists(dir_loanbooks, desc = "Input - loanbooks")

  assert_length(dir_prepared_abcd, 1L)
  assert_inherits(dir_prepared_abcd, "character")
  assert_dir_exists(dir_prepared_abcd, desc = "Output - prepare ABCD")
  assert_file_exists(file.path(dir_prepared_abcd, "abcd_final.csv"), desc = "ABCD final")

  assert_length(dir_matched_loanbooks, 1L)
  assert_inherits(dir_matched_loanbooks, "character")
  assert_dir_exists(dir_matched_loanbooks, desc = "Output - Matched loanbooks")

  assert_length(matching_by_sector, 1L)
  assert_inherits(matching_by_sector, "logical")

  assert_length(matching_min_score, 1L)
  assert_inherits(matching_min_score, "numeric")

  assert_length(matching_method, 1L)
  assert_inherits(matching_method, "character")

  if (matching_method == "jw") {
    assert_length(matching_p, 1L)
    assert_inherits(matching_p, "numeric")
  }

  if (!is.null(matching_overwrite)) {
    assert_inherits(matching_overwrite, "data.frame")
    # cols are based on r2dii.data::overwrite_demo
    assert_expected_columns(
      data = matching_overwrite,
      cols = c("level", "id_2dii", "name", "sector", "source"),
      desc = "matching_overwrite"
    )
  }

  if (!is.null(matching_join_id)) {
    assert_inherits(matching_join_id, "character")
  }

  assert_length(matching_use_manual_sector_classification, 1L)
  assert_inherits(matching_use_manual_sector_classification, "logical")

  # path to manual sector classification only required if boolean TRUE
  if (matching_use_manual_sector_classification) {
    assert_length(path_manual_sector_classification, 1L)
    assert_inherits(path_manual_sector_classification, "character")
    assert_file_exists(path_manual_sector_classification, desc = "Manual sector classification")
  }

  # load data----

  ## load abcd----
  abcd <- readr::read_csv(
    file.path(dir_prepared_abcd, "abcd_final.csv"),
    col_select = dplyr::all_of(cols_abcd),
    col_types = col_types_abcd_final
  )

  ## optionally load manual classification system----
  if (matching_use_manual_sector_classification) {
    sector_classification_system <- readr::read_csv(
      file = path_manual_sector_classification,
      col_types = col_types_sector_classification,
      col_select = dplyr::all_of(col_select_sector_classification)
    )
  }

  ## load raw loan books----
  list_raw <- list.files(path = dir_loanbooks, pattern = "[.]csv$")
  assert_any_file_exists(list_raw, dir_loanbooks, "dir_input", "raw loan book CSVs")

  raw_lbk <- readr::read_csv(
    file = file.path(dir_loanbooks, list_raw),
    col_types = col_types_raw,
    id = "group_id"
  ) %>%
    dplyr::mutate(group_id = tools::file_path_sans_ext(basename(.data[["group_id"]]))) %>%
    dplyr::group_split(.data[["group_id"]])

  # match and save loan books----
  cli::cli_progress_bar(
    total = length(raw_lbk),
    format = "{cli::pb_spin} Matching loanbooks {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"
  )
  for (i in seq_along(raw_lbk)) {
    group_name <- unique(raw_lbk[[i]][["group_id"]])

    ## match data----
    if (matching_use_manual_sector_classification) {
      matched_lbk_i <- r2dii.match::match_name(
        loanbook = raw_lbk[[i]],
        abcd = abcd,
        by_sector = matching_by_sector,
        min_score = matching_min_score,
        method = matching_method,
        p = matching_p,
        overwrite = matching_overwrite,
        join_id = matching_join_id,
        sector_classification = sector_classification_system
      )
    } else {
      matched_lbk_i <- r2dii.match::match_name(
        loanbook = raw_lbk[[i]],
        abcd = abcd,
        by_sector = matching_by_sector,
        min_score = matching_min_score,
        method = matching_method,
        p = matching_p,
        overwrite = matching_overwrite,
        join_id = matching_join_id
      )
    }

    ## write matched data to file----
    matched_lbk_i %>%
      readr::write_csv(
        file = file.path(dir_matched_loanbooks, glue::glue("matched_lbk_{group_name}.csv")),
        na = ""
      )
    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  write_manifest(
    config = config,
    path = file.path(dir_matched_loanbooks, "manifest.yml"),
    prior_input_paths = c(dir_prepared_abcd)
  )
}
