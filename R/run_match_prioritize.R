run_match_prioritize <- function(config) {
  config <- load_config(config)

  # input paths for match prioritize
  dir_matched_loanbooks <- get_dir_matched_loanbooks(config)
  dir_prepared_abcd <- get_dir_prepared_abcd(config)

  # output path for match prioritize
  output_prio_diagnostics_dir <- get_dir_prioritized_loanbooks_and_diagnostics(config)

  match_prio_priority <- get_match_priority(config)

  apply_sector_split <- get_apply_sector_split(config)
  sector_split_type_select <- get_sector_split_type(config)

  # validate config values----
  assert_length(dir_prepared_abcd, 1L)
  assert_inherits(dir_prepared_abcd, "character")
  assert_dir_exists(dir_prepared_abcd, desc = "Output - prepare ABCD")
  assert_file_exists(file.path(dir_prepared_abcd, "abcd_final.csv"), desc = "ABCD final")
  assert_length(dir_matched_loanbooks, 1L)
  assert_inherits(dir_matched_loanbooks, "character")
  assert_dir_exists(dir_matched_loanbooks, desc = "Output - Matched loanbooks")
  assert_inherits(apply_sector_split, "logical")

  if (apply_sector_split) {
    assert_inherits(sector_split_type_select, "character")
  }

  if (!is.null(match_prio_priority)) {
    if (
      !inherits(match_prio_priority, "character") &
      !inherits(match_prio_priority, "formula") &
      !inherits(match_prio_priority, "function")
    ) {
      valid_types <- c(
        "a character vector",
        "a function",
        "a quosure-style lambda function"
      )
      cli::cli_abort(c(
        "x" = paste0(
          "Argument {.arg match_prio_priority} must be of one of: {.or {valid_types}}, ",
          "not {.cls {class(match_prio_priority)}}."
        ),
        "i" = "Check the {.val match_prioritize:priority} parameter set in your {.file config.yml}."
      ))
    }
  }

  # load data----
  ## load manually matched files----
  list_matched_manual <- list.files(path = dir_matched_loanbooks, pattern = "^matched_lbk_.*_manual[.]csv$")
  assert_any_file_exists(list_matched_manual, dir_matched_loanbooks, "dir_output", "manually matched loan book CSVs matching the pattern {.code ^matched_lbk_.*_manual[.]csv$}")

  matched_lbk_manual <- readr::read_csv(
    file = file.path(dir_matched_loanbooks, list_matched_manual),
    col_types = col_types_matched_manual
  ) %>%
    dplyr::group_split(.data[["group_id"]])

  ## optional: load sector split----
  if (apply_sector_split) {
    if (sector_split_type_select == "equal_weights") {
      companies_sector_split <- readr::read_csv(
        file.path(dir_prepared_abcd, "companies_sector_split.csv"),
        col_types = col_types_companies_sector_split,
        col_select = dplyr::all_of(col_select_companies_sector_split)
      )

      abcd <- readr::read_csv(
        file.path(dir_prepared_abcd, "abcd_final.csv"),
        col_select = dplyr::all_of(cols_abcd),
        col_types = col_types_abcd_final
      )
    }
  }

  # prioritize and save files----
  for (i in seq_along(matched_lbk_manual)) {
    group_name <- unique(matched_lbk_manual[[i]][["group_id"]])

    ## prioritize matched loan book----
    matched_prio_i <- matched_lbk_manual[[i]] %>%
      r2dii.match::prioritize(priority = match_prio_priority) %>%
      dplyr::mutate(group_id = .env[["group_name"]])

    # optional: apply sector split----
    if (apply_sector_split) {
      if (sector_split_type_select == "equal_weights") {

        unique_companies_pre_split <- dplyr::distinct(
          .data = matched_prio_i,
          .data[["group_id"]],
          .data[["name_abcd"]]
        )

        matched_prio_i <- matched_prio_i %>%
          apply_sector_split_to_loans(
            abcd = abcd,
            companies_sector_split = companies_sector_split
          )

        unique_companies_post_split <-
          dplyr::distinct(
            .data = matched_prio_i,
            .data[["group_id"]],
            .data[["name_abcd"]]
          )

        if (nrow(unique_companies_pre_split) != nrow(unique_companies_post_split)) {
          lost_companies_i <- unique_companies_pre_split %>%
            dplyr::anti_join(
              y = unique_companies_post_split,
              by = c("group_id", "name_abcd")
            )
          lost_companies_i %>% readr::write_csv(
            file = file.path(
              output_prio_diagnostics_dir,
              glue::glue("lost_companies_sector_split_{group_name}.csv")
            ),
            na = ""
          )
        }
      }
    }

    ## ensure that id_loan is unique across all loan books----
    matched_prio_i <- matched_prio_i %>%
      dplyr::mutate(
        id_loan = paste(.data[["id_loan"]], .data[["group_id"]], sep = "_")
      )

    ## write matched prioritized loan book to file----
    matched_prio_i %>%
      readr::write_csv(
        file = file.path(output_prio_diagnostics_dir, glue::glue("matched_prio_{group_name}.csv")),
        na = ""
      )
  }
}
