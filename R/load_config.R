load_config <- function(config) {
  if (is.character(config)) return(config::get(file = config))
  config
}

get_dir_input <- function(params) {
  params[["directories"]][["dir_input"]]
}

get_dir_loanbooks <- function(params) {
  file.path(
    params[["directories"]][["dir_input"]],
    "loanbooks"
  )
}

get_filename_abcd <- function(params) {
  params[["file_names"]][["filename_abcd"]]
}

get_sheet_abcd <- function(params) {
  params[["file_names"]][["sheet_abcd"]]
}

get_dir_prepared_abcd <- function(params) {
  params[["directories"]][["dir_prepared_abcd"]]
}

get_dir_matched_loanbooks <- function(params) {
  params[["directories"]][["dir_matched_loanbooks"]]
}

get_dir_prioritized_loanbooks_and_diagnostics <- function(params) {
  params[["directories"]][["dir_prioritized_loanbooks_and_diagnostics"]]
}

get_dir_analysis <- function(params) {
  params[["directories"]][["dir_analysis"]]
}

get_filename_scenario_tms <- function(params) {
  params[["file_names"]][["filename_scenario_tms"]]
}

get_filename_scenario_sda <- function(params) {
  params[["file_names"]][["filename_scenario_sda"]]
}

get_start_year <- function(params) {
  params[["project_parameters"]][["start_year"]]
}

get_time_frame <- function(params) {
  params[["project_parameters"]][["time_frame"]]
}

get_remove_inactive_companies <- function(params) {
  params[["prepare_abcd"]][["remove_inactive_companies"]]
}

get_scenario_source <- function(params) {
  params[["project_parameters"]][["scenario_source"]]
}

get_scenario_select <- function(params) {
  params[["project_parameters"]][["scenario_select"]]
}

get_region_select <- function(params) {
  params[["project_parameters"]][["region_select"]]
}

get_by_group <- function(params) {
  params[["project_parameters"]][["by_group"]]
}

get_match_priority <- function(params) {
  params[["match_prioritize"]][["priority"]]
}

get_match_by_sector <- function(params) {
  params[["matching"]][["params_match_name"]][["by_sector"]]
}

get_match_min_score <- function(params) {
  params[["matching"]][["params_match_name"]][["min_score"]]
}

get_match_method <- function(params) {
  params[["matching"]][["params_match_name"]][["method"]]
}

get_match_p <- function(params) {
  params[["matching"]][["params_match_name"]][["p"]]
}

get_match_overwrite <- function(params) {
  params[["matching"]][["params_match_name"]][["overwrite"]]
}

get_match_join_id <- function(params) {
  params[["matching"]][["params_match_name"]][["join_id"]]
}

get_match_plot_width <- function(params) {
  params[["match_success_rate"]][["plot_width"]]
}

get_match_plot_height <- function(params) {
  params[["match_success_rate"]][["plot_height"]]
}

get_match_plot_units <- function(params) {
  params[["match_success_rate"]][["plot_units"]]
}

get_match_plot_resolution <- function(params) {
  params[["match_success_rate"]][["plot_resolution"]]
}

get_apply_sector_split <- function(params) {
  params[["sector_split"]][["apply_sector_split"]]
}

get_sector_split_type <- function(params) {
  params[["sector_split"]][["sector_split_type"]]
}

get_use_manual_sector_classification <- function(params) {
  params[["matching"]][["manual_sector_classification"]][["use_manual_sector_classification"]]
}

get_filename_manual_sector_classification <- function(params) {
  params[["matching"]][["manual_sector_classification"]][["filename_manual_sector_classification"]]
}

get_path_manual_sector_classification <- function(params) {
  file.path(
    get_dir_input(params),
    get_filename_manual_sector_classification(params)
  )
}

get_path_abcd <- function(config) {
  file.path(
    get_dir_input(config),
    get_filename_abcd(config)
  )
}

get_path_sector_split <- function(config) {
  file.path(
    get_dir_input(config),
    config[["sector_split"]][["filename_split_company_id"]]
  )
}

get_path_advanced_company_indicators <- function(config) {
  file.path(
    get_dir_input(config),
    config[["sector_split"]][["filename_advanced_company_indicators"]]
  )
}

get_sheet_advanced_company_indicators <- function(config) {
  config[["sector_split"]][["sheet_advanced_company_indicators"]]
}

get_path_scenario_tms <- function(config) {
  file.path(
    get_dir_input(config),
    get_filename_scenario_tms(config)
  )
}

get_path_scenario_sda <- function(config) {
  file.path(
    get_dir_input(config),
    get_filename_scenario_sda(config)
  )
}
