read_abcd_raw <- function(path, sheet) {
  abcd_names <- readxl::read_xlsx(
    path = file.path(path),
    sheet = sheet,
    col_names = TRUE,
    n_max = 0
  )
  if (
    "lei" %in% names(abcd_names)
  ) {
    abcd <-
      readxl::read_xlsx(
        path = file.path(path),
        sheet = sheet,
        col_types = c(
          company_id = "numeric",
          name_company = "text",
          lei = "text",
          is_ultimate_owner = "logical",
          sector = "text",
          technology = "text",
          plant_location = "text",
          year = "numeric",
          production = "numeric",
          production_unit = "text",
          emission_factor = "numeric",
          emission_factor_unit = "text",
          ald_timestamp = "skip"
        )
      )
  } else if (
    "LEI" %in% names(abcd_names)
  ) {
    abcd <-
      readxl::read_xlsx(
        path = file.path(path),
        sheet = sheet,
        col_types = c(
          company_id = "numeric",
          name_company = "text",
          LEI = "text",
          is_ultimate_owner = "logical",
          sector = "text",
          technology = "text",
          plant_location = "text",
          year = "numeric",
          production = "numeric",
          production_unit = "text",
          emission_factor = "numeric",
          emission_factor_unit = "text",
          ald_timestamp = "skip"
        )
      )
    abcd <- dplyr::rename(abcd, lei = "LEI")
  } else {
    stop("No column 'lei' or 'LEI' found in the data.")
  }

  dplyr::mutate(abcd, year = as.integer(.data[["year"]]))
}
