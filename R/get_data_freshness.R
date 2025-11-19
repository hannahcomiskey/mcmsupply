#' Harvest DHS Dataset Availability
#'
#' Scrapes the DHS Program website to retrieve information about the
#' newest available datasets.
#'
#' @param url Character string. URL of the DHS available datasets page.
#'   Default is "https://dhsprogram.com/data/available-datasets.cfm".
#' @param survey_column Character string. Name of the column containing
#'   survey information. Default is 'Survey'.
#' @param standard_dhs Logical. If TRUE, filters results to include only
#'   Standard DHS surveys. Default is TRUE.
#'
#' @return A tibble containing all tables from the DHS program URL, with
#'   added Country and Year columns extracted from the survey column.
#'
#' @examples
#' \dontrun{
#' dhs_data <- harvest_dhs()
#' }
#'
#' @export
harvest_dhs <- function(url = "https://dhsprogram.com/data/available-datasets.cfm",
                        survey_column = 'Survey',
                        standard_dhs = TRUE) {
  webpage <- rvest::read_html(url)
  dhs <- webpage |>
    rvest::html_element('body') |>
    rvest::html_elements('table') |>
    rvest::html_table() |>
    purrr::reduce(.f = dplyr::bind_rows)
  if(standard_dhs) {
    dhs <- dplyr::filter(dhs, Type == 'Standard DHS')
  }
  survey <- dhs[[survey_column]]
  country <- stringr::str_trim(stringr::str_extract(survey, '[^0-9]+'))
  year <- as.numeric(stringr::str_extract(survey, '\\d\\d\\d\\d'))
  dhs |>
    dplyr::mutate(Country = country,
                  Year = year,
                  .after = !!survey_column)
}

#' Check Data Freshness Against DHS Reference
#'
#' Compares local dataset against DHS reference data to determine if newer
#' surveys are available or exist but are not yet publicly available.
#'
#' @param local Data frame. Local dataset containing Country and Year columns.
#'   If NULL, uses `national_FPsource_data` from the mcmsupply package.
#'   Default is NULL.
#' @param reference Data frame. Reference dataset containing Country, Year,
#'   and survey status columns. If NULL, calls `harvest_dhs()` to retrieve
#'   current DHS data. Default is NULL.
#' @param survey_column Character string. Name of the column in reference data
#'   indicating survey dataset availability. Default is 'Survey Datasets'.
#' @param data_available_value Character string. Value in survey_column that
#'   indicates data is publicly available. Default is 'Data Available'.
#'
#' @return A data frame with columns Country, Year.package, Year.most_recent,
#'   Year.most_recent_available, and Status. Status indicates whether the
#'   local data is 'Up To Date', 'More Recent Available' (publicly available),
#'   or 'More Recent Exists' (exists but not yet publicly available).
#'
#' @examples
#' \dontrun{
#' freshness <- check_data_freshness()
#' }
#'
#' @export
check_data_freshness <- function(local = NULL,
                                 reference = NULL,
                                 survey_column = 'Survey Datasets',
                                 data_available_value = 'Data Available') {
  if (is.null(local)) {
    load('data/national_FPsource_data.rda')
    local <- national_FPsource_data |>
      dplyr::ungroup() |>
      dplyr::mutate(Year = floor(average_year))
  }
  local <- local |>
    dplyr::distinct(Country, Year) |>
    dplyr::slice_max(Year, by = Country)
  if (is.null(reference)) {
    reference <- harvest_dhs(standard_dhs=TRUE) |>
      dplyr::distinct(Country, Year, !!dplyr::sym(survey_column))
  }
  stopifnot(all(c('Country', 'Year') %in% colnames(local)))
  stopifnot(all(c('Country', 'Year', survey_column) %in% colnames(reference)))
  most_recent_available <- reference |>
    dplyr::filter(!!dplyr::sym(survey_column) == !!data_available_value) |>
    dplyr::slice_max(Year, by = Country) |>
    dplyr::select(Country, Year)
  most_recent <- reference |>
    dplyr::slice_max(Year, by = Country) |>
    dplyr::select(Country, Year)
  if (length(setdiff(local$Country, most_recent$Country)) > 0) {
    warning('Not all countries in the local data were found in the reference data.')
  }
  matched <- local |>
    dplyr::left_join(
      most_recent,
      by = dplyr::join_by(Country),
      suffix = c('.package', '.most_recent')
    ) |>
    dplyr::left_join(
      most_recent_available |> dplyr::rename(Year.most_recent_available = Year),
      by = dplyr::join_by(Country),
      suffix = c('.x', '.most_recent_available')
    ) |>
    dplyr::mutate(Status = factor(
      dplyr::case_when(
        Year.package >= Year.most_recent ~ 'Up To Date',
        Year.package < Year.most_recent_available ~  'More Recent Available',
        Year.package < Year.most_recent ~ 'More Recent Exists',
        TRUE ~ NA_character_
      ),
      levels = c('More Recent Available', 'More Recent Exists', 'Up To Date')
    )) |>
    dplyr::arrange(Status)
}
