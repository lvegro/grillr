#' Parse an Excel template file into a tidy cell table
#'
#' Reads every cell in one or more sheets of an `.xlsx` file using
#' [tidyxl::xlsx_cells()] and returns a tidy tibble with one row per
#' occupied cell, augmented with a `grillr_tag` column containing the
#' tag string for that cell.
#'
#' @param path    Path to the `.xlsx` template file.
#' @param sheets  Character vector of sheet names to include.  `NULL`
#'   (default) reads all sheets.
#' @param include_blank If `FALSE` (default) blank / empty cells are dropped.
#'
#' @return A [tibble][dplyr::tibble] with at least the columns:
#'   \describe{
#'     \item{sheet}{Sheet name.}
#'     \item{address}{Cell address (e.g. `"B3"`).}
#'     \item{row, col}{Numeric row and column indices.}
#'     \item{data_type}{tidyxl data-type label.}
#'     \item{value}{Cell value coerced to character.}
#'     \item{grillr_tag}{Parsable tag string produced by [make_tag()].}
#'   }
#'   All original tidyxl columns are retained.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cells <- parse_template("template.xlsx")
#' cells
#' }
parse_template <- function(path, sheets = NULL, include_blank = FALSE) {
  if (!file.exists(path)) {
    rlang::abort(paste0("File not found: ", path), call = NULL)
  }

  raw <- tidyxl::xlsx_cells(path, sheets = sheets)

  if (!include_blank) {
    raw <- dplyr::filter(raw, !is_blank)
  }

  raw <- dplyr::mutate(
    raw,
    value      = .extract_value(raw),
    grillr_tag = mapply(
      make_tag,
      sheet   = sheet,
      address = address,
      row     = row,
      col     = col,
      type    = data_type,
      value   = value,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
  )

  raw
}


# Coerce the appropriate typed value column to character based on data_type.
.extract_value <- function(cells) {
  type <- cells$data_type
  out  <- character(nrow(cells))

  out[type == "character"] <- as.character(cells$character[type == "character"])
  out[type == "numeric"]   <- as.character(cells$numeric  [type == "numeric"])
  out[type == "logical"]   <- as.character(cells$logical  [type == "logical"])
  out[type == "date"]      <- as.character(cells$date     [type == "date"])
  out[type == "error"]     <- as.character(cells$error    [type == "error"])
  out[type == "blank"]     <- NA_character_

  out
}
