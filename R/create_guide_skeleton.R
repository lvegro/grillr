#' Create a guide skeleton workbook from an Excel template
#'
#' Reads an `.xlsx` template with [tidyxl::xlsx_cells()] and writes a new
#' workbook where every occupied cell is replaced by an **empty grillr tag**
#' (`[[grillr|]]`).  A hidden reference sheet (`_grillr_ref`) is appended
#' with the original cell values so the analyst knows what to tag.
#'
#' The analyst opens the output file and fills in the functional dimensions
#' for each cell, e.g. replacing `[[grillr|]]` with
#' `[[grillr|metric=regulatory_capital;scenario=stressed;tenor=t+3]]`.
#' The completed file is then used with [parse_guide()].
#'
#' @param template_path Path to the source `.xlsx` template.
#' @param output_path   Path for the guide `.xlsx` to write.  If `NULL` a
#'   temporary file is created and its path returned invisibly.
#' @param sheets        Character vector of sheet names to process.  `NULL`
#'   (default) processes all sheets.
#' @param overwrite     Overwrite `output_path` if it already exists.
#'
#' @return The path to the written guide file, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' create_guide_skeleton("template.xlsx", "guide.xlsx")
#' # Edit guide.xlsx, filling in [[grillr|dim1=...;dim2=...]] tags
#' }
create_guide_skeleton <- function(template_path,
                                  output_path = NULL,
                                  sheets      = NULL,
                                  overwrite   = FALSE) {
  if (!file.exists(template_path)) {
    rlang::abort(paste0("File not found: ", template_path), call = NULL)
  }
  if (is.null(output_path)) {
    output_path <- tempfile(fileext = ".xlsx")
  }
  if (file.exists(output_path) && !overwrite) {
    rlang::abort(
      paste0("File already exists (use overwrite = TRUE): ", output_path),
      call = NULL
    )
  }

  raw <- tidyxl::xlsx_cells(template_path, sheets = sheets)
  raw <- dplyr::filter(raw, !is_blank)

  wb         <- openxlsx::createWorkbook()
  sheet_list <- unique(raw$sheet)

  for (sh in sheet_list) {
    openxlsx::addWorksheet(wb, sh)
    sh_cells <- raw[raw$sheet == sh, ]

    for (i in seq_len(nrow(sh_cells))) {
      openxlsx::writeData(
        wb,
        sheet    = sh,
        x        = TAG_PREFIX %+% TAG_SUFFIX,   # empty skeleton tag
        startRow = sh_cells$row[[i]],
        startCol = sh_cells$col[[i]],
        colNames = FALSE
      )
    }
  }

  # Reference sheet: address + original character value of each cell
  ref <- dplyr::mutate(
    raw,
    original_value = .extract_value(raw)
  )
  ref <- dplyr::select(ref, sheet, address, row, col, data_type, original_value)

  openxlsx::addWorksheet(wb, "_grillr_ref", visible = FALSE)
  openxlsx::writeData(wb, "_grillr_ref", ref, colNames = TRUE)

  openxlsx::saveWorkbook(wb, output_path, overwrite = overwrite)
  invisible(output_path)
}


# Coerce the appropriate typed value column to character.
.extract_value <- function(cells) {
  type <- cells$data_type
  out  <- character(nrow(cells))

  out[type == "character"] <- as.character(cells$character[type == "character"])
  out[type == "numeric"]   <- as.character(cells$numeric  [type == "numeric"])
  out[type == "logical"]   <- as.character(cells$logical  [type == "logical"])
  out[type == "date"]      <- as.character(cells$date     [type == "date"])
  out[type == "error"]     <- as.character(cells$error    [type == "error"])

  out
}


# Simple string concatenation operator used internally.
`%+%` <- function(a, b) paste0(a, b)
