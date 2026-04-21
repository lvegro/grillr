#' Build a guide workbook from parsed template cells
#'
#' Takes the cell table produced by [parse_template()] and writes an `.xlsx`
#' workbook where each originally-occupied cell is replaced by its grillr tag
#' string.  All other cells are left blank.  The resulting file acts as a
#' *guide*: anyone reading it can determine exactly what should appear at each
#' position in a conforming data file.
#'
#' @param cells  A data frame / tibble as returned by [parse_template()],
#'   containing at least the columns `sheet`, `row`, `col`, and `grillr_tag`.
#' @param output_path  Path for the output `.xlsx` file.  If `NULL` a
#'   temporary file is created and its path is returned invisibly.
#' @param overwrite  If `TRUE` an existing file at `output_path` is
#'   overwritten.  Default `FALSE`.
#'
#' @return The path to the written file, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' cells <- parse_template("template.xlsx")
#' build_guide(cells, "guide.xlsx", overwrite = TRUE)
#' }
build_guide <- function(cells, output_path = NULL, overwrite = FALSE) {
  required_cols <- c("sheet", "row", "col", "grillr_tag")
  missing_cols  <- setdiff(required_cols, names(cells))
  if (length(missing_cols) > 0L) {
    rlang::abort(
      paste0("Missing required columns: ", paste(missing_cols, collapse = ", ")),
      call = NULL
    )
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

  wb     <- openxlsx::createWorkbook()
  sheets <- unique(cells$sheet)

  for (sh in sheets) {
    openxlsx::addWorksheet(wb, sh)
    sh_cells <- cells[cells$sheet == sh, ]

    for (i in seq_len(nrow(sh_cells))) {
      openxlsx::writeData(
        wb,
        sheet    = sh,
        x        = sh_cells$grillr_tag[[i]],
        startRow = sh_cells$row[[i]],
        startCol = sh_cells$col[[i]],
        colNames = FALSE
      )
    }
  }

  openxlsx::saveWorkbook(wb, output_path, overwrite = overwrite)
  invisible(output_path)
}
