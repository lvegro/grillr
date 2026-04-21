#' Extract tagged values from a data file using a guide mapping
#'
#' Reads an `.xlsx` data file with [tidyxl::xlsx_cells()], inner-joins it with
#' the cell-to-dimensions mapping produced by [parse_guide()], and returns a
#' long tidy tibble — one row per tagged cell — with the observed value and all
#' functional dimensions as columns.
#'
#' @param data_path  Path to the `.xlsx` data file that follows the same
#'   layout as the template the guide was built from.
#' @param guide      A data frame as returned by [parse_guide()], containing
#'   at least `sheet`, `row`, `col`, and one or more dimension columns.
#' @param sheets     Sheet names to read from the data file.  `NULL` (default)
#'   reads all sheets present in `guide`.
#' @param metadata   An optional named list (e.g. from [read_metadata()]) whose
#'   elements are appended as constant columns to every row of the result.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{sheet, address, row, col}{Cell position from the data file.}
#'     \item{value}{Cell value coerced to character.}
#'     \item{...}{All dimension columns carried over from `guide`.}
#'     \item{...}{Any fields supplied via `metadata`.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' guide  <- parse_guide("guide.xlsx")
#' result <- extract_data("q1_data.xlsx", guide)
#' result
#'
#' # With metadata
#' meta   <- read_metadata("template.yaml")
#' result <- extract_data("q1_data.xlsx", guide, metadata = meta)
#' }
extract_data <- function(data_path, guide, sheets = NULL, metadata = NULL) {
  if (!file.exists(data_path)) {
    rlang::abort(paste0("File not found: ", data_path), call = NULL)
  }

  required <- c("sheet", "row", "col")
  missing  <- setdiff(required, names(guide))
  if (length(missing) > 0L) {
    rlang::abort(
      paste0("guide is missing required columns: ", paste(missing, collapse = ", ")),
      call = NULL
    )
  }

  # Read only the sheets that appear in the guide (unless overridden)
  target_sheets <- sheets %||% unique(guide$sheet)

  raw <- tidyxl::xlsx_cells(data_path, sheets = target_sheets)
  raw <- dplyr::filter(raw, !is_blank)

  raw <- dplyr::mutate(raw, value = .extract_value(raw))

  # Keep only the positional + value columns for the join
  data_cells <- data.frame(
    sheet   = raw$sheet,
    address = raw$address,
    row     = raw$row,
    col     = raw$col,
    value   = raw$value,
    stringsAsFactors = FALSE
  )

  # Inner join: only positions present in the guide
  result <- merge(
    data_cells,
    guide,
    by    = c("sheet", "row", "col"),
    all.x = FALSE,
    all.y = FALSE,
    suffixes = c("", "_guide")
  )

  # Prefer address from data file; drop guide's address if it was joined
  if ("address_guide" %in% names(result)) {
    result[["address_guide"]] <- NULL
  }

  # Attach metadata as constant columns
  if (!is.null(metadata)) {
    for (nm in names(metadata)) {
      result[[nm]] <- metadata[[nm]]
    }
  }

  result
}


# NULL-coalescing operator (like rlang::`%||%` but avoid importing extra)
`%||%` <- function(x, y) if (is.null(x)) y else x
