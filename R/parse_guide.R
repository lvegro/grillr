#' Parse a completed guide workbook into a cell-to-dimensions mapping
#'
#' Reads an `.xlsx` guide file (produced by [create_guide_skeleton()] and
#' filled in by the analyst) and returns a tidy data frame with one row per
#' tagged cell.  Each grillr tag is expanded so that every dimension becomes
#' its own column.
#'
#' When a `metadata` list with a `dimensions` block is supplied (see
#' [read_metadata()]), the function additionally:
#' \itemize{
#'   \item Orders dimension columns to match the declared schema order.
#'   \item Warns when required dimensions are absent from the guide.
#'   \item Warns when dimensions appear in the guide but are not listed in the
#'     schema.
#'   \item Warns when cell values violate an `allowed` list.
#' }
#'
#' The `_grillr_ref` reference sheet (if present) is automatically skipped.
#'
#' @param guide_path  Path to the completed guide `.xlsx` file.
#' @param sheets      Sheet names to read.  `NULL` (default) reads all sheets
#'   except `_grillr_ref`.
#' @param skip_empty  If `TRUE` (default) cells with an empty tag
#'   (`[[grillr|]]`) are silently dropped.
#' @param metadata    Optional named list as returned by [read_metadata()].
#'   When a `dimensions` block is present it is used to order and validate
#'   the dimension columns.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{sheet}{Sheet name.}
#'     \item{address}{Cell address (e.g. `"B3"`).}
#'     \item{row, col}{Integer row and column indices.}
#'     \item{tag_raw}{The raw tag string as it appears in the guide.}
#'     \item{...}{One character column per unique dimension key.  Column order
#'       follows the schema when `metadata` is supplied; `NA` where a tag
#'       does not contain that dimension.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' meta  <- read_metadata("template.yaml")
#' guide <- parse_guide("guide.xlsx", metadata = meta)
#' guide
#' }
parse_guide <- function(guide_path,
                        sheets     = NULL,
                        skip_empty = TRUE,
                        metadata   = NULL) {
  if (!file.exists(guide_path)) {
    rlang::abort(paste0("File not found: ", guide_path), call = NULL)
  }

  raw <- tidyxl::xlsx_cells(guide_path, sheets = sheets)
  raw <- dplyr::filter(raw, !is_blank, data_type == "character")
  raw <- dplyr::filter(raw, sheet != "_grillr_ref")

  is_tag <- vapply(raw$character, is_grillr_tag, logical(1L))
  raw    <- raw[is_tag, ]

  if (nrow(raw) == 0L) {
    rlang::abort(paste0("No grillr tags found in: ", guide_path), call = NULL)
  }

  parsed <- lapply(raw$character, parse_tag)

  if (skip_empty) {
    keep   <- vapply(parsed, function(x) length(x) > 0L, logical(1L))
    raw    <- raw[keep, ]
    parsed <- parsed[keep]
    if (nrow(raw) == 0L) {
      rlang::abort(
        "All tags in the guide are empty. Fill in the dimensions first.",
        call = NULL
      )
    }
  }

  all_keys <- unique(unlist(lapply(parsed, names)))

  dim_rows <- lapply(parsed, function(dims) {
    row <- stats::setNames(
      as.list(rep(NA_character_, length(all_keys))),
      all_keys
    )
    for (k in names(dims)) row[[k]] <- dims[[k]]
    as.data.frame(row, stringsAsFactors = FALSE)
  })
  dim_tbl <- do.call(rbind, dim_rows)

  result <- cbind(
    data.frame(
      sheet   = raw$sheet,
      address = raw$address,
      row     = raw$row,
      col     = raw$col,
      tag_raw = raw$character,
      stringsAsFactors = FALSE
    ),
    dim_tbl
  )

  apply_dim_schema(result, dim_schema(metadata))
}
