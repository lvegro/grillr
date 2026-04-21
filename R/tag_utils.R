# Tag format:
#   [[grillr|key=value;key=value;...]]
#
# Mandatory keys (always present, in this order):
#   sheet, address, row, col, type, value
#
# `value` is always stored as a string; NA is encoded as the literal "NA".

TAG_PREFIX <- "[[grillr|"
TAG_SUFFIX <- "]]"
TAG_SEP    <- ";"
KV_SEP     <- "="

#' Build a grillr tag string from cell metadata
#'
#' @param sheet  Sheet name (character scalar).
#' @param address Cell address, e.g. `"B3"` (character scalar).
#' @param row    Row index (integer).
#' @param col    Column index (integer).
#' @param type   Data type label: `"character"`, `"numeric"`, `"logical"`,
#'   `"date"`, or `"error"`.
#' @param value  Cell value coerced to character, or `NA`.
#'
#' @return A single character string.
#' @export
#'
#' @examples
#' make_tag("Sheet1", "B3", 3L, 2L, "numeric", "42")
make_tag <- function(sheet, address, row, col, type, value) {
  value_str <- if (is.na(value)) "NA" else as.character(value)

  # Escape any embedded TAG_SEP or KV_SEP inside values to keep the format
  # unambiguous.  We use URL-style percent-encoding for the two reserved chars.
  value_str <- gsub(KV_SEP, "%3D", value_str, fixed = TRUE)
  value_str <- gsub(TAG_SEP, "%3B", value_str, fixed = TRUE)

  pairs <- paste(
    c("sheet", "address", "row", "col", "type", "value"),
    c(sheet, address, as.integer(row), as.integer(col), type, value_str),
    sep = KV_SEP
  )

  paste0(TAG_PREFIX, paste(pairs, collapse = TAG_SEP), TAG_SUFFIX)
}


#' Parse a grillr tag string back into its components
#'
#' @param tag A character scalar produced by [make_tag()].
#'
#' @return A named list with elements `sheet`, `address`, `row` (integer),
#'   `col` (integer), `type`, and `value` (character, or `NA`).
#' @export
#'
#' @examples
#' tag <- make_tag("Sheet1", "B3", 3L, 2L, "numeric", "42")
#' parse_tag(tag)
parse_tag <- function(tag) {
  if (!is_grillr_tag(tag)) {
    rlang::abort(
      paste0("Not a valid grillr tag: ", tag),
      call = NULL
    )
  }

  inner <- substr(tag, nchar(TAG_PREFIX) + 1L, nchar(tag) - nchar(TAG_SUFFIX))
  pairs <- strsplit(inner, TAG_SEP, fixed = TRUE)[[1L]]

  result <- list()
  for (pair in pairs) {
    kv    <- strsplit(pair, KV_SEP, fixed = TRUE)[[1L]]
    key   <- kv[[1L]]
    value <- paste(kv[-1L], collapse = KV_SEP)  # re-join if value had KV_SEP
    value <- gsub("%3B", TAG_SEP, value, fixed = TRUE)
    value <- gsub("%3D", KV_SEP,  value, fixed = TRUE)
    result[[key]] <- value
  }

  result$row <- as.integer(result$row)
  result$col <- as.integer(result$col)
  if (identical(result$value, "NA")) result$value <- NA_character_

  result
}


#' Test whether a string is a grillr tag
#'
#' @param x Character scalar.
#' @return `TRUE` / `FALSE`.
#' @keywords internal
is_grillr_tag <- function(x) {
  is.character(x) &&
    length(x) == 1L &&
    startsWith(x, TAG_PREFIX) &&
    endsWith(x, TAG_SUFFIX)
}
