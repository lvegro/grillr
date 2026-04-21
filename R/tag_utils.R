# Tag format:
#   [[grillr|key1=val1;key2=val2;...]]
#
# Keys are user-defined functional/semantic dimensions, e.g.
#   metric=regulatory_capital;scenario=stressed;tenor=t+3
#
# Cell position is NOT stored inside the tag — it is derived from where
# the tag string sits in the guide workbook.

TAG_PREFIX <- "[[grillr|"
TAG_SUFFIX <- "]]"
TAG_SEP    <- ";"
KV_SEP     <- "="


#' Build a grillr tag string from named functional dimensions
#'
#' @param ... Named character scalars describing the data at a cell, e.g.
#'   `metric = "regulatory_capital"`, `scenario = "stressed"`,
#'   `tenor = "t+3"`.  All arguments must be named and character.
#'
#' @return A single character tag string.
#' @export
#'
#' @examples
#' make_tag(metric = "regulatory_capital", scenario = "stressed", tenor = "t+3")
#' make_tag(item = "total_rwa")
make_tag <- function(...) {
  dims <- list(...)
  if (length(dims) == 0L) {
    return(paste0(TAG_PREFIX, TAG_SUFFIX))
  }

  nms <- names(dims)
  if (is.null(nms) || any(nms == "")) {
    rlang::abort("All arguments to make_tag() must be named.", call = NULL)
  }

  vals <- vapply(dims, function(v) {
    v <- as.character(v)
    v <- gsub(KV_SEP, "%3D", v, fixed = TRUE)
    v <- gsub(TAG_SEP, "%3B", v, fixed = TRUE)
    v
  }, character(1L))

  pairs <- paste(nms, vals, sep = KV_SEP)
  paste0(TAG_PREFIX, paste(pairs, collapse = TAG_SEP), TAG_SUFFIX)
}


#' Parse a grillr tag string into its named dimensions
#'
#' @param tag A character scalar produced by [make_tag()] or authored
#'   manually in a guide workbook.
#'
#' @return A named list of character scalars, one element per dimension.
#'   An empty tag (`[[grillr|]]`) returns an empty list.
#' @export
#'
#' @examples
#' tag <- make_tag(metric = "regulatory_capital", scenario = "stressed")
#' parse_tag(tag)
parse_tag <- function(tag) {
  if (!is_grillr_tag(tag)) {
    rlang::abort(paste0("Not a valid grillr tag: ", tag), call = NULL)
  }

  inner <- substr(tag, nchar(TAG_PREFIX) + 1L, nchar(tag) - nchar(TAG_SUFFIX))
  if (nchar(trimws(inner)) == 0L) return(list())

  pairs <- strsplit(inner, TAG_SEP, fixed = TRUE)[[1L]]

  result <- list()
  for (pair in pairs) {
    sep_pos <- regexpr(KV_SEP, pair, fixed = TRUE)[[1L]]
    key     <- substr(pair, 1L, sep_pos - 1L)
    val     <- substr(pair, sep_pos + 1L, nchar(pair))
    val     <- gsub("%3B", TAG_SEP, val, fixed = TRUE)
    val     <- gsub("%3D", KV_SEP,  val, fixed = TRUE)
    result[[key]] <- val
  }

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
    !is.na(x) &&
    startsWith(x, TAG_PREFIX) &&
    endsWith(x, TAG_SUFFIX)
}
