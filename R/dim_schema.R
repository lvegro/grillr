# Normalise the `dimensions` field from a metadata list into a consistent
# list-of-lists, each with at least: name (chr), required (lgl), allowed (chr or NULL).
#
# The YAML block may be written in two ways:
#
#   Simple (order only):
#     dimensions: [metric, scenario, tenor]
#
#   Detailed (order + constraints):
#     dimensions:
#       - name: metric
#         required: true
#       - name: scenario
#         required: true
#         allowed: [base, stressed, adverse]
#       - name: tenor
#         required: false
#
# Both forms are normalised to the same internal structure.

dim_schema <- function(metadata) {
  dims <- metadata[["dimensions"]]
  if (is.null(dims)) return(NULL)

  # Simple form: character vector
  if (is.character(dims)) {
    return(lapply(dims, function(nm) {
      list(name = nm, required = TRUE, allowed = NULL)
    }))
  }

  # Detailed form: list of scalars or sub-lists
  lapply(dims, function(d) {
    if (is.character(d)) {
      list(name = d, required = TRUE, allowed = NULL)
    } else {
      list(
        name     = d[["name"]],
        required = isTRUE(d[["required"]] %||% TRUE),
        allowed  = d[["allowed"]]
      )
    }
  })
}


# Apply a parsed schema to a guide data frame:
#   - reorder dimension columns to match schema order
#   - warn on required dimensions absent from the guide
#   - warn on dimensions present in the guide but absent from schema
#   - warn on cell values that violate an `allowed` list
apply_dim_schema <- function(df, schema) {
  if (is.null(schema)) return(df)

  fixed_cols <- c("sheet", "address", "row", "col", "tag_raw")
  all_dim_cols <- setdiff(names(df), fixed_cols)

  schema_names <- vapply(schema, `[[`, character(1L), "name")

  # ── Warn: dimensions in guide but missing from schema ─────────────────────
  extra <- setdiff(all_dim_cols, schema_names)
  if (length(extra) > 0L) {
    rlang::warn(
      paste0("Dimensions found in guide but not listed in schema: ",
             paste(extra, collapse = ", ")),
      call = NULL
    )
  }

  # ── Warn: required dimensions absent from guide ────────────────────────────
  required_names <- vapply(
    Filter(function(d) isTRUE(d$required), schema),
    `[[`, character(1L), "name"
  )
  missing_req <- setdiff(required_names, all_dim_cols)
  if (length(missing_req) > 0L) {
    rlang::warn(
      paste0("Required dimensions missing from guide: ",
             paste(missing_req, collapse = ", ")),
      call = NULL
    )
  }

  # ── Warn: values that violate `allowed` ───────────────────────────────────
  for (d in schema) {
    if (!is.null(d$allowed) && d$name %in% names(df)) {
      bad <- setdiff(stats::na.omit(unique(df[[d$name]])), d$allowed)
      if (length(bad) > 0L) {
        rlang::warn(
          paste0("Dimension '", d$name, "': values not in allowed list: ",
                 paste(bad, collapse = ", ")),
          call = NULL
        )
      }
    }
  }

  # ── Reorder columns: fixed | schema order | any extras ────────────────────
  ordered_dims <- c(
    intersect(schema_names, names(df)),
    setdiff(all_dim_cols, schema_names)
  )
  df[, c(intersect(fixed_cols, names(df)), ordered_dims), drop = FALSE]
}
