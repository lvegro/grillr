#' grillr: Parse and Extract Data from Excel Templates
#'
#' grillr reads an Excel template file with [tidyxl::xlsx_cells()] and
#' produces a *guide* file — a copy of the workbook where every occupied cell
#' is replaced by a parsable tag string that encodes the cell's sheet, address,
#' data type, and original value.  Downstream code can use the guide to locate
#' and extract matching data from files that share the same layout.
#'
#' ## Core workflow
#'
#' ```r
#' # 1. Parse the raw cells from the template
#' cells <- parse_template("template.xlsx")
#'
#' # 2. Write a guide workbook with tag strings in each cell
#' build_guide(cells, "guide.xlsx")
#'
#' # 3. Read a tag back into its components
#' parse_tag("[[grillr|sheet=Sheet1;address=B3;row=3;col=2;type=numeric;value=42]]")
#' ```
#'
#' @keywords internal
"_PACKAGE"
