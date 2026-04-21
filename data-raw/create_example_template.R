# Run this script once to (re)generate inst/extdata/example_template.xlsx.
# Requires: openxlsx

library(openxlsx)

wb <- createWorkbook()

# --- Sheet 1: Survey header ---------------------------------------------------
addWorksheet(wb, "Header")
writeData(wb, "Header", data.frame(
  A = c("Project", "Responsible", "Date"),
  B = c("My Project", "Jane Doe",   as.character(Sys.Date()))
), startRow = 1, startCol = 1, colNames = FALSE)

# --- Sheet 2: Data table ------------------------------------------------------
addWorksheet(wb, "Data")
writeData(wb, "Data", data.frame(
  ID    = 1:3,
  Name  = c("Alice", "Bob", "Charlie"),
  Score = c(88.5, 72.0, 95.3)
), startRow = 1, startCol = 1, colNames = TRUE)

# --- Save ---------------------------------------------------------------------
out <- here::here("inst", "extdata", "example_template.xlsx")
saveWorkbook(wb, out, overwrite = TRUE)
message("Written: ", out)
