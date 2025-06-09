library(xlsx)
library(tidyverse)

data_path <- "//sfp.idir.bcgov/s140/S40203/WFC AEB/General/2 Stewardship Mgt Climate Change/Drought/Communications/Periodicity tables/"
wb <- loadWorkbook(paste0(data_path, "KBR periodicty tables_updated May 2025.xlsx"))
sheet <- getSheets(wb)[["Creston Water Management Precin"]]
rows <- getRows(sheet)
cells <- unlist(lapply(rows, getCells), recursive = FALSE)

# Now get proper Excel-style addresses
cell_addresses <- names(cells)
cell_texts <- sapply(cells, getCellValue)
styles <- sapply(cells, getCellStyle)

cellColor <- function(style) {
  fg <- tryCatch(style$getFillForegroundXSSFColor(), error = function(e) NULL)
  rgb <- tryCatch(fg$getRgb(), error = function(e) NULL)
  if (!is.null(rgb)) paste(rgb, collapse = "") else NA
}

cell_colors <- sapply(styles, cellColor)

cell_info <- data.frame(
  address = cell_addresses,
  text = unname(cell_texts),
  color = unname(cell_colors),
  stringsAsFactors = FALSE
)





excel_col_to_num <- function(col_letters) {
  sapply(col_letters, function(cl) {
    sum((utf8ToInt(strsplit(cl, "")[[1]]) - 64) * 26^(rev(seq_along(cl) - 1)))
  })
}

cell_info <- cell_info |> 
  mutate(
    col_letter = gsub("[0-9]", "", address),
    row = as.numeric(gsub("[A-Z]", "", address)),
    col = excel_col_to_num(col_letter)
  )

month_lookup <- setNames(month.name, 2:13)

cell_info <- cell_info |> 
  filter(col %in% 2:13, row >= 13 & row <= 28) |> 
  mutate(
    Month = month_lookup[as.character(col)],
    Presence = !is.na(color) & color != "" & color != "FFFFFF"
  )


# Get the text entries from column 1 (Species and Life Stage column)
row_labels <- cell_info |> 
  filter(col == 1) |> 
  select(row, text) |> 
  rename(label = text)

# Forward-fill species names
row_labels <- row_labels |> 
  mutate(is_species = grepl("Trout", label)) |> 
  mutate(Species = ifelse(is_species, label, NA)) |> 
  fill(Species, .direction = "down") |> 
  filter(!is_species) |> 
  rename(LifeStage = label)

# Merge life stage + species into main data
final_data <- cell_info |> 
  filter(col %in% 2:13) |> 
  left_join(row_labels, by = "row")|> 
  select(Species, LifeStage, Month, Presence, color)
