library(readxl)

# This data file comes from a Microsoft Teams channel (specifically, the Whirling Disease one)
# I have manually downloaded that excel file and placed it in this R project's data folder.

the_filepath = list.files(path = 'data', pattern = "Whirling_Disease_top 100.*",
                          full.names = T)

d = read_excel(path = the_filepath, sheet = "Top 100 priority list")

# Just keep waterbodies that have been flagged as a priority in columns F - L,
# or identified by FN partners in columns N and O.
d |>
  dplyr::mutate(dplyr::across(dplyr::ends_with("(1-20)"),\(x) as.character(x))) |>
  dplyr::mutate()
  tidyr::pivot_longer(cols = c(dplyr::ends_with("(1-20)"),"Identified by FN partners","Potential WD Sampling Overlap with Existing Projects"))

d_f = d |>
  dplyr::filter(`Identified by FN partners` |
                  !is.na(`WLRS AEB Provincial Fisheries Priority Ranking (1-20)`) |
                  !is.na(`WLRS KBR Regional Fisheries Priority Ranking (1-20)`) |
                  !is.na(`WLRS Provincial Water Authorizations Priority Ranking (all regions, incl KBR) (1-20)`) |
                  !is.na(`WLRS KBR Water Authorizations Priority Ranking (1-20)`) |
                  !is.na(`MOTT Priority Ranking (1-20)`) |
                  !is.na(`BC Parks`))

openxlsx::write.xlsx(x = d_f, file = "output/Whirling_Disease_top_flagged_waterbody_list.xlsx")
