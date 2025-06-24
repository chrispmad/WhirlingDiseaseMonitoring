bin_info_labs = function(bins_title){
  labs(color = 'Inspections (bin)',
       fill = 'Inspections (bin)',
       subtitle = paste0(
         "Inspection Bins: ",
         "1 ('very low', ", round(bins_title[1]), " to ", round(bins_title[2]), "), ",
         "2 ('low', ", round(bins_title[2]), " to ", round(bins_title[3]), "), ",
         "3 ('medium', ", round(bins_title[3]), " to ", round(bins_title[4]), "), ",
         "\n4 ('high', ", round(bins_title[4]), " to ", round(bins_title[5]), "), ",
         "5 ('very high', ", round(bins_title[5]), " to ", round(bins_title[6]), ")"
       )
  )
}
