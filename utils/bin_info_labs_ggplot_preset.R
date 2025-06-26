bin_info_labs = function(bins_title,legend_title = NA){
  if(is.na(legend_title)) legend_title = 'Inspections'
  labs(color = paste0(legend_title,' (bin)'),
       fill = paste0(legend_title,' (bin)'),
       subtitle = paste0(
         paste0(legend_title,' Bins: '),
         "1 ('very low', ", round(bins_title[1]), " to ", round(bins_title[2]), "), ",
         "2 ('low', ", round(bins_title[2]), " to ", round(bins_title[3]), "), ",
         "3 ('medium', ", round(bins_title[3]), " to ", round(bins_title[4]), "), ",
         "\n4 ('high', ", round(bins_title[4]), " to ", round(bins_title[5]), "), ",
         "5 ('very high', ", round(bins_title[5]), " to ", round(bins_title[6]), ")"
       )
  )
}
