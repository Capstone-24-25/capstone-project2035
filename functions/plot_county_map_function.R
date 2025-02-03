# Function that takes in the file name and then filters for the counties and jobs that are needed

plot_county_map <- function (data, title = '', save_title = 'basic_map') {
  # values is an 3 x 2 data-frame where one column is labeled "value"
  # and the other column is labeled "subregion" and consist of values 
  # 'ventura', 'san luis obispo', and 'santa barbara'
  # save_title is a string of how the title will be saved
  
  # initiallizing the counties data frame
  selected_counties = map_data('county')
  selected_counties <- selected_counties[selected_counties$subregion %in% c('san luis obispo', 'santa barbara', 'ventura'),]
  
  # joining values onto the selected_counties dataframe
  selected_counties <- left_join(data[c('value', 'subregion')], selected_counties, by = 'subregion')
  
  # creating a plot of the three counties based on their values
  ggplot(selected_counties, aes(long, lat, group = group, fill = value)) + 
    geom_polygon(colour = "gray75") +
    scale_fill_gradient(low = "darkseagreen1", high = "darkseagreen4") +
    coord_equal() +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#ddebe4", color = NA),
      panel.background = element_rect(fill = "#ddebe1", color = NA)      # Change panel background
    ) +
    ggtitle(title)

  ggsave(sprintf("./maps_png/%s.png", save_title))

}
