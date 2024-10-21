f_create_year_ranges_from_lists <- function(years_list) {
  ranges_list <- list()
  
  for (years in years_list) {
    years <- as.numeric(years)
    ranges <- character(0)
    
    if (length(years) == 1) {
      ranges <- as.character(years)
    } else {
      start <- years[1]
      for (i in 2:length(years)) {
        if (years[i] != years[i - 1] + 1) {
          if (start != years[i - 1]) {
            ranges <- c(ranges, paste(start, years[i - 1], sep = "-"))
          } else {
            ranges <- c(ranges, as.character(start))
          }
          start <- years[i]
        }
      }
      
      if (start != years[length(years)]) {
        ranges <- c(ranges, paste(start, years[length(years)], sep = "-"))
      } else {
        ranges <- c(ranges, as.character(start))
      }
    }
    
    ranges_list <- append(ranges_list, list(paste(ranges, collapse = ", ")))
  }
  
  return(ranges_list)
}