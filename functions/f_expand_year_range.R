f_expand_year_range <- function(year_range) {
  ranges <- strsplit(year_range, ", ")
  expanded_years <- character(0)
  
  for (range in ranges) {
    range_parts <- strsplit(range, "-")
    if (length(range_parts[[1]]) == 2) {
      start <- as.numeric(range_parts[[1]][1])
      end <- as.numeric(range_parts[[1]][2])
      expanded_years <- c(expanded_years, as.character(start:end))
    } else {
      expanded_years <- c(expanded_years, as.character(range_parts[[1]]))
    }
  }
  
  return(expanded_years)
}

