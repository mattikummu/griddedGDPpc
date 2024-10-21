f_create_year_ranges <- function(years) {
  if (is.na(years)) {
    return(NA)
  }
  
  years <- as.numeric(strsplit(years, ", ")[[1]])
  ranges <- character(0)
  
  if (length(years) == 1) {
    return(as.character(years))
  }
  
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
  
  return(paste(ranges, collapse = ", "))
}