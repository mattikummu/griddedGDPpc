
f_Plot_errorMap<-function(sf_in,column_in,breaks_in){
  
  pal <-  scico(9, begin = 0.1, end = 0.9,direction = -1, palette = "vik")
  
  plt_subnatMigr <- tm_shape(sf_in, projection = "+proj=robin") +
    tm_fill(col = column_in,
            palette = pal,
            breaks = breaks_in,
            midpoint = 0,
            contrast = c(0, 0.7),
            colorNA = 'white',
            lwd=0,
            legend.is.portrait = FALSE)+
    tm_shape(sf_adm0, projection = "+proj=robin") +
    tm_borders(col = "black",
               lwd = 0.15)+
    tm_layout(#main.title = "Origin of data",
      main.title.position = "center",
      legend.outside = TRUE,
      legend.outside.position = "bottom",
      legend.text.size =.25,
      legend.title.size = .75,
      legend.width = 0.6,
      #legend.height = -10, 
      frame = FALSE)
}
