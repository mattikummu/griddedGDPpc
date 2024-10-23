f_raster2map_v2_bbox <- function(r_in,shape_in, colPalette, plotTitle,bboxUsed) {
  
  
  tmapMap <- tm_shape(r_in,  bbox= bboxUsed) +
    tm_raster(palette = colPalette,
              #breaks = fishing.breaks,
              title = plotTitle,
              #n = 20,
              style = "fixed",
              breaks = gdp_breaks_2,
              colorNA = NULL,
              legend.is.portrait = FALSE) +
    tm_shape(shape_in, bbox= bboxUsed) +
    tm_borders(col="white",lwd = 0.2)+
    tm_layout(legend.bg.color = TRUE,
              legend.outside.position = "bottom",
              legend.outside = TRUE,
              frame = FALSE)
  
  
  return(tmapMap)
}
