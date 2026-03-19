f_raster2map_v2 <- function(r_in, shape_in, colPalette, plotTitle) {

  tmapMap <-
    tm_shape(r_in) +
    tm_raster(
      col.scale = tm_scale_intervals(
        values   = colPalette,
        breaks   = gdp_breaks_2,
        value.na = NA
      ),
      col.legend = tm_legend(
        title       = plotTitle,
        orientation = "landscape",
        position    = tm_pos_out("center", "bottom")
      )
    ) +
    tm_shape(shape_in) +
    tm_borders(col = "grey75", lwd = 0.3) +
    tm_crs("+proj=robin") +
    tm_layout(frame = FALSE)

  return(tmapMap)
}
