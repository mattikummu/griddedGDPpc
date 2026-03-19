f_raster2map_v2_bbox <- function(r_in, shape_in, colPalette, plotTitle, bboxUsed) {

  tmapMap <-
    tm_shape(r_in, bbox = bboxUsed) +
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
    tm_shape(shape_in, bbox = bboxUsed) +
    tm_borders(col = "grey75", lwd = 0.2) +
    tm_layout(frame = FALSE)

  return(tmapMap)
}
