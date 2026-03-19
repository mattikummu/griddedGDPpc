

f_simplify_gpkg <- function(polygon_in, ms_simpli_keep = 0.05) {
  
  
  # define function for simplification
  
  f_simplify_and_save_chunk <- function(data_chunk, chunk_idx, output_file_prefix) {
    # Simplify the chunk
    simplified_chunk <- ms_simplify(data_chunk, keep_shapes = T, keep = 0.05)
    # Save simplified chunk
    output_file <- sprintf("%s_%d.gpkg", output_file_prefix, chunk_idx)
    st_write(simplified_chunk, paste0("results/chunks/",output_file), delete_dsn = T)
    
    return(output_file)
  }
  
  
  
  # simplify by regions (region 6 does not go through)
  
  region <- unique(polygon_in$region_id) %>% 
    sort()
  
  for (i in c(1:5,7:12 )) {
    
    # Extract chunk
    data_chunk <- polygon_in %>% 
      filter(region_id == region[i])
    
    # Simplify and save chunk
    f_simplify_and_save_chunk(data_chunk, i, "simplified_chunk")
  }
  
  # for region 6, go through by countries
  
  reg6_data <- polygon_in %>% 
    filter(region_id == 6)
  
  cntry <- unique(reg6_data$cntry_id) %>% 
    sort()
  
  for (i in 1:length(cntry)) {
    
    # Extract chunk
    data_chunk <- reg6_data %>% 
      filter(cntry_id == cntry[i])
    
    # Simplify and save chunk
    f_simplify_and_save_chunk(data_chunk, 60+i, "simplified_chunk")
  }
  
  
  # List simplified chunk files
  chunk_files <- list.files(path = "results/chunks", pattern = "^simplified_chunk.*\\.gpkg$", full.names = T)
  simplified_chunks <- lapply(chunk_files, st_read)
  
  # Combine simplified chunks
  combined_simplified <- do.call(rbind, simplified_chunks)
  
  # Save combined simplified data
  # st_write(combined_simplified, "results/comb_adm_v3_admID_simpl.gpkg", delete_dsn = T)
  
  return(combined_simplified)
}