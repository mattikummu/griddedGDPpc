#' @title Process and Simplify Administrative Polygons
#' @description This function reads an administrative polygon layer, fixes geometries,
#'   breaks the antimeridian, simplifies the polygons using mapshaper, and saves the result.
#'
#' @param input_gpkg_file A character string specifying the path to the input GPKG file.
#' @param output_gpkg_file A character string specifying the path for the output GPKG file.
#' @param simplify_percentage A numeric value for the simplification percentage (e.g., 1 for 1%).
#' @return A character string indicating the path to the output file on success, or a message on failure.
#'
#' @details
#'   This function requires the 'sf', 'terra', 'qgisprocess', and 'geojsonio' R packages.
#'   It also depends on the 'mapshaper' command-line tool, which must be installed
#'   (e.g., `npm install -g mapshaper`) and accessible in your system's PATH.
#'   The QGIS processing environment must be configured prior to calling this function
#'   with `qgisprocess::qgis_configure()`.
#'
f_simplify_polyg <- function(input_gpkg_file, output_gpkg_file, simplify_percentage = 1) {
  
  
  # Ensure required packages are loaded
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required but not installed.")
  if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required but not installed.")
  if (!requireNamespace("qgisprocess", quietly = TRUE)) stop("Package 'qgisprocess' is required but not installed.")
  if (!requireNamespace("geojsonio", quietly = TRUE)) stop("Package 'geojsonio' is required but not installed.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required but not installed.")
  
  # Configure QGIS processing environment
  qgisprocess::qgis_configure()
  
  tryCatch({
    message("Step 1: Reading input file and preparing data...")
    # Read the vector data using terra
    v_adm2_polyg_comb <- terra::vect(input_gpkg_file)
    
    # Make geometries valid and convert to an sf object
    sf_adm2_polyg_comb_org <- terra::makeValid(v_adm2_polyg_comb) %>%
      sf::st_as_sf()
    
    # Break the antimeridian for processing
    sf_adm <- sf_adm2_polyg_comb_org %>%
      sf::st_break_antimeridian(lon_0 = 0)
    
    message("Step 2: Fixing geometries using QGIS algorithm...")
    # Use QGIS's native fix geometries algorithm
    result <- qgisprocess::qgis_run_algorithm(
      "native:fixgeometries",
      INPUT = sf_adm,
      OUTPUT = qgisprocess::qgis_tmp_vector()
    )
    
    # Extract the output file path and read the fixed sf object
    # The error 'formal argument "quiet" matched by multiple actual arguments'
    # likely stems from a version incompatibility between the 'qgisprocess' and 'sf' packages.
    # We explicitly read the file to avoid potential conflicts with implicit arguments from 'qgisprocess'.
    fixed_file_path <- qgisprocess::qgis_extract_output(result, "OUTPUT")
    sf_adm_fixed <- sf::st_read(fixed_file_path)
    message(paste("Fixed features count:", nrow(sf_adm_fixed)))
    
    message("Step 3: Exporting to GeoJSON for mapshaper processing...")
    # Create temporary file paths for mapshaper workflow
    geojson_file <- file.path(tempdir(), "sf_adm_fixed_polyg.geojson")
    topoj_file <- file.path(tempdir(), "sf_adm_fixed_polyg.topojson")
    topoj_file_simp <- file.path(tempdir(), "sf_adm_fixed_polyg_simpl.topojson")
    
    # Write the fixed geometries to a temporary GeoJSON file
    sf::st_write(obj = sf_adm_fixed, dsn = geojson_file, driver = "GeoJSON", delete_dsn = TRUE)
    
    message("Step 4: Converting to TopoJSON using mapshaper...")
    # Construct and execute the mapshaper command to convert to TopoJSON
    mapshaper_path <- Sys.which("mapshaper")
    if (mapshaper_path == "") {
      stop("mapshaper command-line tool not found. Please ensure it is installed and in your PATH.")
    }
    
    command <- paste0(
      "node --max-old-space-size=8192 ",
      '"', mapshaper_path, '" ',
      geojson_file,
      " -o format=topojson ",
      topoj_file
    )
    system(command, ignore.stdout = TRUE, ignore.stderr = TRUE)
    
    message(paste0("Step 5: Simplifying polygons by ", simplify_percentage, "%..."))
    # Construct and execute the mapshaper command to simplify the TopoJSON
    command_simpl <- paste0(
      "node --max-old-space-size=8192 ",
      '"', mapshaper_path, '" ',
      topoj_file,
      " -clean -simplify ", simplify_percentage, "% keep-shapes -o ",
      topoj_file_simp
    )
    system(command_simpl, ignore.stdout = TRUE, ignore.stderr = TRUE)
    
    message("Step 6: Reading simplified data and finalising...")
    # Read the simplified TopoJSON back into R
    sf_adm_fixed_polyg_topo <- geojsonio::topojson_read(topoj_file_simp)
    
    # Set the CRS from the original data
    sf_adm_fixed_polyg_topo_valid <- sf::st_set_crs(sf_adm_fixed_polyg_topo, sf::st_crs(sf_adm_fixed))
    
    # Mutate and select final columns using dplyr
    sf_adm_fixed_polyg_topo_valid_names <- sf_adm_fixed_polyg_topo_valid# %>%
      # dplyr::mutate(NAME0 = ifelse(is.na(country_name), NAME0, country_name)) %>%
      # dplyr::select(iso3, NAME_0, GID_1, NAME_1, GID_2, NAME_2, GID_3, NAME_3, ppp_2020, region_id, cntry_id, admID)
    
    # Write the final result to the specified output file
    sf::st_write(obj = sf_adm_fixed_polyg_topo_valid_names, dsn = output_gpkg_file, delete_dsn = TRUE)
    
    message(paste("Process complete. Output saved to:", output_gpkg_file))
    
    return(output_gpkg_file)
    
  }, error = function(e) {
    # Print a more informative error message
    message("An error occurred during the process:")
    message(e$message)
    return(invisible(NULL))
  }, finally = {
    # Clean up temporary files
    if (file.exists(geojson_file)) file.remove(geojson_file)
    if (file.exists(topoj_file)) file.remove(topoj_file)
    if (file.exists(topoj_file_simp)) file.remove(topoj_file_simp)
  })
}