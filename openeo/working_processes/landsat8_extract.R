# landsat 8-9 extract

source("setup.R")

tL89 <- c("2013-01-01", "2022-08-01")



cubeL89 <- p$load_collection(
    id = "LANDSAT8-9_L2",
    spatial_extent = aoi,
    temporal_extent = tL89,
    bands=c("B05","B06")
)



# copy for better merging later
cube <- cubeL89


# calculate NDWI for each time step
cube_ndwi <- p$reduce_dimension(cube, reducer = ndwi_, dimension = "bands")
cube_ndwi <- p$add_dimension(data = cube_ndwi, name = "bands", label = "NDWI")



# Extract values in polygon (currently with mean aggregation)
extract = p$aggregate_spatial(data = cube_ndwi,
                              geometries = polys,
                              reducer = function(data, context) { p$mean(data, ignore_nodata = TRUE) })

# specify output format
res <- p$save_result(data = extract, format = "csv")

# generate process graph for openEO
process <- as(res, "Process")
process_json <- jsonlite::toJSON(process$serialize(), auto_unbox = TRUE, force = TRUE) # doesnt work anymore
cat(process_json, file = "working_processes/landsat8_extract.json")
