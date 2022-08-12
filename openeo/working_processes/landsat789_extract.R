# L7 + L89


source("openeo/setup.R")


# Landsat 7
tL7 <- c("2012-01-01", "2012-12-31")
cubeL7 <- p$load_collection(
    id = "LANDSAT7_ETM_L2",
    spatial_extent = aoi,
    temporal_extent = tL7,
    bands=c("B04","B05")
)

cubeL7 <- p$rename_labels(data = cubeL7, dimension = "bands", target = c("NIR", "SWIR"), source = c("B04", "B05"))


# Landsat 8 + 9

tL89 <- c("2013-01-01", "2022-08-01")

cubeL89 <- p$load_collection(
    id = "LANDSAT8-9_L2",
    spatial_extent = aoi,
    temporal_extent = tL89,
    bands=c("B05","B06")
)

cubeL89 <- p$rename_labels(data = cubeL89, dimension = "bands", target = c("NIR", "SWIR"), source = c("B05", "B06"))


# combine cubes

## Resample L7 to L89

cubeL7 <- p$resample_cube_spatial(data = cubeL7, target = cubeL89)
cube <- p$merge_cubes(cubeL7, cubeL89)


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
cat(process_json, file = "openeo/working_processes/landsat789_extract.json")




