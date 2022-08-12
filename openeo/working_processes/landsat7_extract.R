# Landsat 7 extract

source("setup.R")


tL7 <- c("2012-01-01", "2012-12-31")

cubeL7 <- p$load_collection(
    id = "LANDSAT7_ETM_L2",
    spatial_extent = aoi,
    temporal_extent = tL7,
    bands=c("B04","B05")
)



# copy for nomenclature reasons (so it is easier to merge afterwards)
cube <- cubeL7


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
cat(process_json, file = "working_processes/landsat7_extract.json")


