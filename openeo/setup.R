# setup


library(sf)
library(openeo)


con <- connect(host = "https://openeo.cloud")
p <- processes()



# simplify test polygons:
polys = st_read("openeo/vechta.gpkg") |>
    st_centroid() |> 
    st_buffer(dist = 120, nQuadSegs = 1, endCapStyle = "SQUARE") |> 
    st_transform(4326)

# area of interest
aoi <- list(west = st_bbox(polys)[1],
            south = st_bbox(polys)[2],
            east = st_bbox(polys)[3],
            north = st_bbox(polys)[4])




ndwi_ <- function(x, context) {
    nir <- x[1]
    swir <- x[2]
    return(p$normalized_difference(nir, swir))
}



