library(cptcity)
library(raster)
library(stars)
library(rgee)
library(sf)

ee_Initialize(drive = TRUE)

roi <- st_read(system.file("shape/nc.shp", package="sf")) %>%
    st_bbox() %>%
    st_as_sfc() %>%
    sf_as_ee()

modis_ndvi <- ee$ImageCollection("MODIS/006/MOD13A2")

getQABits <- function(image, qa) {
    # Convert binary (character) to decimal (little endian)
    qa <- sum(2^(which(rev(unlist(strsplit(as.character(qa), "")) == 1))-1))
    # Return a mask band image, giving the qa value.
    image$bitwiseAnd(qa)$lt(1)
}

mod13A2_clean <- function(img) {
    ndvi_values <- img$select("NDVI")        # Extract the NDVI band
    ndvi_qa <- img$select("SummaryQA")       # Extract the quality band
    quality_mask <- getQABits(ndvi_qa, "11") # Select pixels to mask
    # Mask pixels with value zero.
    ndvi_values$updateMask(quality_mask)
}

ndvi_composite <- modis_ndvi$
    filter(ee$Filter$date('2001-01-01', '2019-12-31'))$
    filter(ee$Filter$calendarRange(1, field = "month"))$
    map(mod13A2_clean)$
    median()


scale <- 0.0001
Map$setCenter(lon = -79,lat = 35,zoom = 6)
Map$addLayer(
    eeObject = ndvi_composite,
    visParams = list(
        min = 0.2 / scale,
        max = 0.7 / scale,
        palette = cpt("grass_ndvi", 10)
    )
) + Map$addLayer(roi)

mod_ndvi <- ee_as_raster(
    image = ndvi_composite,
    region = roi,
    scale = 1000,
    via = 'drive'
)

