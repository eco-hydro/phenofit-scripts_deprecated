library(gdalcubes)
library(stars)
library(rgee)

# ee_Initialize(drive = TRUE)
ocona <- ee$Geometry$Point(c(-73.10783, -16.43148))$buffer(1000)

# ee_search_dataset() %>%
#     ee_search_title("sentinel") %>%
#     ee_search_title("MSI") %>%
#     ee_search_display()

s2 <- ee$ImageCollection("COPERNICUS/S2_SR")

getQABits <- function(image, qa) {
    # Convert decimal (character) to decimal (little endian)
    qa <- sum(2^(which(rev(unlist(strsplit(as.character(qa), "")) == 1))-1))
    # Return a single band image of the extracted QA bits, giving the qa value.
    image$bitwiseAnd(qa)$lt(1)
}

s2_clean <- function(img) {
    # Estimate the NDVI from B8 and B4
    ndvi <- img$normalizedDifference(c("B8", "B4"))

    # Extract quality band
    ndvi_qa <- img$select("QA60")

    # Select pixels to mask
    quality_mask <- getQABits(ndvi_qa, "110000000000")

    # Mask pixels with value zero.
    ndvi$updateMask(quality_mask)$copyProperties(
        img,
        c('system:id', 'system:time_start','system:time_end')
    )
}

s2_ocona <- s2$
    filterBounds(ocona)$
    filter(ee$Filter$lte("CLOUDY_PIXEL_PERCENTAGE", 20))$
    filter(ee$Filter$date("2017-01-01", as.character(Sys.Date())))$
    filter(ee$Filter$calendarRange(6, field = "month"))$
    map(s2_clean)

nimages <- s2_ocona$size()$getInfo()
ic_date <- ee_get_date_ic(s2_ocona)


Map$centerObject(ocona,zoom = 8)
s2_img_list <- list()
for (index in seq_len(nimages)) {
    py_index <- index - 1
    s2_img <- ee$Image(s2_ocona$toList(1, py_index)$get(0))
    s2_img_list[[index]] <- Map$addLayer(
        eeObject = s2_img,
        visParams = list(min = -0.1, max = 0.8, palette = cpt("grass_ndvi", 10)),
        name = ic_date$id[index]
    )
}
Reduce('+', s2_img_list)


s2_ic_local <- ee_imagecollection_to_local(
    ic = s2_ocona,
    scale = 10,
    region = ocona,
    via = 'drive'
)


s2_stars <- map_chr(s2_ic_local, "dsn") %>%
    read_stars %>%
    merge %>%
    st_set_dimensions(names = c("x", "y", "NDVI")) %>%
    `names<-`("NDVI")

s2_stars %>%
    st_get_dimension_values(3) %>%
    substr(
        start = 2,
        stop = 9
    ) %>%
    as.Date(format="%Y%m%d") %>%
    as.character() %>%
    sprintf("Ocoa Valley, Arequipa, Peru: %s", .) ->
    s2_new_names


library(ggmap)
library(tmap)

m1 <- tm_shape(s2_stars) +
    tm_raster(
        palette = cpt("grass_ndvi", 20),
        n = 20,
        title = "NDVI",
        style = "fisher") +
    tmap_style(style = "natural") +
    tm_facets() +
    # tm_facets(nrow = 1, ncol = 1) +
    tm_layout(
        frame.lwd = 2,
        panel.label.bg.color = NA,
        attr.outside = TRUE,
        panel.show = TRUE,
        legend.title.size = 1,
        legend.title.fontface = 2,
        legend.text.size = 0.7,
        legend.frame = FALSE,
        legend.outside = TRUE,
        legend.position = c(0.20, 0.15),
        legend.bg.color = "white",
        legend.bg.alpha = 1,
        main.title = sprintf("Ocona Valley, Arequipa, Peru: %s", s2_new_names),
        main.title.size = 1.2,
        main.title.fontface = 2
    )+
    tm_credits(
        text = "Source: Sentinel-2 MSI: MultiSpectral Instrument, Level-2A",
        size = 1,
        just = "right"
    )
write_fig(m1, "a.pdf")
# grDevices::dev.size("px")
tmap_animation(tm = m1, width = 699*3,height = 555*3,delay = 100)
