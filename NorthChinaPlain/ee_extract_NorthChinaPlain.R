library(rgee)
# ee$Initialize()

## 1. read tested points
sp <- read_sf("/mnt/n/Research/GEE_repos/gee_whittaker/data-raw/st_test-NorthChina&GuangDong.shp")
sp %<>% mutate(ID = 1:nrow(.)) %>% select(ID, IGBPcode)

## 2. clip EVI data by `rgee`
bands = c('EVI', 'DayOfYear', 'SummaryQA')
imgcol <- ee$ImageCollection('MODIS/006/MOD13A2')$
    select(bands)$
    filterDate('2015-01-01', '2021-12-31')
prj = getProj(imgcol)

{
    system.time({
        df_tmp = ee_extract(imgcol, sp, scale = prj$scale) %>% data.table()
    })
    df = ee_extract_clean(df_tmp)
    save(df, file = "phenofit_NorthChinaPlain_test-(2015-2021).rda")
}
