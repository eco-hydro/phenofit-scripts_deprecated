source("scripts/load_pkgs.R")
library(sf)
library(sf2)
# FOR MOD09A1
load_all()
# // 620-670nm, RED, sur_refl_b01
# // 841-876nm, NIR, sur_refl_b02
# // 459-479nm, BLUE, sur_refl_b03
# // 1628-1652nm, SWIR, sur_refl_b06
# satellite = "MOD09A1"
# df = tidy_modis_09A1(satellite)
# load("N:/Research/r_pkgs/rGEEadd/data-raw/R_challenges/phenofit_NorthChinaPlain_test-(2015-2021).rda")
df <- fread("/mnt/i/Research/phenology/gee_whittaker/data-raw/wWHd2/ChinaPheno_sp1e2_MODIS_006_MCD15A3H.csv")
df$date %<>% as_date()

# df %<>% mutate(t = getRealDate(date, DayOfYear))
df[, c("QC_flag", "w") := qc_FparLai(FparExtra_QC, wmin = 0.2, wmid = 0.5)]
df2 = df[, .(ID = Id, t = date, y = Lai/10, w, QC_flag, QC = FparExtra_QC)]

sp <- read_sf("/mnt/i/Research/phenology/gee_whittaker/data-raw/wWHd2/ChinaPheno1km_10y_DoubleSeasonPixels_Maize&Wheat_st1e2.shp")
sp %<>% mutate(ID = 1:nrow(.)) #%>% select(ID, IGBPcode)
st = as.data.table(sp) %>% data.table()

## 2. phenofit -----------------------------------------------------------------
IDs = sp$ID %>% set_names(., .)
# grps_sites = sites %>% set_names(seq_along(.), .)
# grps_sites = df_bad[, set_names(ID, site)]

lambda0    = 15
nptperyear = 23
# library(glue)
InitCluster(12, kill = FALSE)
# InitCluster(1)
lst_EVI <- foreach(id = IDs, icount()) %dopar% {
    # if (id != 29) return()
    runningId(id)
    i <- id
    d <- df2[ID == id]
    # d[, c("QC_flag", "w") := qc_summary(QC, wmin = 0.1, wmid = wmid)]
    if (all(is.na(d$y))) return()
    minExtendMonth <- 0.5
    extendMonthMin <- 1
    # only optim lambda when lambda_opt < 2
    lambda <- tryCatch(
        {
            l_lambda <- v_curve(d[!is.na(y)], lg_lambdas = seq(1, 3, 0.1), IsPlot = FALSE)
            ans <- l_lambda$lambda
            if (ans > 500) ans <- ans * 2 / 3
            ans
        },
        error = function(e) lambda0
    )
    # if (lambda < 150) {
    #     message(sprintf("%s: lambda = %f", sitename, lambda))
    #     extendMonthMin = 0.5
    #     minExtendMonth = 0
    # } #else lambda = lambda0
    # lambda = NULL
    title <- sprintf(
        "[%03d] lat=%.2f, lon=%.2f, lambda=%.2f",
        id, st$lat[i], st$lon[i], lambda
    )
    tryCatch(
        {
            l <- process_phenofit(d$y, d$t, d$w, d$QC_flag,
                nptperyear = nptperyear,
                brks = NULL,
                wFUN = wTSM,
                .check_season = TRUE,
                rm.closed = TRUE,
                lambda = lambda,
                extendMonthMin = extendMonthMin, minExtendMonth = minExtendMonth,
                # south = FALSE,
                overwrite = TRUE,
                verbose = FALSE,
                ymin = 0.05, wmin = 0.1, wsnow = 0.8,
                write.fig = TRUE,
                use.y0 = TRUE, ylab = "NDVI",
                titlestr = title, show = FALSE
            )
            l
        },
        error = function(e) {
            message(sprintf("[%02d]: %s", i, e$message))
        }
    )
}

{
    version <- 0.2
    outfile <- glue::glue("phenofit_MCD15A3H_LAI_st1e2_{version}.pdf")
    merge_pdf(outfile, pattern = "\\[", del = FALSE)
    # pdf_acrobat(outfile)
}

{
    # file_gof = "INPUT/gof_MOD09A1_EVI_st166_phenofit.csv"
    # df_gof = map(lst_EVI %>% rm_empty, function(l){
    #     if (!is.null(l$`1`)) {
    #         l$`1`$fit[, GOF(y, ziter2, include.r = TRUE)]
    #     } else NULL
    # }) %>% do.call(rbind, .) %>%
    #     {cbind(site = rownames(.), ID = match(rownames(.), sites), data.table(.))}
    # fwrite(df_gof, file_gof)
    # df_gof = fread(file_gof)
    # df_bad <- df_gof[NSE < 0.5, ]
}

save(lst_EVI, file = "pheno_MCD15A3H_LAI_st1e2.rda")

# merge_pdf("EVI_phenofit_test.pdf", pattern = , del = TRUE)
# merge_pdf("EVI_phenofit_v10_st95.pdf", pattern = "\\[", del = TRUE)
# merge_pdf("LAI_phenofit_gpp_brks_v01_st95.pdf", pattern = "\\[", del = TRUE)
# dt = l$dt %>% do.call(rbind, .)
# df_gpp$date %<>% ymd()
# d_gpp = df_gpp[site == sitename, .(site, t = date, GPP_DT, GPP_NT)]
# Sys.setenv(PATH = paste0("/opt/bin:/opt/anaconda3/bin:", Sys.getenv("PATH")))

{
    j = 5
    sitename = "CZ-BK2"
    sitename = "CA-NS5"
    sitename = "CH-Dav"
    sitename = "US-KS2"
    d = df[site == sitename & group == j, ]
    # d = d[t >= "2014-01-01" & t <= "2015-01-01"]
    nptperyear = 92
    INPUT <- check_input(d$t, d$y, d$w, QC_flag = d$QC_flag,
                         nptperyear = nptperyear, south = FALSE,
                         maxgap = nptperyear/4, alpha = 0.02, wmin = 0.2)
    # plot_input(INPUT, show.y0 = TRUE)
    lg_lambdas = seq(1, 4, 0.1)
    lambda <- v_curve(INPUT, lg_lambdas)$lambda

    brks <- season_mov(INPUT,
                       FUN = smooth_wWHIT, wFUN = wTSM,
                       extendMonthMin = 3,
                       lambda = 1e3,
                       r_min = 0.03,
                       .check_season = FALSE,
                       # years.run = 2004,
                       IsPlot = FALSE, IsPlot.OnlyBad = FALSE, print = FALSE)

    write_fig(expression(plot_season(INPUT, brks)), "check_season.pdf", 10, 4)
}

{
    d$y[1:200] %>% plot(type = "b")
    halfwin = ceiling(nptperyear/36)
    movmean(d$y, halfwin) %>% lines(col = "blue", type = "b")
}
