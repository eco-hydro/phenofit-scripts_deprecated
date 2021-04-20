library(phenofit)

# select the site `CA-N6`
{
    # phenofit parameters
    lambda         <- 8
    nptperyear     <- 23
    minExtendMonth <- 0.5
    maxExtendMonth <- 1
    minPercValid   <- 0
    wFUN           <- wTSM # wBisquare
    wmin           <- 0.2
    methods_fine   <- c("AG", "Zhang", "Beck", "Elmore", "Gu")

    d = MOD13A1$dt %>% subset(site == "CA-NS6" & date >= "2010-01-01" & date <= "2016-12-31") %>%
        .[, .(date, y = EVI/1e4, DayOfYear, QC = SummaryQA)]
    d %<>% mutate(t = getRealDate(date, DayOfYear)) %>%
        cbind(d[, as.list(qc_summary(QC, wmin = 0.2, wmid = 0.5, wmax = 0.8))]) %>%
        .[, .(t, y, QC_flag, w)]
}

{
    INPUT <- check_input(d$t, d$y, d$w, QC_flag = d$QC_flag,
        nptperyear = nptperyear,
        maxgap = nptperyear / 4, wmin = 0.2)

    brks <- season_mov(INPUT,
                       FUN = smooth_wWHIT, wFUN = wFUN,
                       maxExtendMonth = 3,
                       wmin = wmin, r_min = 0.1)

    ## 2.4 Curve fitting
    fit  <- curvefits(INPUT, brks,
                    methods = methods_fine, #,"klos",, 'Gu'
                    wFUN = wFUN,
                    iters = 2,
                    nextend = 2,
                    wmin = wmin,
                    maxExtendMonth = maxExtendMonth, minExtendMonth = minExtendMonth,
                    minPercValid = minPercValid)

    ## check the curve fitting parameters
    l_param <- get_param(fit)
    dfit   <- get_fitting(fit)

    ## 2.5 Extract phenology
    TRS = c(0.1, 0.2, 0.5)
    l_pheno <- get_pheno(fit, TRS = TRS, IsPlot = FALSE) #%>% map(~melt_list(., "meth"))
    pheno <- l_pheno$doy %>% melt_list("meth")
}

# only the first 3 year showed at here
write_fig({
    l_pheno <- get_pheno(fit[1:3], method = "AG", TRS = TRS, IsPlot = TRUE, show_title = FALSE)
}, "Figure6_phenology_metrics.pdf", 8, 4)

## visualization
Ipaper::write_fig({ plot_season(INPUT, brks, ylab = "EVI") }, "Figure4_seasons.pdf", 9, 4)

# grid::grid.newpage(); grid::grid.draw(g) # plot to check the curve fitting
g <- plot_curvefits(dfit, brks, title = NULL, cex = 1.5, title.ylab = "EVI", angle = 0)
Ipaper::write_fig(g, "a.pdf", 8, 6, show = TRUE)

{

    plot(1,1)
    exp <- 2
    NSE = 0.2
    Lines <- list(eval(substitute(bquote(NSE == NSE), list(NSE = NSE))),
                  bquote("second line x"^2),
                  bquote("third line x"^2))
    legend("bottomleft", do.call(expression, Lines))
}

# l <- phenofit_site(d$y, d$t, d$w, d$QC_flag,
#     nptperyear = nptperyear,
#     brks = NULL,
#     wFUN = wFUN,
#     .check_season = TRUE,
#     rm.closed = TRUE,
#     lambda = lambda,
#     maxExtendMonth = maxExtendMonth, minExtendMonth = minExtendMonth,
#     # south = FALSE,
#     verbose = FALSE,
#     ymin = 0.05, wmin = 0.1, wsnow = 0.8,
#     write.fig = TRUE,
#     use.y0 = TRUE, ylab = "EVI",
#     titlestr = "b.pdf", show = TRUE
# )
