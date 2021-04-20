# source('G:/Github/phenology/phenology/test/load_pkgs.R')

suppressMessages({
    library(Matrix)
    library(plyr)
    library(data.table)
    library(tidyverse)
    library(broom)

    library(magrittr)
    library(lubridate)
    library(purrr)
    library(zoo)

    library(devtools)
    library(jsonlite)
    library(openxlsx)
    # library(pbmcapply)
    # library(MASS)

    ## visualization pkgs
    library(grid)
    library(gridExtra)
    library(Cairo)
    # library(plotly)

    ## self pkgs
    library(Ipaper)
    library(phenofit)

    # library(bigmemory)
    library(doParallel)
    library(foreach)
    library(iterators)
})

fontsize  <- 14

# nptperyear = 46
# save(df, file = "phenofit_flux90_INPUTS.rda")

# # re-calculate phenology of every site
# recal_pheno.site <- function(fit){
#     # 3. phenology
#     p <- lapply(fit$fits, getFits_pheno)
#     # pheno: list(p_date, p_doy)
#     fit$pheno  <- map(p, tidyFits_pheno, origin = fit$INPUT$t[1]) %>% purrr::transpose()
#     return(fit)
# }

# plotsites <- function(fits, file = 'Fig3_GPP_phenofit_flux112_v13.pdf'){
#     Cairo::CairoPDF(file, width = 10, height = 7)
#     sites <- names(fits)

#     for (i in seq_along(fits)){
#         runningId(i)
#         site <- sites[i]
#         tryCatch({
#             p <- plot_curvefits(fits[[i]]) + ggtitle(site)
#             print(p)
#         }, error = function(e){
#             message(sprintf("%s:%s", site, e$message))
#         })
#     }
#     dev.off()
# }
