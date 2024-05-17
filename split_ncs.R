library(tidyverse)
library(glue)
library(terra)
library(parallel)

cores <- 1

terraOptions(verbose = TRUE)

##r <- rast("/media/smithers/shuysman/data/MACA/gye/forecasts/daily/pr_BNU-ESM_rcp45_2006-2099_daily_gye.nc")

##reference <- rast("/home/steve/OneDrive/burroughs_wb/data/burroughs_creek_USGS1m_clipped_nad83.tif")
reference <- rast("./data/burroughs_creek_USGS1m_clipped_nad83.tif")

##in_dir <- file.path("/media/smithers/shuysman/data/MACA/gye/forecasts/daily/")
in_dir <- file.path("~/data/MACA/gye/forecasts/daily/")
##out_dir <- file.path("/media/smithers/shuysman/data/MACA/gye/forecasts/daily-split/")
out_dir <- file.path("~/out/daily-split/")
##out_dir <- file.path("/tmp/test/")

process_gcm <- function(options) {
    variable <- options[1]
    gcm <- options[2]
    scenario <- options[3]

    filename <- file.path(in_dir,
                          glue("{variable}_{gcm}_{scenario}_2006-2099_daily_gye.nc"))

    r <- rast(filename)
    
    for (n in 1:nlyr(r)) {
        lyr <- subset(r, n)

        first_year <- str_split_i(sources(lyr)[1], i = 4, pattern = '_') %>% str_split_i(i = 1, pattern = "-")
        second_year <- str_split_i(sources(lyr)[1], i = 4, pattern = '_') %>% str_split_i(i = 2, pattern = "-")
        
        yday <- yday(time(lyr))
        year <- year(time(lyr))

        lyr <- project(lyr, reference, method = "near")
        lyr <- crop(lyr, reference)
        lyr <- resample(lyr, reference, method = "near")
        ##lyr <- crop(lyr, reference)
        
        writeRaster(lyr,
                    filename = file.path(out_dir,
                                         glue("{n}_macav2metdata_pr_r1i1p1_{gcm}_{scenario}_{first_year}_{second_year}_GYE_daily_reprojected_with_extent_{n}_resampled.tif")),
                    overwrite = TRUE)
    }
}

models <- c("NorESM1-M", "MRI-CGCM3", "MIROC-ESM-CHEM", "MIROC5", "IPSL-CM5A-LR", "inmcm4", "HadGEM2-CC365", "CSIRO-Mk3-6-0", "CNRM-CM5", "CanESM2", "BNU-ESM", "GFDL-ESM2G")
### CCSM4 tmmn data missing
scenarios <- c('rcp85', 'rcp45')
## variables <- c("tmmx", "tmmn", "pr")
variables <- c("tmmx", "tmmn", "pr")

options <- expand.grid(variables = variables, model = models, scenario = scenarios) %>%
    t() %>%
    data.frame()  ### How to use (mc)lapply with expand.grid https://gist.github.com/ihrke/c54e8d7c82cd91ae51d1d62f8d29b936

mclapply(options,
         FUN = process_gcm,
         mc.cores = parallel::detectCores() - 2)

    #MACA File Format = {YEAR_CHUNK}_macav2metdata_{PARAM}_{MODEL_PART1}_{MODEL_PART2}_{SCENARIO}_{FIRST_YEAR_OF_CHUNK}_{LAST_YEAR_OF_CHUNK}_CONUS_dail_reprojected_with_extent{DAYNUMBER}_resampled.tif
