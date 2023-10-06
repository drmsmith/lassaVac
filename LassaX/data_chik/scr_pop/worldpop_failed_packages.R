# these are some possible packages that claim to query + download worldpop data
# i struggled to use them for various reasons
# safe to assume all the code below is broken

library('raster')
library('terra')
library('tidyverse')
library('countrycode')

################################################
# select countries whose population to extract #
################################################

# burden estimate data from Henrik 
df_burden_estimates = read.csv('LassaX/data_chik/df_countries_burden_estimates_cepi.csv')

# keep only countries where transmission is estimated to occur
df_burden_est_sub = df_burden_estimates %>% filter(classification != 'NoTransmission') %>%
    dplyr::select(., country, who_region, classification, infections_mean, 
                  infections_min, infections_max)

# United_States_of_America name isn't mapped onto USA by countrycode()
df_burden_est_sub$country = ifelse(df_burden_est_sub$country=='United_States_of_America',
                                   'USA', df_burden_est_sub$country)

# map country names onto country codes for query  
ccodes = countrycode(df_burden_est_sub$country,
                     origin = 'country.name', destination = 'iso3c') %>% 
    data.frame(country=df_burden_est_sub$country, code=.)

# adding country code to burden estimate data frame 
df_burden_est_sub = left_join(ccodes, df_burden_est_sub, by='country')


library('wpgpDownloadR')

list_countries = wpgpListCountries()

datasets = wpgpListCountryDatasets(ISO3="USA")

datasets$Covariate

df <- wpgpGetCountryDataset(ISO3 = "USA",
                            covariate = "ppp_2016_UNadj",
                            destDir = "LassaX/data_chik/worldpop/")


# ppp_2016_UNadj is the covariate code 


### the code below is from the source code of the package  


wpgpGetCountryDataset <- function (ISO3 = NULL, covariate = NULL, destDir = tempdir(), 
          quiet = TRUE, method = "auto") 
{
    if (!dir.exists(destDir)) 
        stop(paste0("Please check destDir exists: ", destDir))
    if (is.null(ISO3)) 
        stop("Error: Enter country ISO3")
    if (is.null(covariate)) 
        stop("Error: Enter covariate")
    df <- wpgpGetDFDatasets(quiet)
    ########### the problem #############
    # ISO3 <- toupper(ISO3)             #
    # covariate <- tolower(covariate)   #
    #####################################
    df.filtered <- df[df$ISO3 == ISO3 & df$Covariate == covariate, 
    ]
    if (nrow(df.filtered) < 1) {
        stop(paste0("Entered Covariates: ", paste(covariate, 
                                                  collapse = ", "), 
                    " not present in WP. Please check name of the dataset"))
    }
    file_remote <- as.character(df.filtered$PathToRaster)
    file_local <- paste0(destDir, "/", tolower(ISO3), "_", covariate, 
                         ".tif")
    ftpReturn <- wpgpDownloadFileFromFTP(file_remote, file_local, 
                                         quiet = quiet, method = method)
    if (!is.null(ftpReturn)) {
        return(file_local)
    }
    else {
        return(NULL)
    }
}

wpgpGetDFDatasets <- function(quiet=TRUE) {
    
    if(wpgpCheckDatasetSum()){
        if (!quiet){ message(paste("Datasets up-to-date." )) }  
        return(wpgpDatasets)
    }
    
    
    wpgpAllCSVFilesPath <- paste0(tempdir(),"/wpgpDatasets.csv")
    
    if(!file.exists(wpgpAllCSVFilesPath)){
        
        if (!quiet){ message(paste("Covariates list will be updated" )) }
        
        file_remote <-paste0('assets/wpgpDatasets.csv')
        
        wpgpDownloadFileFromFTP(file_remote, wpgpAllCSVFilesPath, quiet=TRUE)
    }
    
    df.all.Datasets = utils::read.csv(wpgpAllCSVFilesPath, stringsAsFactors=FALSE)
    
    return(df.all.Datasets)
}

wpgpCheckDatasetSum <- function() {
    
    wpgpDatasets.md5 <- packageDescription("wpgpDownloadR",fields="DatasetSum")
    
    
    wpgpDatasets.md5.tmp <- paste0(tempdir(),"/wpgpDatasets.md5")
    
    wpgpDownloadFileFromFTP("assets/wpgpDatasets.md5", wpgpDatasets.md5.tmp,  quiet=TRUE)
    
    con <- file(wpgpDatasets.md5.tmp,"r")
    wpgpDatasets.md5.check <- readLines(con,n=1, warn = F)
    close(con)  
    
    # removing tmp file
    if(file.exists(wpgpDatasets.md5.tmp)){ unlink(wpgpDatasets.md5.tmp, recursive = TRUE, force = FALSE)}
    
    if(wpgpDatasets.md5 == wpgpDatasets.md5.check){
        return(TRUE)
    }else{
        return(FALSE)
    }
}



wpgpDownloadFileFromFTP <- function(file_path, dest_file, quiet, method="auto") {
    
    wpgpFTP <- "ftp.worldpop.org.uk"
    file_remote <-paste0('ftp://',wpgpFTP,'/',file_path)
    
    tmStartDw  <- Sys.time()
    
    checkStatus <- tryCatch(
        {
            utils::download.file(file_remote, destfile=dest_file,mode="wb",quiet=quiet, method=method)
        },
        error=function(cond){
            message(paste("URL does not seem to exist:", file_remote))
            message("Here's the original error message:")
            message(cond)
        },
        warning=function(cond){
            message(paste("URL caused a warning:", file_remote))
            message("Here's the original warning message:")
            message(cond)
        },
        finally={
            if (!quiet){
                tmEndDw  <- Sys.time()
                #message(paste("Processed URL:", file_remote))
                message(paste("It took ", tmDiff(tmStartDw ,tmEndDw,frm="hms"), "to download" ))
            }
        }
    )
    
    if(inherits(checkStatus, "error") | inherits(checkStatus, "warning")){
        return(NULL)
    } else{
        return(1)
    }
}





## my own version to download queries data using a more custom link

countrycode('Bulgaria', origin = 'country.name', destination = 'iso3c')
country = 'BGR'
dataset = 'ppp_2016_UNadj'
file_path = 'LassaX/data_chik/worldpop/'
file_local <- paste0(file_path, "/", tolower(country), "_", dataset, ".tif")

wpgpFTP <- "ftp.worldpop.org.uk"
# remote_path = ''
# file_remote <-paste0('ftp://',wpgpFTP,'/',remote_path)
file_remote='https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/2016/BGR/bgr_ppp_2016_1km_Aggregated_UNadj.tif'

utils::download.file(file_remote, destfile=file_local,mode="wb",quiet=T, method='auto')

# the ftp links did not work for me 
'ftp://ftp.worldpop.org.uk/GIS/Population/Global_2000_2020/2016/USA/usa_ppp_2016_UNadj.tif'



## raster works better than terra 
r <- raster::raster("LassaX/data_chik/worldpop/bgr_ppp_2016_UNadj_____.tif")
rs <- raster::cellStats(r, "sum")



# r <- terra::rast("LassaX/data_chik/worldpop/usa_ppp_2016_UNadj.tif")
# t <- rast(ext = ext(r), crs = crs(r), resolution = res(r) * 4000)
# makeTiles(r, t)

r <- terra::rast("LassaX/data_chik/worldpop/bgr_ppp_2016_UNadj_____.tif")
rs <- terra::global(r, "sum")
to_points = rasterToPoints(r, spatial = TRUE)


### great library but some countries are not available 
library('wopr')

# Retrieve the WOPR data catalogue
catalogue <- getCatalogue()

# Select files from the catalogue by subsetting the data frame
selection <- subset(catalogue,
                    country == 'NGA' &
                        category == 'Population' &
                        version == 'v1.2')

# Download selected files
downloadData(selection)

catalogue$country %>% unique %>%
    countrycode(., origin = 'iso3c', destination = 'country.name')


utils::download.file(url = "ftp://ftp.worldpop.org/GIS/Population/Global_2000_2020/2020/IND/ind_ppp_2020.tif",
                     destfile = "LassaX/data_chik/ind_ppp_2020.tif",
                     mode="wb",
                     quiet=FALSE,
                     method="libcurl")
