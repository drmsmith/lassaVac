unsafe_downld_worldpop_UNadj1km <- function(country = 'BGR', year = '2015',
                                     local_filepath = 'preprocessing/data/2015_UNadj_worlpop_data'){

    # create file path for local file 
    file_local <- paste0(local_filepath, "/", tolower(country), "_", year, ".tif")
    # create https query to download file
    wpgpHTTPS <- 'https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/'
    file_remote = paste0(wpgpHTTPS, year, '/', country, '/', tolower(country), 
                         '_ppp_', year, '_1km_Aggregated_UNadj.tif')
    
    # warning if file will be overwritten 
    if (file.exists(file_local) == T){
        message('Overwriting existing file.') 
        }
    # using trycatch so that a for-loop does not break with an error 
    checkStatus <- tryCatch({
        # try to download file 
        curl::curl_download(url=file_remote, destfile=file_local, quiet=T, mode="wb")
    },
    error=function(e){ # in case of error, print error and return it 
        message(paste0("'", country, "' -- URL does not seem to exist:\n", file_remote))
        message("Here's the original ERROR message:")
        message(e)
        message('\n')
        e
    },
    warning=function(w){ # in case of warning, print warning and return it 
        message(paste0("'", country, "' -- URL caused a WARNING:\n", file_remote))
        message("Here's the original WARNING message:")
        message(w)
        message('\n')
        w
    })
    # check ofr errors or warnings 
    if(inherits(checkStatus, "error") | inherits(checkStatus, "warning")){
        return(country) # if there is an error, func returns country code
    } else{
        return(1) # otherwise return 1 
    }
    
}

# wrap around possibly for graceful failure 
downld_worldpop_UNadj1km <- purrr::possibly(unsafe_downld_worldpop_UNadj1km)
