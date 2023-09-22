downld_worldpop_UNadj1km <- function(country = 'BGR', year = '2016',
                                     local_filepath = 'LassaX/data_chik/worldpop'){

    file_local <- paste0(local_filepath, "/", tolower(country), "_", dataset, ".tif")

    wpgpHTTPS <- 'https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/'
    file_remote = paste0(wpgpHTTPS, year, '/', country, '/', tolower(country), 
                         '_ppp_', year, '_1km_Aggregated_UNadj.tif')
    
    if (file.exists(file_local) == F){
        checkStatus <- tryCatch({
            utils::download.file(file_remote, destfile=file_local,mode="wb",quiet=T, method='auto')
        },
        error=function(e){
            message(paste("URL does not seem to exist:", file_remote))
            message("Here's the original error message:")
            message(e)
        },
        warning=function(w){
            message(paste("URL caused a warning:", file_remote))
            message("Here's the original warning message:")
            message(w)
        })
        
        if(inherits(checkStatus, "error") | inherits(checkStatus, "warning")){
            return(NA)
        } else{
            return(1)
        }
    } else { return(1) }
    
}



#for (a in c('1','3','sdgj','4','5')){ next }

# for (i in 1:10) {
#     skip_to_next <- FALSE
#     print(i)
#     # Note that print(b) fails since b doesn't exist
#     tryCatch(a = as.integer(a) return(a), error = function(e) { skip_to_next <<- TRUE})
#     if(skip_to_next) { next }     
# }