
library(maptools)
library(rgdal)
library(raster)
library(plyr)
library(ggplot2)


# Load shapefile with study locations curated by Paul
# his file is the geopackage which may be best to work with but for speed
# I converted multi part to single part in QGIS so now it's a single
# set of polygons rather than a multi polygon:
refLocs <- readShapePoly("studyLocations.shp")

plot(refLocs)


#***********force of infection************
fois <- read.csv("all_param_summary.csv")

# summarise time-varying FOI...Some discussion on how best to summarise would be good
# we may also want to capture variation. For now, again as a rough first-pass, summarised
# by location median across all years of the median, upper and lower estimates
# also take the max FOI
foisSummary <- ddply(fois,.(country,region,study_year)
                     ,summarise
                     ,medMed=median(medianv)
                     ,medLow=median(lower)
                     ,medUpper=median(upper)
                     ,maxMed=max(medianv))

# Don't take any older than 2010 - this cut off is rather arbitrary but would say
# that definately no datasets before 2000, a lot has changed in 20 years.
foisSummary <- foisSummary[!foisSummary$study_year < 2010,]

#*************suitability************
#* Load the 2016 suitability surface 
suitability <- raster("2016_chikv_suitability.tif")
plot(suitability)

# extract suitability values for each study location polygon
# note that some areas have multiple polygons associated with them so we deal with this
# further down
v <- extract(suitability, refLocs)

# again depends on how we want to summarise the suitability, for now I've gone with
# taking the mean value. Taken the variance also to see
meanVal <- lapply(1:length(v),function(x){
  temp <- v[[x]]
  if(length(temp)>1){
  m<-mean(temp,na.rm=T)
  va<-var(temp,na.rm=T)
  }else{return(c(temp,0))}
  return(c(m,va))
})

# add suitability to study locations shapefile
meanSuit <- data.frame(do.call(rbind,meanVal))
names(meanSuit) <- c("mean","var")
refLocs$meanSuit <- meanSuit$mean
refLocs$varSuit <- meanSuit$var

datRefLocs <- refLocs@data

# Now we need to match up the polygons with the FOI summary

regions <- unique(foisSummary$region)



#**************THIS IS A BIT BODGIT - I WENT AND MANUALLY CHANGED REGIONS COLUMN IN EXCEL***********
countriesMatch <- lapply(unique(datRefLocs$COUNTRY),function(x){
  return(foisSummary$country[foisSummary$country %in% x])
})

#write.csv(foisSummary,"foisSummary.csv")
#write.csv(mergedLocs,"mergedLocationDetails.csv")
#write.csv(datRefLocs,"extractedDataByLocation.csv")

# matched regions manually
dat <- read.csv("extractedDataByLocation.csv")
datSummary <- ddply(dat,.(region),summarise,meanSuit=mean(meanSuit))

finalDat <- merge(foisSummary,datSummary,by="region",all.x=T)
names(finalDat) <- c("region","country","study_year","medMed","medLow","medUpper","maxMed","meanSuit")

ggplot(finalDat) +
  geom_point(aes(x=meanSuit,y=medMed))

ggplot(finalDat) +
  geom_point(aes(x=meanSuit,y=medUpper))


ggplot(finalDat) +
  geom_point(aes(x=meanSuit,y=log10(medUpper)+1))


