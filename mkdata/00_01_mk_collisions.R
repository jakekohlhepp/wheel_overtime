### create collissions data
## we want these to be mapped to bss special events.


### first load special events between jan 1, 2015 and june 30, 2016
# must get api key and enter it as key variable
# key<-

special<-fread('mkdata/20250207_bss_special_events/Building_and_Safety_Temporary_Special_Event__TSE__Permits_20250207.csv')
## process start and end date and expand
special[, start_date:=mdy(`Event Start Date`)]
special[, end_date:=mdy(`Event End Date`)]
special<-special[, .(analysis_workdate=as.Date(start_date:end_date)), by=eval(colnames(special))]
special<-special[analysis_workdate>=as.Date('2015-01-01') & analysis_workdate<=as.Date('2016-06-30'), ]

## construct street address
# some of the address fields are repeated
stopifnot(nrow(special[`Address Start`!=`Address End`])==0)
stopifnot(nrow(special[`Address Fraction End`!=`Address Fraction Start`])==0)
special[, address_start_num:=ifelse(is.na(`Address Start`),"",as.character(`Address Start`))]
special[, address_start_frac_num:=ifelse(is.na(`Address Fraction Start`),"",as.character(`Address Fraction Start`))]

## addresses stored typically one of two ways. first in fields
special[, address:=str_replace_all(paste(address_start_num,address_start_frac_num,`Street Direction`,`Street Name`, `Street Suffix`, `Suffix Direction`),"  ","")]
## than in single event location variable
special[, address:=ifelse(address==" ",`Event Location`,address )]
stopifnot(special$address!=" ")

## some have a latitude and longitude
special[, has_lat:=`Latitude/Longitude`!=""]

## unique addresses
special[, address_withca:=paste0(address, ", Los Angeles, CA")]
get_location<-unique(special[, c("address_withca", "Zip Code")])

doc = mp_geocode(
  addresses = get_location$address_withca,
  region="us",
  key = key,
  quiet = TRUE
)
pnt = mp_get_points(doc)
pnt<-data.table(pnt)
setnames(pnt,"address", "address_withca")
special<-merge(special, data.table(pnt)[,c("address_withca","pnt")], by="address_withca",allx.=TRUE )
special[pnt=="POINT EMPTY", pnt:=NA]
special[, pnt:=str_replace(pnt, "POINT ","")]
special[, lat:= as.numeric(str_replace_all(str_extract(pnt, ',(.*)\\)'),"[, )]",""))]
special[, long:= as.numeric(str_replace_all(str_extract(pnt, '\\((.*),'),"[, (]",""))]

# manual fix
special<-merge(special,fread('mkdata/data/00_01_special_event_latlong.csv'), by="address_withca", all.x=TRUE)
special[!is.na(latlong), long:=word(latlong, 2, sep="\\,")]
special[!is.na(latlong), lat:=word(latlong, 1, sep="\\,")]

## all those still with no lat long are 6801 Hollywood Blvd
stopifnot(str_detect(special[is.na(long) | is.na(lat)]$address_withca, "6801 Hollywood Blvd"))
special[str_detect(address_withca, "6801 Hollywood Blvd")>0,lat:=34.1028524157275]
special[str_detect(address_withca, "6801 Hollywood Blvd")>0,long:=-118.3403568356196]
                

saveRDS(special[,-c("Long", "Lat", "has_lat","latlong","pnt")], file="mkdata/data/00_01_special_events_with_coords.rds")


#special<-readRDS("mkdata/data/00_01_special_events_with_coords.rds")
### now collisions
collide<-fread('mkdata/20250320_collisions/Traffic_Collision_Data_from_2010_to_Present_20250320.csv')
collide[, analysis_workdate:=mdy(`Date Occurred`)]
collide<-collide[analysis_workdate>=as.Date("2015-01-01") & analysis_workdate<=as.Date("2016-06-30"), ]
stopifnot(collide$`Crime Code Description`=="TRAFFIC COLLISION")
collide[, lat:=as.numeric(str_replace(word(Location, 1, sep="\\,"),"\\(",""))]
collide[, long:=as.numeric(str_replace(word(Location, 2, sep="\\,"),"\\)",""))]


## take all unique coordinates on a date.
uniq_coords<-unique(special[, c("analysis_workdate", "lat", "long")])
setkey(uniq_coords, "analysis_workdate", "lat", "long")
uniq_coords[, id_that:=1:.N]
stopifnot(uniqueN(collide[,c("DR Number", "Date Occurred")])==nrow(collide))
setkey(collide, "DR Number", "Date Occurred")
collide[, id_this:=1:.N]

### for each collision compute closest special event on that date.
## if none, set Inf
collide_list<-rep(0,nrow(uniq_coords))
for (i in unique(collide$id_this)){
  coords<-c(collide[id_this==i,]$long,collide[id_this==i,]$lat)
  this_date<-collide[id_this==i,]$analysis_workdate
  if (nrow(uniq_coords[analysis_workdate==this_date,])==0){
    collide_list[i]<-Inf
  } else{
    max_now<-Inf
    for (j in unique(uniq_coords[analysis_workdate==this_date,]$id_that)){
      check_dist<-distHaversine(coords, c(uniq_coords[id_that==j]$long,uniq_coords[id_that==j]$lat ))
      max_now<- min(max_now, check_dist)
    }
    collide_list[i]<-max_now
  }
  
}
collide<-merge(collide, data.table(id_this=unique(collide$id_this), min_dist=collide_list), by="id_this", all.x=TRUE)
saveRDS(collide, file="mkdata/data/00_01_collisions_with_mindist.rds")
