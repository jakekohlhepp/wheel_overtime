## make map of divisions
working<-fread('mkdata/20170803_payworkers_comp/anonymized_data_073117.txt')
working<-unique(working[!is.na(`Div Cd`),c("Div Cd", "Assigned Div")])
setkey(working,`Div Cd` )
# i/s stands for intersection: https://ladotparking.org/wp-content/uploads/2018/04/Common-Abbreviations.pdf

write.csv(working,'mkdata/data/01_05_list_tofill.csv')
# info from https://geohub.lacity.org/datasets/ladot-engineering-districts/explore



shape <- read_sf(dsn = "mkdata/20250311_ladot_enforcement_districts/", layer = "LADOT_Parking_Enforcement_Districts")


la_major <- getbb(place_name = "Los Angeles") %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary")) %>%
  osmdata_sf()




##

hqs<-fread('mkdata/data/01_05_list_complete.csv')
setnames(hqs, "clean_name", "Offices")
hqs[, Offices:=as.factor(Offices)]
hqs<-unique(hqs[`Div Cd`>808 & Offices!="Habitual Parking",c("Offices", "location_parking_enforcement_office")])
hqs<-geocode(hqs, location_parking_enforcement_office)
hqs<-st_as_sf(hqs, coords = c("long","lat"), remove = FALSE)
st_crs(hqs)<-"EPSG:4326"
hqs<-st_transform(hqs,st_crs(shape))

setnames(shape, "District", "Parking Enforcement Districts")

street_plot <- ggplot() +
  geom_sf(data=shape,aes(fill=`Parking Enforcement Districts`), color=NA, alpha=0.5)+
  geom_sf(data = la_major$osm_lines,
          color = "black",alpha=0.3,
          size = 0.2)+  geom_point(data=hqs,
                                   aes(shape = Offices, geometry = geometry),
                                   stat = "sf_coordinates",size=4,fill="black"
          )+
  scale_shape_manual(values=c("diamond", "circle", "asterisk", "square","triangle","triangle down filled"))+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(), legend.text=element_text(size=20),legend.title=element_text(size=20))
# Print the plot

print(street_plot)
ggsave("mkdata/out/figures/01_05_la_street_map.png", width=12, height=12, units="in")
