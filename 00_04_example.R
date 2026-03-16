library('osmdata')
library('data.table')

collide<-readRDS("mkdata/data/00_01_collisions_with_mindist.rds")


la_major <- getbb(place_name = "zip code 90068") %>%
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
