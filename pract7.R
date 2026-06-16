library(here)
library(janitor)
library(sf)
library(tidyverse)
LondonWards <- st_read(here::here("statistical-gis-boundaries-london", 
                                  "ESRI", "London_Ward.shp"))
LondonWardsMerged<- st_read(here::here("statistical-gis-boundaries-london", 
                                       "ESRI",
                                       "London_Ward_CityMerged.shp"))%>%
  st_transform(.,27700)
WardData <- read_csv("https://data.london.gov.uk/download/f33fb38c-cb37-48e3-8298-84c0d3cc5a6c/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                     locale = locale(encoding = "latin1"),
                     na = c("NA", "n/a")) %>% 
  clean_names()
# reporject to confirm

LondonWardsMerged <- LondonWardsMerged%>%
 left_join(WardData, 
           by = c("GSS_CODE" = "new_code"))%>%
             dplyr::distinct(GSS_CODE, .keep_all = TRUE)%>%
             dplyr::select(GSS_CODE, ward_name, average_gcse_capped_point_scores_2014)
         
st_crs(LondonWardsMerged)
library(tmap)

BluePlaques <- st_read(here("open-plaques-london-2018-04-08.geojson")) %>%
  st_transform(.,27700)

st_crs(BluePlaques)

tmap_mode("plot")
tm_shape(LondonWardsMerged) +
  tm_polygons(fill_alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(fill = "blue", size=0.1)
summary(BluePlaques)

# excluding the outlier

BluePlaquesSub = BluePlaques[LondonWardsMerged,]
tmap_mode("plot")
tm_shape(LondonWardsMerged) +
  tm_polygons(fill_alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(fill = "blue", size=0.1)

check_example <- LondonWardsMerged%>%
  st_join(BluePlaquesSub)%>%
  filter(ward_name=="Kingston upon Thames - Coombe Hill")

library(sf)
points_sf_joined <- LondonWardsMerged%>%
  mutate(n = lengths(st_intersects(., BluePlaquesSub)))%>%
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density=n/area)%>%
  #select density and some other variables 
  dplyr::select(density, ward_name, gss_code, n, average_gcse_capped_point_scores_2014)


points_sf_joined<- points_sf_joined %>%                    
  group_by(gss_code) %>%         
  summarise(density = first(density),
            wardname= first(ward_name),
            plaquecount= first(n))

tm_shape(points_sf_joined) +
  tm_polygons(
    fill = "density",
    fill.scale = tm_scale_intervals(
      values = "brewer.blues",
      style="jenks"),
    # set the legend
    fill.legend = tm_legend(title="Blue Plaque Density",
                            title.size=0.85,
                            size=0.8,
                            # plot outside of the main map
                            #explained below
                            position=tm_pos_out("right", 
                                                "center",
                                                pos.v = "center")))
library(spdep)
coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

#create a neighbours list
LWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T)
summary(LWard_nb)
#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(points_sf_joined$geometry, add=T)

#create a spatial weights matrix from these weights
Lward.lw <- LWard_nb %>%
  nb2mat(., style="B")

sum(Lward.lw)
sum(Lward.lw[1,])
