# jag vill göra en karta över sverige med kommuner och färglägga dem efter antal invånare
library(sf)
library(dplyr)
library(mapview)
library(raster)
# läs in data från projektet C:\Users\henri\r\test_sf

deso <- st_read("C:/Users/henri/r/test_sf/DESO_2018_v2.gpkg")

# skapa kommunpolygoner från deso områden med group_by på kommuner

kommuner <- deso %>% group_by(kommun) %>% summarise()
mapview(kommuner)

#välj ut skåne med ett filter

skane <- deso %>% filter(lannamn == "Skåne")
mapview(skane)

# skapa kommunpolygoner från skåne områden med group_by på kommuner

skane_kommuner <- skane %>% group_by(kommun) %>% summarise()
mapview(skane_kommuner)

#
# läs in TIFF från C:\Users\henri\r\skane\data\ 61_3_2023 och 61_4_2023.json

vastra_skane_fil <- "C:/Users/henri/r/skane/data/61_3_2023.tif"
ostra_skane_fil <- "C:/Users/henri/r/skane/data/61_4_2023.tif"

vastra_skane <- raster(vastra_skane_fil)
ostra_skane <- raster(ostra_skane_fil)

# Assuming 'raster_data' is your loaded raster layer
# Define breaks and colors
# breaks <- c(-Inf, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 50, 100, Inf)
breaks <- c(-Inf, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,15, 20,25, 30,35, 40, 45, 50, 60, 70, 80, 90, 100, Inf)
colors <- c("darkblue", "blue", "lightblue", "lightgreen", "green", "darkgreen", "brown", "yellow")

# Create an sf object with the point geometry
Figges_hus <- st_sf(geometry = st_sfc(st_point(c(377014, 6164939)), crs = 3006))

mapview(vastra_skane, col.regions = colors, at = breaks, alpha.regions = 0.2)+
  mapview(ostra_skane, col.regions = colors, at = breaks, alpha.regions = 0.2)+
  mapview(Figges_hus, col.regions = 'pink', pch = 20, cex = 10, popup = 'Figges hus')

# läs in naturvard_ln12.gpkg från C:\Users\henri\r\data\naturvard_ln12

naturvard_fil <- "C:/Users/henri/r/data/naturvard_ln12/naturvard_ln12.gpkg"

st_layers(naturvard_fil)
skyddad_natur <- st_read(naturvard_fil, layer = "skyddadnatur")

mapview(skyddad_natur, zcol = "objekttyp")

# hämta tätorter Tatorter_1980_2020.gpkg från C:\Users\henri\r\data\tatorter_1980_20202 och 
# småorter Smaorter_1990_2020.gpkg från C:\Users\henri\r\data\smaorter_1990_2020

tatort_fil <- "C:/Users/henri/r/data/tatorter_1980_20202/Tatorter_1980_2020.gpkg"
tatort <- st_read(tatort_fil, layer = "To2020_SR99TM")

tatort <- tatort %>% filter(LAN == "12") |> 
  st_centroid()

smaort_fil <- "C:/Users/henri/r/data/smaorter_1990_2020/Smaorter_1990_2020.gpkg"
smaort <- st_read(smaort_fil, layer = "So2020_SR99TM")

smaort <- smaort %>% filter(LAN == "12") |> 
  st_centroid()

mapview(tatort, col.regions = "red", alpha.regions = 0.5, legend = FALSE, hide =TRUE, homebutton = FALSE, cex = "BEF", label = "TATORT")+
  mapview(smaort, zcol = "SMAORT", col.regions = "pink", alpha.regions = 0.5, legend = FALSE, hide =TRUE, homebutton = FALSE, cex = 2.5)+
  mapview(skyddad_natur, zcol = "objekttyp", hide =TRUE, legend = FALSE, homebutton = FALSE)+
  mapview(Figges_hus, col.regions = 'pink', pch = 10, cex = 10, popup = 'Figges hus', legend = FALSE)+
  mapview(vastra_skane, col.regions = colors, at = breaks, alpha.regions = 0.3, legend = FALSE, homebutton = FALSE)+
  mapview(ostra_skane, col.regions = colors, at = breaks, alpha.regions = 0.3, homebutton = FALSE)




