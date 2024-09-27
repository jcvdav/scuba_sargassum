################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Divide the Mexican Caribbean into zones.
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse,
  rnaturalearth,
  sf
)

# Load data --------------------------------------------------------------------
mex <- ne_countries(country = "Mexico", scale = "large") %>% 
  select(sov_a3)

mex_eez <- st_read(dsn = "../data_mex_fisheries/data/spatial_features/raw/World_EEZ_v12_20231025_gpkg/eez_v12.gpkg") %>% 
  select(ISO_SOV1) %>% 
  filter(ISO_SOV1 == "MEX") %>% 
  st_transform(crs = "+proj=lcc +lat_0=12 +lon_0=-102 +lat_1=17.5 +lat_2=29.5 +x_0=2500000 +y_0=0")

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
# Isla Mujeres (21.2334718,-86.7709721) in the Nort
# Boca Paila (20.031666,-87.4880774) in the South
bbox <- c(xmin = -87.5,
          xmax = -86.5,
          ymin = 20,
          ymax = 21.23)

# X ----------------------------------------------------------------------------
mex_crop <- st_crop(mex, bbox) %>% 
  st_transform(crs = "+proj=lcc +lat_0=12 +lon_0=-102 +lat_1=17.5 +lat_2=29.5 +x_0=2500000 +y_0=0") %>%   # https://epsg.io/6361
  st_cast("POLYGON") %>% 
  mutate(area = st_area(.)) %>% 
  filter(area == max(area))

# Define cutoffs
c1 <- st_linestring(cbind(c(bbox[1], bbox[2]),
                          c(20.35, 20.35))) %>%
  st_sfc() %>%
  st_set_crs(value = 4326)

c2 <- st_linestring(cbind(c(bbox[1], bbox[2]),
                          c(20.55, 20.55))) %>%
  st_sfc() %>%
  st_set_crs(value = 4326)

c3 <- st_linestring(cbind(c(bbox[1], bbox[2]),
                          c(20.73, 20.73))) %>%
  st_sfc() %>%
  st_set_crs(value = 4326) 

c4 <- st_linestring(cbind(c(bbox[1], bbox[2]),
                          c(20.98, 20.98))) %>%
  st_sfc() %>%
  st_set_crs(value = 4326)

poly <- cbind(c(bbox[1], bbox[1], bbox[2], bbox[2], bbox[1]),
              c(bbox[3], bbox[4], bbox[4], bbox[3], bbox[3])) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_set_crs(value = 4326) %>% 
  st_as_sf() %>% 
  lwgeom::st_split(c1) %>% 
  st_collection_extract("POLYGON") %>% 
  lwgeom::st_split(c2) %>% 
  st_collection_extract("POLYGON") %>% 
  lwgeom::st_split(c3) %>% 
  st_collection_extract("POLYGON") %>% 
  lwgeom::st_split(c4) %>% 
  st_collection_extract("POLYGON") %>% 
  st_transform(crs = "+proj=lcc +lat_0=12 +lon_0=-102 +lat_1=17.5 +lat_2=29.5 +x_0=2500000 +y_0=0")

mex_buf <- st_buffer(mex_crop,
                     dist = units::as_units(6, "nautical_miles"))

area <- st_intersection(mex_buf, poly) %>% 
  st_intersection(mex_eez) %>% 
  st_transform(crs = 4326) %>% 
  mutate(id = paste("Pol", 1:nrow(.)),
         name = c("TLM",
                  "PAV",
                  "PDC",
                  "PMR",
                  "CUN")) %>% 
  mutate(area_km2 = units::set_units(st_area(.), "km2")) %>% 
  select(name, area_km2) 

## VISUALIZE ###################################################################

# Mapview ----------------------------------------------------------------------
mapview::mapview(area, zcol = "name")


## EXPORT ######################################################################
# Save -------------------------------------------------------------------------
st_write(obj = area,
         dsn = here("data", "processed", "study_area_polygons.gpkg"),
         delete_dsn = T)
