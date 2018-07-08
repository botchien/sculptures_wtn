
setwd("G:/ex/crossfilter_ex")


library(sf)
library(tidyverse)

AU <- st_read("data_raw/statsnzarea-unit-2015-v1-00-clipped-SHP/area-unit-2015-v1-00-clipped.shp")

dat <- st_read("data_raw/Wellington_City_Sculptures/Wellington_City_Sculptures.shp", stringsAsFactors = FALSE)


dat <- dat %>% select(site_name, sculpture, artist, year_installed = installed, location, geometry)

dat <- dat %>% mutate(artist = ifelse(is.na(artist), "unknown", artist))

dat <- dat %>% mutate(site_name = ifelse(is.na(site_name), "unknown", site_name))

dat <- dat %>% mutate(sculpture = ifelse(is.na(sculpture), "unknown", sculpture))

dat <- dat %>% mutate(year_installed = ifelse(is.na(year_installed), "unknown", year_installed))

dat <- dat %>% mutate(year_installed = ifelse(year_installed == "199", "1999", year_installed))

dat <- dat %>% filter(artist != "unknown")

dat$year_installed <- as.integer(dat$year_installed)

dat$year_installed <- as.Date(paste0(dat$year_installed, "-01-01"))

dat <- dat %>% st_intersection(AU %>% st_transform(4326) %>% select(au_name = AU2015_V_1))

# fn to convert sf column to x and y
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}



sfc_as_cols(dat) %>% data.frame %>% select(-geometry) %>% 
    jsonlite::toJSON(pretty=TRUE) %>% write(file = "www/data/sculpture.json")
