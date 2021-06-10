## ---------------------------
##
## Script name: ccdph_jurisdiction_boundaries.R
## Purpose of script: dynamically create CCDPH boundary using latest TIGER/Line data
## Author: C. Scott Smith, PhD AICP
## Date Created: 2021-06-10
## Date Last Updated: 2021-06-10
## Email: christopher.smith@cookcountyhealth.org
## ---------------------------
##
## Notes: Created during inter-epi meeting
##   
##
## ---------------------------

# Activate R packages
library(tigris) 
library(tidyverse)
library(dplyr) 
library(sf) 


# Download and reproject TIGER/Line geometry data
IL_Places_geom <- places(c("IL"), cb=TRUE, class="sf")
IL_Counties_geom <- counties(c("IL"), cb=TRUE, class="sf")
IL_MCDs_geom <- county_subdivisions(c("IL"), cb=TRUE, class="sf")
IL_Places_geom <- st_transform(IL_Places_geom, crs = 26916)
IL_Counties_geom <- st_transform(IL_Counties_geom, crs = 26916)
IL_MCDs_geom <- st_transform(IL_MCDs_geom, crs = 26916)

# Select out of jurisdiction (OOJ) places (outside of Stickney Township)
ooj_places <- IL_Places_geom %>% 
  filter(#NAME=="Chicago" |
           # NAME=="Bedford Park" |
           # NAME=="Stickney" |
           # NAME=="Burbank" |
           # NAME=="Forest View" |
           NAME=="Evanston" | 
           NAME=="Oak Park" | 
           NAME=="Skokie"
           ) %>%
  select(GEOID_place = GEOID,
         NAME_place = NAME)

# Select Minor Civil Divisions (MCD) Stickney Township and Cook County portion of city of Chicago
ooj_mcds <- IL_MCDs_geom %>% 
  filter(COUNTYFP=="031" &
           (NAME=="Stickney" |
           NAME=="Chicago")) %>%
  select(NAME_mcd = NAME,
         GEOID_mcd = GEOID)

# Union OOJ places with Minor Civil Divisions to create partial clipping mask
ooj_mask_partial <-  ooj_places %>% st_union(ooj_mcds, by_feature = FALSE) %>% st_union(by_feature = FALSE) %>% st_as_sf()

# Select Cook County boundary
cc_boundary <- IL_Counties_geom %>% 
  filter(STATEFP=="17" & COUNTYFP=="031") %>%
  select(GEOID_county = GEOID,
         NAME_county = NAME)

# Use partial mask to clip from Cook County
ccdph_boundary_partial <- st_difference(cc_boundary,
                                        ooj_mask_partial) %>%
  mutate(NAME_ccdph="ccdph_partial") %>%
  select(NAME_ccdph) 

# Select Bedford Park (assuming we include in CCDPH jurisdiction)
bedford_park <- IL_Places_geom %>% 
  filter(NAME=="Bedford Park"
  ) %>%
  select(GEOID_place = GEOID,
         NAME_place = NAME)

# Union partial boundary with Bedford Park to create final CCDPH boundary 
ccdph_boundary <-  ccdph_boundary_partial %>% 
  st_union(bedford_park, by_feature = FALSE) %>% 
  st_as_sf() %>%
  select(-c(NAME_ccdph:NAME_place)) %>%
  mutate(NAME="CCDPH Boundary")

plot(ccdph_boundary)