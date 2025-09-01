# source of code: https://doi.org/10.17605/OSF.IO/PWDSG

library(sf)
library(dplyr)
library(mapview)

# load data
# data source PADUS4: https://www.usgs.gov/programs/gap-analysis-project/science/pad-us-data-download
padus <- sf::st_read("shp_PADUS4_all_geom_validated.shp")

# check geometries
summary(st_geometry(padus)) # some are multi-surface

# make geometry type consistent
padus3 <- st_cast(padus, "MULTIPOLYGON")

# check geometries
summary(st_geometry(padus3)) # should all be MULTIPOLYGON

# overcome invalid geometry
# padus4 <- st_make_valid(padus3)

# remove marine feature class
padus6 <- padus3[padus3$FeatClass != "Marine", ]

# exclude proclamation class
padus7 <- padus6[padus6$FeatClass != "Proclamation", ]

# exclude closed access
padus8 <- padus7[padus7$Pub_Access != "XA", ]

# exclude unknown access select land managers
UK_BadLandMang <-
  list(
    "UK_ARS",
    "UK_BLM",
    "UK_DOD",
    "UK_DOE",
    "UK_FWS",
    "UK_JNT",
    "UK_NGO",
    "UK_NOAA",
    "UK_NPS",
    "UK_NRCS",
    "UK_OTHF",
    "UK_OTHS",
    "UK_PVT",
    "UK_RWD",
    "UK_SLB",
    "UK_TRIB",
    "UK_UNK",
    "UK_UNKL",
    "UK_USBR"
  )
padus8 <- padus8 %>%
  mutate(Pub_Access_Mang_Name = paste(Pub_Access, sep = "_", Mang_Name))
padus9 <-
  padus8 %>% filter(!Pub_Access_Mang_Name %in% UK_BadLandMang)

# exclude select SMRA areas
SRMA_BadStates <-
  list(
    "SRMA_MS",
    "SRMA_OK",
    "SRMA_ND",
    "SRMA_SD",
    "SRMA_MT",
    "SRMA_ID",
    "SRMA_WY",
    "SRMA_UT",
    "SRMA_WA",
    "SRMA_OR",
    "SRMA_AZ",
    "SRMA_NM",
    "SRMA_TX",
    "SRMA_CO",
    "SRMA_LA",
    "SRMA_NV"
  )
padus9 <- padus9 %>%
  mutate(Des_Tp_State_Nm = paste(Des_Tp, sep = "_", State_Nm))
padus10 <- padus9 %>% filter(!Des_Tp_State_Nm %in% SRMA_BadStates)

# save to final data
st_write(padus10, "PADUS4_AR.gpkg")
