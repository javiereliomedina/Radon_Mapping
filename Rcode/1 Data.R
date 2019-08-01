######################################################
#################   Read data   ######################
######################################################
 
  library(tidyverse)
  library(sf)

# Administrative divisions (from www.gadm.org) ####
# I got Lithuania but you may download the administrative areas of other countries  
  Country <- readRDS(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_LTU_0_sf.rds"))
  County  <- readRDS(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_LTU_1_sf.rds"))
  Muni    <- readRDS(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_LTU_2_sf.rds"))
  
  plot(st_geometry(Country), border = "blue", col = "grey", main = "Country")
  plot(st_geometry(County),  border = "blue", col = "grey", main = "Counties")
  plot(st_geometry(Muni),    border = "blue", col = "grey", main = "Municipalities")

# Grids 10 x 10 km ####
  
  # Download from https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2
    EEA_Ref_grid_URL <- "https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/lithuania-shapefile/at_download/file.zip"
    temp  <- tempfile()
    temp2 <- tempfile()
    download.file(EEA_Ref_grid_URL, temp)
    unzip(zipfile = temp, exdir = temp2)
    Grids_10km <- read_sf(file.path(temp2, "lt_10km.shp"))
    unlink(c(temp, temp2))
    
    Grids_10km$Id <- seq(1, length(Grids_10km$CELLCODE), 1)   # Add new column with "Id"
    Grids_10km$Id <- as.factor(Grids_10km$Id)                 # Stored as a vector of integer values
    st_crs(Grids_10km)                                        # Coordinate Reference System
    st_crs(Country)                                           # Coordinate Reference System
    Grids_10km <- Grids_10km %>% st_transform(4326)           # Trandform coordinate system (from  EPSG: 3035 to EPSG: 4326)
    st_crs(Grids_10km)                                        # Coordinate Reference System 
    st_crs(Country)                                           # Coordinate Reference System 
    Grids_10km <- st_intersection(Grids_10km, Country)        # Grids in the country   
    plot(Grids_10km["Id"])
    
  # Make our own grid (e.g. 0.1 x 0.1 degrees)
    Grids <- st_make_grid(Country, cellsize = 0.1, what = "polygons") %>%
      st_sf() %>%
      st_intersection(Country) %>%
      mutate(Id = seq(1, dim(Grids)[1], 1))
    SPDF  <- st_centroid(Grids) # Centroid of the grid
    plot(Country["NAME_0"], reset = F, main = "Lithuania: cells 0.1x0.1 degrees")
    plot(Grids, add = T, border = 2)
    plot(SPDF, add = T, col = "blue")
    
  # Another way with sp (What is the difference?))
    library(sp)
    SPDF  <- spsample(as_Spatial(Country), 1000, cellsize = c(0.1, 0.1), type = "regular")  # Centroid of the grid
    SPiDF <- SpatialPixels(SPDF)                                                            # Transfor to SpatialPixel
    Grids <- as(SPiDF, "SpatialPolygons")                                                   # Transfor to a SpatialPolygons
    Grids <- as(Grids, "sf") 
    plot(Country["NAME_0"], reset = F, main = "Lithuania: cells 0.1x0.1 degrees")
    plot(Grids, add = T, border = 2)
    plot(SPDF, add = T, col = "blue")
 
# Geology 1:5M ####
  # Asch, K. (2005): IGME 5000: 1 : 5 Million International Geological Map 
  # of Europe and Adjacent Areas - final version for the internet.- BGR, Hannover
  # https://produktcenter.bgr.de/terraCatalog/DetailResult.do?fileIdentifier=9FD6624C-0AA7-46D4-9DA3-955E558CD5F1 
    
    IGME5000_path <- "C:/Users/elioj/Documents/GitHub/Radon_Mapping/Rdata/IGME5000"  # WARNING My local path, you may need to put yours
    IGME5000 <- read_sf(file.path(IGME5000_path, "IGME5000_europeEPSG3034shp_geology_poly_v01.shp"))
    st_crs(IGME5000)
    IGME5000 <- IGME5000 %>%
      st_transform(4326) %>%
      st_intersection(Country)
    st_crs(IGME5000)
    plot(IGME5000["AgeName"])
        
# I will focus however the data analysis in a region of of 1x1 degrees
  # First: build a rectangle  
    Area <- matrix(NA, ncol = 2, nrow = 4)
    Area <- as.data.frame(Area)
    names(Area)<-c("X","Y")
    Area[1,]<-c(23,55)
    Area[2,]<-c(23,56)
    Area[3,]<-c(24,56)
    Area[4,]<-c(24,55)
    
    coordinates(Area)<- ~X+Y
    Area <- rbind(Area,Area[1,])
    Area <- Polygons(list(Polygon(Area)),ID="Area")
    Area <- SpatialPolygons(list(Area))
    Area <- as(Area, "sf")
    st_crs(Area) <- st_crs(Country)
    
    plot(Country["NAME_0"], axes = TRUE, reset = F)
    plot(Area, col = "blue", add = T)
  # Second: intersect Area with all the data ####
    Country <- st_intersection(Country, Area)
    County <- st_intersection(County, Area)
    Muni <- st_intersection(Muni, Area)
    Grids_10km <- st_intersection(Grids_10km, Area)
    IGME5000 <- st_intersection(IGME5000, Area)
  
# Indoor radon (simulated data)
# Please be aware that I am using SIMULATED data, and therefore data interpretation is NOT real. Any coincidence with a real case (i.e. Lithuania) is casual.  
# Data are only useful for training purpose, you may need to read your own data for data interpretation.  
  
  set.seed(1)  # Make the simulation reproducible

  # Radom points in the study area 
  N <- 1000
  X <- runif(N,23.0001,23.9999)
  Y <- runif(N,55.0001,55.9999)
  points <- cbind(X,Y)
  points <- as.data.frame(points)
  coordinates(points) <- ~X+Y
  proj4string(points) <- CRS("+init=epsg:4326")
  points <- as(points, "sf")
  points <- st_intersection(points, Country)
  points <- as_Spatial(points)                
  # define the gstat object (spatial model)
  library(gstat)
  g_dummy <- gstat(formula = z ~ 1
                   , locations = ~ X + Y
                   , dummy = T
                   , beta = 3
                   , model = vgm(psill = 1.5, model = "Exp", range = 10, nugget = 0.5)
                   , nmax  = 100
                    )
  # Simulations based on the gstat object
  points <- predict(g_dummy, newdata = points, nsim = 1)
  points$Rn    <- exp(points$sim1)
  # Final result: Simulated indoor radon dataset (InRn) in Bq m-3
  InRn <- points[,"Rn"]
  InRn[InRn$Rn <= 10,] <- 5   # Detection Limit (DL): 10 Bq m-3 (replaced by half of the Limit of Detection)
  InRn <- as(InRn, "sf")
  