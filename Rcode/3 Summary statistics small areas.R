##################################################################
#########    Summary styatistics by grids of 10km x 10 km   ######
##################################################################
  
  library(tidyverse)
  library(sf)
  library(ggplot2)
  library(gstat)

# InRn by grids of 10 x 10 km (summry: N, AM, SD, GM, GSD)
  InRn_DL <- st_intersection(InRn_DL, Grids_10km) 
  str(InRn_DL)
  InRn_DL$Case <- as.numeric(as.character(InRn_DL$Case))
  
  InRn_Summary_Grids10km <- InRn_DL %>% 
    group_by(Id) %>% 
    summarize(N = n()
              , Case = sum(Case)
              , AM = mean(Rn) 
              , SD = sd(Rn) 
              , GM = exp(mean((LogRn))) 
              , GSD = exp(sd(LogRn)) 
              , MIN = min(Rn) 
              , MAX = max(Rn) 
    )  

   
# Add summary to grids of 10x10 km 
  Grids_10km_Sum <- left_join(Grids_10km %>% as.data.frame(), InRn_Summary_Grids10km %>% as.data.frame(), by = "Id")
  Grids_10km_Sum <- Grids_10km_Sum %>% 
    st_sf(sf_column_name = "geometry.x") %>% 
    mutate(N = replace_na(N, 0))
  summary(Grids_10km_Sum)
  
# Plot number of data in each grid cell
  cols <- colorRampPalette(c("blue", "red"))(6)
  P_Grids10km_N <- ggplot() +
      geom_sf(data = Country) + 
      geom_sf(data = Grids_10km_Sum, aes(fill = N)) + 
      scale_fill_gradient(low = "blue", high = "red") +
      ggtitle("Number of data")
  P_Grids10km_N
  
# Plot arithmetic mean
  max(Grids_10km_Sum$AM, na.rm = T)
  breaks <- c(0, 25, 50, 75, 100, 200, max(Grids_10km_Sum$AM, na.rm = T))
  Grids_10km_Sum <- Grids_10km_Sum %>% mutate(AM_brks = cut(AM, breaks, include.lowest = T, right = F))
  cols <- colorRampPalette(c("blue", "red"))(6)
  P_Grids10km_AM <- ggplot() +
      geom_sf(data = Country) + 
      geom_sf(data = Grids_10km_Sum, aes(fill = AM_brks)) + 
      scale_fill_manual(name = "Bq/m3", values = cols, guide = guide_legend(reverse = TRUE)) +
      ggtitle("Arithmetic mean")
  P_Grids10km_AM
  
# Probabilistic maps  
# Probability of having an indoor radon concentration above the national reference level (e.g. RL = 200 Bq m-3)
# Based solely on the indoor radon measurements in each grid of 10x10 km,
# and assuming data independence and lognormality
  str(Grids_10km_Sum)
  ## 1st: estimated Prob[InRn > 200 Bq m-3]
      RL <- 200
      Grids_10km_Sum <- Grids_10km_Sum %>%
          mutate(Prob = 100*(1-pnorm(log(RL), mean = log(GM), sd = log(GSD)))) 
  ## 2nd: Replace values in grids with less than n data (i.e. N < 5) by a modeled value
      # Generate the points for the interpolation (centroid)
        DCen <- st_centroid(Grids_10km_Sum)
      # Inverse distance weighted (IDW) interpolation
        Prob_IDW <- idw(Prob ~ 1, filter(DCen, N > 5),
                    newdata = filter(DCen, N <= 5),
                    nmax = 10,
                    idp = 2 )
      # Replace values by the modeled values
        Grids_10km_Sum <- Grids_10km_Sum %>% mutate(Prob = replace(Prob, N <= 5, Prob_IDW$var1.pred))
          
  ## Plot maps
      breaks <- c(0, 1, 5, 10, 20, 30, max(Grids_10km_Sum$Prob))
      Grids_10km_Sum <- Grids_10km_Sum %>% mutate(Prob_brks = cut(Prob, breaks, include.lowest = T, right = F))
      cols <- colorRampPalette(c("blue", "red"))(6)
      P_Grids10km_Prob <- ggplot() +
        geom_sf(data = Country) + 
        geom_sf(data = Grids_10km_Sum, aes(fill = Prob_brks)) + 
        scale_fill_manual(name = "%", values = cols, guide = guide_legend(reverse = TRUE)) +
        ggtitle("Prob[InRn > 200 Bq m-3]")
      P_Grids10km_Prob
  
