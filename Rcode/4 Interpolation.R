##########################################################
###################   Interpolation     ##################
##########################################################

  library(sf)
  library(gstat)
  library(gridExtra)
  library(tidyverse)


# Prediction by grids of 0.015 x 0.015 degrees
  InRn_Pred <- st_make_grid(Country, cellsize = .015, what = "polygons")
  InRn_Pred <- st_sf(InRn_Pred)
  SPDF <- st_centroid(InRn_Pred) # predict in the center of the grid 
  
  plot(InRn_Pred, reset = F)
  plot(SPDF, add = T, col = 2, pch = 16, cex = 0.7)
    
################################################################
#####     Inverse distance weighted interpolation (IDW)    #####
################################################################

## Optimal idp (10 k-fold Croos Validation)
    data <- InRn_DL
    res <- numeric()
    IDP_RMSE <- numeric()
    idp <- seq(1,4,0.25)
    k <- 10
    folds <- sample(x = 1:k, size = length(data$Rn), replace = TRUE)
    CV <- matrix(NA,length(idp),2)
    for (j in 1:length(idp)) {
      for (i in 1:k) {
        m_model <- data[folds != i, ]
        m_valid <- data[folds == i, ]
        m_valid_pred <- idw(Rn ~ 1 , m_model, m_valid
                            , nmax = 100
                            , maxdist = 40
                            , nmin = 5
                            , idp = idp[j]
        )
        res  <- m_valid$Rn - m_valid_pred$var1.pred 
        IDP_RMSE[i] <- sqrt(mean(res^2))
      } 
      CV[j,1] <- idp[j]
      CV[j,2] <- sqrt(mean(IDP_RMSE^2))
    }
    CV <- as.data.frame(CV)
    names(CV) <- c("idp","RSME")
    
    plot(RSME ~ idp, CV)
    lines(RSME ~ idp, CV, col = 2)
    axis(1, labels = F, at = seq(0,10,0.5))
    abline(v = seq(0,4,0.25), lty = 2, lwd = 1.5, col="gray")
    abline(h = seq(100,200,2), lty = 2, lwd = 1.5, col = "gray")
    title("10-fold cross-validation")
  
## IDW (optimal idp = 2)
    Pred_IDW <- idw(Rn ~ 1 , InRn_DL, SPDF
                    , nmax = 100
                    , maxdist = 40 # km; Unprojected data: great-circle distance; For projected data Euclidian distances are computed 
                    , nmin = 5
                    , idp = 2)
    SPDF$IDW_Pred <- Pred_IDW$var1.pred
    
## Plot map (grids cells of 0.015 x 0.015 degrees)
      InRn_Pred$IDW_Pred <- Pred_IDW$var1.pred
    # Breaks 
      breaks <- c(0, 50, 100, 200, 300, 500, max(InRn_Pred$IDW_Pred, na.rm = T))
      InRn_Pred <- InRn_Pred %>% mutate(IDW_brks = cut(IDW_Pred, breaks, include.lowest = T, right = F))
    # Plot predictions
      cols <- colorRampPalette(c("blue", "red"))(6)
      P_IDW_Pred <- ggplot() +
        geom_sf(data = Country) +
        geom_sf(data = InRn_Pred, aes(fill = IDW_brks, color = IDW_brks)) + 
        scale_fill_manual(name = "Bq/m3", values = cols, guide = guide_legend(reverse = TRUE)) +
        scale_color_manual(name = "Bq/m3", values = cols, guide = guide_legend(reverse = TRUE)) +
        geom_sf(data = InRn_DL, cex = 0.5) +
        ggtitle("IDW - Predictions") 
      P_IDW_Pred

#########################################################    
##########       Ordinary Kriging        ################
#########################################################
    
## Variogram (gstat)
    vg <- variogram(LogRn ~ 1, InRn_DL)   # great-circle distances (km)
    vg_fit_Sph <- fit.variogram(vg, vgm("Sph"))
    vg_fit_Exp <- fit.variogram(vg, vgm("Exp"))
    plot(gamma~dist, vg, ylim = c(0, 1.05*max(vg$gamma)), col = 1
         , ylab ="semivariance", xlab = 'distance', main = "Variogram")
    lines(variogramLine(vg_fit_Sph, 100), col = 'red') 
    lines(variogramLine(vg_fit_Exp, 100), col = 'blue')
    vg_fit <- vg_fit_Exp
    
    # Test: Random permutations (100 random variograms)
      V_Env <- list()
      nsim <- 100
      for (i in 1:nsim) {
        RP <- InRn_DL
        RP$LogRn <- sample(InRn_DL$LogRn)
        g_RP <- gstat(formula = LogRn ~ 1, data = RP) 
        vg_RP <- variogram(g_RP, cressie = F)
        V_Env[[i]] <- vg_RP 
      }
      
      vg_fit_Table <- data.frame(Model = vg_fit$model
                                 , psill = round(vg_fit$psill,2)
                                 , range = round(vg_fit$range,2)
                                 #, kappa = round(vg_fit$kappa,2)
                                  )
      vg_Line <- cbind(variogramLine(vg_fit, maxdist = max(vg$dist)), id = "Model")
      
      ggplot(vg, aes(x = dist, y = gamma, colour = id)) +
        geom_line(data = bind_rows(V_Env, .id="df"), aes(x = dist, y = gamma, group = df), colour="grey") +
        geom_point() +
        geom_line(data = vg_Line, size = 0.8) +
        ylim(0,2.5) + 
        annotation_custom(tableGrob(vg_fit_Table,rows = NULL)
                          , xmin = 20, xmax = 45, ymin = 0.1, ymax = 1) + 
        ggtitle("Variogram")

## Interpolation - TransGaussian kriging using Box-Cox transforms (gstat)
    SPDF <- as_Spatial(SPDF)
    InRn_DL <- as_Spatial(InRn_DL)
    lambda <- 0
    Pred_OK <- krigeTg(Rn ~ 1, InRn_DL, SPDF
                       , model = vg_fit
                       , lambda = lambda
                       , maxdist = 40  # km (great-circle distances)
                       , nmax = 100
                       , nmin = 5
    )   
    SPDF$OK_Pred <- Pred_OK$var1TG.pred   
    SPDF$OK_SD <- sqrt(Pred_OK$var1TG.var)
    SPDF$OK_RSD <- sqrt(Pred_OK$var1TG.var)/Pred_OK$var1TG.pred

## Plot predictions (grids cells of 0.025 x 0.025 degrees)
    InRn_Pred$OK_Pred <- Pred_OK$var1TG.pred   
    InRn_Pred$OK_SD <- sqrt(Pred_OK$var1TG.var)
    InRn_Pred$OK_RSD <- sqrt(Pred_OK$var1TG.var)/Pred_OK$var1TG.pred
    # Back to sf format
      InRn_DL <- as(InRn_DL, "sf")
      SPDF <- as(SPDF, "sf")
    # Breaks 
      breaks <- c(0, 50, 100, 200, 300, 500, max(InRn_Pred$OK_Pred, na.rm = T))
      InRn_Pred <- InRn_Pred %>% mutate(OK_brks = cut(OK_Pred, breaks, include.lowest = T, right = F))
    # Plot predictions
      cols <- colorRampPalette(c("blue", "red"))(6)
      P_OK_Pred <- ggplot() +
        geom_sf(data = Country) +
        geom_sf(data = InRn_Pred, aes(fill = OK_brks, color = OK_brks)) + 
        scale_fill_manual(name = "Bq/m3", values = cols, guide = guide_legend(reverse = TRUE)) +
        scale_color_manual(name = "Bq/m3", values = cols, guide = guide_legend(reverse = TRUE)) +
        geom_sf(data = InRn_DL, cex = 0.5) +
        ggtitle("OK - Predictions")
      P_OK_Pred
    
#######################################################################
################   Summarize by grids cells of 10 km x 10 km      #####
#######################################################################
  
## Estimate the mean of the points in each grid cell of 10 km x 10 km (or municipalities, districts, ...)  
    ggplot() +
      geom_sf(data = Country) +
      geom_sf(data = Grids_10km)+
      geom_sf(data = SPDF, col = 2, cex = 0.7)
    
## Intersect predictions (points - SPDF) and Grid cells of 10km x 10km (Grids_10km)  
    SPDF_Grids10km <-  st_intersection(SPDF, Grids_10km) 
    summary(SPDF_Grids10km)

## Sumarize (by Id)
    SPDF_Grids10km_Sum <- SPDF_Grids10km %>% 
      group_by(Id) %>% 
      summarize(N = n(),
                IDW_AM = mean(IDW_Pred, na.rm = T),
                IDW_SD = sd(IDW_Pred, na.rm = T),
                OK_AM  = mean(OK_Pred, na.rm = T), 
                OK_SD  = sd(OK_Pred, na.rm = T)
      )  
  
## Add values to Grid cells of 10 x 10 km (Grids_10km_Sum) 
    Grids_10km_Sum <- left_join(Grids_10km_Sum %>% as.data.frame(), SPDF_Grids10km_Sum %>% as.data.frame(), by = "Id")
    Grids_10km_Sum <- Grids_10km_Sum %>% st_sf(sf_column_name = "geometry.x")
    summary(Grids_10km_Sum)   
    
## Plot maps
    # Breaks 
      breaks_IDW <- c(0, 25, 50, 75, 100, 200, max(Grids_10km_Sum$IDW_AM, na.rm = T))
      breaks_OK  <- c(0, 25, 50, 75, 100, 200, max(Grids_10km_Sum$OK_AM, na.rm = T))
      Grids_10km_Sum <- Grids_10km_Sum %>% 
          mutate(IDW_brks = cut(IDW_AM, breaks_IDW, include.lowest = T, right = F),
                 OK_brks  = cut(OK_AM , breaks_OK , include.lowest = T, right = F)
                 )
    # Maps 
      P_Grids10km_IDW <- ggplot() +
          geom_sf(data = Country) + 
          geom_sf(data = Grids_10km_Sum, aes(fill = IDW_brks)) + 
          scale_fill_manual(name = "Bq/m3", values = cols, guide = guide_legend(reverse = TRUE)) +
          ggtitle("IDW - Predictions")
      P_Grids10km_OK <- ggplot() +
          geom_sf(data = Country) + 
          geom_sf(data = Grids_10km_Sum, aes(fill = OK_brks)) + 
          scale_fill_manual(name = "Bq/m3", values = cols, guide = guide_legend(reverse = TRUE)) +
          ggtitle("OK - Predictions")
    # Plot maps
      grid.arrange(P_Grids10km_AM, P_Grids10km_IDW, P_Grids10km_OK, nrow = 1, ncol = 3)

      
## Export results to shape file (.shp) for GIS: InRn_Pred and  Grids_10km_Sum
      st_write(InRn_Pred, "InRn_Pred.shp", delete_layer = TRUE)           # overwrites
      st_write(Grids_10km_Sum, "Grids_10km_Sum.shp", delete_layer = TRUE) # overwrites
      
