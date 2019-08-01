######################################################################
####################         Dose map         ########################
######################################################################

  summary(Grids_10km_Sum$OK_AM)
  summary(Grids_10km_Sum$OK_SD)

# New dataframe with AM and SD
  Dose <- Grids_10km_Sum %>% transmute(Id = Id,
                                       Rn_AM = OK_AM,
                                       Rn_SD = OK_SD,
                                       )
  
# Dose [mSv/y] = CRn [Bq/m3] * FE * FO * TY [h/y] * FD [mSv / Bq.h.m-3]
# Uncertainty MC simulations
  nsim <- 100
  MC_Sim <- matrix(NA, nrow = length(Dose$Rn_AM), ncol = nsim)
  TY <- 8760
  for (i in 1:nsim) {
    Rn <- truncnorm::rtruncnorm(length(Dose$Rn_AM), a = 0, b = Inf, mean = Dose$Rn_AM, sd = Dose$Rn_SD)  # truncated:  Rn > 0
    FE <- rlnorm(1, meanlog = log(0.4), sdlog = log(1.15))
    FO <- rnorm(1, 0.8, 0.03)
    FD <- rnorm(1, 9e-06, 1.5e-06)
    MC_Sim[,i] <- Rn * FE * FO * TY * FD 
  } 
  MC_Sim <- as.data.frame(MC_Sim)
  MC_Sim$Id <- Dose$Id
  MC_Sim$Dose_AM <- rowMeans(MC_Sim[,1:nsim])
  MC_Sim$Dose_SD <- apply(MC_Sim[,1:nsim], 1, sd)

# Add AM and SD of the MC simulations to the dose table 
  Dose <- left_join(Dose %>% as.data.frame(), MC_Sim[c("Id","Dose_AM","Dose_SD")] %>% as.data.frame, by = "Id")
  Dose <- Dose %>% st_sf(sf_column_name = "geometry.x")
  
# Dose map
  summary(Dose)
  
  P_Dose_AM <- ggplot() +
    geom_sf(data = Country) + 
    geom_sf(data = Dose, aes(fill = Dose_AM)) +
    scale_fill_gradient(name = "mSv/y", low = "blue", high = "red") + 
    ggtitle("Radiation dose - AM")
  P_Dose_AM
  
  P_Dose_SD <- ggplot() +
    geom_sf(data = Country) + 
    geom_sf(data = Dose, aes(fill = Dose_SD)) +
    scale_fill_gradient(name = "mSv/y", low = "blue", high = "red") + 
    ggtitle("Radiation dose - SD")
  P_Dose_SD
  
  grid.arrange(P_Dose_AM, P_Dose_SD, nrow = 1, ncol = 2)

