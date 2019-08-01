########################################################################
#################    Intercative maps, some examples    ################
########################################################################

  library(leaflet)

# Name of municipalities
  Lithuania <- readRDS(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_LTU_2_sf.rds"))
  p_popup <- paste("<strong> Municipality: </strong>", Lithuania$NAME_2)  
  Map <- leaflet() %>%
    addPolygons(data = Lithuania
                , stroke = TRUE, weight = 2
                , fillColor = "grey"
                , fillOpacity = 0.5
                , popup = p_popup
    ) %>%
    addTiles()
  Map

# Plot the AM (of the OK predictions) by grid cells of 10 km x 10 km (Grids_10km_Sum)
  p_popup <- paste("<strong> Id: </strong>", Grids_10km_Sum$Id, "<br>"
                   , "<strong> AM: </strong>", round(Grids_10km_Sum$OK_AM, 2), "Bq/m3")
  pal_fun <- colorQuantile("YlOrRd", NULL, n = 6)
  Map <- leaflet() %>%
    addTiles()  %>%
    addPolygons(data = Grids_10km_Sum 
                , stroke = TRUE, col = "grey", weight = 2     # add/remove polygon borders
                , fillColor = ~pal_fun(Grids_10km_Sum$OK_AM)  # set fill color with function from above and value
                , fillOpacity = 0.8, smoothFactor = 0.5       # make it nicer
                , popup = p_popup
    )
  Map

# Plot the OK predictions by grid cells of 0.01 x 0.01 degrees (InRn_Pred)
  p_popup <- paste("<strong> InRn: </strong>", round(InRn_Pred$OK_Pred), "Bq/m3")
  pal_fun <- colorBin("YlOrRd", InRn_Pred$OK_Pred, bins = c(0,50,100, 200, 300, 500, max(InRn_Pred$OK_Pred)))
  Map <- leaflet() %>%
    addTiles()  %>%
    addPolygons(data = InRn_Pred 
                , stroke = TRUE, col= "grey", weight = 2     # add/remove polygon borders
                , fillColor = ~pal_fun(InRn_Pred$OK_Pred)  # set fill color with function from above and value
                , fillOpacity = 0.8, smoothFactor = 0.5       # make it nicer
                , popup = p_popup
    ) %>%
    addLegend(data = InRn_Pred, pal = pal_fun, values = ~OK_Pred
              , opacity = 1
              , labFormat = labelFormat(prefix = "[", suffix = ")", between = ", ", digits = 0)
              , title = "InRn [Bq/m3]"
               
    )
  Map
  
