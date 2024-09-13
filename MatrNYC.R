source(file = "MatGen.R")

A_NYC <- Matr_adj(nyc_map$geometry)
D_NYC <- Matr_D(A_NYC)

# PlotCheck <-  nyc_map <- sf::read_sf("DatiNewYork/nycdta2020_24a/nycdta2020.shp") %>% 
#                       st_transform(4326) %>% 
#                       subset( select = "geometry")
                                
#   num <- 1:length(PlotCheck$geometry)  
# PlotCheck$num <- num  
# head(PlotCheck)  
#   
# 
# PlotCheck_sf <- st_as_sf(PlotCheck, wkt = "geometry")

# ggplot() +
#   geom_sf(data = PlotCheck_sf) +
#   geom_sf_text(data = PlotCheck_sf, aes(label = num), size = 3, color = "red") 
# 
# graph <- graph_from_adjacency_matrix(Adj_NYC, mode = "undirected")
# 
# plot(graph, layout = layout.circle, vertex.label = V(graph)$name, vertex.size = 10, vertex.label.cex = 0.7)
  #Di difficile lettura
#dev.off()

D_diag(D_NYC)

A_NYC[68,10]
A_NYC[62,58]
A_NYC[67,66]

# la zona 68 e la zona 10 sono indicate come non connesse, nonostante il ponte
  #in maniera analogamente la 62 + 58; non sono connesse anche se c'è un ponte nella realtà
  #allo stesso tempo però notiamo che alcune regioni, come la n. 67 e n. 66 sono erroneamente indicate come confinanti 
#suppongo abbia a che fare con come sono stati definiti i confini dei poligoni o sugli effetti della conversione crs (?)

#modifico  

A_NYC[68,10] <- 1
A_NYC[10,68] <- 1

A_NYC[62,58] <- 1
A_NYC[58,62] <- 1

A_NYC[55, 30] <- 1
A_NYC[30, 55] <- 1

A_NYC[67,66] <- 0
A_NYC[66,67] <- 0
  
  # e ricalcolo la matrice D
D_NYC <- Matr_D(A_NYC)
  D_diag(D_NYC)  
    
  
  
isSymmetric(A_NYC)
  

#non stampa nulla ---> la matrice è simmetrica  
  
  
  
# 
# path <- "DatiNewYork/Mat_A.csv"
# write.csv(Adj_NYC, file = path, row.names = FALSE)
# 
# path <- "DatiNewYork/Mat_D.csv"
# write.csv(D_NYC, file = path, row.names = FALSE)
# 
# rm(list =ls())
#   
  
  
  
  
  