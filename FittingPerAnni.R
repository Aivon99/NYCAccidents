##File unico per modificare dati e fittare modello 
library(rstan)
library(tidyverse)
library(bayesplot)
library(lmtest)
#library(cmdstanr)
library(ggplot2)
library(tmap) 
library(sf)
library(Matrix)

#source("GraphReformat.R")

# DatiModello <- read_csv("DatiNewYork/DatiModello.csv")
# #Per i valori attesi prendo il df con i metri di strada per ogni zona e droppo i vecchi Y_i
# tot_mt <- sum(DatiModello$`Mt Strada`)

nyc_map <- sf::read_sf("DatiNewYork/nycdta2020_24a/nycdta2020.shp") %>% 
      st_transform(4326)

#Importo i dati precedentemente subsettati, droppo colonne inutili e elementi senza data e rendo df 
path <- "DatiNewYork/Deadly_crashes.csv"
crash_data <- read_csv(file = path) %>% 
  drop_na( ,"CRASH DATE") %>% 
  subset( ,select = c("CRASH DATE","LATITUDE", "LONGITUDE", "LOCATION")) %>% 
  as.data.frame( ) %>% 
  mutate(`CRASH DATE` = mdy(`CRASH DATE`))

crash_data <- st_as_sf(crash_data , coords =  c("LONGITUDE", "LATITUDE"), crs = 4326) 

# istogramma incidenti nel tempo
ggplot(crash_data, aes(x = `CRASH DATE`)) +
  geom_histogram(bins = 40, fill = "lightblue", color = "black") +
  labs(title = "",
       x = "Date",
       y = "N Incidenti") +
  theme_minimal()


range(crash_data$`CRASH DATE`)
#Subsetto, provo per ultimi 5 anni (il totale era ultimi 10 anni) 
crash_data_subset <- crash_data %>%
    filter(`CRASH DATE` > as.Date("2020-01-01")) %>%
     filter(`CRASH DATE` < as.Date("2022-01-01"))


      #crash_data_subset <- st_as_sf(crash_data_subset, coords =  c("LONGITUDE", "LATITUDE"), crs = 4326)
      #Faccio Conteggi per zona
      counts_SBS <- st_intersects(nyc_map, crash_data_subset)
      nyc_map$count <- lengths(counts_SBS)
        rm(counts_SBS)
        ####################################MAPPA
         mapthis <- subset(nyc_map, select = c("count"))
        colnames(mapthis)[1] <- "Conteggio"
        fig <- tm_shape(mapthis) +
          tm_fill(col = "Conteggio", palette = "Blues", style = "quantile") +
          tm_borders() +
          tm_layout(legend.outside = TRUE,
                    legend.outside.position = "right",
                    #legend.outside.size = 0.2,
                    legend.title.size = 1.2,
                    legend.text.size = 0.8,
                    legend.bg.color = "white",
                    legend.frame = TRUE)
      
        fig
        ###############################################à
        nyc_Network <- sf::read_sf("DatiNewYork/NYC_roadNetwork/DCM_StreetCenterLine.shp") %>%
          st_transform(4326)  
        # dati presi da https://www.nyc.gov/site/planning/data-maps/open-data/dwn-digital-city-map.page
        tot_road_m <- sum(st_length(nyc_Network))
        nyc_Network <- subset(nyc_Network, select = c("Borough","Street_NM", "geometry", "Streetwidt"))
        
        
        
        nyc_map %>%  count(CDTA2020) %>% filter(n > 1) 
        #CDTA2020 è univoco
        
        nyc_Network %>%  group_by(Street_NM) %>% filter(n() > 1)
        #View(nyc_Network)
        
        
        data_mt <- data.frame(matrix(nrow = nrow(nyc_map), ncol = 2))
        colnames(data_mt) <- c("CDTA2020", "Mt Strada")
        
        for(i in 1:length(nyc_map$CDTA2020)){
          result <- st_intersection(nyc_Network$geometry, nyc_map$geometry[i])
          data_mt[i, 1] <- nyc_map$CDTA2020[i]
          data_mt[i,2] <- sum(st_length(result))
        }
        #########################################################################
        
data_df <- subset(nyc_map, select= c(CDTA2020, Shape_Area, count)) 
        
  data_df <-  full_join(data_df, data_mt, by = "CDTA2020")

  
  tot_ev <- sum(nyc_map$count)
  tot_mt <- sum(data_mt$`Mt Strada`)
  #Valori attesi 
  
  rapp_ev_mt <- tot_ev / tot_mt
                                      
  data_df$e_i <- rapp_ev_mt *data_df$`Mt Strada`

#plot(data_df$count, data_df$count-data_df$e_i)  
  #############################################################
  
  ####################################MAPPA
  mapthis <- full_join(nyc_map, data_df, by = "CDTA2020") %>% 
        subset(, select = e_i)
    
  colnames(mapthis)[1] <- "Valore Atteso"
  fig <- tm_shape(mapthis) +
    tm_fill(col = "Valore Atteso", palette = "Blues", style = "quantile") +
    tm_borders() +
    tm_layout(legend.outside = TRUE,
              legend.outside.position = "right",
              #legend.outside.size = 0.2,
              legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.bg.color = "white",
              legend.frame = TRUE)
 
  ###############################################à


#Zone sono le stesse ergo matrici sono le stesse
if((exists("nbs"))){
    rm(A_NYC)
    rm(D_NYC)
  }

if(!exists("nbs")){
  source(file = "MatrNYC.R")
  source(file = "GraphReformat.R")
  nbs = GraphReformat(A_NYC, D_NYC);
}



 #E non può essere negativo, riporto i valori negativi a 1 --> con val 0 Log prob. valuta a log(0) tende a meno infinito
# data_df$e_mt_ar[data_df$e_mt_ar <= 0] <- 1
  N = nbs$N;
  adj.matrix = sparseMatrix(i=nbs$node1,j=nbs$node2,x=1,symmetric=TRUE)
  Q=  Diagonal(nbs$N, rowSums(adj.matrix)) - adj.matrix
  #Add a small jitter to the diagonal for numerical stability (optional but recommended)
  Q_pert = Q + Diagonal(nbs$N) * max(diag(Q)) * sqrt(.Machine$double.eps)
  
    Q_inv = INLA::inla.qinv(Q_pert, constr=list(A = matrix(1,1,nbs$N),e=0))
  
  scaling_factor = exp(mean(log(diag(Q_inv))))
  
sum(data_df$count == 0)
sum(data_df$count)
############Da file "Fitting.R"
  options(digits=3)
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  y = data_df$count;
  E = data_df$e_i;
    nbs = GraphReformat(A_NYC, D_NYC);
  node1 = nbs$node1;
  node2 = nbs$node2;
  N_edges = nbs$n_edges;
  #x <- matrix(Traffic_df$rapp, nrow = 71, ncol = 1)
                                                    #Altrimenti tira errore: dim = 71 invece che 71x1
  x <- matrix(0, nrow = 71, ncol = 1)
       
dataSB = list("N"= N,
            "N_edges"=N_edges,
            "node1"=node1,
            "node2"=node2,
            "y"=y,
            "x"=x,
            "E"=E,
            "K" = 1,
            "scaling_factor" = scaling_factor);
  
set.seed(42)
BYM_morti_fin_259 = stan("BYM2.stan", data = dataSB, control=list(max_treedepth=10))
  check_hmc_diagnostics(BYM_morti_fin_259)

  
  mu <- rstan::extract(BYM_morti_fin_259 , pars = "mu")[[1]]
y_rep <- rstan::extract(BYM_morti_fin_259 , pars = "y_rep")[[1]]

df_sum <- data.frame(summary(BYM_morti_fin_259))
#dev.off()
  
  color_scheme_set("brightblue")
  ppc_dens_overlay(y , y_rep[1:50,])  
    #geom_point(aes(x = 1:length(y), y = E), color = "red", size = 3)
  
  ppc_intervals(y = y, yrep = y_rep)
  
  
  
  plot(y/E, colMeans(mu)/E,pch = 18 , ylim = c(0,8), xlim = c(0,8), col = "blue")  
  abline(0,1, col = "red")
  plot(y/E, colMeans(y_rep)/E, ylim = c(0,8), xlim = c(0,8))  
  abline(0,1)
  
  
  plot(y, colMeans(y_rep),
       xlab = "Valori osservati (y)", 
       ylab = "Valori previsti ", 
       main = "Dati osservati vs. Previsti",
       pch = 18, 
       col = "lightblue") 
   grid()
  
  abline(a = 0, b = 1, col = "red", lty = 2)
  which(y == 0)
  
  ppc_stat(y, y_rep)
#19 33 35 36 37 54 63 64 67 
  
#71 ---> Miller Field 
  
  #36 ---> Financial District
  #37 --> Confina con 36 
#19 -->prospect park 
#33 --> van cortlandt park
#63 --> areoporto LaGuardia 
#35 -->Pelham Bay Park   
#67 -->Peyton T. Dilbert Park + Jacob Riis Park  
#64 --> Flushing Meadows Corona Park
  #54 --> zona con anche grosse strade

  modello <- rstan::extract(BYM_morti_fin_259)
 
  
  riass1 <- data.frame(geometry = st_geometry(nyc_map$geometry), Theta = colMeans(modello$theta) * sqrt(1 - mean(modello$rho) ))
  riass1 <- st_sf(riass1)
  
  ggplot(data = riass1) +
    geom_sf(aes(fill = Theta)) +
    scale_fill_viridis_c() + # Optional: for a nice color scale
    theme_minimal() +
    labs(fill = "Effetti Casuali")
  
  
  
  riass_b <- data.frame(geometry = st_geometry(nyc_map$geometry), Phi = colMeans(modello$phi) * sqrt(mean(modello$rho)/scaling_factor) )
  riass_b <- st_sf(riass_b)
  
  
  ggplot(data = riass_b) +
    geom_sf(aes(fill = Phi)) +
    scale_fill_viridis_c() + # Optional: for a nice color scale
    theme_minimal() +
    labs(fill = "Effetti Spaziali")
  
  
  
  ##
  