##%######################################################%##
#                                                          #
#### Téléchargement et mises en forme des données ONDE  ####
#                                                          #
##%######################################################%##

## version: 23-11-2022

## --> Objectif : 
## Telechargement donnees ONDE à partir de l'API Hubeau + mise en forme


### Chargement R packages ----

library(hubeau)
library(tidyverse)
library(purrr)
library(sf)
library(ggiraph)
library(mapview)
library(leaflet)
library(leaflet.extras)

### Selection des départements ----

dpt_sel <- c('14','27','50','61','76')

### Date du jour ----
date_jour <- as.character(format(Sys.time(),"%Y-%m-%d"))
date_jour_heure <- as.character(format(Sys.time(),"%Y-%m-%d_%Hh%m"))

### Utilisation de l'API Hubeau ----

#### infos campagnes
campagnes <- map_df(.x = dpt_sel,
                    function(x) get_ecoulement_campagnes(
                      list(
                        code_departement = x,
                        date_campagne_min = "2012-01-01",
                        date_campagne_max = date_jour
                      )
                    )) %>% 
  mutate(code_campagne = as.character(code_campagne))

#### infos stations
param_stations <- 
  list_params(api = "ecoulement", endpoint = "stations") %>% 
  toString() %>% 
  gsub(pattern = " ",replacement = "")

stations <- map_df(.x = dpt_sel,
                   function(x) get_ecoulement_stations(
                     list(code_departement = x,
                          fields = param_stations)
                   ))

#### infos observations
param_obs <- 
  list_params(api = "ecoulement", endpoint = "observations") %>% 
  toString() %>% 
  gsub(pattern = " ",replacement = "")

observations <- map_df(.x = dpt_sel,
                       function(x) get_ecoulement_observations(
                         list(code_departement = x,
                              date_observation_min = "2012-01-01",
                              date_observation_max = date_jour,
                              fields = param_obs)
                       )) %>% 
  mutate(code_campagne = as.character(code_campagne))

### Assemblage des données stations, observations, campagnes ----
onde_df <-
  left_join(observations, campagnes) %>% 
  left_join(stations) %>% 
  mutate(date_campagne = as.Date(date_campagne),
         Annee = lubridate::year(date_campagne)) %>% 
  arrange(code_station,code_departement,desc(Annee))


### Ecriture/Sauvegarde des données ----
write_csv(onde_df, paste0('data/onde_data/','onde_',date_jour_heure,'.csv'))


###---------------------------------------------#####
###---------------------------------------------#####
###---------------------------------------------#####

# couleurs légendes
mes_couleurs_3mod <- c("Ecoulement\nvisible" = "#0570b0",
                       "Ecoulement\nnon visible" = "#feb24c",
                       "Assec" = "#e31a1c",
                       "Observation\nimpossible" = "grey50",
                       "NA" = "grey90")

# légende
mes_couleurs_4mod <- c(#"Ecoulement\nvisible" = "#0570b0",
  "Ecoulement\nvisible\nacceptable" = "#0570b0",
  "Ecoulement\nvisible\nfaible" = "#bdd7e7",
  "Ecoulement\nnon visible" = "#feb24c",
  "Assec" = "#e31a1c",
  "Observation\nimpossible" = "grey50",
  "NA" = "grey90")


# toutes stations toutes annees
onde_periode <-
  onde_df %>% select(-c(libelle_reseau, 
                        code_type_campagne, 
                        uri_reseau, 
                        uri_station, 
                        uri_cours_eau,
                        code_projection_station,
                        date_maj_station)) %>% 
  filter(etat_station == 'Active') %>% 
  mutate(Mois = format(as.Date(date_campagne),"%m"), 
         Mois_campagne = lubridate::ym(paste0(Annee,Mois,sep="-"))) %>% 
  mutate(lib_ecoul3mod = case_when(libelle_ecoulement == 'Ecoulement visible faible' ~ 'Ecoulement visible',
                                   libelle_ecoulement == 'Ecoulement visible acceptable' ~ 'Ecoulement visible',
                                   #is.na(libelle_ecoulement) ~ 'Observation absente',
                                   TRUE ~ libelle_ecoulement),
         lib_ecoul3mod = if_else(is.na(lib_ecoul3mod),'NA',lib_ecoul3mod),
         lib_ecoul4mod = case_when(libelle_ecoulement == 'Ecoulement visible' ~ 'Ecoulement visible acceptable',
                                   #is.na(libelle_ecoulement) ~ 'Observation absente',
                                   TRUE ~ libelle_ecoulement),
         lib_ecoul4mod = if_else(is.na(lib_ecoul4mod),'NA',lib_ecoul4mod)) %>% 
  select(code_station ,
         libelle_station,
         date_campagne,
         Annee,
         Mois,
         lib_ecoul3mod,
         lib_ecoul4mod,
         libelle_type_campagne,
         coordonnee_x_station, 
         coordonnee_y_station,
         code_departement)


## selection sous tableau des dernieres campagnes
onde_dernieres_campagnes_usuelles <-
  onde_periode %>% 
  filter(libelle_type_campagne == 'usuelle') %>% 
  mutate(Mois = format(as.Date(date_campagne),"%m"), 
         Mois_campagne = lubridate::ym(paste0(Annee,Mois,sep="-"))) %>% 
  group_by(libelle_station) %>% 
  slice(which.max(Mois_campagne)) %>% 
  arrange(libelle_type_campagne, libelle_station, Mois_campagne) %>% 
  ungroup() %>% 
  mutate(Couleur = recode(str_wrap(lib_ecoul3mod,12), !!!mes_couleurs_3mod))

onde_dernieres_campagnes_comp <-
  onde_periode %>% 
  filter(libelle_type_campagne != 'usuelle') %>% 
  mutate(Mois = format(as.Date(date_campagne),"%m"), 
         Mois_campagne = lubridate::ym(paste0(Annee,Mois,sep="-"))) %>% 
  group_by(libelle_station) %>% 
  slice(which.max(Mois_campagne)) %>% 
  arrange(libelle_type_campagne, libelle_station, Mois_campagne) %>% 
  ungroup() %>% 
  filter(Mois_campagne == max(Mois_campagne))

## coordonnes stations EPSG 2154 RGF93
stations_onde_geo <- 
  onde_dernieres_campagnes_usuelles %>% 
  dplyr::ungroup() %>% 
  dplyr::select(code_station ,
                libelle_station,
                coordonnee_x_station, 
                coordonnee_y_station,
                code_departement) %>% 
  sf::st_as_sf(coords = c("coordonnee_x_station",
                          "coordonnee_y_station"), 
               crs = 2154)

## calculs assecs periode ete sur campagnes usuelles
assecs <- 
  onde_periode %>% 
  filter(libelle_type_campagne == 'usuelle') %>% 
  filter(Mois %in% c('05', '06', '07', '08', '09')) %>% 
  group_by(code_station, libelle_station) %>%
  summarise(n_donnees = n(),
            n_assecs = length(lib_ecoul3mod[lib_ecoul3mod=='Assec'])) %>%
  ungroup() %>% 
  mutate(pourcentage_assecs = round(n_assecs / n_donnees * 100, digits = 2),
         taille_point = sqrt(pourcentage_assecs))

## jointure + reprojection WGS84
stations_onde_geo_usuelles <- 
  stations_onde_geo %>%
  dplyr::left_join(assecs) %>%
  dplyr::mutate(pourcentage_assecs = replace_na(pourcentage_assecs, replace = 0)) %>% 
  sf::st_transform(crs = 4326) %>% 
  dplyr::mutate(label = paste0(libelle_station,' (',code_station,')'))


stations_onde_geo_comp <- 
  stations_onde_geo %>%
  dplyr::left_join(onde_dernieres_campagnes_comp) %>%
  sf::st_transform(crs = 4326) %>% 
  dplyr::mutate(label = paste0(libelle_station,' (',code_station,')'))


# génération des graphs en serie

produire_graph_pour_une_station <- 
  function(code_station2, onde_df, couleurs){
    
    prov <- onde_df %>%
      #filter(libelle_type_campagne == 'usuelle') %>%
      filter(code_station == code_station2) %>% 
      mutate(label_p = paste0(libelle_type_campagne,'\n',lib_ecoul3mod,'\n',date_campagne),
             label_sta = paste0(libelle_station,' (',code_station,')'),
             label_png = paste0("ONDE_dpt",code_departement,"_",label_sta))
    
    nom_station <- unique(prov$label_sta)
    nom_station_graph <- unique(prov$label_png)
    
    graph1 <-
      ggplot(data = prov) +
      aes(x = Annee,
          y = Mois %>% as.numeric) +
      geom_point(
        aes(
          fill = stringr::str_wrap(lib_ecoul3mod, 12),
          shape = libelle_type_campagne,
          size = libelle_type_campagne,
        ),col='black') +
      coord_flip() +
      scale_fill_manual(values = couleurs, name = 'Modalités') +
      scale_shape_manual(values = c(21,22),name = 'Type campagne') +
      scale_size_manual(values = c(5,10),name = 'Type campagne') +
      scale_y_continuous(breaks = 1:12, labels = 1:12) +
      scale_x_continuous(breaks = min(prov$Annee, na.rm = T):max(prov$Annee, na.rm = T),
                         labels = min(prov$Annee, na.rm = T):max(prov$Annee, na.rm = T)) +
      labs(x = "", y = "Mois", title = nom_station) +
      theme_bw() +
      guides(fill = guide_legend(override.aes=list(shape = 22, size = 5)))
    
    graph1
  }

# produire_graph_pour_une_station_int <- 
#   function(code_station2, onde_df, couleurs){
#     
#     prov <- onde_df %>%
#       #filter(libelle_type_campagne == 'usuelle') %>%
#       filter(code_station == code_station2) %>% 
#       mutate(label_p = paste0(libelle_type_campagne,'\n',lib_ecoul3mod,'\n',date_campagne),
#              label_sta = paste0(libelle_station,' (',code_station,')'),
#              label_png = paste0("ONDE_dpt",code_departement,"_",label_sta))
#     
#     nom_station <- unique(prov$label_sta)
#     nom_station_graph <- unique(prov$label_png)
#     
#     graph1 <-
#       ggplot(data = prov) +
#       aes(x = Annee,
#           y = Mois %>% as.numeric) +
#       ggiraph::geom_point_interactive(
#         aes(tooltip = label_p,
#             fill = stringr::str_wrap(lib_ecoul3mod, 12),
#             shape = libelle_type_campagne,
#             size = libelle_type_campagne,
#         ),col='black') +
#       coord_flip() +
#       scale_fill_manual(values = couleurs, name = 'Modalités') +
#       scale_shape_manual(values = c(21,22),name = 'Type campagne') +
#       scale_size_manual(values = c(5,10),name = 'Type campagne') +
#       scale_y_continuous(breaks = 1:12, labels = 1:12) +
#       scale_x_continuous(breaks = min(prov$Annee, na.rm = T):max(prov$Annee, na.rm = T),
#                          labels = min(prov$Annee, na.rm = T):max(prov$Annee, na.rm = T)) +
#       labs(x = "", y = "Mois", title = nom_station) +
#       theme_bw() +
#       guides(fill = guide_legend(override.aes=list(shape = 22, size = 5)))
#     
#     
#     graph1 <- ggiraph::girafe(ggobj = graph1)
#     graph1 <- ggiraph::girafe_options(graph1, ggiraph::opts_toolbar(pngname = nom_station_graph))
#     graph1
#   }

# produire_graph_pour_une_station_int(onde_df = onde_periode,
#                                     code_station2 = stations_onde_geo_usuelles$code_station[3],
#                                     couleurs = mes_couleurs_3mod)


graphiques_int_3mod <- 
  purrr::map(.x = stations_onde_geo_usuelles$code_station, 
    .f = produire_graph_pour_une_station, 
    onde_df = onde_periode, 
    couleurs = mes_couleurs_3mod)

names(graphiques_int_3mod) <- stations_onde_geo_usuelles$code_station

# Sauvegarde des objets pour page Rmd
save(stations_onde_geo_usuelles, graphiques_int_3mod, onde_dernieres_campagnes_usuelles,file = "data/processed_data/map_data3mod.RData")
