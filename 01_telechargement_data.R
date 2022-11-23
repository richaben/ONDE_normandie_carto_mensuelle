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
write_csv(onde_df, paste0('onde_data/','onde_',date_jour_heure,'.csv'))