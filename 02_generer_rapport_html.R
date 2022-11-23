##%######################################################%##
#                                                          #
####    Generer le fichier Rmarkdown au format html     ####
#                                                          #
##%######################################################%##

## version: 23-11-2022

print("Creation du fichier html !")

library(tidyverse)
library(rmarkdown)

rmarkdown::render("assets/template.Rmd", 
                  output_file = "../index.html",
                  quiet = T)
