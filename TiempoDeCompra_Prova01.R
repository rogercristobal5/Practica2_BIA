### Tiempo de Compra ###
#### Carga de librerias y paquetes ####
install.packages("shiny")
install.packages("dplyr")
if(!require("tidyverse")) {
  install.packages("tidyverse", repos="https://cloud.r-project.org/",
                   quiet=TRUE, type="binary")
  library("tidyverse")
}
install.packages("chron")

library(shiny)
library(dplyr)
library(chron)
