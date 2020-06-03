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

#### Lectura de datos ####
TrainData <- read.table("train_trips.csv", sep=",", dec=".", quote = "\"'",
                        header=TRUE, skip = 0, na.strings = "NA")
OrderData <- read.table("order_items.csv", sep=",", dec=".", quote = "\"'",
                        header=TRUE, skip = 0, na.strings = "NA")
TestData <- read.table("test_trips.csv", sep=",", dec=".", quote = "\"'",
                       header=TRUE, skip = 0, na.strings = "NA")

TrainData$shopping_started_at <- as.POSIXct(TrainData$shopping_started_at)
TrainData$shopping_ended_at <- as.POSIXct(TrainData$shopping_ended_at)