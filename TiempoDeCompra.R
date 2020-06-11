      ### Tiempo de Compra ###
### Roger Cristobal y Marc lÃ³pez ###

#### Carga de librerias y paquetes ####
install.packages("shiny")
install.packages("dplyr")
if(!require("tidyverse")) {
  install.packages("tidyverse", repos="https://cloud.r-project.org/",
                   quiet=TRUE, type="binary")
  library("tidyverse")
}
install.packages("chron")
install.packages("plotly")
install.packages("forcats")

library(shiny)
library(dplyr)
library(chron)
library(plotly)
library(forcats)

#### Lectura de datos ####
TrainData <- read.table("train_trips.csv", sep=",", dec=".", quote = "\"'",
                        header=TRUE, skip = 0, na.strings = "NA")
OrderData <- read.table("order_items.csv", sep=",", dec=".", quote = "\"'",
                        header=TRUE, skip = 0, na.strings = "NA")
TestData <- read.table("datasets_70809_149763_test_trips.csv", sep=",", dec=".", quote = "\"'",
                       header=TRUE, skip = 0, na.strings = "NA")

### Transformamos los datos de formato ###
TrainData$shopping_started_at <- as.POSIXlt(TrainData$shopping_started_at)
TrainData$shopping_ended_at <- as.POSIXlt(TrainData$shopping_ended_at)
TrainData$DayOfWeek <- weekdays.POSIXt(TrainData$shopping_started_at)
# TrainData$Difference <- TrainData$shopping_ended_at - TrainData$shopping_started_at
TrainData$HourofDay <- unclass(TrainData$shopping_started_at)$hour
# 

### Se agrupan los datos de la tabla Order_items por trip_id y se suman las cantidades totales ###
resumOrder <- OrderData %>%
  group_by(trip_id) %>%
  summarise(TotalQuantity = sum(quantity)
            # ,num_dept_visited = sum(count(OrderData, wt = OrderData$department_name))
  )

### Se agrupan los datos de la tabla train_trips pro trip_id y se hace la diferencia para saber el tiempo que se está comprando ###
resumTrain <- TrainData %>%
  group_by(trip_id, store_id) %>%
  summarise(shopping_time = difftime(shopping_ended_at,shopping_started_at, units = "mins"),
            DayWeek = DayOfWeek)

### Se juntan las dos tablas obtenidas previamente ###
allTrainData <- merge(resumOrder,resumTrain, by="trip_id")

### Barplot de tiempo medio de compra por tienda ###

### Se agrupan los datos de la tabla allTrainData por store_id y se hace la media de tiempo en cada tienda ###
data1 <- allTrainData %>%
  group_by(store_id) %>%
  summarise(meanTime = mean(shopping_time)
  )
p <- barplot(height = as.numeric(data1$meanTime), names = data1$store_id,
             xlab = "Tienda",
             ylab = "Tiempo medio de compra (min)",
             main = "Tiempo medio empleado para comprar en una tienda"
             )
text(p, as.numeric(data1$meanTime)+1.2, as.character(round(data1$meanTime, digits = 2)), cex = 1)




### Barplot de tiempo total de compra por día ###

### Se agrupan los datos de la tabla allTrainData por DayWeek y se suma el tiempo total de compra para cada día ###
data2 <- allTrainData %>%
  group_by(DayWeek) %>%
  summarise(shop_time = sum(shopping_time)
  )

data2$DayWeek <- factor(data2$DayWeek,levels = c("lunes","martes","miércoles","jueves","viernes","sábado","domingo"))
data2 <- data2[order(data2$DayWeek),]


p <- barplot(height = as.numeric(data2$shop_time), names = data2$DayWeek,
             xlab = "Día de la semana",
             ylab = "Tiempo total de compra",
             main = "Tiempo total empleado según el dia de la semana"
             ) 
text(p, as.numeric(data2$shop_time)+0.5e5, as.character(data2$shop_time), cex = 1)

### Se mira el tiempo de compra por cada producto realizando una division entre el tiempo de compra y el total de productos comprados ###

allTrainData$TimePerProduct <- as.numeric(allTrainData$shopping_time/allTrainData$TotalQuantity)

### Prueba de Shapiro-Wilk para comprobar la normalidad de la variable TimePerProduct ###

shapiro.test(allTrainData$TimePerProduct[1:4000])

### Grafico del tiempo de compra por producto ###

p0 <- ggplot(allTrainData,
             aes(x = TotalQuantity ,y = TimePerProduct)) + geom_line()
ggplotly(p0)

### Grafico para comprobar la normalidad de la variable TimePerProduct ###

qqline()

ggplot(allTrainData, aes(sample = TimePerProduct)) + geom_qq() + geom_qq_line() + theme_classic()




### Volumen de compras por hora del día ###

### Se coge de la tabla Train_trips las variables que interesan  y se hace un group_by###
resumTrainHour <- TrainData %>%
  group_by(trip_id, store_id) %>%
  summarise(shopping_hour = HourofDay
  )

### Se junta la tabla allTrainData con la creada recientemente con las horas de cada compra ###

allTrainDataHour <- merge(allTrainData,resumTrainHour, by="trip_id")

### De la tabla anterior se agrupa por hora de compra y se suma la cantidad total para cada hora ###

resumHour <- allTrainDataHour %>%
  group_by(shopping_hour) %>%
  summarise(QuantityPerHour = sum(TotalQuantity)
  )

### Grafico de barras ###

h <- barplot(height = as.numeric(resumHour$QuantityPerHour), names = resumHour$shopping_hour,
             xlab = "Hora del día",
             ylab = "Compras totales",
             main = "Compras por hora del dia"
)

### Grafico lineal ###

resumHour %>%
  tail(25) %>%
  ggplot( aes(x=shopping_hour, y=QuantityPerHour)) +
  geom_line() +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  ggtitle("Evolución de las compras según la hora del día")+
  #xlim(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
  scale_x_continuous(breaks=c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))





### Departamentos con mayor y menor volumen de ventas ###

### De la tabla Order_items de necesita el nombre de los departamentos y se suma la cantidad total para cada uno ###
productType <- OrderData %>%
  
  group_by(department_name) %>%
  summarise(TotalQuantityProduct = sum(quantity)
  )

### Se hace un substr para eliminar la parte final del departmenmt_name que en algunos casos incluye el nunero del producto ###
productType$department_name <- substr(productType$department_name,1,11)

productTypeClean <- productType %>%
  group_by(department_name) %>%
  summarise(TotalQuantityProductClean = sum(TotalQuantityProduct)
  )

### Se ordena la tabla en orden decreciente ###

ProductTypeOrdenat <- productTypeClean[order(productTypeClean$TotalQuantityProductClean), decreasing=TRUE]

### Grafico de menor volumen de ventas ###

prodMenos <- barplot(height = as.numeric(ProductTypeOrdenat$TotalQuantityProductClean[1:25]), 
                     horiz=TRUE, 
                     names.arg=(ProductTypeOrdenat$department_name[1:25]), 
                     cex.names=0.6,
                     xlab = "Numero de artículos vendidos",
                     ylab = "",
                     main = "Departamentos con menor volumen de ventas",
                     #            par(las=productTypeClean$department_name),
                     
)

### Grafico de mayor volumen de ventas ###

prodMas <- barplot(height = as.numeric(ProductTypeOrdenat$TotalQuantityProductClean[71:81]), 
                   horiz=TRUE, 
                   names.arg=(ProductTypeOrdenat$department_name[71:81]), 
                   cex.names=0.8,
                   xlab = "Numero de artículos vendidos",
                   ylab = "",
                   main = "Departamentos con mayor volumen de ventas",
                   #            par(las=productTypeClean$department_name),
                   
)







