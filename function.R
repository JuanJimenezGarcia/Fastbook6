

library(highcharter)


#lectura_datos ----
#Realiza la lectura de datos
#
#Devuelve dataframe
lectura_datos <- function(){
  df_venta_diaria <- read.csv("data/venta_diaria.csv")
  return(df_venta_diaria)
}
