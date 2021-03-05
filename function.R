

library(highcharter)


#lectura_datos ----
#Realiza la lectura de datos
#
#Devuelve dataframe
lectura_datos <- function(){
  df_venta_diaria <- read.csv("data/venta_diaria.csv")
  return(df_venta_diaria)
}


#tratamiento_datos ----
#Realiza el tratamiento de los datos. Orden, imputar NA y crear variable venta
#
#Recibe dataframe
#
#Devuelve dataframe con el tratamiento realizado
tratamiento_datos <- function(df_venta_diaria){
  df_venta_diaria <- ordenar_df(df_venta_diaria,"DIA")
  df_venta_diaria <- imputar_missings_df(df_venta_diaria)
  df_venta_diaria["VENTA"] <- df_venta_diaria["UNIDADES"] * df_venta_diaria["PRECIO"]
  return(df_venta_diaria)
}


#ordenar_df ----
#Ordena un dataframe en base a la columna que se le indique
#
#Recibe dataframe y nombre de columna por la que ordenar
#
#Devuelve dataframe ordenado
ordenar_df <- function(df_venta_diaria,columna){
  df_venta_diaria <- df_venta_diaria[order(df_venta_diaria[columna]),]
  row.names(df_venta_diaria) <- NULL
  return(df_venta_diaria)
}


#imputar_missings_df ----
#Imputa missings en todas las columnas de un dataframe
#
#Recibe dataframe
#
#Devuelve dataframe con la imputacion realizada
imputar_missings_df <- function(df_venta_diaria){
  message("NAs antes de la imputacion")
  print(lapply(lapply(df_venta_diaria,is.na),sum))
  df_venta_diaria <- lapply(X = df_venta_diaria,FUN = imputar_missings_vector)
  df_venta_diaria <- as.data.frame(df_venta_diaria)
  message("NAs tras la imputacion")
  print(lapply(lapply(df_venta_diaria,is.na),sum))
  return(df_venta_diaria)
}


#imputar_missings_vector ----
#Imputamos missings en el vector recibido. Si es numerico inserta la media,
#si no la moda.
#
#Recibe vector
#
#Devuelve vector con la imputacion realizada
imputar_missings_vector <- function(vector){
  if(is.numeric(vector)){
    vector[is.na(vector)] <- mean(vector,na.rm=TRUE)
  } else {
    uniqv <- unique(vector[!is.na(vector)])
    moda <- uniqv[which.max(tabulate(match(vector[!is.na(vector)], uniqv)))]
    vector[is.na(vector)] <- moda
  }
  return(vector)
}

#graficar_variable_tienda ----
#Pinta el grafico de la variable seleccionada para la tienda seleccionada
#
#Recibe dataframe, variable que se quiere pintar y tienda a seleccionar
#
#Pinta el grafico 
graficar_variable_tienda <- function(df_venta_diaria,variable,tienda,
                                     color = "#345294"){
  
  
  hchart(df_venta_diaria[(df_venta_diaria$TIENDA==tienda),], 
         type = "line", 
         hcaes(x = DIA, 
               y = !!variable)) %>% 
    hc_title(text = paste0(variable," POR DIA TIENDA ",tienda)) %>%
    hc_colors(color)
}

