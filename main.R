
source("function.R")

#1 Leemos el dato ----
df_venta_diaria <- lectura_datos()

#2 Tratamiento del dato ----
df_venta_diaria <- tratamiento_datos(df_venta_diaria)


#3 Graficamos la serie de ventas de la tienda 24512 ----
graficar_variable_tienda(df_venta_diaria,"VENTA",24512)
