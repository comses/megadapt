# Ejemplo de prediccion

## se carga el modelo

load(file = "c:/Users/abaezaca/Dropbox (Personal)/modelo_ench_inund/arbol de regresion/Modelos_region/encharcamientos/mod_en_reg1.rda")

## Se crean datos de prueba
datos_prueba <- structure(list(f_prec_v = 194941, f_esc = 0, n_tramos = 0, 
                               q100 = 6.87, bombeo_tot = 0, rejillas = 0), 
                          row.names = c(NA, -1L), class = c("data.frame"))
head(datos_prueba)

## Se hace la predicción
predict(modelo_en_region_1, # Modelo de region 1
        datos_prueba, # Una fila con observaciones de todas las variables 
        n.trees = 9566, # Número de árboles que usa el modelo 
        type = "response")
