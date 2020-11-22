# # # # # # # # # # # # # # # # # # # #

# Proyecto: Taller Magallanes Entrena
# Autor:    Matías Castillo Aguilar

# # # # # # # # # # # # # # # # # # # #

# Importamos los datos ----------------------------------------------------

library(googlesheets4)          # Cargamos paquete para descarga de
                                # los datos desde Google Sheets.

httr::reset_config()            # Eliminamos el caché del DNS.
gs4_auth()                      # Autenticamos los permisos para la
                                # descarga de los datos.

data_raw <- read_sheet(             # Descargamos los datos desde Google
  ss = "194XV6AK_1aOlDd2dDcxKWbTlzvrbJvn0Sy0nE_wyaUk",      # Sheets.
  sheet = "Datos para exportación")


library(dplyr); library(tidyr)  # Cargamos paquetes para manipulación
                                # de datos.

data <-                         # eliminamos valores perdidos
  data_raw %>%                  # dentro de los datos, usando
  drop_na(consentimiento) %>%   # el consentimiento como primer filtro...
  distinct(telefono,            # y el teléfono para eliminar los datos
    .keep_all = T) %>%          # duplicados.
  distinct(nombre,
    .keep_all = T) %>% 
  filter(consentimiento ==      # Nos quedamos sólo con aquellos que aceptaron
           "Sí, estoy de acuerdo")               # el consentimiento informado

library(data.table)             # Cargamos paquetes para computación
data <- as.data.table(data)     # optimizada de datos.

# Transformación de variables ---------------------------------------------

str(data)                       # Evaluamos la estructura de los datos.

data <- within(data, {
  
  # Corregimos valores mal tabulados
  comuna[comuna %in% c("PUNTA ARENAS","Punta arenas","Puntarenas",
                       "Pta arenas","punta arenas")] <- "Punta Arenas"
  comuna[comuna %in% c("Puerto Natales","Natales")] <- "Puerto Natales"
  comuna[comuna %in% c("providencia")] <- "Providencia"
  comuna[comuna %in% c("santiago","Santiago centro")] <- "Santiago"
  comuna[comuna %in% c("castro")] <- "Castro"
  comuna[comuna %in% c("ñuñoa","Nunoa")] <- "Ñuñoa"
  comuna[comuna %in% c("Isla dawson")] <- "Isla Dawson"
  comuna[comuna %in% c("Villa alemana")] <- "Villa Alemana"
  comuna[comuna %in% c("valparaíso")] <- "Valparaíso"

  # Le damos el formato correcto a cada variable
  actividad_fisica <- factor(actividad_fisica, 
                              levels = c("Bajo","Moderado","Alto"))
  comuna <- factor(comuna)
  consentimiento <- factor(consentimiento)
  correo <- as.character(correo)
  dispositivo <- factor(dispositivo)
  edad <- as.integer(edad)
  met_caminando <- as.numeric(met_caminando)
  met_moderado <- as.numeric(met_moderado)
  met_total <- as.numeric(met_total)
  met_vigoroso <- as.numeric(met_vigoroso)
  nombre <- as.character(nombre)
  publicidad <- factor(publicidad)
  region <- factor(region)
  sexo <- factor(sexo)
  sf_1  <- factor(sf_1, levels = c("Mala","Regular","Buena","Muy buena","Excelente"))
  sf_2  <- factor(sf_2, levels = c("Peor","Algo peor","Igual","Algo mejor","Mucho mejor"))
  sf_3  <- factor(sf_3, levels = c("Sí, muy limitada","Sí, un poco limitada","No, no limitada"))
  sf_4  <- factor(sf_4, levels = c("Sí, muy limitada","Sí, un poco limitada","No, no limitada"))
  sf_5  <- factor(sf_5, levels = c("Sí, muy limitada","Sí, un poco limitada","No, no limitada"))
  sf_6  <- factor(sf_6, levels = c("Sí, muy limitada","Sí, un poco limitada","No, no limitada"))
  sf_7  <- factor(sf_7, levels = c("Sí, muy limitada","Sí, un poco limitada","No, no limitada"))
  sf_8  <- factor(sf_8, levels = c("Sí, muy limitada","Sí, un poco limitada","No, no limitada"))
  sf_9  <- factor(sf_9, levels = c("Sí, muy limitada","Sí, un poco limitada","No, no limitada"))
  sf_10 <- factor(sf_10, levels = c("Sí, muy limitada","Sí, un poco limitada","No, no limitada"))
  sf_11 <- factor(sf_11, levels = c("Sí, muy limitada","Sí, un poco limitada","No, no limitada"))
  sf_12 <- factor(sf_12, levels = c("Sí, muy limitada","Sí, un poco limitada","No, no limitada"))
  sf_13 <- factor(sf_13, levels = c("Nunca","Pocas veces","Algunas veces",
                                    "La mayor parte del tiempo","Siempre"))
  sf_14 <- factor(sf_14, levels = c("Nunca","Pocas veces","Algunas veces",
                                    "La mayor parte del tiempo","Siempre"))
  sf_15 <- factor(sf_15, levels = c("Nunca","Pocas veces","Algunas veces",
                                    "La mayor parte del tiempo","Siempre"))
  sf_16 <- factor(sf_16, levels = c("Nunca","Pocas veces","Algunas veces",
                                    "La mayor parte del tiempo","Siempre"))
  sf_17 <- factor(sf_17, levels = c("Nunca","Pocas veces","Algunas veces",
                                    "La mayor parte del tiempo","Siempre"))
  sf_18 <- factor(sf_18, levels = c("Nunca","Pocas veces","Algunas veces",
                                    "La mayor parte del tiempo","Siempre"))
  sf_19 <- factor(sf_19, levels = c("Nunca","Pocas veces","Algunas veces",
                                    "La mayor parte del tiempo","Siempre"))
  sf_20 <- factor(sf_20, levels = c("De ninguna manera","Un poco","Moderadamente",
                                    "Bastante","Mucho"))
  sf_21 <- factor(sf_21, levels = c("Ninguno","Muy poco","Leve","Moderado",
                                    "Severo","Muy severo"))
  sf_22 <- factor(sf_22, levels = c("De ninguna manera","Un poco","Moderadamente",
                                    "Bastante","Mucho"))
  sf_23 <- factor(sf_23, levels = c("Nunca","Muy poco tiempo","Un poco",
                                    "Casi todo el tiempo","Siempre"))
  sf_24 <- factor(sf_24, levels = c("Nunca","Muy poco tiempo","Un poco",
                                    "Casi todo el tiempo","Siempre"))
  sf_25 <- factor(sf_25, levels = c("Nunca","Muy poco tiempo","Un poco",
                                    "Casi todo el tiempo","Siempre"))
  sf_26 <- factor(sf_26, levels = c("Nunca","Muy poco tiempo","Un poco",
                                    "Casi todo el tiempo","Siempre"))
  sf_27 <- factor(sf_27, levels = c("Nunca","Muy poco tiempo","Un poco",
                                    "Casi todo el tiempo","Siempre"))
  sf_28 <- factor(sf_28, levels = c("Nunca","Muy poco tiempo","Un poco",
                                    "Casi todo el tiempo","Siempre"))
  sf_29 <- factor(sf_29, levels = c("Nunca","Muy poco tiempo","Un poco",
                                    "Casi todo el tiempo","Siempre"))
  sf_30 <- factor(sf_30, levels = c("Nunca","Muy poco tiempo","Un poco",
                                    "Casi todo el tiempo","Siempre"))
  sf_31 <- factor(sf_31, levels = c("Nunca","Muy poco tiempo","Un poco",
                                    "Casi todo el tiempo","Siempre"))
  sf_32 <- factor(sf_32, levels = c("Nunca","Pocas veces","Algunas veces",
                                    "La mayor parte del tiempo","Siempre"))
  sf_33 <- factor(sf_33, levels = c("Definitivamente falso","Casi siempre falso","No sé",
                                    "Casi siempre cierto","Definitivamente cierto"))
  sf_34 <- factor(sf_34, levels = c("Definitivamente falso","Casi siempre falso","No sé",
                                    "Casi siempre cierto","Definitivamente cierto"))
  sf_35 <- factor(sf_35, levels = c("Definitivamente falso","Casi siempre falso","No sé",
                                    "Casi siempre cierto","Definitivamente cierto"))
  sf_36 <- factor(sf_36, levels = c("Definitivamente falso","Casi siempre falso","No sé",
                                    "Casi siempre cierto","Definitivamente cierto"))
  spaq_1 <- as.integer(spaq_1)
  spaq_2 <- as.numeric(spaq_2)
  spaq_3 <- as.integer(spaq_3)
  spaq_4 <- as.numeric(spaq_4)
  spaq_5 <- as.integer(spaq_5)
  spaq_6 <- as.numeric(spaq_6)
  spaq_7 <- as.numeric(spaq_7)
  telefono <- factor(telefono)
  data
})

str(data)    # Evaluamos nuevamente la estructura de 
             # los datos.

library(mice)
impute <- mice(
  data = data, 
  m = 1, 
  maxit = 1, 
  seed = 199318853)

# Vemos cuantas personas tenemos de cada región.
with(data,{ tab <- table(region)
            par(mar = c(5,23,4,3))
            plot <- barplot(tab, horiz = TRUE, las = 1)
            par(mar = c(5,4,4,3))
            return(plot)  })

# Y por comuna.
with(data,{ tab <- table(comuna)
            par(mar = c(5,8,4,3))
            plot <- barplot(tab, horiz = TRUE, las = 1)
            par(mar = c(5,4,4,3))
            return(plot)  })

# Tomamos a las personas de magallanes
magallanes <- data[
  region == levels(region)[7], 
  -c("region","comuna","nombre","correo","publicidad","consentimiento")
  ]