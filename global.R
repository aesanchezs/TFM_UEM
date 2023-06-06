


libs <- c("readxl", "tidyverse", "tidyr", "shinyWidgets", "ggplot2", "shinydashboardPlus", "stringr", "formattable", "ggimage",
          "FNN", "ggrepel", "shiny", "shinydashboard", "shinythemes", "shinycssloaders", "dplyr", "plotly")
lapply(libs, library, character.only = TRUE)




data <- read_excel('GrandesLigas_2018-2022.xlsx')
data <- data %>% mutate(ID=paste0(Jugador," ",Temporada))
data <- separate(data, "Posición específica", into = c("Posicion", "Posicion2", "Posicion3"), sep = ",", fill = "right")



#Portero
data$Posicion<- gsub("GK", "Portero", data$Posicion)
data$Posicion2<- gsub("GK", "Portero", data$Posicion2)
data$Posicion3<- gsub("GK", "Portero", data$Posicion3)

#Defensas

data$Posicion<- gsub("RB", "Lateral derecho", data$Posicion)
data$Posicion2<- gsub("RB", "Lateral derecho", data$Posicion2)
data$Posicion3<- gsub("RB", "Lateral derecho", data$Posicion3)

data$Posicion<- gsub("RCB|LCB|CB", "Central", data$Posicion)
data$Posicion2<- gsub("RCB|LCB|CB", "Central", data$Posicion2)
data$Posicion3<- gsub("RCB|LCB|CB", "Central", data$Posicion3)

data$Posicion<- gsub("LB", "Lateral izquierdo", data$Posicion)
data$Posicion2<- gsub("LB", "Lateral izquierdo", data$Posicion2)
data$Posicion3<- gsub("LB", "Lateral izquierdo", data$Posicion3)


data$Posicion<- gsub("RWB", "Carrilero derecho", data$Posicion)
data$Posicion<- gsub("LWB", "Carrilero izquierdo", data$Posicion)
data$Posicion2<- gsub("RWB", "Carrilero derecho", data$Posicion2)
data$Posicion2<- gsub("LWB", "Carrilero izquierdo", data$Posicion2)
data$Posicion3<- gsub("RWB", "Carrilero derecho", data$Posicion3)
data$Posicion3<- gsub("LWB", "Carrilero izquierdo", data$Posicion3)

data$Posicion<- gsub("CF", "Delantero", data$Posicion)
data$Posicion2<- gsub("CF", "Delantero", data$Posicion2)
data$Posicion3<- gsub("CF", "Delantero", data$Posicion3)

data$Posicion<- gsub("RWF", "Extremo derecho", data$Posicion)
data$Posicion<- gsub("LWF", "Extremo izquierdo", data$Posicion)
data$Posicion<- gsub("RW", "Extremo derecho", data$Posicion)
data$Posicion<- gsub("LW", "Extremo izquierdo", data$Posicion)

data$Posicion2<- gsub("RWF", "Extremo derecho", data$Posicion2)
data$Posicion2<- gsub("LWF", "Extremo izquierdo", data$Posicion2)
data$Posicion2<- gsub("RW", "Extremo derecho", data$Posicion2)
data$Posicion2<- gsub("LW", "Extremo izquierdo", data$Posicion2)

data$Posicion3<- gsub("RWF", "Extremo derecho", data$Posicion3)
data$Posicion3<- gsub("LWF", "Extremo izquierdo", data$Posicion3)
data$Posicion3<- gsub("RW", "Extremo derecho", data$Posicion3)
data$Posicion3<- gsub("LW", "Extremo izquierdo", data$Posicion3)

data$Posicion<- gsub("RDMF|LDMF|RCMF|LCMF", "Interior", data$Posicion)
data$Posicion2<- gsub("RDMF|LDMF|RCMF|LCMF", "Interior", data$Posicion2)
data$Posicion3<- gsub("RDMF|LDMF|RCMF|LCMF", "Interior", data$Posicion3)

data$Posicion<- gsub("DMF", "Mediocentro", data$Posicion)
data$Posicion2<- gsub("DMF", "Mediocentro", data$Posicion2)
data$Posicion3<- gsub("DMF", "Mediocentro", data$Posicion3)

data$Posicion<- gsub("RAMF|LAMF|AMF", "Mediapunta", data$Posicion)
data$Posicion2<- gsub("RAMF|LAMF|AMF", "Mediapunta", data$Posicion2)
data$Posicion3<- gsub("RAMF|LAMF|AMF", "Mediapunta", data$Posicion3)



#Nuevas columnas

data$`Duelos aéreos ganados en los 90` <- data$`Duelos aéreos ganados, %` * data$`Duelos aéreos/90` / 100
data$`Pases largos correctos en los 90` <- data$`Pases largos/90` * (data$`Precisión pases largos, %` / 100)
data$`Pases en profundidad correctos/90` <- data$`Pases en profundidad/90` * data$`Precisión pases en profundidad, %` / 100
data$`Pases en el último tercio correctos/90` <- data$`Pases en el último tercio/90` * (data$`Precisión pases en el último tercio, %` / 100)

umbral <- 0.001

data$percentil_ADR <- ifelse(data$`Acciones defensivas realizadas/90` < umbral, 0,
                             ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Acciones defensivas realizadas/90`)(data$`Acciones defensivas realizadas/90`),
                                    ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Acciones defensivas realizadas/90`)(data$`Acciones defensivas realizadas/90`),
                                           ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Acciones defensivas realizadas/90`)(data$`Acciones defensivas realizadas/90`),
                                                  ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Acciones defensivas realizadas/90`)(data$`Acciones defensivas realizadas/90`), NA)))))

data$percentil_entradas <- ifelse(data$`Entradas/90` < umbral, 0,
                                  ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Entradas/90`)(data$`Entradas/90`),
                                         ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Entradas/90`)(data$`Entradas/90`),
                                                ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Entradas/90`)(data$`Entradas/90`),
                                                       ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Entradas/90`)(data$`Entradas/90`), NA)))))


data$percentil_TirosIntercep <- ifelse(data$`Tiros interceptados/90` < umbral, 0,
                                       ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Tiros interceptados/90`)(data$`Tiros interceptados/90`),
                                              ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Tiros interceptados/90`)(data$`Tiros interceptados/90`),
                                                     ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Tiros interceptados/90`)(data$`Tiros interceptados/90`),
                                                            ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Tiros interceptados/90`)(data$`Tiros interceptados/90`), NA)))))

data$percentil_interceptaciones <- ifelse(data$`Interceptaciones/90` < umbral, 0,
                                          ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Interceptaciones/90`)(data$`Interceptaciones/90`),
                                                 ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Interceptaciones/90`)(data$`Interceptaciones/90`),
                                                        ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Interceptaciones/90`)(data$`Interceptaciones/90`),
                                                               ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Interceptaciones/90`)(data$`Interceptaciones/90`), NA)))))


data$percentil_AccionesAT <- ifelse(data$`Acciones de ataque exitosas/90` < umbral, 0,
                                    ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Acciones de ataque exitosas/90`)(data$`Acciones de ataque exitosas/90`),
                                           ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Acciones de ataque exitosas/90`)(data$`Acciones de ataque exitosas/90`),
                                                  ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Acciones de ataque exitosas/90`)(data$`Acciones de ataque exitosas/90`),
                                                         ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Acciones de ataque exitosas/90`)(data$`Acciones de ataque exitosas/90`), NA)))))

data$percentil_Goles <- ifelse(data$`Goles/90` < umbral, 0,
                               ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022" & `Goles/90` >= umbral)$`Goles/90`)(data$`Goles/90`),
                                      ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021" & `Goles/90` >= umbral)$`Goles/90`)(data$`Goles/90`),
                                             ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020" & `Goles/90` >= umbral)$`Goles/90`)(data$`Goles/90`),
                                                    ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019" & `Goles/90` >= umbral)$`Goles/90`)(data$`Goles/90`), 0)))))

data$percentil_xG90 <- ifelse(data$`xG/90` < umbral, 0,
                              ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`xG/90`)(data$`xG/90`),
                                     ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`xG/90`)(data$`xG/90`),
                                            ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`xG/90`)(data$`xG/90`),
                                                   ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`xG/90`)(data$`xG/90`), NA))))) 

data$percentil_Remates <- ifelse(data$`Remates/90` < umbral, 0,
                                 ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Remates/90`)(data$`Remates/90`),
                                        ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Remates/90`)(data$`Remates/90`),
                                               ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Remates/90`)(data$`Remates/90`),
                                                      ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Remates/90`)(data$`Remates/90`), NA))))) 

data$percentil_Asistencias <- ifelse(data$`Asistencias/90` < umbral, 0,
                                     ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Asistencias/90`)(data$`Asistencias/90`),
                                            ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Asistencias/90`)(data$`Asistencias/90`),
                                                   ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Asistencias/90`)(data$`Asistencias/90`),
                                                          ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Asistencias/90`)(data$`Asistencias/90`), NA)))))

data$percentil_Centros <- ifelse(data$`Centros/90` < umbral, 0,
                                 ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Centros/90`)(data$`Centros/90`),
                                        ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Centros/90`)(data$`Centros/90`),
                                               ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Centros/90`)(data$`Centros/90`),
                                                      ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Centros/90`)(data$`Centros/90`), NA)))))

data$percentil_Regates <- ifelse(data$`Regates/90` < umbral, 0,
                                 ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Regates/90`)(data$`Regates/90`),
                                        ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Regates/90`)(data$`Regates/90`),
                                               ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Regates/90`)(data$`Regates/90`),
                                                      ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Regates/90`)(data$`Regates/90`), NA)))))

data$percentil_Carreras <- ifelse(data$`Carreras en progresión/90` < umbral, 0,
                                  ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Carreras en progresión/90`)(data$`Carreras en progresión/90`),
                                         ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Carreras en progresión/90`)(data$`Carreras en progresión/90`),
                                                ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Carreras en progresión/90`)(data$`Carreras en progresión/90`),
                                                       ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Carreras en progresión/90`)(data$`Carreras en progresión/90`), NA)))))

data$percentil_pases <- ifelse(data$`Pases/90` < umbral, 0,
                               ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Pases/90`)(data$`Pases/90`),
                                      ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Pases/90`)(data$`Pases/90`),
                                             ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Pases/90`)(data$`Pases/90`),
                                                    ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Pases/90`)(data$`Pases/90`), NA)))))


data$percentil_pases_adelante <- ifelse(data$`Pases hacia adelante/90` < umbral, 0,
                                        ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Pases hacia adelante/90`)(data$`Pases hacia adelante/90`),
                                               ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Pases hacia adelante/90`)(data$`Pases hacia adelante/90`),
                                                      ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Pases hacia adelante/90`)(data$`Pases hacia adelante/90`),
                                                             ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Pases hacia adelante/90`)(data$`Pases hacia adelante/90`), NA)))))

data$percentil_xA <- ifelse(data$`xA/90` < umbral, 0,
                            ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`xA/90`)(data$`xA/90`),
                                   ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`xA/90`)(data$`xA/90`),
                                          ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`xA/90`)(data$`xA/90`),
                                                 ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`xA/90`)(data$`xA/90`), NA)))))


data$percentil_second_assists <- ifelse(data$`Second assists/90` < umbral, 0,
                                        ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Second assists/90`)(data$`Second assists/90`),
                                               ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Second assists/90`)(data$`Second assists/90`),
                                                      ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Second assists/90`)(data$`Second assists/90`),
                                                             ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Second assists/90`)(data$`Second assists/90`), NA)))))

data$percentil_desmarques <- ifelse(data$`Desmarques/90` < umbral, 0,
                                    ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Desmarques/90`)(data$`Desmarques/90`),
                                           ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Desmarques/90`)(data$`Desmarques/90`),
                                                  ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Desmarques/90`)(data$`Desmarques/90`),
                                                         ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Desmarques/90`)(data$`Desmarques/90`), NA)))))

data$percentil_pases_progresivos <- ifelse(data$`Pases progresivos/90` < umbral, 0,
                                           ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Pases progresivos/90`)(data$`Pases progresivos/90`),
                                                  ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Pases progresivos/90`)(data$`Pases progresivos/90`),
                                                         ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Pases progresivos/90`)(data$`Pases progresivos/90`),
                                                                ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Pases progresivos/90`)(data$`Pases progresivos/90`), NA)))))

data$percentil_duelos_aereos <- ifelse(data$`Duelos aéreos ganados en los 90` < umbral, 0,
                                       ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Duelos aéreos ganados en los 90`)(data$`Duelos aéreos ganados en los 90`),
                                              ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Duelos aéreos ganados en los 90`)(data$`Duelos aéreos ganados en los 90`),
                                                     ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Duelos aéreos ganados en los 90`)(data$`Duelos aéreos ganados en los 90`),
                                                            ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Duelos aéreos ganados en los 90`)(data$`Duelos aéreos ganados en los 90`), NA)))))

data$percentil_pases_largos <- ifelse(data$`Pases largos correctos en los 90` < umbral, 0,
                                      ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Pases largos correctos en los 90`)(data$`Pases largos correctos en los 90`),
                                             ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Pases largos correctos en los 90`)(data$`Pases largos correctos en los 90`),
                                                    ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Pases largos correctos en los 90`)(data$`Pases largos correctos en los 90`),
                                                           ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Pases largos correctos en los 90`)(data$`Pases largos correctos en los 90`), NA)))))

data$percentil_pases_profundidad <- ifelse(data$`Pases en profundidad correctos/90` < umbral, 0,
                                           ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Pases en profundidad correctos/90`)(data$`Pases en profundidad correctos/90`),
                                                  ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Pases en profundidad correctos/90`)(data$`Pases en profundidad correctos/90`),
                                                         ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Pases en profundidad correctos/90`)(data$`Pases en profundidad correctos/90`),
                                                                ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Pases en profundidad correctos/90`)(data$`Pases en profundidad correctos/90`), NA)))))

data$percentil_pases_ultimo_tercio <- ifelse(data$`Pases en el último tercio correctos/90` < umbral, 0,
                                             ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Pases en el último tercio correctos/90`)(data$`Pases en el último tercio correctos/90`),
                                                    ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Pases en el último tercio correctos/90`)(data$`Pases en el último tercio correctos/90`),
                                                           ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Pases en el último tercio correctos/90`)(data$`Pases en el último tercio correctos/90`),
                                                                  ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Pases en el último tercio correctos/90`)(data$`Pases en el último tercio correctos/90`), NA)))))
data$percentil_goles_cabeza <- ifelse(data$`Goles de cabeza/90` < umbral, 0,
                                      ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Goles de cabeza/90`)(data$`Goles de cabeza/90`),
                                             ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Goles de cabeza/90`)(data$`Goles de cabeza/90`),
                                                    ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Goles de cabeza/90`)(data$`Goles de cabeza/90`),
                                                           ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Goles de cabeza/90`)(data$`Goles de cabeza/90`), NA))))) 

data$percentil_duelos_atacantes <- ifelse(data$`Duelos atacantes/90` < umbral, 0,
                                          ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Duelos atacantes/90`)(data$`Duelos atacantes/90`),
                                                 ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Duelos atacantes/90`)(data$`Duelos atacantes/90`),
                                                        ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Duelos atacantes/90`)(data$`Duelos atacantes/90`),
                                                               ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Duelos atacantes/90`)(data$`Duelos atacantes/90`), NA))))) 

data$percentil_toques_penalti <- ifelse(data$`Toques en el área de penalti/90` < umbral, 0,
                                        ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Toques en el área de penalti/90`)(data$`Toques en el área de penalti/90`),
                                               ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Toques en el área de penalti/90`)(data$`Toques en el área de penalti/90`),
                                                      ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Toques en el área de penalti/90`)(data$`Toques en el área de penalti/90`),
                                                             ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Toques en el área de penalti/90`)(data$`Toques en el área de penalti/90`), NA))))) 

data$percentil_aceleraciones <- ifelse(data$`Aceleraciones/90` < umbral, 0,
                                       ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Aceleraciones/90`)(data$`Aceleraciones/90`),
                                              ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Aceleraciones/90`)(data$`Aceleraciones/90`),
                                                     ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Aceleraciones/90`)(data$`Aceleraciones/90`),
                                                            ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Aceleraciones/90`)(data$`Aceleraciones/90`), NA))))) 

data$percentil_faltas_recibidas <- ifelse(data$`Faltas recibidas/90` < umbral, 0,
                                          ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Faltas recibidas/90`)(data$`Faltas recibidas/90`),
                                                 ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Faltas recibidas/90`)(data$`Faltas recibidas/90`),
                                                        ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Faltas recibidas/90`)(data$`Faltas recibidas/90`),
                                                               ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Faltas recibidas/90`)(data$`Faltas recibidas/90`), NA))))) 

data$percentil_pases_laterales <- ifelse(data$`Pases laterales/90` < umbral, 0,
                                         ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Pases laterales/90`)(data$`Pases laterales/90`),
                                                ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Pases laterales/90`)(data$`Pases laterales/90`),
                                                       ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Pases laterales/90`)(data$`Pases laterales/90`),
                                                              ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Pases laterales/90`)(data$`Pases laterales/90`), NA))))) 

data$percentil_pases_atras <- ifelse(data$`Pases hacia atrás/90` < umbral, 0,
                                     ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Pases hacia atrás/90`)(data$`Pases hacia atrás/90`),
                                            ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Pases hacia atrás/90`)(data$`Pases hacia atrás/90`),
                                                   ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Pases hacia atrás/90`)(data$`Pases hacia atrás/90`),
                                                          ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Pases hacia atrás/90`)(data$`Pases hacia atrás/90`), NA))))) 

data$percentil_faltas <- ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Faltas/90`)(data$`Faltas/90`),
                                ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Faltas/90`)(data$`Faltas/90`),
                                       ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Faltas/90`)(data$`Faltas/90`),
                                              ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Faltas/90`)(data$`Faltas/90`), NA))))

data$percentil_duelos_defensivos <- ifelse(data$Temporada == "2021-2022", 100 * ecdf(subset(data, Temporada == "2021-2022")$`Duelos defensivos/90`)(data$`Duelos defensivos/90`),
                                           ifelse(data$Temporada == "2020-2021", 100 * ecdf(subset(data, Temporada == "2020-2021")$`Duelos defensivos/90`)(data$`Duelos defensivos/90`),
                                                  ifelse(data$Temporada == "2019-2020", 100 * ecdf(subset(data, Temporada == "2019-2020")$`Duelos defensivos/90`)(data$`Duelos defensivos/90`),
                                                         ifelse(data$Temporada == "2018-2019", 100 * ecdf(subset(data, Temporada == "2018-2019")$`Duelos defensivos/90`)(data$`Duelos defensivos/90`), NA))))



data_2021_2022 <- subset(data, Temporada == "2021-2022")




#DUMBBELL 1
nuevo_dataframe <- select(data_2021_2022, Jugador, percentil_aceleraciones, percentil_desmarques, percentil_faltas_recibidas,
                          percentil_Carreras, percentil_Regates, percentil_toques_penalti, percentil_duelos_atacantes,
                          percentil_AccionesAT, percentil_Remates, percentil_xG90, percentil_Goles) %>%
  distinct()

nuevo_dataframe <- nuevo_dataframe %>% 
  rename(Aceleraciones = percentil_aceleraciones, 
         Desmarques = percentil_desmarques, 
         FaltasRecib = percentil_faltas_recibidas,
         CarrProg = percentil_Carreras,
         Regates = percentil_Regates,
         ToqArea = percentil_toques_penalti,
         DuelAt = percentil_duelos_atacantes,
         AccAt = percentil_AccionesAT,
         Remates = percentil_Remates,
         xG = percentil_xG90,
         Goles = percentil_Goles)

DumbBell1 <- pivot_longer(nuevo_dataframe, 
                          cols = c("Aceleraciones", "Desmarques", "FaltasRecib", "CarrProg", "Regates", "ToqArea",
                                   "DuelAt", "AccAt", "Remates", "xG", "Goles"), 
                          names_to = "Variable", 
                          values_to = "Valor")

#DUMBBELL 2

nuevo_dataframe2 <- select(data_2021_2022, Jugador, percentil_Centros, percentil_pases_atras, percentil_pases_laterales,
                           percentil_pases_adelante, percentil_pases_largos, percentil_pases_progresivos, percentil_pases_profundidad,
                           percentil_pases_ultimo_tercio, percentil_pases, percentil_xA, percentil_Asistencias) %>%
  distinct()

nuevo_dataframe2 <- nuevo_dataframe2 %>% 
  rename(Centros = percentil_Centros,
         P_Atrás = percentil_pases_atras,
         P_Lat = percentil_pases_laterales,
         P_Adel = percentil_pases_adelante,
         P_Lar = percentil_pases_largos,
         P_Prog = percentil_pases_progresivos,
         P_Prof = percentil_pases_profundidad,
         P_UT = percentil_pases_ultimo_tercio,
         Pases = percentil_pases,
         xA = percentil_xA,
         Asis = percentil_Asistencias)

DumbBell2 <- pivot_longer(nuevo_dataframe2, 
                          cols = c("Centros", "P_Atrás", "P_Lat", "P_Adel", "P_Lar", "P_Prog", "P_Prof", "P_UT", "Pases", "xA", "Asis"), 
                          names_to = "Variable", 
                          values_to = "Valor")


#DUMBBELL 3

nuevo_dataframe3 <- select(data_2021_2022, Jugador, percentil_faltas, percentil_TirosIntercep, percentil_interceptaciones,
                           percentil_entradas, percentil_duelos_aereos, percentil_duelos_defensivos, percentil_ADR) %>%
  distinct()

nuevo_dataframe3 <- nuevo_dataframe3 %>% 
  rename(Faltas = percentil_faltas,
         TirInt = percentil_TirosIntercep,
         Inter = percentil_interceptaciones,
         Ent = percentil_entradas,
         DuelAer = percentil_duelos_aereos,
         DuelDef = percentil_duelos_defensivos,
         AccDef = percentil_ADR
         
  )

DumbBell3 <- pivot_longer(nuevo_dataframe3, 
                          cols = c("Faltas", "TirInt", "Inter", "Ent", "DuelAer", "DuelDef", "AccDef"), 
                          names_to = "Variable", 
                          values_to = "Valor")



#PESTAÑA LOPETEGUI


  
Julen_Lopetegui <- read_excel("Julen Lopetegui.xlsx")






#Nuevas Variables 
Julen_Lopetegui$Percentil_PuntosxPartido <- ecdf(Julen_Lopetegui$PuntosxPartido)(Julen_Lopetegui$PuntosxPartido)


Julen_Lopetegui$Percentil_Edad <- ecdf(Julen_Lopetegui$EdadPromedio)(Julen_Lopetegui$EdadPromedio)


Julen_Lopetegui["Goles AF x Partido"] <- Julen_Lopetegui$GolesFavor / Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_Goles_AF_x_Partido <- ecdf(Julen_Lopetegui$`Goles AF x Partido`)(Julen_Lopetegui$`Goles AF x Partido`)


Julen_Lopetegui["Goles EC x Partido"] <- Julen_Lopetegui$GolesContra / Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_Goles_EC_x_Partido <- ecdf(Julen_Lopetegui$`Goles EC x Partido`)(Julen_Lopetegui$`Goles EC x Partido`)


Julen_Lopetegui$Percentil_Posesion<- ecdf(Julen_Lopetegui$PorcentajePosesion)(Julen_Lopetegui$PorcentajePosesion)


Julen_Lopetegui["Tiro de jugada x Partido"] <- Julen_Lopetegui$TiroxJugada / Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_TirodeJugada <- ecdf(Julen_Lopetegui$`Tiro de jugada x Partido`)(Julen_Lopetegui$`Tiro de jugada x Partido`)


Julen_Lopetegui["Tiro tras regate x Partido"] <- Julen_Lopetegui$TiroTrasRegate / Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_TiroTrasRegate <- ecdf(Julen_Lopetegui$`Tiro tras regate x Partido`)(Julen_Lopetegui$`Tiro tras regate x Partido`)


Julen_Lopetegui["Tiro x ABP"] <- Julen_Lopetegui$TiroxABP / Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_TiroxABP <- ecdf(Julen_Lopetegui$`Tiro x ABP`)(Julen_Lopetegui$`Tiro x ABP`)


Julen_Lopetegui$Percentil_DistanciaTiro<- ecdf(Julen_Lopetegui$AvgDistTiro)(Julen_Lopetegui$AvgDistTiro)


Julen_Lopetegui["Pases para tirar"] <- Julen_Lopetegui$AccionesCreacionTiro / Julen_Lopetegui$Pases_Completados
Julen_Lopetegui$Percentil_PasesPararTirar <- ecdf(Julen_Lopetegui$`Pases para tirar`)(Julen_Lopetegui$`Pases para tirar`) #Julen_Lopetegui$Percentil_PasesPararTirar <- 1 - ecdf(Julen_Lopetegui$`Pases para tirar`)(Julen_Lopetegui$`Pases para tirar`)


Julen_Lopetegui["FDJ x Partido"] <- Julen_Lopetegui$FuerasDeJuego / Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_FDJ <- ecdf(Julen_Lopetegui$`FDJ x Partido`)(Julen_Lopetegui$`FDJ x Partido`)


Julen_Lopetegui["Regates x Partido"] <- Julen_Lopetegui$RegatesIntentados / Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_Regates <- ecdf(Julen_Lopetegui$`Regates x Partido`)(Julen_Lopetegui$`Regates x Partido`)


Julen_Lopetegui["Pases x Partido"] <- Julen_Lopetegui$Pases_Completados / Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_Pases <- ecdf(Julen_Lopetegui$`Pases x Partido`)(Julen_Lopetegui$`Pases x Partido`)


Julen_Lopetegui["Pases Largos x Partido"] <- Julen_Lopetegui$Pases_Largos_Completados / Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_PasesLargos <- ecdf(Julen_Lopetegui$`Pases Largos x Partido`)(Julen_Lopetegui$`Pases Largos x Partido`)


Julen_Lopetegui["Pases Progresivos x Pases"] <- Julen_Lopetegui$Pases_Progresivos/ Julen_Lopetegui$Pases_Completados
Julen_Lopetegui$Percentil_Progresividad <- ecdf(Julen_Lopetegui$`Pases Progresivos x Pases`)(Julen_Lopetegui$`Pases Progresivos x Pases`)


Julen_Lopetegui["Cambios Orientación x Partido"] <- Julen_Lopetegui$Cambios_Orientación / Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_CambiosOrientacion <- ecdf(Julen_Lopetegui$`Cambios Orientación x Partido`)(Julen_Lopetegui$`Cambios Orientación x Partido`)


Julen_Lopetegui["Toques primer tercio x Partido"] <- Julen_Lopetegui$ToquesPrimerTercio / Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_Toques1Tercio <- ecdf(Julen_Lopetegui$`Toques primer tercio x Partido`)(Julen_Lopetegui$`Toques primer tercio x Partido`)


Julen_Lopetegui["Toques segundo tercio x Partido"] <- Julen_Lopetegui$ToquesTercioMedio / Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_Toques2Tercio <- ecdf(Julen_Lopetegui$`Toques segundo tercio x Partido`)(Julen_Lopetegui$`Toques segundo tercio x Partido`)


Julen_Lopetegui["Toques tercer tercio x Partido"] <- Julen_Lopetegui$ToquesTercioContrario / Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_Toques3Tercio <- ecdf(Julen_Lopetegui$`Toques tercer tercio x Partido`)(Julen_Lopetegui$`Toques tercer tercio x Partido`)


Julen_Lopetegui["Entradas primer tercio x Partido"] <- Julen_Lopetegui$EntradasTPropio / Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_Entradas1Tercio <- ecdf(Julen_Lopetegui$`Entradas primer tercio x Partido`)(Julen_Lopetegui$`Entradas primer tercio x Partido`)


Julen_Lopetegui["Entradas segundo tercio x Partido"] <- Julen_Lopetegui$EntradasTMedio / Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_Entradas2Tercio <- ecdf(Julen_Lopetegui$`Entradas segundo tercio x Partido`)(Julen_Lopetegui$`Entradas segundo tercio x Partido`)


Julen_Lopetegui["Entradas tercer tercio x Partido"] <- Julen_Lopetegui$EntradasTContrario/ Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_Entradas3Tercio <- ecdf(Julen_Lopetegui$`Entradas tercer tercio x Partido`)(Julen_Lopetegui$`Entradas tercer tercio x Partido`)

Julen_Lopetegui["Despejes x Partido"] <- Julen_Lopetegui$Despejes/ Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_Despejes <- ecdf(Julen_Lopetegui$`Despejes x Partido`)(Julen_Lopetegui$`Despejes x Partido`)


Julen_Lopetegui["Duelos Aereos x Partido"] <- (Julen_Lopetegui$DuelosAGanados + Julen_Lopetegui$DuelosAPerdidos)/ Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_DuelosA <- ecdf(Julen_Lopetegui$`Duelos Aereos x Partido`)(Julen_Lopetegui$`Duelos Aereos x Partido`)


Julen_Lopetegui$Percentil_PPDA <- ecdf(Julen_Lopetegui$PPDA)(Julen_Lopetegui$PPDA)
Julen_Lopetegui$Percentil_PPDA[is.na(Julen_Lopetegui$Percentil_PPDA)] <- 0

Julen_Lopetegui$Percentil_OPPDA <- ecdf(Julen_Lopetegui$OPPDA)(Julen_Lopetegui$OPPDA)
Julen_Lopetegui$Percentil_OPPDA[is.na(Julen_Lopetegui$Percentil_OPPDA)] <- 0

Julen_Lopetegui["Pases al espacio x Partido"] <- Julen_Lopetegui$Pases_Espacio/ Julen_Lopetegui$Partidos
Julen_Lopetegui$Percentil_PasesEspacio <- ecdf(Julen_Lopetegui$`Pases al espacio x Partido`)(Julen_Lopetegui$`Pases al espacio x Partido`)

#Renombrando columnas

Julen_Lopetegui1 <- rename(Julen_Lopetegui, 
                           "Pases completados" = "Pases_Completados",
                           "Pases intentados" = "Pases_Intentados",
                           "% Pases completados" = "%Pases_Completados",
                           "Distancia total de los pases" = "DistTotPases",
                           "Distancia de los pases progresivos" = "DistPasProg",
                           "Pases cortos completados" = "Pases_Cortos_Completados",
                           "Pases cortos intentados" = "Pases_Cortos_Intentados",
                           "% Pases cortos" = "%Pases_Cortos",
                           "Pases medios completados" = "Pases_Medios_Completados",
                           "Pases medios intentados" = "Pases_Medios_Intentados",
                           "% Pases medios" = "%Pases_Medios",
                           "Pases largos completados" = "Pases_Largos_Completados",
                           "Pases largos intentados" = "Pases_Largos_Intentados",
                           "% Pases largos" = "%Pases_Largos",
                           "Pases Clave" = "PasesClave",
                           "Pases último tercio" = "Pases_UltimoTercio",
                           "Pases en el área" = "Pases_Area",
                           "Centros al área" = "Centros_Area",
                           "Pases progresivos" = "Pases_Progresivos",
                           "Tiros a puerta" = "Tiros_Puerta",
                           "% Tiros a puerta" = "%TirosPuerta",
                           "Distancia de tiro media" = "AvgDistTiro",
                           "Tiros de Tl" = "TirosDeTL",
                           "Penales marcados" = "Penales",
                           "Penales totales" = "PKatt",
                           "Goles - xG" = "GolesMenosxG",
                           "npGoles - xG" = "npGolesMenosxG",
                           "Pases vivos" = "Pases_Vivos",
                           "Pases desde ABP" = "Pases_deParado",
                           "Pases de TL" = "PasesTL",
                           "Pases al espacio" = "Pases_Espacio",
                           "Cambios de orientación" = "Cambios_Orientación",
                           "Saques de banda" = "Saques_Banda",
                           "Córners cerrados" = "Corners_Cerrados",
                           "Córners abiertos" = "Corners_Abiertos",
                           "Córners rectos" = "Corners_Rectos",
                           "Acciones de creación de tiro90" = "AccionesCreacionTIro90",
                           "Tiros bloqueados" = "Tiros_Bloqueados",
                           "Pases bloqueados" = "Pases_Bloqueados",
                           
                           
)

#Subset para input de tabla 


datos_sin_equipos <- subset(Julen_Lopetegui1, select = c(-Equipo, -N_jugadores, -Liga,
                                                         -Percentil_PuntosxPartido,
                                                         -Percentil_Edad,
                                                         -Percentil_Goles_AF_x_Partido,
                                                         -Percentil_Goles_EC_x_Partido,
                                                         -Percentil_Posesion,
                                                         -Percentil_TirodeJugada,
                                                         -Percentil_TiroTrasRegate,
                                                         -Percentil_TiroxABP,
                                                         -Percentil_DistanciaTiro,
                                                         -Percentil_PasesPararTirar,
                                                         -Percentil_FDJ,
                                                         -Percentil_Regates,
                                                         -Percentil_Pases,
                                                         -Percentil_PasesLargos,
                                                         -Percentil_Progresividad,
                                                         -Percentil_CambiosOrientacion,
                                                         -Percentil_Toques1Tercio,
                                                         -Percentil_Toques2Tercio,
                                                         -Percentil_Toques3Tercio,
                                                         -Percentil_Entradas1Tercio,
                                                         -Percentil_Entradas2Tercio,
                                                         -Percentil_Entradas3Tercio,
                                                         -Percentil_Despejes,
                                                         -Percentil_DuelosA,
                                                         -Percentil_PPDA,
                                                         -Percentil_OPPDA,
                                                         -Percentil_PasesEspacio))

#Pestaña Wolves


Wolves <- read_excel('Wolves.xlsx')
Premier <- read_excel('Premier.xlsx')


nombres_columna <- colnames(Premier)
nombres_columna_nuevos <- gsub(" ", "_", nombres_columna)
colnames(Premier) <- nombres_columna_nuevos


Premier$image <- c("Arsenal.png", "AstonVilla.png", "Bournemouth.png", "Brentford.png", "Brighton.png" ,"Chelsea.png", "Crystal Palace.png", 
                   "Everton.png", "Fulham.png", "Leeds.png", "Leicester.png", "Liverpool.png", "Manchester City.png", "Manchester United.png", 
                   "Newcastle.png", "Nottingham.png", "Southampton.png", "Tottenham.png", "West Ham.png", "Wolves.png")


