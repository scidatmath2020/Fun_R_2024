setwd("C:/Users/Usuario/Documents/scidata/24_02_fun_r/covid")
covid_muestra = read.csv("covid_muestra.csv")

###############################################
############## Limpieza de datos ##############
###############################################

covid_confirmados = covid_muestra[covid_muestra$CLASIFICACION_FINAL %in% c(1,2,3),]
covid_confirmados = covid_confirmados[covid_confirmados$ENTIDAD_RES %in% c(1:32),]

###############################################
############ Enfermedad y decesos #############
###############################################

## Contar el total de contagios = 16463
nrow(covid_confirmados)

## Contar el total de defunciones = 1241

table(covid_confirmados$FECHA_DEF)
covid_confirmados$muerte = ifelse(covid_confirmados$FECHA_DEF != "9999-99-99",1,0)
table(covid_confirmados$muerte)

## Calcular el porcentaje de defunciones respecto de contagios: 7.5381

1241*100/16463

## Mostrar las 10 entidades con mas contagios junto con el total de contagios

covid_confirmados$factor = 1

catalogo_entidades = read.csv("catalogo_entidades.csv")

covid_confirmados = merge(covid_confirmados,
      catalogo_entidades,
      by.x="ENTIDAD_RES",
      by.y="CLAVE_ENTIDAD",
      all.x=TRUE)

total_entidad = tapply(covid_confirmados$factor,
       covid_confirmados$ENTIDAD_FEDERATIVA,
       sum)

entidad_contagios = data.frame(total_entidad)
entidad_contagios$entidad = row.names(entidad_contagios)
entidad_contagios = entidad_contagios[,c(2,1)]

entidad_ordenados = entidad_contagios[order(entidad_contagios$total_entidad,
                                            decreasing = TRUE),]

entidad_ordenados[1:10,]

## Mostrar las 32 entidades con contagios segregados por sexo

tapply(covid_confirmados$factor,list(covid_confirmados$ENTIDAD_FEDERATIVA,
                                     covid_confirmados$SEXO),
       sum)

## Encontrar la fecha con más contagios registrados.

tot_fecha = tapply(covid_confirmados$factor,
       covid_confirmados$FECHA_SINTOMAS,
       sum)

tot_fecha[tot_fecha == max(tot_fecha)]

#### Las tres entidades que más aportaron en dicha fecha

covid_confirmados_fecha_max = 
  covid_confirmados[covid_confirmados$FECHA_SINTOMAS == "01/08/2021",]

#### TAREA: RESOLVER EL PUNTO ANTERIOR

#### TAREA: - ¿Qué porcentaje representó cada una de esas 
# entidades respecto del total de contagios de 
# dicha fecha?

#### TAREA: ¿En qué fecha se dio la mayor cantidad de muertes?

###############################################
######### Condición de intubamiento ###########
###############################################

# Calcular de cuántos se conoce la condición

condicion_intub = covid_confirmados[covid_confirmados$INTUBADO %in% c(1,2),]
nrow(condicion_intub)

condicion_si = condicion_intub[condicion_intub$INTUBADO == 1,]
nrow(condicion_si)

nrow(condicion_si)*100/nrow(condicion_intub)

table(condicion_intub$muerte)

## Calcular qué porcentaje de las muertes anteriores requirió intubación

table(condicion_si$muerte)

254*100/1141

###############################################
#### Distribución porcentual de contagios #####
###############################################

### Mostrar, del total de contagios, la tabla de Pareto
##  respecto de las entidades

entidad_ordenados

tot_contagios = sum(entidad_ordenados$total_entidad)

entidad_ordenados$acumulado = cumsum(entidad_ordenados$total_entidad)

entidad_ordenados$per = entidad_ordenados$acumulado*100/tot_contagios

######################
######################
######################




covid
covid_confirmado_or = covid[covid$CLASIFICACION_FINAL %in% c(1,2,3),]
covid_confirmado_or$muerte = ifelse(covid_confirmado_or$FECHA_DEF != "9999-99-99",1,0)

table(covid_confirmado_or[covid_confirmado_or$EDAD < 12,]$muerte)

covid_confirmado_or$factor = 1

covid_confirmado_menores = covid_confirmado_or[covid_confirmado_or$EDAD < 12,]


tapply(covid_confirmado_menores$factor,list(covid_confirmado_menores$ENTIDAD_RES,
                                            covid_confirmado_menores$muerte == 1),sum)



