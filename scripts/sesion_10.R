######## 
######## Siempre establecerse en la carpeta donde vamos a trabajar
########


# Tener la ruta como texto 
ruta = "C:/Users/Usuario/Documents/scidata/24_02_fun_r/mis_canciones"
ruta_sis = "C:/Users/Usuario/Documents/scidata/24_02_fun_r/sismos"

##### Dirigirse a la carpeta de trabajo
setwd(ruta)
setwd(ruta_sis)

##### Te dice en qué lugar de tu computadora estás trabajando.
getwd()

######## 
######## Leer el, o los, archivo que me interesen
########

tabla_canciones = read.csv("canciones.csv")

######## Nos hacemos a una idea de la información en nuestra tabla

str(tabla_canciones)

View(tabla_canciones)

names(tabla_canciones)

########

# Extrae toda la información de Santana ft. Rob Thomas

tabla_canciones[tabla_canciones$Artista == "Santana ft. Rob Thomas",] 

## Función unique: me da los elementos diferentes en un vector
unique(tabla_canciones$Artista)  # Esto me dice que en mi tabla tengo 79 artistas diferentes

## Table: me indica cuántas veces aparece un elemento dentro de un vector
table(tabla_canciones$Artista)

### ver la tabla de frecuencias de artistas ordenadas de menor a mayor
View(sort(table(tabla_canciones$Artista)))

### ver la tabla de frecuencias de artistas ordenadas de mayor a menor
View(sort(table(tabla_canciones$Artista),decreasing=TRUE))

######## Extrae toda la información de The Beatles
tabla_canciones[tabla_canciones$Artista == "The Beatles",]  

######## Se hace la visualización
View(tabla_canciones[tabla_canciones$Artista == "The Beatles",])

######## Crear la tabla de información sobre The Beatles
tabla_beatles = tabla_canciones[tabla_canciones$Artista == "The Beatles",]  

######## Veamos a Queen con David Bowie

########  Queen & David Bowie

#### función startswith(vector o texto, prefijo). Lo que hace es ver si cada elemento
#### del vector o el texto comienzan con prefijo
#### También existe el endsWith(vector o texto, sufijo)

startsWith("Hola a todos","SciData")
startsWith("SciData me gusta","SciData")

tabla_queen = tabla_canciones[startsWith(tabla_canciones$Artista,"Queen"),]
View(tabla_queen)

#### función grepl(texto a buscar, texto donde se busca)
grepl("SciData","Me gusta Scidata porque ahí puedo aprender")

tabla_canciones[grepl("Queen",tabla_canciones$Artista),]

##### Añadir una columna que indique la duración en minutos y decimales
##### Por ejemplo, 4:30 significa 4.5

tabla_canciones$Duracion[1:10]


##### Función substr(texto,posicion inicial,posicion final)
substr("Hola a todos",4,7)
substr("SciData me gusta mucho",5,10)

### Volviendo al problema de los minutos

#### Creo el vector que tiene cuántos minutos dura cada canción
minutos = as.numeric(substr(tabla_canciones$Duracion,1,2))

#### Creo el vector que tiene los segundos restantes de cada canción y los 
#### convierto a decimal; es decir, fracción de minuto

segundos = as.numeric(substr(tabla_canciones$Duracion,4,5))/60

#### Resuelvo el problema: añado la columna duracion_minutos que es donde
#### se encuentran los minutos en punto decimal

tabla_canciones$Duracion_minutos = minutos + segundos


class(tabla_canciones$Duracion_minutos)


##### Calcular el promedio de duración de las canciones de The Beatles

#### Obtenemos las duraciones en minutos de cada una de sus canciones
tabla_canciones[tabla_canciones$Artista == "The Beatles",]$Duracion_minutos

#### Calculamos la media
mean(tabla_canciones[tabla_canciones$Artista == "The Beatles",]$Duracion_minutos)

#### Calculemos la desviación y la varianza
# Desviación estándard
sd(tabla_canciones[tabla_canciones$Artista == "The Beatles",]$Duracion_minutos)

# Varianza
var(tabla_canciones[tabla_canciones$Artista == "The Beatles",]$Duracion_minutos)

#####################
######## Calcular la duración promedio de las canciones DE CADA UNO DE LOS ARTISTAS
#####################

###### Función tapply. Sirve para hacer cálculos con discriminación
##### tapply(columna a la que le vas a aplicar los cálculos,
#            columna con la cual se va a hacer la discriminación,
#            operación a aplicar)

duracion_promedio = tapply(tabla_canciones$Duracion_minutos,
                           tabla_canciones$Artista,mean)

View(as.data.frame(duracion_promedio))




##########

getwd()

write.csv(tabla_beatles,"mi_tabla.csv")


































