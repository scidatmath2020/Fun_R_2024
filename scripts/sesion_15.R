vec1 = c(1,4,1,8,6)
vec2 = c(6,2,8,0,-1)

vec_suma = c()

for(indice in 1:5){
  vec_suma[indice] = vec1[indice]+vec2[indice]
}

vec_suma

vec1+vec2


##########
##########  Uso de tapply
##########

data(mtcars)
mtcars

########## 

### Calcular el mínimo de disp por cilindros

tapply(mtcars$disp,mtcars$cyl,min)

# calcular el mínimo de disp por cilindros (cyl) y engranaje (gear)

# Engranes
unique(mtcars$gear)

# Cilindros
unique(mtcars$cyl)

minimo_sin_na = function(vector){
  return(min(vector,na.rm=TRUE))
}

tapply(mtcars$disp,
       list(mtcars$cyl,mtcars$gear),
       min)

min(c(2,4,NA))
minimo_sin_na(c(2,4,NA))

tapply(mtcars$disp,
       list(mtcars$cyl,mtcars$gear),
       minimo_sin_na)

# calcular el promedio de disp por cilindros (cyl), engranaje (gear) y transmisión (am)

respuesta = tapply(mtcars$disp,
       list(mtcars$cyl,mtcars$gear,mtcars$am),
       mean)

dim(respuesta)

respuesta[,,1]

respuesta[,,2]


# calcular el promedio de disp por para transmisión automática

tapply(mtcars$disp,mtcars$am==0,mean)

# calcular el promedio de disp por para transmisión automática y por cilindros

tapply(mtcars$disp,list(mtcars$am==0,mtcars$cyl),mean)

# calcular el promedio de disp por para transmisión automática y de 4 cilindros

tapply(mtcars$disp,list(mtcars$am==0,mtcars$cyl==4),mean)


########################## Calcular la media, mediana y el total de ventqs

ventas_data <- data.frame(
  Region = c("Norte", "Norte", "Sur", "Sur", "Este", "Este", "Oeste", "Oeste", "Norte", "Sur"),
  Producto = c("A", "B", "A", "B", "A", "B", "A", "B", "A", "A"),
  Ventas = c(150, 200, 250, 300, 100, 120, 80, 90, 200, 220)
)

tapply(ventas_data$Ventas,ventas_data$Producto,mean)
tapply(ventas_data$Ventas,ventas_data$Producto,median)
tapply(ventas_data$Ventas,ventas_data$Producto,sum)

estadisticas = function(vector){
  auxiliar = c(mean(vector),median(vector),sum(vector))
  names(auxiliar) = c("promedio","mediana","total")
  return(auxiliar)
}

tapply(ventas_data$Ventas,ventas_data$Producto,estadisticas)

##########
##########  Uso de lapply
##########

## Construir una lista de textos: textos = list("Texto 1","Texto 2",...,"Texto n")

mi_funcion1 <- function(letrero){
  paste0("Texto_",letrero)
}

mi_funcion1("Un dataframe es una tabla")

lapply(1:100,mi_funcion1)

#Dada una lista de textos, convertir las vocales a mayúsculas


mis_frases <- c("La vida es una obra teatral que no importa cuánto haya durado, sino lo bien que haya sido representada",
                "Elige un trabajo que ames, y no tendrás que trabajar un solo día de tu vida",
                "Nuestra mayor gloria no es no caer nunca, sino levantarnos cada vez que caemos")

convertidor = function(texto){
  n = nchar(texto)
  letras = c()
  for(indice in 1:n){
    letra_actual = substr(texto,indice,indice)
    if(letra_actual %in% c("a","e","i","o","u","á","é","í","ó","ú")){
      letra_actual = toupper(letra_actual)
      }
    letras[indice] = letra_actual
    }
  return(paste(letras,collapse=""))
}

lapply(mis_frases,convertidor)


######
# Lectura de archivos:


setwd("C:/Users/Usuario/Documents/scidata/24_01_aabd/librería")

dir()  # MUESTRA los elementos que hay en esa carpeta

archivos_interes = dir()[endsWith(dir(),".csv")]
archivos = lapply(archivos_interes,read.csv)
names(archivos) = unlist(strsplit(archivos_interes,".csv"))

archivos$ventas

##########
##########  Uso de sapply
##########


rompimiento = function(texto){
  strsplit(texto,".csv")[[1]]
}

names(archivos) = sapply(archivos_interes,rompimiento)

#############

#### Un sapply con simplify=FALSE y USE.NAMES = FALSE es lo mismo que un lapply

lapply(mis_frases,convertidor)

sapply(mis_frases,convertidor,simplify=FALSE,USE.NAMES=FALSE)
       
## Insertar una columna a mtcars con la marca del automovil

### Crea una lista donde cada elemento es un vector; dicho vector se
### forma partiendo el nombre de la fila por los espacios

#strsplit("Ford Pantera L"," ")

nombres_partidos = strsplit(row.names(mtcars)," ")

primer_elemento = function(vector){
  vector[1]
}

primer_elemento(c("Ford","Pantera","L"))
primer_elemento(c("Maserati","Bora"))


mtcars$marca = sapply(nombres_partidos,primer_elemento)

mtcars





nombres_partidos <- strsplit(row.names(mtcars)," ")
mtcars$marca <- sapply(nombres_partidos,function(x){x[1]})



  
}