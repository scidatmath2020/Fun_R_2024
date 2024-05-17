mi_vec1  = c(1,5,27,5,187,34) 
mi_vec2  = c(45,848,24,7) 
mi_vec3  = c(87,8789,23) 
mi_vec4  = c(1,32) 
mi_vec5  = c(2022)
mi_vec6  = c(98,977,88,23,35,872) 
mi_vec7  = c(83,97,9000)


### Para cada uno, queremos calcular la suma de todos sus elementos exceptuando
### el valor máximo y el valor mínimo


sum(mi_vec1)-max(mi_vec1)-min(mi_vec1)
sum(mi_vec2)-max(mi_vec2)-min(mi_vec2)
sum(mi_vec3)-max(mi_vec3)-min(mi_vec3)
sum(mi_vec4)-max(mi_vec4)-min(mi_vec4)
sum(mi_vec5)-max(mi_vec5)-min(mi_vec5)
sum(mi_vec6)-max(mi_vec6)-min(mi_vec6)
sum(mi_vec7)-max(mi_vec7)-min(mi_vec7)

###### Vamos a crear una función que se alimente con un vector y devuelva la suma
##### de sus elementos exceptuando al máximo y al mínimo

mi_funcion = function(vector){
  suma = sum(vector)
  maximo = max(vector)
  minimo = min(vector)
  return(suma-maximo-minimo)
}

class(mi_funcion(mi_vec1))

mi_funcion(c(27,15,97,23))

######## Una función que no tiene argumentos

saludar = function(){
  print("Hola a todos.")
}

class(saludar())

######## Diferencia con función que termina con print

mi_funcion_2 = function(vector){
  suma = sum(vector)
  maximo = max(vector)
  minimo = min(vector)
  print(suma-maximo-minimo)
}

class(mi_funcion_2(mi_vec1))

mi_funcion(mi_vec1)+1000 # mi_funcion termina con un return
mi_funcion_2(mi_vec1)+1000 # mi_funcion_2 termina con un print


###### EL RETURN SE UTILIZA CUANDO LA RESPUESTA DE LA FUNCIÓN SE VAYA A UTILZAR
###### POSTERIORMENTE. pOR EJEMPLO, SI A LA RESPUESTA LE VAS A SUMAR ALGO

###### EN CAMBIO, EL PRINT SE UTILIZA CUANDO ÚNICAMENTE QUIERES MOSTRAR UN AVISO.
###### ES DECIR, LA RESPUESTA DE LA FUNCIÓN NO SE VA A UTILZAR NUEVAMENTE

## Una función que toma cualquier dataframe X con datos numéricos y nos devuelve
## el mismo dataframe con un renglón formado por el promedio de cada una 
## de las columnas


mi_data <- data.frame(val1 = c(1,4,2,5),
                      val2 = c(2,1,1,6),
                      val3 = c(12,4,28,2))


mi_funcion3 = function(tabla){
  promedios = colMeans(tabla)
  nvo_renglon = paste("promedio",names(tabla),sep="_")
  return(rbind(tabla,nvo_renglon,promedios))
  #return(nvo_renglon)
}

setwd("C:/Users/Usuario/Documents/scidata/24_02_fun_r")

write.csv(mi_funcion3(mi_data),"salida.csv",row.names=FALSE)

#################

mi_funcion4 = function(tabla){
  promedios = colMeans(tabla)
  auxiliar = rbind(NA,tabla,promedios)
  auxiliar = cbind(NA,auxiliar)
  auxiliar[1,1] = "Tabla"
  auxiliar[nrow(auxiliar),1] = "Promedios"
  return(auxiliar)
}


View(mi_funcion4(mi_data))

write.table(mi_funcion4(mi_data),
          "salida_2.csv",
          sep = ",",
          na = "NA",
          row.names=FALSE,
          col.names=FALSE)

otra_data = data.frame(col1=c(1,5,2,3,5),
                       col2=c(1,6,2,33,5)
                       )

mi_funcion4(otra_data)

###########

# Crear una función que acepte un vector de characteres y 
# forme una palabra con la primer letra de cada entrada

# mi_funcion5(c("Hola","a","todos","linda tarde")) = "Hatlt" 


mi_funcion5 = function(vector_de_textos){
  letras = substr(vector_de_textos,1,1)
  return(paste(letras,collapse=""))
}

mi_funcion5(c("Hola","a","todos","linda tarde"))
mi_funcion5(c("SciData","Manuel","Esta tarde vi llover"))

######################
######################
######################

ruta = "C:/Users/Usuario/Documents/scidata/24_02_fun_r/mis_canciones"
setwd(ruta)

tabla_canciones = read.csv("canciones.csv")

minutos = as.numeric(substr(tabla_canciones$Duracion,1,2))
segundos = as.numeric(substr(tabla_canciones$Duracion,4,5))/60

tabla_canciones$Duracion_minutos = minutos + segundos


View(tabla_canciones)


###### Obtener los artistas que aparezcan 3 o mas veces


frecuencias = as.data.frame(table(tabla_canciones$Artista))
artistas_frecuentes = frecuencias[frecuencias$Freq > 2,]


###### Para cada artista anterior, quiero ver las tres canciones
###### con mayor duración ordenadas de mayor a menor. Quiero que 
###### se muestre también el álbum

extractor = function(artista){
  auxiliar = tabla_canciones[tabla_canciones$Artista == artista,]
  auxiliar = auxiliar[order(auxiliar$Duracion_minutos,decreasing = TRUE),] 
  auxiliar = auxiliar[1:3,c("Titulo","Album","Duracion")]
  auxiliar = rbind(NA,auxiliar)
  auxiliar = cbind(NA,auxiliar)
  auxiliar[1,1] = artista
  return(auxiliar)
}

extractor("Michael Jackson")


#############################




