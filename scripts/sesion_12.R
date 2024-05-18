mi_vec1  = c(1,5,27,5,187,34) 
mi_vec2  = c(45,848,24,7) 
mi_vec3  = c(87,8789,23) 
mi_vec4  = c(1,32) 
mi_vec5  = c(2022)
mi_vec6  = c(98,977,88,23,35,872) 
mi_vec7  = c(83,97,9000)

mi_funcion = function(vector){
  suma = sum(vector)
  maximo = max(vector)
  minimo = min(vector)
  return(suma-maximo-minimo)
}

lista_vectores = list(mi_vec1,
                      mi_vec2,
                      mi_vec3,
                      mi_vec4,
                      mi_vec5,
                      mi_vec6,
                      mi_vec7)

#### Función lapply(lista o vector,funcion) y devuelve una lista


L = lapply(lista_vectores, mi_funcion)  
names(L) = paste0("mi_vec",c(1:7))

L

########

mi_funcion_extendida = function(vector){
  suma = sum(vector)
  maximo = max(vector)
  minimo = min(vector)
  return(list(vector,suma-maximo-minimo))
}


mi_funcion_extendida(mi_vec1)

resultados_extendidos = lapply(lista_vectores,mi_funcion_extendida)
names(resultados_extendidos) = paste0("mi_vec",c(1:7))

resultados_extendidos


resultados_extendidos[[3]][1]


###############################


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

as.character(artistas_frecuentes$Var1)


###### Para cada artista anterior, quiero ver las tres canciones
###### con mayor duración ordenadas de mayor a menor. Quiero que 
###### se muestre también el álbum

extractor = function(artista){
  auxiliar = tabla_canciones[tabla_canciones$Artista == artista,]
  auxiliar = auxiliar[order(auxiliar$Duracion_minutos,decreasing = TRUE),] 
  auxiliar = auxiliar[1:3,c("Titulo","Album","Duracion")]
  auxiliar = rbind(NA,auxiliar,NA)
  auxiliar = cbind(NA,auxiliar)
  auxiliar[1,1] = artista
  return(auxiliar)
}

extractor("Queen")

auxiliar = lapply(as.character(artistas_frecuentes$Var1),extractor)

##### La poderosa combinación lapply, do.call y rbind

resultado = do.call(rbind,auxiliar) #

class(resultado)

# rbind(auxiliar[[1]],auxiliar[[2]],auxiliar[[3]],auxiliar[[4]])

write.csv(resultado,"tabla_final.csv")





