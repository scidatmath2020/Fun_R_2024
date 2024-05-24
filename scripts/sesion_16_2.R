# Encontrar el primer número cuyo cuadrado exceda un número dado
## Esto significa resolver x^2>n para x entero positivo

numero_dado  = 2024
numero  = 0
while(numero^2 <= numero_dado){
  numero = numero+1
}

numero

45^2

# Encontrar todos los enteros positivos que cumplan n < x^2 < m

cota_inf = 2024
cota_sup = 3000


numero = floor(sqrt(2024))
resultados = c()

while(numero<sqrt(cota_sup)){
  numero = numero + 1
  resultados = c(resultados,numero)
  }
numero

resultados

##################################


# Dado un vector numérico, sustituir cada uno de sus elementos por su duplicado 
# hasta que encuentre un múltiplo de 5.
# No debe duplicar a este múltiplo de 5 ni a todos los demás elementos
(mi_vector  <- sample(1:100,20))

indice = 1

while(mi_vector[indice] %% 5 != 0){
  mi_vector[indice] = 2*mi_vector[indice]
  indice = indice + 1
}


mi_vector


#########
# Dado un string, guardar en una lista el recorrido que se hace desde el inicio
# hasta cada letra junto con el número de caracteres recorridos.
# Por ejemplo: "hola" debe dar como salida una lista con los elementos 
# "h", "ho","hol", "hola"

###### Un for SIEMPRE se puede escribir como un while

indice = 1
respuesta = list()
texto = "En un lugar de la Mancha"
n = nchar(texto)

#for(indice in 1:n){
#  respuesta[[indice]] = substr(texto,1,indice)
#}

while(indice <= n){
  respuesta[[indice]] = c(substr(texto,1,indice),indice)
  indice = indice + 1
}

respuesta

###########
########### uso de break
###########

# Dados un vector numérico X y un número fijo N, obtener la suma de 
# los elementos de X mientras sea menor que N

X  <- c(1,2,23,16,76,16)
N  = 50
indice = 1
suma = 0

while(TRUE){
  if(suma + X[indice] < N){
    suma = suma + X[indice]
    indice = indice + 1
  } else{
    break
  }
}

suma







