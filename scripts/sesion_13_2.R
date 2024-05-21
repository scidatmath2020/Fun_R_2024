#######
#######  if simple
#######

mi_animal <- readline(prompt="¿Qué animal eres?: ")

mi_animal

if(mi_animal == "gato"){
  print("Este animal hace 'miau'")    
  names(brands_origin)[1] = "marca" 
}

brands_origin

#######
#######  if-else
#######

mi_guerrero <- readline(prompt="Nombre del guerrero: ")

mi_guerrero

if(tolower(mi_guerrero) == "yamcha" | mi_guerrero == "Krilin"){
  print("Seguro se va a morir")
  
} else{
  print("Tenemos oportunidad de que nos salve")
}

print("Así pasa en DBZ")

#######
#######  if-elif-else
#######


mi_guerrero <- readline(prompt="Nombre del guerrero: ")

if(mi_guerrero == "Yamcha" | mi_guerrero == "Krilin"){
  print("Es un humano")
} else if (mi_guerrero == "Gokú" | mi_guerrero == "Vegueta"){
  print("Es un saijajín")
} else if(mi_guerrero == "18" | mi_guerrero == "17"){
  print("Es un androide")
} else if(mi_guerrero == "Pikoro"){
  print("Es un namekuseí")
} else{
  print("No es un personaje importante")
}
print("Esas son razas de DBZ")

#######
#######  Condicionales anidados
#######

dato_texto = readline(prompt="ingrese un número: ")
dato  <- as.numeric(dato_texto)
dato

if(is.na(dato) == FALSE){
  print('Es un número.')
  if(dato < 0){
    print('Es negativo.')
  } else if(dato > 0){
    print('Es positivo.')
  } else{
    print('Es cero')
  }
} else{
  print('No es un número.')
}



dato_texto = readline(prompt="ingrese un número: ")
dato  <- as.numeric(dato_texto)

if(is.na(dato) == FALSE & dato<0){
  print("Es un número")
  print("Es un negativo")
} else if(is.na(dato) == FALSE & dato>0){
  print("Es un número")
  print("Es un positivo")
} else if(is.na(dato) == FALSE & dato==0){
  print("Es un número")
  print("Es cero")
} else{
  print("No es un número")
}


#######
#######  Funciones con if
#######

#### factorial(n) = 1*2*3*...*n
factorial(6)

primalidad  <- function(n){
  if(n>1 & (factorial(n-1) %% n) == n-1){
    return(TRUE)
  } else{
    return(FALSE)
  }   
}


primalidad(17)

#######
#######  if vectorizado
#######

mi_vector  <- c(1,5,27,5,187,34,45,848,24,7)
mean(mi_vector)

ifelse(mi_vector < mean(mi_vector),"Es menor que la media","No es menor")

alumnos = data.frame(nombre = c("Ana","Luis","José","Sol"),
                     sexo = c(0,1,1,0))

alumnos

alumnos$sufijo = ifelse(alumnos$sexo == 1,"o","a")

alumnos


