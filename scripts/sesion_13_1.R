data(mtcars)

# Agregramos la columna brand a mtcars
mtcars$brand  <- c("Mazda","Mazda","Datsun","Hornet","Hornet","Valiant","Duster","Merc",
                   "Merc","Merc","Merc","Merc","Merc","Merc","Cadillac","Lincoln",
                   "Chrysler","Fiat","Honda","Toyota","Toyota","Dodge","AMC","Camaro",
                   "Pontiac","Fiat","Porsche","Lotus","Ford","Ferrari","Maserati","Volvo")

mtcars$model  <- row.names(mtcars)

# creamos el dataframe de marcas
brands_origin <- data.frame(
  car_brand = c("Mazda","Toyota","Fiat","Volvo","Skoda"),
  country = c("Japan","Japan","Italy","Sweden","Checa"),
  type = c("A","B","C","D","E")
)

########
######## inner join
########

inner_join = merge(mtcars,brands_origin,   
      by.x="brand",by.y="car_brand")

View(inner_join)


########
######## left join
########

left_join = merge(mtcars,brands_origin,
      by.x = "brand", by.y = "car_brand",
      all.x = TRUE
)

View(left_join)

########
######## right join
########

right_join = merge(mtcars,brands_origin,
      by.x = "brand", by.y = "car_brand",
      all.y = TRUE
)

#merge(brands_origin,mtcars,
#                   by.x = "brand", by.y = "car_brand",
#                   all.x = TRUE
#)

View(right_join)

########
######## full join
########

full_join = merge(mtcars,brands_origin,
      by.x = "brand", by.y = "car_brand",
      all= TRUE
)

View(full_join)