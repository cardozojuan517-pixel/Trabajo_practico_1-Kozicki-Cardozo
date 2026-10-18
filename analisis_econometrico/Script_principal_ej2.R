set.seed(1234)

library(tidyverse)

gapminder <- read_csv("data/gapminder.csv",
  na = "")

View(gapminder)

#tomamos funciones de nuestro script

source("Funciones_ej2.R")

# ################################ #
# Exploración inicial de los datos #
# ################################ #

# Vemos los nombres de las variables

names(gapminder)

# Revisamos la estructura del dataframe

str(gapminder)
#------------------------------
#PARTE 1- Ingreso por persona#
#------------------------------

#Inciso 1- evolución income per person en Argentina

#filtramos datos de Argentina 

argentina <- gapminder %>%
  filter(country == "Argentina") %>%
  select(country, year, income_per_person)

View(argentina)

# Revisamos un resumen de las variables numéricas

summary(argentina)

# chequeamos si existen valores faltantes en income_per_person

sum(is.na(argentina$income_per_person))

# finalmente graficamos la evolución temporal del ingreso por persona

ggplot(argentina, aes(x = year, y = income_per_person)) +
  geom_line() +
  labs(title = "Evolución del ingreso por persona en Argentina",
    x = "Año",
    y = "Ingreso por persona") +
  theme_minimal()

#agregamos una regresion lineal pára hacer más evidente la tendencia creciente 

ggplot(argentina, aes(x = year, y = income_per_person)) +
  geom_line() +
  labs(title = "Evolución del ingreso por persona en Argentina",
       x = "Año",
       y = "Ingreso por persona") +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE)

#Inciso 2- modelar sacando los ultimos 10 años 

#Nos quedamos solo con los últios 10 años de del income per capita de argentina

argentina10 <- argentina %>% 
  filter(year >= max(argentina$year)- 9)
 
#Utilizamos la parte restante de argentina como datos para la estimación 

train <- argentina %>%
  filter(year < min(argentina10$year))

#MODELO LINEAL#
  
#Realizamos un modelo lineal con los datos de la base reducida, donde la variable dependiente es income per person y la independiente es year 

modelolineal <- lm(income_per_person ~ year, data = train)

#vemos los resultados de la estimación 

summary(modelolineal)

#generamos prediccciones para train usando el modelo y contrastamos con los datos reales 

modelolineal_train <- predict(modelolineal, newdata = train)

# graficamos la predicción de train (1960-2000) junto a los datos reales

ggplot(train, aes(x = year)) +
  geom_line(aes(y = income_per_person)) +
  geom_line(aes (y = modelolineal_train, color = "pink")) +
  labs(title = "Modelo lineal - Ingreso por persona en Argentina",
    x = "Año",
    y = "Ingreso por persona") +
  theme_classic()

 #MODELO POLINÓMICO DE GRADO 2#
  
#Estimamos la regresión de segundo grado con la base de datos train
  
modelopoli2 <-lm(income_per_person ~ year + I(year^2), data = train)

#vemos resultados de la estimación

summary(modelopoli2)

#hacemos predicciones con el modelo para luego comparar con los valores reales 

modelopoli2_train <- predict(modelopoli2, newdata = train)

#graficamos contrastando las predicciones para los años correspondientes a train(1960-2000) con los valores de los datos 

ggplot(train, aes(x = year)) +
  geom_line(aes(y = income_per_person)) +
  geom_line(aes(y = modelopoli2_train), color = "blue") +
  labs(
    title = "Modelo cuadrático - Ingreso por persona en Argentina",
    x = "Año",
    y = "Ingreso por persona"
  ) +
  theme_classic()


#MODELO POLINÓMICO DE GRADO 10#

#Estimamos el modelo polinómico de grado 10 con train como base de datos

modelopoli10 <-lm(income_per_person ~ poly(year,10), 
                  data = train)

#observamos los resultados del mismo

summary(modelopoli10)

#Hacemos estimaciones para los años de train con el modelo 

modelopoli10_train <- predict(modelopoli10, newdata = train)

#Graficamos el contraste predicción datos reales. 

ggplot(train, aes(x = year)) +
  geom_line(aes(y = income_per_person)) +
  geom_line(aes(y = modelopoli10_train), color = "red") +
  labs(
    title = "Modelo polinómico de grado 10 - Ingreso por persona en Argentina",
    x = "Año",
    y = "Ingreso por persona"
  ) +
  theme_classic()

############
  #Ahora usamos los tres modelos para predecir los 10 años que dejamos fuera
 
# renombramos argentina10 como test para que sea más claro 
test <- argentina10

# Predecimos los últimos 10 años con el modelo lineal
predmodelolineal_test <- predict(modelolineal, newdata = test)

# Predecimos los últimos 10 años con el modelo cuadrático
predmodelopoli2_test <- predict(modelopoli2, newdata = test)

# Predecimos los últimos 10 años con el modelo polinómico de grado 10
predmodelopoli10_test <- predict(modelopoli10, newdata = test)

# ------------------------------------------------------------
# Comparación de predicciones sobre el conjunto de test
# ------------------------------------------------------------

#Graficamos datos reales y predicciones de los distintos modelos juntas.
ggplot(test, aes(x = year)) +
  geom_line(aes(y = income_per_person)) +
  geom_line(aes(y = predmodelolineal_test), color = "red") +
  geom_line(aes(y = predmodelopoli2_test), color = "blue") +
  geom_line(aes(y = predmodelopoli10_test), color = "green") +
  labs(title = "Predicciones sobre los últimos 10 años",
    x = "Año",
    y = "Ingreso por persona")

#Lo volvemos a graficar haciendo "zoom" en el rango de valore donde estan más cerca las estimaciones

ggplot(test, aes(x = year)) +
  geom_line(aes(y = income_per_person)) +
  geom_line(aes(y = predmodelolineal_test), color = "red") +
  geom_line(aes(y = predmodelopoli2_test), color = "blue") +
  geom_line(aes(y = predmodelopoli10_test), color = "green") +
  labs(title = "Predicciones sobre los últimos 10 años",
       x = "Año",
       y = "Ingreso por persona"
  ) +
  theme_classic()+ coord_cartesian(ylim = c(10000, 20000))


# Calculamos el RMSE de cada modelo sobre el conjunto de test

rmselineal <- rmse(test$income_per_person, predmodelolineal_test)

rmsepoli2 <- rmse(test$income_per_person, predmodelopoli2_test)

rmsepoli10 <- rmse(test$income_per_person, predmodelopoli10_test)

# vemos los resultrados

rmselineal
rmsepoli2
rmsepoli10

# Armamos un dataframe con los RMSE de los tres modelos
rmse_modelos <- data.frame(
  modelo = c("Lineal", "Grado 2", "Grado 10"),
  rmse = c(rmselineal, rmsepoli2, rmsepoli10)
)

# Graficamos el RMSE de cada modelo
ggplot(rmse_modelos, aes(x = modelo, y = rmse, fill = modelo)) +
  geom_bar(stat = "identity") +
  labs(
    title = "RMSE de los modelos sobre el conjunto de test",
    x = "Modelo",
    y = "RMSE"
  ) +
  theme_minimal()

#Inciso 3- Correlaciones 5 países

#Armamos un vector con los 4 países sudamericanos seleccionados y Argentina
 
paises <-c("Bolivia","Brazil","Paraguay","Peru", "Argentina")

#filtramos de gapminder el income per capita y año de cada uno de los países usando el vector

income5países <- gapminder %>%
  filter(country %in% paises) %>%
  select(country, year,income_per_person)

#chequeamos que hayan quedado los 5 países

unique(income5países$country)

#a) armamos matriz de correlaciones entre los ingresos
#para eso queremos "trasponer" los datos para que cada país quede como columna

paisescolumna <- income5países %>%
  select(country, year, income_per_person) %>%
  pivot_wider(names_from = country, values_from = income_per_person)

#chequeamos

View(paisescolumna)

#sacamos la columna de años que no aporta a la correlación entre países y
#sacamos la correlación haciendo que solo use las filas con datos completos (todas)

corincome <- paisescolumna %>%
  select(-year) %>% cor(use = "complete.obs")

#Observamos la matriz de correlaciones

corincome

#b)matriz de correlaciones entre variaciones porcentuales anuales

#retomamos income5países
#calculamos la variación porcentual anual del ingreso por país y 
#lo agregamos a otra columna

crecimientointeranual <- income5países %>%
  group_by(country) %>%
  mutate(variación_porcentual_anual =
  (income_per_person - lag (income_per_person))/lag(income_per_person)) %>%
            ungroup()
         
View(crecimientointeranual)

#nuevamente cambiamos de formato para luego armar la matriz, dejando fuera de income  per person
#y armamos la matriz sacando la columna de años y sacando los NA

crecimientocolumna <- crecimientointeranual %>%
  select(country, year, variación_porcentual_anual) %>%
  pivot_wider(names_from = country,
              values_from = variación_porcentual_anual)

corcrecimiento <- crecimientocolumna %>%
  select(-year) %>%
  cor(use = "complete.obs")

#miramos la matriz
View(corcrecimiento)

#-----------------------------------
#PARTE 2- Esperanza de vida y género  
#-----------------------------------

#Inciso 5- Life expectancy vs life expectancy female

#Filtro el año  (1965)

lifeexp1965 <- gapminder %>%
  filter(year == 1965) %>%
  select (life_expectancy, life_expectancy_female, year, country)

#chequeamos si hay datos NA, en las columnas que queremos graficar

sum(is.na(lifeexp1965$life_expectancy))
sum(is.na(lifeexp1965$life_expectancy_female))

View(lifeexp1965)

#Encontramos datos faltamtes en life expectancy por lo que vamos a eliminar los países que no tengan datos

lifeexp1965clean <- lifeexp1965 %>%
  filter(!is.na(life_expectancy) & 
           !is.na(life_expectancy_female))

#Chequeamos que pudimos limpiar la base y ya no hay NA

sum(is.na(lifeexp1965clean$life_expectancy))
sum(is.na(lifeexp1965clean$life_expectancy_female))

#graficamos

ggplot(lifeexp1965clean, aes(x = life_expectancy_female, y = life_expectancy)) + 
  geom_point() + 
  labs(title = "Esperanza de vida y Esperanza de vida femenina", 
       x = "Esperanza de vida mujeres",
       y= "Esperamza de vida") +
  theme_classic()
  
#convertimos la columna de esperanza de vida de las mujeres en una numérica

lifeexp1965clean <- lifeexp1965clean %>%
  mutate(life_expectancy_female = as.numeric(life_expectancy_female))

#Ahora si volvemos a correr el gráfico

ggplot(lifeexp1965clean, aes(x = life_expectancy_female, y = life_expectancy)) + 
  geom_point() + 
  labs(title = "Esperanza de vida y Esperanza de vida femenina", 
       x = "Esperanza de vida mujeres",
       y= "Esperamza de vida") +
  theme_classic()

#Inciso 6- REGRESION LINEAL 

#Modelamos la regresión lineal 

lifemodelolineal <- lm(life_expectancy ~ life_expectancy_female, data = lifeexp1965clean)

#Vemos los resultados

summary(lifemodelolineal)

#Inciso 7- CONTRASTE DE LA HIPÓTESIS NULA 

# H0: life_expectancy_female = life_expectancy
# H1: life_expectancy_female > life_expectancy

Test_Exp_Vida <- t.test(lifeexp1965clean$life_expectancy_female,
  lifeexp1965clean$life_expectancy,
  paired = TRUE,
  alternative = "greater")

Test_Exp_Vida

#Inciso 8- REGRESION MULTIPLE

#Armamos una nueva base que tenga income per person
#sacamos los NA
#y convertimos la variable de life expectancy female en numérica porque estaba en texto


Rmultipledata <- gapminder %>%
filter(year == 1965) %>%
  select(country, life_expectancy,life_expectancy_female,income_per_person) %>%
  filter(!is.na(life_expectancy) & 
           !is.na(life_expectancy_female) & !is.na(income_per_person)) %>%
  mutate(life_expectancy_female = as.numeric(life_expectancy_female))


#Armamos la regresión

Rmultiple <- lm( life_expectancy ~ income_per_person 
                              + life_expectancy_female, data = Rmultipledata)

summary(Rmultiple)

#Inciso 9- modelo lineal múltiple 

#Armamos una base con las variables que queremos usar para explicar la expectativa de vida
# también incluimos los datos de life_expectancy y country
#sumamos una columna con el logaritmo del ingreso y eliminamos los NA

datosmlm <- gapminder %>%
  filter(year == 1965) %>%
  mutate(log_income = log(income_per_person)) %>%
  select(life_expectancy, country, log_income, children_per_woman, child_mortality)%>%
  filter(!is.na(life_expectancy) &
            !is.na(log_income) &
            !is.na(children_per_woman) &
            !is.na(child_mortality))

#Vemos como quedo la nueva base

View(datosmlm)

#Armamos una matriz de cor para ver como es la relacion entre las posibles variables explicativas del modelo
# y la var dependiente

correlaciones <- datosmlm %>%
  select(life_expectancy,
    log_income,
    children_per_woman,
    child_mortality) %>%
  cor()

correlaciones

# Primero usamos solamente mortalidad infantil que es la que presenta una mayor cor

modelo1 <- lm(life_expectancy ~ child_mortality,
  data = datosmlm)

summary(modelo1)

# Incorporamos como segunda variable explicativa al logaritmo del ingreso

modelo2 <- lm(life_expectancy ~ child_mortality + log_income,
  data = datosmlm
)

summary(modelo2)

# Finalmente agregamos children_per_woman

modelo3 <- lm(life_expectancy ~ child_mortality + log_income + children_per_woman,
  data = datosmlm)

summary(modelo3)