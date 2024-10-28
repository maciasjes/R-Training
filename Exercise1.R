
##Install packages

install.packages('tidyverse')
library (tidyverse)
library (readxl)
library (dplyr)


##Creación de vectores

nombre <- c("Emilia", "Maximo", "Axel", "Diana") # Nombre de las personas
ciudad <- c("La Plata","Concepción", "Cuzco", "Bogota") # Ciudad de residencia
edad <- c(18, 20, 37, 42)  # Edad de las personas
vacunado <- c(TRUE, FALSE, FALSE, TRUE) # Estado de vacunación
dosis <- c(2L, 0L, 1L, 2L) # Número de dosis recibidas`

##Creación de tablas

datos_vacunas <- data.frame(
  nombre = nombre, 
  ciudad = ciudad,
  edad = edad,
  vacunado = vacunado,
  dosis = dosis)

datos_vacunas

str(datos_vacunas)
str(datos_vacunas$nombre)

#[filas,columnas]

datos_vacunas [2,5]

##Creación de funciones

IMC <- function(peso_kg, talla_m){
  resultado <- peso_kg/talla_m^2
  return(resultado)
}

IMC(peso_kg = 80, talla_m = 1.75)

##Tydyverse
#Pipelines
library(dplyr) # Esta librería se carga automáticamente con tidyverserse 
datos <- data.frame(edad = c(12, 23, 32, 60), dosis = c(1, 2, 3, 1))

#Sin pipe
datos_filtrados <- filter(datos, edad > 18) 
datos_con_esquema <- mutate(datos_filtrados, 
                            esquema = ifelse(dosis > 2, "Completo", "Incompleto")) 
datos_agrupados <- group_by(datos_con_esquema, esquema)
datos_por_esquema <- summarise(datos_agrupados, personas = n())

#Con pipe
datos_por_esquema <- 
  filter(datos, edad > 18) %>% 
  mutate( esquema = ifelse(dosis > 2, "Completo", "Incompleto")) %>%
  group_by(esquema) %>% 
  summarise(personas = n())

#Get data

library(readxl) 
dat <- read_excel("Datos/datos_covid.xlsx")

#Funciones básicas de tydyverse
dat %>% glimpse()
dat %>% summarise(media = mean(edad), casos= n())
dat %>% group_by(sexo) %>% 
  summarise(casos = n(), media_edad = mean(edad))
dat %>% select(edad, sexo) 
dat %>% filter(edad < 28)
dat %>% filter(sexo == "F", edad <= 28) #Ahora sabe como filtrar el sexo
dat %>% arrange(edad) 
dat %>% arrange(edad,sexo)
dat %>% arrange(desc(edad))
dat %>% mutate(SEXO= toupper(sexo)) %>% select(SEXO)
dat %>% rename(edad_años = edad)
dat %>% slice(10:15)



##Practica
url <- "https://github.com/TRACE-LAC/TRACE-LAC-data/raw/refs/heads/main/otros/datos_limpios_covid.RDS"

covid19 <- readr::read_rds(url)

##hello
dat <- read_excel("Datos/datos_covid.xlsx")

#Funciones básicas de tydyverse
dat %>% glimpse()

#Cargar datos
url <- "https://github.com/TRACE-LAC/TRACE-LAC-data/raw/refs/heads/main/otros/datos_limpios_covid.RDS"

covid19 <- readr::read_rds(url)

#Explorar los datos
glimpse (covid19)

#Agrupar los datos por nombre_del_pais y cuenta los casos por cada uno
covid19 %>% group_by(nombre_del_pais) %>% 
  summarise(registros = n())

#Filtra los datos para Perú 
covidperu <- covid19 %>% filter(nombre_del_pais == "Perú") %>%
  glimpse()

#Agrupa los datos por sexo y cuenta los casos por cada uno
covid19 %>% group_by(sexo) %>% 
  summarise(registros = n())

#Agrupa los datos por nombre_del_pais y calcula la media de edad de cada uno
covid19 %>% group_by(nombre_del_pais) %>% 
  summarise(media_edad = mean(edad))

#Cambia el nombre de “estado” por “gravedad”
covid19 %>% rename(gravedad = estado) %>% 
  glimpse ()

#Ordena los datos por sexo y selecciona las 5 primeras filas de cada uno
covid19 %>% arrange(sexo) %>%
  group_by(sexo) %>%
  slice (1:5)

#Realiza una tabla que muestre cuántas personas de cada sintoma aparecen en la base ubicados en Colombia
covid19 %>% group_by(sintomas) %>%
  filter(nombre_del_pais == "Colombia") %>%
  summarise(casos = n())

#Selecciona las 5 primeras filas de solo el número de identificación del caso (id_de_caso)
covid19 %>% select(id_de_caso) %>%
  slice(1:5)

#¿Cuál es el país con mayor número de fallecidos de acuerdo con la variable estado?
covid19 %>% filter(estado == "Fallecido") %>%
  group_by(nombre_del_pais) %>%
  summarise(casos = n())

#¿Cuál es el país que presenta la mayor edad media en sus registros?
covid19 %>% group_by(nombre_del_pais) %>%
  summarise(media_edad = mean(edad)) %>%
  arrange(desc(media_edad))