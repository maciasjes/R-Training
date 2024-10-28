##Install packages

install.packages('tidyverse')
library (tidyverse)
library (readxl)
library (dplyr)
library(ggplot2)


##Datos
url <- "https://github.com/TRACE-LAC/TRACE-LAC-data/blob/main/otros/muestra_covid.RDS?raw=true"

covid19 <- readr::read_rds(url)

glimpse(covid19)

#crear nuevos datos
covid19_resumen <- covid19 %>% 
  group_by(fecha_reporte_web,sexo) %>%
  summarize(casos = n())

#crear grafico dispersion
ggplot(data = covid19_resumen, aes(x = fecha_reporte_web, y = casos, colour = sexo)) + 
  geom_point()

#crear grafico lineas
covid19_fecha <- covid19 %>%
  group_by(fecha_reporte_web) %>%
  summarise(casos = n())

ggplot(data = covid19_fecha, aes(x = fecha_reporte_web, y = casos)) +
  geom_line()

#crear grafico de barras
ggplot(data = covid19) +
  geom_bar(aes(x = sexo))

#crear grafico de barras2
covid19_ubicacion <- covid19 %>%
  group_by(ubicacion_del_caso) %>%
  summarise(casos = n())

ggplot(data = covid19_ubicacion, aes(x = reorder(ubicacion_del_caso, +casos), y = casos)) + 
  geom_bar(stat = "identity") + coord_flip()