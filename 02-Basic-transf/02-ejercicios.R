
## Respuestas a los ejercicios de la clase 2

library(tidyr)
library(dplyr)
library(ggplot2)

# 1. Leemos los datos
tb <- tbl_df(read.csv("data/tb.csv", stringsAsFactors = FALSE))

# 1.1 Limpiar: utiliza la función gather para apilar las columnas 
# correspondientes a sexo-edad.
tb_2 <- gather(tb, demo, n, -iso2, -year, na.rm = TRUE)
tb_2


# 2.Leemos los datos
flights <- tbl_df(read.csv("flights.csv", stringsAsFactors = FALSE))
flights$date <- as.Date(flights$date)

weather <- tbl_df(read.csv("weather.csv", stringsAsFactors = FALSE))
weather$date <- as.Date(weather$date)

planes <- tbl_df(read.csv("planes.csv", stringsAsFactors = FALSE))

airports <- tbl_df(read.csv("airports.csv", stringsAsFactors = FALSE))

# 2.1 Filtrar: encuentra todos los vuelos hacia SFO ó OAK
filter(flights, dest %in% c("SFO", "OAK"))
filter(flights, dest == "SFO" | dest == "OAK")

# 2.2 Los vuelos con un retraso mayor a una hora.
filter(flights, arr_delay > 60) # no se especifica si es de llegada
filter(flights, dep_delay > 60) # o salida

# 2.3 En los que el retraso de llegada es más del doble que el retraso de salida
filter(flights, arr_delay > 2 * dep_delay, dep_delay > 0) 

# 3.1 Seleccionar: Escribe tres maneras de seleccionar las variables de retraso
select(flights, dep_delay, arr_delay)
select(flights, ends_with("delay"))
select(flights, contains("delay"))
select(flights, matches("delay"))
select(flights, starts_with("arr_d"), starts_with("dep_d"))
select(flights, arr_delay:dep_delay)

# 4.1 Arreglar: Ordena los vuelos por fecha y hora de salida
arrange(flights, date, hour, minute)

# 4.2 ¿Cuáles son los vuelos con mayor retraso?
arrange(flights, -dep_delay)
arrange(flights, desc(dep_delay))

# 4.3 ¿Qué vuelos ganaron más tiempo en el aire?
arrange(flights, arr_delay-dep_delay)

# 5.1 Mutar: Calcula la velocidad en millas por hora a partir de la variable 
# tiempo y la distancia (en millas). ¿Quá vuelo fue el más rápido?
flights_2 <- mutate(flights, speed = dist / time * 60)
arrange(flights_2, desc(speed))

# 5.2 Crea una nueva variable que muestre cuánto tiempo se ganó o perdió durante 
# el vuelo
mutate(flights, speed = dist / time * 60, time_diff = arr_delay - dep_delay)

# 6.1 Summarise: Calcula el retraso promedio por fecha
by_date <- group_by(flights, date)
summarise(by_date,  mean_delay = mean(dep_delay, na.rm = TRUE))
no_miss <- filter(by_date, !is.na(dep_delay))
summarise(no_miss, mean_delay = mean(dep_delay))

# 6.2 ¿Qué otros resúmenes puedes hacer para explorar el retraso por fecha?
delays <- summarise(no_miss,
  mean = mean(dep_delay),         # media
  median = median(dep_delay),     # mediana
  q75 = quantile(dep_delay, 0.75),# cuantil 0.75
  over_15 = mean(dep_delay > 15), # retraso promedio de auquellos vuelos con retraso mayor a 15 min en cada fecha
  over_30 = mean(dep_delay > 30),
  over_60 = mean(dep_delay > 60), 
  n = n()                         # número vuelos en esa fecha
)

# 7.1 Pipes: ¿Qué destinos tienen el promedio de retrasos más alto?
flights %>%                      # base de datos
  group_by(dest) %>%             # agrupar por destino
  summarise(                     # calcular retraso promedio (por destino)
    arr_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>%                 # calcular número de vuelos por destino
  arrange(desc(arr_delay))       # ordenar de acuerdo a la variable arr_delay (descendiente)

# 7.2 ¿Qué vuelos (compañía + vuelo) ocurren diario?
flights %>%
  group_by(carrier, flight, dest) %>% # agrupamos por compañía, vuelo y destino (este último solo es para asegurar)
  summarise(n = n()) %>% 
  filter(n == 365)

# En promedio, ¿Cómo varían a lo largo del día los retrasos de vuelos no cancelados?
