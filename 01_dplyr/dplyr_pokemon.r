#----Instalar librería----
# install.packages("tidyverse")

#----Llamar librería----
library(tidyverse)

# Importar datos
pokemon <- read_csv("../00_datasets/pokemon.csv")

# Tipo de objeto que es pokemon
class(pokemon)

# Conocer las primeras filas 
head(pokemon)

#----Exploración inicial de los datos----
# Conocer las dimensiones del dataset
dim(pokemon)

# Conocerlos nombres de las columnas
colnames(pokemon)

# Explorar una columna en específico 
# pokemon$type1

# Explorar solo algunos elementos 
head(pokemon$type1, 30)


head(pokemon$classfication, 20)

#----select()----
# select() pues... para seleccionar que no? 

# Solo queremos algunas cuantas columnas no las 41

columnas <- c('abilities', 'name', 'type1', 'classfication', 'is_legendary')
# select()
select(pokemon, all_of(columnas))

# Seleccionar por rango de columnas 
select(pokemon, 10:15) 

# Selecciona de manera invertida
select(pokemon, 20:1)

# Columna 1
pokemon[1]

# Hacer select a la antigua
pokemon[1:10,columnas] 

# Seleccionar utilizando un patron de caracteres: 
# opcion1
select(pokemon, contains("against"))

# Opcion2
select(pokemon, matches("against")) 

# Seleccionar por tipo de dato 
select(pokemon, where(is.numeric)) 

#----filter()----
# Hacer subconjuntos de datos con filter() 
# Utiliza operadores relacionales, logicos y de pertenecia para trabajar, ya que va a filtrar en funcion del valor que haya en una columna.
filter(pokemon, type1 == "fire")

# Todo lo que sea diferente a determinado valor
filter(pokemon, type1 != "fire") 

# Cuantas categorias hay en type1
dplyr::count(pokemon, type1) 

# Escoger aquellos pokemones que sean de roca, agua, pasto y fuego 
# Opción 1. Uso de operadores 

# ¿Cuál es la diferencia entre AND y OR? ¿Deberia usar AND u OR?

filter(pokemon, type1 == "rock" & type1 == "water" & type1 == "grass" & type1 == "fire")

filter(pokemon, type1 == "rock" | type1 == "water" | type1 == "grass" | type1 == "fire") %>%
    head()

tipos_pokemones <- c("rock", "water", "grass", "fire")
filter(pokemon, type1 %in% tipos_pokemones) 

# Filtrar una variable numérica
# Conocer el valor mínimo y máximo de una variable numérica
summary(pokemon$weight_kg) 

# Ahora si a hacer el filtrado
# ¿Usamos OR o AND?
filter(pokemon, weight_kg > 100 | weight_kg < 300 ) 

# Saber qué pokemones son diferentes
filter(pokemon, weight_kg > 100 | weight_kg < 300 ) -> confusion

# La columna del nombre de los pokemones es única
length(unique(pokemon$name))

# Conocer las dimensiones del dataframe que nos causa confusion
dim(confusion)

# Convertir a vector los nombres de los pokemones que estan en confusion 
nombres_confusion <- confusion$name

# Filtrar por esos nombres 
los_excluidos <- filter(pokemon, !name %in% nombres_confusion)

filter(pokemon, weight_kg > 100 & weight_kg < 300 )

los_excluidos$weight_kg

filter(pokemon, weight_kg >= 100 & weight_kg <= 300 ) 

# usar between, toma en cuenta los rangos
filter(pokemon, between(weight_kg, 100, 300)) 

# Ver de nuevo los nombres de columnas
colnames(pokemon)

# Hacer un vector con las columnas que se solicitan 
columnas2 <- c("name", 'type1','classfication','abilities', 'weight_kg', 'is_legendary')
tipos <- c('water', 'ice', 'fire', 'electric')
pokemon2 <- pokemon %>%
    select(all_of(columnas2)) %>%
    filter(type1 %in% tipos)

head(pokemon2)

#----arrange()----
# Ordena los nombres de los pokemones en orden alfabético
arrange(pokemon2, name) 

# Ordenar los nombres en orden alfábetico pero el peso de mayor a menor 
arrange(pokemon2, name, desc(weight_kg)) 

#----mutate()----
# Agrega nuevas variables y preserva las existentes.
# Sumar el total de las variables `against`.
# Opcion 1
pokemon %>% 
  select(name, contains("against")) %>% 
  mutate(total = rowSums(select(., -name))) %>% 
  select(name, total)

# opcion2
pokemon %>% 
  select(name, contains("against")) %>% 
  mutate(total = reduce(select(., -name), `+`)) 

#----group_by() y summarise()----
pokemon2 %>% 
    group_by(type1) %>%
    summarise(across(weight_kg, .fns = list(media = mean))) 

# Comprobar que hay NAs
filter(pokemon2, type1 == "electric") %>%
    select(weight_kg) 

pokemon2 %>% 
    group_by(type1) %>%
    summarise(across(weight_kg, .fns = list(media = mean), na.rm = T))
