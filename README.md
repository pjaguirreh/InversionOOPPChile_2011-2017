
Análisis Inversión Obras Públicas CHile
================

En este documento realizaremos un análisis de las inversiones realizadas por el *Ministerio de Obras Públicas de Chile* en el periodo 2011-2017.

Primero cargamos las librerías que se utilizarán en este ejercicio.

``` r
library(dplyr) # manejo de datos
library(ggplot2) # visualización
library(ggridges) # más opciones de visualización
library(stringr) # manejo de variables "character"
library(forcats) # manejo de variables "factor"
library(readxl) # leer Excel
library(httr) # Leer info de internet
library(tidytext) # Análisis de texto
library(stopwords) # Complementaria a análisis de texto
library(kableExtra) # tablas
```

La informacióna utilizar está disponible en la web de datos abiertos del gobierno de Chile (<http://datos.gob.cl>) por lo que podemos acceder a esta a través de una URL. Luego de descargar los datos desde la web procedemos a cargar estos a nuestra sesión de R.

``` r
url <- "http://datos.gob.cl/dataset/104d1ebf-4d1b-4c3d-af9e-e85e5bbf1fc9/resource/3fe6aa75-b611-48bb-ae94-abf745bc0553/download/detalleinversionhistoricamop2011-2019.xlsx"
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
```

    ## Response [http://datos.gob.cl/dataset/104d1ebf-4d1b-4c3d-af9e-e85e5bbf1fc9/resource/3fe6aa75-b611-48bb-ae94-abf745bc0553/download/detalleinversionhistoricamop2011-2019.xlsx]
    ##   Date: 2019-10-16 14:37
    ##   Status: 200
    ##   Content-Type: application/vnd.openxmlformats-officedocument.spreadsheetml.sheet
    ##   Size: 675 kB
    ## <ON DISK>  C:\Users\PABLO~1.AGU\AppData\Local\Temp\Rtmp6Vz1oo\file2387d9770c9.xlsx

``` r
datos <- read_excel(tf)
```

Utilizando `head()` y `str()` podemos tener una primera impresión sobre los datos que acabamos de cargar.

``` r
head(datos)
```

    ## # A tibble: 6 x 8
    ##     AÑO REGIÓN  SERVICIO  PROVINCIA  COMUNA BIP   NOMBRE   `INVERSIÓN (MIL~
    ##   <dbl> <chr>   <chr>     <chr>      <chr>  <chr> <chr>               <dbl>
    ## 1  2011 Arica ~ Direcció~ ARICA      ARICA  3008~ AMPLIAC~          1121062
    ## 2  2011 Arica ~ Direcció~ PARINACOTA PUTRE  3003~ CONSTRU~            23272
    ## 3  2011 Arica ~ Direcció~ ARICA      ARICA  3003~ CONSTRU~           770137
    ## 4  2011 Arica ~ Direcció~ ARICA      ARICA  3006~ CONSTRU~           498645
    ## 5  2011 Arica ~ Direcció~ ARICA      ARICA  3006~ CONSTRU~            25805
    ## 6  2011 Arica ~ Direcció~ ARICA      ARICA  3007~ CONSTRU~           233856

``` r
str(datos)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    9367 obs. of  8 variables:
    ##  $ AÑO                               : num  2011 2011 2011 2011 2011 ...
    ##  $ REGIÓN                            : chr  "Arica y Parinacota" "Arica y Parinacota" "Arica y Parinacota" "Arica y Parinacota" ...
    ##  $ SERVICIO                          : chr  "Dirección de Arquitectura" "Dirección de Obras Hidráulicas" "Dirección de Obras Hidráulicas" "Dirección de Obras Hidráulicas" ...
    ##  $ PROVINCIA                         : chr  "ARICA" "PARINACOTA" "ARICA" "ARICA" ...
    ##  $ COMUNA                            : chr  "ARICA" "PUTRE" "ARICA" "ARICA" ...
    ##  $ BIP                               : chr  "30088379-0" "30034648-0" "30034659-0" "30069191-0" ...
    ##  $ NOMBRE                            : chr  "AMPLIACION OFICINAS M.O.P. EDIFICIO SERVICIOS PUBLICOS ARICA, ARTURO PRAT 305, ARICA" "CONSTRUCCION EMBALSE LIVILCAR VALLE DE AZAPA, COMUNA DE ARICA" "CONSTRUCCION EMBALSE CHIRONTA VALLE DEL LLUTA" "CONSTRUCCION INFRAESTRUCTURA EN CAUCE URBANO RÍO SAN JOSÉ ARICA" ...
    ##  $ INVERSIÓN (MILES DE $ DE CADA AÑO): num  1121062 23272 770137 498645 25805 ...

Teniendo en cuenta las características de los datos haremos unas pequeñas modificaciones antes de avanzar:

-   Sacar la columna `BIP` que pareciera ser un identificador que no otorga mayor información
-   Cambiar los nombres de las variables para dejarlas en formato **sentence** (primer letra mayúscula y el resto minúscula) así como sacar caractéres especiales
-   Cambiar la variable `Inversion` de miles a millones de pesos
-   Modificar los valores de las columnas `Region`, `Provincia` y `Comuna` a formato **sentence**
-   Modificar los valores de la columna `Nombre` a minúscula
-   Modificar la columna `Region` de **character** a **factor**

Todos estos cambios serán almacenados en un objeto llamado `df`.

``` r
df <- datos %>%
  select(-BIP) %>%
  rename(
    Anio = AÑO,
    Region = REGIÓN,
    Servicio = SERVICIO,
    Provincia = PROVINCIA,
    Comuna = COMUNA,
    Nombre = NOMBRE,
    Inversion = `INVERSIÓN (MILES DE $ DE CADA AÑO)`
  ) %>% 
  mutate(Inversion = round(Inversion / 1000000, 1),
         Region = str_to_sentence(Region),
         Provincia = str_to_sentence(Provincia),
         Comuna = str_to_sentence(Comuna),
         Nombre = str_to_lower(Nombre),
         Region = as_factor(Region))
```

Teniendo listos nuestros datos, procedemos a hacer un primer análisis del gasto en Obras Públicas a través de los años.

``` r
df %>% 
  group_by(Anio) %>% 
  summarise(Inversion = sum(Inversion)) %>% 
  ggplot(aes(x = Anio, y = Inversion, label = Inversion)) +
  scale_x_continuous(breaks = c(2011:2017)) +
  geom_line(size = 1.2, col = "blue") +
  geom_label() +
  ylim(0, 1600) +
  labs(title = "Inversión en OOPP en el periodo 2011-2017 (MM CLP)", 
       subtitle = "Desglose por año",
       x = "",
       y= "") +
  theme_minimal()
```

![](OOPPChile_files/figure-markdown_github/Inversión%20por%20año-1.png)

Es posible observar que la inversión presenta una leve alza sostenida durante el periodo estudiado con un **peak** el año 2015 de casi 1.600 millones de pesos.

Sumado a lo anterior, podemos ver como se comporta la inversión de obras públicas en términos de las regiones del país.

``` r
df %>% 
  group_by(Region) %>% 
  summarise(Inversion = sum(Inversion)) %>% 
  ggplot(aes(x = reorder(Region, Inversion), y = Inversion, label = Inversion)) +
  geom_col(fill = "red") +
  coord_flip() +
  ylim(0, 1150) + 
  geom_text(hjust = -0.1, size = 3) +
  labs(title = "Inversión en OOPP en el periodo 2011-2017 (MM CLP)", 
       subtitle = "Desglose por región",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank())
```

![](OOPPChile_files/figure-markdown_github/Inversión%20por%20Región-1.png)

Como podría de esperarse, la gran parte de la inversión de concentra en la región metropolitana (donde se encuentra la capital) seguida por la región de BioBío donde se encuentra la ciudad de Concepción (segunda ciudad más grande de Chile).

``` r
df %>% 
  group_by(Region, Anio) %>% 
  summarise(Inversion = sum(Inversion)) %>% 
  ggplot(aes(x = Anio, y = Inversion, label = Inversion)) + 
  facet_wrap(~ Region, nrow = 4) +
  geom_line(col = "blue") +
  geom_point() +
  ylim(0, 300) + 
  geom_text(size = 2, vjust = -0.4) +
  labs(title = "Inversión en OOPP en el periodo 2011-2017 (MM CLP)", 
       subtitle = "Desglose por región y año",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())
```

![](OOPPChile_files/figure-markdown_github/Inversión%20por%20Año%20y%20Región-1.png)

Al considerar ambas dimensiones (tiempo y regiones) vemos que -en general- la inversión se mantiene constante durante los años en la mayor parte de las regiones. Destacan el caso de la región metropolitana donde más bien pareciera haber una disminución del gasto en el tiempo.

Teniendo una concepción general de como se comportó la inversión en Obras Públicas durante el periódo 2011-2017 procederemos a hacer un análisis de que tipo de proyectos son los que se han realizado. Para esto nos concentraremos en la columna `Nombre` que nos permitiría extraer un poco más de información sobre el tipo de inversión que se ha realizado.

Para realizar este análisis lo primero que haremos es **tokenizar** la información disponible en `Nombre`, esto corresponde a separar las frases o conjuntos de palabra de cada proyecto de inversión dejando una observación (fila) para cada una de las palabras de cada valor en `Nombre` (por ej. una fila con valor de `Nombre` **Ampliación de calle** pasa a ser tres filas correspondientes a **Ampliación**, **de**, y **calle**).

``` r
df_palabra <- df %>% 
  unnest_tokens(palabra, Nombre)

df_palabra %>% 
  count(palabra, sort = TRUE) %>% 
  top_n(10)
```

    ## # A tibble: 10 x 2
    ##    palabra          n
    ##    <chr>        <int>
    ##  1 de            4992
    ##  2 ruta          2726
    ##  3 conservacion  2194
    ##  4 mejoramiento  2184
    ##  5 y             1617
    ##  6 construccion  1543
    ##  7 region        1472
    ##  8 sector        1439
    ##  9 vial          1122
    ## 10 la             984

Al ver las 10 palabras más comunes nos encontramos con algunas que son informativas tales como **conservación** y **mejoramiento** mientas que otras no tanto **de** e **y**.

Entonces, lo que haremos es hacer un poco de limpieza de la nueva columna `palabra` consistente en: - Eliminar puntuaciones de las palabras - Eliminar palabras que contengan número - Eliminar **stopwords** (ej. a, de, y, etc utilizando la librería `stopwords`) - Reemplazar letras con tíldes - Sacar otras palabras identificadas al analizar resultados - Eliminar toda palabra que tenga 2 o menos letras

``` r
df_palabra <- df %>% 
  unnest_tokens(palabra, Nombre) %>% 
  mutate(palabra = str_replace(palabra, "[:punct:]", " "),
         palabra = str_replace(palabra, "\\w*[0-9]+\\w*\\s*", " "),
         palabra = stringi::stri_trans_general(palabra, "Latin-ASCII"),
         nchar = nchar(palabra)) %>%
  filter(!is.na(palabra) & 
         !palabra %in% c("y", "yy", "i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix", "x", "xi", "xii", "xiv", "", " ", "apr", "ano", "anos", "san"),
         !palabra %in% stopwords(language = "es"),
         nchar > 2)

df_palabra %>% 
  count(palabra, sort = TRUE) %>% 
  top_n(10)
```

    ## # A tibble: 10 x 2
    ##    palabra          n
    ##    <chr>        <int>
    ##  1 ruta          2726
    ##  2 conservacion  2563
    ##  3 region        2409
    ##  4 mejoramiento  2184
    ##  5 construccion  1880
    ##  6 sector        1439
    ##  7 vial          1122
    ##  8 reposicion    1091
    ##  9 red            937
    ## 10 sistema        726

``` r
df_palabra %>% 
  count(Region, palabra) %>% 
  filter(n > 5) %>% 
  arrange(Region, desc(n))
```

    ## # A tibble: 2,136 x 3
    ##    Region             palabra          n
    ##    <fct>              <chr>        <int>
    ##  1 Arica y parinacota arica          163
    ##  2 Arica y parinacota ruta           159
    ##  3 Arica y parinacota sector         126
    ##  4 Arica y parinacota conservacion   125
    ##  5 Arica y parinacota reposicion     111
    ##  6 Arica y parinacota region          97
    ##  7 Arica y parinacota construccion    77
    ##  8 Arica y parinacota mejoramiento    57
    ##  9 Arica y parinacota vial            51
    ## 10 Arica y parinacota red             50
    ## # ... with 2,126 more rows
