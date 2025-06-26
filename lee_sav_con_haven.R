
# Instalar el paquete si no lo tienes
install.packages("haven")

# Cargar el paquete
library(haven)

# Leer el archivo .sav
datos <- haven::read_sav("/home/pedro/Escritorio/2024_proyectos/estudio CIS 3501/MD3501/3501.sav")

# ERROR !
# Invalid date string (length=9): 12 014 25

library(tidyverse)

datos <- read.csv( "/home/pedro/Escritorio/2024_proyectos/estudio CIS 3501/MD3501/3501_num.csv"
  , sep = ';')
# Ver los primeros registros
head(datos)

save(datos, file = "datos_num_3501.rda")

datos2 <- read.csv( "/home/pedro/Escritorio/2024_proyectos/estudio CIS 3501/MD3501/3501_etiq.csv"
                   , sep = ';')
# Ver los primeros registros
head(datos2)

save(datos2, file = "datos_txt_3501.rda")

names(datos2)
dim(datos2)
