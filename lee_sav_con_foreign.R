# Instalar si es necesario
install.packages("foreign")

# Cargar el paquete
library(foreign)

# Leer el archivo .sav
datos <- read.spss("/home/pedro/Escritorio/2024_proyectos/estudio CIS 3501/MD3501/3501.sav", 
                   to.data.frame = TRUE)

# No funciona por un error en fecha?
# 

# Ver los primeros registros
head(datos)
