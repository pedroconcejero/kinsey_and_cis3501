# log-linear models
# a partir de variables reagrupadas edad_intervalos, sexo y orienta_sex
# https://cran.r-project.org/web/packages/vcdExtra/vignettes/loglinear.html

load("datos_txt_trabajados.rda")

names(datos2)

library(MASS)

# Primero debemos calcular una tabla.
# Probemos con 
# # P6_2.En.una.relación.sexual.vale.todo..siempre.que.las.personas.implicadas.estén.totalmente.de.acuerdo"
names(datos2)[35]

tabla_loglm <- table(datos2[, 35],
                     datos2$orienta_sex,
                     datos2$sexo)

tabla_l_df <- as.data.frame(tabla_loglm)
tabla_l_df

# Mutual independence

hec.1 <- loglm(Freq~Var1+Var2+Var3, data=tabla_l_df)
hec.1

# conditional independence 

hec.2 <- loglm(Freq~(Var2 + Var3) * Var1, data=tabla_l_df)
hec.2

# and joint independence

hec.3 <- loglm(Freq~(Var2 * Var3) + Var1, data=tabla_l_df)
hec.3

anova(hec.1, hec.2, hec.3)
