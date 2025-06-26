# http://analyticswithr.com/contingencytables.html


install.packages("gmodels")
install.packages("janitor")
library(gmodels)

names(datos2)

CrossTable(datos2[,53], datos2[,12])

t <- table(datos2[, 53])
t/nrow(datos2)*100
plot(t)

names(datos2)[53] <- "orienta_sex"
names(datos2)[12] <- "edad"

t2 <- table(datos2[, "orienta_sex"], 
            datos2[, "edad"])
t2

colSums((t2))


tabla_df <- as.data.frame(t2)
tabla <- pivot_wider(tabla_df,
                     names_from = Var1,
                     values_from = Freq)

# Group by 'group' and calculate the percentage within each group
result <- tabla_df %>%
  group_by(edad) %>%
  mutate(percentage = Freq / sum(Freq) * 100)

# View the result
print(result)


names(tabla)[1] <- "edad"
names(tabla_df)[2] <- "edad"
ggplot(result, 
       aes(x = edad,
           y = percentage,
           color = Var1,
           group = Var1)) + 
  geom_line()

names(datos2)[12]
names(datos2)[11] <- "sexo"
names(datos2)[53]

tbl <- xtabs(~ orienta_sex + sexo + edad, 
             data = datos2)
tbl
marginSums(tbl, "orienta_sex")/sum(tbl) *100

plot(table(datos2$edad))

# [17] "P1.Grado.de.importancia.de.tener.pareja.para.llevar.una.vida.satisfactoria"
table(datos2[,17])

# [18] "P2_1.Mantener.conversaciones.subidas.de.tono.con.otra.persona.a.través.de.mensajes..teléfono.o.redes.sociales"             
table(datos2[,18])

# [19] "P2_2.Enamorarse.de.otra.persona.aunque.no.mantenga.relaciones.sexuales.con.ella"                                           
table(datos2[,19])

# [20] "P2_3.Dar.un.beso.en.los.labios.a.otra.persona"                                                                             
table(datos2[,20])

# [21] "P2_4.Tener.relaciones.sexuales.a.través.de.las.redes.sociales.sin.contacto.presencial"                                     
table(datos2[,21])

# [22] "P2_5.Hablar.o.quedar.con.una.ex.pareja.sin.mantener.relaciones.sexuales"                                                   
table(datos2[,22])

# [23] "P2_6.Tener.una.relación.sexual.ocasional.con.otra.persona"                                                                 
table(datos2[,23])

# [24] "P2_7.Tener.relaciones.sexuales.con.una.persona.a.la.que.se.paga"                                                           
table(datos2[,24])

# [25] "P2_8.Mantener.una.relación.afectiva.y.sexual.con.otra.persona"                                                             
table(datos2[,25])

# [26] "P3.Número.de.relaciones.de.pareja.tenidas.a.lo.largo.de.la.vida"                                                           
library(plyr)
datos2[ ,26] <- mapvalues(datos2[ , 26],
                          from = c("4 parejas ", 
                                   "3 parejas ",
                                   "2 parejas ",
                                   "1 pareja ",
                                   "Ninguna"),
                          to = c(4, 3, 2, 1, 0))
datos2$n_parejas <- as.numeric(datos2[, 26])
datos2$n_parejas[datos2$n_parejas > 50] <- "fantasma"

summary(datos2$n_parejas)
hist(datos2$n_parejas[datos2$n_parejas < 20])

# [27] "P4.Tenencia.de.una.relación.de.pareja.en.la.actualidad"                                                                    
table(datos2[,27])

# [28] "PAREJA.Situación.de.pareja.de.la.persona.entrevistada"                                                                     

# [29] "P4A.Año.de.comienzo.de.la.relación.de.pareja"                                                                              

# [30] "P4B.Identidad.de.género.de.la.pareja"                                                                                      

# [31] "P4C.Convivencia.con.la.pareja"                                                                                             

# [32] "P4D.Tipo.de.unión.con.la.pareja"                                                                                           

# [33] "P5.Grado.de.importancia.de.tener.relaciones.sexuales.para.llevar.una.vida.satisfactoria"                                   
table(datos2[,33])

# [34] "P6_1.En.nuestra.sociedad.sigue.habiendo.muchos.prejuicios.y.ocultación.en.relación.al.sexo"                                
table(datos2[,34])

# [35] "P6_2.En.una.relación.sexual.vale.todo..siempre.que.las.personas.implicadas.estén.totalmente.de.acuerdo"                    
table(datos2[,35])

# [36] "P6_3.Los.hombres.tienen.más.deseos.sexuales.que.las.mujeres"                                                               
table(datos2[,36])

# [37] "P6_4.El.sexo.de.verdad.incluye.penetración"                                                                                
table(datos2[,37])

# [38] "P6_5.A.lo.largo.de.la.vida.una.persona.puede.variar.sus.preferencias.sexuales.y.tener.relaciones.con.mujeres.o.con.hombres"
table(datos2[,38])

# [39] "P7_1.Besos.y.caricias.con.otra.persona"                                                                                    

# [40] "P7_2.Masturbación"                                                                                                         

# [41] "P7_3.Masturbación.mutua..con.otra.s.persona.s."                                                                            

# [42] "P7_4.Sexo.oral"                                                                                                            

# [43] "P7_5.Penetración.anal"                                                                                                     

# [44] "P7_6.Penetración.vaginal"                                                                                                  

# [45] "P7_7.Uso.de.juguetes.y.objetos.destinados.a.dar.y.recibir.placer.sexual"                                                   
table(datos2[,45])

# [46] "P7_96.Otras.prácticas.no.citadas.anteriormente"                                                                            
table(datos2[,46])

# [47] "P7_97.Nunca.he.tenido.ninguna.experiencia.sexual.de.ningún.tipo"      
table(datos2[,47])


# Recodificación de orientación sexual para simplificar categorías
# Agrupar en lgtbiqa+ vs. hetero y agrupar NS NC
table(datos2$orienta_sex)
datos2$orienta_sex <- mapvalues(datos2$orienta_sex,
                          from = c("Asexual", 
                                   "Bisexual",
                                   "Demisexual",
                                   "Homosexual (gay o lesbiana)",
                                   "No binario",
                                   "Pansexual",
                                   "Queer",
                                   "N.C.",
                                   "N.S.",
                                   "Otros"),
                          to = c(rep("Diverso LGTBIQA+", 7),
                                 rep("NS/NC/Otros",3)))

datos2$orienta_sex <- mapvalues(datos2$orienta_sex,
                                from = c("(NO LEER) Con ninguna"),
                                to = c("NS/NC/Otros"))

t2 <- table(datos2[, "orienta_sex"], 
            datos2[, "edad"])
t2

porcentajes_col <- prop.table(t2, 
                              margin = 2) * 100

porcentajes_col

tabla_df <- as.data.frame(porcentajes_col)
tabla_df

names(tabla_df) <- c("orienta_sex",
                     "edad",
                     "porcentaje")

ggplot(tabla_df, 
       aes(x = edad,
           y = porcentaje,
           color = orienta_sex,
           group = orienta_sex)) + 
  geom_line() + 
  geom_hline(yintercept = 10,
               color = "magenta")



# Crear intervalos automáticos cada 4 años
datos2$edad_num <- as.numeric(as.character(datos2$edad))
breaks <- seq(18, max(datos2$edad_num), by = 4)
breaks

# Agrupar con cut
datos2$edades_intervalos <- cut(datos2$edad_num, 
                                  breaks = breaks, 
                                  right = FALSE)

t3 <- table(datos2[, "orienta_sex"], 
            datos2[, "edades_intervalos"])
t3

porcentajes_col <- prop.table(t3, 
                              margin = 2) * 100

porcentajes_col

tabla_df2 <- as.data.frame(porcentajes_col)
tabla_df2

names(tabla_df2) <- c("orienta_sex",
                     "edad",
                     "porcentaje")

ggplot(tabla_df2, 
       aes(x = edad,
           y = porcentaje,
           color = orienta_sex,
           group = orienta_sex)) + 
  geom_line() + 
  geom_hline(yintercept = 10,
             color = "magenta")


# Por edad por sexo

t4 <- table(datos2[, "orienta_sex"], 
            datos2[, "edades_intervalos"],
            datos2[, 11])
t4

str(t4)

t5 <- prop.table(t4,
                 margin = c(2, 3))* 100

t5

tabla_sexo <- as.data.frame(t5)
tabla_sexo
names(tabla_sexo) <- c("orienta_sex",
                       "edad",
                       "sexo",
                       "porcentaje")

tabla_sexo_fin <- tabla_sexo[tabla_sexo$orienta_sex != "NS/NC/Otros", ]
tabla_sexo_fin






ggplot(tabla_sexo_fin, 
       aes(x = edad,
           y = porcentaje,
           fill = interaction(orienta_sex, sexo),
           group = interaction(orienta_sex, sexo))) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("pink", 
                               "blue", 
                               "black", 
                               "green"))


# [38] "P6_5.A.lo.largo.de.la.vida.una.persona.puede.variar.sus.preferencias.sexuales.y.tener.relaciones.con.mujeres.o.con.hombres"
table(datos2[,38])

datos2[, 38] <- mapvalues(datos2[, 38],
                                from = c("Bastante de acuerdo", 
                                         "Muy de acuerdo",
                                         "Nada de acuerdo",
                                         "Poco de acuerdo",
                                         "(NO LEER) Ni de acuerdo ni en desacuerdo",
                                         "N.C.",
                                         "N.S."),
                                to = c(rep("SÍ PUEDEN CAMBIAR", 2),
                                       rep("NO CAMBIAN", 2),
                                       rep("NI IDEA, PASO", 3)))


t10 <- table(datos2[, "sexo"], 
            datos2[, "edades_intervalos"],
            datos2[, 38])
t10

str(t10)
t10 <- prop.table(t10,
                 margin = c(1, 2))* 100

t10

tabla_sexo <- as.data.frame(t10)
tabla_sexo
names(tabla_sexo) <- c("sexo",
                       "edad",
                       "opinion_cambio",
                       "porcentaje")

ggplot(tabla_sexo, 
       aes(x = edad,
           y = porcentaje,
           fill = interaction(opinion_cambio, sexo),
           group = interaction(opinion_cambio, sexo))) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("grey", 
                               "blue", 
                               "red", 
                               "grey", 
                               "blue", 
                               "red"))


# Número de parejas por orientación sexual y edad

table(datos2$n_parejas)
hist(datos2$n_parejas[datos2$n_parejas < 20])

# Agrupar con cut
breaks <- c(0, 1, 2, 3, 4, 5, 6, 7, 10 , 20, 100)
breaks


datos2$parejas_intervalos <- cut(datos2$n_parejas, 
                                breaks = breaks, 
                                right = FALSE)

table(datos2$parejas_intervalos)
t30 <- table(datos2[, "orienta_sex"], 
             datos2[, "parejas_intervalos"])
t30

porcentajes_col <- prop.table(t30, 
                              margin = 2) * 100

porcentajes_col

tabla_df <- as.data.frame(porcentajes_col)
tabla_df

names(tabla_df) <- c("orienta_sex",
                     "n_parejas",
                     "porcentaje")

ggplot(tabla_df, 
       aes(x = n_parejas,
           y = porcentaje,
           fill = orienta_sex,
           group = orienta_sex)) + 
  geom_bar(stat = 'identity') 


t300 <- table(datos2[, "orienta_sex"], 
              datos2[, "sexo"],
             datos2[, "parejas_intervalos"])
t300

str(t300)
porcentajes_col <- prop.table(t300, 
                              margin = c(2, 3)) * 100

porcentajes_col

tabla_df <- as.data.frame(porcentajes_col)
tabla_df

names(tabla_df) <- c("orienta_sex",
                     "sexo",
                     "n_parejas",
                     "porcentaje")

ggplot(tabla_df, 
       aes(x = n_parejas,
           y = porcentaje,
           fill = interaction(orienta_sex, sexo),
           group = interaction(orienta_sex, sexo))) + 
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("pink", 
                               "blue", 
                               "grey",
                               "black", 
                               "green",
                               "grey"))


# SAVE DATASET

names(datos2)
save(datos2, file ="datos_txt_trabajados.rda")
