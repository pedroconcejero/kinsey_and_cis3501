# Pruebas chi-square de las tablas
# a partir de variables reagrupadas edad_intervalos, sexo y orienta_sex

load("datos_txt_trabajados.rda")

names(datos2)

# Ejemplo primera tabla, orienta_sex por sexo y edades_intervalos

tabla00 <- table(datos2$orienta_sex,
                 datos2$edades_intervalos,
                 datos2$sexo)
tabla00

#chisq.test() asume que las frecuencias esperadas son ≥ 5. 

# Aplicar chisq.test() a cada "corte" en var3
apply(tabla00, 
      3, 
      function(tab2d) {
  chisq.test(tab2d)
})

apply(tabla00, 
      3, 
      function(tab2d) {
      fisher.test(tab2d,
                  simulate.p.value=TRUE)})


# Var17

names(datos2)[17]

tabla17 <- table(datos2[, 17],
                 datos2$edades_intervalos,
                 datos2$sexo)
tabla17

#chisq.test() asume que las frecuencias esperadas son ≥ 5. 

apply(tabla17, 
      3, 
      function(tab2d) {
        fisher.test(tab2d,
                    simulate.p.value=TRUE)})

# Bucle para variables de cuestionario 17 a 46

# PREGUNTAS BLOQUES 1 Y 2: PAREJAS

for (i in c(17:25)){
  print(names(datos2)[i])
  tabla_b <- table(datos2[, i],
                   datos2$edades_intervalos,
                   datos2$sexo)
  print(tabla_b)
  #chisq.test() asume que las frecuencias esperadas son ≥ 5. 
  apply(tabla_b, 
        3, 
        function(tab2d) {
          print(fisher.test(tab2d,
                      simulate.p.value=TRUE))})
}

# Solo parece haber diferencias en:
# "P2_2.Enamorarse.de.otra.persona.aunque.no.mantenga.relaciones.sexuales.con.ella"
names(datos2)[19]
# y quizás aunque menor efecto en
# "P2_8.Mantener.una.relación.afectiva.y.sexual.con.otra.persona"
names(datos2)[25]

# Veamos con el cruce por orienta_sex (sin edad)

for (i in c(17:25)){
  print(names(datos2)[i])
  tabla_b <- table(datos2[, i],
                   datos2$orienta_sex,
                   datos2$sexo)
  print(tabla_b)
  #chisq.test() asume que las frecuencias esperadas son ≥ 5. 
  apply(tabla_b, 
        3, 
        function(tab2d) {
          print(fisher.test(tab2d,
                            simulate.p.value=TRUE))})
}

# HUYYYYYY HAY DIFERENCIAS SIGNIFICATIVAS EN
# "P2_7.Tener.relaciones.sexuales.con.una.persona.a.la.que.se.paga"
# Los hombres sí, las mujeres no
names(datos2)[24]
tabla <- table(datos2[, 24],
               datos2$sexo)
tabla
fisher.test(tabla)

# BLOQUES 5 Y 6: 33 A 38 - OPINION RELACIONES SEXUALES, TABÚES...

for (i in c(33:38)){
  print(names(datos2)[i])
  tabla_b <- table(datos2[, i],
                   datos2$orienta_sex,
                   datos2$sexo)
  print(tabla_b)
  #chisq.test() asume que las frecuencias esperadas son ≥ 5. 
  apply(tabla_b, 
        3, 
        function(tab2d) {
          print(fisher.test(tab2d,
                            simulate.p.value=TRUE))})
}

# Diferencias quizás en
# "P6_3.Los.hombres.tienen.más.deseos.sexuales.que.las.mujeres"
# Y SOBRE TODO EN
# P6_2.En.una.relación.sexual.vale.todo..siempre.que.las.personas.implicadas.estén.totalmente.de.acuerdo"
# Más a favor los hombres

# Los hombres sí, las mujeres no
names(datos2)[35]
tabla <- table(datos2[, 35],
               datos2$sexo)
tabla
fisher.test(tabla,
            simulate.p.value=TRUE)

tabla <- table(datos2[, 35],
               datos2$orienta_sex)
tabla
fisher.test(tabla,
            simulate.p.value=TRUE)

names(datos2)

# BLOQUE 7: DETALLES DE RELACIÓN SEXUAL
# 39 A 47 Y 51

for (i in c(39:47, 51)){
  print(names(datos2)[i])
  tabla_b <- table(datos2[, i],
                   datos2$orienta_sex,
                   datos2$sexo)
  print(tabla_b)
  #chisq.test() asume que las frecuencias esperadas son ≥ 5. 
  apply(tabla_b, 
        3, 
        function(tab2d) {
          print(fisher.test(tab2d,
                            simulate.p.value=TRUE))})
}

