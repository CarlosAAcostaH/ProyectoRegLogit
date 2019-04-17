rm(list = ls())

#Lista de paquetes necesarios para el codigo
list.of.packages = c("faraway", "repmis")
#Define los paquetes no instalados
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#Instala los paquetes no instalados
if (length(new.packages)) install.packages(new.packages)
#Carga las librerias necesarias
lapply(list.of.packages, require, character.only = TRUE)

#Cargamos la base de datos del repositorio
source_data("https://github.com/CarlosAAcostaH/ProyectoRegLineal/blob/master/base_matute.rdata?raw=true")

###################################
# Graficos y medidas descriptivas #
###################################
#Graficas independientes para cada variable
hist(base_matute$id, xlab = "id", ylab = "Frecuencia", main = "")
hist(base_matute$edadMadre, xlab = "Edad de la madre (a??os)", 
     ylab = "Frecuencia", main = "")
barplot(table(base_matute$oxitocico), 
        xlab = "Oxit??cico administrado a la madre", ylab = "Frecuencia", 
        main = "")
hist(base_matute$edadGestacional, xlab = "Edad gestacional (semanas)", 
     ylab = "Frecuencia", main = "")
barplot(table(base_matute$gravidez), xlab = "N??mero de embarazos", 
        ylab = "Frecuencia", main = "")
barplot(table(base_matute$escolaridadMadre), 
        xlab = "Mayor grado de escolaridad de la madre", ylab = "Frecuencia", 
        main = "")
hist(base_matute$peso, xlab = "Peso del neonato (kg)", ylab = "Frecuencia", 
     main = "")
hist(base_matute$talla, xlab = "Estatura del neonato (cm)", 
     ylab = "Frecuencia", main = "")
hist(base_matute$perimetroCefalico, 
     xlab = "Per??metro Cef??lico del neonato (cm)", ylab = "Frecuencia", 
     main = "")
hist(base_matute$HbPre, 
     xlab = "Hemoglobina de la madre previa al parto (g/dL)", 
     ylab = "Frecuencia", main = "")
hist(base_matute$HCTPre, 
     xlab = "Hematocrito de la madre previo al parto (%)", 
     ylab = "Frecuencia", main = "")
hist(base_matute$HbPos, 
     xlab = "Hemoglobina de la madre posterior al parto (g/dL)", 
     ylab = "Frecuencia", main = "")
hist(base_matute$HCTPos, 
     xlab = "Hematocrito de la madre posterior al parto (%)", 
     ylab = "Frecuencia", main = "")


# Definimos funcion de graficar histograma en la diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

# Definimos funcion de escribir cor debajo de la diagonal
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- (cor(x[is.na(x*y) == FALSE], 
               y[is.na(x*y) == FALSE]))
  #  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, round(r, digits = 3))
  if (missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 2*cex.cor * max(c(abs(r),0.4)))
}

# Definimos funcion de graficar nube de puntos o boxplot encima de la diagonal
panel.plot <- function(x,y, col.plot, ...)
{
  points(x,y, ...)
  r <- round(cor(x[is.na(x + y) == FALSE], 
                 y[is.na(x + y) == FALSE]),
             digits = 2)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  #txt <- paste0("R = ", r)
  #usr <- par("usr"); on.exit(par(usr))
  #par(usr = c(0, 1, 0, 1))
  #text(0.5, 0.9, txt)
}
# Se grafican una variable frente a la otra
pairs(base_matute,
      gap = 0, pch = '.',
      diag.panel = panel.hist, 
      lower.panel = panel.cor, 
      upper.panel = panel.plot)

################################
# Creacion y ajuste del modelo #
################################
#Seleccion de modelo por eliminaci??n backward
#Se van eliminando las variables que no tengan significancia para el modelo (5%)
#Primer modelo con todas las variables
modelo = lm(HbPos~id+edadMadre+oxitocico+edadGestacional+gravidez
            +escolaridadMadre+peso+talla+perimetroCefalico+HbPre+HCTPre,
            data = base_matute)
summary(modelo)
anova(modelo)

#Se quita edadGestacional por tener mayor valor F
modelo = lm(HbPos~id+edadMadre+oxitocico+gravidez+escolaridadMadre+peso+talla
            +perimetroCefalico+HbPre+HCTPre, data = base_matute)
summary(modelo)
anova(modelo)

#Se quita perimetroCefalico por tener mayor valor F
modelo = lm(HbPos~id+edadMadre+oxitocico+gravidez+escolaridadMadre+peso+talla
            +HbPre+HCTPre, data = base_matute)
summary(modelo)
anova(modelo)

#Se quita escolaridadMadre por tener mayor valor F
modelo = lm(HbPos~id+edadMadre+oxitocico+gravidez+peso+talla+HbPre+HCTPre, 
            data = base_matute)
summary(modelo)
anova(modelo)

#Se quita talla por tener mayor valor F
modelo = lm(HbPos~id+edadMadre+oxitocico+gravidez+peso+HbPre+HCTPre,
            data = base_matute)
summary(modelo)
anova(modelo)

#Se quita id por tener mayor valor F
modelo = lm(HbPos~edadMadre+oxitocico+gravidez+peso+HbPre+HCTPre,
            data = base_matute)
summary(modelo)
anova(modelo)

#Se quita HCTPre por tener mayor valor F
modelo = lm(HbPos~edadMadre+oxitocico+gravidez+peso+HbPre,
            data = base_matute)
summary(modelo)
anova(modelo)

#Se quita edadMadre por tener mayor valor F
modelo = lm(HbPos~oxitocico+gravidez+peso+HbPre,
            data = base_matute)
summary(modelo)
anova(modelo)

##############
# LINEALIDAD #
##############
HbPos_estimado = predict(modelo)
plot(base_matute$HbPos, HbPos_estimado, pch = 20,
     xlab = "HbPos estimado (g/dl)", ylab = "HbPos (g/dl)")
abline(a = 0, b = 1, col = "red")

##############
# RESIDUALES #
##############
residuales = residuals(modelo)
plot(residuales, pch = 20, xlab = "", ylab = "Residuales (g/dl)")
abline(h = 0, col = "red")
abline(h = min(residuales), col = "red", lty = 2)
abline(h = max(residuales), col = "red", lty = 2)
sprintf("Los residuales se encuentran entre %0.4f y %0.4f", min(residuales),
        max(residuales))

plot(rstandard(modelo), pch = 20, xlab = "", ylab = "Residuales estandarizados")
abline(h = 0, col = "red")
abline(h = min(rstandard(modelo)), col = "red", lty = 2)
abline(h = max(rstandard(modelo)), col = "red", lty = 2)
sprintf("Los residuales estandarizados se encuentran entre %0.4f y %0.4f",
        min(rstandard(modelo)),max(rstandard(modelo)))

#####################
# HOMOCEDASTIICIDAD #
#####################

plot(predict(modelo), rstandard(modelo), pch = 20,
     xlab = "HbPos estimado (g/dl)", ylab = "Residuales estandarizados")
abline(h = 0, col = "red")
abline(h = min(rstandard(modelo)), col = "red", lty = 2)
abline(h = max(rstandard(modelo)), col = "red", lty = 2)

#Definimos el numero de grupos a comparar
n_grupos = 4
#Creamos los grupos teniendo en cuenta el orden
s = sort.int(HbPos_estimado, index.return = TRUE)
g = integer(length(s$ix))
g[s$ix] = 1 + floor(n_grupos*(1:length(s$ix) - 1)/length(s$ix))
#Graficamos los residuales estandarizados para los distintos grupos
plot(predict(modelo), rstandard(modelo), pch = 20, col = g,
     xlab = "HbPos estimado (g/dl)", ylab = "Residuales estandarizados")
abline(h = 0, col = "red")
#Marcamos los puntos extremos de cada grupo
for (i in g) {
  abline(h = min(rstandard(modelo)[g == i]), col = i, lty = 2)
  abline(h = max(rstandard(modelo)[g == i]), col = i, lty = 2)
  abline(v = max(HbPos_estimado[g == i]), col = i, lty = 2)
}
#Hacemos la prueba de Bartlett
#H_0:=Hay homocedastidad entre los grupos (varianza igual)
#H_1:=Los grupos son heterocedasticos (varianza diferente en al menos un grupo)
bartlett.test(rstandard(modelo)~g)
b = bartlett.test(rstandard(modelo)~g)
sprintf("Como p-value=%0.4f>0.05, no se rechaza H_O y se verifica el supuesto de homocedastidad",
        b$p.value)

##############
# NORMALIDAD #
##############
#Se mira graficamente la normalidad
qqnorm(residuales)
qqline(residuales, col = "red")
#Hacemos la prueba de Shapiro para probar normalidad de los residuales
#H_0:=Los residuales tienen distribucion normal
#H_1:=Los residuales no tienen distribucion normal
shapiro.test(residuales)
s = shapiro.test(residuales)
sprintf("Como p-value=%0.4f>0.05, no se rechaza H_O y se verifica el supuesto de normalidad",
        s$p.value)

#####################
# MULTICOLINEALIDAD #
#####################
vif(modelo)
