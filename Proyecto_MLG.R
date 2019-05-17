rm(list = ls())
if (!is.null(dev.list())) dev.off()
par(pty = 'm')

#Lista de paquetes necesarios para el codigo
list.of.packages = c("faraway", "repmis","MASS", "pROC", 'utils')
#Define los paquetes no instalados
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#Instala los paquetes no instalados
if (length(new.packages)) install.packages(new.packages)
#Carga las librerias necesarias
lapply(list.of.packages, require, character.only = TRUE)

#Cargamos la base de datos del repositorio
source_data("https://github.com/CarlosAAcostaH/ProyectoRegLineal/blob/master/base_matute.rdata?raw=true")

#Creamos la variable anemiaPos con base a un umbral
umbral = 11
base_matute$anemiaPos = factor(base_matute$HbPos<umbral)

table(base_matute$anemiaPos,base_matute$gravidez)
table(base_matute$anemiaPos,base_matute$talla)


#Se empieza por un modelo con todas las variables
modelo = glm(anemiaPos~id+edadMadre+oxitocico+edadGestacional+gravidez
             +escolaridadMadre+peso+talla+perimetroCefalico+HbPre+HCTPre,
             family = binomial, data = base_matute)

#Se optimiza el modelo de acuerdo con el AIC 
modelo = stepAIC(modelo)

#Graficamos probabilidad de ser anemica (predicción del modelo) vs ser anemica (real)
xlinea = seq(min(predict(modelo)),max(predict(modelo)),0.1)
ylinea = exp(xlinea)/(1 + exp(xlinea))
plot(predict(modelo), as.numeric(base_matute$anemiaPos) - 1,
     xlab = "Predicción del modelo", ylab = 'Paciente anemica', pch = 20,
     col = xor((predict(modelo)>0),(as.numeric(base_matute$anemiaPos)-1)) + 1)
lines(xlinea, ylinea, col = 'red')
legend(-10,1,c('bien calificados','mal clasificados'),pch = 20, col = c(1,2))

#Para interpretar los coeficientes del modelo
exp(coef(modelo))
#Quiere decir:
#El chance (odd) de sufrir anemia postparto disminuye 90.1164% si se suministra
  #misoprostol y el resto de variables se conserva
#El chance de sufrir anemia postparto disminuye 97.4085% si se suministra
  #oxitocina y el resto de variables se conserva
#El chance de sufrir anemia postparto disminuye 48.2530% por cada parto anterior
  #si se conserva el resto de variables
#El chance de sufrir anemia postparto aumenta 0.4726% por cada kg mas del
  #neonato y el resto de las variables se conservan
#El chance de sufrir anemia postparto disminuye 20.2908% por cada gr/dl mas de
  #hemoglobina en sangre antes del parto y el resto de varibles no varian

###Inferencia
##H0:El modelo es igual al modelo saturado
#El estadístico de prueba es 
modelo$deviance
#el cuál es menor que el percentil
qchisq(0.95,modelo$df.residual)
#por lo tanto no se rechaza la hipótesis nula

##H0:beta1=beta2=0
#El estadístico de prueba es:
modelo$null.deviance - modelo$deviance
#que es mayor que el percentil
qchisq(0.90,modelo$df.null - modelo$df.residual)
#Por lo tanto se rechaza la hipótesis nula, es decir que al menos una de las 
#variables tiene relación lineal con el logit del pulso alto

#La prueba de Wald se utiliza para ver si un coeficiente es cero
#H0:beta=0
#El valor de la prueba es la columna Pr(>|z|)
summary(modelo)
#Con una significancia del 10% se rechazan las hipotesis nulas para todas 
#variables explicativas

###Diagnóstico del modelo
#Predicciones
#probabilidades estimadas
pi = predict(modelo, type = "response")
#la siguiente gráfica compara las probabilidades de sufrir anemia postparto 
#estimadas por el modelo contra la variable respuesta original
plot(base_matute$anemiaPos, pi, ylab = expression(hat(pi)), 
     xlab = 'Anemia postparto', pch = 20)
#el ideal es que el modelo estime probabilidades muy bajas para las madres no 
#anemicas y probabilidades altas para madres anemicas

#Una forma de evaluar ésto es a partir de una curva ROC, evaluando la 
#sensibilidad y la especificidad del modelo
g = roc(base_matute$anemiaPos ~ pi)
par(pty = 's')
plot(1 - g$specificities, g$sensitivities, type = 'l',
     xlim = 0:1, ylim = 0:1, xlab = '1-Especificidad', ylab = 'Sensitividad')
polygon(c(1 - g$specificities,1), c(g$sensitivities,0), col = 'cyan')
areaRoc = auc(g)
text(0.5,0.55, 'Area bajo la curva')
text(0.5,0.45, sprintf('AUC=%0.4f',areaRoc))
par(pty = 'm')

#matriz de diseño del modelo
x = model.matrix(modelo)
#número de parámetros del modelo (incluyendo el intercepto)
p = ncol(x)
#vector de ponderaciones
w = modelo$weights
#matriz cuya diagonal tiene las ponderaciones del modelo
W = diag(w)
#matriz H
H = solve(t(x) %*% W %*% x)
H = sqrt(W) %*% x %*% H %*% t(x) %*% sqrt(W)
#leverage
h = diag(H)

##Puntos de palanca
#tamaño de la muestra
n = nrow(x)
#crea un vector con los números de 1 a n
i = 1:n
#gráfica de las probabilidades estimadas vs leverage
plot(pi,h,xlab = expression(hat(pi)(x)), ylab = expression(h[ii,v]),
     col = as.numeric(base_matute$anemiaPos), pch = 20)
legend(0, 0.25, c('No anemica','Anemica'), col = 1:2, pch = 20)
#punto de corte para identificar los datos de palanca
abline(h = 2*p/n, col = "red")
#identificación de los puntos que sobrepasan el límite
for (dato in i[h > 2*p/n]) {
  text(pi[dato], h[dato]+0.005, sprintf('%i',dato), cex = 0.75, adj = 0.5, 
       col = as.numeric(base_matute$anemiaPos))
}
i[h >= 2*p/n]

##Datos aberrantes
#Residuales de componente de desvío
td = resid(modelo, type = "deviance")/sqrt(1 - h)
#gráfica de las estimaciones de las probabilidades estimadas vs residuales de desvío
plot(pi,td, xlab = expression(hat(pi)(x)), ylab = "Componente de desvío",
     col = (abs(td) > 2) + 1, pch = 20)
#dado que éstos residuales tienen una distribución aproximadamente normal, los
#datos extremos son aquellos con valor absoluto mayor a 2
abline(h = c(-2, 0, 2), col = c("red","gray","red"), lty = c(1,2,1))
#identificación de los datos aberrantes
for (dato in i[abs(td) > 2]) {
  text(pi[dato] + 0.01, td[dato], sprintf('%i',dato), cex = 0.75, adj = 0, col = 'red')
}

##Residuales de Pearson
rp = resid(modelo, type = "pearson")
plot(rp, xlab = '', ylab = 'Residuales de Pearson', 
     col = (abs(rp) > 2) + 1, pch = 20)
abline(h = c(-2, 0, 2), col = c("red","gray","red"), lty = c(1,2,1))
for (dato in i[abs(rp) > 2]) {
  text(dato + 1,rp[dato], sprintf('%i',dato), cex = 0.75, adj = 0, col = 'red')
}


###Influencia
##Distancia de Cook
cook = cooks.distance(modelo)
#gráfica de las estimaciones de las probabilidades estimadas vs la distancia de Cook
plot(pi,cook,xlab = expression(hat(pi)(x)), ylab = expression(LD[i]),
     col = (cook > (max(cook)/2)) + 1, pch = 20)
#se deben identificar las observaciones con las distancias de Cook mas altas
for (dato in i[cook > (max(cook)/2)]) {
  text(pi[dato] + 0.01,cook[dato], sprintf('%i',dato), cex = 0.75, adj = 0, col = 'red')
}

##ENVELOPE
#Esta gráfica nos permite identificar si el modelo elegido es el apropiado para ajustar los datos
#El modelo es apropiado siempre que los residuales de desvío (puntos) se ubiquen dentro de las 
#franjas. Éstas franjas se construyen con base en simulaciones que suponendo que el modelo 
#que se ajustó es el apropiado

#Definimos el numero de simulaciones
n_sim = 1000

#Matriz con ceros de tamaño n por n_sim (número de simulaciones)
e = matrix(0,n,n_sim)
#Creamos barra de progreso
pb <- txtProgressBar(min = 0, max = n_sim, style = 3)
#simulación
for (i in 1:n_sim){
  #crea una variable con distribución bernoulli
  dif = runif(n)-fitted(modelo)
  dif[dif >= 0] = 0
  dif[dif < 0] = 1
  resp_sim = dif
  #ajusta un modelo cuya variable respuesta es la creada anteriormente y con las mismas variables
  #explicativas con las que ajustamos nuestro modelo
  modelo_sim = glm(resp_sim~x, family = "binomial")
  #pesos del modelo
  w_sim = modelo_sim$weights
  #matriz de pesos
  W_sim = diag(w_sim)
  #matriz H
  H_sim = solve(t(x) %*% W_sim %*% x)
  H_sim = sqrt(W_sim) %*% x %*% H_sim %*% t(x) %*% sqrt(W_sim)
  #leverage
  h_sim = diag(H_sim)
  #residuales de desvío
  e[,i] = sort(resid(modelo,type="deviance")/sqrt(1 - h_sim))
  #actualizamos barra de progreso
  setTxtProgressBar(pb, i)
}
#vectores numéricos con tamaño igual al tamaño de muestra
e_min = numeric(n)
e_max = numeric(n)
#guarda en los anteriores vectores los percentiles 0.05 y 0.95 de los residuales de desvío
for (i in 1:n) {
  e0 = sort(e[i,])
  e_min[i] = e0[round(5*n_sim/100, digits = 0)]
  e_max[i] = e0[round(95*n_sim/100, digits = 0)]
}
#vector de medias de los residuales de desvío
media = apply(e, 1, mean)
rango = range(td, e_min, e_max)
par(pty = "s")
#gráfico de probabilidad normal de los residuales de desvío
qqnorm(td, xlab = "Percentiles N(0,1)", ylab = "Componente de Desvío",
       ylim = rango, cex = 0.5, pch = 20)
par(new = TRUE)
#gráfico de probabilidad normal del percentil 0.05 de los residuales de la simulación
qqnorm(e_min, axes = FALSE, xlab = "", ylab = "", type = "l", ylim = rango, 
       col = 'red', lty = 2)
par(new = TRUE)
#gráfico de probabilidad normal del percentil 0.95 de los residuales de la simulación
qqnorm(e_max, axes = FALSE, xlab = "", ylab = "", type = "l", ylim = rango, 
       col = 'red', lty = 2)
par(new = TRUE)
#gráfico de probabilidad normal de la media de los residuales de la simulación
qqnorm(media, axes = FALSE, xlab = "", ylab = "", type = "l", ylim = rango, 
       col = 'gray', lty = 1)
legend(-2.5, 2, c('Percentiles 5% y 95%','Media'), cex = 0.75,
       lty = c(2,1), col = c('red', 'gray'), box.lty = 0, bg = 'transparent')
#Las franjas se encuentran bastante cercanas, sin embargo, no hay puntos fuera 
#de dichas franjas, lo cual es un indicativo de un buen ajuste del modelo

#Prueba de Hoslem-Lemeshow
#Se divide las predicciones en cierto número de intervalos y se compara lo
#observado con lo esperado por las predicciones en cada intervalo
#La prueba depende del número de grupos
#H0: El modelo se ajusta a la realidad observada
n_g = 2:n
hl_test = n_g
for (i in 1:length(n_g)) {
  hl_test[i]=(hoslem.test(pi,as.numeric(base_matute$anemiaPos), n_g[i])$p.value)
}
plot(n_g, hl_test, xlab = 'número de grupos', ylab = 'p-value', 
     col = (hl_test < 0.05) + 1, pch = 20)
abline(h=0.05, col = 'red')


