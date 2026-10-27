##Examen
##Genaro_López_Grimaldo

datos <- data_frame(
  Parcela =sprintf("P%02d", 1:30),
  Tratamiento = factor(
    rep(c("Testigo", "Ligero", "Moderado"), each = 10),
    levels = c ("Testigo", "Ligero", "Moderado")
),
Area_basal_inicial = c(
  32, 26, 39, 29, 43, 35, 28, 41, 34, 37, 
  31, 42, 27, 38, 33, 45, 29, 40, 36, 44,
  30, 37, 46, 28, 41, 34, 43, 32, 39, 35
),
Incremento = c(
  1.32, 1.16, 1.61, 1.42, 1.59,
  1.27, 1.35, 1.54, 1.43, 1.62,
  1.48, 1.72, 1.39, 1.82, 1.61,
  1.53, 1.88, 1.67, 1.59, 1.76,
  1.57, 1.93, 1.81, 2.08, 1.69,
  1.84, 2.16, 1.73, 1.98, 1.88
 )
)
Testigo <- subset(datos, Tratamiento == "Testigo")
dos_grupos <- droplevels(
  subset(datos, Tratamiento %in% c("Testigo", "Moderado")))

str(datos)
table (datos$Tratamiento)
mean(datos$Area_basal_inicial)
Testigo <- datos[(datos$Tratamiento=="Testigo"),]
Ligero <- datos[(datos$Tratamiento=="Ligero"),]
Moderado <- datos[(datos$Tratamiento=="Moderado"),]
tapply(datos$Incremento, datos$Tratamiento, mean
)
library(dplyr)

tabla <- datos %>%
  group_by(Tratamiento) %>%
  summarise(
    n = sum(!is.na(Incremento)),
    Media = mean(Incremento, na.rm = TRUE),
    Mediana = median(Incremento, na.rm = TRUE),
    DE = sd(Incremento, na.rm = TRUE),
    Varianza = var(Incremento, na.rm = TRUE),
    Minimo = min(Incremento, na.rm = TRUE),
    Maximo = max(Incremento, na.rm = TRUE),
    CV_pct = (DE / Media) * 100,
    .groups = "drop"
  )
## El Tratamiento Moderado presenta una mayor media y mayor dispersion absoluta 
## En el Testigo se presenta una mayor dispersión relativa 

##Representación Grafica

op <- par(no.readonly = TRUE)
par(mar = c(5, 8, 4, 2))
boxplot(
  Incremento ~ Tratamiento,
  data = datos,
  col = c("#D9D9D8", "#A6CEE6", "#B2DF3A"),
  xlab = "Area_basal_inicial",
  ylab = expression(
    "Incremento (" *
      plain(m)^2 ~ plain(ha)^{-1} ~ "año"^{-1} * ")"
  ),
  outline = FALSE
)
set.seed(123)

stripchart(
  Incremento ~ Area_basal_inicial,
  data = datos,
  vertical = TRUE,
  method = "jitter",
  jitter = 0.10,
  pch = 16,
  col = "#333333",
  add = TRUE
)
modelo_reg <- lm(Incremento ~ Area_basal_inicial, data = Testigo)

plot(
  testigo$Area_basal_inicial, testigo$Incremento,
  pch = 16, col = "green",
  xlab = "Área basal inicial (m² ha⁻¹)",
  ylab = "Incremento de área basal (m² ha⁻¹ año⁻¹)"
)
abline(modelo_reg, col = "red", lwd = 2)
par(op)

##Las medianas de estos tratamiento son para el testigo 1.25 para el tratammiento Ligero 1.55 y moderado 1.81
##El incremento del area basal aumenta conforme se pase del 
#EExiste poca supersposion en las distribuciones y no se encunetran valores atipicos

# cuenta con dirrecion positiva 
# dispersción moderada alrededor de la recta


##Correlación_de_Pearson

resultado_cor <- cor.test(
  Testigo$Area_basal_inicial, Testigo$Incremento,
  method = "pearson", alternative = "two.sided"
)
resultado_cor
##Con los valores obtenidos muestra que existe evidencia de asociación lineal positiva para este ejercicio

##Regresión_lineal_simple
modelo_reg <- lm(Incremento ~ Area_basal_inicial, data = Testigo)
summary(modelo_reg)
confint(modelo_reg)
plot(
  Testigo$Area_basal_inicial, Testigo$Incremento,
  pch = 16, col = "#2166AC",
  xlab = "Área basal inicial (m² ha⁻¹)",
  ylab = "Incremento de área basal (m² ha⁻¹ año⁻¹)"
)
abline(modelo_reg, col = "#B2182B", lwd = 2)

##Estimación_dentro_del_intervalo_observado
predict(
  modelo_reg,
  newdata = data.frame(Area_basal_inicial = 30),
  interval = "confidence", level = 0.95
)
resultado_t <- t.test(
  Incremento ~ Tratamiento, data = dos_grupos,
  alternative = "two.sided", var.equal = TRUE,
  conf.level = 0.95
)
resultado_t
modelo_anova <- aov(Incremento ~ Tratamiento, data = datos)
summary(modelo_anova)
par(mfrow = c(1, 2))
plot(modelo_reg, which = 1)
plot(modelo_reg, which = 2)
par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
plot(modelo_anova, which = 1)
plot(modelo_anova, which = 2)
par(mfrow = c(1, 1))


