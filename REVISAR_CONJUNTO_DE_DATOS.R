# Genaro López Grimaldo
# 1280362
# 18/08/2026

#Importar Datos---
# Función read.csv importa datos de excel a R

IE <- read.csv("Vivero1.csv", header = T)
IE$Tratamiento <-as.factor(IE$Tratamiento)

# Revisar----
# Revisar solo una porción dee datos
head(IE) # Primeras filas dee BD IE

tail(IE) # Ultimas filas de BD IE

#Graficar----

hist(IE$IE,
     main = "",
     xlab ="Indice",
     ylab = "Frecuencia",
     ylim = c(0,12),
     col = "lightblue")

# Normalidad datos ----

shapiro.test(IE$IE)

mean(IE$IE) #obtencion media

sd(IE$IE)   #obtencion de la desviación estandar

var(IE$IE)       #obtención de variación estandar

fivenum(IE$IE) 

# Boxplot ---
boxplot(IE$IE ~ IE$Tratamiento,
        col = "red",
        xlab = "Tratamientos",
        ylab = "Índice")

# Revisar homogeneidad de varianzas---
bartlett.test(IE$IE ~ IE$Tratamiento)

df_ctrl <- subset(IE, Tratamiento == "Ctrl")
df_fert <- subset(IE, Tratamiento == "Fert")

shapiro.test(df_ctrl$IE)
shapiro.test(df_fert$IE)

# qqplot para cada grupo de observaciones

qqnorm(df_ctrl$IE); qqline(df_ctrl$IE)
qqnorm(df_fert$IE); qqline(df_fert$IE)

t.test(IE$IE ~ IE$Tratamiento)
t.test(df_ctrl$IE , df_fert$IE)


# Dependientes o apareados

# Ctrl vamos a asumir que es antes de fertilizar

# Fert asumimos un mes despues de fertilizar las mismas plantas

t.test(df_ctrl$IE, df_fert$IE, paired = TRUE)

# Prueba de una sola muestra

boxplot(IE$IE)
t.test(IE$IE, mu = 0.95)

