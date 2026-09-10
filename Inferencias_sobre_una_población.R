#Ejercicio 3.1
# URL de los datos
url <- paste0("https://raw.githubusercontent.com/mgtagle/",
              "Estadistica_Investigacion_2026/refs/heads/main/", 
              "Datos/Crudos/cuadro1.csv")
# Importar datos directamente desde GitHub
cuadro1 <- read.csv(url, header = TRUE)
# Revisar los datos
head (cuadro1)
str(cuadro1)

# Formule la hipótesis nula(H0) y la hipótesis alternativa(H1).
# Hipótesis Nula (H0): No existe diferencia significativas entre los diametros medios de los arboles 
# Hipótesis Alternativa (H1): Existe una diferencia significativa entre los diametrosmedios de los arboles 

#¿La pregunta planteada requiere una prueba de una cola o de dos colas? Justifique su respuesta.
# Se requiere una prueba de dos colas ya que solo queremos saber si existe una diferencia entree los diametros, no tenemos una especificación si será mayor o menor.

#Calcule la media y la desviación estándar de la variable Diametro.
#El valor de la media es 15.794
#La desviación estandar es 3.227017

mean(cuadro1$Diametro)
sd(cuadro1$Diametro)

#Realice la prueba t de una muestra.
t.test(cuadro1$Diametro, mu = 16.5, alternative = "two.sided")

#Identifique en los resultados:
#estadístico de t; -1.547
#grados de libertad; 49
#valor de p: 0.1283
#media de la muestra; 15.79
#intervalo de confianza del 95%. 14.87689 16.71111

#Con α =0,05, indique si se rechaza o no se rechaza H0.
#p= 0.1283 No se rechaza la Hipótesis Nula 

#Redacte una conclusión en términos del diámetro medio de los árboles del rodal.
#Con base a los resultados obtenidos tenemos que no se rechaza la hipótesis utilizando un valor de significancia del 5% y se estima que el diámetro medio poblacional se encuentra entre 14.88 cm y 16.71 cm con un 95% de confianza.

#¿El intervalo de confianza del 95% contiene el valor de referencia de 16.5 cm?
#Si, el valor de 16.5 se encuentra dentro del rango dee 14.87 16.71

#¿Cómo se relaciona este resultado con la decisión obtenida mediante el valor de p?
#Existe una relación directa ya que ambos nos dicen el resultado estadístico el primero nos dice que no se rechaza la Ho y en el otro nos dice que el valor esta dentro del intervalo de confianza


#Ejercicio 3.2

costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94, 80.7, 82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89, 82.95, 73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01, 80.47, 76.23, 78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29, 75.52, 77.21, 75.99, 81.94, 80.41, 77.7)

#Explique con sus propias palabras qué representan H0 y H1.
#La H0 nos dice que la media es igual a 80 que es el valor que se desea comprobar y se asume verdadera mientras que estadisticamente no se pueda rechazar por otra parte H1 nos dice que la media es menor a 80 esta se aceptara si se tienen los datos encontra de la H0

#¿Por qué corresponde utilizar una prueba de una cola?
#Porque lo se se busca comprobar especificamente es si la media es menor a lo que plantea la hipotesis 

#Calcule la media y desviación estándar de los 44 costales.
mean(costal)
sd(costal) 
# media  78.91
# desviación estándar 3.05

#Realice la prueba:
t.test(costal, mu= 80, alternative = "less")
#Identifique: 
#estadístico t; -2.3644
#grados de libertad; 43
#valor de p; 0.01132
#media observada.78.91068

#Compare el valor de p con α = 0,05.
#El valor de p=0.01 es menor α = 0,05

#¿Se rechaza o no se rechaza H0?
# Si se rechaza debido a que el valor de p es menor

#Redacte una conclusión en el contexto del problema, indicando si existe evidencia estadística suficiente para afirmar que los costales contienen, en promedio, menos de 80 kg.
# Estadisticamente comprobamos que la sospeecha del consumidor es verdadera ya que los valores obtenidos nos dan la certeza que los costales contienen en promedio menor cantida de 80 kg.

