######### PAQUETES #############

library(readxl)
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

######### CONFIGURACIÓN #############

df <- read_excel("./data/distance.dataset.xlsx", sheet=1, col_names = TRUE, col_types = c("numeric", "text", "text", "numeric"))
# Formatea la segunda columna
df$structure <- gsub("\\r\\n", "", df$structure)

######### ESTADÍSTICAS DESCRIPTIVAS #############

descriptive_stats <- function(type) {
  # Calcula estadísticas descriptivas para la variable de distancia en un conjunto de datos.
  #
  # @param type Tipo de estructura para filtrar los datos (opcional). En caso no sea especificado, se hace de toda la matrix.
  # @return Imprime en una sola línea las estadísticas descriptivas básicas como media, mediana, desviación estándar, rango intercualítico, mínimo y máximo.
  
  if(missing(type)) {
    input <- df$distance
  }else{
    input <- df$distance[df$structure==type]
  }
  
  output <- c(
  paste("mean:", mean(input)),
  paste("median:", median(input)),
  paste("standard deviation", sd(input)),
  paste("IQR:", IQR(input)),
  paste("Min:", min(input)),
  paste("Max:", max(input))
  )
  
  print(output)
  print(quantile(input))
}

descriptive_stats()
descriptive_stats("overt connective")
descriptive_stats("juxtaposition")
descriptive_stats("elliptical")

######### HISTOGRAMAS DE FRECUENCIAS #############

freq_histogram <- function(type) {
  # Crea un histograma de frecuencia para la variable de distancia en un conjunto de datos.
  #
  # @param type Tipo de estructura para filtrar los datos. (Opcional)
  # @return Muestra un histograma de frecuencia en la ventana gráfica.
  
  if(missing(type)) {
    input <- df$distance
    text <- paste("Distance (n) of all structures")
  } else {
    input <- df$distance[df$structure==type]
    text <- paste("Distance (n) of structure", type)
  }
  hist(input,
       col = "green",
       breaks = seq(0, length(input), by = 5),
       freq = TRUE,
       xlab = text,
       main=NULL)
}

# Divide la pantalla en 4
par(mfrow=c(2,2))

# Muestra los histogramas
freq_histogram()
freq_histogram("overt connective")
freq_histogram("juxtaposition")
freq_histogram("elliptical")

######### HISTOGRAMAS DE DENSIDAD #############

density_histogram <- function(type) {
  # Crea un histograma de densidad para la variable de distancia en un conjunto de datos.
  #
  # @param type Tipo de estructura para filtrar los datos. (Opcional)
  # @return Muestra un histograma de densidad en la ventana gráfica.
  if(missing(type)) {
    input <- df$distance
    text <- paste("Density of all structures")
  } else {
    input <- df$distance[df$structure==type]
    text <- paste("Density of structure", type)
  }
  hist(input,
       freq = FALSE,
       col = "green",
       breaks = seq(0, length(input), by = 5),
       #xlim = c(0, length(input)/5),
       xlab = text,
       main=NULL)
  lines(density(input))
}

par(mfrow=c(2,2))

density_histogram()
density_histogram("overt connective")
density_histogram("juxtaposition")
density_histogram("elliptical")

######### T-TESTS ######### 

two_tailed_t_test <- function(type1, type2) {
  # Realiza una prueba t de dos colas comparando las distancias entre dos tipos de estructuras.
  #
  # @param type1 Tipo de estructura 1.
  # @param type2 Tipo de estructura 2.
  # @return Devuelve el resultado de la prueba t.
  input <- df[df$structure%in% c(type1, type2),]
  return(t.test(distance~structure, data = input))
}

two_tailed_t_test("overt connective", "juxtaposition") #No significativamente diferente, pero tampooco estadísticamente significativo.
two_tailed_t_test("overt connective", "elliptical") #Significativamente diferente
two_tailed_t_test("juxtaposition", "elliptical")  #Significativamente diferente

######### REGRESIÓN LOGÍSTICA ######### 

#Primero transformamos la información
logreg <- transform(df, probabilities=cut(distance,
        breaks=c(-Inf, median(distance), Inf),
        labels=c("Sí", "No")))

# Y visualizamos el resultado
par(mfrow=c(2,2))

cdplot(probabilities ~ distance, data=logreg, subset=structure == "juxtaposition",
       main="Probabilidades estimadas de la categoría 'Juxtaposition'")
cdplot(probabilities ~ distance, data=logreg, subset=structure == "overt connective",
       main="Probabilidades estimadas de la categoría 'Overt connective'")
cdplot(probabilities ~ distance, data=logreg, subset=structure == "elliptical",
       main="Probabilidades estimadas de la categoría 'Elliptical'")

# Por último, intentamos observar si el modelo es plausible
(glmFit <- glm(probabilities ~ distance + structure, family=binomial(link="logit"),
               data=logreg))

######### NAIVE BAYES ######### 

# Primero preparamos nuestra data
nb <- df[1:2]
nb$structure <- as.factor(nb$structure)
nb$distance <- as.factor(nb$distance)

# Y dividimos nuestra información entre aquella que servirá para entrenar y para probar el modelo
set.seed(1234)
ind <- sample(2, nrow(nb), replace = T, prob = c(0.8, 0.2))
train <- nb[ind == 1,]
test <- nb[ind == 2,]

# Después entrenamos nuestro modelo
model <- naive_bayes(structure ~ ., data = train, laplace = 1, usekernel = TRUE) 
plot(model) 

# Y hacemos algunas predicciones
p <- predict(model, train, type = 'prob')
head(cbind(p, train))

# También medimos la efectividad del modelo generando predicciones para la info de testeo
p <- predict(model, test)
(tab <- table(p, test$structure))
sum(diag(tab)) / sum(tab) # El modelo tiene un 65,84158% de efectividad en la prueba

# Para mejorar el modelo, es una buena idea intentar encontrar la variable k donde se maximice la efectividad:
best_laplace <- 0
best_accuracy <- 0

# Probamos con distintos números
for (laplace_value in seq(0, 5, by = 0.1)) {
  print(laplace_value)
  model <- naive_bayes(structure ~ ., data = train, laplace = laplace_value, usekernel = TRUE)
  p <- predict(model, test)
  (tab <- table(p, test$structure))
  accuracy <- sum(diag(tab)) / sum(tab)
  
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_laplace <- laplace_value
  }
}

# Hasta obtener el modelo donde se maximice la efectividad
final_model <- naive_bayes(structure ~ ., data = train, laplace = best_laplace, usekernel = TRUE)

# Y observar su efectividad
p <- predict(final_model, test)
(tab <- table(p, test$structure))
sum(diag(tab)) / sum(tab) # Alcanzando un 65,84158% con k = 0,1

# Este resultado indica que cualquier k entre 0,1 hasta 1,0 sería suficiente para 
# maximizar la efectividad.

# Por último, podemos hacer nuestros nuevos data frames y nuevas visualizaciones:
winsorize_and_plot <- function(input_df) {
  # Primero normalizamos la información
  winsorize <- function(x, trim = 0.15) {
    q <- quantile(x, c(trim, 1 - trim), na.rm = TRUE)
    x[x < q[1]] <- q[1]
    x[x > q[2]] <- q[2]
    x[x < 0] <- 0
    x[x > 1] <- 1
    return(x)
  }
  
  # Y generamos el data frame y las visualizaciones
  df_prediction <- input_df
  df_prediction$probability <- winsorize(df_prediction$probability)
  plot(df_prediction)
  lm_model <- lm(df_prediction$probability ~ df_prediction$distance)
  abline(lm_model, col = "blue")
  return(df_prediction)
}

# Para la categoría elliptical:
p <- predict(final_model, train, type = 'prob')
df_prediction <- data.frame(seq(1, length(p[, 1][1:100])), p[, 1][1:100])
colnames(df_prediction) <- c("distance", "probability")
winsorized_df <- winsorize_and_plot(df_prediction)

# Para la categoría juxtaposition:
p <- predict(final_model, train, type = 'prob')
df_prediction <- data.frame(seq(1, length(p[, 2][1:100])), p[, 2][1:100])
colnames(df_prediction) <- c("distance", "probability")
winsorized_df <- winsorize_and_plot(df_prediction)

# Y para over connective:
p <- predict(final_model, train, type = 'prob')
df_prediction <- data.frame(seq(1, length(p[, 3][1:100])), p[, 3][1:100])
colnames(df_prediction) <- c("distance", "probability")
winsorized_df <- winsorize_and_plot(df_prediction)


######### VISUALIZACIONES ######### 
ggplot(df, aes(x = distance, y = structure)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Distance and Structure")
