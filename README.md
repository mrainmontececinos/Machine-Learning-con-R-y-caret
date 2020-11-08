# Machine Learning con R y caret

# 1. Introducción
Durante los últimos años, el interés y la aplicación de machine learning ha experimentado tal expansión, que se ha convertido en una disciplina aplicada en prácticamente todos los ámbitos de investigación académica e industrial. El creciente número de personas dedicadas a esta disciplina ha dado como resultado todo un repertorio de herramientas con las que, perfiles con especialización media, consiguen acceder a métodos predictivos potentes. El lenguaje de programación R es un ejemplo de ello.

```{r}
data <- read.delim("F:/mrain/data.txt", header=T,encoding = 'UTF-8')


data$Target=factor(data$Target, levels = c(0,1),
                   labels = c("No","Si"))

set.seed(345)


library(caret)
library(tidyverse)

data$Ciudad=as.factor(data$Ciudad)
data$Estrato=as.factor(data$Estrato)
data$Canal=as.factor(data$Canal)
data$Campaña=as.factor(data$Campaña)
data$Nivel_Academico=as.factor(data$Nivel_Academico)
data$Motivo_Retiro=as.factor(data$Motivo_Retiro)
data$Franquicia=as.factor(data$Franquicia)
data$Genero=as.factor(data$Genero)
data$Vivienda=as.factor(data$Vivienda)
data$Auto=as.factor(data$Auto)
data$Contrato=as.factor(data$Contrato)
data$Act._Economica=as.factor(data$Act._Economica)
data$Estado_civil=as.factor(data$Estado_civil)
data$Cta._Ahorro_.Ent.=as.factor(data$Cta._Ahorro_.Ent.)
```


# 2 Análisis exploratorio de los datos 
Antes de entrenar un modelo predictivo, o incluso antes de realizar cualquier cálculo con un nuevo conjunto de datos, es muy importante realizar una exploración descriptiva de los mismos. Este proceso permite entender mejor que información contiene cada variable, así como detectar posibles errores.

  ## 2.1 Distribución de variables respuesta 
  Cuando se crea un modelo, es muy importante estudiar la distribución de la variable respuesta, ya que, a fin de cuentas, es lo que nos interesa predecir.
  
  ```{r}
ggplot(data = data, aes(x = Target, y = ..count.., fill = Target)) +
  geom_bar() + scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Target") + theme_bw() +
  theme(legend.position = "bottom")
prop.table(table(data$Target)) %>% round(digits = 2)

```


Porcentaje de aciertos si se predice para todas las observaciones

```{r}
n_observaciones <- nrow(data) 
predicciones <- rep(x = "No", n_observaciones)
mean(predicciones == data$Target) * 100

```
  
  
 ## 2.2 Distribución de variables continuas y cualitativas 
 Como el objetivo del estudio es predecir Target si y no, el análisis de cada variable se hace en relación a la variable respuesta Target.          
 Analizando los datos de esta forma, se pueden empezar a extraer ideas sobre qué variables están más relacionadas con la Target.
  
  
  
  
```{r}
library(ggpubr)

p1 <- ggplot(data = data, aes(x = Edad, fill = Target)) + 
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = Target), alpha = 0.5) + 
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw() 

p2 <- ggplot(data = data, 
             aes(x = Target, y = Edad, color = Target)) + 
  geom_boxplot() +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()


final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, 
                              top = text_grob("Edad", size = 15)) 
final_plot
```



```{r}
data %>% group_by(Target) %>%
  summarise(count=n(),
            media = mean(Edad),
            mediana = median(Edad),
            min = min(Edad), 
            max = max(Edad))
```


##  Distribución de variables cualitativas


```{r}
ggplot(data = data, aes(x =Ciudad , y = ..count.., fill = Target)) + 
  geom_bar() + scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Ciudad") + theme_bw() +
  theme(legend.position = "bottom")


prop.table(table(data$Ciudad, data$Target), margin = 1) %>% 
  round(digits = 2)
```


```{r}
ggplot(data = data, aes(x =Estado_civil , y = ..count.., fill = Target)) + 
  geom_bar() + scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Ciudad") + theme_bw() +
  theme(legend.position = "bottom")


prop.table(table(data$Estado_civil, data$Target), margin = 1) %>% 
  round(digits = 2)
```


```{r}
ggplot(data = data, aes(x =Contrato , y = ..count.., fill = Target)) + 
  geom_bar() + scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Contrato") + theme_bw() +
  theme(legend.position = "bottom")


prop.table(table(data$Contrato, data$Target), margin = 1) %>% 
  round(digits = 2)
```


```{r}
ggplot(data = data, aes(x =Nivel_Academico , y = ..count.., fill = Target)) + 
  geom_bar() + scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Nivel Academico") + theme_bw() +
  theme(legend.position = "bottom")


prop.table(table(data$Nivel_Academico, data$Target), margin = 1) %>% 
  round(digits = 2)


```

  
  ## 2.4 Importancia de las variables 
  La representación gráfica de la distribución de las variables en función de si los pasajeros sobrevivieron o no, ayuda a tener una idea de qué variables pueden ser buenos 
  predictores para el modelo y cuales no aportan información o la que aportan es redundante.
  
  
  
  ### 2.4.1 Contraste de proporciones 
  Para la identificación de potenciales predictores cualitativos, es interesante encontrar las variables y niveles de las mismas que muestran una proporción de Target se aleja 
  de lo esperado por el nivel basal, en este caso el 13%
  
  
```{r}
datos_cualitativos <-  data %>% 
  select(-Tipo_documento, -ID, -Fecha_de_Corte, 
         -Fecha_nacimiento, -Fecha_activación,-Corte_AAAAMM,
         -Motivo_Retiro,-Promedio_Uso._12_meses_atras,-Cupo,
         -Ingresos_smlv,-Ingresos_Cliente,-Promedio_Uso._historico,
         -Edad,-Máximo_Plazo_Diferido,-Score_Central,-endeudamiento_rotativo_Sector,
         -Corte_AAAAMM)


datos_cualitativos_tidy <- datos_cualitativos %>% 
  gather(key = "variable", value = "grupo",-Target)


datos_cualitativos_tidy <- datos_cualitativos_tidy %>% 
  mutate(variable_grupo = paste(variable, grupo, sep="_"))



test_proporcion <- function(df){ 
  n_si <- sum(df$Target == "Si")
  n_no <- sum(df$Target == "No") 
  n_total <- n_si + n_no
  test <- prop.test(x = n_si, n = n_total, p = 0.13) 
  prop_si <- n_si / n_total 
  return(data.frame(p_value = test$p.value, prop_si)) 
  }


analisis_prop <- datos_cualitativos_tidy %>% 
  group_by(variable_grupo) %>% nest() %>% 
  arrange(variable_grupo) %>%
  mutate(prop_test = map(.x = data, .f = test_proporcion)) %>% 
  unnest(prop_test) %>% arrange(p_value) %>% 
  select(variable_grupo,p_value, prop_si) 

analisis_prop


top6_grupos <- analisis_prop %>% pull(variable_grupo) %>% 
  head(6)
```


```{r}

plot_grupo <- function(grupo, df, threshold_line = 0.13){
  p <- ggplot(data = df, aes(x = 1, y = ..count.., fill = Target)) +
    geom_bar() + 
  scale_fill_manual(values = c("gray50", "orangered2")) + 
  # Se añade una línea horizontal en el nivel basal 
    geom_hline(yintercept = nrow(df) * threshold_line,
               linetype = "dashed") + labs(title = grupo) +
    theme_bw() + 
    theme(legend.position = "bottom",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(), 
          axis.ticks.x = element_blank()) 
  return(p) }




datos_graficos <- datos_cualitativos_tidy %>%
  filter(variable_grupo %in% top6_grupos) %>% 
  group_by(variable_grupo) %>% nest() %>% 
  arrange(variable_grupo)

plots <- map2(datos_graficos$variable_grupo,
              .y = datos_graficos$data, .f = plot_grupo)

ggarrange(plotlist = plots, common.legend = TRUE)



```
  
 
  ### 2.4.2 Random forest 
  
  Otra estrategia ampliamente extendida para estudiar la importancia de variables es el empleo de Random Forest. El problema de aplicarlo a este ejemplo es que no acepta 
  valores ausentes, tema que todavía no se ha tratado (visto más adelante).
  
  
```{r}
library(randomForest) 

datos_rf <- data %>% select(-Tipo_documento, -ID, -Fecha_de_Corte, 
 -Fecha_nacimiento, -Fecha_activación,-Corte_AAAAMM,-Motivo_Retiro)



datos_rf <- map_if(.x = datos_rf, .p = is.character, .f = as.factor) %>% 
  as.data.frame()

modelo_randforest <- randomForest(formula = Target ~ . ,
                                  data =datos_rf,
                                  mtry = 5, importance = TRUE,
                                  ntree = 100) 

importancia <- as.data.frame(modelo_randforest$importance) 
importancia <- rownames_to_column(importancia,var = "variable")

p1 <- ggplot(data = importancia, 
             aes(x = reorder(variable, MeanDecreaseAccuracy),
                 y = MeanDecreaseAccuracy, 
                 fill = MeanDecreaseAccuracy)) +
  labs(x = "variable", title = "Reducción de Accuracy") +
  geom_col() + coord_flip() + theme_bw() +theme(legend.position = "bottom") 

p2 <- ggplot(data = importancia,
             aes(x = reorder(variable, MeanDecreaseGini), 
                 y = MeanDecreaseGini, fill = MeanDecreaseGini)) + 
  labs(x = "variable", title = "Reducción de pureza (Gini)") +
  geom_col() + coord_flip() + theme_bw() + 
  theme(legend.position = "bottom") 
library("ggpubr")
ggarrange(p1, p2)

```

  
  
# 3 División de los datos en entrenamiento y test 

  Evaluar la capacidad predictiva de un modelo consiste en comprobar cómo de próximas son sus predicciones a los verdaderos valores de la variable respuesta. 
  Para poder cuantificar de forma correcta este error, se necesita disponer de un conjunto de observaciones, de las que se conozca la variable respuesta, pero que el modelo no   haya  “visto”, es decir, que no hayan participado en su ajuste. 
  
  
   Se crean los índices de las observaciones de entrenamiento
```{r}
set.seed(342)
train <- createDataPartition(y = data$Target, 
                             p = 0.8, list = FALSE, times = 1)
datos_train <- data[train, ] 
datos_test <- data[-train, ]
```

Es importante verificar que la distribución de la variable respuesta es similar en el conjunto de entrenamiento y en el de test. Por defecto, la función createDataPartition() garantiza una distribución aproximada.



```{r}
prop.table(table(datos_train$Target))
prop.table(table(datos_test$Target))
```

  

# 4 Preprocesado de los datos 
  El preprocesado de datos engloba aquellas transformaciones de los datos hechas con la finalidad de que puedan ser aceptados por el algoritmo de machine learning o que mejoren   sus resultados. Todo preprocesado de datos debe aprenderse de las observaciones de entrenamiento y luego aplicarse al conjunto de entrenamiento y al de test.
  
 
  
```{r}
library(recipes)


objeto_recipe <- recipe(formula = Target ~Ciudad+Estado_civil+Edad+
                        Contrato+Nivel_Academico, data = datos_train) 
objeto_recipe


```
  
  
  
  ## 4.1 Variables con varianza próxima a cero
  No se deben incluir en el modelo predictores que contengan un único valor (cero-varianza) ya que no aportan información. Tampoco es conveniente incluir predictores que tengan   una varianza próxima a cero, es decir, predictores que toman solo unos pocos valores, de los cuales, algunos aparecen con muy poca frecuencia.
  
  ```{r}
data %>% select(Ciudad,Estado_civil,Edad,Contrato,Nivel_Academico) %>% 
  nearZeroVar(saveMetrics = TRUE)
```
  
  
  ## 4.2 Estandarización y escalado 
  Cuando los predictores son numéricos, la escala en la que se miden, así como la magnitud de su varianza pueden influir en gran medida en el modelo. Muchos algoritmos de 
  machine learning (SVM, redes neuronales, lasso…) son sensibles a esto, de forma que, si no se igualan de alguna forma los predictores, aquellos que se midan en una escala 
  mayor o que tengan más varianza, dominarán el modelo aunque no sean los que más relación tienen con la variable respuesta. Existen principalmente 2 estrategias para evitarlo:

  Centrado: consiste en restarle a cada valor la media del predictor al que pertenece. Si los datos están almacenados en un dataframe, el centrado se consigue restándole a cada 
  valor la media de la columna en la que se encuentra. Como resultado de esta transformación, todos los predictores pasan a tener una media de cero, es decir, los valores se 
  centran en torno al origen.

  Normalización (estandarización): consiste en transformar los datos de forma que todos los predictores estén aproximadamente en la misma escala. Hay dos formas de lograrlo.
  
  ## 4.3 Binarización de variables cualitativas y numerica
  
  
```{r}
objeto_recipe <- objeto_recipe %>% step_center(all_numeric()) 
objeto_recipe <- objeto_recipe %>% step_scale(all_numeric())

```
  
  La binarización consiste en crear nuevas variables dummy con cada uno de los niveles de las variables cualitativas. A este proceso también se le conoce como one hot encoding.
  
  

```{r}

objeto_recipe <- objeto_recipe %>% step_dummy(all_nominal(), -all_outcomes())

```

Una vez que se ha creado el objeto recipe con todas las transformaciones de preprocesado, se aprenden con los datos de entrenamiento y se aplican a los dos conjuntos.

 Se entrena el objeto recipe
```{r}
trained_recipe <- prep(objeto_recipe, training = datos_train) 
trained_recipe
```


Se aplican las transformaciones al conjunto de entrenamiento y de test

```{r}
datos_train_prep <- bake(trained_recipe, new_data = datos_train)
datos_test_prep <- bake(trained_recipe, new_data = datos_test)
glimpse(datos_train_prep)

```

  
# 5 Modelos 
  ## 5.1 K-Nearest Neighbor (kNN)
  
  
```{r}
set.seed(342)
control_train <- trainControl(method = "repeatedcv",
                              number = 10, 
                              repeats = 5)

hiperparametros <- data.frame(k =  5)




set.seed(342)

modelo_knn <- train(Target~ ., data = datos_train_prep,
                    method = "knn",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    trControl = control_train) 
modelo_knn


print(modelo_knn$finalModel)



#Predicion

predicciones_knn <- predict(modelo_knn,
                           newdata = datos_test_prep, 
                         type = "raw") 

#Error de test

confusionMatrix(data = predicciones_knn,
                reference = datos_test_prep$Target)

#Error de test 
error_test <- mean(predicciones_knn != datos_test_prep$Target)
paste("El error de test del modelo:", round(error_test*100, 2), "%")


```

  ## 5.2 Naive Bayes 
  
  

```{r}
set.seed(342) 
control_train <- trainControl(method = "repeatedcv",
                              number = 10, 
                              repeats = 5) 


hiperparametros <- data.frame(usekernel = TRUE, fL = 0 ,
                            adjust= 5)


set.seed(342) 

modelo_nb <- train(Target ~ ., data = datos_train_prep,
                   method = "nb",
                   metric = "Accuracy",
                   tuneGrid = hiperparametros,
                   trControl = control_train)
modelo_nb





#predicion

predicciones_nb<- predict(modelo_nb,
                            newdata = datos_test_prep, 
                            type = "raw") 


confusionMatrix(data = predicciones_nb,
                reference = datos_test_prep$Target)

#Error de test 
error_test <- mean(predicciones_nb != datos_test_prep$Target)
paste("El error de test del modelo:", round(error_test*100, 2), "%")


```
  
  ## 5.3 Regresión logística 
  
  
```{r}
set.seed(342) 
control_train <- trainControl(method = "repeatedcv",
                              number = 10, 
                              repeats = 5)

set.seed(342)

modelo_logistic <- train(Target~ ., data = datos_train_prep,
                         method = "glm", 
                         metric = "Accuracy", 
                         trControl = control_train,
                         family = "binomial") 

modelo_logistic


summary(modelo_logistic$finalModel)



#predicion

predicciones_logistic<- predict(modelo_logistic,
                          newdata = datos_test_prep, 
                          type = "raw") 

confusionMatrix(data = predicciones_logistic,
                reference = datos_test_prep$Target)

#Error de test 
error_test <- mean(predicciones_logistic != datos_test_prep$Target)
paste("El error de test del modelo:", round(error_test*100, 2), "%")


```

  ## 5.4 Árbol de clasificación simple 
  
```{r}
set.seed(342)
control_train <- trainControl(method = "repeatedcv",
                              number = 10, 
                              repeats = 5)




modelo_C50Tree <- train(Target ~ ., data = datos_train_prep,
                        method = "C5.0Tree",
                        metric = "Accuracy",
                        trControl = control_train) 
modelo_C50Tree

summary(modelo_C50Tree$finalModel)


#predicion
set.seed(342)
predicciones_C50Tree<- predict(modelo_C50Tree,
                                newdata = datos_test_prep, 
                                type = "raw") 

confusionMatrix(data = predicciones_C50Tree,
                reference = datos_test_prep$Target)

#Error de test 
error_test <- mean(predicciones_C50Tree != datos_test_prep$Target)
paste("El error de test del modelo:", round(error_test*100, 2), "%")



```

  ## 5.5 RandomForest 
  
```{r}
set.seed(342)
control_train <- trainControl(method = "repeatedcv",
                              number = 10, 
                              repeats = 5)

hiperparametros <- expand.grid(mtry = 7, 
                    min.node.size = 20, 
                    splitrule = "gini")



set.seed(342)
modelo_rf <- train(Target~ .,
                   data = datos_train_prep,
                   method = "ranger",  
                   metric = "Accuracy", 
                   tuneGrid = hiperparametros,
                   trControl = control_train, 
                   num.trees = 100) 
modelo_rf

summary(modelo_rf$finalModel)





#predicion
set.seed(342)
predicciones_rf<- predict(modelo_rf,
                               newdata = datos_test_prep, 
                               type = "raw") 

confusionMatrix(data = predicciones_rf,
                reference = datos_test_prep$Target)

#Error de test 
error_test <- mean(predicciones_rf != datos_test_prep$Target)
paste("El error de test del modelo:", round(error_test*100, 2), "%")



```

  
  ## 5.6 Gradient Boosting
  

```{r}
set.seed(342)
control_train <- trainControl(method = "repeatedcv",
                              number = 10, 
                              repeats = 5)


hiperparametros <- expand.grid(interaction.depth = 2, 
                               n.trees = 200, 
                               shrinkage = 0.1, 
                               n.minobsinnode = 15)





set.seed(342)


modelo_boost <- train(Target ~ ., 
                      data = datos_train_prep, method = "gbm",
                      tuneGrid = hiperparametros, 
                      metric = "Accuracy",
                      trControl = control_train,
                      # Número de árboles ajustados distribution = "adaboost",
                      verbose = FALSE) 
modelo_boost





#predicion
set.seed(342)
predicciones_boost<- predict(modelo_boost,
                          newdata = datos_test_prep, 
                          type = "raw") 


confusionMatrix(data = predicciones_boost,
                reference = datos_test_prep$Target)

#Error de test 
error_test <- mean(predicciones_boost != datos_test_prep$Target)
paste("El error de test del modelo:", round(error_test*100, 2), "%")


```

  ## 5.7 SVM 
  
```{r}
set.seed(342)
control_train <- trainControl(method = "repeatedcv",
                              number = 10, 
                              repeats = 5)
# Hiperparámetros 

hiperparametros <- expand.grid(sigma =  0.01,
                               C = 100)


set.seed(342)

modelo_svmrad <- train(Target ~ .,
                       data = datos_train_prep, 
                       method = "svmRadial",
                       tuneGrid = hiperparametros,
                       metric = "Accuracy", 
                       trControl = control_train) 
modelo_svmrad





#predicion
set.seed(342)
predicciones_svmrad<- predict(modelo_svmrad,
                             newdata = datos_test_prep, 
                             type = "raw") 

confusionMatrix(data = predicciones_svmrad,
                reference = datos_test_prep$Target)

#Error de test 
error_test <- mean(predicciones_svmrad != datos_test_prep$Target)
paste("El error de test del modelo:", round(error_test*100, 2), "%")


```

  ## 5.9 Redes neuronales (NNET) 
  
```{r}
# Hiperparámetros 

set.seed(342)

control_train <- trainControl(method = "repeatedcv",
                              number = 10, 
                              repeats = 5)


hiperparametros <- expand.grid(size = 80,
                               decay = 0.1)




set.seed(342) 
modelo_nnet <- train(Target~ ., data = datos_train_prep, 
                     method = "nnet", 
                     tuneGrid = hiperparametros,
                     metric = "Accuracy",
                     trControl = control_train,
                     # Rango de inicialización de los pesos 
                     rang = c(-0.7, 0.7),
                     # Se aumenta el número máximo de pesos 
                     MaxNWts = 2000, 
                     # Para que no se muestre cada iteración por pantalla 
                     trace = FALSE) 

modelo_nnet
                     




#predicion
set.seed(342)
predicciones_nnet<- predict(modelo_nnet,
                              newdata = datos_test_prep, 
                              type = "raw") 

#Error de test

confusionMatrix(data = predicciones_nnet,
                reference = datos_test_prep$Target)

#Error de test 
error_test <- mean(predicciones_nnet != datos_test_prep$Target)
paste("El error de test del modelo:", round(error_test*100, 2), "%")

```

# 6 Comparación de modelos 
  Una vez que se han entrenado y optimizado distintos modelos, se tiene que identificar cuál de ellos consigue mejores resultados para el problema en cuestión, en este caso,     predecir la supervivencia de los pasajeros. Con los datos disponibles, existen dos formas de comparar los modelos. Si bien las dos no tienen por qué dar los mismos     
  resultados, son complementarias a la hora de tomar una decisión final.


```{r}

modelos <- list(KNN = modelo_knn,
                NB = modelo_nb, 
                logistic = modelo_logistic, 
                arbol = modelo_C50Tree,
                rf = modelo_rf, 
                boosting = modelo_boost,
                SVMradial = modelo_svmrad,
                NNET = modelo_nnet)

resultados_resamples <- resamples(modelos) 



metricas_resamples <- resultados_resamples$values %>%
  gather(key = "modelo", value = "valor", -Resample) %>% 
  separate(col = "modelo", into = c("modelo", "metrica"), 
           sep = "~", remove = TRUE) 


metricas_resamples %>% 
  group_by(modelo, metrica) %>% 
  summarise(media = mean(valor)) %>% 
  spread(key = metrica, value = media) %>% 
  arrange(desc(Accuracy))



metricas_resamples %>% 
  filter(metrica == "Accuracy") %>% 
  group_by(modelo) %>% 
  summarise(media = mean(valor)) %>% 
  ggplot(aes(x = reorder(modelo, media),
             y = media, label = round(media, 2))) + 
  geom_segment(aes(x = reorder(modelo, media), 
                   y = 0, xend = modelo, yend = media),
               color = "grey50") + 
  geom_point(size = 7, color = "firebrick") + 
  geom_text(color = "white", size = 2.5) + 
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal geom_hline(yintercept = 0.62, linetype = "dashed") + 
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media", x = "Modelo") +
  coord_flip() + theme_bw()


```

# 7 Error de test

Aunque está demostrado que los métodos de validación tipo CV, bootstrapping, LOOCV… consiguen estimaciones muy buenas del error que comente un modelo, es conveniente hacer una medición final con nuevas observaciones para asegurar que, durante la optimización, no se haya generado overfitting. Esta es la razón por la que, al inicio de un análisis, se separa un conjunto de test que se mantiene aislado de todo el proceso de transformaciones, entrenamiento y optimización.

Tal y como se describió anteriormente, si se desea obtener predicciones para varios modelos, es conveniente emplear la función extractPrediction(). Esta función devuelve un dataframe con las predicciones de cada uno de los modelos, tanto para las observaciones de entrenamiento como para las de test. Además, muestra el verdadero valor de cada observación.



```{r}

predicciones <- extractPrediction( models = modelos, 
                                   testX = datos_test_prep[, -2], 
                                   testY = datos_test_prep$Target)

predicciones %>% head()



metricas_predicciones <- predicciones %>% 
  mutate(acierto = ifelse(obs == pred, TRUE, FALSE)) %>%
  group_by(object, dataType) %>% summarise(accuracy = mean(acierto))

metricas_predicciones %>%
  spread(key = dataType, value = accuracy) %>% 
  arrange(desc(Test))



ggplot(data = metricas_predicciones, 
       aes(x = reorder(object, accuracy),
           y = accuracy, color = dataType,
           label = round(accuracy, 2))) + geom_point(size = 8) + 
  scale_color_manual(values = c("orangered2", "gray50")) + 
  geom_text(color = "white", size = 3) + 
 # geom_hline(yintercept = 0.85, linetype = "dashed") + 
  annotate(geom = "text", y = 0.86, x = 8.5, label = "") +
  coord_flip() +
  labs(title = "Accuracy de entrenamiento y test", x = "modelo") + 
  theme_bw() + theme(legend.position = "bottom")


```






