



# Análisis exploratorio de los datos


```{r}
data <- read.delim("F:/mrain/data.txt", header=T,encoding = 'UTF-8')


data$Target=factor(data$Target, levels = c(0,1),
                   labels = c("No","Si"))

set.seed(345)


library(caret)
library(tidyverse)

data$Target=as.factor(data$Target)
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


### Distribución de variables respuesta

```{r}
ggplot(data = data, aes(x = Target, y = ..count.., fill = Target)) +
  geom_bar() + scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Target") + theme_bw() +
  theme(legend.position = "bottom")
prop.table(table(data$Target)) %>% round(digits = 2)

```


```{r}
n_observaciones <- nrow(data) 
predicciones <- rep(x = "No", n_observaciones)
mean(predicciones == data$Target) * 100

```


### Distribución de variables continuas



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


### Distribución de variables cualitativas


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

## Importancia de las variables




### Correlación entre variables continuas



```{r}
cor.test(x = data$Edad, y = data$Ingresos_Cliente, method = "pearson")
```

```{r}
ggplot(data = data, aes(x = Edad, y = log(Ingresos_Cliente))) + geom_point(color = "gray30") + geom_smooth(color = "firebrick") + theme_bw()
```



### Contraste de proporciones


Para la identificación de potenciales predictores cualitativos, es interesante encontrar las variables y niveles de las mismas que muestran una proporción de supervivientes alejada de lo esperado por el nivel basal, en este caso el 13%. Este porcentaje se corresponde con la proporción de Target respecto al total de lso clientes, es decir, el valor esperado si no existiese relación entre la variable y la supervivencia. Estas diferencias no siempre son fáciles de apreciar en una gráfica, sobre todo, cuando el número de observaciones es distinto en cada grupo.


Para facilitar este tipo de análisis, resulta útil crear variables dummy con todos los niveles de las variables cualitativas (proceso conocido como binarización o one hot encoding) y aplicar un test de contraste de proporciones.





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


 Representación gráfica de la distribución de los 6 grupos con menor p-value

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


### Random forest


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






# División de los datos en entrenamiento y test


```{r}
set.seed(342)
train <- createDataPartition(y = data$Target, 
                             p = 0.8, list = FALSE, times = 1)
datos_train <- data[train, ] 
datos_test <- data[-train, ]
```

```{r}
prop.table(table(datos_train$Target))
prop.table(table(datos_test$Target))
```



### Preprocesado de los datos


```{r}
library(recipes)


objeto_recipe <- recipe(formula = Target ~Ciudad+Estado_civil+Edad+
                        Contrato+Nivel_Academico, data = datos_train) 
objeto_recipe


```


### Variables con varianza próxima a cero



```{r}
data %>% select(Ciudad,Estado_civil,Edad,Contrato,Nivel_Academico) %>% 
  nearZeroVar(saveMetrics = TRUE)
```

### Estandarización y escalado

```{r}
objeto_recipe <- objeto_recipe %>% step_center(all_numeric()) 
objeto_recipe <- objeto_recipe %>% step_scale(all_numeric())

```

### Binarización de variables cualitativas



```{r}

objeto_recipe <- objeto_recipe %>% step_dummy(all_nominal(), -all_outcomes())

```


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




# Modelos


#K-Nearest Neighbor (kNN)

# k: número de observaciones vecinas empleadas.

set.seed(342)
control_train <- trainControl(method = "repeatedcv",
                              number = 10, 
                              repeats = 5)

hiperparametros <- data.frame(k = c(1, 2, 5, 10, 15, 20, 30, 50))




set.seed(342)

modelo_knn <- train(Target~ ., data = datos_train_prep,
                    method = "knn",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    trControl = control_train) 
modelo_knn


ggplot(modelo_knn, highlight = TRUE) +
  scale_x_continuous(breaks = hiperparametros$k) +
  labs(title = "Evolución del accuracy del modelo KNN", x = "K") + 
  theme_bw()


# Naive Bayes


# usekernel: TRUE para emplear un kernel que estime la densidad o 
# FALSE para asumir una distribución de densidad gaussiana. 

# fL: factor de corrección de Laplace, 0 para no aplicar ninguna 
# corrección. 

# adjust: parámetro pasado a la función density si usekernel = TRUE.


set.seed(342)
control_train <- trainControl(method = "repeatedcv",
                              number = 10, 
                              repeats = 5) 


hiperparametros <- data.frame(usekernel = TRUE, fL = 0 ,
                              adjust= c(1,3,5))


set.seed(342) 

modelo_nb <- train(Target ~ ., data = datos_train_prep,
                   method = "nb",
                   metric = "Accuracy",
                   tuneGrid = hiperparametros,
                   trControl = control_train)
modelo_nb

ggplot(modelo_nb, highlight = TRUE) +
  scale_x_continuous(breaks = hiperparametros$adjust) +
  labs(title = "Evolución del accuracy del modelo Nb",
       x = "adjust") + 
  theme_bw()





# Regresión logística



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




# Árbol de clasificación simple

set.seed(342)
control_train <- trainControl(method = "repeatedcv",
                              number = 10, 
                              repeats = 5)

hiperparametros <- data.frame(parameter = "none")


set.seed(342)
modelo_C50Tree <- train(Target ~ ., data = datos_train_prep,
                        method = "C5.0Tree",
                        tuneGrid = hiperparametros, 
                        metric = "Accuracy",
                        trControl = control_train) 
modelo_C50Tree

summary(modelo_C50Tree$finalModel)



# RandomForest


# mtry: número predictores seleccionados aleatoriamente en cada árbol. 
# min.node.size: tamaño mínimo que tiene que tener un nodo para poder 
# ser dividido. 

#splitrule: criterio de división.
set.seed(342)
control_train <- trainControl(method = "repeatedcv",
                              number = 10, 
                              repeats = 5)

hiperparametros <- expand.grid(mtry = c(3, 4, 5, 7), 
                               min.node.size = c(2, 3, 4, 5, 10, 15, 20, 30), 
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


modelo_rf$finalModel

ggplot(modelo_rf, highlight = TRUE) +
  scale_x_continuous(breaks = 1:30) +
  labs(title = "Evolución del accuracy del modelo Random Forest") +
  guides(color = guide_legend(title = "mtry"), 
         shape = guide_legend(title = "mtry")) +
  theme_bw()






# Gradient Boosting


#n.trees: número de iteraciones del algoritmo de boosting, es decir, 
# número de modelos que forman el ensemble. Cuanto mayor es este valor, 
# más se reduce el error de entrenamiento, pudiendo llegar generarse 
#overfitting.


# interaction.depth: complejidad de los árboles empleados como weak 
# learner, en concreto, el número total de divisiones que tiene el árbol.
#Emplear árboles con ente 1 y 6 nodos suele dar buenos resultados.


#shrinkage: este parámetro, también conocido como learning rate, 
# controla la influencia que tiene cada modelo sobre el conjunto 
#del ensemble.


# n.minobsinnode: número mínimo de observaciones que debe tener un 
# nodo para poder ser dividido. Al igual que interaction.depth, 
# permite controlar la complejidad de los weak learners basados en árboles.



# Hiperparámetros 

set.seed(342)

control_train <- trainControl(method = "repeatedcv",
                              number = 10, 
                              repeats = 5)

hiperparametros <- expand.grid(interaction.depth = c(1, 2), 
                               n.trees = c(50, 100, 200), 
                               shrinkage = c(0.001, 0.01, 0.1), 
                               n.minobsinnode = c(2, 5, 15))



set.seed(342)


modelo_boost <- train(Target ~ ., 
                      data = datos_train_prep, method = "gbm",
                      tuneGrid = hiperparametros, 
                      metric = "Accuracy",
                      trControl = control_train,
                      # Número de árboles ajustados distribution = "adaboost",
                      verbose = FALSE) 
modelo_boost



ggplot(modelo_boost, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo Gradient Boosting") +
  guides(color = guide_legend(title = "shrinkage"),
         shape = guide_legend(title = "shrinkage")) +
  theme_bw() + 
  theme(legend.position = "bottom")


# SVM


# sigma: coeficiente del kernel radial. 
#C: penalización por violaciones del margen del hiperplano.
set.seed(342)
control_train <- trainControl(method = "repeatedcv",
                              number = 10, 
                              repeats = 5)

# Hiperparámetros 

hiperparametros <- expand.grid(sigma = c(0.001, 0.01, 0.1, 0.5, 1),
                               C = c(1 , 20, 50, 100, 200, 500, 700))


set.seed(342)

modelo_svmrad <- train(Target ~ .,
                       data = datos_train_prep, 
                       method = "svmRadial",
                       tuneGrid = hiperparametros,
                       metric = "Accuracy", 
                       trControl = control_train) 
modelo_svmrad



ggplot(modelo_svmrad, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo SVM Radial") +
  theme_bw()




# Redes neuronales (NNET)


#size: número de neuronas en la capa oculta. 
#decay: controla la regularización durante el entrenamiento de la red.


# Hiperparámetros 

set.seed(342)
control_train <- trainControl(method = "repeatedcv",
                              number = 10, 
                              repeats = 5)


hiperparametros <- expand.grid(size = c(10, 20, 50, 80, 100),
                               decay = c(0.0001, 0.1, 0.5))




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


ggplot(modelo_nnet, highlight = TRUE) + 
  labs(title = "Evolución del accuracy del modelo NNET") + theme_bw()



## Comparación de modelos


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




#Error de test

set.seed(342) 

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






