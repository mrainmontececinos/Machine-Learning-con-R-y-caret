

data <- read.delim("F:/sergio/taller 3/data.txt", header=T,encoding = 'UTF-8')

set.seed(345)
data2= sample(1:nrow(data),size=2000,replace=FALSE)

data<- data[data2, ]


library(caret)
library(tidyverse)


data$Target=factor(data$Target, levels = c(0,1),
                   labels = c("No","Si"))


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


# Número de datos ausentes por variable
map_dbl(data, .f = function(x){sum(is.na(x))})





ggplot(data = data, aes(x = Target, y = ..count.., fill = Target)) +
  geom_bar() + scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Supervivencia") + theme_bw() +
  theme(legend.position = "bottom")


prop.table(table(data$Target)) %>% round(digits = 2)




# Porcentaje de aciertos si se predice para todas las observaciones

n_observaciones <- nrow(data) 
predicciones <- rep(x = "No", n_observaciones)
mean(predicciones == data$Target) * 100






# Importancia de las variables


library(randomForest) 

datos_rf <- data %>% select(-Tipo_documento, -ID, -Fecha_de_Corte, 
                            -Fecha_nacimiento, -Fecha_activación,-Corte_AAAAMM,-Motivo_Retiro)



datos_rf <- map_if(.x = datos_rf, .p = is.character, .f = as.factor) %>% 
  as.data.frame()

modelo_randforest <- randomForest(formula = Target ~ . ,
                                  data =datos_rf,
                                  mtry = 5, importance = TRUE,
                                  ntree = 500) 

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




set.seed(123)
# Se crean los índices de las observaciones de entrenamiento 

train <- createDataPartition(y = data$Target, 
                             p = 0.7, list = FALSE, times = 1)

datos_train <- data[train, ] 
datos_test <- data[-train, ]


prop.table(table(datos_train$Target))

prop.table(table(datos_test$Target))


library(recipes)


objeto_recipe <- recipe(formula = Target ~Ciudad+Estado_civil+Edad+
                          Contrato+Nivel_Academico, data = datos_train) 

objeto_recipe


objeto_recipe <- objeto_recipe %>% step_center(all_numeric()) 
objeto_recipe <- objeto_recipe %>% step_scale(all_numeric())



objeto_recipe <- objeto_recipe %>% step_dummy(all_nominal(), -all_outcomes())


trained_recipe <- prep(objeto_recipe, training = datos_train) 
trained_recipe



# Se aplican las transformaciones al conjunto de entrenamiento y de test 

datos_train_prep <- bake(trained_recipe, new_data = datos_train)
datos_test_prep <- bake(trained_recipe, new_data = datos_test)
glimpse(datos_train_prep)





# Modelos


#K-Nearest Neighbor (kNN)

# k: número de observaciones vecinas empleadas.


control_train <- trainControl(method = "repeatedcv",
                              number = 10, 
                              repeats = 5)

hiperparametros <- data.frame(k = c(1, 2,3,4,5, 10, 15, 20, 30, 50))




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




control_train <- trainControl(method = "repeatedcv",
                              number = 10, 
                              repeats = 5) 


hiperparametros <- data.frame(usekernel = TRUE, fL = 0 ,
                              adjust= c(1,2,3,4,5))


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



set.seed(342)
modelo_C50Tree <- train(Target ~ ., data = datos_train_prep,
                        method = "C5.0Tree",
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
                   num.trees = 500) 
modelo_rf



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
