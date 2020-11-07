# Machine Learning con R y caret

# 1. Introducción
Durante los últimos años, el interés y la aplicación de machine learning ha experimentado tal expansión, que se ha convertido en una disciplina aplicada en prácticamente todos los ámbitos de investigación académica e industrial. El creciente número de personas dedicadas a esta disciplina ha dado como resultado todo un repertorio de herramientas con las que, perfiles con especialización media, consiguen acceder a métodos predictivos potentes. El lenguaje de programación R es un ejemplo de ello.

# 2 Análisis exploratorio de los datos 
Antes de entrenar un modelo predictivo, o incluso antes de realizar cualquier cálculo con un nuevo conjunto de datos, es muy importante realizar una exploración descriptiva de los mismos. Este proceso permite entender mejor que información contiene cada variable, así como detectar posibles errores.

  ## Distribución de variables respuesta 
  Cuando se crea un modelo, es muy importante estudiar la distribución de la variable respuesta, ya que, a fin de cuentas, es lo que nos interesa predecir.
  
  ## Distribución de variables continuas y cualitativas 
  Como el objetivo del estudio es predecir Target si y no, el análisis de cada variable se hace en relación a la variable respuesta Target.          
  Analizando los datos de esta forma, se pueden empezar a extraer ideas sobre qué variables están más relacionadas con la Target.
  
  ## Importancia de las variables 
  La representación gráfica de la distribución de las variables en función de si los pasajeros sobrevivieron o no, ayuda a tener una idea de qué variables pueden ser buenos 
  predictores para el modelo y cuales no aportan información o la que aportan es redundante.
  
  ## Contraste de proporciones 
  Para la identificación de potenciales predictores cualitativos, es interesante encontrar las variables y niveles de las mismas que muestran una proporción de Target se aleja 
  de lo esperado por el nivel basal, en este caso el 13%
 
  ## Random forest 
  Otra estrategia ampliamente extendida para estudiar la importancia de variables es el empleo de Random Forest. El problema de aplicarlo a este ejemplo es que no acepta 
  valores ausentes, tema que todavía no se ha tratado (visto más adelante).
  
# 3 División de los datos en entrenamiento y test 

  Evaluar la capacidad predictiva de un modelo consiste en comprobar cómo de próximas son sus predicciones a los verdaderos valores de la variable respuesta. Para poder      
  cuantificar de forma correcta este error, se necesita disponer de un conjunto de observaciones, de las que se conozca la variable respuesta, pero que el modelo no haya 
  “visto”, es decir, que no hayan participado en su ajuste. C

# 4 Preprocesado de los datos 
  ## Variables con varianza próxima a cero
  ## Estandarización y escalado 
  ## Binarización de variables cualitativas
# 5 Modelos 
  ## K-Nearest Neighbor (kNN)
  ## Naive Bayes 
  ## Regresión logística 
  ## Árbol de clasificación simple 
  ## RandomForest 
  ## Gradient Boosting
  ## SVM 
  ## Redes neuronales (NNET) 
# 6 Comparación de modelos 
# 7 Métricas de validación
# 8 Error de test
