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

  Evaluar la capacidad predictiva de un modelo consiste en comprobar cómo de próximas son sus predicciones a los verdaderos valores de la variable respuesta. 
  Para poder cuantificar de forma correcta este error, se necesita disponer de un conjunto de observaciones, de las que se conozca la variable respuesta, pero que el modelo no   haya  “visto”, es decir, que no hayan participado en su ajuste. C

# 4 Preprocesado de los datos 
  El preprocesado de datos engloba aquellas transformaciones de los datos hechas con la finalidad de que puedan ser aceptados por el algoritmo de machine learning o que mejoren   sus resultados. Todo preprocesado de datos debe aprenderse de las observaciones de entrenamiento y luego aplicarse al conjunto de entrenamiento y al de test.
  
  ## Variables con varianza próxima a cero
  No se deben incluir en el modelo predictores que contengan un único valor (cero-varianza) ya que no aportan información. Tampoco es conveniente incluir predictores que tengan   una varianza próxima a cero, es decir, predictores que toman solo unos pocos valores, de los cuales, algunos aparecen con muy poca frecuencia.
  
  ## Estandarización y escalado 
  Cuando los predictores son numéricos, la escala en la que se miden, así como la magnitud de su varianza pueden influir en gran medida en el modelo. Muchos algoritmos de 
  machine learning (SVM, redes neuronales, lasso…) son sensibles a esto, de forma que, si no se igualan de alguna forma los predictores, aquellos que se midan en una escala 
  mayor o que tengan más varianza, dominarán el modelo aunque no sean los que más relación tienen con la variable respuesta. Existen principalmente 2 estrategias para evitarlo:

  Centrado: consiste en restarle a cada valor la media del predictor al que pertenece. Si los datos están almacenados en un dataframe, el centrado se consigue restándole a cada 
  valor la media de la columna en la que se encuentra. Como resultado de esta transformación, todos los predictores pasan a tener una media de cero, es decir, los valores se 
  centran en torno al origen.

  Normalización (estandarización): consiste en transformar los datos de forma que todos los predictores estén aproximadamente en la misma escala. Hay dos formas de lograrlo.
  
  ## Binarización de variables cualitativas
  La binarización consiste en crear nuevas variables dummy con cada uno de los niveles de las variables cualitativas. A este proceso también se le conoce como one hot encoding.
  
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
  Una vez que se han entrenado y optimizado distintos modelos, se tiene que identificar cuál de ellos consigue mejores resultados para el problema en cuestión, en este caso,     predecir la supervivencia de los pasajeros. Con los datos disponibles, existen dos formas de comparar los modelos. Si bien las dos no tienen por qué dar los mismos     
  resultados, son complementarias a la hora de tomar una decisión final.

# 7 Error de test

Aunque está demostrado que los métodos de validación tipo CV, bootstrapping, LOOCV… consiguen estimaciones muy buenas del error que comente un modelo, es conveniente hacer una medición final con nuevas observaciones para asegurar que, durante la optimización, no se haya generado overfitting. Esta es la razón por la que, al inicio de un análisis, se separa un conjunto de test que se mantiene aislado de todo el proceso de transformaciones, entrenamiento y optimización.

Tal y como se describió anteriormente, si se desea obtener predicciones para varios modelos, es conveniente emplear la función extractPrediction(). Esta función devuelve un dataframe con las predicciones de cada uno de los modelos, tanto para las observaciones de entrenamiento como para las de test. Además, muestra el verdadero valor de cada observación.







