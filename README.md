# Multivariate Forecasting using ARIMA, LM & ARIMAX on Pollution Data using R

#### Author: Francisco Bahamondes

## Motivación
Cada cierto tiempo las autoridades se ven con la obligación de decretar emergencia ambiental al presentarse casos de intoxicaciones masivas en la zona a causa de la presencia de contaminantes como el material particulado 2.5 (Pm 2.5) o dióxido de azufre (SO2). La motivación del presente estudio nace de mejorar las condiciones en las que viven las personas, en especial niños, de la zona de sacrificio Quintero-Puchuncaví. Este último grupo de habitantes de esta zona son quienes más sufren a causa de la inhalación de estos gases. Irritación en los ojos, dificultades para respirar, quemaduras en la nariz y garganta, dolor punzante son algunos de los síntomas a corto plazo, mientras que la exposición a largo plazo puede desencadenar en cuadros de bronquitis, pérdida de olfato y cambios en la función pulmonar.

## Contexto
A fines de los años 50’s, la zona Quintero-Puchuncaví se consolida como zona industrial a escala regional y nacional, generando consecuencias negativas en el medio ambiente y en la salud de sus habitantes. Debido a los procesos productivos de la industria del complejo Ventanas ubicado en esta zona, sumado a las características geográficas y meteorológicas del lugar, se favorece la formación y acumulación de contaminantes. El año 1993 debido a la alta presencia de dióxido de azufre (SO2), anhídrido sulfuroso (O2) y material particulado (PM10) por el emplazamiento del Complejo Industrial Ventanas, la zona se convirtió en la primera zona saturada del país (INDH, 2015), enfrentándola a constantes conflictos sociales y ambientales.

![complejo ventanas](complejo_industrial_ventanas.jpeg)

## Solución
Como solución planteamos un modelo de predicción que pueda realizar un pronóstico acabado de las emisiones considerando las variables medioambientales que las influyen. De esta forma, se podrá tener una visión futura del comportamiento de la emisión de estos gases y detectar con anticipación en qué días los niveles podrían alcanzar los límites de alarma y finalmente poder decretar medidas preventivas con mayor antelación.

## Datos
Los datos que utilizamos para nuestro estudio fueron obtenidos del sitio oficial del Sistema de Información Nacional de Calidad del Aire (SINCA, https://sinca.mma.gob.cl/index.php/), del Ministerio del Medioambiente. El conjunto de datos que consolidamos corresponden a 2 parámetros de contaminantes (Pm 2.5 y SO2) y 6 parámetros meteorológicos (temperatura, humedad relativa, radiación global, presión atmosférica, velocidad y dirección del viento). El período de tiempo de este conjunto corresponde a una serie diaria y se comprende desde inicios de enero de 2017 hasta fines de junio de 2022.

## ¿Cómo lo estamos haciendo?
El conjunto de datos presentaba filas que no contaban con un registro asignado, esto puede deberse a fallas en los sensores de medición o la mantención de estos mismos. Por este motivo, lo primero que hicimos fue completar estos registros faltantes con una técnica de imputación. En segundo lugar, acotamos nuestro conjunto de datos seleccionando las variables que contribuían más a nuestro estudio. Esto se logró mediante ánalisis de las componentes principales. Como resultado, se excluyó la presión atmosférica ya que demostró no tener un mayor aporte a la componente principal. Luego, se hizo un ánalisis de correlación entre las variables para poder comprender mejor cuáles influenciaban más a cada contaminante. Una vez que preparamos y entendimos nuestros datos hicimos 3 modelos de predicción y obtuvimos resultados.

## Modelos
El pronostico que se hizo fue de junio de 2022, y para conseguir esto tuvimos que construir 3 modelos de predicción. En primer lugar, separamos nuestros datos en un set de entrenamiento y en un set de testing o validación. El set de entrenamiento comprende los datos de inicios de enero de 2017 hasta fines de mayo de 2022. Mientras que los de validación comprenden el mes de junio de 2022.

El primer modelo fue ARIMA, este modelo consideró solamente una serie de tiempo univariada, es decir, una vez construido, hará pronósticos basados solamente en el pasado de un contaminante, sin considerar las variables meteorológicas que lo influyen. El segundo modelo fue Linear Regression (o regresión lineal). Este modelo consideró tanto el contaminante como las variables meteorológicas. Y por último, ARIMAX. Este modelo, al igual que la regresión lineal, también considera las variables meteorológicas, pero además incorpora funciones propias para ajustar un mejor modelo. 

## Resultados
### Pm 2.5
| Modelo        | MSE           | MAE   | RMSE  | 
| ------------- |:-------------:| -----:| -----:|
| ARIMA         | 28,222        | 4,094 | 5,312 |
| LM            | 32,086        | 4,300 | 5,664 |
| ARIMAX        | 23,775        | 3,717 | 4,876 |

![pm25](pm25.png)

### SO2
| Modelo        | MSE           | MAE   | RMSE  | 
| ------------- |:-------------:| -----:| -----:|
| ARIMA         | 68,6248       | 5,457 | 8,284 |
| LM            | 71,423        | 5,678 | 8,451 |
| ARIMAX        | 59,057        | 5,187 | 7,684 |

![so2](so2.png)

## Conclusión
Como conclusión obtuvimos que los modelos que consideran las variables meteorológicas (Linear Regression y ARIMAX) pueden lograr marcar una tendencia del comportamiento, sin embargo no son capaces de predecir con exactitud el valor actual del contaminante. Mientras que el modelo ARIMA no es capaz de realizar una buena predicción. Conocer la tendencia del comportamiento a corto plazo puede facilitar la visión a futuro de la presencia de estos contaminantes y de esta forma se puede tener una noción de qué días se debería ejecutar una medida preventiva.

Por otra parte, sería conveniente poder tener mayor precisión. Esto lo podemos conseguir con modelos que se ajusten mejor. Por esto concluímos que es conveniente no solo trabajar con los datos diarios sino que también con los datos horarios y ver qué tanto mejoran nuestros modelos.

## Trabajo Futuro
Otro set de datos al que tenemos acceso y es de importancia son las hospitalizaciones de los distintos centros de atención de la zona de sacrificio. Como trabajo futuro se plantea la estimación de hospitalizaciones en base al pronóstico de los contaminantes realizados en este estudio, y de esta forma poder ser un aporte para la planificación preventiva.





