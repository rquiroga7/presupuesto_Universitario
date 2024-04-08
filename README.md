Análisis de ejecución presupuestaria de las Universidades Nacionales de Argentina
Última actualización 08/04/2024
Dr. Rodrigo Quiroga
Investigador Asistente INFIQC-CONICET
Profesor Adjunto de Bioinformática y Biología Computacional, Facultad de Ciencias Químicas, Universidad Nacional de Córdoba

INTRODUCCIÓN
============

Ante la decisión del gobierno de Javier Milei de no enviar una ley de presupuesto para 2024, se recondujo el presupuesto 2023 ([Decreto 23/2024](https://www.boletinoficial.gob.ar/detalleAviso/primera/301615/20240105)). Debido a la alta inflación que se observa en el país desde principios de 2023, con un gran salto a fines del 2023 relacionado a la decisión de devaluar el peso más del 50% el 12 de diciembre (el precio del dólar oficial saltó de 367 a 800 pesos, ver [aquí](https://elpais.com/argentina/2023-12-12/milei-anuncia-una-devaluacion-del-peso-del-50-y-grandes-recortes-del-gasto-publico.html)), el presupuesto 2024 (con montos similares a los de 2023) es obviamente insuficiente para mantener funcionando a las distintas dependencias estatales, en particular esto aplica también para las Universidades Nacionales.

El Ministerio de Economía mantiene una base de datos llamada [Presupuesto Abierto](https://www.presupuestoabierto.gob.ar/sici/) de donde pueden descargarse los datos de ejecución presupuestaria. Utilizando dichos datos, analizamos la ejecución presupuestaria mensual, no de los montos pagados, sino de los montos devengados. Para leer una explicación sobre qué significan estos términos, consultar este [glosario](https://www.presupuestoabierto.gob.ar/sici/glosario-e).

Todo éste análisis se basa en analizar el crédito devengado bajo el programa 26 (Des) del ex Ministerio de Educación y ahora Ministerio de Capital Humano.

Repositorio con código de bash y R que permite descargar, analizar y graficar los datos de ejecución presupuestaria de 2017-2024. Los datos se descargan de la API de Presupuesto abierto, pero ya están disponibles en el repositorio, no es necesario volver a descargarlos. El script API_datos.R analiza y genera los gráficos de ejecución presupuestaria mensual, y los scripts UNC_2015-2024.R y 2015_2024.R generan los gráficos anuales, para la UNC y para la totalidad de las Universidades Nacionales, respectivamente. 

Aquí la ejecución mensual para presupuesto total (todas las actividades) y la actividad 14 (funcionamiento).

![plot](https://github.com/rquiroga7/presupuesto_UNC/blob/main/plot_14_70p_prom.png)
![plot](https://github.com/rquiroga7/presupuesto_UNC/blob/main/plot_all_70p_prom.png)

Aquí los gráficos de ejecución anual, suponiendo que para 2024 el crédito devengado se ajustará por IPC desde abril en adelante.
Presupuesto de funcionamiento (act 14):
![plot](https://github.com/rquiroga7/presupuesto_UNC/blob/main/presupuesto_anual_func_2017-2024.png)

Presupuesto total:
![plot](https://github.com/rquiroga7/presupuesto_UNC/blob/main/presupuesto_anual_2017-2024.png)
