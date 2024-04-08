Análisis de ejecución presupuestaria de las Universidades Nacionales de Argentina
Última actualización 08/04/2024
Dr. Rodrigo Quiroga
Investigador Asistente INFIQC-CONICET
Profesor Adjunto de Bioinformática y Biología Computacional, Facultad de Ciencias Químicas, Universidad Nacional de Córdoba

INTRODUCCIÓN Y METODOLOGÍA
============
<details>
<summary>Expandir para leer la sección de introducción y metodología</summary>
Ante la decisión del gobierno de Javier Milei de no enviar una ley de presupuesto para 2024, se recondujo el presupuesto 2023 ([Decreto 23/2024](https://www.boletinoficial.gob.ar/detalleAviso/primera/301615/20240105)). Debido a la alta inflación que se observa en el país desde principios de 2023, con un gran salto a fines del 2023 relacionado a la decisión de devaluar el peso más del 50% el 12 de diciembre (el precio del dólar oficial saltó de 367 a 800 pesos, ver [aquí](https://elpais.com/argentina/2023-12-12/milei-anuncia-una-devaluacion-del-peso-del-50-y-grandes-recortes-del-gasto-publico.html)), el presupuesto 2024 (con montos similares a los de 2023) es obviamente insuficiente para mantener funcionando a las distintas dependencias estatales, en particular esto aplica también para las Universidades Nacionales.

El Ministerio de Economía mantiene una base de datos llamada [Presupuesto Abierto](https://www.presupuestoabierto.gob.ar/sici/) de donde pueden descargarse los datos de ejecución presupuestaria. Utilizando dichos datos, analizamos la ejecución presupuestaria mensual, no de los montos pagados, sino de los montos devengados. Para leer una explicación sobre qué significan estos términos, consultar este [glosario](https://www.presupuestoabierto.gob.ar/sici/glosario-e).

Todo éste análisis se basa en analizar el crédito devengado bajo el programa 26 (DESARROLLO DE LA EDUCACIÓN SUPERIOR) del ex Ministerio de Educación y ahora Ministerio de Capital Humano. Dentro de este programa, se encuentran distintas actividades que podemos resumir en la siguiente lista:
- #actividad_id==1 - Conduccion, Gestion y Apoyo a las Politicas de Educacion Superior
- #actividad_id==11 Fundar
- #actividad_id==12 Salarios Docentes
- #actividad_id==13 Salarios No-Docentes
- #actividad_id==14 Asistencia Financiera para el Funcionamiento Universitarios
- #actividad_id==15 Salud (Hospitales Universitarios)
- #actividad_id==16 CyT
- #actividad_id==23 Desarrollo de Institutos Tecnologicos de Formacion Profesional
- #actividad_id==24 Promoción de carreras estratégicas
- #actividad_id==25 Extensión Universitaria

Como metodología, en general vamos a mostrar gráficos de ejecución presupuestaria (crédito devengado) en pesos reales, es decir ajustado por inflación. Esto permite una comparación más realista de los presupuestos de cada mes, dado que los montos se ajustan por IPC para estimar cómo permite afrontar los costos que ese presupuesto está destinado a afrontar. Adicionalmente, cabe aclarar que los montos en pesos se expresarán en millones de pesos equivalentes a los del último mes analizado. Por lo tanto, los montos devengados coinciden para el último mes con los datos que uno puede encontrar en la página de presupuesto abierto, pero para meses anteriores, no habrá coincidencias dado que la página muestra montos nominales.

El código de bash y R utilizado para descargar, analizar y graficar los datos de ejecución presupuestaria de 2017-2024 están disponibles abiertamente en este repositorio. Los datos se descargan de la API de Presupuesto abierto (aunque no es necesario que el usuario los descargue ya que están disponibles en este repositorio). El script API_datos.R analiza y genera los gráficos de ejecución presupuestaria mensual, y los scripts UNC_2015-2024.R y 2015_2024.R generan los gráficos anuales, para la UNC y para la totalidad de las Universidades Nacionales, respectivamente. 
</details>


Aquí la ejecución mensual para presupuesto total (todas las actividades) y la actividad 14 (funcionamiento).

![plot](https://github.com/rquiroga7/presupuesto_UNC/blob/main/plot_14_70p_prom.png)
![plot](https://github.com/rquiroga7/presupuesto_UNC/blob/main/plot_all_70p_prom.png)

Aquí los gráficos de ejecución anual, suponiendo que para 2024 el crédito devengado se ajustará por IPC desde abril en adelante.
Presupuesto de funcionamiento (act 14):
![plot](https://github.com/rquiroga7/presupuesto_UNC/blob/main/presupuesto_anual_func_2017-2024.png)

Presupuesto total:
![plot](https://github.com/rquiroga7/presupuesto_UNC/blob/main/presupuesto_anual_2017-2024.png)
