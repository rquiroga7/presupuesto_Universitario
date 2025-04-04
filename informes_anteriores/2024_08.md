Universidades Nacionales públicas de Argentina: 
Análisis de ejecución presupuestaria 2024 y presupuesto 2025
=================================================================================

Última actualización 20/09/2024

 **Dr. Rodrigo Quiroga**  
 Investigador Asistente INFIQC-CONICET  
 Profesor Adjunto de Bioinformática y Biología Computacional  
 Facultad de Ciencias Químicas, Universidad Nacional de Córdoba

 Repositorio disponible con todo el código utilizado para descargar, analizar y graficar los datos de ejecución presupuestaria de Universidades Nacionales [aquí](https://github.com/rquiroga7/presupuesto_Universitario).
 
Ejecución presupuestaria mensual de 2023 y 2024
============

Presupuesto de funcionamiento
-----------------------------
<div align="justify">
 
Es importante aclarar que las Universidades Nacionales en muchos casos generan fondos propios que se destinan a funcionamiento (en general por prestación de servicios). Sin embargo, en la mayoría de las Universidades el presupuesto enviado por el gobierno nacional para tal fin es determinante.

En el siguiente gráfico observamos el presupuesto mensual real (ajustado por inflación) para funcionamiento de todas las Universidades Nacionales del país. Se grafica también el promedio mensual 2023 como una línea punteada verde. Vemos que como explicamos en la introducción, el presupuesto de funcionamiento se fija en 12 cuotas iguales, por lo que los montos ajustados por inflación están por arriba del promedio anual durante los primeros meses del año y luego caen por debajo. Esto ocurre TODOS LOS AÑOS. Lo que no ocurrió, fue el aumento normal que ocurre con un nuevo presupuesto, que debería haber entrado en vigencia en enero de 2024.

![plot](https://github.com/rquiroga7/presupuesto_Universitario/blob/main/plots/plot_14_70p_prom.png)

Vemos que en Marzo de 2024 y en Mayo (luego de la masiva marcha federal universitaria) se otorgaron aumentos del presupuesto mensual de funcionamiento que equiparan aproximadamente los montos recibidos mensualmente durante 2023.

Sin embargo, vale aclarar que con los [aumentos de tarifas de servicios previstos para 2024](https://www.infobae.com/economia/2024/04/05/tarifas-de-gas-todo-lo-que-hay-que-saber-y-de-cuanto-seran-los-nuevos-aumentos/), y el hecho de que [el gobierno eliminó a las instituciones educativas de la lista de beneficiarias de subsidios de energía eléctrica](https://www.datadiario.com/sociedad/aumentan-las-facturas-de-luz-para-usuarios-de-mayores-ingresos-20242518120), incluso estos aumentos otorgados resultan insuficientes para afrontar los costos de funcionamiento, que aumentaron por encima de la inflación promedio durante 2024.

</div>

Presupuesto total
-----------------

En el siguiente gráfico observamos el presupuesto mensual real total (ajustado por inflación), de todas las Universidades Nacionales del país. Se grafica también el promedio mensual 2023 como una línea punteada verde (calculado sobre los meses sin aguinaldo). Vemos que los presupuestos mensuales devengados (es decir los pagos comprometidos) están muy por debajo de 2023.

![plot](https://github.com/rquiroga7/presupuesto_Universitario/blob/main/plots/plot_all_70p_prom.png)


**Como vemos en el gráfico, los aumentos de gastos de funcionamiento de marzo y mayo (acumulando un 270% aumento interanual) anunciado por el gobierno con bombos y platillos, en realidad representan un aumento del 2.2% del presupuesto total!**


Ejecución presupuestaria anual para el período 2017-2025
============

Para poner en contexto histórico el presupuesto 2024 y también el recientemente presentado presupuesto 2025, comparamos la ejecución presupuestaria 2017-2023 con la ejecución presupuestaria 2024 (asumiendo que el presupuesto se ajustaría por inflación (IPC) a partir de octubre) y el presupuesto 2025, tomando como válida la estimación del gobierno de 18,3% de inflación anual para 2025. Ambas presunciones son muy optimistas, es decir, estamos analizando los presupuestos 2024 y 2025 en el mejor de los casos. 

Presupuesto total:
![plot](https://github.com/rquiroga7/presupuesto_Universitario/blob/main/plots/presupuesto_anual_2017-2025.png)

De no haber significativos aumentos en lo que queda de 2024, el presupuesto universitario del corriente año sería por lejos el menor desde que se comenzaron a recopilar datos de Presupuesto Abierto. El presupuesto ajustado por inflación de 2024 representaría el 73% del presupuesto universitario de 2023. El presupuesto 2025 es aún menor, representando el 63% del presupuesto 2023, y recordando que si la inflación anual es superior al 18,3% que estima el gobierno, el presupuesto real 2025 sería aún menor!


Podemos analizar estos mismos datos con base 100 equivalente al presupuesto de 2017. 

![plot](https://github.com/rquiroga7/presupuesto_Universitario/blob/main/plots/presupuesto_anual_100_2017-2025.png)

Este gráfico es un poco más intuitivo. Lo que observamos es que el presupuesto cayó fuertemente durante 2019, creció levemente desde 2020-2023 y en 2024 observamos la mayor caída de la serie histórica, con una nueva caída en 2025. Para 2025 el presupuesto ajustado por inflación sería un 56% del presupuesto 2017 si la inflación 2025 es del 18,3%, pero podría caer a 51% si la inflación 2025 se acerca al 38% como [estiman algunas consultoras](https://www.infobae.com/economia/2024/09/17/presupuesto-2025-el-nivel-de-inflacion-que-estimo-el-gobierno-es-menos-de-la-mitad-que-las-proyecciones-privadas/).

Sin embargo, esto no alcanza para dimensionar por completo la magnitud de la crisis universitaria, dado que el número de alumnos del sistema universitario nacional ha aumentado considerablemente desde 2017. Realizando el mismo análisis de arriba, pero ajustamos el presupuesto por inflación, pero también por el número total de alumnos universitarios, nuevamente tomando el presupuesto por alumno del año 2017 como base igual a 100.

![plot](https://github.com/rquiroga7/presupuesto_Universitario/blob/main/plots/presupuesto_anual_porest_100_2017-2025.png)

Este gráfico permite dimensionar la crisis presupuestaria del sistema universitario en toda su magnitud. Observamos que por cada 100 pesos de poder adquisitivo por estudiante que recibían las universidades públicas en 2017, en 2024 se recibirán 47, es decir, menos de la mitad. En 2025, ese valor cae a 41 si la inflación anual 2025 es de 18,3%, o a 37 si la inflación 2025 es de 38%. Es decir, en 2025 las Universidades podrían llegar a recibir aproximadamente 1/3 del presupuesto por alumno que recibían en 2017.


Conclusiones
============

**La situación presupuestaria de las Universidades es crítica.** Ya se encuentran suspendidos programas valiosísimos de becas, investigación y extensión universitaria, y el presupuesto es tan escueto que muchas universidades están gastando ahorros para poder pagar las tarifas de servicios básicos. Ya entró en cesación de pagos la Universidad Nacional del Comahue, a pesar de que aún no entran en efecto todos los incrementos de tarifas programados por el gobierno para este año 2024. 
**La ley de financiamiento universitario que fue sancionada el 12 de septiembre en el senado corre peligro de ser [vetada por el presidente Milei](https://www.ambito.com/politica/financiamiento-universitario-el-gobierno-insiste-el-veto-pero-toma-nota-advertencias-n6060799). La ley no es una panacea, ya que la situación presupuestaria de las universidades ya era complicada en 2022 y 2023, pero al menos permitiría garantizar la continuidad de las clases durante 2024 y 2025, retrotrayendo la situación presupuestaria y salarial a la de noviembre de 2023.** Esto no es "campaña de miedo" sino un necesario llamado de atención sobre el ajuste *que ya efectuó* el gobierno nacional. El CIN se ha puesto al frente de estos reclamos ante el Ministerio de Capital Humano, pero también creo necesario que comuniquemos ampliamente esta emergencia presupuestaria a docentes, no-docentes y estudiantes. La urgencia de la situación es clara para la mayoría de los rectores, pero no es de amplio conocimiento en la totalidad de la comunidad universitaria. Aún estamos a tiempo de ponernos de pie y defender la Universidad Pública, aunque el tiempo se nos acaba rápidamente y muchas autoridades mantienen un silencio que aturde. 

Es importante también comprender que el ataque del gobierno de Milei al sistema universitario argentino no se justifica desde la mera defensa del "déficit cero", sino desde el punto de vista ideológico y político de un presidente que se entiende perfectamente a partir de sus propias palabras ["Soy el topo que destruye el Estado desde adentro"](https://www.infobae.com/politica/2024/06/06/javier-milei-soy-el-topo-que-destruye-el-estado-desde-adentro/). Las universidades públicas son parte del estado que viene a destruir. Si la preocupación fuera meramente fiscal, no hubieran reducido el impuesto a los bienes personales, que pagan los ciudadanos más ricos del país, y [cuyo costo fiscal es similar a esta ley que recompone el presupuesto universitario](https://www.infobae.com/politica/2024/09/03/segun-la-uba-el-ajuste-del-gobierno-a-las-universidades-es-igual-a-los-beneficios-impositivos-para-el-sector-mas-rico/).

Defender la universidad pública, gratuita, inclusiva y de calidad es un deber que recae sobre cada ciudadano argentino. Sólo con más y mejor universidad será posible construir un futuro mejor para nuestro país. Una marcha federal para defender la ley de financiamiento universitario que el presidente quiere vetar, sería un primer paso fundamental para poder mantener de pie a nuestras universidades. Invito a todos a participar de dicha marcha convocada para el 2 de octubre de 2024.

 *Comentario adicional respecto a la situación salarial docente*  
Adicionalmente, no entramos en detalles sobre el tema en este informe, pero la pérdida de poder adquisitivo de los docentes universitarios también es un componente crítico que amenaza la continuidad de las clases en la segunda mitad del año. Adicionalmente a las paupérrimas paritarias docentes, la inexistencia de convocatoria a paritaria nacional para discutir el aumento de la garantía salarial lleva a que muchos docentes universitarios jóvenes, con cargos de dedicación simple y antiguedad menor a 5 años hayan recibido aumentos del 0% durante los meses de enero, febrero, marzo y abril. Muchos de ellos comienzan a presentar sus renuncias a pesar de su vocación y amor por la docencia. La situación es insostenible.

El gobierno anunció aumentos salariales del 3% para agosto y septiembre, y ha declarado que  [habría congelamiento de los salarios en octubre, noviembre y diciembre](https://www.eldiarioar.com/economia/gobierno-planifica-congelar-sueldos-estatales-ano-despues-otorgar-3-septiembre-octubre_1_11628962.html). De ser así, y la inflación mensual continúa su tendencia de mantenerse alrededor de 4%, los salarios reales de los docentes más jóvenes de nuestras universidades habrían perdido la mitad de su poder adquisitivo en 1 año de gobierno de Javier Milei. La situación es aún más drástica dado que la brecha salarial entre docentes universitarios de universidades privadas y públicas se ha ampliado a 2, 3 y hasta 4 veces en algunos casos.


![plot](https://github.com/rquiroga7/presupuesto_Universitario/blob/main/plots/salario_profesor_ayudante_ds.png)
Podemos observar esto en el gráfico de salario mensual de bolsillo de un profesor ayudante A (DS) en azul, que en diciembre de 2024 cobraría 145 mil pesos mensuales, cuando su sueldo de noviembre de 2023 ajustado por inflación sería de 297 mil pesos, más del doble!

INTRODUCCIÓN Y METODOLOGÍA
============

<details>
<summary>Para quien quiera entrar en detalles metodológicos, expandir para leer la sección de introducción y metodología</summary>

<div align="justify">
 
Ante la decisión del gobierno de Javier Milei de no enviar una ley de presupuesto para 2024, se recondujo el presupuesto 2023 ([Decreto 23/2024](https://www.boletinoficial.gob.ar/detalleAviso/primera/301615/20240105)). Debido a la alta inflación que se observa en el país desde principios de 2023, con un gran salto a fines del 2023 relacionado a la decisión de devaluar el peso un 55% el 12 de diciembre (el precio del dólar oficial saltó un 118%, de 367 a 800 pesos, ver [aquí](https://elpais.com/argentina/2023-12-12/milei-anuncia-una-devaluacion-del-peso-del-50-y-grandes-recortes-del-gasto-publico.html)), el presupuesto 2024 (con montos similares a los de 2023) es obviamente insuficiente para mantener funcionando a las distintas dependencias estatales. En particular esto aplica también para las Universidades Nacionales. Aquí es necesario aclarar que el presupuesto para salarios se está actualizando con cada paritaria, mientras que otros presupuestos como los de funcionamiento, hospitales, extensión, becas e investigación se vieron prácticamente congelados desde noviembre de 2023 hasta febrero de 2024.

El presupuesto indica los montos que el gobierno planifica dedicar a cada ministerio, secretaría, programa y actvidad. Sin embargo, esos montos son simplemente indicativos. Los fondos finalmente devengados y pagados pueden ser mayores o menores (sobreejecución y subejecución). 

El Ministerio de Economía mantiene una base de datos llamada [Presupuesto Abierto](https://www.presupuestoabierto.gob.ar/sici/) de donde pueden descargarse los datos de ejecución presupuestaria. Utilizando dichos datos, analizamos la ejecución presupuestaria mensual y anual, no de los montos pagados, sino de los montos devengados. Para leer una explicación sobre qué significan estos términos, consultar este [glosario](https://www.presupuestoabierto.gob.ar/sici/glosario-e). Esto nos va a permitir saber cuanto dinero se está enviando a las universidades, más allá de cuánto se haya prometido.

Vamos a analizar el crédito devengado bajo el programa 26 (DESARROLLO DE LA EDUCACIÓN SUPERIOR) del ex Ministerio de Educación y actual Ministerio de Capital Humano. Dentro de este programa, se encuentran distintas actividades que podemos resumir en la siguiente lista:
- #actividad_id==1 - Conduccion, Gestion y Apoyo a las Politicas de Educacion Superior
- #actividad_id==11 Fundar
- #actividad_id==12 Salarios Docentes
- #actividad_id==13 Salarios No-Docentes
- #actividad_id==14 Asistencia Financiera para el Funcionamiento Universitario
- #actividad_id==15 Salud (Hospitales Universitarios)
- #actividad_id==16 CyT
- #actividad_id==23 Desarrollo de Institutos Tecnologicos de Formacion Profesional
- #actividad_id==24 Promoción de carreras estratégicas
- #actividad_id==25 Extensión Universitaria

En general vamos a enfocarnos en el presupuesto total (programa 26), o en particular en el presupuesto de funcionamiento, es decir, la actividad 14 (Asistencia Financiera para el Funcionamiento Universitario).

Como metodología, en general vamos a mostrar gráficos de ejecución presupuestaria (crédito devengado) en pesos reales, es decir ajustado por inflación. Esto permite una comparación más realista de los presupuestos de cada mes, dado que los montos se ajustan por IPC para estimar cómo permite afrontar los costos que ese presupuesto está destinado a afrontar. Adicionalmente, cabe aclarar que los montos en pesos se expresarán en millones de pesos equivalentes a los del último mes analizado. Por lo tanto, los montos devengados coinciden para el último mes con los datos que uno puede encontrar en la página de presupuesto abierto, pero para meses anteriores, no habrá coincidencias dado que la página muestra montos nominales. También cabe la aclaración de que el IPC no es el instrumento ideal para deflactar el presupuesto universitario dado que no está diseñado para medir los costos de una universidad, pero es un indicador útil y de alta frecuencia de publicación que bastará para este análisis.

El código de bash y R utilizado para descargar, analizar y graficar los datos de ejecución presupuestaria de 2017-2024 están disponibles abiertamente en este repositorio. Los datos se descargan de la API de Presupuesto abierto (aunque no es necesario que el usuario los descargue ya que están disponibles en este repositorio). El script API_datos.R analiza y genera los gráficos de ejecución presupuestaria mensual, y los scripts UNC_2015-2024.R y 2015_2024.R generan los gráficos anuales, para la UNC y para la totalidad de las Universidades Nacionales, respectivamente. 
</details>
</div>

Para descargar este informe en versión PDF, click [aquí](https://github.com/rquiroga7/presupuesto_Universitario/raw/main/informe_pres_univ.pdf?raw=1).
