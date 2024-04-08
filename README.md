Ejecución presupuestaria 2024 de las Universidades Nacionales de Argentina
=================================================================================

Última actualización 08/04/2024

 **Dr. Rodrigo Quiroga**  
 Investigador Asistente INFIQC-CONICET  
 Profesor Adjunto de Bioinformática y Biología Computacional  
 Facultad de Ciencias Químicas, Universidad Nacional de Córdoba  

INTRODUCCIÓN Y METODOLOGÍA
============

<details>
<summary>Expandir para leer la sección de introducción y metodología</summary>

<div align="justify">
 
Ante la decisión del gobierno de Javier Milei de no enviar una ley de presupuesto para 2024, se recondujo el presupuesto 2023 ([Decreto 23/2024](https://www.boletinoficial.gob.ar/detalleAviso/primera/301615/20240105)). Debido a la alta inflación que se observa en el país desde principios de 2023, con un gran salto a fines del 2023 relacionado a la decisión de devaluar el peso más del 50% el 12 de diciembre (el precio del dólar oficial saltó de 367 a 800 pesos, ver [aquí](https://elpais.com/argentina/2023-12-12/milei-anuncia-una-devaluacion-del-peso-del-50-y-grandes-recortes-del-gasto-publico.html)), el presupuesto 2024 (con montos similares a los de 2023) es obviamente insuficiente para mantener funcionando a las distintas dependencias estatales, en particular esto aplica también para las Universidades Nacionales.

El Ministerio de Economía mantiene una base de datos llamada [Presupuesto Abierto](https://www.presupuestoabierto.gob.ar/sici/) de donde pueden descargarse los datos de ejecución presupuestaria. Utilizando dichos datos, analizamos la ejecución presupuestaria mensual, no de los montos pagados, sino de los montos devengados. Para leer una explicación sobre qué significan estos términos, consultar este [glosario](https://www.presupuestoabierto.gob.ar/sici/glosario-e).

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

Como metodología, en general vamos a mostrar gráficos de ejecución presupuestaria (crédito devengado) en pesos reales, es decir ajustado por inflación. Esto permite una comparación más realista de los presupuestos de cada mes, dado que los montos se ajustan por IPC para estimar cómo permite afrontar los costos que ese presupuesto está destinado a afrontar. Adicionalmente, cabe aclarar que los montos en pesos se expresarán en millones de pesos equivalentes a los del último mes analizado. Por lo tanto, los montos devengados coinciden para el último mes con los datos que uno puede encontrar en la página de presupuesto abierto, pero para meses anteriores, no habrá coincidencias dado que la página muestra montos nominales.

El código de bash y R utilizado para descargar, analizar y graficar los datos de ejecución presupuestaria de 2017-2024 están disponibles abiertamente en este repositorio. Los datos se descargan de la API de Presupuesto abierto (aunque no es necesario que el usuario los descargue ya que están disponibles en este repositorio). El script API_datos.R analiza y genera los gráficos de ejecución presupuestaria mensual, y los scripts UNC_2015-2024.R y 2015_2024.R generan los gráficos anuales, para la UNC y para la totalidad de las Universidades Nacionales, respectivamente. 
</details>
</div>
 
Ejecución presupuestaria mensual de 2023 y 2024
============

Presupuesto de funcionamiento
-----------------------------
<div align="justify">
 
El presupuesto de funcionamiento para enero y febrero de 2024  se mantuvo exactamente igual al de 2023,en términos nominales. Es decir, el crédito devengado en pesos fue exactamente el mismo desde enero de 2023 a febrero de 2024. Cabe aclarar que el presupuesto para funcionamiento anual se calcula a principio de año en base a la inflación esperada. Luego, ese monto se divide por 12 y se envía ese monto en cada mes del año. Por lo tanto, durante 2023 el presupuesto de funcionamiento no estuvo "congelado" como dijo el actual subsecretario de políticas universitarias [aquí](https://x.com/AleCiroAlvarez/status/1768374421440410066)), sino que se calcula en base a la inflación esperada para ese año. Justamente, lo esperable para 2024, es que se actualizara el monto en base a la inflación de 2023 y la esperada para 2024, estimando un nuevo presupuesto anual de funcionamiento, que debería haberse dividido en 12. No ocurrió eso, sino que como estipula el [Decreto 23/2024](https://www.boletinoficial.gob.ar/detalleAviso/primera/301615/20240105), se volvieron a utilizar los montos de 2023, sin ajustar por inflación. Todo esto fue explicado en detalle por Jorge Aliaga (Secretario de Planeamiento y Evaluación Institucional de la UNAHUR y actual miembro del directorio de CONICET en representación de las Universidades Nacionales) [en este hilo de Twitter](https://x.com/jorgeluisaliaga/status/1769806057600081985).

También es importante explicar que las Universidades Nacionales en muchos casos generan fondos propios que se destinan a funcionamiento (en general por prestación de servicios). Sin embargo, en la mayoría de las Universidades el presupuesto enviado por el gobierno nacional para tal fin es determinante.

Een el siguiente gráfico observamos el presupuesto mensual real (ajustado por inflación) para funcionamiento de todas las Universidades Nacionales del país. Se grafica también el promedio mensual 2023 como una línea punteada verde.

![plot](https://github.com/rquiroga7/presupuesto_UNC/blob/main/plot_14_70p_prom.png)

Vemos que en Marzo de 2024, se otorgó un aumento del 70% del presupuesto mensual de funcionamiento, que efectivamente fue devengado. Sin embargo, al ajustar por inflación, observamos que el presupuesto mensual para funcionamiento quedó licuado por inflación. Por este motivo, el presupuesto de marzo, aún con el aumento del 70% nominal respecto a febrero, representa un monto real que es aprox el 60% del promedio mensual de 2023, y un 40% del promedio de la primer mitad de 2023.

Sería necesario un aumento adicional del 70% para que las Universidades vuelvan a tener un presupuesto de funcionamiento similar al promedio de 2023, y del 120% para alcanzar niveles similares a la primer mitad de 2023. Además, vale aclarar que con los [aumentos de tarifas de servicios previstos para 2024](https://www.infobae.com/economia/2024/04/05/tarifas-de-gas-todo-lo-que-hay-que-saber-y-de-cuanto-seran-los-nuevos-aumentos/), y el hecho de que [el gobierno eliminó a las instituciones educativas de los subsidios de energía eléctrica](https://www.datadiario.com/sociedad/aumentan-las-facturas-de-luz-para-usuarios-de-mayores-ingresos-20242518120), incluso un nuevo aumento del 120% podría ser insuficiente para afrontar los costos operativos del año 2024.

Como vemos, es absolutamente falso lo que declaró el Secretario de Educación Carlos Torrendel el 8 de abril [aquí](https://twitter.com/somoscorta/status/1777322866268139561). Sin dudas existe una política de desfinanciamiento presupuestario de las Universidades, y la situación es absolutamente crítica. Actualmente las Universidades Nacionales están gastando ahorros para poder seguir funcionando en este contexto, y en estos días se difundió la [noticia](https://www.lmneuquen.com/neuquen/no-la-dejemos-morir-la-universidad-nacional-del-comahue-entro-cesacion-pagos-n1104420) de que la Universidad Nacional del Comahue entró en cesación de pagos por no poder afrontar las boletas de servicios. Es la primera Universidad en esta situación, pero si no hay un nuevo aumento urgente del presupuesto de funcionamiento (que debería ser mayor al 70%), seguramente entrarán en cesación de pagos muchas otras universidades y será imposible afrontar la continuidad de las clases en el segundo cuatrimestre del año. 
</div>

Presupuesto total
-----------------
Otro punto importante para aclarar, es que numerosos funcionarios del gobierno nacional están repitiendo el dato falso "en marzo hubo un aumento del 70% del presupuesto universitario", incluso [repetido por el vocero presidencial en conferencia de prensa](https://x.com/somoscorta/status/1776262854003310864). Como vimos arriba, esto es falso porque el aumento del 70% era del presupuesto de funcionamiento, y ese aumento está MUY por debajo del aumento necesario para compensar la inflación y aumento de tarifas.

En el siguiente gráfico observamos el presupuesto mensual real total (ajustado por inflación), de todas las Universidades Nacionales del país. Se grafica también el promedio mensual 2023 como una línea punteada verde (calculado sobre los meses sin aguinaldo).

![plot](https://github.com/rquiroga7/presupuesto_UNC/blob/main/plot_all_70p_prom.png)


**Como vemos en el gráfico, el aumento del 70% anunciado por el gobierno con bombos y platillos en realidad representa un aumento del 2.2% del presupuesto total!**
No sólo eso, sino que el presupuesto real de marzo está por debajo del presupuesto real de febrero, debido a que los aumentos nominales fueron menores a la inflación esperada para marzo (12%).

Ejecución presupuestaria anual para el período 2017-2024
============

Para poner en contexto histórico el presupuesto 2024, realizamos el siguiente ejercicio. Realizamos la optimista presunción de que el presupuesto se ajustaría por inflación (IPC) a partir de abril, y comparamos ese presupuesto anual ajustado por inflación con años anteriores. Es decir, calculamos cual sería el presupuesto anual para 2024 si no hay aumentos del presupuesto y si a partir de abril se ajusta por IPC. Analizamos primero el presupuesto de funcionamiento y luego el presupuesto total.

Presupuesto de funcionamiento (act 14):
![plot](https://github.com/rquiroga7/presupuesto_UNC/blob/main/presupuesto_anual_func_2017-2024.png)

Observamos que sin aumentos adicionales, a pesar del aumento de 70% de marzo, el presupuesto de funcionamiento ajustado por inflación para 2024 es aproximadamente el 50% del presupuesto de 2023. También podemos observar que el presupuesto de funcionamiento 2024 sería incluso inferior al de 2020, cuando en plena pandemia las Universidades no funcionaron de manera presencial! Debería quedar claro que las Universidades son objeto de un ajuste presupuestario fenomenal, lo cual también confirma las afirmaciones del rector y vicerector de la UBA, quienes dijeron que [sin aumentos presupuestarios significativos será imposible afrontar un funcionamiento normal durante el segundo cuatrimestre](https://www.pagina12.com.ar/726980-la-universidad-publica-al-borde-del-abismo).

Respecto al Presupuesto total:
![plot](https://github.com/rquiroga7/presupuesto_UNC/blob/main/presupuesto_anual_2017-2024.png)

Observamos que el presupuesto sería el menor desde que se comenzaron a recopilar datos de Presupuesto Abierto. El presupuesto ajustado por inflación de 2024 representaría el 66% del presupuesto 2023.


Consideraciones finales
============

Aquí termina la parte analítica del informe y comienza una sección con opiniones personales. **La situación presupuestaria de las Universidades es crítica.** Ya se encuentran suspendidos programas valiosísimos de becas, investigación y extensión universitaria, pero aún así, el presupuesto es tan escueto que las universidades están gastando ahorros para poder pagar las tarifas de servicios básicos. Ya entró en cesación de pagos la Universidad Nacional del Comahue, a pesar de que aún no entran en efecto los incrementos de tarifas programados por el gobierno para este año. 
**Sería necesario un nuevo aumento de 120% (o más, dependiendo de los aumentos de tarifas) del presupuesto de funcionamiento para asegurar la continuidad de las clases durante la segunda mitad del año 2024. De no ocurrir, muchas Universidades deberían cerrar sus puertas para el segundo cuatrimestre.** Esto no es "campaña de miedo" sino un necesario llamado de atención sobre el ajuste *que ya efectuó* el gobierno nacional, pero cuyos efectos aún no vemos en la práctica en algunas universidades simplemente porque aún tienen ahorros para mantener abiertas sus puertas. El CIN debe nuevamente motorizar estos reclamos ante el Ministerio de Capital Humano, pero también creo necesario que comuniquemos ampliamente esta emergencia presupuestaria a docentes, no-docentes y estudiantes. La urgencia de la situación es clara para los rectores, pero no es de amplio conocimiento en la comunidad universitaria. Aún estamos a tiempo de ponernos de pie y defender la Universidad Pública, aunque el tiempo se nos acaba rápidamente y muchas autoridades mantienen un silencio que aturde. 

 *Comentario adicional respecto a la situación salarial docente*  
Adicionalmente, no entramos en detalles sobre el tema en este informe, pero la pérdida de poder adquisitivo de los docentes universitarios también es un componente crítico que amenaza la continuidad de las clases en la segunda mitad del año. Adicionalmente a las paupérrimas paritarias docentes, la inexistencia de convocatoria a paritaria nacional para discutir el aumento de la garantía salarial lleva a que muchos docentes universitarios jóvenes, con cargos de dedicación simple y antiguedad menor a 5 años hayan recibido aumentos del 0% durante los meses de enero y febrero. Muchos de ellos comienzan a presentar sus renuncias a pesar de su vocación y amor por la docencia. La situación es insostenible.
