Repositorio con código de bash y R que permite descargar, analizar y graficar los datos de ejecución presupuestaria de 2017-2024. Los datos se descargan de la API de Presupuesto abierto, pero ya están disponibles en el repositorio, no es necesario volver a descargarlos. El script API_datos.R analiza y genera los gráficos de ejecución presupuestaria mensual, y los scripts UNC_2015-2024.R y 2015_2024.R generan los gráficos anuales, para la UNC y para la totalidad de las Universidades Nacionales, respectivamente. 

Aquí la ejecución mensual para presupuesto total (todas las actividades) y la actividad 14 (funcionamiento).

![plot](https://github.com/rquiroga7/presupuesto_UNC/blob/main/plot_14_70p_prom.png)
![plot](https://github.com/rquiroga7/presupuesto_UNC/blob/main/plot_all_70p_prom.png)

Aquí los gráficos de ejecución anual, suponiendo que para 2024 el crédito devengado se ajustará por IPC desde abril en adelante.
Presupuesto de funcionamiento (act 14):
![plot](https://github.com/rquiroga7/presupuesto_UNC/blob/main/presupuesto_anual_func_2017-2024.png)

Presupuesto total:
![plot](https://github.com/rquiroga7/presupuesto_UNC/blob/main/presupuesto_anual_2017-2024.png)
