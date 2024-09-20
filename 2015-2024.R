library(dplyr)
library(jsonlite)
library(lubridate)
library(ggplot2)
library(zoo)
library(tidyverse)
`%notin%` <- Negate(`%in%`)



#Load IPC from file
ipc <- read.csv("ipc/ipc.csv")
#ipc$cumulative <- cumprod(1+ipc$ipc/100)/(1+ipc$ipc[1]/100)
ipc$cumulative<-ipc$ipc_indice
#Divide cumulative by the value corresponding to max month
max_mes <- max(ipc$fecha)
normalize_value <- ipc %>% filter(as.Date(fecha) == max_mes) %>% pull(cumulative)
ipc <- ipc %>% mutate(cumulative = round(cumulative / normalize_value, 4))
ipc$fecha <- as.Date(ipc$fecha, format = "%Y-%m-%d")


#Load ipc25_18 from file
ipc25_18 <- read.csv("ipc/ipc_proy2025_18anual.csv")
ipc25_18$fecha <- as.Date(ipc25_18$fecha, format = "%Y-%m-%d")
ipc25_18 <- ipc25_18 %>% mutate(ipc_indice = round(ipc_indice / normalize_value, 4)) %>% rename(cumulative = ipc_indice)


#Read json files into table (2017-2024)
data2017 <- fromJSON("datos/2017.json")  %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2018 <- fromJSON("datos/2018.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2019 <- fromJSON("datos/2019.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2020 <- fromJSON("datos/2020.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2021 <- fromJSON("datos/2021.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2022 <- fromJSON("datos/2022.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2023 <- fromJSON("datos/2023.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2024 <- fromJSON("datos/2024.json") 


#Join into data
data <- as.data.frame(rbind(data2017, data2018, data2019, data2020, data2021, data2022, data2023, data2024))
data<-data %>%  
    #If impacto_presupuestario_fecha is 2023-03-30 or 2023-03-31, then the value of impacto_presupuestario_mes should change to 4
    mutate(impacto_presupuestario_mes = ifelse(actividad_id %in% c(14,15,16) & impacto_presupuestario_fecha >= as.Date("2023-03-30") & impacto_presupuestario_fecha <=as.Date("2023-03-31"), 4, impacto_presupuestario_mes)) %>%
    mutate(impacto_presupuestario_mes = ifelse(actividad_id %in% c(14,15,16) & impacto_presupuestario_fecha >= as.Date("2023-06-23") & impacto_presupuestario_fecha <=as.Date("2023-06-30"), 7, impacto_presupuestario_mes)) %>%
    mutate(impacto_presupuestario_mes = ifelse(actividad_id %in% c(14,15,16) & impacto_presupuestario_fecha >= as.Date("2023-07-30") & impacto_presupuestario_fecha <=as.Date("2023-07-31"), 8, impacto_presupuestario_mes)) %>%
    mutate(impacto_presupuestario_mes = ifelse(actividad_id %in% c(14,15,16) & impacto_presupuestario_fecha >= as.Date("2023-09-28") & impacto_presupuestario_fecha <=as.Date("2023-09-30"), 1, impacto_presupuestario_mes)) %>%
    mutate(impacto_presupuestario_mes = ifelse(actividad_id %in% c(14,15,16) & impacto_presupuestario_fecha >= as.Date("2023-12-01") & impacto_presupuestario_fecha <=as.Date("2023-12-06"), 11, impacto_presupuestario_mes)) %>%
    mutate(impacto_presupuestario_mes = ifelse(actividad_id %in% c(14,15,16) & impacto_presupuestario_fecha >= as.Date("2024-03-01") & impacto_presupuestario_fecha <=as.Date("2024-03-06"), 2, impacto_presupuestario_mes)) 

#create new date column using impacto_presupuestario_mes and impacto_presupuestario_anio
data$fecha <- as.Date(paste(data$impacto_presupuestario_anio, data$impacto_presupuestario_mes, "01", sep = "-"), format = "%Y-%m-%d")


#Por Universidad subparcial_desc=="Universidad Nacional de Córdoba"
#programa_id==26 Desarrollo de la Educacion Superior
##actividad_id==14 Asistencia Financiera para el Funcionamiento Universitarios
##actividad_id==12 Salarios Docentes
##actividad_id==13 Salarios No-Docentes
##actividad_id==15 Hospitales
##actividad_id==11 Fundar
##actividad_id==25 Extensión
##actividad_id==16 CyT
##actividad_id==24 Promoción de carreras estratégicas
##actividad_id==23 Desarrollo de Institutos Tecnologicos de Formacion Profesional
##actividad_id==1 - Conduccion, Gestion y Apoyo a las Politicas de Educacion Superior


data<-data %>% 
    ungroup() %>%
    group_by(fecha) %>%
  left_join(ipc, by = "fecha") %>%
  mutate(credito_devengado_real = credito_devengado/cumulative) 


#Chequear que todo esté bien
View(data %>% 
filter(programa_id==26 & actividad_id == 14 & impacto_presupuestario_anio==2023) %>%
  group_by(fecha) %>% 
  summarise(
    credito_vigente = sum(credito_vigente),
    credito_devengado = sum(credito_devengado),
    credito_devengado_real = sum(credito_devengado_real),
    cumulative = mean(cumulative)
  )
)

#check the number of values for credito_devengado not equal to 0, for each month 
#View(data %>% 
  #filter(subparcial_desc=="Universidad Nacional de Córdoba") %>%
  #group_by(fecha) %>%
  #summarise(n = sum(credito_devengado != 0),min = min(credito_devengado[credito_devengado != 0]),max=max(credito_devengado),sum=sum(credito_devengado))
  #)


#Calcular data mensual
data_mensual <- data %>% 
  group_by(fecha,impacto_presupuestario_anio) %>%
  filter(fecha<=max_mes) %>%
  summarise(credito_devengado = round(sum(credito_devengado),0), credito_devengado_real = round(sum(credito_devengado_real),0))

ggplot(data_mensual, aes(x=fecha)) +
    geom_rect(aes(xmin=as.Date("2017-03-01"), xmax=as.Date("2019-11-30"), ymin=-Inf, ymax=Inf), fill="#ffffc5") +
    geom_rect(aes(xmin=as.Date("2019-12-01"), xmax=as.Date("2023-11-30"), ymin=-Inf, ymax=Inf), fill="#a4fcfc") +
    geom_rect(aes(xmin=as.Date("2023-12-01"), xmax=max_mes, ymin=-Inf, ymax=Inf), fill="#ffa7f8") +
  geom_line(aes(y=credito_devengado_real),linewidth=0.6) +
  #geom_smooth(aes(y=credito_devengado_real), method = "loess", se = FALSE, color="red") +
  geom_smooth(aes(y=credito_devengado_real), method = "loess", se = FALSE, color="black", span = 0.3,linewidth=2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%b", limits=as.Date(c("2017-03-01",max_mes))) +
  labs(title = "Crédito devengado por mes",
       x = "Fecha",
       y = "Credito devengado") +
    #add pale yellow background for dates up to 2019-11-30, pale blue for 2019-12-01 to 2023-11-30, and pale violet for 2023-12-01 to 2021-12-31, pale red for 2022-01-01 to 2022-12-31, pale purple for 2023-01-01 to 2024-03-01
  theme_light() +
  theme(legend.position = "bottom")


data_mensual <- data %>% 
  ungroup() %>%
  mutate(fecha = as.Date(paste(impacto_presupuestario_anio, impacto_presupuestario_mes, "01", sep = "-"), format = "%Y-%m-%d")) %>% 
  filter(fecha <= max_mes) %>% 
  group_by(fecha, impacto_presupuestario_anio,impacto_presupuestario_mes) %>% 
  summarise(credito_devengado = round(sum(credito_devengado), 0), credito_devengado_real = round(sum(credito_devengado_real), 0),cumulative=mean(cumulative))

last <- tail(data_mensual, 1)
first_proj_month <- as.Date(paste0(year(last$fecha), "-", month(last$fecha) + 1, "-01"))
last_month <- as.Date(paste0(year(last$fecha), "-12-01"))
n_months <- interval(first_proj_month, last_month) %/% months(1) + 1

resto <- data.frame(
  fecha = seq(first_proj_month, last_month, by = "months"),
  impacto_presupuestario_mes = month(seq(first_proj_month, last_month, by = "months")),
  impacto_presupuestario_anio = 2024,
  credito_devengado = 0,
  credito_devengado_real = rep(last$credito_devengado_real, n_months)
)
# If fecha is 2024-12-01 or 2024-06-01 and credito_devengado is 0, then modify the value of credito_devengado_real to be 1.6 times the previous value
resto <- resto %>% 
  mutate(credito_devengado_real = ifelse(fecha %in% as.Date(c("2024-12-01", "2024-06-01")) & credito_devengado == 0, credito_devengado_real * 1.5, credito_devengado_real))

resto2 <- merge(resto, ipc25_18, by.x = "fecha", by.y = "fecha") %>% 
          select(-ipc) %>%
          mutate(credito_devengado = round(if_else(credito_devengado == 0, credito_devengado_real*cumulative, credito_devengado),0))
data_mensual_2 <- rbind(data_mensual, resto2)

data_anual <- data_mensual_2 %>% 
  group_by(impacto_presupuestario_anio) %>% 
  summarise(credito_devengado = sum(credito_devengado), credito_devengado_real = sum(credito_devengado_real))
#########################

colors8=c("#d4d400","#d4d400","#d4d400", "#31ffff", "#31ffff", "#31ffff", "#31ffff", "#a8009d")
colors9=c("#d4d400","#d4d400","#d4d400", "#31ffff", "#31ffff", "#31ffff", "#31ffff", "#a8009d", "#a8009d")

#Plot annual data show every year in x axis. Fill columns 2017-2019 in yellow, 2020-2023 in cyan and 2024 in purple
ggplot(data_anual, aes(x=as.factor(impacto_presupuestario_anio), y=credito_devengado_real, fill=as.factor(impacto_presupuestario_anio))) +
  geom_bar(stat="identity") +
  labs(title = "Universidades Nacionales: Presupuesto anual devengado",subtitle=paste0("Ajustado por inflación (IPC). En pesos de ",max_mes),
       x = "Año",
       y = paste0("Credito anual devengado\n(millones de $ de ",max_mes)) +
    scale_fill_manual(values=colors8) +
  theme_light(base_size=14) +
    geom_text(aes(y = credito_devengado_real, label = round(credito_devengado_real, 0)), vjust = -0.5,size=5) +
  #scale y axis to show values in millions
  scale_y_continuous(labels = scales::comma, limits = c(NA, max(data_anual$credito_devengado_real) * 1.1)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  labs(caption = paste0("Se ajustó el crédito devengado en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor).\nSe asume ajuste por IPC septiembre-diciembre 2024.En millones de pesos de ", max_mes, ", con montos anualizados.\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario "))
ggsave("plots/presupuesto_anual_2017-2024.png",width = 10, height = 6, units = "in",dpi=300)

#Repeat plot but with 2013 == 100
data_anual_100 <- data_anual %>% 
  mutate(credito_devengado_real_base100 = credito_devengado_real/credito_devengado_real[1]*100)

View(data_anual_100)

#Plot annual data show every year in x axis. Fill columns 2017-2019 in yellow, 2020-2023 in cyan and 2024 in purple
ggplot(data_anual_100, aes(x=as.factor(impacto_presupuestario_anio), y=credito_devengado_real_base100, fill=as.factor(impacto_presupuestario_anio))) +
  geom_bar(stat="identity") +
  labs(title = "Universidades Nacionales: Presupuesto anual devengado",subtitle="Ajustado por inflación (IPC). Base 100 = 2017",
       x = "Año",
       y = "Credito anual devengado\n(base 100 = 2017)") +
    scale_fill_manual(values=colors8) +
  theme_light(base_size=14) +
    geom_text(aes(y = credito_devengado_real_base100, label = round(credito_devengado_real_base100, 0)), vjust = -0.5,size=5) +
  #scale y axis to show values in millions
  scale_y_continuous(labels = scales::comma, limits = c(NA, max(data_anual_100$credito_devengado_real_base100) * 1.1)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  labs(caption = paste0("Se ajustó el crédito devengado en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor).\nSe asume aumentos equivalentes al IPC para septiembre-diciembre 2024 (incluyendo aguinaldos).\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario "))
ggsave("plots/presupuesto_anual_100_2017-2024.png",width = 10, height = 6, units = "in",dpi=300)

#Presupuesto por número de estudiantes
anios<-c(2017,2018,2019,2020,2021,2022,2023,2024)
estudiantes<-c(2005152,2071270,2187292,2318255,2549789,2540854,2730754,2730754)

df_estudiantes<-data.frame(anio=anios,estudiantes=estudiantes)
data_anual_estudiantes<-merge(data_anual,df_estudiantes,by.x="impacto_presupuestario_anio",by.y="anio")
data_anual_estudiantes<-data_anual_estudiantes %>% 
  mutate(credito_devengado_real_por_estudiante = credito_devengado_real/estudiantes) %>%
  mutate(credito_devengado_real_por_est_100 = credito_devengado_real_por_estudiante/credito_devengado_real_por_estudiante[1]*100)


#Plot annual data show every year in x axis. Fill columns 2017-2019 in yellow, 2020-2023 in cyan and 2024 in purple
ggplot(data_anual_estudiantes, aes(x=as.factor(impacto_presupuestario_anio), y=credito_devengado_real_por_est_100, fill=as.factor(impacto_presupuestario_anio))) +
  geom_bar(stat="identity") +
  labs(title = "Universidades Nacionales: Presupuesto anual devengado",subtitle="Ajustado por inflación y número de estudiantes. Base 100 = 2017",
       x = "Año",
       y = "Credito anual devengado por estudiante\n(base 100 = 2017)\n") +
    scale_fill_manual(values=colors8) +
  theme_light(base_size=14) +
    geom_text(aes(y = credito_devengado_real_por_est_100, label = round(credito_devengado_real_por_est_100, 0)), vjust = -0.5,size=5) +
  #scale y axis to show values in millions
  scale_y_continuous(labels = scales::comma, limits = c(NA, max(data_anual_estudiantes$credito_devengado_real_por_est_100) * 1.1)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  labs(caption = paste0("Se ajustó el crédito devengado en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor).\nSe asume aumentos equivalentes al IPC para septiembre-diciembre 2024 (incluyendo aguinaldos).\nNúmero de estudiantes según Anuario SPU. Dato 2024 estimado como = 2023. Se utiliza base 100 = 2017.\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario "))
ggsave("plots/presupuesto_anual_porest_100_2017-2024.png",width = 10, height = 6, units = "in",dpi=300)




# Annual budget for 2025 is 3804260. The ipc_indice column is the cumulative IPC index for each month. Calculate the budget for each month of 2025
data_2025 <- data_mensual_2 %>% 
  filter(impacto_presupuestario_anio == 2024) %>%
  mutate(impacto_presupuestario_anio = 2025) %>%
  mutate(fecha = as.Date(paste(impacto_presupuestario_anio, impacto_presupuestario_mes, "01", sep = "-"), format = "%Y-%m-%d"))

data_2025 <- data_2025 %>% 
  select(-cumulative) %>%
  ungroup() %>%
  group_by(fecha) %>%
  left_join(ipc25_18, by = "fecha")

# Calculate initial credito_devengado values
data_2025 <- data_2025 %>%
  ungroup() %>%
  mutate(
    initial_credito_devengado = 3804260 * cumulative / (13 * data_2025$cumulative[1])
  )

# Apply the 1.5 multiplier to the specified dates
data_2025 <- data_2025 %>%
  mutate(
    initial_credito_devengado = ifelse(fecha %in% as.Date(c("2025-12-01", "2025-06-01")), initial_credito_devengado * 1.5, initial_credito_devengado)
  )

# Calculate the sum of initial values
sum_initial <- sum(data_2025$initial_credito_devengado)

# Adjust values proportionally to ensure the total sum is 3804260
data_2025 <- data_2025 %>%
  mutate(
    credito_devengado = round(initial_credito_devengado * (3804260 / sum_initial), 0)
  ) %>%
  mutate(credito_devengado_real = round(credito_devengado / cumulative, 0)) %>%
  select(-ipc, -initial_credito_devengado)

# Check the sum of credito_devengado
sum(data_2025$credito_devengado)


data_mensual_3<-rbind(data_mensual_2,data_2025)
data_anual_2025_100 <- data_mensual_3 %>% 
  group_by(impacto_presupuestario_anio) %>% 
  summarise(credito_devengado = round(sum(credito_devengado),0), credito_devengado_real = round(sum(credito_devengado_real),0)) %>%
  mutate(credito_devengado_real_base100 = credito_devengado_real/credito_devengado_real[1]*100)

View(data_mensual_3)  

max_mes25 <- as.Date("2025-06-01")
normalize_value25 <- ipc25_18 %>% filter(as.Date(fecha) == max_mes25) %>% pull(cumulative)
#Recalculate credito_devengado_real for data_mensual_3
data_mensual_3_25 <- data_mensual_3 %>% 
  ungroup() %>%
  mutate(credito_devengado_real = credito_devengado/cumulative*normalize_value25)
data_anual_2025 <- data_mensual_3_25 %>% 
  group_by(impacto_presupuestario_anio) %>% 
  summarise(credito_devengado = round(sum(credito_devengado),0), credito_devengado_real = round(sum(credito_devengado_real),0))

ggplot(data_anual_2025, aes(x=as.factor(impacto_presupuestario_anio), y=credito_devengado_real, fill=as.factor(impacto_presupuestario_anio))) +
  geom_bar(stat="identity") +
  labs(title = "Universidades Nacionales: Presupuesto anual devengado",subtitle=paste0("Ajustado por inflación (IPC). En pesos de ",max_mes25,".  Inflación 2025 estimada 18,3%"),
       x = "Año",
       y = paste0("Credito anual devengado\n(millones de $ de ",max_mes25)) +
    scale_fill_manual(values=colors9) +
  theme_light(base_size=14) +
    geom_text(aes(y = credito_devengado_real, label = round(credito_devengado_real, 0)), vjust = -0.5,size=5) +
  #scale y axis to show values in millions
  scale_y_continuous(labels = scales::comma, limits = c(NA, max(data_anual_2025$credito_devengado_real) * 1.10)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  labs(caption = paste0("Se ajustó el crédito devengado en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor).\nSe asume ajuste por IPC septiembre-diciembre 2024. En millones de pesos de ", max_mes25, ", con montos anualizados.\nPara 2025 se toma el proyecto de presupuesto 2025, que estima inflación anual para 2025 de 18,3%.\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario "))
ggsave("plots/presupuesto_anual_2017-2025.png",width = 10, height = 6, units = "in",dpi=300)



  
ggplot(data_anual_2025_100, aes(x=as.factor(impacto_presupuestario_anio), y=credito_devengado_real_base100, fill=as.factor(impacto_presupuestario_anio))) +
  geom_bar(stat="identity") +
    labs(title = "Universidades Nacionales: Presupuesto anual devengado",subtitle="Ajustado por inflación (IPC). Base 100 = 2017.  Inflación 2025 estimada 18,3%",
       x = "Año",
       y = "Credito anual devengado\n(base 100 = 2017)") +
    scale_fill_manual(values=colors9) +
  theme_light(base_size=14) +
    geom_text(aes(y = credito_devengado_real_base100, label = round(credito_devengado_real_base100, 0)), vjust = -0.5,size=5) +
  #scale y axis to show values in millions
  scale_y_continuous(labels = scales::comma, limits = c(NA, max(data_anual_100$credito_devengado_real_base100) * 1.1)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  labs(caption = paste0("Se ajustó el crédito devengado en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor).\nSe asume aumentos del presupuesto equivalentes al IPC para septiembre-diciembre 2024 (incluyendo aguinaldos).\nSe toma el presupuesto 2025 que estima IPC anual de 18,3%.\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario "))
  ggsave("plots/presupuesto_anual_100_2017-2025.png",width = 10, height = 6, units = "in",dpi=300)



#Presupuesto por número de estudiantes
anios25<-c(anios,2025)
estudiantes25<-c(estudiantes,2730754)
df_estudiantes<-data.frame(anio=anios25,estudiantes=estudiantes25)
data_anual_estudiantes_25<-merge(data_anual_2025_100,df_estudiantes,by.x="impacto_presupuestario_anio",by.y="anio")
data_anual_estudiantes_25<-data_anual_estudiantes_25 %>% 
  mutate(credito_devengado_real_por_estudiante = credito_devengado_real/estudiantes) %>%
  mutate(credito_devengado_real_por_est_100 = credito_devengado_real_por_estudiante/credito_devengado_real_por_estudiante[1]*100)


#Plot annual data show every year in x axis. Fill columns 2017-2019 in yellow, 2020-2023 in cyan and 2024 in purple
ggplot(data_anual_estudiantes_25, aes(x=as.factor(impacto_presupuestario_anio), y=credito_devengado_real_por_est_100, fill=as.factor(impacto_presupuestario_anio))) +
  geom_bar(stat="identity") +
  labs(title = "Universidades Nacionales: Presupuesto anual devengado",subtitle="Ajustado por inflación y número de estudiantes. Inflación 2025 estimada 18,3%",
       x = "Año",
       y = "Credito anual devengado por estudiante\n(base 100 = 2017)\n") +
    scale_fill_manual(values=colors9) +
  theme_light(base_size=14) +
    geom_text(aes(y = credito_devengado_real_por_est_100, label = round(credito_devengado_real_por_est_100, 0)), vjust = -0.5,size=5) +
  #scale y axis to show values in millions
  scale_y_continuous(labels = scales::comma, limits = c(NA, max(data_anual_estudiantes$credito_devengado_real_por_est_100) * 1.1)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  labs(caption = paste0("Se ajustó el crédito devengado en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor).\nSe asume aumentos del presupuesto equivalentes al IPC para septiembre-diciembre 2024 (incluyendo aguinaldos). \nPara 2025 se toma el proyecto de Presupuesto 2025, que estima IPC de 18,3% para 2025.\nNúmero de estudiantes según Anuario SPU (2024 y 2025 estimados como = 2023).\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario "))
ggsave("plots/presupuesto_anual_porest_100_2017-2025.png",width = 10, height = 6, units = "in",dpi=300)
