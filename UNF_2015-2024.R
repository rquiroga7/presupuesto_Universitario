library(dplyr)
library(jsonlite)
library(lubridate)
library(ggplot2)
library(zoo)
library(tidyverse)
`%notin%` <- Negate(`%in%`)



#Load IPC from file, asumimos 12% de inflación para marzo de 2024
ipc <- read.csv("ipc.csv")
ipc$cumulative <- cumprod(1+ipc$ipc/100)/(1+ipc$ipc[1]/100)
#Divide cumulative by the value corresponding to 2024-02-01
normalize_value <- ipc %>% filter(as.Date(fecha) == as.Date("2024-03-01")) %>% pull(cumulative)
ipc <- ipc %>% mutate(cumulative = round(cumulative / normalize_value, 4))
ipc$fecha <- as.Date(ipc$fecha, format = "%Y-%m-%d")


#Read json files into table (2017-2024)
data2017 <- fromJSON("2017.json")  %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2018 <- fromJSON("2018.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2019 <- fromJSON("2019.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2020 <- fromJSON("2020.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2021 <- fromJSON("2021.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2022 <- fromJSON("2022.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2023 <- fromJSON("2023.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2024 <- fromJSON("2024.json")


#Join into data
data <- as.data.frame(rbind(data2017, data2018, data2019, data2020, data2021, data2022, data2023, data2024))
data<- data %>%
    mutate(impacto_presupuestario_mes = ifelse(actividad_id==14 & impacto_presupuestario_fecha >= as.Date("2023-03-30") & impacto_presupuestario_fecha <=as.Date("2023-03-31"), 4, impacto_presupuestario_mes)) %>%
    mutate(impacto_presupuestario_mes = ifelse(impacto_presupuestario_fecha >= as.Date("2023-07-30") & impacto_presupuestario_fecha <=as.Date("2023-07-31"), 8, impacto_presupuestario_mes)) %>%
    mutate(impacto_presupuestario_mes = ifelse(impacto_presupuestario_fecha >= as.Date("2024-03-01") & impacto_presupuestario_fecha <=as.Date("2024-03-06"), 2, impacto_presupuestario_mes)) 
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
  mutate(credito_devengado_real = credito_devengado/cumulative) %>%
  filter(subparcial_desc=="Universidad Nacional de Formosa")


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


#pokasdolijasdoi
#check the number of values for credito_devengado not equal to 0, for each month 
#View(data %>% 
  #filter(subparcial_desc=="Universidad Nacional de Córdoba") %>%
  #group_by(fecha) %>%
  #summarise(n = sum(credito_devengado != 0),min = min(credito_devengado[credito_devengado != 0]),max=max(credito_devengado),sum=sum(credito_devengado))
  #)


#Calcular data mensual
data_mensual <- data %>% 
  group_by(fecha,impacto_presupuestario_anio) %>%
  filter(fecha<=as.Date("2024-03-01")) %>%
  summarise(credito_devengado = round(sum(credito_devengado),0), credito_devengado_real = round(sum(credito_devengado_real),0))

ggplot(data_mensual, aes(x=fecha)) +
    geom_rect(aes(xmin=as.Date("2017-03-01"), xmax=as.Date("2019-11-30"), ymin=-Inf, ymax=Inf), fill="#ffffc5") +
    geom_rect(aes(xmin=as.Date("2019-12-01"), xmax=as.Date("2023-11-30"), ymin=-Inf, ymax=Inf), fill="#a4fcfc") +
    geom_rect(aes(xmin=as.Date("2023-12-01"), xmax=as.Date("2024-02-01"), ymin=-Inf, ymax=Inf), fill="#ffa7f8") +
  geom_line(aes(y=credito_devengado_real),linewidth=0.6) +
  #geom_smooth(aes(y=credito_devengado_real), method = "loess", se = FALSE, color="red") +
  geom_smooth(aes(y=credito_devengado_real), method = "loess", se = FALSE, color="black", span = 0.3,linewidth=2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%b", limits=as.Date(c("2017-03-01","2024-02-01"))) +
  labs(title = "Crédito devengado por mes",
       x = "Fecha",
       y = "Credito devengado") +
    #add pale yellow background for dates up to 2019-11-30, pale blue for 2019-12-01 to 2023-11-30, and pale violet for 2023-12-01 to 2021-12-31, pale red for 2022-01-01 to 2022-12-31, pale purple for 2023-01-01 to 2024-03-01
  theme_light() +
  theme(legend.position = "bottom")

#Create interactive plot
library(plotly)
p <- ggplot(data_mensual3, aes(x=fecha)) +
  geom_line(aes(y=credito_devengado_real, color="credito_devengado_real")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%b", limits=as.Date(c("2017-03-01","2024-02-01"))) +
  labs(title = "Crédito devengado por mes",
       x = "Fecha",
       y = "Credito devengado") +
    #add vertical line for 2018-08-01
    geom_vline(xintercept = as.numeric(as.Date("2018-08-01")), linetype="dashed", color = "blue") +
  theme_light() +
  theme(legend.position = "bottom")
ggplotly(p)
  


marzo<-tail(data_mensual,1) 
#Create a data frame with fecha 2024-04-01 to 2024-12-01, with credito_devengado = 0 and credito_devengado_real equal to marzo$credito_devengado_real
resto<-data.frame(fecha=seq(as.Date("2024-04-01"), as.Date("2024-12-01"), by="months"),impacto_presupuestario_anio=2024,credito_devengado=0,credito_devengado_real=rep(marzo$credito_devengado_real,9))
#join data_mensual with marzo and resto
data_mensual2 <- rbind(data_mensual, marzo, resto)


#Calcular data anual
data_anual <- data_mensual2 %>% 
  group_by(impacto_presupuestario_anio) %>%
  summarise(credito_devengado = sum(credito_devengado), credito_devengado_real = sum(credito_devengado_real))

#Plot annual data show every year in x axis. Fill columns 2017-2019 in yellow, 2020-2023 in cyan and 2024 in purple
ggplot(data_anual, aes(x=as.factor(impacto_presupuestario_anio), y=credito_devengado_real, fill=as.factor(impacto_presupuestario_anio))) +
  geom_bar(stat="identity") +
  labs(title = "Crédito anual devengado UNF ajustado por inflación\nincluye aumento de marzo y suponiendo que\nen 2024 el presupuesto se ajustará por inflación desde abril",
       x = "Año",
       y = "Credito anual devengado\n(millones de $ de 03/2024)") +
  scale_fill_manual(values=c("#d4d400","#d4d400","#d4d400", "#31ffff", "#31ffff", "#31ffff", "#31ffff", "#ff28ed")) +
  theme_light(base_size=14) +
    geom_text(aes(y = credito_devengado_real, label = round(credito_devengado_real, 0)), vjust = -0.5,size=6) +
  #scale y axis to show values in millions
  scale_y_continuous(labels = scales::comma, limits = c(NA, max(data_anual$credito_devengado_real) * 1.1)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(caption = "Se ajustó el crédito devengado en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor).\nSe asume una inflación del 12% para marzo de 2024 y ajuste por IPC abril-diciembre 2024.\nSe calcula el equivalente a millones de pesos de marzo de 2024 y se anualizaron los montos.\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_UNC ")
ggsave("UNF_presupuesto_anual_2017-2024.png",width = 10, height = 6, units = "in",dpi=300)



#Ahora sólo gastos de funcionamiento
#Calcular data mensual
data_mensual14 <- data %>% 
  ungroup() %>%
  #mutate(impacto_presupuestario_mes=ifelse(impacto_presupuestario_anio==2024 & fecha>=as.Date("2024-03-01"),2,impacto_presupuestario_mes)) %>%
  mutate(fecha=as.Date(paste(impacto_presupuestario_anio, impacto_presupuestario_mes, "01", sep = "-"), format = "%Y-%m-%d")) %>%
  filter(fecha<=as.Date("2024-03-01"),actividad_id==14) %>%
    group_by(fecha,impacto_presupuestario_anio) %>%
  summarise(credito_devengado = round(sum(credito_devengado),0), credito_devengado_real = round(sum(credito_devengado_real),0))

marzo14<-tail(data_mensual14,1) 
#Create a data frame with fecha 2024-04-01 to 2024-12-01, with credito_devengado = 0 and credito_devengado_real equal to marzo$credito_devengado_real
resto14<-data.frame(fecha=seq(as.Date("2024-04-01"), as.Date("2024-12-01"), by="months"),impacto_presupuestario_anio=2024,credito_devengado=0,credito_devengado_real=rep(marzo14$credito_devengado_real,9))
#join data_mensual with marzo and resto
data_mensual14_2 <- rbind(data_mensual14, resto14)
#Calcular data anual
data_anual14 <- data_mensual14_2 %>% 
  group_by(impacto_presupuestario_anio) %>%
  summarise(credito_devengado = sum(credito_devengado), credito_devengado_real = sum(credito_devengado_real))


#Plot annual data show every year in x axis. Fill columns 2017-2019 in yellow, 2020-2023 in cyan and 2024 in purple
ggplot(data_anual14, aes(x=as.factor(impacto_presupuestario_anio), y=credito_devengado_real, fill=as.factor(impacto_presupuestario_anio))) +
  geom_bar(stat="identity") +
  labs(title = "Crédito anual devengado UNF para FUNCIONAMIENTO ajustado por inflación\n(pesos de marzo de 2024), incluye aumento de marzo y suponiendo que\nen 2024 el presupuesto se ajustará por inflación desde abril",
       x = "Año",
       y = "Credito anual devengado\n(millones de $ de 03/2024)") +
  scale_fill_manual(values=c("#d4d400","#d4d400","#d4d400", "#31ffff", "#31ffff", "#31ffff", "#31ffff", "#ff28ed")) +
  theme_light(base_size=14) +
  geom_text(aes(y = credito_devengado_real, label = round(credito_devengado_real, 0)), vjust = -0.5,size=6) +
  #scale y axis to show values in millions
  scale_y_continuous(labels = scales::comma, limits = c(NA, max(data_anual14$credito_devengado_real) * 1.1)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(caption = "Se ajustó el crédito devengado en cada mes para la actividad 14 por inflación mensual, utilizando el IPC (índice de precios al consumidor).\nSe asume una inflación del 12% para marzo de 2024 y ajuste por IPC abril-diciembre 2024.\nSe calcula el equivalente a millones de pesos de marzo de 2024 y se anualizaron los montos.\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_UNC ")
ggsave("UNF_presupuesto_anual_func_2017-2024.png",width = 10, height = 6, units = "in",dpi=300)

