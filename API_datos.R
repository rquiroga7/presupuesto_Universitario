library(dplyr)
library(jsonlite)
library(lubridate)
`%notin%` <- Negate(`%in%`)
#Read 2022_1.json into table
data2023 <- fromJSON("2023.json")
data2024 <- fromJSON("2024.json")
#Get data where the value of entidad_desc equals "Ministerio de Educación"
data2023ed <- data2023 %>% filter(entidad_desc == "Ministerio de Educación")
data2024ed <- data2024 %>% filter(jurisdiccion_desc == "Ministerio de Capital Humano")

#Limitar a Prov De Córdoba
#ubicacion_geografica_id==14
#Resultado Nacional
#ubicacion_geografica_id==97
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

#Calcular total gastado por actividad a nivel nacional en febrero de 2023 y 2024
feb23<- data2023ed %>%                  #filter(impacto_presupuestario_mes==2) %>% 
                filter(impacto_presupuestario_mes==2 & programa_id==26 & actividad_id %in% c(14,15,16)) %>% 
                group_by(actividad_desc) %>% 
                summarise(credito_vigente = sum(credito_vigente),
                    credito_comprometido = sum(credito_comprometido),
                    credito_devengado = sum(credito_devengado),
                    credito_pagado = sum(credito_pagado),
                    credito_devengado = sum(credito_devengado))

View(data2024ed %>% filter(impacto_presupuestario_mes==2) %>% group_by(actividad_desc) %>% summarise(credito_vigente = sum(credito_vigente),credito_comprometido = sum(credito_comprometido),credito_devengado = sum(credito_devengado),credito_pagado = sum(credito_pagado),credito_devengado = sum(credito_devengado)))

#Create barplot from data2023ed, filtering actividad_id %in% c(14,15,16), and plotting one bar per month (impacto_presupuestario_mes)
library(ggplot2)

#merge data2024ed and data2023ed
dataed <- rbind(data2023ed, data2024ed)
dataed<-dataed %>%  
    #If impacto_presupuestario_fecha is 2023-03-30 or 2023-03-31, then the value of impacto_presupuestario_mes should change to 4
    mutate(impacto_presupuestario_mes = ifelse(impacto_presupuestario_fecha == as.Date(c("2023-03-30","2023-03-31")), 4, impacto_presupuestario_mes)) %>%
    mutate(impacto_presupuestario_mes = ifelse(impacto_presupuestario_fecha == as.Date(c("2023-07-30","2023-07-31")), 8, impacto_presupuestario_mes)) %>%
    mutate(impacto_presupuestario_mes = ifelse(impacto_presupuestario_fecha == as.Date(c("2024-03-01","2024-03-05")), 2, impacto_presupuestario_mes)) 
#create new date column using impacto_presupuestario_mes and impacto_presupuestario_anio
dataed$fecha <- as.Date(paste(dataed$impacto_presupuestario_anio, dataed$impacto_presupuestario_mes, "01", sep = "-"), format = "%Y-%m-%d")

data2024ed %>% filter(actividad_id %in% c(14,15,16)) %>% group_by(impacto_presupuestario_mes) %>% summarise(credito_devengado = sum(credito_devengado))
dataed %>% 
  mutate(fecha = as.Date(fecha)) %>%  # convert to date if not already
  filter(actividad_id %in% c(14,15,16)) %>% 
  group_by(fecha) %>% 
  summarise(credito_devengado = sum(credito_devengado)) %>% 
  ggplot(aes(x = fecha, y = credito_devengado)) +
  geom_bar(stat = "identity", fill = "blue", width = 20) +  # set width to 1 to fill the entire day
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = c(0.01,0.01)) +  # set date breaks and labels
  scale_y_continuous(labels = scales::dollar_format(scale = 1),limits=c(0,15000)) +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Mes", y = "Devengado en $ (millones)", title = "Crédito mensual devengado 2023-2024 actividad 14+15+16\n (funcionamiento+salud+CyT)")

ggsave("plot_14_15_16.png", width = 10, height = 6, dpi = 300)


dataed %>% 
  mutate(fecha = as.Date(fecha)) %>%  # convert to date if not already
  filter(actividad_id %in% c(14)) %>% 
  group_by(fecha) %>% 
  summarise(credito_devengado = sum(credito_devengado)) %>% 
  ggplot(aes(x = fecha, y = credito_devengado)) +
  geom_bar(stat = "identity", fill = "blue", width = 20) +  # set width to 1 to fill the entire day
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = c(0.01,0.01)) +  # set date breaks and labels
  scale_y_continuous(labels = scales::dollar_format(scale = 1),limits=c(0,15000)) +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Mes", y = "Devengado en $ (millones)", title = "Crédito mensual devengado 2023-2024\nactividad 14 (funcionamiento)")

ggsave("plot_14.png", width = 10, height = 6, dpi = 300)


#IPC
#create data frame with a column "fecha" with monthly dates from 2023-01-01 to 2024-03-01, and values equal to : 6.0,6.6,7.7,8.4,7.8,6.0,6.3,12.4,12.7,8.3,12.8,25.5,20.6,13.2,10.8
ipc <- data.frame(fecha = seq(as.Date("2023-01-01"), as.Date("2024-02-01"), by = "month"), ipc = c(6.0,6.6,7.7,8.4,7.8,6.0,6.3,12.4,12.7,8.3,12.8,25.5,20.6,13.2))
#add cumulative IPC column, multiplying each value by the previous one
ipc$cumulative <- cumprod(1+ipc$ipc/100)/1.06
ipc<-ipc %>% mutate(cumulative = round(cumulative/max(cumulative), 2))

ipc_14_15_16<-dataed %>% 
  mutate(fecha = as.Date(fecha)) %>%  # convert to date if not already
  filter(actividad_id %in% c(14,15,16)) %>% 
  group_by(fecha) %>% 
  summarise(credito_devengado = sum(credito_devengado)) %>%
    left_join(ipc, by = "fecha") %>%
  mutate(credito_devengado_real = credito_devengado/cumulative)

ipc_14<-dataed %>% 
  mutate(fecha = as.Date(fecha)) %>%  # convert to date if not already
  filter(actividad_id %in% c(14)) %>% 
  group_by(fecha) %>% 
  summarise(credito_devengado = sum(credito_devengado)) %>%
    left_join(ipc, by = "fecha") %>%
  mutate(credito_devengado_real = credito_devengado/cumulative)

  ipc_16<-dataed %>% 
  mutate(fecha = as.Date(fecha)) %>%  # convert to date if not already
  filter(actividad_id %in% c(16)) %>% 
  group_by(fecha) %>% 
  summarise(credito_devengado = sum(credito_devengado)) %>%
    left_join(ipc, by = "fecha") %>%
  mutate(credito_devengado_real = credito_devengado/cumulative)


#Plot ipc_14_15_16 as above
ipc_14_15_16 %>% 
  ggplot(aes(x = fecha, y = credito_devengado_real)) +
  geom_bar(stat = "identity", fill = "blue", width = 20) +  # set width to 1 to fill the entire day
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = c(0.01,0.01)) +  # set date breaks and labels
  scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
  geom_text(aes(label = round(credito_devengado_real, 0)), vjust = -0.5) +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Mes", y = "Devengado en $ (millones)", title = "Crédito mensual devengado 2023-2024 actividad 14+15+16\n (funcionamiento+salud+CyT) ajustado por IPC ($ de febrero de 2024)")
ggsave("plot_14_15_16_real.png", width = 10, height = 6, dpi = 300)

ipc_14 %>% 
  ggplot(aes(x = fecha, y = credito_devengado_real)) +
  geom_bar(stat = "identity", fill = "blue", width = 20) +  # set width to 1 to fill the entire day
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = c(0.01,0.01)) +  # set date breaks and labels
  scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
  geom_text(aes(label = round(credito_devengado_real, 0)), vjust = -0.5) +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Mes", y = "Devengado en $ (millones)", title = "Crédito mensual devengado 2023-2024 actividad 14+15+16\n (funcionamiento+salud+CyT) ajustado por IPC ($ de febrero de 2024)")
ggsave("plot_14_real.png", width = 10, height = 6, dpi = 300)


#REPETIR PARA LA UNC
unc_ipc_14_15_16<-dataed %>% 
  mutate(fecha = as.Date(fecha)) %>%  # convert to date if not already
  filter(actividad_id %in% c(14,15,16), subparcial_desc=="Universidad Nacional de Córdoba") %>% 
  group_by(fecha) %>% 
  summarise(credito_devengado = sum(credito_devengado)) %>%
    left_join(ipc, by = "fecha") %>%
  mutate(credito_devengado_real = credito_devengado/cumulative)

unc_ipc_14<-dataed %>% 
  mutate(fecha = as.Date(fecha)) %>%  # convert to date if not already
  filter(actividad_id %in% c(14), subparcial_desc=="Universidad Nacional de Córdoba") %>% 
  group_by(fecha) %>% 
  summarise(credito_devengado = sum(credito_devengado)) %>%
    left_join(ipc, by = "fecha") %>%
  mutate(credito_devengado_real = credito_devengado/cumulative)

  unc_ipc_16<-dataed %>% 
  mutate(fecha = as.Date(fecha)) %>%  # convert to date if not already
  filter(actividad_id %in% c(16), subparcial_desc=="Universidad Nacional de Córdoba") %>% 
  group_by(fecha) %>% 
  summarise(credito_devengado = sum(credito_devengado)) %>%
    left_join(ipc, by = "fecha") %>%
  mutate(credito_devengado_real = credito_devengado/cumulative)

prom_2023_cred_real<-mean(unc_ipc_14 %>% filter(fecha < as.Date("2023-12-31")) %>% pull(credito_devengado_real))
prom_2024_cred_real<-mean(unc_ipc_14 %>% filter(fecha > as.Date("2023-12-31")) %>% pull(credito_devengado_real))

unc_ipc_14_15_16 %>% 
  ggplot(aes(x = fecha, y = credito_devengado_real)) +
  geom_bar(stat = "identity", fill = "blue", width = 20) +  # set width to 1 to fill the entire day
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = c(0.01,0.01)) +  # set date breaks and labels
  scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
  geom_text(aes(label = round(credito_devengado_real, 0)), vjust = -0.5) +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Mes", y = "Devengado en $ (millones)", title = "Crédito mensual devengado a la UNC 2023-2024 actividad 14+15+16\n (funcionamiento+salud+CyT) ajustado por IPC ($ de febrero de 2024)")
ggsave("plot_unc_14_15_16_real.png", width = 10, height = 6, dpi = 300)

unc_ipc_14 %>% 
  ggplot(aes(x = fecha, y = credito_devengado)) +
  geom_bar(stat = "identity", fill = "blue", width = 20) +  # set width to 1 to fill the entire day
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = c(0.01,0.01)) +  # set date breaks and labels
    scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
    geom_text(aes(label = round(credito_devengado, 0)), vjust = -0.5) +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Mes", y = "Devengado en $ (millones)", title = "Crédito mensual devengado a la UNC 2023-2024 actividad 14\n (funcionamiento)")
ggsave("plot_unc_14_nominal.png", width = 10, height = 6, dpi = 300)



unc_ipc_14 %>% 
  ggplot(aes(x = fecha, y = credito_devengado_real)) +
  geom_bar(stat = "identity", fill = "blue", width = 20) +  # set width to 1 to fill the entire day
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = c(0.01,0.01)) +  # set date breaks and labels
    scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
    geom_text(aes(label = round(credito_devengado_real, 0)), vjust = -0.5) +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Mes", y = "Devengado en $ (millones)", title = "Crédito mensual devengado a la UNC 2023-2024 actividad 14\n (funcionamiento) ajustado por IPC ($ de febrero de 2024)")
ggsave("plot_unc_14_real.png", width = 10, height = 6, dpi = 300)

unc_ipc_14 %>% 
  ggplot(aes(x = fecha, y = credito_devengado_real)) +
  geom_bar(stat = "identity", fill = "blue", width = 20) +  # set width to 1 to fill the entire day
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = c(0.01,0.01)) +  # set date breaks and labels
  #add red horizontal line at y = mean(credito_devengado_real) for 2023
    geom_hline(yintercept = prom_2023_cred_real, color = "red",, linetype = "dashed") +
    #Add text label at y = mean(credito_devengado_real) for 2023
    geom_text(aes(x = as.Date("2023-11-01"), y = prom_2023_cred_real, label = paste("Promedio 2023:\n",round(prom_2023_cred_real, 0))), vjust = -0.5,color="red") +
    scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
    geom_text(aes(label = round(credito_devengado_real, 0)), vjust = -0.5) +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Mes", y = "Devengado en $ (millones)", title = "Crédito mensual devengado a la UNC 2023-2024 actividad 14\n (funcionamiento) ajustado por IPC ($ de febrero de 2024)")
ggsave("plot_unc_14_real_promanual.png", width = 10, height = 6, dpi = 300)

#For month 2 in 2024, add a red stacked bar with the value of credito_devengado_real for that month *1.7
# Combine the original data and the increased data
combined_data <- unc_ipc_14 %>% 
  mutate(type = "original") %>% 
  bind_rows(
    unc_ipc_14 %>% 
      filter(year(fecha) == 2024, month(fecha) == 2) %>% 
      mutate(credito_devengado_real = credito_devengado_real * 0.7, type = "increased")
  )
# Create the plot
combined_data %>% 
  group_by(fecha) %>% 
  mutate(cumulative_credito = cumsum(credito_devengado_real)) %>% 
  ggplot(aes(x = fecha, y = credito_devengado_real, fill = type)) +
  geom_bar(stat = "identity", width = 20) +
  scale_fill_manual(values = c("red","blue")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = c(0.01,0.01)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
  geom_text(aes(y = cumulative_credito, label = round(cumulative_credito, 0)), vjust = -0.5) +
  theme_light(base_size = 14) +
  labs(x = "Mes", y = "Devengado en $ (millones)", title = "Crédito mensual devengado a la UNC 2023-2024 actividad 14\n (funcionamiento) ajustado por IPC ($ de febrero de 2024)\nAumento del 70% propuesto por el gobierno en rojo") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1))
# Save the plot
ggsave("plot_unc_14_70p.png", width = 10, height = 6, dpi = 300)

unc_ipc_16 %>% 
  ggplot(aes(x = fecha, y = credito_devengado)) +
  geom_bar(stat = "identity", fill = "blue", width = 20) +  # set width to 1 to fill the entire day
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = c(0.01,0.01)) +  # set date breaks and labels
    scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
    geom_text(aes(label = round(credito_devengado, 0)), vjust = -0.5) +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Mes", y = "Devengado en $ (millones)", title = "Crédito mensual devengado a la UNC 2023-2024 actividad 16 (CyT)")
ggsave("plot_unc_16_nominal.png", width = 10, height = 6, dpi = 300)



unc_ipc_16 %>% 
  ggplot(aes(x = fecha, y = credito_devengado_real)) +
  geom_bar(stat = "identity", fill = "blue", width = 20) +  # set width to 1 to fill the entire day
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = c(0.01,0.01)) +  # set date breaks and labels
    scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
    geom_text(aes(label = round(credito_devengado_real, 0)), vjust = -0.5) +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Mes", y = "Devengado en $ (millones)", title = "Crédito mensual devengado a la UNC 2023-2024 actividad 16\n (CyT) ajustado por IPC ($ de febrero de 2024)")
ggsave("plot_unc_16_real.png", width = 10, height = 6, dpi = 300)


unc_ipc_all<-dataed %>% 
  mutate(fecha = as.Date(fecha)) %>%  # convert to date if not already
  filter(subparcial_desc=="Universidad Nacional de Córdoba") %>% 
  group_by(fecha) %>% 
  summarise(credito_devengado = sum(credito_devengado)) %>%
    left_join(ipc, by = "fecha") %>%
  mutate(credito_devengado_real = credito_devengado/cumulative)

unc_ipc_all %>% filter(fecha<as.Date("2024-03-01")) %>%
  ggplot(aes(x = fecha, y = credito_devengado_real)) +
  geom_bar(stat = "identity", fill = "blue", width = 20) +  # set width to 1 to fill the entire day
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = c(0.01,0.01)) +  # set date breaks and labels
  geom_text(aes(label = round(credito_devengado_real, 0)), vjust = -0.5) +
   scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Mes", y = "Devengado en $ (millones)", title = "Crédito mensual devengado a la UNC 2023-2024\najustado por IPC ($ de febrero de 2024)")
ggsave("plot_unc_all_real.png", width = 10, height = 6, dpi = 300)

unc_ipc_all %>% filter(fecha<as.Date("2024-03-01")) %>%
  ggplot(aes(x = fecha, y = credito_devengado)) +
  geom_bar(stat = "identity", fill = "blue", width = 20) +  # set width to 1 to fill the entire day
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = c(0.01,0.01)) +  # set date breaks and labels
  geom_text(aes(label = round(credito_devengado, 0)), vjust = -0.5) +
   scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Mes", y = "Devengado en $ (millones)", title = "Crédito mensual devengado a la UNC 2023-2024")
ggsave("plot_unc_all_nominal.png", width = 10, height = 6, dpi = 300)

combined_data_all <- unc_ipc_all %>% 
  mutate(type = "original") %>% 
  bind_rows(
    unc_ipc_all %>% 
      filter(year(fecha) == 2024, month(fecha) == 2) %>% 
      mutate(credito_devengado_real = 268.8, type = "increased")
  )

combined_data_all %>% filter(fecha<as.Date("2024-03-01")) %>%
  group_by(fecha) %>% 
  mutate(cumulative_credito = cumsum(credito_devengado_real),
         vjust = if_else(type == "original" & fecha==as.Date("2024-02-01"), 1.5, -0.5),
         tcolor = if_else(type == "original" & fecha==as.Date("2024-02-01"), "white", "black")) %>% 
  ggplot(aes(x = fecha, y = credito_devengado_real, fill = type)) +
  geom_bar(stat = "identity", width = 20) +
  scale_fill_manual(values = c("red","blue")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = c(0.01,0.01)) +  # set date breaks and labels
  scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
  geom_text(aes(y = cumulative_credito, label = round(cumulative_credito, 0), vjust = vjust, color = tcolor)) +
  scale_color_manual(values = c("black", "white")) +
  theme_light(base_size = 14) +
  labs(x = "Mes", y = "Devengado en $ (millones)", title = "Crédito mensual devengado a la UNC 2023-2024 ajustado por IPC\n($ de febrero de 2024). Aumento del 70% propuesto por el gobierno en rojo") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1))

# Save the plot
ggsave("plot_unc_all_70p.png", width = 10, height = 6, dpi = 300)



#TOTAL NO SALARIAL
unc_ipc_all<-dataed %>% 
  mutate(fecha = as.Date(fecha)) %>%  # convert to date if not already
  filter(actividad_id %notin% c(12,13),subparcial_desc=="Universidad Nacional de Córdoba") %>% 
  group_by(fecha) %>% 
  summarise(credito_devengado = sum(credito_devengado)) %>%
    left_join(ipc, by = "fecha") %>%
  mutate(credito_devengado_real = credito_devengado/cumulative)

unc_ipc_all %>% 
  ggplot(aes(x = fecha, y = credito_devengado_real)) +
  geom_bar(stat = "identity", fill = "blue", width = 20) +  # set width to 1 to fill the entire day
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = c(0.01,0.01)) +  # set date breaks and labels
  geom_text(aes(label = round(credito_devengado_real, 0)), vjust = -0.5) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Mes", y = "Devengado en $ (millones)", title = "Crédito mensual devengado a la UNC 2023-2024 no salarial\najustado por IPC ($ de febrero de 2024)")
ggsave("plot_unc_nosalarial_real.png", width = 10, height = 6, dpi = 300)


#TOTAL SALARIAL
unc_ipc_all<-dataed %>% 
  mutate(fecha = as.Date(fecha)) %>%  # convert to date if not already
  filter(actividad_id %in% c(12,13),subparcial_desc=="Universidad Nacional de Córdoba") %>% 
  group_by(fecha) %>% 
  summarise(credito_devengado = sum(credito_devengado)) %>%
    left_join(ipc, by = "fecha") %>%
  mutate(credito_devengado_real = credito_devengado/cumulative)

unc_ipc_all %>% 
  ggplot(aes(x = fecha, y = credito_devengado_real)) +
  geom_bar(stat = "identity", fill = "blue", width = 20) +  # set width to 1 to fill the entire day
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = c(0.01,0.01)) +  # set date breaks and labels
  geom_text(aes(label = round(credito_devengado_real, 0)), vjust = -0.5) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Mes", y = "Devengado en $ (millones)", title = "Crédito mensual devengado a la UNC 2023-2024 salarial\najustado por IPC ($ de febrero de 2024)")
ggsave("plot_unc_salarial_real.png", width = 10, height = 6, dpi = 300)



#Count different dates 
data2024ed %>% filter(impacto_presupuestario_mes==3, subparcial_desc=="Universidad Nacional de Córdoba" ) %>% pull(impacto_presupuestario_fecha)
data2023ed %>% filter(impacto_presupuestario_mes==3, subparcial_desc=="Universidad Nacional de Córdoba" ) %>% pull(impacto_presupuestario_fecha)