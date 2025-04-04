library(dplyr)
library(jsonlite)
library(lubridate)
library(ggplot2)
library(zoo)
library(tidyverse)
`%notin%` <- Negate(`%in%`)

mutate_month <- function(data, source_month, target_month, year) {
  # Group by the first 49 columns and perform operations
  data <- data %>%
    group_by(across(1:49)) %>% # Group by the first 49 columns
    mutate(
      is_duplicate = duplicated(pick(everything())), # Check for duplicates within the group
      min_credito = min(credito_devengado, na.rm = TRUE) # Calculate the minimum value within the group
    ) %>%
    ungroup() %>%
    mutate(
      fecha = ifelse(
        actividad_id %in% c(12, 13, 14, 15, 16) &
        impacto_presupuestario_fecha >= as.Date(paste0(year, "-", source_month, "-01")) &
        impacto_presupuestario_fecha <= as.Date(paste0(year, "-", source_month, "-31")) &
        is_duplicate & credito_devengado == min_credito,
        as.Date(paste(year, target_month, "01", sep = "-"), format = "%Y-%m-%d"),
        fecha
      )
    ) 

  # Print how many rows were changed
  count_changed_rows <- sum(
    data$actividad_id %in% c(12, 13, 14, 15, 16) &
      data$impacto_presupuestario_fecha >= as.Date(paste0(year, "-", source_month, "-01")) &
      data$impacto_presupuestario_fecha <= as.Date(paste0(year, "-", source_month, "-31")) &
      data$is_duplicate &
      data$credito_devengado == min(data$credito_devengado, na.rm = TRUE)
  )
  print(paste("Rows changed from", source_month, "to", target_month, ":", count_changed_rows))
  # Return the modified dataframe
  return(data %>% select(-is_duplicate, -min_credito))
}

generate_government_column <- function(df) {

  # Add the "gobierno" column based on the date ranges
  df <- df %>%
    mutate(gobierno = case_when(
      fecha >= as.Date("2017-01-01") & fecha <= as.Date("2019-11-30") ~ "Macri",
      fecha >= as.Date("2019-12-01") & fecha <= as.Date("2023-11-30") ~ "Fernández",
      fecha >= as.Date("2023-12-01") & fecha <= as.Date("2027-11-30") ~ "Milei",
      TRUE ~ "other" # Optional: Handle dates outside these ranges
    ))
  
  # Return the modified dataframe
  return(df)
}

#Load IPC from file
ipc <- read.csv("ipc/ipc.csv")
#ipc$cumulative <- cumprod(1+ipc$ipc/100)/(1+ipc$ipc[1]/100)
ipc$cumulative<-ipc$ipc_indice
#Divide cumulative by the value corresponding to max month
max_mes <- as.Date(max(ipc$fecha))
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
data2025 <- fromJSON("datos/2025.json") 


#Join into data
data <- as.data.frame(rbind(data2017, data2018, data2019, data2020, data2021, data2022, data2023, data2024, data2025))
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

data<-generate_government_column(data)
color_mapping <- c(
  "Macri" = "#d4d400", # Yellow
  "Fernández" = "#31ffff", # Cyan
  "Milei" = "#a8009d" # Violet
)

dark_color_mapping <- c(
  "Macri" = "#9c9c00", # Yellow
  "Fernández" = "#0078af", # Cyan
  "Milei" = "#700069" # Violet
)

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


#PROYECCIÓN 2025
data_mensual <- data %>% 
  ungroup() %>%
  mutate(fecha = as.Date(paste(impacto_presupuestario_anio, impacto_presupuestario_mes, "01", sep = "-"), format = "%Y-%m-%d")) %>% 
  filter(fecha <= max_mes) %>% 
  group_by(fecha, gobierno, impacto_presupuestario_anio,impacto_presupuestario_mes) %>% 
  summarise(credito_devengado = round(sum(credito_devengado), 0), credito_devengado_real = round(sum(credito_devengado_real), 0),cumulative=mean(cumulative))

last <- tail(data_mensual, 1)
first_proj_month <- as.Date(paste0(year(last$fecha), "-", month(last$fecha) + 1, "-01"))
last_month <- as.Date(paste0(year(last$fecha), "-12-01"))
n_months <- interval(first_proj_month, last_month) %/% months(1) + 1
proy_anio <- year(last$fecha)

resto <- data.frame(
  fecha = seq(first_proj_month, last_month, by = "months"),
  impacto_presupuestario_mes = month(seq(first_proj_month, last_month, by = "months")),
  impacto_presupuestario_anio = proy_anio,
  credito_devengado = 0,
  credito_devengado_real = rep(last$credito_devengado_real, n_months)
)
# If fecha is 2024-12-01 or 2024-06-01 and credito_devengado is 0, then modify the value of credito_devengado_real to be 1.6 times the previous value
resto <- resto %>% 
  mutate(credito_devengado_real = ifelse(fecha %in% as.Date(c("2024-12-01", "2024-06-01")) & credito_devengado == 0, credito_devengado_real * 1.5, credito_devengado_real))

resto<-generate_government_column(resto)
resto2 <- merge(resto, ipc25_18, by.x = "fecha", by.y = "fecha") %>% 
          select(-ipc) %>%
          mutate(credito_devengado = round(if_else(credito_devengado == 0, credito_devengado_real*cumulative, credito_devengado),0))
data_mensual_2 <- rbind(data_mensual, resto2)

data_anual <- data_mensual_2 %>% 
  group_by(impacto_presupuestario_anio) %>% 
  summarise(gobierno= first(gobierno),credito_devengado = sum(credito_devengado), credito_devengado_real = sum(credito_devengado_real)) %>%
  mutate(fecha = as.Date(paste(impacto_presupuestario_anio, "01", "01", sep = "-"), format = "%Y-%m-%d"))
#########################


#Calcular data mensual
data_mensual <- data %>% 
  group_by(fecha,impacto_presupuestario_anio,gobierno) %>%
  filter(fecha<=max_mes) %>%
  summarise(credito_devengado = round(sum(credito_devengado),0), credito_devengado_real = round(sum(credito_devengado_real),0))

View(data_mensual)

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


#Remover efecto de aguinaldos para meses de junio y diciembre
min_mes=as.Date("2023-01-01")
data_mensual_noagui <- data_mensual %>% 
  filter(fecha<=max_mes & fecha >= min_mes) %>%
    mutate(credito_devengado_real = ifelse(month(fecha) == 6 & credito_devengado_real > 0, credito_devengado_real / 1.4, credito_devengado_real)) %>%
  mutate(credito_devengado_real = ifelse(month(fecha) == 12 & credito_devengado_real > 0, credito_devengado_real /1.4, credito_devengado_real)) %>%
  mutate(credito_devengado_real = ifelse(credito_devengado_real < 0, 0, credito_devengado_real)) %>%
  mutate(credito_devengado_real = round(credito_devengado_real,0))

# Calculate 3-month averages
three_month_averages <- data_mensual_noagui %>%
  mutate(quarter = paste0(year(fecha), "-Q", quarter(fecha))) %>% # Add a quarter column
  group_by(quarter) %>%
  summarise(average_credito_devengado_real = mean(credito_devengado_real, na.rm = TRUE)) %>%
  ungroup()

# Add 3-month averages to the original dataset
data_with_three_month_averages <- data_mensual_noagui %>%
  mutate(quarter = paste0(year(fecha), "-Q", quarter(fecha))) %>% # Add a quarter column
  left_join(three_month_averages, by = "quarter") # Join 3-month averages back to the dataset

#Plot monthly data show every year in x axis. Fill columns 2017-2019 in yellow (d4d400), 2020-2023 in cyan (31ffff) and 2024-2025 in purple (a8009d)
ggplot(data_with_three_month_averages, aes(x=as.factor(fecha), y=credito_devengado_real, fill=as.factor(gobierno), color = as.factor(gobierno))) +
  geom_vline(
    xintercept = as.factor("2024-04-01"), # Convert the date to match the x-axis factor
    color = "red", linetype = "dashed", size = 0.8
  ) +
    geom_vline(
    xintercept = as.factor("2024-10-01"), # Convert the date to match the x-axis factor
    color = "red", linetype = "dashed", size = 0.8
  ) +
  geom_bar(stat = "identity", alpha = 0.5)+
  labs(title = "Universidades Nacionales: Presupuesto anual devengado",
      subtitle=paste0("Ajustado por inflación (IPC). En pesos de ",max_mes),
      x = "Año-Mes",
      y = paste0("Credito anual devengado\n(millones de $ de ",max_mes,")"),
      fill = "Gobierno", color = "Gobierno" # Change legend title to "Gobierno"
      ) +
  scale_fill_manual(values = color_mapping) + # Use the same color mapping for bars
  scale_color_manual(values = dark_color_mapping) +
  theme_light(base_size=13) +
  geom_text(aes(y = credito_devengado_real, label = round(credito_devengado_real, 0)), vjust = 0.5,size=3, hjust = 1.5, angle = 90, color = "black") +
  geom_line(
    aes(y = average_credito_devengado_real, group = 1),
    size = 1
    ) +
  scale_x_discrete(
    labels = function(x) format(as.Date(x), "%Y-%m") # Format factor levels as YYYY-MM
  ) +
  #scale y axis to show values in millions
  scale_y_continuous(breaks = seq(0,max(data_mensual_noagui$credito_devengado_real)*1.1,100000) ,labels = scales::comma, limits = c(0, max(data_mensual_noagui$credito_devengado_real) * 1.1), expand = c(0,0)) +
  coord_cartesian(ylim = c(200000, max(data_mensual_noagui$credito_devengado_real) * 1.1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),legend.position = "top", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  labs(caption = paste0("Se ajustó el crédito devengado en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor).\nEn millones de pesos de ", max_mes, ", promedios trimestrales mostrados como una línea. Meses con aguinaldo fueron normalizados.\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario "))+
annotate(
    "text",
    x = as.factor("2024-04-01"), y = 400000, # Adjust `y` value as needed
    label = "1ra marcha\nuniversitaria",
    color = "red", size = 4, hjust = 0, angle = 90
  ) +
  annotate(
    "text",
    x = as.factor("2024-10-01"), y = 400000, # Adjust `y` value as needed
    label = "2da marcha\nuniversitaria",
    color = "red", size = 4, hjust = 0, angle = 90
  )
ggsave(paste0("plots/presupuesto_mensual_2023-",proy_anio,".png"),width = 10, height = 10, units = "in",dpi=300)





#Plot annual data show every year in x axis. Fill columns 2017-2019 in yellow, 2020-2023 in cyan and 2024-2025 in purple
ggplot(data_anual, aes(x=as.factor(impacto_presupuestario_anio), y=credito_devengado_real, fill=as.factor(gobierno))) +
  geom_bar(stat="identity") +
  labs(title = "Universidades Nacionales: Presupuesto anual devengado",subtitle=paste0("Ajustado por inflación (IPC). En pesos de ",max_mes),
       x = "Año",
       y = paste0("Credito anual devengado\n(millones de $ de ",max_mes,")"),
       fill = "Gobierno") +
  scale_fill_manual(values = color_mapping) + 
  theme_light(base_size=14) +
    geom_text(aes(y = credito_devengado_real, label = round(credito_devengado_real, 0)), vjust = -0.5,size=5) +
  #scale y axis to show values in millions
  scale_y_continuous(labels = scales::comma, limits = c(NA, max(data_anual$credito_devengado_real) * 1.1)) +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  labs(caption = paste0("Se ajustó el crédito devengado en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor).\nSe asume ajuste por IPC para los meses faltantes de ",proy_anio,". En millones de pesos de ", max_mes, ", montos anualizados.\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario "))
ggsave(paste0("plots/presupuesto_anual_2017-",proy_anio,".png"),width = 10, height = 10, units = "in",dpi=300)

#Repeat plot but with 2013 == 100
data_anual_100 <- data_anual %>% 
  mutate(credito_devengado_real_base100 = credito_devengado_real/credito_devengado_real[1]*100)

View(data_anual_100)

#Plot annual data show every year in x axis. Fill columns 2017-2019 in yellow, 2020-2023 in cyan and 2024-2025 in purple
ggplot(data_anual_100, aes(x=as.factor(impacto_presupuestario_anio), y=credito_devengado_real_base100, fill=as.factor(gobierno))) +
  geom_bar(stat="identity") +
  labs(title = "Universidades Nacionales: Presupuesto anual devengado",subtitle="Ajustado por inflación (IPC). Base 100 = 2017",
       x = "Año",
       y = "Credito anual devengado\n(base 100 = 2017)",
       fill = "Gobierno") +
  scale_fill_manual(values = color_mapping) + 
  theme_light(base_size=14) +
    geom_text(aes(y = credito_devengado_real_base100, label = round(credito_devengado_real_base100, 0)), vjust = -0.5,size=5) +
  #scale y axis to show values in millions
  scale_y_continuous(labels = scales::comma, limits = c(NA, max(data_anual_100$credito_devengado_real_base100) * 1.1)) +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  labs(caption = paste0("Se ajustó el crédito devengado en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor).\nSe asume aumentos equivalentes al IPC para septiembre-diciembre 2024 (incluyendo aguinaldos).\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario "))
ggsave(paste0("plots/presupuesto_anual_100_2017-",proy_anio,".png"),width = 10, height = 10, units = "in",dpi=300)

#Presupuesto por número de estudiantes
anios<-c(2017,2018,2019,2020,2021,2022,2023,2024,2025)
estudiantes<-c(2005152,2071270,2187292,2318255,2549789,2540854,2730754,2730754,2730754)
df_estudiantes<-data.frame(anio=anios,estudiantes=estudiantes) %>%
  mutate(fecha = as.Date(paste(anio, "01", "01", sep = "-"), format = "%Y-%m-%d"))
df_estudiantes<-generate_government_column(df_estudiantes)

#Create barplot of estudiantes
ggplot(df_estudiantes, aes(x=as.factor(anio), y=estudiantes, fill=as.factor(gobierno))) +
  geom_bar(stat="identity") +
  labs(title = "Universidades Nacionales: Número de estudiantes",subtitle="Anuario SPU",
       x = "Año",
       y = "Número de estudiantes",
       fill = "Gobierno") +
  scale_fill_manual(values = color_mapping) + 
  theme_light(base_size=14) +
    geom_text(aes(y = estudiantes, label = round(estudiantes, 0)), vjust = -0.5,size=5) +
  #scale y axis to show values in millions
  scale_y_continuous(labels = scales::comma, limits = c(NA, max(df_estudiantes$estudiantes) * 1.1)) +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  labs(caption = paste0("Número de estudiantes según Anuario SPU. Dato 2024 y 2025 estimado como = 2023.\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario "))
ggsave("plots/estudiantes_2017-2025.png",width = 10, height = 10, units = "in",dpi=300)


data_anual_estudiantes<-merge(data_anual,df_estudiantes,by = c("fecha","gobierno"))
data_anual_estudiantes<-data_anual_estudiantes %>% 
  mutate(credito_devengado_real_por_estudiante = credito_devengado_real/estudiantes) %>%
  mutate(credito_devengado_real_por_est_100 = credito_devengado_real_por_estudiante/credito_devengado_real_por_estudiante[1]*100)


#Plot annual data show every year in x axis. Fill columns 2017-2019 in yellow, 2020-2023 in cyan and 2024 in purple
ggplot(data_anual_estudiantes, aes(x=as.factor(impacto_presupuestario_anio), y=credito_devengado_real_por_est_100, fill=as.factor(gobierno))) +
  geom_bar(stat="identity") +
  labs(title = "Universidades Nacionales: Presupuesto anual devengado",subtitle="Ajustado por inflación y número de estudiantes. Base 100 = 2017",
       x = "Año",
       y = "Credito anual devengado por estudiante\n(base 100 = 2017)\n",
       fill = "Gobierno") +
  scale_fill_manual(values = color_mapping) + 
  theme_light(base_size=14) +
    geom_text(aes(y = credito_devengado_real_por_est_100, label = round(credito_devengado_real_por_est_100, 0)), vjust = -0.5,size=5) +
  #scale y axis to show values in millions
  scale_y_continuous(labels = scales::comma, limits = c(NA, max(data_anual_estudiantes$credito_devengado_real_por_est_100) * 1.1)) +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  labs(caption = paste0("Se ajustó el crédito devengado en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor).\nSe asume aumentos equivalentes al IPC para 2025 (incluyendo aguinaldos).\nNúmero de estudiantes según Anuario SPU. Dato 2024 y 2025 estimado como = 2023. Se utiliza base 100 = 2017.\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario "))
ggsave("plots/presupuesto_anual_porest_100_2017-2025.png",width = 10, height = 6, units = "in",dpi=300)



