library(dplyr)
library(jsonlite)
library(lubridate)
library(ggplot2)
library(zoo)
library(scales)
library(tidyverse)
`%notin%` <- Negate(`%in%`)

plot_budget_data <- function(data, include_three_month_avg = TRUE, title = "Presupuesto mensual devengado", 
                             x_axis_title = "Año-Mes", y_axis_title = "Crédito mensual devengado (millones de $)", 
                             output_file = "plot.png", max_mes, color_mapping= color_mapping, dark_color_mapping = dark_color_mapping, 
                             coord_cartesian_min = 200000, breaks_y=10000, marcha_y= 400000,
                             caption = paste0(
        "Se ajustó el crédito devengado (prog 26) en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor).\n",
        "En millones de pesos de ", max_mes, ", promedios trimestrales mostrados como una línea. Meses con aguinaldo fueron normalizados.\n",
        "Por Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario"
      )
                             ) {
  
  # Base plot
  p <- ggplot(data, aes(x = as.factor(fecha), y = credito_devengado_real, fill = as.factor(gobierno), color = as.factor(gobierno))) +
    geom_bar(stat = "identity", alpha = 0.5) +
    geom_vline(
      xintercept = as.factor("2024-04-01"), # Convert the date to match the x-axis factor
      color = "red", linetype = "dashed", size = 0.8
    ) +
    geom_vline(
      xintercept = as.factor("2024-10-01"), # Convert the date to match the x-axis factor
      color = "red", linetype = "dashed", size = 0.8
    ) +
    labs(
      title = title,
      subtitle = paste0("Ajustado por inflación (IPC). En pesos de ", max_mes),
      x = x_axis_title,
      y = y_axis_title,
      fill = "Gobierno", color = "Gobierno"
    ) +
    scale_fill_manual(values = color_mapping) + # Use the same color mapping for bars
    scale_color_manual(values = dark_color_mapping) +
    theme_light(base_size = 13) +
    geom_text(aes(y = credito_devengado_real, label = round(credito_devengado_real, 0)), 
              vjust = 0.5, size = 3, hjust = 1.5, angle = 90, color = "black") +
    scale_x_discrete(
      labels = function(x) format(as.Date(x), "%Y-%m") # Format factor levels as YYYY-MM
    ) +
    scale_y_continuous(
      breaks = seq(0, max(data$credito_devengado_real) * 1.1, breaks_y),
      labels = scales::comma,
      limits = c(0, max(data$credito_devengado_real) * 1.1),
      expand = c(0, 0)
    ) +
    coord_cartesian(ylim = c(coord_cartesian_min, max(data$credito_devengado_real) * 1.1)) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      legend.position = "top",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    labs(
      caption = caption
    ) +
    annotate(
      "text",
      x = as.factor("2024-04-01"), y = marcha_y, # Adjust `y` value as needed
      label = "1ra marcha\nuniversitaria",
      color = "red", size = 4, hjust = 0, angle = 90
    ) +
    annotate(
      "text",
      x = as.factor("2024-10-01"), y = marcha_y, # Adjust `y` value as needed
      label = "2da marcha\nuniversitaria",
      color = "red", size = 4, hjust = 0, angle = 90
    )
  
  # Add 3-month average line if requested
  if (include_three_month_avg) {
    p <- p + geom_line(
      aes(y = average_credito_devengado_real, group = 1),
      size = 1
    )
  }
  
  # Save the plot
  ggsave(output_file, plot = p, width = 10, height = 10, units = "in", dpi = 300)
  
  # Return the plot object (optional, for further customization or display)
  return(p)
}

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

calculate_three_month_averages <- function(data) {
  # Calculate 3-month averages
  three_month_averages <- data %>%
    mutate(quarter = paste0(year(fecha), "-Q", quarter(fecha))) %>% # Add a quarter column
    group_by(quarter) %>%
    summarise(average_credito_devengado_real = mean(credito_devengado_real, na.rm = TRUE)) %>%
    ungroup()
  
  # Add 3-month averages to the original dataset
  data_with_three_month_averages <- data %>%
    mutate(quarter = paste0(year(fecha), "-Q", quarter(fecha))) %>% # Add a quarter column
    left_join(three_month_averages, by = "quarter") # Join 3-month averages back to the dataset
  
  # Return the modified dataset
  return(data_with_three_month_averages)
}

remove_aguinaldo_effect <- function(data, max_mes, min_mes, value=1.4) {
  library(dplyr)
  library(lubridate)
  
  # Filter and adjust the data
  data_no_aguinaldo <- data %>%
    filter(fecha <= max_mes & fecha >= min_mes) %>%
    mutate(
      credito_devengado_real = ifelse(
        month(fecha) == 6 & credito_devengado_real > 0,
        credito_devengado_real / value,
        credito_devengado_real
      ),
      credito_devengado_real = ifelse(
        month(fecha) == 12 & credito_devengado_real > 0,
        credito_devengado_real / value,
        credito_devengado_real
      ),
      credito_devengado_real = ifelse(
        credito_devengado_real < 0,
        0,
        credito_devengado_real
      ),
      credito_devengado_real = round(credito_devengado_real, 0)
    )
  
  # Return the modified dataset
  return(data_no_aguinaldo)
}

plot_annual_budget <- function(data, title = "Universidades Nacionales: Presupuesto anual devengado", 
                               caption = "Se ajustó el crédito devengado para ciencia (act 16) en cada mes por inflación, utilizando el IPC-INDEC.\nSe toma el promedio para el año proyectado y se asume ajuste por IPC para los meses faltantes.\nEn millones de pesos de la fecha máxima, montos anualizados.\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario",
                               output_file = "plots/presupuesto_anual.png", 
                               max_mes = max_mes, proy_anio = 2025, color_mapping = color_mapping) {
  library(ggplot2)
  library(scales)
  
  ggplot(data, aes(x = as.factor(impacto_presupuestario_anio), y = credito_devengado_real, fill = as.factor(gobierno))) +
    geom_bar(stat = "identity") +
    labs(
      title = title,
      subtitle = paste0("Ajustado por inflación (IPC). En pesos de ", max_mes),
      x = "Año",
      y = paste0("Crédito anual devengado\n(millones de $ de ", max_mes, ")"),
      fill = "Gobierno"
    ) +
    scale_fill_manual(values = color_mapping) +
    theme_light(base_size = 14) +
    geom_text(aes(y = credito_devengado_real, label = round(credito_devengado_real, 0)), vjust = -0.5, size = 5) +
    scale_y_continuous(labels = scales::comma, limits = c(NA, max(data$credito_devengado_real) * 1.1)) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    labs(caption = caption)
  
  # Save the plot
  ggsave(output_file, width = 10, height = 10, units = "in", dpi = 300)
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


#Chequeo devengado salarial Universidad de Buenos Aires para meses 3,4,5 y 6 de 2023
salario_BSAS <- data %>% filter(
  subparcial_desc == "Universidad de Buenos Aires" &
  programa_id == 26 &
  actividad_id %in% c(12) &
  impacto_presupuestario_anio == 2023 &
  impacto_presupuestario_mes %in% c(3, 4, 5, 6)
) 

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

generate_projection <- function(data, ipc25_18, actividad_ids = NULL, adjust_specific_months = FALSE, adjustment_factor = 1.5, use_average = FALSE) {
  
  # Filter data if actividad_ids is provided
  if (!is.null(actividad_ids) && length(actividad_ids) > 0) {
    data <- data %>% filter(actividad_id %in% actividad_ids)
  }

data_mensual<-data %>% 
  ungroup() %>%
  mutate(fecha = as.Date(paste(impacto_presupuestario_anio, impacto_presupuestario_mes, "01", sep = "-"), format = "%Y-%m-%d")) %>% 
  filter(fecha <= max_mes) %>% 
  group_by(fecha, gobierno, impacto_presupuestario_anio,impacto_presupuestario_mes) %>% 
  summarise(credito_devengado = round(sum(credito_devengado), 0), credito_devengado_real = round(sum(credito_devengado_real), 0),cumulative=mean(cumulative))

  # Get the last year of data_mensual
  last_year <- max(data_mensual$impacto_presupuestario_anio)
  last_year_data <- data_mensual %>% filter(impacto_presupuestario_anio == last_year)
  
  # Determine the value to use for projections
  projection_value <- if (use_average) {
    mean(last_year_data$credito_devengado_real, na.rm = TRUE)
  } else {
    tail(last_year_data$credito_devengado_real, 1)
  }
  
  # Define projection parameters
  last <- tail(data_mensual, 1)
  first_proj_month <- as.Date(paste0(year(last$fecha), "-", month(last$fecha) + 1, "-01"))
  last_month <- as.Date(paste0(year(last$fecha), "-12-01"))
  n_months <- interval(first_proj_month, last_month) %/% months(1) + 1
  proy_anio <- year(last$fecha)
  
  # Create projection data frame
  resto <- data.frame(
    fecha = seq(first_proj_month, last_month, by = "months"),
    impacto_presupuestario_mes = month(seq(first_proj_month, last_month, by = "months")),
    impacto_presupuestario_anio = proy_anio,
    credito_devengado = 0,
    credito_devengado_real = rep(projection_value, n_months)
  )
  
  # Adjust specific months if enabled
  if (adjust_specific_months) {
    resto <- resto %>% 
      mutate(credito_devengado_real = ifelse(
        fecha %in% as.Date(c("2024-12-01", "2024-06-01")) & credito_devengado == 0,
        credito_devengado_real * adjustment_factor,
        credito_devengado_real
      ))
  }
  
  # Add government column
  resto <- generate_government_column(resto)
  
  # Merge with IPC data and adjust values
  resto2 <- merge(resto, ipc25_18, by.x = "fecha", by.y = "fecha") %>% 
    select(-ipc) %>%
    mutate(credito_devengado = round(if_else(
      credito_devengado == 0,
      credito_devengado_real * cumulative,
      credito_devengado
    ), 0))
  
  # Combine with original data_mensual
  data_mensual_2 <- rbind(data_mensual, resto2)
  
  # Return the updated data
  return(data_mensual_2)
}

annualize <- function(data_mensual) {
  # Group by year and summarize data
  data_anual <- data_mensual %>%
    group_by(impacto_presupuestario_anio) %>%
    summarise(
      gobierno = first(gobierno),
      credito_devengado = sum(credito_devengado),
      credito_devengado_real = sum(credito_devengado_real),
      .groups = "drop"
    ) %>%
    mutate(fecha = as.Date(paste(impacto_presupuestario_anio, "01", "01", sep = "-"), format = "%Y-%m-%d"))
  
  # Return the annualized data
  return(data_anual)
}



#########################


#Calcular data mensual
data_mensual <- data %>% 
  group_by(fecha,impacto_presupuestario_anio,gobierno) %>%
  filter(fecha<=max_mes) %>%
  summarise(credito_devengado = round(sum(credito_devengado),0), credito_devengado_real = round(sum(credito_devengado_real),0))

View(data_mensual)

#check plot
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
max_mes=as.Date("2025-03-01")
data_mensual_noagui <- remove_aguinaldo_effect(data_mensual, max_mes, min_mes,1.4)

# Calculate 3-month averages
data_with_three_month_averages<- calculate_three_month_averages(data_mensual_noagui)

plot_budget_data(
  data = data_with_three_month_averages,
  include_three_month_avg = TRUE,
  title = "Universidades Nacionales: Presupuesto mensual total",
  output_file = paste0("plots/presupuesto_mensual_2023-",proy_anio,".png"),
  max_mes = as.Date("2025-03-01"),
  color_mapping = color_mapping,
  dark_color_mapping = dark_color_mapping,
  coord_cartesian_min = 200000, # Custom minimum value for coord_cartesian
  breaks_y = 100000 # Custom breaks for y-axis
)

#Ahora mensual no salarial y salarial
s_data_mensual <- data %>%
  mutate(salarial = ifelse(actividad_id %in% c(12, 13), "salarial", ifelse(actividad_id %in% c(14), "funcionamiento","no_salarial"))) %>%
  group_by(fecha, impacto_presupuestario_anio, gobierno, salarial) %>%
  summarise(
    credito_devengado = round(sum(credito_devengado), 0),
    credito_devengado_real = round(sum(credito_devengado_real), 0),
    .groups = "drop"
  )
  
#Salarial
#Remover efecto de aguinaldos para meses de junio y diciembre
s_data_mensual_noagui <- remove_aguinaldo_effect(s_data_mensual %>%
        filter(salarial=="salarial"), max_mes, min_mes,1.5)
# Calculate 3-month averages
s_data_with_three_month_averages<- calculate_three_month_averages(s_data_mensual_noagui)

plot_budget_data(
  data = s_data_with_three_month_averages,
  include_three_month_avg = TRUE,
  title = "Universidades Nacionales: Presupuesto mensual salarial",
  output_file = paste0("plots/presupuesto_mensual_salarial_2023-",proy_anio,".png"),
  max_mes = as.Date("2025-03-01"),
  color_mapping = color_mapping,
  dark_color_mapping = dark_color_mapping,
  coord_cartesian_min = 200000, # Custom minimum value for coord_cartesian
  breaks_y = 100000 # Custom breaks for y-axis
)

#NO salarial
ns_data_mensual_noagui <- s_data_mensual %>%
                mutate(salarial = ifelse(salarial=="salarial","salarial","no salarial")) %>%
                group_by(fecha, impacto_presupuestario_anio, gobierno, salarial) %>%
                summarise(credito_devengado_real=sum(credito_devengado_real)) %>%
                filter(salarial=="no salarial" & fecha<=max_mes & fecha>=min_mes)
ns_data_with_three_month_averages<- calculate_three_month_averages(ns_data_mensual_noagui)

plot_budget_data(
  data = ns_data_with_three_month_averages,
  include_three_month_avg = TRUE,
  title = "Universidades Nacionales: Presupuesto mensual no salarial",
  output_file = paste0("plots/presupuesto_mensual_nosalarial_2023-",proy_anio,".png"),
  max_mes = as.Date("2025-03-01"),
  color_mapping = color_mapping,
  dark_color_mapping = dark_color_mapping,
  coord_cartesian_min = 0, # Custom minimum value for coord_cartesian
  breaks_y = 10000, # Custom breaks for y-axis
  marcha_y= 52000,
  caption = paste0(
        "Se ajustó el crédito devengado (prog 26) en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor).\n",
        "Se incluyen todas las actividades excepto las salariales (12 y 13). Esto incluye funcionamiento, ciencia, salud, becas, extensión, etc.\nEn millones de pesos de ", max_mes, ", promedios trimestrales mostrados como una línea.\n",
        "Por Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario"
      )
)

#funcionamiento
ns_data_mensual_noagui <- s_data_mensual %>% 
                          filter(salarial=="funcionamiento" & fecha<=max_mes & fecha>=min_mes)
ns_data_with_three_month_averages<- calculate_three_month_averages(ns_data_mensual_noagui)

plot_budget_data(
  data = ns_data_with_three_month_averages,
  include_three_month_avg = TRUE,
  title = "Universidades Nacionales: Presupuesto mensual funcionamiento (act. 14)",
  output_file = paste0("plots/presupuesto_mensual_funcionamiento_2023-",proy_anio,".png"),
  max_mes = as.Date("2025-03-01"),
  color_mapping = color_mapping,
  dark_color_mapping = dark_color_mapping,
  coord_cartesian_min = 0, # Custom minimum value for coord_cartesian
  breaks_y = 10000, # Custom breaks for y-axis
  marcha_y= 42000,
  caption = paste0(
        "Se ajustó el crédito devengado (prog 26) en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor).\n",
        "Se incluye la actividad 14 (funcionamiento). Esto incluye funcionamiento, ciencia, salud, becas, extensión, etc.\nEn millones de pesos de ", max_mes, ", promedios trimestrales mostrados como una línea.\n",
        "Por Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario"
      )
)

##############
#DATA ANUAL
#############
data_mensual_2 <- generate_projection(data, ipc25_18, adjust_specific_months = TRUE, adjustment_factor = 1.4,use_average = TRUE)
data_anual <- annualize(data_mensual_2)
#ANUAL TOTAL
plot_annual_budget( data = data_anual, 
  title = "Universidades Nacionales: Presupuesto anual total devengado",
  caption = paste0("Se ajustó el crédito devengado (prog 26) en cada mes por inflación, utilizando el IPC-INDEC (índice de precios al consumidor).\nSe toma el promedio para ",proy_anio," y se asume ajuste por IPC para los meses faltantes.\nEn millones de pesos de ", max_mes, ", montos anualizados.\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario "),
  output_file = paste0("plots/presupuesto_anual_2017-",proy_anio,".png"),
  max_mes = max_mes,
  color_mapping = color_mapping
)

#ANUAL SALARIAL
data_mensual_2 <- generate_projection(data, ipc25_18,actividad_ids = c(12,13) ,adjust_specific_months = TRUE, adjustment_factor = 1.5,use_average = TRUE)
data_anual_salarial <- annualize(data_mensual_2)
plot_annual_budget( data = data_anual_salarial, 
  title = "Universidades Nacionales: Presupuesto anual salarial devengado",
  caption = paste0("Se ajustó el crédito salarial devengado (act 12 y 13) en cada mes por inflación, utilizando el IPC-INDEC.\nSe toma el promedio para ",proy_anio," y se asume ajuste por IPC para los meses faltantes (se toma en cuenta aguinaldos).\nEn millones de pesos de ", max_mes, ", montos anualizados.\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario "),
  output_file = paste0("plots/presupuesto_anual_salarial_2017-",proy_anio,".png"),
  max_mes = max_mes,
  color_mapping = color_mapping
)

#ANUAL FUNCIONAMIENTO
data_mensual_2 <- generate_projection(data, ipc25_18,actividad_ids = c(14) ,adjust_specific_months = FALSE, use_average = TRUE)
data_anual_func <- annualize(data_mensual_2)
plot_annual_budget( data = data_anual_func, 
  title = "Universidades Nacionales: Presupuesto anual devengado (funcionamiento)",
  caption = paste0("Se ajustó el crédito devengado para funcionamiento (act 14) en cada mes por inflación, utilizando el IPC-INDEC.\nSe toma el promedio para ",proy_anio," y se asume ajuste por IPC para los meses faltantes.\nEn millones de pesos de ", max_mes, ", montos anualizados.\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario "),
  output_file = paste0("plots/presupuesto_anual_funcionamiento_2017-",proy_anio,".png"),
  max_mes = max_mes,
  color_mapping = color_mapping
)

#ANUAL CIENCIA
data_mensual_2 <- generate_projection(data, ipc25_18,actividad_ids = c(16) ,adjust_specific_months = FALSE, use_average = TRUE)
data_anual_cyt <- annualize(data_mensual_2)
plot_annual_budget( data = data_anual_cyt, 
  title = "Universidades Nacionales: Presupuesto anual devengado (Ciencia)",
  caption = paste0(
    "Se ajustó el crédito devengado para ciencia (act 16) en cada mes por inflación, utilizando el IPC-INDEC.\n",     "Se toma el promedio para ", proy_anio, " y se asume ajuste por IPC para los meses faltantes.\n",     "En millones de pesos de ", max_mes, ", montos anualizados.\n",     "Por Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario"),
  output_file = paste0("plots/presupuesto_anual_ciencia_2017-", proy_anio, ".png"),
  max_mes = max_mes,
  color_mapping = color_mapping
)



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
  labs(caption = paste0("Se ajustó el crédito devengado (prog 26) en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor).\nSe asume aumentos equivalentes al IPC para septiembre-diciembre 2024 (incluyendo aguinaldos).\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario "))
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
  labs(title = "Universidades Nacionales: Presupuesto total anual devengado",subtitle="Ajustado por inflación y número de estudiantes. Base 100 = 2017",
       x = "Año",
       y = "Credito anual devengado por estudiante\n(base 100 = 2017)\n",
       fill = "Gobierno") +
  scale_fill_manual(values = color_mapping) + 
  theme_light(base_size=14) +
    geom_text(aes(y = credito_devengado_real_por_est_100, label = round(credito_devengado_real_por_est_100, 0)), vjust = -0.5,size=5) +
  #scale y axis to show values in millions
  scale_y_continuous(labels = scales::comma, limits = c(NA, max(data_anual_estudiantes$credito_devengado_real_por_est_100) * 1.1)) +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  labs(caption = paste0("Se ajustó el crédito devengado (prog 26) en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor).\nSe asume aumentos equivalentes al IPC para 2025 (incluyendo aguinaldos).\nNúmero de estudiantes según Anuario SPU. Dato 2024 y 2025 estimado como = 2023. Se utiliza base 100 = 2017.\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario "))
ggsave("plots/presupuesto_anual_porest_100_2017-2025.png",width = 10, height = 6, units = "in",dpi=300)



