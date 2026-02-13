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
                             coord_cartesian_min = 200000, breaks_y=10000, marcha_y= 400000, ancho = 10, alto = 10, base_size = 13, marcha_size =4,
                             caption = paste0(
        "Se ajustó el crédito devengado (prog 26) en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor).\n",
        "En millones de pesos de ", max_mes, ", promedios trimestrales mostrados como una línea. Meses con aguinaldo fueron normalizados.\n",
        "Por Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario"
      )
                             ) {
  
  # Base plot
  p <- ggplot(data, aes(x = as.factor(fecha), y = credito_devengado_real, fill = gobierno, color = gobierno)) +
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
    theme_light(base_size = base_size) +
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
      color = "red", size = marcha_size, hjust = 0, angle = 90
    ) +
    annotate(
      "text",
      x = as.factor("2024-10-01"), y = marcha_y, # Adjust `y` value as needed
      label = "2da marcha\nuniversitaria",
      color = "red", size = marcha_size, hjust = 0, angle = 90
    )
  
  # Add 3-month average line if requested
  if (include_three_month_avg) {
    p <- p + geom_line(
      aes(y = average_credito_devengado_real, group = 1),
      size = 1
    )
  }
  
  # Save the plot
  ggsave(output_file, plot = p, width = ancho, height = alto, units = "in", dpi = 300)
  
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
    )) %>%
    mutate(gobierno = factor(gobierno, levels = c("Macri", "Fernández", "Milei", "other")))
  
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
  
  ggplot(data, aes(x = as.factor(impacto_presupuestario_anio), y = credito_devengado_real, fill = gobierno)) +
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

generate_projection <- function(data, ipc25_18, actividad_ids = NULL, adjust_specific_months = FALSE, adjustment_factor = 1.5, use_average = FALSE, inc = NULL, noinc = NULL, last_year = NULL) {
  
  # Filter data if actividad_ids is provided
  if (!is.null(actividad_ids) && length(actividad_ids) > 0) {
    data <- data %>% filter(actividad_id %in% actividad_ids)
  }

  if (!is.null(inc) && length(inc) > 0) {
    data <- data %>% filter(subparcial_desc %in% inc)
  }

  if (!is.null(noinc) && length(noinc) > 0) {
    data <- data %>% filter(subparcial_desc %notin% noinc)
  }

  # Add gobierno column if it doesn't exist
  if (!"gobierno" %in% colnames(data)) {
    data <- generate_government_column(data)
  }

  # Add cumulative column if it doesn't exist (join with IPC data)
  if (!"cumulative" %in% colnames(data)) {
    data <- data %>%
      left_join(ipc25_18 %>% select(fecha, cumulative), by = "fecha")
  }

  # Add credito_devengado_real column if it doesn't exist
  if (!"credito_devengado_real" %in% colnames(data)) {
    data <- data %>%
      mutate(credito_devengado_real = credito_devengado / cumulative)
  }

  data_mensual <- data %>% 
    ungroup() %>%
    mutate(fecha = as.Date(paste(impacto_presupuestario_anio, impacto_presupuestario_mes, "01", sep = "-"), format = "%Y-%m-%d")) %>% 
    filter(fecha <= max_mes) %>% 
    group_by(fecha, gobierno, impacto_presupuestario_anio, impacto_presupuestario_mes) %>% 
    summarise(
      credito_devengado = round(sum(credito_devengado), 0),
      credito_devengado_real = round(sum(credito_devengado_real), 0),
      cumulative = mean(cumulative)
    )

  # Ensure last_year is defined
  if (is.null(last_year)) {
    last_year <- max(data_mensual$impacto_presupuestario_anio)
  }

  # Define last_year_data
  last_year_data <- data_mensual %>% filter(impacto_presupuestario_anio == last_year)

  # Determine the value to use for projections
  # When use_average = TRUE, use the last COMPLETE year (12 months) for averaging
  # This handles cases where we're in a new year with only partial data
  if (use_average) {
    # Check if last_year has full 12 months of data
    if (nrow(last_year_data) < 12) {
      # Use the previous year's data for averaging (the last complete year)
      prev_year_data <- data_mensual %>% filter(impacto_presupuestario_anio == last_year - 1)
      if (nrow(prev_year_data) == 12) {
        projection_value <- mean(prev_year_data$credito_devengado_real, na.rm = TRUE)
      } else {
        # Fall back to last_year data if prev year also incomplete
        projection_value <- mean(last_year_data$credito_devengado_real, na.rm = TRUE)
      }
    } else {
      projection_value <- mean(last_year_data$credito_devengado_real, na.rm = TRUE)
    }
  } else if (!use_average && nrow(last_year_data) > 0) {
    projection_value <- tail(last_year_data$credito_devengado_real, 1)
  } else {
    projection_value <- 0
  }

  # Define projection parameters
  last <- tail(data_mensual, 1)
  first_proj_month <- as.Date(paste0(year(last$fecha), "-", month(last$fecha) + 1, "-01"))
  last_month <- as.Date(paste0(last_year, "-12-01"))
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
  if (adjust_specific_months & is.null(adjustment_factor)) {
    print("Please provide an adjustment factor.")
    return(NULL)
  }
  if (adjust_specific_months & !is.null(adjustment_factor)) {
    # Dynamically calculate the specific months for all years in the projection
    specific_months <- as.Date(unlist(lapply(unique(na.omit(resto$impacto_presupuestario_anio)), function(year) {
    as.Date(c(paste0(year, "-12-01"), paste0(year, "-06-01")))
    })))
    
    # Apply the adjustment to all specific months
    resto <- resto %>% 
      mutate(credito_devengado_real = ifelse(
        fecha %in% specific_months & credito_devengado == 0,
        credito_devengado_real * adjustment_factor,
        credito_devengado_real
      ))
  }

  # Project empty years
  if (last_year > year(last$fecha)) {
    anios <- last_year - year(last$fecha)
    for (i in 1:anios) {
      # Generate months for each year up to `last_year`
      resto <- rbind(resto, data.frame(
        fecha = seq(as.Date(paste0(year(last$fecha) + i, "-01-01")), as.Date(paste0(year(last$fecha) + i, "-12-01")), by = "months"),
        impacto_presupuestario_mes = month(seq(as.Date(paste0(year(last$fecha) + i, "-01-01")), as.Date(paste0(year(last$fecha) + i, "-12-01")), by = "months")),
        impacto_presupuestario_anio = year(last$fecha) + i,
        credito_devengado = rep(0, 12),
        credito_devengado_real = rep(0, 12)
      ))
    }
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
  #sort levels to Macri, Fernandez, Milei
  data_anual$gobierno <- factor(data_anual$gobierno, levels = c("Macri", "Fernández", "Milei"))
  # Return the annualized data
  return(data_anual) 
}

