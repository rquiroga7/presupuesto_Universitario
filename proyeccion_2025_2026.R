library(dplyr)
library(jsonlite)
library(lubridate)
library(ggplot2)
library(zoo)
library(tidyverse)
`%notin%` <- Negate(`%in%`)
source("funciones.R")

#Load IPC from file
ipc <- read.csv("ipc/ipc.csv")
ipc$cumulative<-ipc$ipc_indice
#Divide cumulative by the value corresponding to max month
max_mes <- max(ipc$fecha)
normalize_value <- ipc %>% filter(as.Date(fecha) == max_mes) %>% pull(cumulative)
ipc <- ipc %>% mutate(cumulative = round(cumulative / normalize_value, 4))
ipc$fecha <- as.Date(ipc$fecha, format = "%Y-%m-%d")

#Read json files into table (2017-2025)
data2017 <- fromJSON("datos/2017.json")  %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2018 <- fromJSON("datos/2018.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2019 <- fromJSON("datos/2019.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2020 <- fromJSON("datos/2020.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2021 <- fromJSON("datos/2021.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2022 <- fromJSON("datos/2022.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2023 <- fromJSON("datos/2023.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2024 <- fromJSON("datos/2024.json") 
data2025 <- fromJSON("datos/2025.json") 
data2026 <- fromJSON("datos/2026.json") 

#Join into data
data <- as.data.frame(rbind(data2017, data2018, data2019, data2020, data2021, data2022, data2023, data2024,data2025,data2026))
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

data<-data %>% 
    ungroup() %>%
    group_by(fecha) %>%
  left_join(ipc, by = "fecha") %>%
  mutate(credito_devengado_real = credito_devengado/cumulative) 

colors11=c("#d4d400","#d4d400","#d4d400", "#31ffff", "#31ffff", "#31ffff", "#31ffff", "#a8009d", "#a8009d", "#ff6600", "#ff6600")

#Load ipc_proy_rem.csv from file for REM projections
ipc_REM <- read.csv("ipc/ipc_proy_rem.csv")
ipc_REM$fecha <- as.Date(ipc_REM$fecha, format = "%Y-%m-%d")
ipc_REM <- ipc_REM %>% mutate(ipc_indice = round(ipc_indice / normalize_value, 4)) %>% rename(cumulative = ipc_indice)

# Use generate_projection to create base 2026 projection (uses 2025 average)
data_mensual_base <- generate_projection(data, ipc_REM, adjust_specific_months = TRUE, adjustment_factor = 1.45, use_average = TRUE, last_year = 2026)
data_anual_base <- annualize(data_mensual_base)

# Create initial data_mensual_complete with historical data and projected 2025/2026 base
data_mensual_complete <- data_mensual_base %>%
  mutate(scenario = "Histórico")

# Create initial annual summaries  
data_anual_scenarios <- data_mensual_complete %>%
  group_by(impacto_presupuestario_anio, scenario) %>%
  summarise(
    credito_devengado_real_anual = round(sum(credito_devengado_real), 0),
    .groups = "drop"
  )

# Get 2023 annual budget to match for "Ley de financiamiento universitario" scenario
budget_2023 <- data_anual_scenarios %>% 
  filter(scenario == "Histórico" & impacto_presupuestario_anio == 2023) %>% 
  pull(credito_devengado_real_anual)

# Calculate monthly real budget needed to reach 2023 annual level
# Account for months 6 and 12 having 1.45 multiplier
# Total year = 10 normal months + 2 months * 1.45 = 10 + 2.9 = 12.9 month-equivalents
monthly_real_for_2023_level <- budget_2023 / (10 + 2 * 1.45)

# Get the base 2026 monthly projection from generate_projection
# Extract the monthly average (non-aguinaldo months) from the base projection
base_2026_monthly <- data_mensual_base %>% 
  filter(impacto_presupuestario_anio == 2026 & !month(fecha) %in% c(6, 12)) %>%
  pull(credito_devengado_real) %>%
  mean()

# Apply cumulative increases for "Propuesta aumento Milei":
# Three cumulative 4% increases in March, June, and September
# - Jan-Feb: no increase (1.00)
# - Mar-May: 4% increase (1.04)
# - Jun-Aug: 4% on top of 4% (1.04^2 = 1.0816)
# - Sep-Dec: 4% on top of both (1.04^3 = 1.124864)
months_2026 <- 1:12
n_increases <- (months_2026 >= 3) + (months_2026 >= 6) + (months_2026 >= 9)
monthly_multipliers <- 1.04^n_increases

# Create monthly values for Propuesta aumento Milei
monthly_real_milei <- base_2026_monthly * monthly_multipliers
# Apply aguinaldo multiplier for months 6 and 12
monthly_real_milei <- ifelse(months_2026 %in% c(6, 12), monthly_real_milei * 1.45, monthly_real_milei)

# Scenario 1: Ley de financiamiento universitario (same total as 2023)
proj_2026_ley <- data.frame(
  fecha = seq(as.Date("2026-01-01"), as.Date("2026-12-01"), by = "months"),
  impacto_presupuestario_mes = month(seq(as.Date("2026-01-01"), as.Date("2026-12-01"), by = "months")),
  impacto_presupuestario_anio = 2026,
  credito_devengado = 0,
  credito_devengado_real = ifelse(month(seq(as.Date("2026-01-01"), as.Date("2026-12-01"), by = "months")) %in% c(6, 12), 
                                 monthly_real_for_2023_level * 1.45, 
                                 monthly_real_for_2023_level),
  scenario = "Ley de financiamiento aprobada"
)

# Scenario 2: Propuesta aumento Milei (base 2026 + 5.1% + cumulative 4% increases)
nuevo_proj_2026_ley <- data.frame(
  fecha = seq(as.Date("2026-01-01"), as.Date("2026-12-01"), by = "months"),
  impacto_presupuestario_mes = months_2026,
  impacto_presupuestario_anio = 2026,
  credito_devengado = 0,
  credito_devengado_real = monthly_real_milei,
  scenario = "Propuesta aumento Milei"
)

# Add the "Ley financiamiento" scenario to the complete data
data_mensual_complete <- bind_rows(
  data_mensual_complete,
  proj_2026_ley,
  nuevo_proj_2026_ley
)

# Recreate annual summaries with the new scenario included
data_anual_scenarios <- data_mensual_complete %>%
  group_by(impacto_presupuestario_anio, scenario) %>%
  summarise(
    credito_devengado_real_anual = round(sum(credito_devengado_real), 0),
    .groups = "drop"
  )

# Display monthly data table before grouping by year
print("=== TABLA MENSUAL DE PRESUPUESTO (antes de agrupar por año) ===")
monthly_table <- data_mensual_complete %>%
  arrange(scenario, impacto_presupuestario_anio, impacto_presupuestario_mes) %>%
  mutate(
    credito_real_millones = round(credito_devengado_real / 1000000, 3),
    fecha_display = format(fecha, "%Y-%m")
  ) %>%
  select(scenario, fecha_display, impacto_presupuestario_anio, impacto_presupuestario_mes, credito_real_millones)

View(monthly_table)

print("=== RESUMEN POR ESCENARIO ===")
summary_by_scenario <- monthly_table %>%
  group_by(scenario, impacto_presupuestario_anio) %>%
  summarise(
    meses_incluidos = n(),
    total_anual_millones = sum(credito_real_millones),
    promedio_mensual = round(mean(credito_real_millones), 3),
    .groups = "drop"
  )
print(summary_by_scenario)

# Prepare data for plotting - convert everything to January 2026 pesos
# Get January 2026 IPC value from ipc_REM
jan_2026_ipc <- ipc_REM %>% filter(fecha == as.Date("2026-01-01")) %>% pull(cumulative)



# Prepare combined data for single plot - convert all values to January 2026 pesos
# Filter out Histórico 2026 (incomplete year) - only show 2026 scenarios
plot_data_combined <- bind_rows(
  data_anual_scenarios %>% filter(scenario == "Histórico" & impacto_presupuestario_anio < 2026) %>% 
    mutate(scenario_display = "Histórico", 
           # Convert historical real values to January 2026 pesos by multiplying by January 2026 IPC
           credito_jan_2026_pesos = credito_devengado_real_anual * jan_2026_ipc),
  data_anual_scenarios %>% filter(scenario == "Propuesta aumento Milei") %>%
    mutate(scenario_display = "2026 - Propuesta aumento Milei",
           # 2026 real values already in real terms, convert to January 2026 pesos
           credito_jan_2026_pesos = credito_devengado_real_anual * jan_2026_ipc),
  data_anual_scenarios %>% filter(scenario == "Ley de financiamiento aprobada") %>%
    mutate(scenario_display = "2026 - Ley de financiamiento aprobada",
           # 2026 real values already in real terms, convert to January 2026 pesos
           credito_jan_2026_pesos = credito_devengado_real_anual * jan_2026_ipc)
)

# Create a proper factor ordering for the x-axis
plot_data_combined <- plot_data_combined %>%
  mutate(
    year_scenario = case_when(
      scenario == "Histórico" ~ as.character(impacto_presupuestario_anio),
      scenario %in% c("Ley de financiamiento aprobada", "Propuesta aumento Milei") ~ "2026",
      TRUE ~ as.character(impacto_presupuestario_anio)
    ),
    scenario_type = case_when(
      scenario == "Histórico" ~ "Histórico",
      scenario == "Ley de financiamiento aprobada" ~ "Ley de financiamiento aprobada",
      scenario == "Propuesta aumento Milei" ~ "Propuesta aumento Milei",
      TRUE ~ "Histórico"
    ),
    color_group = case_when(
      impacto_presupuestario_anio %in% 2017:2019 ~ "yellow",
      impacto_presupuestario_anio %in% 2020:2023 ~ "cyan", 
      impacto_presupuestario_anio %in% 2024:2025 ~ "purple",
      scenario == "Ley de financiamiento aprobada" ~ "green",
      scenario == "Propuesta aumento Milei" ~ "purple_2",
      TRUE ~ "other"
    ),
    # Add explicit factor ordering for scenario to control dodge order
    scenario_factor = factor(scenario, levels = c("Histórico", "Propuesta aumento Milei", "Ley de financiamiento aprobada"))
  ) %>%
  arrange(impacto_presupuestario_anio, scenario)

# Define colors mapping
color_mapping <- c(
  "yellow" = "#d4d400",
  "cyan" = "#31ffff", 
  "purple" = "#a8009d",
  "green" = "#00b300",
  "purple_2" = "#a5069acb"
)

# Normalize max_mes for display
max_mes_display <- as.Date(max_mes)

# Single plot: Historical timeline with 2026 scenarios
plot2026<-ggplot(plot_data_combined, aes(x=factor(year_scenario, levels=unique(year_scenario)), 
                               y=credito_jan_2026_pesos/1000000, 
                               fill=color_group,
                               group=scenario_factor)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.8), width = ifelse(plot_data_combined$year_scenario == "2026", 0.8, 0.8)) +
  labs(title = "Universidades Nacionales: Presupuesto anual real devengado",
       subtitle = "Histórico (2017-2025) y escenarios 2026 - Valores en pesos de enero 2026",
       x = "Año",
       y = "Crédito anual devengado\n(billones de pesos de enero 2026)") +
  scale_fill_manual(values=color_mapping) +
  theme_light(base_size=14) +
  geom_text(aes(y = credito_jan_2026_pesos/1000000, 
                label = format(round(credito_jan_2026_pesos/1000000, 1), decimal.mark=",", nsmall=1)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size=4.5, fontface = "bold") +
  # Add rotated white text inside 2026 scenario bars
  geom_text(data = plot_data_combined %>% filter(scenario == "Ley de financiamiento aprobada"),
            aes(x = factor(year_scenario, levels=unique(plot_data_combined$year_scenario)), 
                y = credito_jan_2026_pesos/2000000, label = "Ley de financiamiento aprobada"),
            position = position_nudge(x = 0.2), color = "white", angle = 90, size = 4, fontface = "bold") +
  geom_text(data = plot_data_combined %>% filter(scenario == "Propuesta aumento Milei"),
            aes(x = factor(year_scenario, levels=unique(plot_data_combined$year_scenario)), 
                y = credito_jan_2026_pesos/2000000, label = "Propuesta aumento Milei"),
            position = position_nudge(x = -0.2), color = "white", angle = 90, size = 4, fontface = "bold") +
  scale_y_continuous(labels = function(x) format(x, decimal.mark=",", nsmall=1), 
                     limits = c(NA, max(plot_data_combined$credito_jan_2026_pesos/1000000) * 1.1)) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 15, b = 10, l = 10, r = 10),
                                   color = "black", lineheight = 1.2),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5)) +
  labs(caption = str_wrap("Se ajustó el crédito devengado en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor) y se anualizan los montos. Para 2026 se muestran dos escenarios: 'Ley de financiamiento aprobada' (equivalente al presupuesto 2023) y 'Propuesta aumento Milei' (tres aumentos acumulativos del 4% en marzo, junio y septiembre sobre el presupuesto 2025). Por Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario", width = 120))

ggsave("plots/proyeccion_historica_2017_2026.png", plot = plot2026, width = 9, height = 9, units = "in", dpi = 300)

# =============================================================================
# NEW STACKED BAR CHART FOR 2026 SCENARIOS
# =============================================================================

# Get base 2026 value (same as 2025 projection, before 4% increases)
base_2026_annual <- data_anual_scenarios %>% 
  filter(scenario == "Histórico" & impacto_presupuestario_anio == 2026) %>% 
  pull(credito_devengado_real_anual)

# Get the incremental amount from the 4% increases
milei_total <- data_anual_scenarios %>% 
  filter(scenario == "Propuesta aumento Milei") %>% 
  pull(credito_devengado_real_anual)
milei_increment <- milei_total - base_2026_annual

# Get Ley de financiamiento value and calculate increment over Milei
ley_total <- data_anual_scenarios %>% 
  filter(scenario == "Ley de financiamiento aprobada") %>% 
  pull(credito_devengado_real_anual)
ley_increment <- ley_total - milei_total

# Create stacked data for 2026
# For years 2017-2025: single bar
# For 2026: stacked bar with base + milei increment + ley increment
plot_data_stacked <- bind_rows(
  # Historical years (2017-2025)
  data_anual_scenarios %>% 
    filter(scenario == "Histórico" & impacto_presupuestario_anio < 2026) %>%
    mutate(
      credito_jan_2026_pesos = credito_devengado_real_anual * jan_2026_ipc,
      component = case_when(
        impacto_presupuestario_anio %in% 2017:2019 ~ "Macri",
        impacto_presupuestario_anio %in% 2020:2023 ~ "Fernández",
        impacto_presupuestario_anio %in% 2024:2025 ~ "Milei",
        TRUE ~ "other"
      ),
      year_label = as.character(impacto_presupuestario_anio)
    ),
  # 2026 - Base (same as 2025) - dark purple
  data.frame(
    impacto_presupuestario_anio = 2026,
    scenario = "Base 2026",
    credito_devengado_real_anual = base_2026_annual,
    credito_jan_2026_pesos = base_2026_annual * jan_2026_ipc,
    component = "Milei",
    year_label = "2026"
  ),
  # 2026 - Milei increment (the additional amount from 4% increases) - light purple
  data.frame(
    impacto_presupuestario_anio = 2026,
    scenario = "Milei increment",
    credito_devengado_real_anual = milei_increment,
    credito_jan_2026_pesos = milei_increment * jan_2026_ipc,
    component = "Propuesta aumento Milei",
    year_label = "2026"
  ),
  # 2026 - Ley de financiamiento increment (additional over Milei) - green
  data.frame(
    impacto_presupuestario_anio = 2026,
    scenario = "Ley increment",
    credito_devengado_real_anual = ley_increment,
    credito_jan_2026_pesos = ley_increment * jan_2026_ipc,
    component = "Ley de financiamiento aprobada",
    year_label = "2026"
  )
)

# Set factor order for stacking (bottom to top: Milei base, Milei increment, Ley increment)
# ggplot stacks in reverse order of factor levels
# For historical years we need Macri, Fernandez, Milei in correct order
# For 2026 stacked bar we need: Milei (bottom), Propuesta (middle), Ley (top)
plot_data_stacked$component <- factor(plot_data_stacked$component, 
                                       levels = c("Ley de financiamiento aprobada",
                                                  "Propuesta aumento Milei",
                                                  "Milei",
                                                  "Fernández",
                                                  "Macri"))

# Define colors for the stacked chart
color_mapping_stacked <- c(
  "Macri" = "#d4d400",
  "Fernández" = "#31ffff", 
  "Milei" = "#a8009d",
  "Propuesta aumento Milei" = "#d070c9",
  "Ley de financiamiento aprobada" = "#00b300"
)

# Labels for legend (same as component names now)
legend_labels <- c(
  "Macri" = "Macri (2017-2019)",
  "Fernández" = "Fernández (2020-2023)",
  "Milei" = "Milei (2024-2027)",
  "Propuesta aumento Milei" = "Propuesta aumento Milei (+12% en 3 cuotas)",
  "Ley de financiamiento aprobada" = "Ley de financiamiento aprobada"
)

# Calculate total for 2026 label
total_2026_ley <- ley_total * jan_2026_ipc
total_2026_milei <- milei_total * jan_2026_ipc
base_2026_pesos <- base_2026_annual * jan_2026_ipc
milei_increment_pesos <- milei_increment * jan_2026_ipc

# Create stacked bar plot
plot_stacked <- ggplot(plot_data_stacked, aes(x = year_label, y = credito_jan_2026_pesos/1000000, fill = component)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  # Add value labels on top for historical years (all years except 2026)
  geom_text(data = plot_data_stacked %>% filter(year_label != "2026"),
            aes(label = format(round(credito_jan_2026_pesos/1000000, 1), decimal.mark=",", nsmall=1)),
            vjust = -0.5, size = 4, fontface = "bold") +
  # Add total label for 2026 stacked bar (on top of green)
  annotate("text", x = "2026", y = total_2026_ley/1000000, 
           label = format(round(total_2026_ley/1000000, 1), decimal.mark=",", nsmall=1),
           vjust = -0.5, size = 4, fontface = "bold") +
  # Add white label for dark purple (Milei base) - above top of dark purple section
  annotate("text", x = "2026", y = base_2026_pesos/1000000, 
           label = format(round(base_2026_pesos/1000000, 1), decimal.mark=",", nsmall=1),
           color = "white", size = 4, fontface = "bold", vjust = -0.5) +
  # Add white label for light purple total (Propuesta aumento Milei) - above top of light purple section
  annotate("text", x = "2026", y = total_2026_milei/1000000, 
           label = format(round(total_2026_milei/1000000, 1), decimal.mark=",", nsmall=1),
           color = "white", size = 4, fontface = "bold", vjust = -0.5) +
  labs(title = "Universidades Nacionales: Presupuesto anual real devengado",
       subtitle = "Histórico (2017-2025) y escenarios 2026 - Valores en pesos de enero 2026",
       x = "Año",
       y = "Crédito anual devengado\n(billones de pesos de enero 2026)",
       fill = "Escenario") +
  scale_fill_manual(values = color_mapping_stacked, labels = legend_labels, 
                    breaks = c("Macri", "Fernández", "Milei", "Propuesta aumento Milei", "Ley de financiamiento aprobada"),
                    drop = FALSE) +
  scale_y_continuous(labels = function(x) format(x, decimal.mark=",", nsmall=1), 
                     limits = c(0, max(plot_data_stacked$credito_jan_2026_pesos)/1000000 * 1.15),
                     expand = c(0, 0)) +
  theme_light(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 15, b = 10, l = 10, r = 10),
                                   color = "black", lineheight = 1.2),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(caption = str_wrap("Se ajustó el crédito devengado en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor) y se anualizan los montos. Para 2026 se proyecta el presupuesto anual en base a los presupuestos mensuales, asumiendo que se van a ajustar por inflación. Se proyectan dos escenarios, el incremento de la 'Propuesta aumento Milei' (12% de aumento en 3 cuotas), y el incremento adicional necesario para alcanzar lo estipulado en la 'Ley de Financiamiento Universitario', la ley aprobada que el gobierno se niega a cumplir. Por Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario", width = 120))

ggsave("plots/proyeccion_historica_2017_2026_stacked.png", plot = plot_stacked, width = 10, height = 10, units = "in", dpi = 300)

# =============================================================================
# STACKED BAR CHART WITH 10% INCREASE SCENARIO
# =============================================================================

# Calculate 10% increases scenario (3 cumulative 10% increases in March, June, September)
monthly_multipliers_10pct <- 1.10^n_increases
monthly_real_milei_10pct <- base_2026_monthly * monthly_multipliers_10pct
monthly_real_milei_10pct <- ifelse(months_2026 %in% c(6, 12), monthly_real_milei_10pct * 1.45, monthly_real_milei_10pct)

# Calculate annual totals for 10% scenario
milei_10pct_total <- sum(monthly_real_milei_10pct)
milei_10pct_increment <- milei_10pct_total - base_2026_annual
ley_increment_over_10pct <- ley_total - milei_10pct_total

# Create stacked data for 2026 with 10% scenario
plot_data_stacked_10pct <- bind_rows(
  # Historical years (2017-2025) - reuse from previous
  data_anual_scenarios %>% 
    filter(scenario == "Histórico" & impacto_presupuestario_anio < 2026) %>%
    mutate(
      credito_jan_2026_pesos = credito_devengado_real_anual * jan_2026_ipc,
      component = case_when(
        impacto_presupuestario_anio %in% 2017:2019 ~ "Macri",
        impacto_presupuestario_anio %in% 2020:2023 ~ "Fernández",
        impacto_presupuestario_anio %in% 2024:2025 ~ "Milei",
        TRUE ~ "other"
      ),
      year_label = as.character(impacto_presupuestario_anio)
    ),
  # 2026 - Base (same as 2025) - dark purple
  data.frame(
    impacto_presupuestario_anio = 2026,
    scenario = "Base 2026",
    credito_devengado_real_anual = base_2026_annual,
    credito_jan_2026_pesos = base_2026_annual * jan_2026_ipc,
    component = "Milei",
    year_label = "2026"
  ),
  # 2026 - 10% increment - orange
  data.frame(
    impacto_presupuestario_anio = 2026,
    scenario = "Propuesta 10% increment",
    credito_devengado_real_anual = milei_10pct_increment,
    credito_jan_2026_pesos = milei_10pct_increment * jan_2026_ipc,
    component = "Propuesta aumento 10%",
    year_label = "2026"
  ),
  # 2026 - Ley de financiamiento increment (additional over 10% scenario) - green
  data.frame(
    impacto_presupuestario_anio = 2026,
    scenario = "Ley increment",
    credito_devengado_real_anual = ley_increment_over_10pct,
    credito_jan_2026_pesos = ley_increment_over_10pct * jan_2026_ipc,
    component = "Ley de financiamiento aprobada",
    year_label = "2026"
  )
)

# Set factor order for stacking
plot_data_stacked_10pct$component <- factor(plot_data_stacked_10pct$component, 
                                             levels = c("Ley de financiamiento aprobada",
                                                        "Propuesta aumento 10%",
                                                        "Milei",
                                                        "Fernández",
                                                        "Macri"))

# Define colors for the 10% stacked chart
color_mapping_stacked_10pct <- c(
  "Macri" = "#d4d400",
  "Fernández" = "#31ffff", 
  "Milei" = "#a8009d",
  "Propuesta aumento 10%" = "#ff6600",
  "Ley de financiamiento aprobada" = "#00b300"
)

# Labels for legend
legend_labels_10pct <- c(
  "Macri" = "Macri (2017-2019)",
  "Fernández" = "Fernández (2020-2023)",
  "Milei" = "Milei (2024-2026 base)",
  "Propuesta aumento 10%" = "Propuesta aumento 10% (+3x10%)",
  "Ley de financiamiento aprobada" = "Ley de financiamiento aprobada"
)

# Calculate totals for labels
total_2026_10pct <- milei_10pct_total * jan_2026_ipc
milei_10pct_increment_pesos <- milei_10pct_increment * jan_2026_ipc

# Create stacked bar plot with 10% scenario
plot_stacked_10pct <- ggplot(plot_data_stacked_10pct, aes(x = year_label, y = credito_jan_2026_pesos/1000000, fill = component)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  # Add value labels on top for historical years (all years except 2026)
  geom_text(data = plot_data_stacked_10pct %>% filter(year_label != "2026"),
            aes(label = format(round(credito_jan_2026_pesos/1000000, 1), decimal.mark=",", nsmall=1)),
            vjust = -0.5, size = 4, fontface = "bold") +
  # Add total label for 2026 stacked bar (on top of green)
  annotate("text", x = "2026", y = total_2026_ley/1000000, 
           label = format(round(total_2026_ley/1000000, 1), decimal.mark=",", nsmall=1),
           vjust = -0.5, size = 4, fontface = "bold") +
  # Add white label for dark purple (Milei base) - above top of dark purple section
  annotate("text", x = "2026", y = base_2026_pesos/1000000, 
           label = format(round(base_2026_pesos/1000000, 1), decimal.mark=",", nsmall=1),
           color = "white", size = 4, fontface = "bold", vjust = -0.5) +
  # Add white label for orange (10% increment) - above top of orange section
  annotate("text", x = "2026", y = total_2026_10pct/1000000, 
           label = format(round(total_2026_10pct/1000000, 1), decimal.mark=",", nsmall=1),
           color = "white", size = 4, fontface = "bold", vjust = -0.5) +
  labs(title = "Universidades Nacionales: Presupuesto anual real devengado",
       subtitle = "Histórico (2017-2025) y escenarios 2026 - Valores en pesos de enero 2026",
       x = "Año",
       y = "Crédito anual devengado\n(billones de pesos de enero 2026)",
       fill = "Escenario") +
  scale_fill_manual(values = color_mapping_stacked_10pct, labels = legend_labels_10pct, 
                    breaks = c("Macri", "Fernández", "Milei", "Propuesta aumento 10%", "Ley de financiamiento aprobada"),
                    drop = FALSE) +
  scale_y_continuous(labels = function(x) format(x, decimal.mark=",", nsmall=1), 
                     limits = c(0, max(plot_data_stacked_10pct$credito_jan_2026_pesos)/1000000 * 1.15),
                     expand = c(0, 0)) +
  theme_light(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 15, b = 10, l = 10, r = 10),
                                   color = "black", lineheight = 1.2),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(caption = str_wrap("Se ajustó el crédito devengado en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor) y se anualizan los montos. Para 2026 se muestra el presupuesto base (igual a 2025), el incremento de la 'Propuesta aumento 10%' (tres aumentos acumulativos del 10% en marzo, junio y septiembre), y el incremento adicional para alcanzar la 'Ley de financiamiento aprobada' (equivalente al presupuesto 2023). Por Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario", width = 120))

ggsave("plots/proyeccion_historica_2017_2026_stacked_10pct.png", plot = plot_stacked_10pct, width = 10, height = 10, units = "in", dpi = 300)
