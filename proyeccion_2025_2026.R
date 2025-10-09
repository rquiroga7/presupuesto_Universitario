library(dplyr)
library(jsonlite)
library(lubridate)
library(ggplot2)
library(zoo)
library(tidyverse)
`%notin%` <- Negate(`%in%`)

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

#Join into data
data <- as.data.frame(rbind(data2017, data2018, data2019, data2020, data2021, data2022, data2023, data2024,data2025))
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

#Load ipc25_18 from file for 2025 projections
ipc25_18 <- read.csv("ipc/ipc_proy2025_rem.csv")
ipc25_18$fecha <- as.Date(ipc25_18$fecha, format = "%Y-%m-%d")
ipc25_18 <- ipc25_18 %>% mutate(ipc_indice = round(ipc_indice / normalize_value, 4)) %>% rename(cumulative = ipc_indice)

data_mensual <- data %>% 
  ungroup() %>%
  mutate(fecha = as.Date(paste(impacto_presupuestario_anio, impacto_presupuestario_mes, "01", sep = "-"), format = "%Y-%m-%d")) %>% 
  filter(fecha <= max_mes) %>% 
  group_by(fecha, impacto_presupuestario_anio,impacto_presupuestario_mes) %>% 
  summarise(credito_devengado = round(sum(credito_devengado), 0), credito_devengado_real = round(sum(credito_devengado_real), 0),cumulative=mean(cumulative))

# Get August 2025 real budget as baseline for projections
august_2025 <- data_mensual %>% filter(fecha == as.Date("2025-08-01"))
if(nrow(august_2025) == 0) {
  # If August 2025 not available, use last available month
  last <- tail(data_mensual, 1)
  base_credito_real <- last$credito_devengado_real
} else {
  base_credito_real <- august_2025$credito_devengado_real
}

# Project remaining months of 2025 with constant real budget (applying 1.45 multiplier for month 12)
remaining_2025 <- seq(as.Date("2025-09-01"), as.Date("2025-12-01"), by = "months")
proj_2025_remaining <- data.frame(
  fecha = remaining_2025,
  impacto_presupuestario_mes = month(remaining_2025),
  impacto_presupuestario_anio = 2025,
  credito_devengado = 0,
  credito_devengado_real = ifelse(month(remaining_2025) == 12, base_credito_real * 1.45, base_credito_real)
)

# Join with IPC data for remaining 2025 months
proj_2025_remaining <- proj_2025_remaining %>% 
  left_join(ipc25_18, by = "fecha") %>%
  mutate(credito_devengado = round(credito_devengado_real * cumulative, 0)) %>%
  select(-ipc)

# Combine historical data with projected remaining 2025
data_mensual_with_2025 <- bind_rows(
  data_mensual,
  proj_2025_remaining
)

# For 2026 projections, we work directly with real budget amounts
# No need for IPC projections since we're plotting real values

# Scenario 2: 20% inflation with 0.99 real budget multiplier
proj_2026_20 <- data.frame(
  fecha = seq(as.Date("2026-01-01"), as.Date("2026-12-01"), by = "months"),
  impacto_presupuestario_mes = month(seq(as.Date("2026-01-01"), as.Date("2026-12-01"), by = "months")),
  impacto_presupuestario_anio = 2026,
  credito_devengado = 0,
  credito_devengado_real = ifelse(month(seq(as.Date("2026-01-01"), as.Date("2026-12-01"), by = "months")) %in% c(6, 12), 
                                 base_credito_real * 0.99 * 1.45, 
                                 base_credito_real * 0.99),
  scenario = "Escenario 20%"
)

# Combine all data for analysis
data_mensual_complete <- bind_rows(
  data_mensual_with_2025 %>% mutate(scenario = "Histórico"),
  proj_2026_20
)

# Create annual summaries - focusing on real budget sums
data_anual_scenarios <- data_mensual_complete %>%
  group_by(impacto_presupuestario_anio, scenario) %>%
  summarise(
    credito_devengado_real_anual = round(sum(credito_devengado_real), 0),
    .groups = "drop"
  )

# Now create the "Ley financiamiento" scenario after data_anual_scenarios exists
# Get 2023 annual budget to match for "Ley de financiamiento universitario" scenario
budget_2023 <- data_anual_scenarios %>% 
  filter(scenario == "Histórico" & impacto_presupuestario_anio == 2023) %>% 
  pull(credito_devengado_real_anual)

# Calculate monthly real budget needed to reach 2023 annual level
# Account for months 6 and 12 having 1.45 multiplier
# Total year = 10 normal months + 2 months * 1.45 = 10 + 2.9 = 12.9 month-equivalents
monthly_real_for_2023_level <- budget_2023 / (10 + 2 * 1.45)

# Scenario 1: Ley de financiamiento universitario (same total as 2023)
proj_2026_ley <- data.frame(
  fecha = seq(as.Date("2026-01-01"), as.Date("2026-12-01"), by = "months"),
  impacto_presupuestario_mes = month(seq(as.Date("2026-01-01"), as.Date("2026-12-01"), by = "months")),
  impacto_presupuestario_anio = 2026,
  credito_devengado = 0,
  credito_devengado_real = ifelse(month(seq(as.Date("2026-01-01"), as.Date("2026-12-01"), by = "months")) %in% c(6, 12), 
                                 monthly_real_for_2023_level * 1.45, 
                                 monthly_real_for_2023_level),
  scenario = "Ley financiamiento"
)

# Add the "Ley financiamiento" scenario to the complete data
data_mensual_complete <- bind_rows(
  data_mensual_complete,
  proj_2026_ley
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

# Prepare data for plotting - convert everything to June 2026 pesos
# Calculate June 2026 reference point using 2025 IPC projections + 10% annual inflation for 2026
# Get August 2025 IPC value from projections
dec_2025_ipc <- ipc25_18 %>% filter(fecha == as.Date("2025-12-01")) %>% pull(cumulative)
# Calculate June 2026 IPC assuming 10% annual inflation (monthly rate = 1.10^(1/12) = 1.00797)
monthly_rate_2026 <- 1.10^(1/12)
june_2026_ipc <- dec_2025_ipc * (monthly_rate_2026^6)  # 6 months from Dec 2025 to June 2026



# Prepare combined data for single plot - convert all values to June 2026 pesos
plot_data_combined <- bind_rows(
  data_anual_scenarios %>% filter(scenario == "Histórico") %>% 
    mutate(scenario_display = "Histórico", 
           # Convert historical real values to June 2026 pesos by multiplying by June 2026 IPC
           credito_june_2026_pesos = credito_devengado_real_anual * june_2026_ipc),
  data_anual_scenarios %>% filter(scenario == "Escenario 20%") %>%
    mutate(scenario_display = "2026 - Escenario 20%",
           # 2026 real values already in real terms, convert to June 2026 pesos
           credito_june_2026_pesos = credito_devengado_real_anual * june_2026_ipc),
  data_anual_scenarios %>% filter(scenario == "Ley financiamiento") %>%
    mutate(scenario_display = "2026 - Ley financiamiento",
           # 2026 real values already in real terms, convert to June 2026 pesos
           credito_june_2026_pesos = credito_devengado_real_anual * june_2026_ipc)
)

# Create a proper factor ordering for the x-axis
plot_data_combined <- plot_data_combined %>%
  mutate(
    year_scenario = case_when(
      scenario == "Histórico" ~ as.character(impacto_presupuestario_anio),
      scenario %in% c("Ley financiamiento", "Escenario 20%") ~ "2026",
      TRUE ~ as.character(impacto_presupuestario_anio)
    ),
    scenario_type = case_when(
      scenario == "Histórico" ~ "Histórico",
      scenario == "Ley financiamiento" ~ "Ley financiamiento",
      scenario == "Escenario 20%" ~ "20% inflación",
      TRUE ~ "Histórico"
    ),
    color_group = case_when(
      impacto_presupuestario_anio %in% 2017:2019 ~ "yellow",
      impacto_presupuestario_anio %in% 2020:2023 ~ "cyan", 
      impacto_presupuestario_anio %in% 2024:2025 ~ "purple",
      scenario == "Ley financiamiento" ~ "green",
      scenario == "Escenario 20%" ~ "purple_2",
      TRUE ~ "other"
    ),
    # Add explicit factor ordering for scenario to control dodge order
    scenario_factor = factor(scenario, levels = c("Histórico", "Escenario 20%", "Ley financiamiento"))
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
                               y=credito_june_2026_pesos/1000000, 
                               fill=color_group,
                               group=scenario_factor)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.8), width = ifelse(plot_data_combined$year_scenario == "2026", 0.8, 0.8)) +
  labs(title = "Universidades Nacionales: Presupuesto anual real devengado",
       subtitle = "Histórico (2017-2025) y proyecciones 2026 - Valores en pesos de junio 2026",
       x = "Año",
       y = "Crédito anual devengado\n(billones de pesos de junio 2026)") +
  scale_fill_manual(values=color_mapping) +
  theme_light(base_size=14) +
  geom_text(aes(y = credito_june_2026_pesos/1000000, 
                label = format(round(credito_june_2026_pesos/1000000, 1), decimal.mark=",", nsmall=1)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size=4.5, fontface = "bold") +
  # Add rotated white text inside bars
  geom_text(data = plot_data_combined %>% filter(impacto_presupuestario_anio == 2025),
            aes(y = credito_june_2026_pesos/2000000, label = "Proyección"),
            color = "white", angle = 90, size = 4, fontface = "bold") +
  geom_text(data = plot_data_combined %>% filter(scenario == "Ley financiamiento"),
            aes(x = factor(year_scenario, levels=unique(plot_data_combined$year_scenario)), 
                y = credito_june_2026_pesos/2000000, label = "Ley de financiamiento universitario"),
            position = position_nudge(x = 0.2), color = "white", angle = 90, size = 4, fontface = "bold") +
  geom_text(data = plot_data_combined %>% filter(scenario == "Escenario 20%"),
            aes(x = factor(year_scenario, levels=unique(plot_data_combined$year_scenario)), 
                y = credito_june_2026_pesos/2000000, label = "Presupuesto Milei 2026 (20% inflación anual)"),
            position = position_nudge(x = -0.2), color = "white", angle = 90, size = 4, fontface = "bold") +
  scale_y_continuous(labels = function(x) format(x, decimal.mark=",", nsmall=1), 
                     limits = c(NA, max(plot_data_combined$credito_june_2026_pesos/1000000) * 1.1)) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 15, b = 10, l = 10, r = 10),
                                   color = "black", lineheight = 1.2),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5)) +
  labs(caption = str_wrap("Se ajustó el crédito devengado en cada mes por inflación mensual, utilizando el IPC (índice de precios al consumidor) y se anualizan los montos. Para proyectar 2025 se considera un ajuste mensual del presupuesto igual al IPC. Para 2026 se proyecta una ejecución mensual actualizada por IPC del mes de noviembre de 2025 con un aumento anual del 18,8%. Por Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Universitario", width = 120))

ggsave("plots/proyeccion_historica_2017_2025.png", plot = plot2026, width = 9, height = 9, units = "in", dpi = 300)

# Summary table
print("Resumen de proyecciones (en billones de pesos de junio 2026):")
print(plot_data_combined %>% 
      filter(impacto_presupuestario_anio >= 2024) %>%
      mutate(credito_june_2026_miles_millones = format(round(credito_june_2026_pesos/1000000, 1), decimal.mark=",", nsmall=1)) %>%
      select(impacto_presupuestario_anio, scenario_display, credito_june_2026_miles_millones))

print(paste0("Presupuesto base (agosto 2025): ", format(round(base_credito_real/1000000, 1), decimal.mark=",", nsmall=1), " billones"))
print(paste0("IPC junio 2026 (referencia): ", round(june_2026_ipc, 4)))
print(paste0("Valor referencia junio 2026: ", format(round(base_credito_real * june_2026_ipc/1000000, 1), decimal.mark=",", nsmall=1), " billones"))

