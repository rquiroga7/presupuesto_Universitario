library(dplyr)
library(jsonlite)
library(lubridate)
library(ggplot2)

#Cargamos los datos 2023 de presupuesto abierto para el programa 26
#Fortalecimiento de la Educación Superior	
data2023 <- fromJSON("2023.json")

#Chequeamos los montos totales de crédito vigente y crédito devengado
total2023<-data2023 %>% summarise(credito_vigente = sum(credito_vigente), credito_devengado = sum(credito_devengado))
total2023

#Vemos qué porcentaje del crédito vigente corresponde a "Universidades sin Discriminar"
credv2023<-data2023 %>% group_by(subparcial_desc) %>% summarise(credito_vigente = sum(credito_vigente)) %>% mutate(credito_vigente_porc=credito_vigente/total2023$credito_vigente*100) %>% arrange(-credito_vigente_porc)
credv2023

#Del crédito vigente que corresponde a "Universidades sin Discriminar", vemos qué actividades involucra
actividades2023<-data2023 %>% filter(subparcial_desc=="Universidades sin Discriminar") %>% group_by(actividad_desc) %>% summarise(credito_vigente = sum(credito_vigente)) %>% mutate(credito_vigente_porc=credito_vigente / credv2023$credito_vigente[1] * 100) %>% arrange(-credito_vigente)
View(actividades2023)

#Ahora, vemos qué porcentaje del crédito devengado corresponde a "Universidades sin Discriminar"
credd2023<-data2023 %>% group_by(subparcial_desc) %>% summarise(credito_devengado = sum(credito_devengado)) %>% mutate(credito_devengado_porc=credito_devengado/total2023$credito_devengado*100) %>% arrange(-credito_devengado_porc)
credd2023
#Cómo? Cero?

#Entonces, el crédito vigente que corresponde a "Universidades sin Discriminar" en realidad luego se devenga a las universidades
# y la finalidad está clarísima, es pagar aumentos salariales acordados por paritarias!