# rm(list=ls())
library(RSocrata)
library(data.table)
library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)
library(stringi)
library(Hmisc)
library(glue)

source("Funcion_lectura_datos_SIEDCO.R")

homicidios<-
  dir( path = "../Origen_datos/homicidios/",full.names = T,  pattern = c(".xls")) %>% 
  as_data_frame() %>% 
  mutate(
    read=map(.x = value,~lectura_union(path = .x))) %>% 
  unnest() %>% 
  select(departamento ,municipio_hecho , mpio_ccdgo , arma_medio, fecha_hecho, genero, grupo_etario, 
         mpio_ccdgo, cantidad) %>% 
  mutate(fecha_hecho=as.Date(fecha_hecho)) %>% 
  mutate(conducta="Homicidio")

hurto_personas<-
  dir( path = "../Origen_datos/hurto personas/",full.names = T,  pattern = c(".xls")) %>% 
  as_data_frame() %>% 
  mutate(
    read=map(.x = value,~lectura_union(path = .x))) %>% 
  unnest() %>% 
  select(departamento ,municipio_hecho , mpio_ccdgo , contains("arma_medio"), fecha_hecho, contains("genero"), 
         mpio_ccdgo, cantidad, contains("grupo_etario")) %>% 
  mutate(fecha_hecho=as.Date(fecha_hecho)) %>% 
  mutate(conducta="Hurto a personas")

delitos_sexuales<-
  dir( path = "../Origen_datos/delitos sexuales/",full.names = T,  pattern = c(".xls")) %>% 
  as_data_frame() %>% 
  mutate(
    read=map(.x = value,~lectura_union(path = .x))) %>% 
  unnest() %>% 
  select(departamento ,municipio_hecho , mpio_ccdgo , contains("arma_medio"), fecha_hecho, contains("genero"), 
         mpio_ccdgo, cantidad, contains("grupo_etario")) %>% 
  mutate(fecha_hecho=as.Date(fecha_hecho)) %>% 
  mutate(conducta="Delitos sexuales")

consolidada <- 
  bind_rows(homicidios, hurto_personas, delitos_sexuales) 

save(consolidada, homicidios, hurto_personas, delitos_sexuales, file = "../Salidas/Datos_PONAL.Rdata")

lkp_conducta <- read_excel("../Origen_datos/LKP/LKP_Conducta.xlsx")

lkp_arma <- read_excel("../Origen_datos/LKP/LKP_arma.xlsx")

lkp_genero <- read_excel("../Origen_datos/LKP/LKP_genero.xlsx")

lkp_rango_etario <- read_excel("../Origen_datos/LKP/LKP_rango_etario.xlsx")

Base <-
  consolidada %>%
  filter(departamento=="ANTIOQUIA") %>% 
  left_join(lkp_conducta) %>%
  left_join(lkp_genero) %>%
  left_join(lkp_rango_etario) %>%
  left_join(lkp_arma) %>%
  select(conducta, fecha_hecho, id_conducta, mpio_ccdgo, id_genero, id_rango_etario,
         id_arma, cantidad) %>%
  mutate(fecha_hecho=ymd(fecha_hecho)) %>%
  mutate(id_genero = ifelse(is.na(id_rango_etario), 3, id_genero)) %>% 
  mutate(id_rango_etario = ifelse(is.na(id_genero), 4, id_genero)) %>% 
  separate(fecha_hecho, c("Año", "mes", "dia"), sep="-") %>%
  mutate(fecha_hecho=ymd(paste0(Año,mes,dia))) %>%
  # filter(Año>2015) %>% 
  select(-c(Año, mes, dia)) %>% 
  mutate(mpio_ccdgo= substr(mpio_ccdgo,start = 1, stop = 4),
         id_arma = replace_na(id_genero, 165))

save(Base, file = "../Salidas/Base.Rdata")
