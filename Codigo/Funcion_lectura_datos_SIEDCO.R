library(tidyverse)
library(lubridate)
library(readxl)
require(glue)
user=Sys.getenv("USERNAME")
lkp_mpio <- read_excel(glue("C:/Users/{user}/OneDrive - Gobernacion de Antioquia/CASCDH/2_Subsistema_Cuantitativo/2_1_Procesos_internos/2_2_5_Tableros_de_control/2_2_5_4_Delitos_alto_impacto_publicos/Origen_datos/LKP/Lkp_Municipios.xlsx"))
nombres_columna_datosgov<- read_xlsx("../Origen_datos/LKP/lkp_nombrescolumna.xlsx")


lectura_union <- function(path="../Origen_datos/homicidios/homicidios_2010_2.xlsx") {
  require(tidyverse) 
  require(tidyr)
  require(janitor)
  require(readxl)
  require(magrittr)
  
  print(path)
  
  nskip<-
    read_excel(path=path, range = "B1:C20") %>%
    mutate(id=row_number()) %>%
    drop_na() %>%
    summarise(valor=min(id)) %>%
    as.integer()
  
  data<-
    read_excel(path=path, skip = nskip) %>%
    clean_names()
  
  data<-
    data %>%
    slice(1:  which(is.na(data[[2]]))[1]-1) %>%
    mutate_at(vars(contains("codigo")), as.character)
  
  
  colnames<-
    data.frame(names=colnames(data)) %>% 
    left_join(nombres_columna_datosgov) %>% 
    mutate(nombre=ifelse(is.na(nombre), names, nombre)) %>% 
    group_by(nombre) %>% 
    mutate(nombre=ifelse(n()>1&row_number()>1,paste0(nombre, row_number()),nombre)) %>% 
    ungroup()
  
  
  data<-
    data %>% 
    set_colnames(colnames$nombre) %>% 
    filter(!is.na(fecha_hecho)) %>% 
    select(-contains("borrar")) %>% 
    mutate(across(where(is.character), toupper)) %>% 
    # separate(fecha_hecho, c("dia", "mes", "ano"), sep = "/") %>% 
    mutate(dia=day(fecha_hecho), mes=month(fecha_hecho), ano=year(fecha_hecho)) %>% 
    mutate(codigo_dane=ifelse(codigo_dane=='NO REGISTRA' | codigo_dane=='NO REPORTA',
                             "", codigo_dane)) %>% 
    filter(codigo_dane%in%paste0("0",lkp_mpio$mpio_ccdgo,"000")) %>% 
    mutate(mpio_ccdgo=round(as.numeric(codigo_dane),0)/1000) %>%  
    mutate(codigo_dane=as.character(codigo_dane))
  
  
  
  return(data)
}




lectura_opera <- function(path="data/WEB_PONAL_2022/hurto_a_personas.xlsx") {
  require(tidyverse) 
  require(tidyr)
  require(janitor)
  require(readxl)
  require(magrittr)
  
  print(path)
  
  nskip<-
    read_excel(path=path, range = "B1:C20") %>%
    mutate(id=row_number()) %>%
    drop_na() %>%
    summarise(valor=min(id)) %>%
    as.integer()
  
  data<-
    read_excel(path=path, skip = nskip) %>%
    clean_names()
  
  data<-
    data %>%
    slice(1:  which(is.na(data[[2]]))[1]-1) %>%
    mutate_at(vars(contains("codigo")), as.character)
  
  
  
  
  
  return(data)
}
