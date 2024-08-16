archivo="Prospectos_Referidos"
hoy={today()}
ext=".xlsx"
nombre_archivo=paste0(archivo,hoy,ext)

ruta="C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/output_data/"
ruta_completa=paste0(ruta,nombre_archivo)


#Cargamos la base de referidos_efectivos
files_efectivos <- list.files(
  "./raw_data/",
  "referidos_efectivos_2022.csv",
  full.names = TRUE
)

referidos_efectivos_2022<- read_csv2(
  files_efectivos[[1]],
  locale = locale(encoding = "ISO-8859-1"),
  col_types = cols(.default = col_character())
) %>%
  clean_names()

#Cargamos la base de referidos campaña usc
base_campaña_usc <- read_excel(
  path = "C:/Users/yrhu0672/Desktop/R/Informe - USC V2/output_data/Base_Campaña_USC_2022-06-30.xlsx",
  col_types = "text"
) %>% 
  select(referido_id_personal,
         Cedula_Auxiliar,
         Nombre_Auxiliar)


#Cargamos la base de crm
files_ejecutivo <- list.files(
  "./raw_data/",
  "reporte_crm_ejecutivo.csv",
  full.names = TRUE
)

registrados_fc <- read_csv2(
  files_ejecutivo[[1]],
  locale = locale(encoding = "ISO-8859-1"),
  col_types = cols(.default = col_character())
) %>%
  clean_names()


#Cargamos la base de referidos
base_referidos <- read_excel(
  path = "C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos - 2022/output_data/Base_Red_Cooperamos_2022-06-30.xlsx",
  sheet="Base_Final",
  col_types = "text"
) %>% 
  left_join(
    base_campaña_usc,
    by = c("referido_id_personal" = "referido_id_personal")
  ) %>% 
  mutate(campaña_usc=if_else(Cedula_Auxiliar!=is.na(Cedula_Auxiliar),"Si","No")) %>% 
  select(referido_fuente,
         referido_fecha_registro,
         referido_nombre,
         referido_id_personal,
         tinker_nombre,
         tinker_id_personal,
         colaborador_empresa,
         colaborador_regional,
         colaborador_puesto_de_trabajo,
         colaborador,
         preselecta,
         validacion_origen_referidos
         
  )


#EXPORTAMOS LA BASE FINAL PARA ENVIAR A BI
sheets <- list("Base_Final" = base_referidos, "Registrados_Fuerza_Ventas" = registrados_fc, "Referidos_Efectivos"=referidos_efectivos_2022) #assume sheet1 and sheet2 are data frames
write_xlsx(sheets, ruta_completa)

rm(
  base_campaña_usc,
  base_referidos,
  referidos_efectivos_2022,
  registrados_fc,
  sheets
)

gc()

