
#ruta para guardar el archivo y generar el archivo con dos hojas

archivo="Base_Red_Cooperamos_"
hoy={today()}
ext=".xlsx"
nombre_archivo=paste0(archivo,hoy,ext)

ruta="C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/output_data/"
ruta_completa=paste0(ruta,nombre_archivo)


mes_actual="julio"
ano_actual=2022

#codigo para quitar espacios en blanco al principio y al final
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Establecemos la conexion con la base de datos
#fecha del primer registrado en tink - junio 2020
con <- DBI::dbConnect(odbc::odbc(), "Multiactiva - MULCLIDAT",
                      UID = "yrhu0672", PWD = "Enero2022", Database = "CLIMAE",
                      trusted_connection = TRUE)


# Conexion para sacar el archivo 320_referidos


tinker_referidos1 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing<19600102')

tinker_referidos2 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>19600101 AND fecing<19750102')

tinker_referidos3 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>19750101 AND fecing<19850102')

tinker_referidos4 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>19850101 AND fecing<19900102')

tinker_referidos5 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>19900101 AND fecing<19950102')

tinker_referidos6 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>19950101 AND fecing<19960102')

tinker_referidos7 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>19960101 AND fecing<19970102')

tinker_referidos8 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>19970101 AND fecing<19980102')

tinker_referidos9 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>19980101 AND fecing<19990102')

tinker_referidos10 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>19990101 AND fecing<20000102')

tinker_referidos11<- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20000101 AND fecing<20010102')

tinker_referidos12<- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20010101 AND fecing<20020102')

tinker_referidos13<- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20020101 AND fecing<20030102')

tinker_referidos14<- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20030101 AND fecing<20040102')

tinker_referidos15 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20040101 AND fecing<20050102')

tinker_referidos16 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20050101 AND fecing<20060102')

tinker_referidos17 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20060101 AND fecing<20070102')

tinker_referidos18 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20070101 AND fecing<20080102')

tinker_referidos19 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20080101 AND fecing<20090102')

tinker_referidos20 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20090101 AND fecing<20100102')

tinker_referidos21 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20100101 AND fecing<20110102')

tinker_referidos22 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20110101 AND fecing<20120102')

tinker_referidos23 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20120101 AND fecing<20130102')

tinker_referidos24 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20130101 AND fecing<20140102')

tinker_referidos25 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20140101 AND fecing<20150102')

tinker_referidos26 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20150101 AND fecing<20160102')

tinker_referidos27 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20160101 AND fecing<20170102')

tinker_referidos28 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20170101 AND fecing<20180102')

tinker_referidos29 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20180101 AND fecing<20190102')

tinker_referidos30 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20190101 AND fecing<20200102')

tinker_referidos31 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20200101 AND fecing<20210102')

tinker_referidos32 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20210101 AND fecing<20220102')

tinker_referidos33 <- dbGetQuery(con , 'SELECT nitcli,fecing,asocia,tipcli,tipdoc,fecret,agcvin,procod,estaso,numint,indest
                               FROM D103285p.MULCLIDAT.CLIMAE
                               WHERE fecing>20220101')




tinker_referidos <- bind_rows(tinker_referidos1,tinker_referidos2) %>% 
  bind_rows(tinker_referidos3) %>% 
  bind_rows(tinker_referidos4) %>% 
  bind_rows(tinker_referidos5) %>% 
  bind_rows(tinker_referidos6) %>% 
  bind_rows(tinker_referidos7) %>% 
  bind_rows(tinker_referidos8) %>% 
  bind_rows(tinker_referidos9) %>% 
  bind_rows(tinker_referidos10) %>% 
  bind_rows(tinker_referidos11) %>% 
  bind_rows(tinker_referidos12) %>% 
  bind_rows(tinker_referidos13) %>% 
  bind_rows(tinker_referidos14) %>% 
  bind_rows(tinker_referidos15) %>% 
  bind_rows(tinker_referidos16)%>% 
  bind_rows(tinker_referidos17) %>% 
  bind_rows(tinker_referidos18) %>% 
  bind_rows(tinker_referidos19) %>% 
  bind_rows(tinker_referidos20) %>% 
  bind_rows(tinker_referidos21) %>% 
  bind_rows(tinker_referidos22) %>% 
  bind_rows(tinker_referidos23) %>% 
  bind_rows(tinker_referidos24) %>% 
  bind_rows(tinker_referidos25) %>% 
  bind_rows(tinker_referidos26) %>% 
  bind_rows(tinker_referidos27) %>% 
  bind_rows(tinker_referidos28) %>% 
  bind_rows(tinker_referidos29)%>% 
  bind_rows(tinker_referidos30) %>% 
  bind_rows(tinker_referidos31) %>% 
  bind_rows(tinker_referidos32) %>% 
  bind_rows(tinker_referidos33) %>% 
  clean_names()


tinker_referidos <- tinker_referidos[with(tinker_referidos, order(tinker_referidos$nitcli)), ]
tinker_referidos <- tinker_referidos[with(tinker_referidos, order(-tinker_referidos$fecing)), ]


tinker_referidos %<>%
  rename(referido_fecha_ing=fecing,
         referido_asociado=asocia,
         referido_tipo_cliente=tipcli,
         referido_tipo_documento=tipdoc,
         referido_id_personal=nitcli,
         referido_codigo_oficina=agcvin,
         referido_fecha_retiro=fecret,
         referido_cedula_promotor=procod,
         referido_codigo_estado=estaso,
         referido_codigo_unico=numint,
         referido_codigo_nivel_academico=indest) %>%
  mutate(referido_fecha_ing=ymd(referido_fecha_ing),
         referido_asociado=case_when(referido_asociado==1~"asociado",
                                     TRUE~"no asociado"),
         referido_tipo_cliente=case_when(referido_tipo_cliente==1~"Solo asociado",
                                         referido_tipo_cliente==2~"Mixto(asociado/Cliente)"),
         referido_id_personal=as.character(referido_id_personal)) %>% 
  clean_names()




regional_referidos_conexion  <- dbGetQuery ( con , " SELECT AGCORI,NOMAGC,CODCIU,CODSUC FROM D103285P.MULINDFIN.PLTAGCORI") %>%
  rename(referido_codigo_oficina=AGCORI,
         referido_nombre_agencia=NOMAGC,
         referido_codigo_ciudad=CODCIU,
         referido_codigo_regional=CODSUC,
  ) %>%
  mutate(referido_regional=case_when(referido_codigo_regional==1~"Cali",
                                     referido_codigo_regional==2~"Eje Cafetero",
                                     referido_codigo_regional==3~"Medellin",
                                     referido_codigo_regional==4~"Palmira",
                                     referido_codigo_regional==5~"Bogota",
                                     referido_codigo_regional==6~"Caribe",
                                     TRUE~"Sin Regional"))

#quitamos los espacios en blanco de la tabla y la columna
regional_referidos_conexion$referido_nombre_agencia <- trim(regional_referidos_conexion$referido_nombre_agencia)


#codigo_estado=estaso
codigo_estado_conexion  <- dbGetQuery ( con , " SELECT CODTAB,CODINT,CODNOM FROM D103285P.MULINDFIN.CLITAB WHERE CODTAB=110") %>%
  rename(referido_codigo_estado=CODINT,
         referido_nombre_estado=CODNOM) %>% 
  select(referido_codigo_estado,
         referido_nombre_estado) %>% 
  mutate(referido_nombre_estado=if_else(referido_codigo_estado==48,"Expulsion de la Cooperativa",
                                        if_else(referido_codigo_estado==24,"Suspendido por Terminacion Tiempo Inactivo",referido_nombre_estado))) %>% 
  clean_names()


#quitamos los espacios en blanco de la tabla y la columna
codigo_estado_conexion$referido_nombre_estado <- trim(codigo_estado_conexion$referido_nombre_estado)





#referido_codigo_unico=numint
financiero_referido_conexion  <- dbGetQuery ( con , " SELECT NITCTA,CUOFAC,CUOCAN,CODPRO,FAPERT,FCANCE 
                                              FROM D103285P.MULAPODAT.APOMAECTA 
                                              WHERE CODPRO=41 AND CUOCAN>0") %>%
  rename(referido_codigo_unico=NITCTA,
         referido_cuota_facturada=CUOFAC,
         referido_cuota_pagada=CUOCAN,
         referido_fecha_apertura=FAPERT,
         referido_fecha_cancelacion=FCANCE) %>% 
  select(referido_codigo_unico,
         referido_cuota_facturada,
         referido_cuota_pagada,
         referido_fecha_apertura,
         referido_fecha_cancelacion) %>% 
  clean_names()

financiero_referido_conexion <- financiero_referido_conexion[with(financiero_referido_conexion, order(-financiero_referido_conexion$referido_fecha_apertura)), ]
financiero_referido_conexion <- financiero_referido_conexion[with(financiero_referido_conexion, order(financiero_referido_conexion$referido_fecha_cancelacion)), ]
financiero_referido_conexion %<>%
  distinct(referido_codigo_unico, .keep_all = TRUE)

#referido_codigo_nivel_academico=indest
nivel_academico_conexion <- dbGetQuery ( con , " SELECT CODTAB,CODINT,CODNOM FROM D103285P.MULINDFIN.CLITAB WHERE CODTAB=177") %>%
  rename(referido_codigo_nivel_academico=CODINT,
         referido_nivel_academico=CODNOM) %>% 
  select(referido_codigo_nivel_academico,
         referido_nivel_academico) %>% 
  clean_names() %>% 
  mutate(referido_nivel_academico=if_else(referido_codigo_nivel_academico==9,"Jardin",referido_nivel_academico))

#quitamos los espacios en blanco de la tabla y la columna
nivel_academico_conexion$referido_nivel_academico <- trim(nivel_academico_conexion$referido_nivel_academico)


tinker_referidos %<>%
  left_join(regional_referidos_conexion,by= c("referido_codigo_oficina" = "referido_codigo_oficina")) %>% 
  left_join(codigo_estado_conexion,by= c("referido_codigo_estado" = "referido_codigo_estado")) %>%
  left_join(financiero_referido_conexion,by= c("referido_codigo_unico" = "referido_codigo_unico")) %>%
  left_join(nivel_academico_conexion,by= c("referido_codigo_nivel_academico" = "referido_codigo_nivel_academico")) %>% 
  select(-referido_codigo_estado,-referido_codigo_nivel_academico,-referido_codigo_regional,-referido_codigo_ciudad) %>% 
  mutate(referido_id_personal=as.character(referido_id_personal),
         referido_cedula_promotor=as.character(referido_cedula_promotor),
         referido_asociado=case_when(referido_asociado=is.na(referido_asociado)~"no asociado",
                                     TRUE~referido_asociado),
         referido_regional=case_when(referido_regional=is.na(referido_regional)~"Sin Regional",
                                     TRUE~referido_regional),
         referido_id_personal=case_when(referido_id_personal=is.na(referido_id_personal)~"0",
                                        TRUE~referido_id_personal),
         referido_nombre_estado=case_when(referido_nombre_estado=is.na(referido_nombre_estado)~"Sin estado",
                                          TRUE~referido_nombre_estado))

tinker_referidos$referido_cuota_pagada[is.na(tinker_referidos$referido_cuota_pagada)]=0
tinker_referidos$referido_cuota_facturada[is.na(tinker_referidos$referido_cuota_facturada)]=0
tinker_referidos$referido_fecha_apertura[is.na(tinker_referidos$referido_fecha_apertura)]=0
tinker_referidos$referido_fecha_cancelacion[is.na(tinker_referidos$referido_fecha_cancelacion)]=0

gc()

rm(tinker_referidos1,
   tinker_referidos2,
   tinker_referidos3,
   tinker_referidos4,
   tinker_referidos5,
   tinker_referidos6,
   tinker_referidos7,
   tinker_referidos8,
   tinker_referidos9,
   tinker_referidos10,
   tinker_referidos11,
   tinker_referidos12,
   tinker_referidos13,
   tinker_referidos14,
   tinker_referidos15,
   tinker_referidos16,
   tinker_referidos17,
   tinker_referidos18,
   tinker_referidos19,
   tinker_referidos20,
   tinker_referidos21,
   tinker_referidos22,
   tinker_referidos23,
   tinker_referidos24,
   tinker_referidos25,
   tinker_referidos26,
   tinker_referidos27,
   tinker_referidos28,
   tinker_referidos29,
   tinker_referidos30,
   tinker_referidos31,
   tinker_referidos32,
   tinker_referidos33,
   financiero_referido_conexion)

#conexion tinker_320 - referentes

tinker <- tinker_referidos %>% 
  select(referido_id_personal,
         referido_asociado,
         referido_regional,
         referido_nombre_estado) %>% 
  rename(tinker_cedula=referido_id_personal,
         tinker_asociado=referido_asociado,
         tinker_regional=referido_regional,
         tinker_nombre_estado=referido_nombre_estado) %>% 
  clean_names()




# Importar referidos ------------------------------------------------------

files_referidos <- list.files(
  "./raw_data/",
  "csv_referreds-campaign-givnbnb",
  full.names = TRUE
)

referidos <- read_csv(
  files_referidos[[1]],
  locale = locale(encoding = "ISO-8859-1"),
  col_types = cols(.default = col_character())
) %>%
  clean_names()

referidos %<>%
  select(
    referido_fuente,
    referido_correo,
    referido_telefono,
    referido_fecha_registro,
    referido_nombre,
    referido_id_personal,
    tinker_nombre,
    tinker_fecha_registro,
    tinker_telefono,
    tinker_correo,
    tinker_id_personal,
    tinker_fuente,
    tinker_cc_asesor
  )

referidos %<>%
  distinct(referido_id_personal, .keep_all = TRUE)



# Importar colaborador ----------------------------------------------------

colaborador_files <- list.files(
  "./raw_data/",
  "planta_colaboradores",
  full.names = TRUE
)

colaborador <- read_excel(
  colaborador_files[[1]],
  skip = 5,
  col_types = "text"
) %>%
  clean_names()

# colaborador %>%
#     group_by(n_cedula) %>%
#     filter(n() > 1) %>%
#     View()

colaborador %<>%
  mutate(fecha_inicio = excel_numeric_to_date(as.numeric(fecha_inicio))) %>%
  arrange(desc(fecha_inicio)) %>%
  distinct(n_cedula, .keep_all = TRUE)

colaborador %<>%
  select(
    n_cedula,
    empresa,
    regional,
    puesto_de_trabajo
  ) %>%
  rename_with(~ glue("colaborador_{.}"))



# Importar dirigentes -----------------------------------------------------

dirigentes <- read_excel(
  "./raw_data/Cedulas Dirigencia Coomeva.xlsx",
  col_types = "text"
) %>%
  clean_names()

dirigentes %<>%
  distinct(cedula) %>%
  pull()

# Importar call center ----------------------------------------------------


call_center_marcaciones <- read_excel(
  path = "C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos - 2022/raw_data/Informe vinculación Nacional Consolidado enero- marzo.xlsx",
  #  files_call_center[[1]],
  sheet = "Marcaciones",
  col_types = "text"
) %>%
  clean_names()

call_center_marcaciones_fecha <- call_center_marcaciones %>%
  mutate(fecha_primera_marcacion = convert_to_date(fecha)) %>%
  arrange(fecha_primera_marcacion) %>%
  distinct(identification, .keep_all = TRUE)

call_center_marcaciones %<>%
  count(identification) %>%
  rename(numero_llamadas = n) %>%
  mutate(
    numero_llamadas = replace_na(numero_llamadas, 0)
  )

call_center_marcaciones %<>%
  left_join(
    select(
      call_center_marcaciones_fecha,
      identification,
      fecha_primera_marcacion
    ),
    by = "identification"
  )

tabla_cod_cierre <- read_excel(
  "./raw_data/tabla_codigo_cierre.xlsx",
  col_types = "text"
) %>%
  clean_names()

call_center_ultima_llamada <- read_excel(
  path = "C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos - 2022/raw_data/Informe vinculación Nacional Consolidado enero- marzo.xlsx",
  #  files_call_center[[1]],
  sheet = "Ultima llamada",
  col_types = "text"
) %>%
  clean_names()

call_center_ultima_llamada %<>%
  select(
    identification,
    name_4,
    observaciones,
    date
  ) %>%
  rename(cierre_ultima_llamada = name_4) %>%
  left_join(
    tabla_cod_cierre,
    by = c("cierre_ultima_llamada" = "cod_de_cierre")
  ) %>%
  mutate(date = convert_to_date(date)) %>%
  rename(fecha_ultima_llamada = date) %>%
  arrange(desc(fecha_ultima_llamada)) %>%
  distinct(identification, .keep_all = TRUE) %>%
  rename_with(~ glue("referido_{.}"))

# Importar preingresos ----------------------------------------------------

preingresos <- read_excel(
  "./raw_data/Coomeva_Total.xlsx",
  skip = 6,
  col_types = "text"
) %>%
  clean_names()

tabla_estado_preingresos <- read_excel(
  "./raw_data/tabla_estado_preingresos.xlsx",
  col_types = "text"
) %>%
  clean_names()

preingresos %<>%
  select(
    identificacion,
    estado,
    cod_promotor
  )

preingresos %<>%
  left_join(tabla_estado_preingresos, by = "estado") %>%
  distinct(identificacion, .keep_all = TRUE) %>%
  drop_na(identificacion)

# Import CF ---------------------------------------------------------------

fuerza_comercial_mes <- read_excel(
  "./raw_data/planta_fuerza_comercial.xlsx",
  col_types = "text"
) %>%
  clean_names() %>% 
  distinct(cedula_ejecutivo_asesor_diligenciar, .keep_all = TRUE)


fuerza_comercial_mes %<>%
  mutate(estado_planta_lico="A") %>% 
  select(
    cedula_ejecutivo_asesor_diligenciar,
    categoria_cargos_diligenciar,
    estado_planta_lico,
    regional_diligenciar,
    zona_no_diligenciar
  ) %>% 
  rename(cedula_ejecutivo_planta_lico = cedula_ejecutivo_asesor_diligenciar,
         cargo_planta_lico = categoria_cargos_diligenciar,
         regional_planta_lico = regional_diligenciar,
         zona_planta_lico = zona_no_diligenciar)

# Import LICO -------------------------------------------------------------

planta_lico_files <- list.files(
  "./raw_data/",
  "planta_lico",
  full.names = TRUE
)

planta_lico <- read_excel(
  planta_lico_files[[1]],
  col_types = "text"
) %>%
  clean_names() %>% 
  distinct(idntfccion, .keep_all = TRUE)

planta_lico %<>%
  select(
    idntfccion,
    cargo,
    epr_cdgo,
    ergional,
    zona
  ) %>%
  rename(
    cedula_ejecutivo_planta_lico = idntfccion,
    cargo_planta_lico = cargo,
    regional_planta_lico = ergional,
    zona_planta_lico = zona,
    estado_planta_lico = epr_cdgo
  )

plant_total <- bind_rows(fuerza_comercial_mes, planta_lico) %>% 
  distinct(cedula_ejecutivo_planta_lico, .keep_all = FALSE)



# Import CRM Ejecutivos --------------------------------------------------------------


crm_ejecutivos <- reticulate::py$crm_ejecutivos %>% 
  clean_names()

#files_ejecutivo <- list.files(
#  "./raw_data/",
#  "reporte_crm_ejecutivo.csv",
#  full.names = TRUE
#)

#crm_ejecutivos <- read_csv2(
#  files_ejecutivo[[1]],
#  locale = locale(encoding = "ISO-8859-1"),
#  col_types = cols(.default = col_character())
#) %>%
#  clean_names()

crm_exportar <- crm_ejecutivos %<>% 
  mutate(fecha_de_creacion = as.Date(fecha_de_creacion,format="%d/%m/%Y"),
         regional_registrada_crm=if_else(propietario_de_oportunidad_regional %in% c("Centro Oriente",
                                                                                    "Bogotá"),
                                         "Bogota",if_else(propietario_de_oportunidad_regional %in% c("Suroccidente",
                                                                                                     "Cali",
                                                                                                     "Dirección Nacional",
                                                                                                     "Sur Occidente"),
                                                          "Cali",if_else(propietario_de_oportunidad_regional=="Medellín",
                                                                         "Medellin",propietario_de_oportunidad_regional))),
         mes_registro_crm=months.Date(fecha_de_creacion),
         ano_registro_crm=as.numeric(format(fecha_de_creacion,'%Y')),
         dia_registro_crm=as.numeric(format(fecha_de_creacion,'%d')))

crm_ejecutivos %<>%
  select(
    fecha_de_creacion,
    nombre_de_la_cuenta_numero_de_identificacion,
    propietario_de_oportunidad_identificacion,
    propietario_de_oportunidad_regional
  ) %>% 
  mutate(fecha_de_creacion = as.Date(fecha_de_creacion,format="%d/%m/%Y"),
         regional_registrada_crm=if_else(propietario_de_oportunidad_regional %in% c("Centro Oriente",
                                                                                    "Bogotá"),
                                         "Bogota",if_else(propietario_de_oportunidad_regional %in% c("Suroccidente",
                                                                                                     "Cali",
                                                                                                     "Dirección Nacional",
                                                                                                     "Sur Occidente"),
                                                          "Cali",if_else(propietario_de_oportunidad_regional=="Medellín",
                                                                         "Medellin",propietario_de_oportunidad_regional))),
         mes_registro_crm=months.Date(fecha_de_creacion),
         ano_registro_crm=as.numeric(format(fecha_de_creacion,'%Y')),
         dia_registro_crm=as.numeric(format(fecha_de_creacion,'%d')))%>%
  arrange(fecha_de_creacion) %>%
  distinct(nombre_de_la_cuenta_numero_de_identificacion, .keep_all = TRUE)

# tabla dinamica crm para registrados y combinar con registrados call que sale de la base final

#resumen mes actual registrados fv


#files_ejecutivo1 <- list.files(
#  "./raw_data/",
#  "reporte_crm_ejecutivo.csv",
#  full.names = TRUE
#)

#crm_ejecutivos_registrados_mes_actual <- read_csv2(
#  files_ejecutivo1[[1]],
#  locale = locale(encoding = "ISO-8859-1"),
#  col_types = cols(.default = col_character())
#) %>%
#  clean_names()

crm_ejecutivos_registrados_mes_actual <- crm_exportar
crm_ejecutivos_registrados_mes_actual %<>%
  select(
    fecha_de_creacion,
    nombre_de_la_cuenta_numero_de_identificacion,
    propietario_de_oportunidad_identificacion,
    propietario_de_oportunidad_regional
  ) %>% 
  mutate(fecha_de_creacion = as.Date(fecha_de_creacion,format="%d/%m/%Y"),
         regional_registrada_crm=if_else(propietario_de_oportunidad_regional %in% c("Centro Oriente",
                                                                                    "Bogotá"),
                                         "Bogota",if_else(propietario_de_oportunidad_regional %in% c("Suroccidente",
                                                                                                     "Cali",
                                                                                                     "Dirección Nacional",
                                                                                                     "Sur Occidente"),
                                                          "Cali",if_else(propietario_de_oportunidad_regional=="Medellín",
                                                                         "Medellin",propietario_de_oportunidad_regional))),
         mes_registro_crm=months.Date(fecha_de_creacion),
         ano_registro_crm=as.numeric(format(fecha_de_creacion,'%Y')),
         dia_registro_crm=as.numeric(format(fecha_de_creacion,'%d')))

resumen_mes_actual_registrados_fv <- filter(crm_ejecutivos_registrados_mes_actual,mes_registro_crm==mes_actual,ano_registro_crm==2022) %>% 
  count(dia_registro_crm)
names(resumen_mes_actual_registrados_fv) <- c("Dia_Registro","Cantidad_Registrados")


(tabla_dinamica_crm_ejecutivo <-filter(crm_ejecutivos_registrados_mes_actual,ano_registro_crm=="2022") %>% 
    count(mes_registro_crm))
names(tabla_dinamica_crm_ejecutivo) <- c("Mes_Registro","Cantidad_Registrados_Fuerza_De_Ventas")





















# Importe CRM Gestion -------------------------------------------------------------

#GUARDAMOS CRM GESTION Y CRM EJECUTIVO Y ELIMINAMOS DEL R PARA VOLVER A IMPORTARLOS Y PODER EDITAR LOS TITULOS DE COLUMNA


crm_gestion <- reticulate::py$crm_gestion %>% 
  clean_names()
  





#files_gestion <- list.files(
#  "./raw_data/",
#  "reporte_crm_gestion.csv",
#  full.names = TRUE
#)

#crm_gestion <- read_csv2(
#  files_gestion[[1]],
#  locale = locale(encoding = "ISO-8859-1"),
#  col_types = cols(.default = col_character())
#) %>%
#  clean_names()

crm_gestion %<>%
  select(
    nombre_de_la_cuenta_numero_de_identificacion,
    propietario_de_oportunidad_identificacion,
    tipo_de_tarea,
    tipo_de_respuesta,
    fecha_de_creacion
  ) %>%
  mutate(fecha_de_creacion = as.Date(fecha_de_creacion,format="%d/%m/%Y")) %>%
  arrange(desc(fecha_de_creacion)) %>%
  distinct(nombre_de_la_cuenta_numero_de_identificacion, .keep_all = TRUE)

crm <- crm_ejecutivos %>%
  left_join(
    crm_gestion,
    by = "nombre_de_la_cuenta_numero_de_identificacion"
  ) %>% 
  rename(cedula_referido_crm=nombre_de_la_cuenta_numero_de_identificacion,
         cedula_ejecutivo_crm=propietario_de_oportunidad_identificacion.x,
         tipo_de_tarea_crm=tipo_de_tarea,
         tipo_de_respuesta_crm=tipo_de_respuesta,
         fecha_creacion_ultima_oportunidad_crm=fecha_de_creacion_6,
         fecha_de_creacion_referido_crm=fecha_de_creacion) %>% 
  select(fecha_de_creacion_referido_crm,
         cedula_referido_crm,
         cedula_ejecutivo_crm,
         regional_registrada_crm,
         ano_registro_crm,
         mes_registro_crm,
         dia_registro_crm,
         tipo_de_tarea_crm,
         tipo_de_respuesta_crm,
         fecha_creacion_ultima_oportunidad_crm
  )

# Import profile ----------------------------------------------------------

files_consolidado_gestor_trafico<- list.files(
  "./raw_data/",
  "Consolidado",
  full.names = TRUE
)

consolidado_gestor_trafico <- read_excel(
  files_consolidado_gestor_trafico[[1]],
  sheet = "Consolidado",
  col_types = "text"
) %>%
  clean_names()

consolidado_gestor_trafico %<>%
  select(
    identificacion,
    preselecta,
    fecha_13
  ) %>% 
  mutate(fecha_13=convert_to_date(fecha_13)) %>% 
  arrange(desc(fecha_13))

consolidado_gestor_trafico %<>%
  distinct(identificacion, .keep_all = TRUE) %>% 
  select(identificacion,
         preselecta)

eliminados_gdw <- read_excel(
  files_consolidado_gestor_trafico[[1]],
  sheet = "Eliminados GDW",
  col_types = "text"
) %>%
  clean_names()

eliminados_gdw %<>%
  distinct(cedula_del_prospecto) %>%
  pull(cedula_del_prospecto)

eliminados_formulario <- read_excel(
  files_consolidado_gestor_trafico[[1]],
  sheet = "Eliminados Formularios",
  col_types = "text"
) %>%
  clean_names()

eliminados_formulario %<>%
  distinct(identificacion, .keep_all = TRUE) %>%
  select(identificacion, formulario)
#    rename(nombre_formulario = x19)



# Total -------------------------------------------------------------------

planta_lico_sub <- planta_lico %>%
  filter(
    cargo_planta_lico %in% c(
      "AGENTE DE VINCULACIÓN CALL CENTER",
      "ASESOR INTEGRAL CELULA ESPECIALIZADA",
      "ASESORES INTEGRALES DE SERVICIO",
      "ANFITRION",
      "EJECUTIVO DE PROFUNDIZACION",
      "EJECUTIVO COMERCIAL",
      "EJECUTIVO COMERCIAL campana VINC",
      "ASESOR INTEGRAL",
      "EJECUTIVO INTEGRAL",
      "EJECUTIVO DE FIDELIZACION OFICINA",
      "ASESOR INTEGRAL campana REACTIVACIÓN",
      "EJECUTIVO DE PROFUNDIZACION MIX",
      "CORREDOR ACUERDO EMPRESARIAL",
      "ASESOR INTEGRAL TRABAJO EN CASA",
      "EJECUTIVO INTEGRALcampana COLOCA",
      "EJECUTIVO INTEGRAL TRABAJO EN CASA",
      "EJECUTIVO DE PROFUNDIZACION CONEXIÓN TEMPRANA",
      "EJECUTIVO DE FIDELIZACION VISITAS",
      "EJECUTIVO DE PROFUNDIZACION MUNICIPIOS",
      "COORDINADOR NACIONAL ASOCIATIVIDAD",
      "COORDINADOR NACIONAL REDES",
      "DIR. NAL.  DE ASOCIATIVIDAD",
      "EJECUTIVO REFERIDOS",
      "GERENTE COMERCIAL",
      "GERENTE NAL COMERCIAL",
      "JEFE NACIONAL COMERCIAL",
      "JEFE REGIONAL VINCULACION ASOCIADOS"
      
    ),
    estado_planta_lico != "R"
  )

planta_lico_sub %<>%
  pull(cedula_ejecutivo_planta_lico)

planta_lico_tinker <- planta_lico %>%
  filter(cargo_planta_lico %in% "REDES DE ASOCIATIVIDAD") %>%
  pull(cedula_ejecutivo_planta_lico)


# Consolidado campanas Referidos ------------------------------------------

#consolidado_campana_referidos <- read_excel(
#  "./raw_data/Consolidado campanas Referidos.xlsx",
#  skip = 4,
#  col_types = "text",
#  .name_repair = ~ make_clean_names(.)
#)

#consolidado_campana_referidos %<>%
#  select(
#    cedula_referido,
#    medio_de_pago_asociado,
#    estado_del_pago_asociado,
#    premio_asociado,
#    fecha_de_pago_asociado,
#    medio_de_pago_referido,
#    estado_del_pago_referido,
#    premio_referido,
#    fecha_de_pago_referido
# )

# empresas corretaje --------------------------------------------------------

empresas_corretaje <- read_excel(
  "./raw_data/empresas_corretaje.xlsx",
  col_types = "text",
  .name_repair = ~ make_clean_names(.)
) %>%
  clean_names()

empresas_corretaje %<>% 
  rename(cod_promotor=nit)

base_vinculaciones_corretaje <- merge(preingresos,empresas_corretaje,by="cod_promotor") %>% 
  filter(cod_promotor!=is.na(cod_promotor))

####################################################################################################
# Ctrl + Alt + B hasta aqui
#####################################################################################################

# Base Final ------------------------------------------------------------------

# comenzamos a cruzar lo que va ser toda la base final de referidos

base_final <- referidos %>%
  mutate(
    tinker_id_personal = case_when(
      tinker_id_personal == "" ~ "",
      TRUE ~ tinker_id_personal
    ),
    referido_id_personal = case_when(
      referido_id_personal == "" ~ "",
      TRUE ~ referido_id_personal
    )
  ) %>%
  left_join(
    colaborador, by = c("tinker_id_personal" = "colaborador_n_cedula")
  ) %>%
  mutate(
    colaborador_regional = case_when(
      colaborador_regional == "Bogotá - Centro Oriente" ~ "Bogotá",
      colaborador_regional %in% c(
        "Cali - Suroccidente", "Direccion Nacional"
      ) ~ "Cali",
      colaborador_regional == "Caribe" ~ "Caribe",
      colaborador_regional == "Eje Cafetero" ~ "Eje Cafetero",
      colaborador_regional %in% c(
        "Medellín - Noroccidente", "Nororiente"
      ) ~ "Medellín",
      colaborador_regional == "Palmira" ~ "Palmira"
    ),
    colaborador = if_else(
      tinker_id_personal %in% colaborador[["colaborador_n_cedula"]],
      "Colaborador",
      "No es colaborador"
    )
  ) %>%
  left_join(
    tinker,
    by = c("tinker_id_personal" = "tinker_cedula")
  ) %>%
  left_join(
    tinker_referidos,
    by = c("referido_id_personal" = "referido_id_personal")
  ) %>%
  mutate(
    dirigentes = if_else(
      tinker_id_personal %in% dirigentes,
      "Dirigente",
      "No es dirigente"
    )
  ) %>%
  left_join(
    call_center_marcaciones,
    by = c("referido_id_personal" = "identification")
  ) %>%
  left_join(
    call_center_ultima_llamada,
    by = c("referido_id_personal" = "referido_identification")
  ) %>%
  mutate(
    rango_numero_llamadas = cut(
      numero_llamadas,
      c(0, 1, 2, 3, 5, 1e6),
      c(
        "1 llamada",
        "2 llamadas",
        "3 llamadas",
        "4-5 llamadas",
        "6 o más llamadas"
      )
    )
  ) %>%
  left_join(
    preingresos,
    by = c("referido_id_personal" = "identificacion")
  ) %>%
  left_join(
    crm,
    by = c("referido_id_personal" = "cedula_referido_crm")
  ) %>%
  left_join(
    consolidado_gestor_trafico,
    by = c("referido_id_personal" = "identificacion")
  ) %>%
  mutate(
    eliminados_gdw_c = if_else(
      referido_id_personal %in% eliminados_gdw,
      "Eliminado GDW",
      NA_character_
    )
  ) %>%
  left_join(
    eliminados_formulario,
    by = c("referido_id_personal" = "identificacion")
  ) %>%
  mutate(
    referido_fecha_registro = dmy_hm(referido_fecha_registro),
    referido_fecha_registro = as.Date(referido_fecha_registro),
    # referido_fecha_ing = ymd(referido_fecha_ing),
    Mes_Vinculacion=months.Date(referido_fecha_ing),
    ano_vinculacion=as.numeric(format(referido_fecha_ing,'%Y')),
    Dia_Vinculacion=as.numeric(format(referido_fecha_ing,'%d')),
    Mes_Registro=months.Date(referido_fecha_registro),
    Dia_Registro=as.numeric(format(referido_fecha_registro,'%d')),
    ano_registro=as.numeric(format(referido_fecha_registro,'%Y')),
    Dias=referido_fecha_ing-referido_fecha_registro,
    referido_efectivo=if_else(Mes_Vinculacion %in% c("enero",
                                                     "febrero",
                                                     "marzo",
                                                     "abril",
                                                     "mayo",
                                                     "junio",
                                                     "julio",
                                                     "agosto",
                                                     "septiembre",
                                                     "octubre") & referido_fecha_ing>=referido_fecha_registro & Dias<60&ano_vinculacion==2021&referido_asociado=="asociado",
                              "Si",if_else(Mes_Vinculacion %in% c("noviembre",
                                                                  "diciembre")&referido_fecha_ing>=referido_fecha_registro&ano_vinculacion==2021&referido_asociado=="asociado",
                                           "Si",if_else(Mes_Vinculacion %in% c("enero",
                                                                               "febrero",
                                                                               "marzo",
                                                                               "abril",
                                                                               "mayo",
                                                                               "junio",
                                                                               "julio",
                                                                               "agosto",
                                                                               "septiembre",
                                                                               "octubre",
                                                                               "noviembre",
                                                                               "diciembre")&referido_fecha_ing>=referido_fecha_registro&ano_vinculacion==2022&referido_asociado=="asociado",
                                                        "Si","No"))),
    
    referido_efectivo = if_else(
      is.na(referido_efectivo),
      "No",
      referido_efectivo
    ),
    validacion_pago_asociado = if_else(
      (
        (tinker_nombre_estado %in% c("Activo Normal", "Activo Cobranza Interna")) |
          colaborador == "Colaborador"
      ) &
        referido_efectivo == "Si" &
        referido_cuota_pagada >= 1 &
        (!tinker_id_personal %in% fuerza_comercial_mes[["cedula_ejecutivo_planta_lico"]]) &
        (!tinker_id_personal %in% planta_lico_sub) &
        (!referido_cedula_promotor %in% planta_lico_tinker),
      "Si",
      "No"
    ),
    validacion_pago_asociado = if_else(
      tinker_id_personal == "94390062",
      "No",
      validacion_pago_asociado
    ),
    validacion_origen_referidos = case_when(tinker_cc_asesor!=is.na(tinker_cc_asesor)~"Fuerza Comercial",
                                            referido_id_personal %in% crm_ejecutivos[["nombre_de_la_cuenta_numero_de_identificacion"]]&fecha_de_creacion_referido_crm<=referido_fecha_registro~"Fuerza Comercial",
                                            # eliminados_gdw_c!=is.na(eliminados_gdw_c)~"Fuerza Comercial", # LO QUITO POR QUE EL GESTOR ME DICE QUE ESTA HOJA NO SE VOLVIO A MANEJAR.
                                            # formulario!=is.na(formulario)~"Fuerza Comercial",#LO QUITO POR QUE EL GESTOR ME DICE QUE ES EL MISMO CRUCE DE TINK Y CRM DE CEDULAS
                                            TRUE~"Call Center"
    ),
    validacion_pago_referido = if_else(
      referido_efectivo == "Si" &
        (!referido_cedula_promotor %in% planta_lico_tinker) &
        referido_cuota_pagada >= 1,
      "Si",
      "No"
    ),
    estado_tink = case_when(
      validacion_pago_asociado == "Si" ~ "3",
      referido_efectivo == "Si" &
        (!referido_cedula_promotor %in% planta_lico_tinker) &
        validacion_pago_asociado == "No" ~ "2",
      referido_efectivo == "No" &
        (
          referido_tipo_de_cierre %in% c(
            "No interesado",
            "No cumple perfil",
            "Fuera del país",
            "Edad superada"
          ) |
            (
              numero_llamadas >= 3 & referido_tipo_de_cierre %in% c(
                "Telefono_Danado",
                "Telefono_Errado",
                "Volver_a_Llamar",
                "Agenda Cita",
                "No contesta"
              )
            )
        ) ~ "4",
      TRUE ~ "1"
    ),
    extracto_coomeva = case_when(
      referido_efectivo == "Si" &
        (!referido_cedula_promotor %in% planta_lico_tinker) &
        referido_cuota_pagada >= 1 ~ "asociado",
      referido_efectivo == "Si" &
        (!referido_cedula_promotor %in% planta_lico_tinker) &
        referido_cuota_pagada < 1 ~ "Previnculado",
      referido_efectivo == "No" &
        (
          referido_fecha_ing < referido_fecha_registro &
            referido_asociado == "asociado"
        ) ~ "Existente",
      estado_tink == "4" ~ "No interesado",
      TRUE ~ "En proceso"
    )) %>%
  left_join(
    planta_lico,
    by = c("referido_cedula_promotor" = "cedula_ejecutivo_planta_lico")
  ) %>%
  mutate(numero_llamadas_final=if_else(numero_llamadas!="","Si","No"),
         cono_contactado=if_else(referido_cierre_ultima_llamada %in% c("Agenda_Cita",
                                                                       "Contacto con Tercero",
                                                                       "Desiste_De_La_Vinculacion",
                                                                       "Edad_Superada",
                                                                       "Entendio_Que_Era_Oferta_Laboral",
                                                                       "Entrega_Referido",
                                                                       "Fuera_Del_Pais",
                                                                       "Gestion_De_Cartera",
                                                                       "Interesado_Freemium",
                                                                       "Llamada_caida",
                                                                       "llamada_caida_o_De_mala_calidad",
                                                                       "Llamada_Caida_o_Mala_Calidad",
                                                                       "Llamada_entrecortada",
                                                                       "No Interesado_Freemium",
                                                                       "No_Cuenta_Con_Referidos",
                                                                       "No_Cumple_Perfil",
                                                                       "No_hay_acuerdo_situacionEPS",
                                                                       "No_hay_interes_situacionEPS",
                                                                       "No_Interesado",
                                                                       "No_Interesado_Desempleo",
                                                                       "No_interesado_En_Dar_Referidos",
                                                                       "No_Interesado_En_Modelo_Cooperativo",
                                                                       "No_Interesado_Falta_Recursos_Contribución_Inicial",
                                                                       "No_Interesado_Falta_Recursos_Contribución_Mensual",
                                                                       "No_Interesado_Falta_Recursos_Economicos",
                                                                       "No_Interesado_Freemium",
                                                                       "No_Interesado_Mala_Experiencia_Anterior",
                                                                       "No_Interesado_Mala_Referencia_De_Coomeva",
                                                                       "No_Interesado_Necesita_Otro_Producto_Gecc",
                                                                       "No_Interesado_Negaciones_Servicios_Gecc",
                                                                       "No_Interesado_No_Manifiesta_Detalle",
                                                                       "No_Interesado_Oferta_No_Cumple_Expectativas",
                                                                       "No_Interesado_Otra_Cooperativa_Otro_Ahorro",
                                                                       "No_Interesado_Otro_Plan_Ahorro_Programado",
                                                                       "No_Interesado_Tiene_Beneficios_Con_Familiar_asocia",
                                                                       "No_Permite_Brindar_Informacion",
                                                                       "No_Recuerda_Haberse_Registrado_En_Campana",
                                                                       "No_Volver_a_Llamar",
                                                                       "Pendiente_Documento",
                                                                       "Prospecto_Efectivo",
                                                                       "Prospecto_Efectivo_Mas_Mp",
                                                                       "Publicidad_Diferente_A_Lo_Ofertado",
                                                                       "Se Oferta Freemium_No Acepta",
                                                                       "Se_Deja_Mensaje_Con_Tercero",
                                                                       "Se_Dirige_a_La_Oficina",
                                                                       "Se_Oferta_Freemium_No_Acepta",
                                                                       "Seguimiento_Inconsistencias",
                                                                       "Sin_Perfil_Edad",
                                                                       "Sin_Perfil_Ingresos",
                                                                       "Sin_Perfil_Nivel_Educativo",
                                                                       "Volver_a_Llamar",
                                                                       "Volver_a_Llamar_Interesado",
                                                                       "Volver_a_llamar_se_deja_mensaje_con_tercero",
                                                                       "Ya_Se_Encuentra_En_Proceso",
                                                                       "Ya_Se_Encuentra_Vinculado"),
                                 "Si","No"),
         cono_estado_vinculacion=if_else(estado_vinculacion %in% c("Preingreso",
                                                                   "En proceso"),
                                         "Si","No"),
         regional_final=case_when(regional_planta_lico!=""~regional_planta_lico,
                                  referido_regional!=""~referido_regional,
                                  tinker_regional!=""~tinker_regional,
                                  colaborador_regional!=""~colaborador_regional,
                                  TRUE~"Sin Regional"),
         regional_final=case_when(regional_final %in% c("Bogotá",
                                                        "REGIONAL BOGOTA",
                                                        "bogota",
                                                        "Bogota")~"Bogota",
                                  regional_final %in% c("Medellin",
                                                        "REGIONAL MEDELLIN",
                                                        "medellin",
                                                        "MEDELLIN")~"Medellin",
                                  regional_final %in% c("EJE CAFETERO",
                                                        "REGIONAL EJE CAFETERO",
                                                        "eje cafetero",
                                                        "Eje Cafetero")~"Eje Cafetero",
                                  regional_final %in% c("caribe",
                                                        "CARIBE",
                                                        "REGIONAL CARIBE",
                                                        "Caribe")~"Caribe",
                                  regional_final %in% c("palmira",
                                                        "PALMIRA",
                                                        "REGIONAL PALMIRA",
                                                        "Palmira")~"Palmira",
                                  regional_final %in% c("cali",
                                                        "CALI",
                                                        "REGIONAL CALI",
                                                        "Cali")~"Cali",
                                  TRUE~"Sin Regional"),
         segmento=case_when(dirigentes=="Dirigente"&colaborador=="Colaborador"&tinker_asociado=="asociado"~dirigentes,
                            dirigentes=="No es dirigente"&colaborador=="Colaborador"&tinker_asociado=="asociado"~colaborador,
                            dirigentes=="No es dirigente"&colaborador=="No es colaborador"&tinker_asociado=="asociado"~tinker_asociado,
                            dirigentes=="Dirigente"&colaborador=="Colaborador"&tinker_asociado=="no asociado"~dirigentes,
                            dirigentes=="Dirigente"&colaborador=="No es colaborador"&tinker_asociado=="no asociado"~dirigentes,
                            dirigentes=="Dirigente"&colaborador=="No es colaborador"&tinker_asociado=="asociado"~dirigentes,
                            colaborador=="Colaborador"&tinker_asociado=="no asociado"&dirigentes=="No es dirigente"~colaborador,
                            colaborador=="Colaborador"&is.na(tinker_asociado)&dirigentes=="No es dirigente"~colaborador,
                            colaborador=="No es colaborador"&dirigentes=="Dirigente"~dirigentes,
                            colaborador=="No es colaborador"&dirigentes=="No es dirigente"~"Sin Segmento",
                            colaborador=="No es colaborador"&tinker_asociado=="no asociado"&dirigentes=="No es dirigente"~"Sin Segmento",
                            TRUE~"Sin Segmento"),
         #        validacion_origen_referidos=case_when(fecha_de_creacion_referido_crm<referido_fecha_registro~"Fuerza Comercial",
         #                                             TRUE~"Call Center"),
         canal_vinculacion_referido=case_when(cargo_planta_lico=="AGENTE DE VINCULACIÓN CALL CENTER"~"Call Center",
                                              cargo_planta_lico %in% c("CORREDOR",
                                                                       "CORREDOR  CODIGO COMPARTIDO",
                                                                       "CORREDOR PERSONA JURIDICA",
                                                                       "CORREDOR ACUERDO EMPRESARIAL")~"Corredor",
                                              cargo_planta_lico %in% c("ASESOR INTEGRAL",
                                                                       "EJECUTIVO COMERCIAL campana VINC",
                                                                       "EJECUTIVO COMERCIAL",
                                                                       "EJECUTIVO COMERCIAL TRABAJO EN CASA",
                                                                       "EJECUTIVO INTEGRAL",
                                                                       "EJECUTIVO INTEGRAL TRABAJO EN CASA",
                                                                       "EJECUTIVO DE PROFUNDIZACION",
                                                                       "EJECUTIVO REFERIDOS",
                                                                       "EJECUTIVO INTEGRALcampana COLOCA",
                                                                       "ASESOR INTEGRAL TRABAJO EN CASA",
                                                                       "EJECUTIVO DE PROFUNDIZACION MIX",
                                                                       "ASESOR INTEGRAL campana REACTIVACIÓN",
                                                                       "EJECUTIVO DE PROFUNDIZACION CONEXIÓN TEMPRANA",
                                                                       "ASESOR INTEGRAL CELULA ESPECIALIZADA")~"Fuerza Comercial",
                                              cargo_planta_lico %in% c("EJECUTIVO COMERCIAL MP",
                                                                       "OFICINA ADMINISTRACION",
                                                                       "GESTORES FINANCIEROS SOLIDARIDAD",
                                                                       "ANFITRION",
                                                                       "EJECUTIVOS SALUD",
                                                                       "GERENTE DE ZONA",
                                                                       "COORDINADOR COMERCIAL campana CA",
                                                                       "CORREDOR DE SALUD",
                                                                       "COORDINADOR NACIONAL RECAUDO",
                                                                       "COORDINADOR ASOCIATIVIDAD")~"Fuerza Comercial",
                                              cargo_planta_lico %in% c("REDES DE ASOCIATIVIDAD",
                                                                       "COORDINADOR REDES ASOCIATIVIDAD")~"Redes Asociatividad",
                                              TRUE~validacion_origen_referidos)
         
  )



#EXPORTAMOS LA BASE FINAL

#base_final %>%
#  write_xlsx(
#    glue(
#      "./output_data/",
#      "Base_Red_Cooperamos",
#      "_{today()}.xlsx"
#    )
# )


#limpiamos la consulta
gc()

# CTRL+ALT+B hasta aqui...................

referidos_efectivos_2022 <- read_excel(
  path = "C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/raw_data/referidos_efectivos_2022.xlsx",
  #  referidos_efectivos_22[[1]],
  #  sheet = "referidos_efectivos",
  col_types = "text"
) %>%
  mutate(referido_fecha_registro = convert_to_date(referido_fecha_registro),
         referido_fecha_ing = convert_to_date(referido_fecha_ing),
         referido_tipo_documento=as.numeric(referido_tipo_documento),
         referido_fecha_retiro=as.numeric(referido_fecha_retiro),
         referido_codigo_oficina=as.numeric(referido_codigo_oficina),
         referido_codigo_unico=as.numeric(referido_codigo_unico),
         referido_cuota_facturada=as.numeric(referido_cuota_facturada),
         referido_cuota_pagada=as.numeric(referido_cuota_pagada),
         referido_fecha_apertura=as.numeric(referido_fecha_apertura),
         referido_fecha_cancelacion=as.numeric(referido_fecha_cancelacion),
         numero_llamadas=as.integer(numero_llamadas),
         fecha_primera_marcacion = ymd(fecha_primera_marcacion),
         referido_fecha_ultima_llamada = ymd(referido_fecha_ultima_llamada),
         fecha_de_creacion_referido_crm = ymd(fecha_de_creacion_referido_crm),
         mes_registro_crm=as.character(mes_registro_crm),
         dia_registro_crm=as.integer(dia_registro_crm),
         fecha_creacion_ultima_oportunidad_crm = ymd(fecha_creacion_ultima_oportunidad_crm),
         Dia_Vinculacion=as.integer(Dia_Vinculacion),
         ano_vinculacion=as.integer(ano_vinculacion),
         Dia_Registro=as.integer(Dia_Registro),
         ano_registro=as.integer(ano_registro),
         Dias=as.integer(Dias))


Base_efectivos_cruce <- base_final %>% 
  distinct(referido_id_personal, .keep_all = TRUE) %>% 
  filter(referido_efectivo=="Si",
         ano_vinculacion==2022) %>% 
  mutate(Dias=as.integer(Dias),
         ano_registro_crm=as.character(ano_registro_crm)) #%>% 
#rename(Mes_Vinculacion=Mes_Vinculacion,
#       Dia_Vinculacion=Dia_Vinculacion,
#       mes_registro=Mes_Registro,
#       dia_registro=Dia_Registro,
#       dias=Dias)

#filtrar una base por los datos de otra base
base_efectivos <- Base_efectivos_cruce %>% filter(!referido_id_personal%in%referidos_efectivos_2022[["referido_id_personal"]]) 

referidos_efectivos_2022 = bind_rows(referidos_efectivos_2022, base_efectivos)

#exportar la base de referidos efectivos
referidos_efectivos_2022 %>%
  write_xlsx(
    glue(
      "./raw_data/",
      "referidos_efectivos_2022.xlsx"
    )
  )


# REALIZAMOS EL INFORME DE LA RED COOPERAMOS -------------------------------------------------------------------------------

#CALL CENTER REGISTRADOS X REGIONAL  
tabla_dinamica_regional_cc <-count(base_final,validacion_origen_referidos,ano_registro,Mes_Registro,regional_final) %>%
  filter(ano_registro==2022,validacion_origen_referidos=="Call Center") %>% 
  rename(Regional=regional_final,
         Cantidad_Registrados=n,
         Canal=validacion_origen_referidos,
         ano_Registro=ano_registro) %>% 
  mutate(Regional=if_else(is.na(Regional),"Sin Regional",Regional)) %>% 
  select(ano_Registro,
         Mes_Registro,
         Canal,
         Regional,
         Cantidad_Registrados)


#FUERZA DE VENTAS REGISTRADOS X REGIONAL
tabla_dinamica_regional_fv <-count(crm_ejecutivos_registrados_mes_actual,ano_registro_crm,mes_registro_crm,regional_registrada_crm) %>%
  filter(ano_registro_crm==2022) %>% 
  mutate(regional_registrada_crm=if_else(is.na(regional_registrada_crm),"Sin Regional",regional_registrada_crm),
         Canal="Fuerza de Ventas") %>% 
  rename(Regional=regional_registrada_crm,
         Cantidad_Registrados=n,
         ano_Registro=ano_registro_crm,
         Mes_Registro=mes_registro_crm) %>% 
  select(ano_Registro,
         Mes_Registro,
         Canal,
         Regional,
         Cantidad_Registrados)

#UNIMOS LAS BASES DE REGISTRADOS POR CANAL Y POR REGIONAL
tabla_dinamica_regional_final1 = bind_rows(tabla_dinamica_regional_cc, tabla_dinamica_regional_fv)

#REGISTRADOS MES ACTUAL
tabla_dinamica_regional_registrados_mes_actual <- tabla_dinamica_regional_final1 %>%
  filter(ano_Registro==2022,Mes_Registro==mes_actual) %>% 
  select(Regional,
         Cantidad_Registrados)%>% 
  group_by(Regional) %>% 
  summarise(Cantidad_Registrados=sum(Cantidad_Registrados))

#AÑADIR MES
#REGISTRADOS ACUMULADO 2022
tabla_dinamica_regional_registrados_acumulado <- tabla_dinamica_regional_final1 %>%
  filter(ano_Registro==2022,Mes_Registro=="enero"|Mes_Registro=="febrero"|Mes_Registro=="marzo"|Mes_Registro=="abril"|Mes_Registro=="mayo"|Mes_Registro=="junio"|Mes_Registro==mes_actual) %>% 
  select(Regional,
         Cantidad_Registrados)%>% 
  group_by(Regional) %>% 
  summarise(Cantidad_Registrados=sum(Cantidad_Registrados))


# ------------------

#EFECTIVOS NACIONAL X MES Y REGIONAL
tabla_dinamica_regional_efectivos <- referidos_efectivos_2022 %>% 
  distinct(referido_id_personal, .keep_all = TRUE) %>% 
  count(regional_final,referido_efectivo,ano_vinculacion,Mes_Vinculacion,canal_vinculacion_referido)%>% 
  rename(Cantidad_Efectivos=n)


#EFECTIVOS MES ACTUAL X REGIONAL
tabla_dinamica_regional_efectivos_mes_actual <- tabla_dinamica_regional_efectivos %>%
  filter(ano_vinculacion==2022,Mes_Vinculacion==mes_actual) %>% 
  rename(Regional=regional_final) %>% 
  select(Regional,
         Cantidad_Efectivos)%>% 
  group_by(Regional) %>% 
  summarise(Cantidad_Efectivos=sum(Cantidad_Efectivos))

#AÑADIR MES
#EFECTIVOS ACUMULADO X REGIONAL
tabla_dinamica_regional_efectivos_acumulado <- referidos_efectivos_2022 %>% 
  filter(ano_vinculacion==2022,Mes_Vinculacion=="enero"|Mes_Vinculacion=="febrero"|Mes_Vinculacion=="marzo"|Mes_Vinculacion=="abril"|Mes_Vinculacion=="mayo"|Mes_Vinculacion=="junio"|Mes_Vinculacion==mes_actual) %>% 
  count(regional_final) %>% 
  rename(Regional=regional_final,
         Cantidad_Efectivos=n) %>% 
  select(Regional,
         Cantidad_Efectivos)%>% 
  group_by(Regional) %>% 
  summarise(Cantidad_Efectivos=sum(Cantidad_Efectivos))


# ------------------

#PREINGRESOS NACIONAL X MES Y REGIONAL
tabla_dinamica_regional_preingresos <- count(base_final,validacion_origen_referidos,ano_registro,Mes_Registro,regional_final,estado_vinculacion) %>% 
  filter(estado_vinculacion=="En proceso"|estado_vinculacion=="Preingreso",ano_registro==2022) %>% 
  rename(Regional=regional_final,
         Cantidad_Preingresos=n,
         Canal=validacion_origen_referidos,
         ano_Registro=ano_registro) %>% 
  mutate(Regional=if_else(is.na(Regional),"Sin Regional",Regional)) %>% 
  select(ano_Registro,
         Mes_Registro,
         Canal,
         Regional,
         Cantidad_Preingresos)


#PREINGRESOS MES ACTUAL X REGIONAL
tabla_dinamica_regional_preingresos_mes_actual <- tabla_dinamica_regional_preingresos %>%
  filter(ano_Registro==2022,Mes_Registro==mes_actual) %>% 
  select(Regional,
         Cantidad_Preingresos)%>% 
  group_by(Regional) %>% 
  summarise(Cantidad_Preingresos=sum(Cantidad_Preingresos))

#AÑADIR MES
#PREINGRESOS ACUMULADO X REGIONAL
tabla_dinamica_regional_preingresos_acumulado <- tabla_dinamica_regional_preingresos %>%
  filter(ano_Registro==2022,Mes_Registro=="enero"|Mes_Registro=="febrero"|Mes_Registro=="marzo"|Mes_Registro=="abril"|Mes_Registro=="mayo"|Mes_Registro=="junio"|Mes_Registro==mes_actual) %>% 
  select(Regional,
         Cantidad_Preingresos)%>% 
  group_by(Regional) %>% 
  summarise(Cantidad_Preingresos=sum(Cantidad_Preingresos))


#CUADRO ACUMULADO
cuadro_acumulado1 <- merge(tabla_dinamica_regional_registrados_acumulado,tabla_dinamica_regional_efectivos_acumulado, by="Regional", all.x=TRUE )
cuadro_acumulado2 <- merge(cuadro_acumulado1,tabla_dinamica_regional_preingresos_acumulado, by="Regional",all.x=TRUE)
cuadro_acumulado2$Cantidad_Efectivos[is.na(cuadro_acumulado2$Cantidad_Efectivos)]=0
cuadro_acumulado2$Cantidad_Preingresos[is.na(cuadro_acumulado2$Cantidad_Preingresos)]=0

cuadro_acumulado2 %<>%
  mutate(Total_efectivos_y_Preingresos=Cantidad_Efectivos+Cantidad_Preingresos)

#AÑADIR MES
#cargamos la meta acumulada
metas_red_cooperamos_cuadro_regist<- list.files(
  "./raw_data/",
  "metas_red_cooperamos",
  full.names = TRUE
)

metas_red_cooperamos_cuadro_registrados <- read_excel(
  metas_red_cooperamos_cuadro_regist[[1]],
  sheet = "meta_cuadro_registrados",
  col_types = "text"
) %>%
  clean_names() %>% 
  mutate(meta=as.numeric(meta)) %>% 
  filter(mes=="enero"|mes=="febrero"|mes=="marzo"|mes=="abril"|mes=="mayo"|mes=="junio"|mes==mes_actual) %>% 
  rename(Regional=regional,
         Meta=meta) %>% 
  group_by(Regional) %>% 
  summarise(Meta=sum(Meta))


cuadro_acumulado3 <- merge(cuadro_acumulado2,metas_red_cooperamos_cuadro_registrados, by="Regional",all.x=TRUE)
cuadro_acumulado3$Meta[is.na(cuadro_acumulado3$Meta)]=0
cuadro_acumulado3%<>%
  mutate(Cumplimiento=percent(Total_efectivos_y_Preingresos/Meta))


cuadro_acumulado4 <-  cuadro_acumulado3 %>% 
  summarise(Cantidad_Registrados=sum(Cantidad_Registrados),
            Cantidad_Efectivos=sum(Cantidad_Efectivos),
            Cantidad_Preingresos=sum(Cantidad_Preingresos),
            Total_efectivos_y_Preingresos=sum(Total_efectivos_y_Preingresos),
            Meta=sum(Meta)) %>% 
  mutate(Regional="Total",
         Cumplimiento=if_else(Meta>0,percent(Total_efectivos_y_Preingresos/Meta),"0%")) %>% 
  select(Regional,
         Cantidad_Registrados,
         Cantidad_Efectivos,
         Cantidad_Preingresos,
         Total_efectivos_y_Preingresos,
         Meta,
         Cumplimiento)

cuadro_acumulado_final <- bind_rows(cuadro_acumulado3,cuadro_acumulado4)

#informacion para envio de correo
preingresos_acumulado <- cuadro_acumulado4$Cantidad_Preingresos
vinculaciones_acumulado <- cuadro_acumulado4$Total_efectivos_y_Preingresos
cumplimiento_efectivo_acumulado <- cuadro_acumulado4$Cumplimiento


efect_nacional_nuevo_acum <- cuadro_acumulado4$Cantidad_Efectivos
efect_nacional_nuevo_acum_cumpl <- percent(cuadro_acumulado4$Cantidad_Efectivos/cuadro_acumulado4$Meta)


#CUADRO MES ACTUAL
cuadro_mes1 <- merge(tabla_dinamica_regional_registrados_mes_actual,tabla_dinamica_regional_efectivos_mes_actual, by="Regional", all.x=TRUE )
cuadro_mes2 <- merge(cuadro_mes1,tabla_dinamica_regional_preingresos_mes_actual, by="Regional",all.x=TRUE)
cuadro_mes2$Cantidad_Efectivos[is.na(cuadro_mes2$Cantidad_Efectivos)]=0
cuadro_mes2$Cantidad_Preingresos[is.na(cuadro_mes2$Cantidad_Preingresos)]=0

cuadro_mes2 %<>%
  mutate(Total_efectivos_y_Preingresos=Cantidad_Efectivos+Cantidad_Preingresos)

#cargamos la meta mensual
metas_red_cooperamos_cuadro_mes_regist<- list.files(
  "./raw_data/",
  "metas_red_cooperamos",
  full.names = TRUE
)

metas_red_cooperamos_cuadro_mes_registrados <- read_excel(
  metas_red_cooperamos_cuadro_mes_regist[[1]],
  sheet = "meta_cuadro_registrados",
  col_types = "text"
) %>%
  clean_names() %>% 
  mutate(meta=as.numeric(meta)) %>% 
  filter(mes==mes_actual) %>% 
  rename(Regional=regional,
         Meta=meta) %>% 
  group_by(Regional) %>% 
  summarise(Meta=sum(Meta))

cuadro_mes3 <- merge(cuadro_mes2,metas_red_cooperamos_cuadro_mes_registrados, by="Regional",all.x=TRUE)
cuadro_mes3$Meta[is.na(cuadro_mes3$Meta)]=0
cuadro_mes3%<>%
  mutate(Cumplimiento=percent(Total_efectivos_y_Preingresos/Meta))


cuadro_mes4 <-  cuadro_mes3 %>% 
  summarise(Cantidad_Registrados=sum(Cantidad_Registrados),
            Cantidad_Efectivos=sum(Cantidad_Efectivos),
            Cantidad_Preingresos=sum(Cantidad_Preingresos),
            Total_efectivos_y_Preingresos=sum(Total_efectivos_y_Preingresos),
            Meta=sum(Meta)) %>% 
  mutate(Regional="Total",
         Cumplimiento=if_else(Meta>0,percent(Total_efectivos_y_Preingresos/Meta),"0%")) %>% 
  select(Regional,
         Cantidad_Registrados,
         Cantidad_Efectivos,
         Cantidad_Preingresos,
         Total_efectivos_y_Preingresos,
         Meta,
         Cumplimiento)

cuadro_mensual_final <- bind_rows(cuadro_mes3,cuadro_mes4)

#informacion para el envio del correo
preingresos_mes_actual <- cuadro_mes4$Cantidad_Preingresos
vinculaciones_mes_actual <- cuadro_mes4$Total_efectivos_y_Preingresos
cumplimiento_efectivo_mes_actual <- cuadro_mes4$Cumplimiento


#resumen mes actual registrados call center

(resumen_mes_actual_registrados_call <- filter(base_final,Mes_Registro==mes_actual,ano_registro==2022,validacion_origen_referidos=="Call Center") %>% 
    count(Dia_Registro))
names(resumen_mes_actual_registrados_call) <- c("Dia_Registro","Cantidad_Registrados")

#resumen_mes_actual_registrados_nacional

resumen_mes_actual_registrados_nacional <- full_join(resumen_mes_actual_registrados_call,resumen_mes_actual_registrados_fv, by="Dia_Registro") %>% 
  mutate(Cantidad_Registrados=Cantidad_Registrados.x+Cantidad_Registrados.y) %>% 
  select(Dia_Registro,
         Cantidad_Registrados)

#resumen mes actual efectivos nacional

(resumen_mes_actual_efectivos_nacional <- filter(referidos_efectivos_2022,Mes_Vinculacion==mes_actual,ano_vinculacion==2022) %>% 
    count(Dia_Vinculacion))
names(resumen_mes_actual_efectivos_nacional) <- c("Dia_Vinculacion","Cantidad_Efectivos")


#resumen mes actual efectivos fv

(resumen_mes_actual_efectivos_fv <- filter(referidos_efectivos_2022,Mes_Vinculacion==mes_actual,ano_vinculacion==2022,canal_vinculacion_referido=="Fuerza Comercial") %>% 
    count(Dia_Vinculacion))
names(resumen_mes_actual_efectivos_fv) <- c("Dia_Vinculacion","Cantidad_Efectivos")


#datos para call center - CONO CALL CENTER ------------------------------------------------------------------------------------------

#cumplimiento perfil

(resumen_cumplimiento_perfil_call <- filter(base_final,Mes_Registro==mes_actual,ano_registro==2022,validacion_origen_referidos=="Call Center",preselecta=="SI") %>% 
   count(preselecta) %>% 
   mutate(id=1))
names(resumen_cumplimiento_perfil_call) <- c("preselecta","Cumple_Perfil","id")

#Gestionados

(resumen_gestionados_call <- filter(base_final,Mes_Registro==mes_actual,ano_registro==2022,validacion_origen_referidos=="Call Center",numero_llamadas_final=="Si",preselecta=="SI") %>% 
    count(numero_llamadas_final)%>% 
    mutate(id=1))
names(resumen_gestionados_call) <- c("Numero_Llamadas_Final","Gestionados","id")

#Contactados

(resumen_contactados_call <- filter(base_final,Mes_Registro==mes_actual,ano_registro==2022,validacion_origen_referidos=="Call Center",numero_llamadas_final=="Si",preselecta=="SI",cono_contactado=="Si") %>% 
    count(cono_contactado)%>% 
    mutate(id=1))
names(resumen_contactados_call) <- c("Cono_Contactado","Contactados","id")

#preingresos

(resumen_preingresos_call <- filter(base_final,Mes_Registro==mes_actual,ano_registro==2022,validacion_origen_referidos=="Call Center",estado_vinculacion=="En proceso"|estado_vinculacion=="Preingreso",numero_llamadas_final=="Si",preselecta=="SI") %>% 
    count()%>% 
    mutate(id=1))
names(resumen_preingresos_call) <- c("Preingresos","id")

#efectivos

(resumen_efectivos_call <- filter(referidos_efectivos_2022,Mes_Vinculacion==mes_actual,ano_vinculacion==2022,canal_vinculacion_referido=="Call Center") %>% 
    count()%>% 
    mutate(id=1))
names(resumen_efectivos_call) <- c("Efectivos","id")


#union 

union_call <- full_join(resumen_cumplimiento_perfil_call,resumen_gestionados_call,resumen_contactados_call,by="id")
union_call1 <- full_join(union_call,resumen_preingresos_call,resumen_efectivos_call,by="id")
union_call2<- full_join(union_call1,resumen_contactados_call,resumen_efectivos_call,by="id")
union_call_final<- full_join(union_call2,resumen_efectivos_call,by="id") %>% 
  mutate(No_Contactados=Gestionados-Contactados) %>% 
  select(Cumple_Perfil,
         Gestionados,
         Contactados,
         No_Contactados,
         Preingresos,
         Efectivos
  )

# NUEVO CONO PARA CALL CENTER
#transponer la informacion
df_transpose <- data.frame(t(union_call_final))
colnames(df_transpose) <- "Cantidad"
df_transpose$Dato <- rownames(df_transpose)
#df_transpose %<>% 


# plot
cono_imagen_cc <- df_transpose %>%
  arrange(Cantidad) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(Dato=factor(Dato, levels=Dato)) %>%   # This trick update the factor levels
  ggplot( aes(x=Dato, y=Cantidad)) +
  geom_segment( aes(xend=Dato, yend=0)) +
  geom_point( size=7, color="green") +
  geom_text(aes(label=Cantidad),
            position = position_stack(vjust=1.2))+
  coord_flip() +
  theme_tufte() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank()) +
  xlab("") 
png(filename="C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/Imagenes/cono_call_center.png",height=400, width=1100) 
plot(cono_imagen_cc) 
dev.off()


#resumen mes actual efectivos call center

(resumen_mes_actual_efectivos_call_center <- filter(referidos_efectivos_2022,Mes_Vinculacion==mes_actual,ano_vinculacion==2022,canal_vinculacion_referido=="Call Center") %>% 
    count(Dia_Vinculacion))
names(resumen_mes_actual_efectivos_call_center) <- c("Dia_Vinculacion","Cantidad_Efectivos")


# TABLA PARA MOSTRAR ACUMULADO DE REGISTRADOS 2022
#fuerza de ventas acumulado registrados
tabla_acumulado_regional_fv <- filter(crm_ejecutivos,ano_registro_crm==2022) %>% 
  count(mes_registro_crm)
names(tabla_acumulado_regional_fv) <- c("Mes_Registro","Cantidad_Registrados")
#call center acumulado registrados
tabla_acumulado_regional_cc <-count(base_final,validacion_origen_referidos,ano_registro,Mes_Registro) %>%
  filter(ano_registro==2022,validacion_origen_referidos=="Call Center") %>% 
  rename(Cantidad_Registrados=n,
         Canal=validacion_origen_referidos,
         ano_Registro=ano_registro) %>%
  select(Mes_Registro,
         Cantidad_Registrados)

tabla_acumulado_registrados <-   
  left_join(tabla_acumulado_regional_fv,tabla_acumulado_regional_cc, by=c("Mes_Registro")) %>% 
  mutate(Cantidad_Registrados = Cantidad_Registrados.x+Cantidad_Registrados.y) %>% 
  select(Mes_Registro,
         Cantidad_Registrados)

#meta para cruzar REGISTRADOS
metas_red_cooperamos_inicial<- list.files(
  "./raw_data/",
  "metas_red_cooperamos",
  full.names = TRUE
)

metas_red_cooperamos_nacional <- read_excel(
  metas_red_cooperamos_inicial[[1]],
  sheet = "meta_registrados_nacional",
  col_types = "text"
) %>%
  clean_names() %>% 
  rename(Mes_Registro=mes,
         Meta=meta)

metas_red_cooperamos_nacional$Meta <- as.integer(metas_red_cooperamos_nacional$Meta)

Tabla_total_Registrados_y_metas_inicial <- merge(tabla_acumulado_registrados,metas_red_cooperamos_nacional, by="Mes_Registro", all.y = TRUE)
Tabla_total_Registrados_y_metas_inicial$Cantidad_Registrados[is.na(Tabla_total_Registrados_y_metas_inicial$Cantidad_Registrados)]=0

Tabla_total_Registrados_y_metas_inicial%<>% 
  mutate(Cumplimiento=percent(Cantidad_Registrados/Meta),
         Mes_Registro = factor(Mes_Registro, levels = c("enero",
                                                        "febrero",
                                                        "marzo",
                                                        "abril",
                                                        "mayo",
                                                        "junio",
                                                        "julio",
                                                        "agosto",
                                                        "septiembre",
                                                        "octubre",
                                                        "noviembre",
                                                        "diciembre")
         )) %>% arrange(Mes_Registro)



# TABLA PARA MOSTRAR ACUMULADO DE EFECTIVOS 2022
tabla_acumulado_efectivos <- filter(referidos_efectivos_2022,ano_vinculacion==2022) %>% 
  count(Mes_Vinculacion) %>% 
  rename(Cantidad_Efectivos=n)

#meta para cruzar EFECTIVOS
metas_red_cooperamos_iniciall<- list.files(
  "./raw_data/",
  "metas_red_cooperamos",
  full.names = TRUE
)

metas_red_cooperamos_nacional_efectivos<- read_excel(
  metas_red_cooperamos_iniciall[[1]],
  sheet = "meta_efectivos_nacional",
  col_types = "text"
) %>%
  clean_names() %>% 
  rename(Mes_Vinculacion=mes,
         Meta=meta)

metas_red_cooperamos_nacional_efectivos$Meta <- as.integer(metas_red_cooperamos_nacional_efectivos$Meta)

Tabla_total_efectivos_nacional <- merge(tabla_acumulado_efectivos,metas_red_cooperamos_nacional_efectivos, by="Mes_Vinculacion", all.y = TRUE)
Tabla_total_efectivos_nacional$Cantidad_Efectivos[is.na(Tabla_total_efectivos_nacional$Cantidad_Efectivos)]=0

Tabla_total_efectivos_nacional%<>% 
  mutate(Cumplimiento=percent(Cantidad_Efectivos/Meta),
         Mes_Vinculacion = factor(Mes_Vinculacion, levels = c("enero",
                                                              "febrero",
                                                              "marzo",
                                                              "abril",
                                                              "mayo",
                                                              "junio",
                                                              "julio",
                                                              "agosto",
                                                              "septiembre",
                                                              "octubre",
                                                              "noviembre",
                                                              "diciembre")
         )) %>% arrange(Mes_Vinculacion)


#detalle referidos registrados acumulados

(detalle_referidos_registrados_acumulados <- base_final %>% 
    select(tinker_id_personal,
           tinker_nombre,
           referido_id_personal,
           referido_nombre,
           referido_fecha_registro,
           referido_fecha_ing,
           ano_vinculacion,
           Mes_Vinculacion,
           regional_final,
           canal_vinculacion_referido
    ) %>% 
    rename(Cedula_Referente=tinker_id_personal,
           Nombre_Referente= tinker_nombre,
           Cedula_Referido=referido_id_personal,
           Nombre_Referido=referido_nombre,
           Fecha_Registro=referido_fecha_registro,
           Fecha_Vinculacion=referido_fecha_ing,
           Regional=regional_final,
           Canal=canal_vinculacion_referido,
           ano_Vinculacion=ano_vinculacion
    ))


setwd("C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/Imagenes/")
png("cuadro_acumulado_final.png", height=200, width=1100)
u<-tableGrob(cuadro_acumulado_final)
grid.arrange(u)
dev.off()

setwd("C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/")


# REALZAMOE EL AVANCE DE YENIFFER -----------------------------------------------------------------------------------------

#EFECTIVOS
cuadro_resumen_yen1 <- filter(referidos_efectivos_2022,ano_vinculacion==ano_actual,Mes_Vinculacion==mes_actual) %>% 
  distinct(referido_id_personal, .keep_all = TRUE) %>% 
  count(canal_vinculacion_referido) %>% 
  rename(Indicador=canal_vinculacion_referido,
         Efectivos=n)

#PREINGRESOS
cuadro_resumen_yen2 <- filter(base_final,ano_registro==ano_actual,estado_vinculacion=="En proceso"|estado_vinculacion=="Preingreso") %>% 
  count(canal_vinculacion_referido) %>% 
  rename(Indicador=canal_vinculacion_referido,
         Preingresos=n)

#TOTAL
cuadro_resumen_yen3 <- merge(cuadro_resumen_yen1,cuadro_resumen_yen2, by="Indicador", all.y = TRUE)
cuadro_resumen_yen3$Efectivos[is.na(cuadro_resumen_yen3$Efectivos)]=0
cuadro_resumen_yen3$Preingresos[is.na(cuadro_resumen_yen3$Preingresos)]=0
cuadro_resumen_yen3 %<>% 
  mutate(Total_Efectivos=Efectivos+Preingresos)


#CARGAMOS LAS METAS
#cargamos la meta acumulada
metas_red_cooperamos_cuadro_yen<- list.files(
  "./raw_data/",
  "metas_red_cooperamos",
  full.names = TRUE
)

metas_red_cooperamos_cuadro_resumen_yen <- read_excel(
  metas_red_cooperamos_cuadro_yen[[1]],
  sheet = "meta_resumen_yen",
  col_types = "text"
) %>%
  clean_names() %>% 
  mutate(meta=as.numeric(meta)) %>% 
  filter(mes==mes_actual) %>% 
  select(indicador,
         meta) %>% 
  rename(Meta=meta,
         Indicador=indicador)


#META
cuadro_resumen_yen4 <- merge(cuadro_resumen_yen3,metas_red_cooperamos_cuadro_resumen_yen, by="Indicador", all.x = TRUE)
cuadro_resumen_yen4$Meta[is.na(cuadro_resumen_yen3$Meta)]=0

#CUMPLIMIENTO
cuadro_resumen_yen4 %<>% 
  mutate(Cumplimiento=if_else(Meta>0,percent(Total_Efectivos/Meta),"0%"))

#TOTAL CUADRO
cuadro_total_yen <-  cuadro_resumen_yen4 %>% 
  summarise(Efectivos=sum(Efectivos),
            Preingresos=sum(Preingresos),
            Total_Efectivos=sum(Total_Efectivos),
            Meta=sum(Meta)) %>% 
  mutate(Indicador="Total",
         Cumplimiento=if_else(Meta>0,percent(Total_Efectivos/Meta),"0%")) %>% 
  select(Indicador,
         Efectivos,
         Preingresos,
         Total_Efectivos,
         Meta,
         Cumplimiento)

#CUADRO FINAL
cuadro_final_yen <- bind_rows(cuadro_resumen_yen4,cuadro_total_yen)


#CUADRO CORRETAJE

cuadro_final_corretaje <- base_vinculaciones_corretaje %>% 
  mutate(Cantidad=1) %>% 
  select(regional,
         empresa,
         coordinador_encargado,
         Cantidad) %>% 
  group_by(regional,empresa,coordinador_encargado) %>% 
  summarise(Cantidad=sum(Cantidad))

cuadro_preingreso_corretaje <- base_vinculaciones_corretaje %>% 
  mutate(Cantidad=1) %>% 
  select(estado_vinculacion,
         Cantidad) %>% 
  group_by(estado_vinculacion) %>% 
  summarise(Cantidad=sum(Cantidad))


#TOTAL PERSONAS REFIRIENDO - PLATAFORMA REFERIDOS

personas_refiriendo <- base_final%>% 
  filter(Mes_Registro==mes_actual,
         ano_registro==ano_actual) %>% 
  select(tinker_id_personal) %>% 
  distinct(tinker_id_personal, .keep_all = TRUE) 


total_personas_refiriendo <- personas_refiriendo %>% 
  count(tinker_id_personal) %>% 
  summarise(n=sum(n))

personas_refiriendo_segmento <- base_final%>% 
  filter(Mes_Registro==mes_actual,
         ano_registro==ano_actual) %>% 
  select(tinker_id_personal,
         segmento) %>% 
  distinct(tinker_id_personal,segmento, .keep_all = TRUE) %>% 
  group_by(segmento) %>% 
  count(segmento) %>% 
  rename(cantidad=n) %>% 
  mutate(part_segmento=percent(cantidad/total_personas_refiriendo$n))


# PARTICIPACION DE VINCULACION POR CANAL

total_participacion_canal <- referidos_efectivos_2022 %>% 
  filter(Mes_Vinculacion==mes_actual,
         ano_vinculacion==ano_actual) %>% 
  select(canal_vinculacion_referido) %>% 
  count(canal_vinculacion_referido) %>% 
  summarise(n=sum(n))

participacion_canal <- referidos_efectivos_2022 %>% 
  filter(Mes_Vinculacion==mes_actual,
         ano_vinculacion==ano_actual) %>% 
  select(canal_vinculacion_referido) %>% 
  group_by(canal_vinculacion_referido) %>% 
  count(canal_vinculacion_referido) %>% 
  rename(cantidad=n) %>% 
  mutate(part_segmento=percent(cantidad/total_participacion_canal$n))

gc()

setwd("C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/")

####################################################################################################
# Ctrl + Alt + B hasta aqui
#####################################################################################################

# Exportamos la base final -------------------------------------------------------------------------------------

# para exportar las bases en diferentes hojas en el mismo libro es necesario
sheets <- list("Base_Final" = base_final, "Registrados_Fuerza_Ventas" = crm_exportar, "Referidos_Efectivos"= referidos_efectivos_2022) #assume sheet1 and sheet2 are data frames
write_xlsx(sheets ,ruta_completa)



# GUARDAMOS LAS TABLAS COMO IMAGENES

setwd("C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/Imagenes/")
png("cuadro_acumulado_final.png", height=200, width=1100)
u<-tableGrob(cuadro_acumulado_final)
grid.arrange(u)
dev.off()

setwd("C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/Imagenes/")
png("cuadro_mensual_final.png", height=200, width=1100)
t<-tableGrob(cuadro_mensual_final)
grid.arrange(t)
dev.off()

setwd("C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/Imagenes/")
png("cuadro_final_yen.png", height=200, width=1100)
v<-tableGrob(cuadro_final_yen)
grid.arrange(v)
dev.off()

setwd("C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/Imagenes/")
png("cuadro_final_corretaje.png", height=500, width=1100)
r<-tableGrob(cuadro_final_corretaje)
grid.arrange(r)
dev.off()

setwd("C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/Imagenes/")
png("cuadro_preingreso_corretaje.png", height=200, width=1100)
s<-tableGrob(cuadro_preingreso_corretaje)
grid.arrange(s)
dev.off()

setwd("C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/Imagenes/")
png("participacion_canal.png", height=200, width=1100)
x<-tableGrob(participacion_canal)
grid.arrange(x)
dev.off()

setwd("C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/Imagenes/")
png("personas_refiriendo_segmento.png", height=200, width=1100)
k<-tableGrob(personas_refiriendo_segmento)
grid.arrange(k)
dev.off()

setwd("C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/")

# renderisamos los informes con la informacion actualizada
#render del informe principal de la red cooperamos

render("C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/Informe Red Cooperamos.Rmd",
       output_file = glue("Informe_Red_Cooperamos.html"),
       output_dir = "./")


#codigo para enviar correo


#informacion que vamos a tener en el email y que se encuentra en el rmardown #INFORME RED
my_email_objet <- render_email('Informe Red Cooperamos Envio.Rmd') %>% 
  add_attachment("Informe_Red_Cooperamos.html")

class(my_email_objet)

# codigo final para enviar el correo por medio del outlook
smtp_send(my_email_objet,
          from = "yrhu0672@coomeva.com.co",
          to="yrhu0672@coomeva.com.co",
          subject="Informe Gestion Referidos - Red Cooperamos",
          credentials = creds_key("outlook"))



#Envio informe YENIFFER
my_email_objet2 <- render_email('Informe Yeniffer.Rmd')

class(my_email_objet)

# codigo final para enviar el correo por medio del outlook
smtp_send(my_email_objet2,
          from = "yrhu0672@coomeva.com.co",
          to="yrhu0672@coomeva.com.co",
          subject="Avance Red Cooperamos",
          credentials = creds_key("outlook"))


gc()

#-------------------------------------------------------------------------------------------------------------

# BASE campana USC - BASE campana USC - BASE campana USC - BASE campana USC - BASE campana USC - BASE campana USC  - BASE campana USC - BASE campana USC
# BASE campana USC - BASE campana USC - BASE campana USC - BASE campana USC - BASE campana USC - BASE campana USC  - BASE campana USC - BASE campana USC


# para exportar las bases en diferentes hojas en el mismo libro es necesario

archivoo="Base_campana_USC_"
hoyy={today()}
extt=".xlsx"
nombree_archivo=paste0(archivoo,hoyy,extt)

rutaa="C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/output_data/"
ruta_completaa=paste0(rutaa,nombree_archivo)


# Planta auxiliares CS -------------------------------------------------------

files_planta_auxiliares_cs<- list.files(
  "C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/raw_data/",
  "planta_auxiliares_cs",
  full.names = TRUE
)

planta_auxiliares_cs <- read_excel(
  files_planta_auxiliares_cs[[1]],
  sheet = "Base Auxiliares CS",
  col_types = "text"
) %>%
  clean_names()

planta_auxiliares_cs %<>%
  select(
    identification,
    nombre_1,
    supervisor,
    campana_costeo,
    coordinador,
    restriccion,
    sede,
    estado,
    mes_ingreso,
    ano_ingreso
  )%>% 
  distinct(identification,.keep_all = TRUE)


# Formulario consolidado 7000 ------------------------------------------------

files_formulario_7000<- list.files(
  "C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/raw_data/",
  "Consolidado referidos 7000",
  full.names = TRUE
)

formulario_7000 <- read_excel(
  files_formulario_7000[[1]],
  sheet = "BD",
  col_types = "text"
) %>%
  clean_names()

formulario_7000 %<>%
  select(
    nombre_referido,
    cedula_referido,
    fecha_registro,
    cedula_agente,
    nombre_agente
  ) %>% 
  distinct(cedula_referido,.keep_all = TRUE)

# CARGAMOS LA BASE FINAL DE LA RED COOPERAMOS PARA INICIAR CON EL INFORME

base_final_usc <- base_final %>% 
  left_join(
    planta_auxiliares_cs,
    by=c("tinker_id_personal"="identification")
  ) %>% 
  left_join(
    formulario_7000,
    by=c("referido_id_personal"="cedula_referido")
  ) %>% 
  left_join(
    planta_auxiliares_cs,
    by=c("cedula_agente"="identification")
  ) %>% 
  mutate(Val_Fecha_junio_Comienzo_campana_registro=if_else(referido_fecha_registro>="2021-06-20","Si","No"),
         Tipo_De_Comunicacion=case_when(nombre_1.x!=is.na(nombre_1.x)&Val_Fecha_junio_Comienzo_campana_registro=="Si"~"Comunicación Cliente o Usuario",
                                        nombre_referido!=is.na(nombre_referido)&Val_Fecha_junio_Comienzo_campana_registro=="Si"~"Comunicación Asociado"),
         Coordinador_Auxiliar=case_when(Tipo_De_Comunicacion=="Comunicación Cliente o Usuario"~coordinador.x,
                                        Tipo_De_Comunicacion=="Comunicación Asociado"~coordinador.y),
         Cedula_Auxiliar=case_when(Tipo_De_Comunicacion=="Comunicación Cliente o Usuario"~tinker_id_personal,
                                   Tipo_De_Comunicacion=="Comunicación Asociado"~cedula_agente),
         Nombre_Auxiliar=case_when(Tipo_De_Comunicacion=="Comunicación Cliente o Usuario"~tinker_nombre,
                                   Tipo_De_Comunicacion=="Comunicación Asociado"~nombre_agente),
         Cumple_Perfil=case_when(referido_efectivo=="Si"&canal_vinculacion_referido=="Fuerza Comercial"~"Referido Efectivo",
                                 preselecta==is.na(preselecta)&canal_vinculacion_referido=="Fuerza Comercial"~"Gestionado Por La Fuerza Comercial",
                                 preselecta==is.na(preselecta)&canal_vinculacion_referido=="Otro"~"Gestionado Por Otros Canales",
                                 referido_efectivo=="Si"~"Referido Efectivo",
                                 preselecta=="Asociado"~"Ya Se Encuentra Asociado",
                                 preselecta=="ERROR"~"Inconsistencia En Los Datos",
                                 preselecta=="SI"~"Si Cumple Con Puntaje Preselecta",
                                 preselecta=="NO"~"No Cumple Con Puntaje Preselecta",
                                 preselecta=="No Aplica"~"No Cumple Preselecta",
                                 preselecta=="En proceso"~"Ya Se Encuentra En Proceso"),
         campana_Auxiliar=case_when(Tipo_De_Comunicacion=="Comunicación Cliente o Usuario"~campana_costeo.x,
                                    Tipo_De_Comunicacion=="Comunicación Asociado"~campana_costeo.y),
         Sede_Auxiliar=case_when(Tipo_De_Comunicacion=="Comunicación Cliente o Usuario"~sede.x,
                                 Tipo_De_Comunicacion=="Comunicación Asociado"~sede.y)) 

base_final_usc_completa <- base_final_usc


gc()


# Exportamos la base final --------------------------------------------------- campana USC

#quitamos los duplicados para no tener errores de datos en la muestra de informacion
# base final
base_final_usc %<>% 
  filter(Tipo_De_Comunicacion=="Comunicación Asociado"|Tipo_De_Comunicacion=="Comunicación Cliente o Usuario")


# base_registrados_mes
Registrados_Mes_Usc <- base_final_usc %>% 
  filter(Mes_Registro==mes_actual,
         ano_registro==ano_actual,
         Tipo_De_Comunicacion=="Comunicación Asociado"|Tipo_De_Comunicacion=="Comunicación Cliente o Usuario")

# base_efectivos_mes
Efectivos_Mes_Usc <- base_final_usc %>% 
  filter(Mes_Vinculacion==mes_actual,
         ano_vinculacion==ano_actual,
         referido_efectivo=="Si",
         Tipo_De_Comunicacion=="Comunicación Asociado"|Tipo_De_Comunicacion=="Comunicación Cliente o Usuario")


sheets2 <- list("Base_Final" = base_final_usc, "Registrados_Mes" = Registrados_Mes_Usc,"Efectivos_Mes" = Efectivos_Mes_Usc) #assume sheet1 and sheet2 are data frames
write_xlsx(sheets2, ruta_completaa)

rm(formulario_7000)

#-------------------------------------------------------------------------------------------------------------

# BASE PROSPECTOS REFERIDOS- BASE PROSPECTOS REFERIDOS - BASE PROSPECTOS REFERIDOS- BASE PROSPECTOS REFERIDOS - BASE PROSPECTOS REFERIDOS- BASE PROSPECTOS REFERIDOS
# BASE PROSPECTOS REFERIDOS- BASE PROSPECTOS REFERIDOS - BASE PROSPECTOS REFERIDOS- BASE PROSPECTOS REFERIDOS - BASE PROSPECTOS REFERIDOS- BASE PROSPECTOS REFERIDOS

# para exportar las bases en diferentes hojas en el mismo libro es necesario

archivooo="Prospectos_Referidos_"
hoyyy={today()}
exttt=".xlsx"
nombreee_archivo=paste0(archivooo,hoyyy,exttt)

rutaaa="C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/output_data/"
ruta_completaaa=paste0(rutaaa,nombreee_archivo)



base_final_prospectos_referidos <- base_final_usc_completa %>% 
  filter(ano_registro==2022) %>% 
  mutate(campana_usc=if_else(Cedula_Auxiliar!=is.na(Cedula_Auxiliar),"Si","No")) %>% 
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

base_fc_prospectos_referidos <- crm_exportar %>% 
  filter(ano_registro_crm==2022) %>% 
  rename(nombre_campana=origen_de_la_campana_principal_nombre_de_la_campana,
         ciudad=propietario_de_oportunidad_ciudad,
         fecha_registro=fecha_de_creacion,
         nombre_referente=propietario_de_oportunidad_nombre_completo,
         cedula_referente=propietario_de_oportunidad_identificacion,
         nombre_referido=nombre_de_la_cuenta_nombre_de_la_cuenta,
         cedula_referido=nombre_de_la_cuenta_numero_de_identificacion) %>% 
  select(nombre_campana,
         regional_registrada_crm,
         ciudad,
         etapa,
         nombre_de_la_oportunidad,
         fecha_registro,
         nombre_referente,
         cedula_referente,
         nombre_referido,
         cedula_referido)

base_efectivos_prospectos_referidos <- referidos_efectivos_2022 %>% 
  select(referido_fuente,
         referido_fecha_registro,
         referido_fecha_ing,
         referido_nombre,
         referido_id_personal,
         tinker_nombre,
         tinker_id_personal,
         colaborador_empresa,
         colaborador_regional,
         colaborador_puesto_de_trabajo,
         colaborador,
         canal_vinculacion_referido)


# para exportar las bases en diferentes hojas en el mismo libro es necesario
sheets3 <- list("Base_Final" = base_final_prospectos_referidos, "Registrados_Fuerza_Ventas" = base_fc_prospectos_referidos, "Referidos_Efectivos"= base_efectivos_prospectos_referidos) #assume sheet1 and sheet2 are data frames
write_xlsx(sheets3, ruta_completaaa)


gc()

#rm(base_final_usc_completa)

#-------------------------------------------------------------------------------------------------------------

# BASE REPORTE MP - BASE REPORTE MP - BASE REPORTE MP - BASE REPORTE MP - BASE REPORTE MP - BASE REPORTE MP - BASE REPORTE MP - BASE REPORTE MP
# BASE REPORTE MP - BASE REPORTE MP - BASE REPORTE MP - BASE REPORTE MP - BASE REPORTE MP - BASE REPORTE MP - BASE REPORTE MP - BASE REPORTE MP



archivoooo="Base_Red_Cooperamos_MP_"
hoyyyy={today()}
extttt=".xlsx"
nombreeee_archivo=paste0(archivoooo,hoyyyy,extttt)

rutaaaa="C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos 2.0 - 2022/output_data/"
ruta_completaaaa=paste0(rutaaaa,nombreeee_archivo)

# CARGAMOS LA PLANTA DE MEDICINA PREPAGADA
files_planta_auxiliares_mp<- list.files(
  "C:/Users/yrhu0672/Desktop/R/Informe - Red Cooperamos - 2022/raw_data/",
  "Planta comercial MP",
  full.names = TRUE
)

planta_auxiliares_mp <- read_excel(
  files_planta_auxiliares_mp[[1]],
  col_types = "text"
) %>%
  clean_names()


base_final_mp <- base_final%>%
  left_join(
    planta_auxiliares_mp,
    by=c("tinker_id_personal"="n_documento_de_identidad")
  )  %>% 
  filter(nombre_completo!=is.na(nombre_completo),ano_registro==2022,Mes_Registro=="abril"|Mes_Registro=="mayo"|Mes_Registro=="junio"|Mes_Registro==mes_actual) %>% 
  rename(cedula_ejecutivo_mp=tinker_id_personal,
         nombre_ejecutivo_mp=tinker_nombre,
         regional_mp=regional,
         regional=regional_final,
         cedula_promotor=referido_cedula_promotor,
         # nombre_promotor=referido_nombre_promotor,
         jefe_mp=nombre_jefe) %>%
  mutate(pago_asociado="",
         fecha_pago_asociado="",
         estado_referido_mp=case_when(referido_efectivo=="Si"~"referido efectivo",
                                      TRUE~"en proceso")) %>% 
  select(cedula_ejecutivo_mp,
         nombre_ejecutivo_mp,
         referido_id_personal,
         referido_nombre,
         Dia_Registro,
         Mes_Registro,
         ano_registro,
         Dia_Vinculacion,
         Mes_Vinculacion,
         ano_vinculacion,
         regional,
         referido_efectivo,
         cedula_promotor,
         #   nombre_promotor,
         estado_referido_mp,
         referido_cuota_pagada,
         pago_asociado,
         fecha_pago_asociado,
         regional_mp,
         jefe_mp) %>% 
  distinct(referido_id_personal,.keep_all = TRUE)


# para exportar las bases en diferentes hojas en el mismo libro es necesario
sheets4 <- list("Base_Final_MP" = base_final_mp) #assume sheet1 and sheet2 are data frames
write_xlsx(sheets4, ruta_completaaaa)



rm(call_center_marcaciones,
   call_center_marcaciones_fecha,
   call_center_ultima_llamada,
   consolidado_gestor_trafico,
   crm,
   crm_ejecutivos,
   crm_exportar,
   crm_gestion,
   detalle_referidos_registrados_acumulados,
   eliminados_formulario,
   tinker,
   tinker_referidos)


gc()
