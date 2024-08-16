#INSTRUCCIONES
#1.CAMBIAR LA FECHA_ACTUAL_CIERRE MENSUALMENTE EN EL MES QUE NO SE HA CERRADO Y ESTA ACTUALMENTE(COLUMNA CATEGORIA DE PAGO)

hoy=Sys.Date()
fecha_filtro={today()}-90

#CORTE PARA FILTRAR LOS FORMATOS
dia_corte=as.numeric(format(Sys.Date(),'%d'))
corte=if_else(dia_corte>=5&dia_corte<10,"Al 05 del Mes",
              if_else(dia_corte>=10&dia_corte<15,"Al 10 del Mes",
                      if_else(dia_corte>=15&dia_corte<20,"Al 15 del Mes",
                              if_else(dia_corte>=20&dia_corte<25,"Al 20 del Mes",
                                      if_else(dia_corte>=25&dia_corte<30,"Al 25 del Mes",
                                              if_else(dia_corte>=30&dia_corte<5,"Al 30 del Mes","Sin Corte"))))))


#codigo para quitar espacios en blanco al principio y al final
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#FECHA DESDE DONDE APLICA LOS PAGOS DE REFERIDOS
fecha_inicio="2021-08-20"#FECHA DONDE INICIA EL NUEVO MODELO DE REFERIDOS
fecha_actual_cierre="2022-08-01" #MES ACTUAL PARA SACAR LA COLUMNA DE CATEGORIA DE PAGO(mayor al primer dia del mes)

# Establecemos la conexion con la base de datos
#fecha del primer registrado en tink - junio 2020
con <- DBI::dbConnect(odbc::odbc(), "Multiactiva - MULCLIDAT",
                      UID = "yrhu0672", PWD = "Abby2024", Database = "CLIMAE",
                      trusted_connection = TRUE)

#INFORMACION DE LOS ASOCIADOS-25MIN
conexion1  <- dbGetQuery ( con , " SELECT DISTINCT NITCLI,FECING,ASOCIA,TIPCLI,TIPDOC,FECRET,AGCVIN,PROCOD,ESTASO,NUMINT,INDEST,ASOCOR,ASOCOD
                                   FROM D103285p.MULCLIDAT.CLIMAE 
                                   ORDER BY NITCLI, FECING DESC") %>% 
  rename(referido_fecha_ing=FECING,
         referido_asociado=ASOCIA,
         referido_tipo_cliente=TIPCLI,
         referido_tipo_documento=TIPDOC,
         cedula_referido=NITCLI,
         referido_codigo_oficina=AGCVIN,
         referido_fecha_retiro=FECRET,
         referido_cedula_promotor=PROCOD,
         referido_codigo_estado=ESTASO,
         referido_codigo_unico=NUMINT,
         referido_codigo_nivel_academico=INDEST,
         referido_codigo_corte=ASOCOR,
         codigo_primera_contribuccion=ASOCOD) %>%
  mutate(referido_asociado=case_when(referido_asociado==1~"asociado",
                                     TRUE~"no asociado"),
         cedula_referido=as.character(cedula_referido),
         referido_fecha_ing= ymd(referido_fecha_ing)) %>% 
  clean_names()



#CODIGO ESTADO=ESTASO
conexion2  <- dbGetQuery ( con , " SELECT CODTAB,CODINT,CODNOM 
                                   FROM D103285P.MULINDFIN.CLITAB 
                                   WHERE CODTAB=110") %>%
  rename(referido_codigo_estado=CODINT,
         referido_nombre_estado=CODNOM) %>% 
  select(referido_codigo_estado,
         referido_nombre_estado) %>% 
  mutate(referido_nombre_estado=if_else(referido_codigo_estado==48,"Expulsion de la Cooperativa",
                                        if_else(referido_codigo_estado==24,"Suspendido por Terminacion Tiempo Inactivo",referido_nombre_estado))) %>% 
  clean_names()



#FECHA PRIMERA CONTRIBUCION-15MIN
conexion3  <- dbGetQuery ( con , " SELECT CTANRO, MIN(FORIGE) as FEPRCO
                                    FROM D103285P.MULAPODAT.APOHISTORY
                                    GROUP BY CTANRO") %>%
  rename(codigo_primera_contribuccion=CTANRO,
         fecha_primera_contribuccion=FEPRCO) %>% 
  mutate(codigo_primera_contribuccion=as.numeric(codigo_primera_contribuccion),
         fecha_primera_contribuccion=ymd(fecha_primera_contribuccion))


#INFORMACION FINANCIERA DEL ASOCIADO-20MIN
conexion4  <- dbGetQuery ( con , " SELECT DISTINCT NITCTA,CUOFAC,CODPRO,CUOCAN,FAPERT,FCANCE 
                                   FROM D103285P.MULAPODAT.APOMAECTA 
                                   WHERE CODPRO=41
                                   ORDER BY FCANCE, FAPERT DESC
                           ") %>%
  
  rename(referido_codigo_unico=NITCTA,
         referido_cuota_facturada=CUOFAC,
         referido_cuota_pagada=CUOCAN,
         referido_fecha_apertura=FAPERT,
         referido_fecha_cancelacion=FCANCE) %>% 
  select(-CODPRO) %>% 
  clean_names()%>% 
  distinct(referido_codigo_unico, .keep_all = TRUE)#VALIDAR SI EL DISTINCT NO ESTA GENERANDO INFORMACION ERRADA

#conexion4 <- conexion4[with(conexion4, order(-conexion4$asociado_fecha_apertura)), ]#MAS RECIENTE A MAS ANTIGUA(DESC)
#conexion4 <- conexion4[with(conexion4, order(conexion4$asociado_fecha_cancelacion)), ]#MAS ANTIGUA A MAS RECIENTE(SOLA SIN DESC)
#conexion4 %<>%
#  distinct(asociado_codigo_unico, .keep_all = TRUE)

#CODIGO CORTES
conexion5  <- dbGetQuery ( con , " SELECT CODTAB,CODINT,CODNOM 
                                   FROM D103285P.MULCLIDAT.CLITAB
                                   WHERE CODTAB=93") %>%
  rename(referido_codigo_corte=CODINT,
         referido_nombre_corte=CODNOM) %>% 
  select(referido_nombre_corte,
         referido_codigo_corte) %>% 
  clean_names()

#REGIONAL
conexion6  <- dbGetQuery ( con , " SELECT AGCORI,NOMAGC,CODCIU,CODSUC,CODZON 
                                   FROM D103285P.MULINDFIN.PLTAGCORI") %>%
  rename(referido_codigo_oficina=AGCORI,
         referido_nombre_agencia=NOMAGC,
         referido_codigo_ciudad=CODCIU,
         referido_codigo_regional=CODSUC,
         referido_codigo_zona=CODZON,
  ) %>%
  mutate(referido_regional=case_when(referido_codigo_regional==1~"Cali",
                                     referido_codigo_regional==2~"Eje Cafetero",
                                     referido_codigo_regional==3~"Medellin",
                                     referido_codigo_regional==4~"Palmira",
                                     referido_codigo_regional==5~"Bogota",
                                     referido_codigo_regional==6~"Caribe",
                                     TRUE~"Sin Regional"))


#CODIGO ZONA
conexion7  <- dbGetQuery ( con , " SELECT CODTAB,CODINT,CODNOM 
                                   FROM D103285P.MULCLIDAT.CLITAB
                                   WHERE CODTAB=908") %>%
  rename(referido_codigo_zona=CODINT,
         referido_nombre_zona=CODNOM) %>% 
  select(referido_codigo_zona,
         referido_nombre_zona) %>% 
  clean_names()



# CONSOLIDAMOS LAS CONEXIONES 
base_final_conexiones <- conexion1 %>% 
  left_join(conexion2, by ="referido_codigo_estado") %>% 
  left_join(conexion3, by ="codigo_primera_contribuccion") %>%
  left_join(conexion4, by ="referido_codigo_unico") %>%
  left_join(conexion5, by ="referido_codigo_corte") %>%
  left_join(conexion6, by ="referido_codigo_oficina") %>% 
  left_join(conexion7, by ="referido_codigo_zona")


# rm(archivo_consolidado_de_pagos,
#    archivo_consolidado_de_pagos2,
#    archivo_premio_x_logro,
#    archivo_recategorizacion,
#    base_pagos_pxl,
#    archivo_novedades_operaciones,
#    archivo_excepciones,
#    archivo_gestion_telemercadeo,
#    archivo_planta_colaboradores,
#    archivo_planta_lico,
#    archivo_planta_usc,
#    archivo_referentes,
#    archivo_referidos,
#    archivo_sms,
#    archivo_terceros,
#    pagos_red_cooperamos,
#    referente_fuerza_comercial,
#    tabla_conteo_final,
#    tabla_conteo_final1,
#    tabla_conteo_referidos_vinculados_inicio,
#    tabla_conteo_referidos_vinculados_mes_actual,
#    tabla_orden_historico_categ_nombramiento,
#    base_pagos,
#    base_pagos_cruce,
#    formato_abono_estado_de_cuenta,
#    formato_giros_y_consignaciones,
#    tabla_formato_abono_estado_de_cuenta,
#    tabla_formato_giros_y_consignaciones,
#    formato_pxl_giros_y_consignaciones,
#    tabla_conteo_final_usc,
#    tabla_conteo_referidos_vinculados_mes_actual_usc,
#    archivo_codigos_centros_de_costos,
#    archivo_codigos_ciudad,
#    consolidado_formulario_7000,
#    archivo_cedulas_rl_corretaje,
#    archivo_sucursal)

#--------------------------------------------------------------------------------------------------------------------------
# SUBIMOS EL ARCHIVO CONSOLIDADO DE PAGO - HISTORICO

archivo_consolidado_de_pagos <- read_xlsx(path = "./raw_data/consolidado_de_pagos.xlsx") %>% 
  clean_names()


#--------------------------------------------------------------------------------------------------------------------------

# ARCHIVO REFERIDOS - HERRAMIENTA PROPIA
## SUBIMOS EL ARCHIVO DE REFERIDOS HERRAMIENTA PROPIA(O DE TINK)
files_referidos <- list.files(
  "./raw_data/",
  "Reporte_referido_export",
  full.names = TRUE
)

archivo_referidos <- read_excel(
  files_referidos[[1]]) %>% 
  clean_names() %>% 
  rename(cedula_referido=numero_de_identificacion,
         cedula_asociado=cedula_referente,
         nombre_asociado=nombre_referente,
         telefono_asociado=telefono_referente,
         correo_asociado=correo_referente) %>% 
  select(cedula_referido,
         nombre_referido,
         telefono_referido,
         correo_referido,
         fecha_registro_referido,
         cedula_asociado,
         nombre_asociado,
         telefono_asociado,
         correo_asociado) %>% 
  mutate(fecha_registro_referido = as.Date(fecha_registro_referido),
         cedula_referido=as.character(cedula_referido),
         cedula_asociado=as.character(cedula_asociado))


# ARCHIVO REFERENTES - HERRAMIENTA PROPIA --------------------------------------------------------------------------------------
## SUBIMOS EL ARCHIVO DE REFERIDOS HERRAMIENTA PROPIA(O DE TINK)

files_referentes <- list.files(
  "./raw_data/",
  "Reporte_referente_export",
  full.names = TRUE
)

archivo_referentes <- read_excel(
  files_referentes[[1]]) %>% 
  clean_names() %>% 
  rename(cedula_asociado=numero_de_identificacion,
         metodo_de_pago_asociado=metodo_de_pago,
         ciudad_sucursal_de_pago_asociado=ciudad_sucursal_de_pago,
         sucursal_pago_seleccionado_asociado=sucursal_pago_seleccionado,
         tipo_cuenta_asociado=tipo_cuenta,
         numero_de_cuenta_asociado=numero_de_cuenta,
         entidad_financiera_asociado=entidad_financiera) %>% 
  select(cedula_asociado,
         metodo_de_pago_asociado,
         ciudad_sucursal_de_pago_asociado,
         sucursal_pago_seleccionado_asociado,
         tipo_cuenta_asociado,
         numero_de_cuenta_asociado,
         entidad_financiera_asociado) %>% 
  mutate(cedula_asociado=as.character(cedula_asociado)) %>% 
  distinct(cedula_asociado, .keep_all = TRUE)


#ORDENAR INFORMACION POR FECHA REGISTRO REFERIDO
#archivo_referentes <-archivo_referentes[order(archivo_referentes$fecha_registro_referido,decreasing = TRUE), ]


#------------------------------------------------------------------------------------------------------------------------------         

#UNIMOS EL ARCHIVO DE REFERIDOS CON LA BASE CONEXION CONSOLIDADA
pagos_red_cooperamos <- archivo_referidos %>% 
  left_join(base_final_conexiones, by="cedula_referido") %>% 
  mutate(mes_vinculacion=months.Date(referido_fecha_ing),
         ano_vinculacion=as.numeric(format(referido_fecha_ing,'%Y')),
         dia_vinculacion=as.numeric(format(referido_fecha_ing,'%d')),
         dias=referido_fecha_ing-fecha_registro_referido,
         referido_efectivo=if_else(mes_vinculacion %in% c("enero",
                                                          "febrero",
                                                          "marzo",
                                                          "abril",
                                                          "mayo",
                                                          "junio",
                                                          "julio",
                                                          "agosto",
                                                          "septiembre",
                                                          "octubre") & referido_fecha_ing>=fecha_registro_referido & dias<60&ano_vinculacion==2021&referido_asociado=="asociado",
                                   "Si",if_else(mes_vinculacion %in% c("noviembre",
                                                                       "diciembre")&referido_fecha_ing>=fecha_registro_referido&ano_vinculacion==2021&referido_asociado=="asociado",
                                                "Si",if_else(mes_vinculacion %in% c("enero",
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
                                                                                    "diciembre")&referido_fecha_ing>=fecha_registro_referido&ano_vinculacion==2022&referido_asociado=="asociado",
                                                             "Si","No"))),
         
         referido_efectivo = if_else(
           is.na(referido_efectivo),
           "No",
           referido_efectivo
         )) %>% 
  select(cedula_referido,#SELECCIONAMOS LOS CAMPOS CON LO CUAL VA QUEDAR EL "REPORTE PAGOS" HASTA ESTE MOMENTO
         nombre_referido,
         telefono_referido,
         correo_referido,
         fecha_registro_referido,
         cedula_asociado,
         nombre_asociado,
         telefono_asociado,
         correo_asociado,
         referido_fecha_ing,
         referido_efectivo,
         referido_cuota_pagada,
         fecha_primera_contribuccion,
         referido_cedula_promotor,
         referido_nombre_zona,
         referido_regional)

# COLUMNA CATEGORIA DE NOMBRAMIENTO
##CONTEO REFERIDOS EFECTIVOS PARA SACAR LA COLUMA "CATEGORIA DE NOMBRAMIENTO"
tabla_conteo_referidos_vinculados_inicio <- pagos_red_cooperamos %>% 
  filter(referido_efectivo=="Si",fecha_registro_referido>=fecha_inicio,referido_fecha_ing>=fecha_inicio) %>% 
  select(cedula_asociado) %>% 
#  group_by(cedula_asociado) %>% 
  count(cedula_asociado) %>% 
  rename(cantidad_referidos_vinculados=n)

#UNIMOS LA COLUMNA "CATEGORIA DE NOMBRAMIENTO" A LA BASE
pagos_red_cooperamos %<>%
  left_join(tabla_conteo_referidos_vinculados_inicio,by="cedula_asociado") %>% 
  mutate(categoria_nombramiento=case_when(cantidad_referidos_vinculados>0&cantidad_referidos_vinculados<3~"pionero",
                                          cantidad_referidos_vinculados>=3&cantidad_referidos_vinculados<5~"lider",
                                          cantidad_referidos_vinculados>=5~"mentor", 
                                          TRUE~"Sin Nombramiento"))

#COLUMNA ESTADO ASOCIADO(TINKER)ACTIVO, ACTIVO COBRANZA, ETC Y COLUMNA INDICATIVO_UNICO ----------------------------------------
#BUSCAMOS EL ESTADO DEL TINKER(HACEMOS UN CRUCE CON LA COLUMNA CEDULA_REFERIDO DE BASE CONEXIONES Y TINKER_ID_PERDONAL DE BASE PAGOS RED COOPERAMOS)
estado_asociado <- base_final_conexiones %>% 
  select(cedula_referido,
         referido_nombre_estado)
gc()
pagos_red_cooperamos %<>%
  left_join(estado_asociado,by=c("cedula_asociado"="cedula_referido")) %>% 
  rename(estado_asociado=referido_nombre_estado) %>% 
  mutate(indicativo_unico=paste0(cedula_asociado,cedula_referido))




#--------------------------------------------------------------------------------------------------------------------------------
#  ---------------------------------------------------------------------------------------------------------------------
#    ----------------------------------------------------------------------------------------------------------


#COLUMNA CATEGORIA DE PAGO
# SE NECESITA EL CONSOLIDADO PARA REALIZAR LA COLUMNA CATEGORIA DE PAGO

#COLUMNA CATEGORIA DE PAGO
#CREAMOS UNA TABLA ADICIONAL PARA LA COLUMNA CATEGORIA DE PAGO - TABLA VINCULACIONES MES ACTUAL
tabla_conteo_referidos_vinculados_mes_actual <- pagos_red_cooperamos %>%
  filter(referido_efectivo=="Si",referido_fecha_ing>=fecha_actual_cierre) %>%
  select(cedula_asociado) %>%
  count(cedula_asociado) %>%
  rename(vinculaciones_mes_actual=n)


tabla_conteo_final <- tabla_conteo_referidos_vinculados_inicio %>%
  left_join(tabla_conteo_referidos_vinculados_mes_actual,by="cedula_asociado")


#TABLA CANTIDAD DE CATEGORIA DE NOMBRAMIENTO - SE COGE EL HISTORICO SIN INCLUIR EL MES EN CURSO PARA PODER REALIZAR LA CATEGORIA DE PAGO
tabla_orden_historico_categ_nombramiento <- archivo_consolidado_de_pagos %>% 
  select(fecha_registro_referido,
         cedula_asociado,
         categoria_nombramiento) %>% 
  mutate(fecha_registro_referido = as.Date(fecha_registro_referido,format="%d/%m/%Y"),
         cedula_asociado=as.double(cedula_asociado)) %>% 
  rename(categoria_nombramiento_cons_pagos=categoria_nombramiento)

#ORDENAMOS LA TABLA PARA QUE QUEDEN LAS CEDULAS PEGADAS Y LA FECHA DE REGISTRO MAS RECIENTE ARRIBA PARA QUITAR DUPLICADOS
tabla_orden_historico_categ_nombramiento <-tabla_orden_historico_categ_nombramiento[order(tabla_orden_historico_categ_nombramiento$cedula_asociado, tabla_orden_historico_categ_nombramiento$fecha_registro_referido,decreasing = TRUE), ]

#QUITAMOS DUPLICADOS DE LA TABLA
tabla_orden_historico_categ_nombramiento %<>% 
  distinct(cedula_asociado, .keep_all = TRUE) %>% 
  select(-fecha_registro_referido) %>% 
  mutate(cedula_asociado=as.character(cedula_asociado))

#UNIMOS A LA TABLA CONTEO FINAL
tabla_conteo_final %<>%
  left_join(tabla_orden_historico_categ_nombramiento,by="cedula_asociado")



#CATEGORIA PAGO
tabla_conteo_final %<>% 
  mutate(categoria_nombramiento_real=case_when(cantidad_referidos_vinculados>0&cantidad_referidos_vinculados<3~"pionero",
                                               cantidad_referidos_vinculados>=3&cantidad_referidos_vinculados<5~"lider",
                                               cantidad_referidos_vinculados>=5~"mentor", 
                                               TRUE~"Sin Nombramiento"),
         vinculaciones_minimas=case_when(categoria_nombramiento_real=="mentor"~3,#SE CAMBIO
                                         categoria_nombramiento_real=="lider"~1,#SE CAMBIO
                                         categoria_nombramiento_real=="pionero"~1,#SE CAMBIO
                                         TRUE~0),
         categoria_de_pago=case_when(categoria_nombramiento_real=="mentor"&vinculaciones_mes_actual>=3~"mentor",#SE CAMBIO
                                  categoria_nombramiento_real=="lider"&vinculaciones_mes_actual>=1~"lider",#SE CAMBIO
                                  categoria_nombramiento_real=="pionero"&vinculaciones_mes_actual>=1~"pionero",#SE CAMBIO
                                  categoria_nombramiento_real=="mentor"&vinculaciones_mes_actual<3~"pionero",#SE CAMBIO
                                  categoria_nombramiento_real=="lider"&vinculaciones_mes_actual<1~"pionero",#SE CAMBIO
                                  categoria_nombramiento_real=="pionero"&vinculaciones_mes_actual<1~"pionero",#SE CAMBIO
                                  TRUE~"sin categoria de pago"))

#CREAMOS LA MISMA TABLA DE CONTEO FINAL PERO SOLO LAS COLUMNAS QUE NECESITAMOS PARA EL ARCHIVO DE PAGOS(CEDULA Y CATEGORIA PAGO)     
tabla_conteo_final1 <-  tabla_conteo_final %>% 
  select(cedula_asociado,
         categoria_de_pago)
  
#COLUMNA CATEGORIA DE PAGO
pagos_red_cooperamos %<>% 
  left_join(tabla_conteo_final1,by="cedula_asociado")




#--------------------------------------------------------------------------------------------------------------------------------
#  ---------------------------------------------------------------------------------------------------------------------
#  ----------------------------------------------------------------------------------------------------------

#COLUMA EXCEPCIONES
## SUBIMOS EL ARCHIVO EXCEPCIONES
files_excepciones <- list.files(
  "./raw_data/",
  "consolidado_excepciones",
  full.names = TRUE
)

archivo_excepciones <- read_xlsx(
  files_excepciones[[1]]) %>%
  distinct(indicativo_unico, .keep_all = TRUE) %>% 
  select(indicativo_unico,
         excepcion_estado,
         descripcion_excepcion) 

#UNIMOS LA BASE A LA NUEVA COLUMNA DE EXCEPCIONES
pagos_red_cooperamos %<>% 
  left_join(archivo_excepciones,by="indicativo_unico")


#COLUMNAS DE LA PLANTA LICO -------------------------------------------------------------------------------------
#CARGAMOS LA PLANTA LICO CON LA INFORMACION DE TODOS LOS ASESORES
files_planta_lico <- list.files(
  "./raw_data/",
  "planta_lico",
  full.names = TRUE
)

archivo_planta_lico <- read_xlsx(
  files_planta_lico[[1]]) %>%
  mutate(cedula_promotor=as.double(cedula_promotor)) %>% 
  distinct(cedula_promotor, .keep_all = TRUE)

#UNIMOS LA BASE A LA NUEVA COLUMNA QUE NOS VA TRAER TODA LA INFORMACION DE LOS ASESORES
pagos_red_cooperamos %<>% 
  left_join(archivo_planta_lico,by=c("referido_cedula_promotor"="cedula_promotor")) %>% 
  select(-regional_promotor,
         -zona_promotor,
         -fecha_ingreso_promotor)

#COLUMNA REFERENTE_FUERZA_COMERCIAL-----------------------------------------------------------------------
#CREAMOS UNA TABLA PARA LUEGO CRUZAR Y VALIDAR SI EL REFERENTE ES DE LA FUERZA COMERCIAL
referente_fuerza_comercial <- archivo_planta_lico %>% 
  select(cedula_promotor) %>% 
  mutate(referente_fuerza_comercial="Si",
         cedula_promotor=as.character(cedula_promotor))

pagos_red_cooperamos %<>%
  left_join(referente_fuerza_comercial,by=c("cedula_asociado"="cedula_promotor")) %>% 
  mutate(referente_fuerza_comercial=case_when(referente_fuerza_comercial=="Si"~"Si",
                                              TRUE~"No"))

#COLUMNA AUXILIAR CAMPAÑA USC-----------------------------------------------------------------------------------------
#CARGAMOS LA PLANTA AUXILIARES USC
files_planta_auxiliares_usc <- list.files(
  "./raw_data/",
  "planta_auxiliares_cs",
  full.names = TRUE
)

archivo_planta_usc <- read_xlsx(
  files_planta_auxiliares_usc[[1]]) %>%
  distinct(cedula, .keep_all = TRUE) %>% 
  select(cedula,
         auxiliar_campana_usc) %>% 
  mutate(cedula=as.character(cedula))

pagos_red_cooperamos %<>% 
  left_join(archivo_planta_usc,by=c("cedula_asociado"="cedula"))




#COLUMNA CORRESPONDE PREMIO AL ASOCIADO-----------------------------------------------------------------------
pagos_red_cooperamos %<>%
  mutate(corresponde_premio_al_asociado=if_else(referido_efectivo == "Si"&referido_cuota_pagada >= 1&referente_fuerza_comercial=="No",
                                                "Si","No"),
         corresponde_premio_al_asociado=case_when(excepcion_estado=="excepcion"~"Si",
                                                  TRUE~corresponde_premio_al_asociado),
         corresponde_premio_al_asociado=case_when(auxiliar_campana_usc=="aux_usc"~"No Aux Usc",
                                                  TRUE~corresponde_premio_al_asociado))


#COLUMNA ESTADO PAGO, PREMIO ASOCIADO-----------------------------------------------------------------------
pagos_red_cooperamos %<>%
  mutate(premio_asociado=case_when(corresponde_premio_al_asociado=="Si"&categoria_de_pago=="pionero"~"100000",
                                   corresponde_premio_al_asociado=="Si"&categoria_de_pago=="lider"~"120000",
                                   corresponde_premio_al_asociado=="Si"&categoria_de_pago=="mentor"~"150000",
                                   TRUE~"0"),
         fecha_de_pago_asociado={today()},#VALIDAR ESTA COLUMNA
         fecha_de_solicitud="",
         estado_de_pago="",#SE DEBE DE SACAR DE ARCHIVO MANUAL
         canal_de_pago_asociado="")#SE DEBE DE SACAR DE ARCHIVO MANUAL

#CONVERTIMOS LA FECHA_DE_SOLICITUD EN TIPO DATE
pagos_red_cooperamos$fecha_de_solicitud=as.character(pagos_red_cooperamos$fecha_de_solicitud)



# COLUMNAS = medio_pago_asociado,	numero cuenta,	banco,	tipo_cuenta,	codigo_oficina_giro,	abono_estado_cuenta
#ESTAS COLUMNAS SALEN DE LA HERRAMIENTA PROPIA DEL ARCHIVO DE REFERENTES

pagos_red_cooperamos %<>%
  left_join(archivo_referentes,by="cedula_asociado")



#COLUMNA NOVEDADES PAGO OPERACIONES-----------------------------------------------------------------------
#CARGAMOS EL CONSOLIDADO DE NOVEDADES OPERACIONES


archivo_novedades_operaciones <- read_xlsx(path = "./raw_data/consolidado_novedades_operaciones.xlsx") %>% 
  clean_names() %>% 
  select(indicativo_unico,
         estado_solicitud) %>% 
  rename(novedades_pago_operaciones=estado_solicitud)

pagos_red_cooperamos %<>%
  left_join(archivo_novedades_operaciones,by="indicativo_unico")


#COLUMNA ENVIO SMS-----------------------------------------------------------------------
#CARGAMOS EL CONSOLIDADO SMS
files_sms <- list.files(
  "./raw_data/",
  "consolidado_sms",
  full.names = TRUE
)

archivo_sms <- read_xlsx(
  files_sms[[1]]) %>%
  distinct(indicativo_unico, .keep_all = TRUE) %>% 
  select(-cedula_referido,
         -cedula_asociado)

pagos_red_cooperamos %<>% 
  left_join(archivo_sms,by="indicativo_unico")


#COLUMNA ASOCIADO CREADO COMO TERCERO-----------------------------------------------------------------------
#CARGAMOS EL CONSOLIDADO CREACION DE TERCERO
files_creacion_terceros <- list.files(
  "./raw_data/",
  "consolidado_creacion_de_terceros",
  full.names = TRUE
)

archivo_terceros <- read_xlsx(
  files_creacion_terceros[[1]]) %>%
  distinct(nit, .keep_all = TRUE) %>% 
  select(-numero,
  #       -razon_social,
         -fecha_solicitud,
         -numero_cuenta,
         -banco) %>% 
  mutate(nit=as.character(nit))

pagos_red_cooperamos %<>% 
  left_join(archivo_terceros,by=c("cedula_asociado"="nit"))

#COLUMNA SOLICITUD GESTION TELEMERCADEO-----------------------------------------------------------------------
#CARGAMOS EL CONSOLIDADO GESTION TELEMERCADEO
files_gestion_telemercadeo <- list.files(
  "./raw_data/",
  "consolidado_gestion_telemercadeo",
  full.names = TRUE
)

archivo_gestion_telemercadeo <- read_xlsx(
  files_gestion_telemercadeo[[1]]) %>%
  distinct(cedula, .keep_all = TRUE) %>% 
  select(-contactado_gestion) %>% 
  mutate(cedula=as.character(cedula))

pagos_red_cooperamos %<>% 
  left_join(archivo_gestion_telemercadeo,by=c("cedula_asociado"="cedula"))



#VALIDAR DE DONDE SALE ESTA COLUMNA-----------------------------------------------------------------------------------
#COLUMNA ENVIADO FINANCIERA
pagos_red_cooperamos %<>% 
  mutate(enviado_financiera="no")


#COLUMNA COLABORADOR -------------------------------------------------------------------------------------------------
archivo_planta_colaboradores <- read_xlsx(path = "./raw_data/planta_colaboradores.xlsx") %>% 
  clean_names() %>% 
  select(n_cedula) %>% 
  mutate(colaborador="colaborador",
         n_cedula=as.character(n_cedula)) %>% 
  distinct(n_cedula, .keep_all = TRUE)

pagos_red_cooperamos %<>% 
  left_join(archivo_planta_colaboradores,by=c("cedula_asociado"="n_cedula"))

#COLUMNA CORTE ASOCIADO----------------------------------------------------------------------------------------------

corte_asociado <- base_final_conexiones %>% 
  select(cedula_referido,
         referido_nombre_corte)
gc()
pagos_red_cooperamos %<>%
  left_join(corte_asociado,by=c("cedula_asociado"="cedula_referido")) %>% 
  rename(fecha_corte_asociado=referido_nombre_corte)



#ORGANIZAMOS EL ARCHIVO PARA EXPORTARLO -----------------------------------------------------------------------------

pagos_red_cooperamos %<>% 
  mutate(fecha_vinculacion_referido=referido_fecha_ing,
         administracion_promotor=piloto_call_center) %>% 
  select(indicativo_unico	,
         cedula_referido	,
         nombre_referido	,
         telefono_referido	,
         correo_referido	,
         fecha_registro_referido	,
         cedula_asociado	,
         nombre_asociado	,
         telefono_asociado	,
         correo_asociado	,
         fecha_vinculacion_referido	,
         referido_efectivo	,
         cantidad_referidos_vinculados	,
         estado_asociado	,
         categoria_nombramiento	,
         categoria_de_pago	,
         excepcion_estado	,
         referido_cuota_pagada	,
         fecha_primera_contribuccion	,
         referido_cedula_promotor	,
         nombre_promotor	,
         cargo_promotor	,
         lider_promotor	,
         nombre_lider_promotor	,
         referido_nombre_zona	,
         referido_regional	,
         administracion_promotor	,
         referente_fuerza_comercial	,
         corresponde_premio_al_asociado	,
         fecha_de_pago_asociado	,
         estado_de_pago	,
         premio_asociado	,
         fecha_de_solicitud	,
         canal_de_pago_asociado	,
         metodo_de_pago_asociado	,
         ciudad_sucursal_de_pago_asociado	,
         sucursal_pago_seleccionado_asociado	,
         tipo_cuenta_asociado	,
         numero_de_cuenta_asociado	,
         entidad_financiera_asociado	,
         fecha_corte_asociado	,
         novedades_pago_operaciones	,
         envio_sms	,
         asociado_creado_como_tercero	,
         fecha_de_creacion_tercero,
         solicitud_gestion_telemercadeo	,
         auxiliar_campana_usc	,
         enviado_financiera	,
         colaborador) %>% 
  mutate(estado_de_pago=if_else(corresponde_premio_al_asociado=="Si"&metodo_de_pago_asociado!=""&premio_asociado!="0",
                                    "En proceso de pago",""),
         fecha_de_solicitud=if_else(estado_de_pago!=""|excepcion_estado=="excepcion",hoy,as.Date("")))


#fecha_de_pago_asociado={today()},#VALIDAR ESTA COLUMNA-DEBE SALIR DE ARCHIVO MANUAL
#estado_de_pago="",#VALIDAR ESTA COLUMNA-DEBE SALIR DE ARCHIVO MANUAL - PARA LA OBSERVACION ("SE PAGO")

pagos_red_cooperamos$fecha_corte_asociado <- trim(pagos_red_cooperamos$fecha_corte_asociado)

#--------------------------------------------------------------------------------------------------------------------
#EXPORTAMOS LA BASE FINAL--------------------------------------------------------------------------------------------
pagos_red_cooperamos %>%
 write_xlsx(
   glue(
     "./output_data/",
     "Pagos_Red_Cooperamos_",
     "{today()}.xlsx"
   )
)



#--------------------------------------------------------------------------------------------------------------------


#FILTRAMOS LA BASE PAGOS RED COOPERAMOS POR LA BASE QUE YA TENEMOS DE CONSOLIDADO DE PAGOS PARA FILTRAR SOLO LOS QUE NOS HACEN FALTA Y PEGARLOS DEBAJO
pagos_red_cooperamos <-pagos_red_cooperamos[order(pagos_red_cooperamos$fecha_registro_referido,decreasing = TRUE), ]

#CONVERTIMOS TODAS LAS COLUMNAS EN TIPO TEXTO PARA UNIR SIN PROBLEMAS LAS DOS BASES EN EL SIGUIENTE CODIGO
archivo_consolidado_de_pagos %<>%
  mutate(cedula_referido=as.character(cedula_referido),
         fecha_registro_referido=convert_to_date(fecha_registro_referido),
         cedula_asociado=as.character(cedula_asociado),
         fecha_vinculacion_referido=convert_to_date(fecha_vinculacion_referido),
         fecha_primera_contribuccion=convert_to_date(fecha_primera_contribuccion),
         referido_cedula_promotor=as.integer(referido_cedula_promotor),
         lider_promotor=as.character(lider_promotor),
         premio_asociado=as.character(premio_asociado),
       #  fecha_de_solicitud=as.character(fecha_de_solicitud),
         sucursal_pago_seleccionado_asociado=as.character(sucursal_pago_seleccionado_asociado),
         referido_cuota_pagada=as.double(referido_cuota_pagada),
         fecha_de_pago_asociado=convert_to_date(fecha_de_pago_asociado))



archivo_consolidado_de_pagos2 <- archivo_consolidado_de_pagos %>%
  mutate(fecha_de_solicitud=as.Date(fecha_de_solicitud)) %>% 
  filter(fecha_de_solicitud!=is.na(fecha_de_solicitud),
         metodo_de_pago_asociado!=is.na(metodo_de_pago_asociado))


base_pagos_cruce <- pagos_red_cooperamos %>% 
  distinct(indicativo_unico, .keep_all = TRUE) #%>% 
#  filter(referido_efectivo=="Si",fecha_registro_referido>=fecha_inicio,fecha_vinculacion_referido>=fecha_inicio)

#FILTRAMOS UNA BASE DE DATOS POR OTRA BASE DE DATOS SEGUN UNA COLUMNA EN ESTE CASO "INDICATIVO_UNICO"
base_pagos <- base_pagos_cruce %>% filter(!indicativo_unico%in%archivo_consolidado_de_pagos2[["indicativo_unico"]])


#UNIMOS LA BASE DE PAGOS RESULTANTE CON EL CONSOLIDADO DE PAGOS AÑADIENDO LO QUE HACE FALTA
archivo_consolidado_de_pagos = bind_rows(archivo_consolidado_de_pagos2, base_pagos)


#--------------------------------------------------------------------------------------------------------------


#PREMIO POR LOGRO
#FORMATOS LIQUIDACION PREMIO POR LOGRO
archivo_recategorizacion <- read_xlsx(path = "./raw_data/consolidado_recategorizados.xlsx") %>% 
  clean_names() %>% 
  select(cedula_asociado,
         premio_x_logro) %>% 
  mutate(cedula_asociado=as.character(cedula_asociado)) %>% 
  distinct(cedula_asociado, .keep_all = TRUE)


#ADICIONAMOS LA COLUMNA SI LE CORRESPONDE PREMIO POR LOGRO AL ARCHIVO CONSOLIDADO DE PAGOS
archivo_consolidado_de_pagos %<>%
  left_join(archivo_recategorizacion,by=c("cedula_asociado"="cedula_asociado"))

#CONDICIONES PARA PAGAR PREMIO POR LOGRO
archivo_consolidado_de_pagos %<>% 
  mutate(premio_x_logro=case_when(metodo_de_pago_asociado=="Giro en Oficina Bancoomeva"|metodo_de_pago_asociado=="Consignación a mi cuenta Bancoomeva"|metodo_de_pago_asociado=="Consignación a cuentas de otros Bancos"&
                                  cantidad_referidos_vinculados>=4&
                                  premio_x_logro==""&
                                  estado_asociado=="Activo Cobranza Cobranza Interna"|estado_asociado=="Activo Normal"&
                                  premio_x_logro=="No"~"Si",
                                  TRUE~"No"),
         fecha_de_solicitud_pxl=if_else(premio_x_logro=="Si",hoy,as.Date("")))


#-----------------------------------------------------------------------------------------------------------------

#CAMPAÑA USC
files_formulario_7000<- list.files(
  "./raw_data/",
  "Consolidado referidos 7000",
  full.names = TRUE
)

consolidado_formulario_7000 <- read_excel(
  files_formulario_7000[[1]],
  sheet = "BD",
  col_types = "text"
) %>%
  clean_names()

consolidado_formulario_7000 %<>%
  select(
    #    nombre_referido,
    cedula_referido,
    cedula_agente) %>% 
  #    fecha_registro,
  #    cedula_agente,
  #    nombre_agente) %>% 
  mutate(referido_usc="Si") %>% 
  distinct(cedula_referido, .keep_all = TRUE)

archivo_consolidado_de_pagos %<>%
  left_join(consolidado_formulario_7000,by=c("cedula_referido"="cedula_referido")) %>% 
  mutate(validacion_campana_usc=if_else(fecha_registro_referido>="2021-06-20","Si","No"),
         tipo_de_comunicacion=case_when(auxiliar_campana_usc=="aux_usc"&validacion_campana_usc=="Si"~"Comunicacion Cliente o Usuario",
                                        referido_usc=="Si"&validacion_campana_usc=="Si"~"Comunicacion Asociado"),
         cedula_auxiliar_usc=case_when(tipo_de_comunicacion=="Comunicacion Cliente o Usuario"~cedula_asociado,
                                       tipo_de_comunicacion=="Comunicacion Asociado"~cedula_agente))

estado_asociado_usc <- conexion1 %>% 
  select(cedula_referido,
         referido_asociado) %>% 
  rename(estado_auxiliar_usc=referido_asociado,
         cedula_auxiliar_usc=cedula_referido) %>% 
  distinct(cedula_auxiliar_usc, .keep_all = TRUE)

archivo_consolidado_de_pagos %<>%
  left_join(estado_asociado_usc,by=c("cedula_auxiliar_usc"="cedula_auxiliar_usc"))
#REEMPLAZAR NA POR NO ASOCIADO
archivo_consolidado_de_pagos$estado_auxiliar_usc[is.na(archivo_consolidado_de_pagos$estado_auxiliar_usc)]="no asociado"

# #CATEGORIA DE PAGO
# #TABLAS CONTEOS DE USC PARA PODER SACAR LA CATEGORIA DE PAGO
# tabla_conteo_referidos_vinculados_inicio_usc <- archivo_consolidado_de_pagos %>% 
#   filter(referido_efectivo=="Si",fecha_registro_referido>=fecha_inicio,fecha_vinculacion_referido>=fecha_inicio,cedula_auxiliar_usc!=is.na(cedula_auxiliar_usc)) %>% 
#   select(cedula_auxiliar_usc) %>%
#   count(cedula_auxiliar_usc,) %>% 
#   rename(cantidad_referidos_vinculados_usc=n)

tabla_conteo_referidos_vinculados_mes_actual_usc <- archivo_consolidado_de_pagos %>% 
filter(referido_efectivo=="Si",fecha_vinculacion_referido>=fecha_actual_cierre,cedula_auxiliar_usc!=is.na(cedula_auxiliar_usc)) %>%
  select(cedula_auxiliar_usc,
         tipo_de_comunicacion) %>%
  count(cedula_auxiliar_usc,
        tipo_de_comunicacion) %>%
  rename(vinculaciones_mes_actual=n)

#UNION TABLAS
tabla_conteo_final_usc <- tabla_conteo_referidos_vinculados_mes_actual_usc %>% 
  left_join(estado_asociado_usc,by="cedula_auxiliar_usc")

#REEMPLAZAR VALORES EN 0
#tabla_conteo_final_usc$vinculaciones_mes_actual[is.na(tabla_conteo_final_usc$vinculaciones_mes_actual)]=0
tabla_conteo_final_usc$estado_auxiliar_usc[is.na(tabla_conteo_final_usc$estado_auxiliar_usc)]="no asociado"

#CATEGORIA DE PAGO USC
tabla_conteo_final_usc %<>% 
  mutate(categoria_de_pago_usc=case_when(vinculaciones_mes_actual>0&vinculaciones_mes_actual<2&estado_auxiliar_usc=="asociado"&tipo_de_comunicacion=="Comunicacion Asociado"~"Categoria A",
                                         vinculaciones_mes_actual>0&vinculaciones_mes_actual<2&estado_auxiliar_usc=="no asociado"&tipo_de_comunicacion=="Comunicacion Asociado"~"Categoria A",
                                         vinculaciones_mes_actual>1&vinculaciones_mes_actual<4&estado_auxiliar_usc=="asociado"&tipo_de_comunicacion=="Comunicacion Asociado"~"Categoria B",
                                         vinculaciones_mes_actual>1&vinculaciones_mes_actual<4&estado_auxiliar_usc=="no asociado"&tipo_de_comunicacion=="Comunicacion Asociado"~"Categoria B",
                                         vinculaciones_mes_actual>3&vinculaciones_mes_actual<7&estado_auxiliar_usc=="asociado"&tipo_de_comunicacion=="Comunicacion Asociado"~"Categoria C",
                                         vinculaciones_mes_actual>3&vinculaciones_mes_actual<7&estado_auxiliar_usc=="no asociado"&tipo_de_comunicacion=="Comunicacion Asociado"~"Categoria C",
                                         vinculaciones_mes_actual>7&estado_auxiliar_usc=="asociado"&tipo_de_comunicacion=="Comunicacion Asociado"~"Categoria D",
                                         vinculaciones_mes_actual>7&estado_auxiliar_usc=="no asociado"&tipo_de_comunicacion=="Comunicacion Asociado"~"Categoria D",
                                         vinculaciones_mes_actual>=1&estado_auxiliar_usc=="asociado"&tipo_de_comunicacion=="Comunicacion Cliente o Usuario"~"Categoria Cliente",
                                         vinculaciones_mes_actual>=1&estado_auxiliar_usc=="no asociado"&tipo_de_comunicacion=="Comunicacion Cliente o Usuario"~"Categoria Cliente",
                                         TRUE~"0"),
         premio_auxiliar_usc_x_und=case_when(vinculaciones_mes_actual>0&vinculaciones_mes_actual<2&estado_auxiliar_usc=="asociado"&tipo_de_comunicacion=="Comunicacion Asociado"~"5000",
                                       vinculaciones_mes_actual>0&vinculaciones_mes_actual<2&estado_auxiliar_usc=="no asociado"&tipo_de_comunicacion=="Comunicacion Asociado"~"0",
                                       vinculaciones_mes_actual>1&vinculaciones_mes_actual<4&estado_auxiliar_usc=="asociado"&tipo_de_comunicacion=="Comunicacion Asociado"~"10000",
                                       vinculaciones_mes_actual>1&vinculaciones_mes_actual<4&estado_auxiliar_usc=="no asociado"&tipo_de_comunicacion=="Comunicacion Asociado"~"7000",
                                       vinculaciones_mes_actual>3&vinculaciones_mes_actual<7&estado_auxiliar_usc=="asociado"&tipo_de_comunicacion=="Comunicacion Asociado"~"20000",
                                       vinculaciones_mes_actual>3&vinculaciones_mes_actual<7&estado_auxiliar_usc=="no asociado"&tipo_de_comunicacion=="Comunicacion Asociado"~"14000",
                                       vinculaciones_mes_actual>7&estado_auxiliar_usc=="asociado"&tipo_de_comunicacion=="Comunicacion Asociado"~"30000",
                                       vinculaciones_mes_actual>7&estado_auxiliar_usc=="no asociado"&tipo_de_comunicacion=="Comunicacion Asociado"~"21000",
                                       vinculaciones_mes_actual>=1&estado_auxiliar_usc=="asociado"&tipo_de_comunicacion=="Comunicacion Cliente o Usuario"~"90000",
                                       vinculaciones_mes_actual>=1&estado_auxiliar_usc=="no asociado"&tipo_de_comunicacion=="Comunicacion Cliente o Usuario"~"63000",
                                       TRUE~"0"),
         premio_auxiliar_usc_x_und=as.numeric(premio_auxiliar_usc_x_und),
         premio_auxiliar_usc=vinculaciones_mes_actual*premio_auxiliar_usc_x_und)

tabla_conteo_final_usc %<>% 
  select(cedula_auxiliar_usc,
         categoria_de_pago_usc,
         premio_auxiliar_usc)

archivo_consolidado_de_pagos %<>%
  left_join(tabla_conteo_final_usc,by="cedula_auxiliar_usc")

#------------------------------------------------------------------------------------------------------------------

#INFORMACION PARA REALIZAR LA LIQUIDACION FINANCIERA
#COLUMNAS DE FINANCIERO
archivo_consolidado_de_pagos %<>%
  mutate(canal_vinculacion_pago=cargo_promotor,#SE CREA DE NUEVO PARA VER COMO SE INGRESA LOS PREMIO POR LOGRO
         canal_formulado_pago=tolower(paste0(cargo_promotor," ","regional"," ",referido_regional," ",administracion_promotor)))

#CARGAMOS EL ARCHIVO DE CODIGOS
archivo_codigos_ciudad <- read_xlsx(path = "./raw_data/codigos.xlsx",
                             sheet = "codigos_ciudad") %>% 
  clean_names() %>% 
  select(-geografia,
         -regional)

#CARGAMOS EL ARCHIVO DE CODIGOS
archivo_codigos_centros_de_costos <- read_xlsx(path = "./raw_data/codigos.xlsx",
                                    sheet = "codigos_centro_de_costos") %>% 
  clean_names() %>% 
  select(canal_formulado_pago,
         codigo_centro_de_costos_financiero)


#COLUMNA CODIGO CIUDAD
#COLUMNA GASTO FINANCIERO
#COLUMNA PASIVO FINANCIERO
#COLUMNA DESCRPCION FINANCIERO
#COLUMNA ESTADO CREACION FINANCIERO
archivo_consolidado_de_pagos %<>%
  left_join(archivo_codigos_ciudad,by=c("referido_nombre_zona"="ciudad")) %>% 
  mutate(codigo_ciudad=as.character(codigo_ciudad),
         codigo_ciudad_financiero=case_when(administracion_promotor=="Admon. Call Nal"~"76999",
                                            administracion_promotor=="Admon. Call Nal 2.0"~"76999",
                                            referido_regional=="Cali"~"76001",
                                            TRUE~codigo_ciudad)) %>% 
  left_join(archivo_codigos_centros_de_costos,by=c("canal_formulado_pago")) %>% 
  mutate(cuenta_gasto_financiero="5295951000",
         cuenta_pasivo_financiero="2495951300",
         descripcion_financiero=paste0("referido"," ",cedula_referido," ",nombre_referido," ",codigo_centro_de_costos_financiero," ",codigo_ciudad_financiero),
         fecha_de_creacion_tercero=convert_to_date(fecha_de_creacion_tercero),
         dias_tercero={today()}-fecha_de_creacion_tercero,
         estado_creacion_tercero=if_else(dias_tercero<90,"ACTIVO","INACTIVO"))


  





#------------------------------------------------------------------------------------------------------------------

archivo_consolidado_de_pagos %>%
  write_xlsx(
    glue(
      "./output_data/",
      "Consolidado_De_Pagos_",
      "{today()}.xlsx"
    )
  )



#---------------------------------------------------------------------------------------------------------------
#EL FILTRO ES LA FECHA DE GENERACION (VALIDAR SI LO QUE NO TIENEN VALORES SE LES QUITA ESA FECHA O QUE SE HACE)


#TRANSFORMAMOS LAS FECHAS EN TIPO DATE PARA PODER REALIZAR LOS FORMATOS CON LA INFORMACION
#archivo_consolidado_de_pagos %<>% 
#  mutate(fecha_generacion_informe=as_date(fecha_generacion_informe))


#FORMATO ABONO ESTADO DE CUENTA
formato_abono_estado_de_cuenta <- archivo_consolidado_de_pagos %>% 
  filter(fecha_de_solicitud==hoy,#CAMBIAR FECHA POR {today()}
         fecha_corte_asociado==corte,
         corresponde_premio_al_asociado=="Si",
         metodo_de_pago_asociado=="Abono a mi estado de cuenta de Asociado",
         premio_asociado!=0) %>% 
  select(cedula_referido,
         nombre_referido,
         cedula_asociado,
         nombre_asociado,
         premio_asociado,
         fecha_de_solicitud) %>% 
  mutate(fecha_de_solicitud={today()},
         observacion="Liquid Normal")

#TABLA DINAMICA FORMATO ABONO ESTADO DE CUENTA
tabla_formato_abono_estado_de_cuenta <- formato_abono_estado_de_cuenta %>% 
  mutate(premio_asociado=as.integer(premio_asociado)) %>% 
  group_by(cedula_asociado,
           nombre_asociado,
           premio_asociado) %>% 
  summarise(premio_asociado=sum(premio_asociado))


#EXPORTAMOS EL FORMATO ABONO ESTADO DE CUENTA
tabla_formato_abono_estado_de_cuenta %>%
  write_xlsx(
    glue(
      "./output_data/",
      "Formato_Abono_Estado_De_Cuenta",
      "{today()}.xlsx"
    )
  ) 

# 
# sheets2 <- list("Base_Final" = base_final_usc, "Registrados_Mes" = Registrados_Mes_Usc,"Efectivos_Mes" = Efectivos_Mes_Usc) #assume sheet1 and sheet2 are data frames
# write_xlsx(sheets2, ruta_completaa)



#---------------------------------------------------------------------------------------------------------------
  
#FORMATO GIROS Y CONSIGNACIONES
formato_giros_y_consignaciones <- archivo_consolidado_de_pagos %>% 
  filter(fecha_de_solicitud==hoy,
         corresponde_premio_al_asociado=="Si",
         metodo_de_pago_asociado=="Giro en Oficina Bancoomeva"|metodo_de_pago_asociado=="Consignación a mi cuenta Bancoomeva"|metodo_de_pago_asociado=="Consignación a cuentas de otros Bancos",
         premio_asociado!=0) %>% 
  rename(total_valor=premio_asociado,
         medio_de_pago=metodo_de_pago_asociado) %>% 
  mutate(fecha_de_solicitud={today()},
         observacion="Liquid Normal",
         codigo_oficina="PENDIENTE") %>% #PENDIENTE AGREGAR ESTA COLUMNA(VALIDAR DE DONDE SALE)
  select(fecha_de_solicitud,
         cedula_asociado,
         nombre_asociado,
         total_valor,
         medio_de_pago,
         numero_de_cuenta_asociado,
         codigo_oficina) 
  
  

#TABLA DINAMICA FORMATO GIROS Y CONSIGNACIONES
tabla_formato_giros_y_consignaciones <- formato_giros_y_consignaciones %>% 
  mutate(total_valor=as.integer(total_valor)) %>% 
  group_by(fecha_de_solicitud,
           cedula_asociado,
           nombre_asociado,
           total_valor,
           medio_de_pago,
           numero_de_cuenta_asociado,
           codigo_oficina,
           observacion) %>% 
  summarise(total_valor=sum(total_valor))


#EXPORTAMOS EL FORMATO GIROS Y CONSIGNACIONES
tabla_formato_giros_y_consignaciones %>%
  write_xlsx(
    glue(
      "./output_data/",
      "Formato_Giros_y_Consignaciones",
      "{today()}.xlsx"
    )
  ) 

#---------------------------------------------------------------------------------------------------------------

#FORMATO PREMIO X LOGRO

#GENERAMOS EL FORMATO CON LA INFORMACION NECESARIA
formato_pxl_abono_estado_cuenta <- archivo_consolidado_de_pagos %>% 
  filter(premio_x_logro=="Si",
         fecha_corte_asociado==corte,
         metodo_de_pago_asociado=="Abono a mi estado de cuenta de Asociado") %>% 
  mutate(fecha_de_solicitud={today()},
         observacion="Liquid Normal") %>% 
  select(cedula_referido,
         nombre_referido,
         cedula_asociado,
         nombre_asociado,
         premio_asociado,
         fecha_de_solicitud,
         observacion) %>% 
  distinct(cedula_asociado, .keep_all = TRUE) 
  


#FILTRAMOS EL FORMATO CON LA TABLA DEL CONSOLIDADO PARA OMITIR LOS QUE YA SE LES HAYAN REALIZADO PAGOS
formato_pxl_abono_estado_cuenta %<>% filter(!cedula_asociado%in%archivo_premio_x_logro[["cedula_asociado"]])

base_pagos_pxl <- formato_pxl_abono_estado_cuenta %>% 
  select(cedula_asociado)

#PEGAMOS LAS NUEVAS CEDULAS QUE SE LE REALIZO EL PAGO EN EL ARCHIVO DE CONSOLIDADO_PREMIO_X_LOGRO.XLSX
archivo_premio_x_logro = bind_rows(archivo_premio_x_logro, base_pagos_pxl)



#EXPORTAMOS EL FORMATO GIROS Y CONSIGNACIONES
archivo_premio_x_logro %>%
  write_xlsx(
    glue(
      "./raw_data/",
      "consolidado_premio_x_logro.xlsx"
    )
  ) 



#---------------------------------------------------------------------------------------------------------------

#FORMATO PREMIO X LOGRO

#CARGAMOS BASE CON LAS CEDULAS DE ASOCIADOS QUE YA SE LES HA REALIZADO PREMIO X LOGRO PARA CRUZAR Y NO PAGAR
#FORMATOS LIQUIDACION PREMIO POR LOGRO
archivo_premio_x_logro <- read_xlsx(path = "./raw_data/consolidado_premio_x_logro.xlsx") %>% 
  clean_names() %>%  
  mutate(cedula_asociado=as.character(cedula_asociado)) %>% 
  distinct(cedula_asociado, .keep_all = TRUE)


#GENERAMOS EL FORMATO CON LA INFORMACION NECESARIA
formato_pxl_giros_y_consignaciones <- archivo_consolidado_de_pagos %>% 
  filter(premio_x_logro=="Si",
         metodo_de_pago_asociado=="Giro en Oficina Bancoomeva"|metodo_de_pago_asociado=="Consignación a mi cuenta Bancoomeva"|metodo_de_pago_asociado=="Consignación a cuentas de otros Bancos") %>% 
  rename(medio_de_pago=metodo_de_pago_asociado) %>% 
  mutate(fecha_de_solicitud={today()},
         observacion="Premio x Logro",
         codigo_oficina="PENDIENTE",#PENDIENTE AGREGAR ESTA COLUMNA(VALIDAR DE DONDE SALE)
         total_valor="400000") %>% 
  select(fecha_de_solicitud,
         cedula_asociado,
         nombre_asociado,
         total_valor,
         medio_de_pago,
         numero_de_cuenta_asociado,
         codigo_oficina) %>% 
  distinct(cedula_asociado, .keep_all = TRUE) 
  

#FILTRAMOS EL FORMATO CON LA TABLA DEL CONSOLIDADO PARA OMITIR LOS QUE YA SE LES HAYAN REALIZADO PAGOS
formato_pxl_giros_y_consignaciones %<>% filter(!cedula_asociado%in%archivo_premio_x_logro[["cedula_asociado"]])

base_pagos_pxl <- formato_pxl_giros_y_consignaciones %>% 
  select(cedula_asociado)

#PEGAMOS LAS NUEVAS CEDULAS QUE SE LE REALIZO EL PAGO EN EL ARCHIVO DE CONSOLIDADO_PREMIO_X_LOGRO.XLSX
archivo_premio_x_logro = bind_rows(archivo_premio_x_logro, base_pagos_pxl)



#EXPORTAMOS EL FORMATO GIROS Y CONSIGNACIONES
archivo_premio_x_logro %>%
  write_xlsx(
    glue(
      "./raw_data/",
      "consolidado_premio_x_logro.xlsx"
    )
  ) 



#---------------------------------------------------------------------------------------------------------------






