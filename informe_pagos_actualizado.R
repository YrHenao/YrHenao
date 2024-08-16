
# Establecemos la conexion con la base de datos

 con <- DBI::dbConnect(odbc::odbc(), "Multiactiva - MULCLIDAT",
                      UID = "yrhu0672", PWD = "Abby2022", Database = "MULCLIDAT",
                      trusted_connection = TRUE)

# darle nombre a la conexion y a las columnas

# CONEXION MULCLIDAT.CLIMAE
conexion1  <- dbGetQuery ( con , " SELECT* FROM D103285P.MULCLIDAT.CLIMAE") %>%
  select(NITCLI,FECING,ASOCIA,TIPCLI,TIPDOC,AGCVIN,ESTASO,NUMINT,INDEST) %>%
  rename(fecha_ingreso=FECING,
         asociado=ASOCIA,
         tipo_de_client=TIPCLI,
         tipo_de_documento=TIPDOC,
         cedula_referido=NITCLI,
         codigo_agencia=AGCVIN,
         codigo_estado=ESTASO,
         codigo_unico=NUMINT,
         codigo_estado_nivel_academico=INDEST
  ) %>% 
  mutate(fecha_ingreso=ymd(fecha_ingreso)) %>% 
  mutate(cedula_referido=as.numeric(cedula_referido)) %>% 
  mutate(tipo_de_cliente=case_when(tipo_de_client==2~"Mixto(Asociado/Cliente)",
                                   tipo_de_client==1~"Solo Asociado",
                                   TRUE~"Pendiente Tipo Cliente"))
 

  
  # CONEXION MULINDFIN.PLTAGCORI
  conexion2  <- dbGetQuery ( con , " SELECT* FROM D103285P.MULINDFIN.PLTAGCORI") %>%
  select(CODEMP,AGCORI,NOMAGC,CODCIU,CODSUC) %>%
  rename(codigo_agencia=AGCORI,
         nombre_agencia=NOMAGC,
         codigo_ciudad=CODCIU,
         codigo_regional=CODSUC,
  ) %>% 
  mutate(regional=case_when(codigo_regional==1~"Cali",
                            codigo_regional==2~"Eje Cafetero",
                            codigo_regional==3~"Medellin",
                            codigo_regional==4~"Palmira",
                            codigo_regional==5~"Bogota",
                            codigo_regional==6~"Caribe",
                            TRUE~"Sin Regional"))

# CONEXION MULINDFIN.CLITAB
conexion3  <- dbGetQuery ( con , " SELECT* FROM D103285P.MULINDFIN.CLITAB") %>%
  select(CODTAB,CODINT,CODNOM) %>%
  filter(CODTAB==110) %>%
  rename(codigo_estado=CODINT,
         nombre_estado=CODNOM
  )


# CONEXION MULINDFIN.CLITAB
conexion4  <- dbGetQuery ( con , " SELECT* FROM D103285P.MULAPODAT.APOMAECTA") %>%
  select(NITCTA,NOMCTA,CUOFAC,CUOCAN,CODPRO,FAPERT,FCANCE) %>%
  filter(CODPRO==41) %>%
  rename(codigo_unico=NITCTA,
         nombre_referido=NOMCTA,
         cuota_facturada=CUOFAC,
         cuota_pagada=CUOCAN,
         fecha_apertura=FAPERT,
         fecha_cancelacion=FCANCE)
#VALIDACION <- conexion4 %>% filter(codigo_unico==9022112581127000)

conexion4 <- conexion4[with(conexion4, order(-conexion4$fecha_apertura)), ]
conexion4 <- conexion4[with(conexion4, order(conexion4$fecha_cancelacion)), ]

# CAMPO NIVEL ACADEMICO
conexion5 <- dbGetQuery ( con , " SELECT* FROM D103285P.MULINDFIN.CLITAB") %>%
  select(CODTAB,CODINT,CODNOM) %>%
  filter(CODTAB==177) %>%
  rename(codigo_estado_nivel_academico=CODINT,
         nivel_academico=CODNOM
  )

# CAMPO TIPO DE CLIENTE
conexion6 <- dbGetQuery ( con , " SELECT* FROM D103285P.MULINDFIN.CLITAB") %>%
  select(CODTAB,CODINT,CODNOM) %>%
  filter(CODTAB==480) %>%
  rename(tipo_de_client=CODINT,
         tipo_de_cliente=CODNOM
  )


# Importar archivo tink ------------------------------------------------------

files_referidos <- list.files(
  "./raw_data/",
  "csv_referreds-campaign-givnbnb",
  full.names = TRUE
)

archivo_tink <- read_csv(
  files_referidos[[1]],
  locale = locale(encoding = "ISO-8859-1"),
  col_types = cols(.default = col_character())
) %>%
  clean_names() %>% 
  rename(cedula_referido=referido_id_personal) %>% 
  mutate(origen="plataforma tink") %>% 
  mutate(cedula_referido=as.numeric(cedula_referido)) %>% 
  mutate(referido_fecha_registro=as.Date(referido_fecha_registro,format="%d/%m/%Y"))



# Importar archivo crm ------------------------------------------------------

files_crm <- list.files(
  "./raw_data/",
  "total_crm",
  full.names = TRUE
)

archivo_crm <- read_csv2(
  files_crm[[1]],
  locale = locale(encoding = "ISO-8859-1"),
  col_types = cols(.default = col_character())
) %>%
  clean_names()%>% 
  rename(cedula_referido=nombre_de_la_cuenta_numero_de_identificacion)%>% 
  mutate(origen="plataforma crm")%>% 
  mutate(cedula_referido=as.numeric(cedula_referido))%>% 
  mutate(fecha_de_creacion =as.Date(fecha_de_creacion ,format="%d/%m/%Y"))



# cedulas tink --------------------------------------

cedulas_tink <- archivo_tink %>%
  select(cedula_referido)

# cedulas crm ----------------------------------------------

cedulas_crm <- archivo_crm %>%
  select(cedula_referido)


# Cedulas referidos completas - union tink y crm

cedulas_referidos <- bind_rows(cedulas_tink,cedulas_crm) %>% 
  mutate(cedula_referido=as.numeric(cedula_referido)) %>% 
  distinct(cedula_referido, .keep_all = TRUE)

# archivo medios de  pago -----------------------------------------

medios_de_pago <- read_excel(
  "./raw_data/medios_de_pago.xlsx",
  col_types = "text"
) %>%
  clean_names()

medios_de_pago %<>%
  mutate(cuenta_codigo_estado_de_cuenta=case_when(tipo_de_pago_escogido=="abono al estado de cuenta"~validacion_estado_cuenta_asociado,
                                                  tipo_de_pago_escogido=="consignacion bancaria"~numero_cuenta_asociado,
                                                  tipo_de_pago_escogido=="giro en oficina"~codigo_oficina,),
         banco_oficina_corte=case_when(tipo_de_pago_escogido=="abono al estado de cuenta"~fecha_de_corte,
                                       tipo_de_pago_escogido=="consignacion bancaria"~banco_asociado,
                                       tipo_de_pago_escogido=="giro en oficina"~nombre_oficina)) %>% 
  select(cedula_asociado,
         tipo_de_pago_escogido,
         cuenta_codigo_estado_de_cuenta,
         banco_oficina_corte) %>% 
  rename(cedula_referente=cedula_asociado)
#  mutate(cedula_referente=as.numeric(cedula_referente))


# base referidos --------------------------------------------------

base_referidos <- left_join(cedulas_referidos,archivo_tink, by="cedula_referido") %>% 
  select(cedula_referido,
         referido_nombre,
         referido_telefono,
         referido_correo,
         referido_fecha_registro,
         tinker_id_personal,
         tinker_nombre,
         tinker_telefono,
         tinker_correo,
         origen
         ) %>% 
  left_join(archivo_crm, by= c("cedula_referido" = "cedula_referido"))%>% 
  left_join(conexion1,by= c("cedula_referido" = "cedula_referido")) %>% 
  left_join(conexion2,by= c("codigo_agencia" = "codigo_agencia")) %>% 
  left_join(conexion3,by= c("codigo_estado" = "codigo_estado")) %>% 
  left_join(conexion4,by=c("codigo_unico"="codigo_unico")) %>% 
  left_join(conexion5,by=c("codigo_estado_nivel_academico"="codigo_estado_nivel_academico")) %>%
  distinct(cedula_referido, .keep_all = TRUE) %>% 
  mutate(nombre_referido=if_else(is.na(referido_nombre),nombre_de_la_cuenta_nombre_de_la_cuenta,referido_nombre),
         celular_referido=if_else(is.na(referido_telefono),nombre_de_la_cuenta_celular,referido_telefono),
         correo_referido=if_else(is.na(referido_correo),nombre_de_la_cuenta_email_1,referido_correo),
         fecha_registro_referido=if_else(is.na(referido_fecha_registro),fecha_de_creacion,referido_fecha_registro),
         cedula_referente=if_else(is.na(tinker_id_personal),propietario_de_oportunidad_identificacion,tinker_id_personal),
         nombre_referente=if_else(is.na(tinker_nombre),propietario_de_oportunidad_nombre_completo,tinker_nombre),
         origen_referido=if_else(is.na(origen.x),origen.y,origen.x),
         dias=fecha_ingreso-fecha_registro_referido,
         mes_vinculacion=months.Date(fecha_ingreso),
         año_vinculacion=as.numeric(format(fecha_ingreso,'%Y')),
         asociado=case_when(asociado==1~"asociado",
                            asociado==2~"no asociado",
                            TRUE~"sin informacion"),
         referido_efectivo=if_else(fecha_ingreso>=fecha_registro_referido,
                                                             "Si","No"),
         formula_referidos_vinculados=case_when(referido_efectivo=="Si"&fecha_ingreso==""~0,
                                             referido_efectivo=="No"~0,
                                             TRUE~1),
         formula_referidos_vinculados=as.numeric(formula_referidos_vinculados)) %>% 
  left_join(medios_de_pago, by= c("cedula_referente" = "cedula_referente")) %>% 
  rename(celular_referente=tinker_telefono,
         correo_referente=tinker_correo,
         campaña_referido_crm=campana) %>% 
  select(origen_referido,
         cedula_referido,
         nombre_referido,
         celular_referido,
         correo_referido,
         fecha_registro_referido,
         cedula_referente,
         nombre_referente,
         celular_referente,
         correo_referente,
         campaña_referido_crm,
         asociado,
         tipo_de_cliente,
         nivel_academico,
         fecha_ingreso,
         regional,
         nombre_agencia,
         nombre_estado,
         referido_efectivo,
         cedula_referido,
         nombre_referido,
         cuota_facturada,
         cuota_pagada,
         codigo_unico,
         dias,
         mes_vinculacion,
         año_vinculacion,
         formula_referidos_vinculados,
         tipo_de_pago_escogido,
         cuenta_codigo_estado_de_cuenta,
         banco_oficina_corte)


suma_vinculados_referente <- base_referidos %>%
  group_by(cedula_referente) %>% 
  summarise(referidos_vinculados=sum(formula_referidos_vinculados))

base_referidos <- left_join(base_referidos,suma_vinculados_referente, by= c("cedula_referente" = "cedula_referente")) %>% 
  mutate(corresponde_premio_al_asociado=if_else(referido_efectivo == "Si"&cuota_pagada >= 1,"corresponde premio al asociado","No"),
         categoria_nombramiento=case_when(referidos_vinculados>0&referidos_vinculados<3~"Pionero",
                                  referidos_vinculados>=3&referidos_vinculados<5~"Lider",
                                  referidos_vinculados>=5~"Lider", 
                                  TRUE~"Sin Nombramiento"),
         categoria_pago=case_when(referidos_vinculados>0&referidos_vinculados<3&categoria_nombramiento=="Lider"~"Lider",
                                   referidos_vinculados==4&categoria_nombramiento=="Lider"~"Lider",
                                   referidos_vinculados==0&categoria_nombramiento=="Lider"~"Pionero",
                                   referidos_vinculados>2&referidos_vinculados<5&categoria_nombramiento=="Lider"~"Lider",
                                   referidos_vinculados>4&categoria_nombramiento=="Lider"~"Mentor",
                                   referidos_vinculados>2&categoria_nombramiento=="Mentor"~"Mentor",
                                   referidos_vinculados<3&categoria_nombramiento=="Pionero"~"Pionero",
                                   referidos_vinculados>2&referidos_vinculados<5&categoria_nombramiento=="Pionero"~"Lider",
                                   referidos_vinculados>4&categoria_nombramiento=="Pionero"~"Mentor",
                                   referidos_vinculados==5&categoria_nombramiento=="Mentor"~"Mentor",
                                   referidos_vinculados==0&categoria_nombramiento=="Mentor"~"Pionero",
                                   referidos_vinculados==0&categoria_nombramiento=="Pionero"~"",
                                   TRUE~""),
         corresponde_premio_al_asociado=if_else(referido_efectivo == "Si"&cuota_pagada >= 1,"corresponde premio al asociado","No"),
         union=paste0(cedula_referido,cedula_referente))
  
  


#rm(cedulas_crm,
#   cedulas_tink,
#   archivo_crm,
#   archivo_tink)

#rm(archivo_crm,
#   archivo_tink,
#   cedulas_crm,
#   cedulas_tink,
#   cedulas_referidos,
#   conexion1,
#   conexion2,
#   conexion3,
#   conexion4,
#   suma_vinculados_referente)


gc()

# Importamos el archivo del consolidado de pagos----------------------------------

consolidado_de_pagos <- read_excel(
  "./raw_data/consolidado_de_pagos.xlsx",
  col_types = "text"
) %>%
  clean_names()


base_pagos_cruce <- base_referidos %>% 
  filter(corresponde_premio_al_asociado=="corresponde premio al asociado",
         fecha_registro_referido>"2021-12-31",
         nombre_estado=="Activo Normal                                                         "
        |nombre_estado=="Activo Cobranza Cobranza Interna                                      ")%>%
  mutate(estado_pago="Se pagó",
         medio_de_pago=tipo_de_pago_escogido,
         premio_del_asociado=case_when(categoria_pago=="Pionero"~100000,
                                       categoria_pago=="Lider"~100000,
                                       categoria_pago=="Mentor"~100000,
                                       TRUE~0),
         fecha_de_pago={today()},
         canal_de_pago=case_when(medio_de_pago=="giro en oficina"~"Operaciones",
                                 medio_de_pago=="abono al estado de cuenta"~"Administrativo",
                                 medio_de_pago=="consignacion bancaria"~"Operaciones",
                                 TRUE~"sin canal de pago"),
         mes_de_pago=months.Date(fecha_de_pago),
         exepciones="")

#filtrar una base por los datos de otra base
base_pagos <- base_pagos_cruce %>% filter(!union%in%consolidado_de_pagos[["union"]]) 

base_de_pagos_consignacion_o_giro <- base_pagos %>% 
    filter(medio_de_pago=="giro en oficina"|medio_de_pago=="consignacion bancaria")

# Exportamos la base final -------------------------------------------------------------------------------------

base_referidos %>%
  write_csv2(
    glue(
      "./output_data/",
      "Base_Pagos_Final",
      "_{today()}.xlsx"
    )
  )

base_pagos_cruce %>%
  write_csv2(
    glue(
      "./output_data/",
      "Base_Pagos_cruce.csv",
    )
  )

cedulas_referidos %>%
  write_csv2(
    glue(
      "./output_data/",
      "cedulas_referidos"
    )
  )
