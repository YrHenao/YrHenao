
# run all

library("reticulate")

#ruta de la version del python que queremos usar
use_python("C:/Users/yrhu0672/Anaconda3",required = TRUE)

#generar la consola modo python
repl_python()


#1.EJECUTAR EL SCRIPT DE PYTHON
reticulate::source_python("./R/conexion_crm.py")

#2.CONVERTIR LAS BASES DE CRM DE PYTHON EN DATAFRAME DE R
crm_gestion <- reticulate::py$crm_gestion
crm_ejecutivos <- reticulate::py$crm_ejecutivo

#3.EJECUTAR SCRIPT DE RUN DONDE ESTAN LAS LIBRERIAS
source("./R/Run.R")

#4.EJECUTAR SCRIPT DE R COMPLETO
source("./R/informe_red2.R")

#alt+ctrl + b