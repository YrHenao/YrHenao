#library("reticulate")

#ruta de la version del python que queremos usar
#use_python("C:/Users/yrhu0672/Anaconda3",required = TRUE)

#generar la consola modo python
#repl_python()


#IMPORTAMOS LOS PAQUETES QUE VAMOS A NECESITAR DE PYTHON

from simple_salesforce import Salesforce
import requests
import pandas as pd
from io import StringIO
import pyodbc

sf = Salesforce(username='yrhu0672@coomeva.com.co', #USUARIO
                password='Coomevagcca09$', #CONTRASEÑA
                security_token='iIXLtpRiMPZUp1ShBNFy80Qtt')#TOKEN QUE TE DA SALESFORCE

#IMPORTAMOS EL REPORTE DE CRM - CRM GESTION
sf_instance = 'https://coomeva.lightning.force.com/' #INSTANCIA URL
reportId = '00O5x000005ISw0EAG' # ID DEL INFORME - SE CAMBIA SEGUN EL INFORME
export = '?isdtp=p1&export=1&enc=UTF-8' #QUEDA IGUAL
sfUrl = sf_instance + reportId + export
response = requests.get(sfUrl, headers=sf.headers, cookies={'sid': sf.session_id})
download_report = response.content.decode('utf-8')
crm_gestion = pd.read_csv(StringIO(download_report))

#IMPORTAMOS EL REPORTE DE CRM - CRM EJECUTIVO
sf_instancee = 'https://coomeva.lightning.force.com/' #INSTANCIA URL
reportIdd = '00O5x000005ISSjEAO' # ID DEL INFORME - SE CAMBIA SEGUN EL INFORME
exportt = '?isdtp=p1&export=1&enc=UTF-8' #QUEDA IGUAL
sfUrll = sf_instancee + reportIdd + exportt
responsee = requests.get(sfUrll, headers=sf.headers, cookies={'sid': sf.session_id})
download_reportt = responsee.content.decode('utf-8')
crm_ejecutivos = pd.read_csv(StringIO(download_reportt))
