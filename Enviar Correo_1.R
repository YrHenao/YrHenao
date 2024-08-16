
library(blastula)
library(keyring)

#informacion que vamos a tener en el email y que se encuentra en el rmardown

my_email_objet <- render_email('Informe_Fuerza_de_Ventas.Rmd')
class(my_email_objet)

print(my_email_objet)

#guardamos las credenciales para enviar el correo

create_smtp_creds_key(
  id="outlook",
  user="yrhu0672@coomeva.com.co",
  provider = "office365",
  host="smtp.office365.com",
  port=587,
  use_ssl=TRUE,
  overwrite = TRUE
)

# codigo final para enviar el correo por medio del outlook

smtp_send(my_email_objet,
          from = "yrhu0672@coomeva.com.co",
          to="yrhu0672@coomeva.com.co",
          subject="Informe Gestion Referidos - Fuerza de Ventas",
          credentials = creds_key("outlook")
)




## Validacion para el puerto de la empresa 8080

#guardamos las credenciales para enviar el correo

create_smtp_creds_key(
  id="outlook_empresa",
  user="yrhu0672@coomeva.com.co",
  host="smtp.office365.com",
  port=8080,
  use_ssl=TRUE,
  overwrite = TRUE
)