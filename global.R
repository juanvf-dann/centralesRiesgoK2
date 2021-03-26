library(DBI)
library(RSQLite)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tidyr)
library(shinyalert)


# source("Modules/CentralesRiesgoUI.R",encoding = "UTF-8")
# source("Modules/centralesRiesgoServer.R",encoding = "UTF-8")


db.pricing <- "../DBs/pricing.db"
db.centrales.riesgo <- "../DBs/centralesRiesgo.db"
con.pricing <<- dbConnect(RSQLite::SQLite(), db.pricing) # db.pricing en global.R
con.centralesRiesgo <<- dbConnect(RSQLite::SQLite(), db.centrales.riesgo) 

tabla.entidades <<- dbReadTable(con.pricing,"TABLA_ENTIDADES")
escala.calificacion_homologada <- c("A","B", "C","D","E","K")



# dbRemoveTable(con.centralesRiesgo,"TABLA_REPORTES_CENTRALES_RIESGO",tabla.variables.2)
# dbCreateTable(con.centralesRiesgo,"TABLA_REPORTES_CENTRALES_RIESGO",tabla.variables.2)
# dbReadTable(con.centralesRiesgo,"TABLA_REPORTES_CENTRALES_RIESGO")

