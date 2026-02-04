v <- numeric_version("1.2.2")
library(shiny)
library(shinythemes)
library(shinyhelper)
library(shinycssloaders)
library(tidyverse)
library(jsonlite)
library(data.table)
library(DT)
library(DBI)
library(RSQLite) # swap with RPostgres/MySQL if needed
library(gghighlight)
library(treemapify)



OS <- Sys.info()[['sysname']]
if (OS  == "Linux") {
   #datapath <- "/project-vol"
   datapath <- "/srv/shiny-server/data"
   # datapath <- "/home/data" # (project-vol (FinnPRIO-Explorer Adapted))
 } else {
  datapath <- "data/"
 }

# Read data tables:
# hv <- fread("data/hv.csv")                                     # "hv.csv" contains FinnPRIO hypervolume scores./It is used in tab 4. Rank pests
db_file <- list.files(datapath, pattern = "FinnPrio_DB_", recursive = TRUE, full.names = TRUE)
#get the latest file if multiple
if(length(db_file) > 1){
  db_file <- db_file[which.max(file.info(db_file)$mtime)]
}

consql <- dbConnect(RSQLite::SQLite(), db_file[])
threats <- dbReadTable(consql, "threatenedSectors")
pests <- dbReadTable(consql, "pests")
taxa <- dbReadTable(consql, "taxonomicGroups")
quaran <- dbReadTable(consql, "quarantineStatus")
pathways <- dbReadTable(consql, "pathways")
questions_main <- dbReadTable(consql, "questions")
questions_entry <- dbReadTable(consql, "pathwayQuestions")

assessments <- dbReadTable(consql, "assessments")
entryPathways <- dbReadTable(consql, "entryPathways")
answers_main <- dbReadTable(consql, "answers")
answers_entry <- dbReadTable(consql, "pathwayAnswers")

# simulations <- dbReadTable(consql, "simulations")
simulations <- dbGetQuery(consql, "SELECT
       s.*
      FROM simulations s
      JOIN assessments a ON s.idAssessment = a.idAssessment
      WHERE a.valid = 1
      AND s.date = (
        SELECT MAX(date)
        FROM simulations
        WHERE idAssessment = s.idAssessment )")
                          
simulationSummaries <- dbReadTable(consql, "simulationSummaries")


