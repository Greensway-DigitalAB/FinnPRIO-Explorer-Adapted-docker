v <- numeric_version("1.0.0")
library(shiny)
library(shinythemes)
library(shinyhelper)
library(shinycssloaders)
library(tidyverse)
library(jsonlite)
# library(dplyr)
library(data.table)
library(DT)
library(DBI)
library(RSQLite) # swap with RPostgres/MySQL if needed
# library(ggplot2)
library(gghighlight)
library(treemapify)


# source("functions.R")

# The colors used in the Plot and in the Tables are from the palette of Ruokavirasto (2022).

# Read data tables:
# cleanfinnprioresults <- fread("data/cleanfinnprioresults.csv") # "cleanfinnprioresults.csv" contains FinnPRIO scores./It is used in tabs 1. Plot pests on a graph and 2. Show pests in data table/ The pest's names are used in tab 3. Compare pests by questions
# pestquestions <- fread("data/pestquestions_est3.csv")          # "pestquestions_est3.csv" contains FinnPRIO assessments. Each pest is in a column!/ It is used in tab 3. Compare pests by questions
hv <- fread("data/hv.csv")                                     # "hv.csv" contains FinnPRIO hypervolume scores./It is used in tab 4. Rank pests

# Connect to the FinnPRIO database and read tables
# 'project-vol/my-folder/my-file.txt'
db_file <- list.files("data/", pattern = "FinnPrio_DB_", recursive = TRUE, full.names = TRUE)
#get the latest file if multiple
if(length(db_file) > 1){
  db_file <- db_file[which.max(file.info(db_file)$mtime)]
}

consql <- dbConnect(RSQLite::SQLite(), db_file[])
# assessors <- dbReadTable(consql, "assessors")
# assessors$fullName <- paste(assessors$data$firstName, assessors$data$lastName)
threats <- dbReadTable(consql, "threatenedSectors")
pests <- dbReadTable(consql, "pests")
taxa <- dbReadTable(consql, "taxonomicGroups")
quaran <- dbReadTable(consql, "quarantineStatus")
pathways <- dbReadTable(consql, "pathways")
questions_main <- dbReadTable(consql, "questions")
questions_entry <- dbReadTable(consql, "pathwayQuestions")

assessments <- dbReadTable(consql, "assessments")
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


