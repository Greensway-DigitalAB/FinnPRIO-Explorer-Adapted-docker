# Format table header for the table under the plot (https://rstudio.github.io/DT/):
tbl_hdr <- htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, ''),
      th(class = 'dt-left', rowspan = 2, 'Pest'),
     
      th(class = 'dt-center', rowspan = 2, 'Taxonomic group'),
      th(class = 'dt-center', rowspan = 2, 'Presence in Europe'),
      th(class = 'dt-center', colspan = 3, 'Entry scores'),
      th(class = 'dt-center', colspan = 3, 'Establishment and spread scores'),
      th(class = 'dt-center', colspan = 3, 'Invasion scores'),
      th(class = 'dt-center', colspan = 3, 'Impact scores')
    ),
    tr(
      lapply(rep(c('5th percentile', 'median', '95th percentile'), 4), th)
    )
  )
))

# Format table header for the table with all pests
tbl_hdr_all <- htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, ''),
      th(class = 'dt-center', rowspan = 2, 'Pest'),
     # th(class = 'dt-center', rowspan = 2, 'EPPO code'),
      th(class = 'dt-center', rowspan = 2, 'EPPO Code'),
      th(class = 'dt-center', rowspan = 2, 'Taxonomic group'),
      th(class = 'dt-center', rowspan = 2, 'Quarantine status'),
      th(class = 'dt-center', rowspan = 2, 'Presence in Europe'),
      th(class = 'dt-center', colspan = 8, 'Median scores for'),
      th(class = 'dt-left', rowspan = 2, 'Assessed, month/year')
    ),
    tr(
      lapply(rep(c('Entry', 'Establishment and spread', 'Invasion', 'Impact', 
                   'Risk', 'Preventability', 'Controllability', 'Manageability'), 1), th)
      
    )
  )
))

# Format table header for the table with all pests
tbl_hdr_all_2 <- htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, ''),
      th(class = 'dt-center', rowspan = 2, 'Pest'),
      # th(class = 'dt-center', rowspan = 2, 'EPPO code'),
      th(class = 'dt-center', rowspan = 2, 'EPPO Code'),
      th(class = 'dt-center', rowspan = 2, 'Taxonomic group'),
      th(class = 'dt-center', rowspan = 2, 'Quarantine status'),
      th(class = 'dt-center', rowspan = 2, 'Presence in Europe'),
      th(class = 'dt-center', colspan = 8, 'Mean scores for'),
      th(class = 'dt-left', rowspan = 2, 'Assessed, month/year')
    ),
    tr(
      lapply(rep(c('Entry', 'Establishment and spread', 'Invasion', 'Impact', 
                   'Risk', 'Preventability', 'Controllability', 'Manageability'), 1), th)
      
    )
  )
))

# Format table header for the table under treemap
tbl_hdr_map <- htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, ''),
      th(class = 'dt-center', rowspan = 2, 'Pest'),
      th(class = 'dt-center', rowspan = 2, 'Quarantine status'),
      th(class = 'dt-center', colspan = 2, 'Entry score'),
      th(class = 'dt-center', colspan = 2, 'Establishment and spread score'),
      th(class = 'dt-center', colspan = 2, 'Invasion score'),
      th(class = 'dt-center', colspan = 2, 'Impact score')
    ),
    tr(
      lapply(rep(c('rank', 'hypervolume'), 4), th)
    )
  )
))

# HELP TEXTS FOR THE POP UP WINDOWS ####

help_plot_grid <- function(){
  tags$i() |> 
    helper(type="inline",
           size = "m",
           title = "Number of plots",
           content = c(
             "'Single plot' presents all pests that have the selected characteristics on one plot.",
             "",
             "'Multiple plots' splits all pests that have the selected characteristics
                       to separate plots based on their quarantine status."),
           buttonLabel = "OK", 
           easyClose = TRUE,
           icon = "circle-question",
           colour = "#912D20",
           fade = FALSE)
}

help_add <- function(){
  tags$i() |> 
    helper(type="inline",
           size = "m",
           title = "Uncertainty",
           content = c("The whiskers for y- and x-axis show the 25th and the 75th percentiles of the score distributions."),
           buttonLabel = "OK", 
           easyClose = TRUE,
           icon = "circle-question",
           colour = "#912D20",
           fade = FALSE)
}


help_threshold <- function(){
  tags$i() |> 
    helper(type="inline",
           size = "m",
           title = "Threshold line",
           content = c("Add a number between 0 and 1 to display threshold lines on the plot (for y- and x-axis)."),
           buttonLabel = "OK", 
           easyClose = TRUE,
           icon = "circle-question",
           colour = "#912D20",
           fade = FALSE)
}



help_pw <- function(){
  tags$i() |> 
    helper(type="inline",
           size = "m",
           title = "ENT2A & ENT2B",
           content = c(
             "Can the pest:" ,
             "",
             "i) be transported in international trade with the host plant commodity considered in the pathway (pathways A-E)?",
             "",
             "ii) be transported from one country to another with other than host plant commodity, transport or passengers (pathway F)?",
             "",
             "iii) spread naturally to the PRA area from its current ranges during the next ten years (pathway G)?",
             "",
             "iv) be intentionally introduced to the PRA area (pathway H)?"
           ),
           buttonLabel = "OK", 
           easyClose = TRUE,
           icon = "circle-question",
           colour = "#912D20",
           fade = FALSE)
}


help_pw_a_h <- function(){
  tags$i() |> 
    helper(type="inline",
           size = "m",
           title = "Pathways",
           content = c("",
                       "The pathways are assess in order of their likelihood, i.e.",
                       "Pathway 1 is the most likely pathway, Pathway 2 is the next likely pathway, etc.",
                       "",
                       "Pathways:",
                       "A. Seeds" ,
                       "B. Plants for planting",
                       "C. Wood and wood products",
                       "D. Food and fodder",
                       "E. Other living plant parts",
                       "F. Hitchhiking",
                       "G. Natural spread",
                       "H. Intentional introduction",
                       
                       ""),
           buttonLabel = "OK", 
           easyClose = TRUE,
           icon = "circle-question",
           colour = "#912D20",
           fade = FALSE)
}







