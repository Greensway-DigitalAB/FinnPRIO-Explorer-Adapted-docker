# SERVER ####
function(input, output, session) {
  observe_helpers()
  
  # Filter data:----
  ## Subset data -> filter data based on the input selections:-----
  selected_status <- reactive({
    req(cleanfinnprioresults)
    req(input$'group_Greenhouse crops')
    #req(input$impact_score)
    x1 <- input$'group_Greenhouse crops'
    x2 <- input$'group_Open-field crops'
    x3 <- input$'group_Trees and shrubs'
    x4 <- input$'group_Others'
    thr_sec <- c(x1, x2, x3, x4)
# print( thr_sec )
    if (is.null(input$quarantine_status) | is.null(input$taxonomic_group) | 
        is.null(input$presence_in_europe) | is.null(thr_sec) == TRUE) {
      validate(
        need(input$quarantine_status, "Select a quarantine status"),
        need(input$taxonomic_group, "Select a taxonomic group(s)"),
        need(input$presence_in_europe, "Select presence in Europe"),
        need(thr_sec, "Select a threatened sector(s)")
      )
      
    }else  {
      
      cleanfinnprioresults |> 
        filter(
          quarantine_status  %in% input$quarantine_status,
          taxonomic_group %in% input$taxonomic_group,
          presence_in_europe %in% input$presence_in_europe,
          entry_median > input$entry_score[1] & entry_median < input$entry_score[2],
          establishment_and_spread_median > input$establishment_score[1] & establishment_and_spread_median < input$establishment_score[2],
          invasion_median > input$invasion_score[1] & invasion_median < input$invasion_score[2],
          impact_median > input$impact_score[1] & impact_median < input$impact_score[2]
        )
    }
  })
  
  output$threat_checkboxes <- renderUI({
    # Assuming there's a column called 'group' to group threats
    threat_groups <- split(threats, threats$threatGroup)
    
    # Generate UI for each group
    group_ui <- lapply(names(threat_groups), function(group_name) {
      group_threats <- threat_groups[[group_name]]
      
      checkboxGroupInput(
        inputId = paste0("group_", group_name),
        label = group_name,
        choices = setNames(group_threats$idThrSect, group_threats$name),
        selected = group_threats$idThrSect,
        inline = FALSE
      )
    })
    
    half <- ceiling(length(group_ui) / 2)
    
    ui <- tagList(
      h4(strong("Threatened Sectors"), style = "color:#7C6A56"),
      group_ui
      # fluidRow(
      #   column(6, group_ui[1:half]),
      #   column(6, group_ui[(half + 1):length(group_ui)])
      # )
    )
    return (ui)
  })
  
  ## Selection of threatened sector: ---- 
  ## https://dplyr.tidyverse.org/reference/filter_all.html
  # selected_status1 <- reactive({ 
  #   # selected_status()
  #   # filter_at(
  #   #   selected_status(), vars(input$threatened_sek, input$threatened_sek2,input$threatened_sek3, input$threatened_sek4), 
  #   #   any_vars(. == "Yes")) ### https://stackoverflow.com/questions/53197150/filter-multiple-columns-by-value-using-checkbox-group
  #   
  # })
  
  
  ## Subset data -> filter data for the table generated under the plot:----
  infotable <- reactive({
    req(input$plot_brush)
    select(selected_status(), #selected_status1(), 
           pest, 
           taxonomic_group, 
           presence_in_europe,
           entry_25perc,
           # paste0("entry_", input$center), #median,
           entry_median,
           entry_75perc,
           establishment_and_spread_25perc,
           establishment_and_spread_median,
           establishment_and_spread_75perc,
           invasion_25perc,
           invasion_median, 
           invasion_75perc,
           impact_25perc,
           impact_median,
           impact_75perc
    )
  })
  
  
  # Display data on the tab-panels:----
  ## Data used for generating the table from brush in "1. Select pests to plot"-tab: ----
  selected_pests <- reactive({
    infotable()  |> 
      brushedPoints(input$plot_brush, input$xaxis, input$yaxis)
  })
  
  ## Generate table from the plot using brush in "1. Select pests to plot"-tab:----
  output$tableBrush <- DT::renderDataTable({
    
    req(input$xaxis)
    req(input$yaxis)
    
    # Returns rows from a data frame which are selected with the brush used in plotOutput:
    DT::datatable(selected_pests(),
                  colnames = c("Sort" = "pest", 
                               "Pest" = "pest", 
                               "Taxonomic group" = "taxonomic_group", 
                               "Presence in Europe" = "presence_in_europe",
                               "Entry, min" = "entry_25perc",
                               "Entry, median" = "entry_median",
                               "Entry, max" = "entry_75perc",
                               "Establishment and spread, min" = "establishment_and_spread_25perc",
                               "Establishment and spread, median" = "establishment_and_spread_median",
                               "Establishment and spread, max" = "establishment_and_spread_75perc",
                               "Invasion, min" = "invasion_25perc",
                               "Invasion, median" = "invasion_median", 
                               "Invasion, max" = "invasion_75perc",
                               "Impact, min" = "impact_25perc",
                               "Impact, median" = "impact_median",
                               "Impact, max" = "impact_75perc"
                  ),
                  class = 'cell-border stripe',
                  container = tbl_hdr, # -> Transforms the table header into 2 rows. To make changes go to 'tbl_hdr' in functions.R
                  rownames = TRUE, # It is required for the brush option to have "rownames = TRUE"!
                  extensions = "Buttons",
                  options = list(
                    ## Hide the first column that contains rownames and display columns based on the selections for x and y:
                    columnDefs = list(list(targets = c(0), visible = FALSE), 
                                      if(input$xaxis == "invasion_median" | input$yaxis == "invasion_median") {
                                        list(targets = c(10,11,12), visible = TRUE)
                                      } else list(targets = c(10,11,12), visible = FALSE),
                                      if(input$xaxis == "entry_median" | input$yaxis == "entry_median") {
                                        list(targets = c(4,5,6), visible = TRUE)
                                      } else list(targets = c(4,5,6), visible = FALSE),
                                      if(input$xaxis == "establishment_and_spread_median" | input$yaxis == "establishment_and_spread_median") {
                                        list(targets = c(7,8,9), visible = TRUE)
                                      } else list(targets = c(7,8,9), visible = FALSE),
                                      if(input$xaxis == "impact_mediaani" | input$yaxis == "impact_mediaani") {
                                        list(targets = c(13,14,15), visible = TRUE)
                                      } else list(targets = c(13,14,15), visible = FALSE)
                    ),
                    
                    ## Add build-in buttons for download
                    dom = "Bfrtip", #createEmptyCells = TRUE,
                    buttons = list(list(extend = "excel", text = '<span class="glyphicon glyphicon-th"></span> Excel <sup>1</sup>', title = NULL, 
                                        exportOptions = list(columns = ":visible")), 
                                   list(extend = "csv", text = '<span class="glyphicon glyphicon-download-alt"></span> CSV <sup>1</sup>', title = NULL, 
                                        exportOptions = list(columns = ":visible"))
                    ),
                    ## All results appear on same page:
                    paging = FALSE)) |> 
      
      formatStyle("Pest",  color = "black", fontWeight = "bold", fontStyle = "normal") |>
      formatRound(c("Entry, min", "Entry, median", "Entry, max", "Establishment and spread, min",
                    "Establishment and spread, median", "Establishment and spread, max",
                    "Invasion, min", "Invasion, median", "Invasion, max",
                    "Impact, min", "Impact, median", "Impact, max"), 2) |>
      formatStyle("Entry, median",
                  background = styleColorBar(cleanfinnprioresults$entry_median, "#DAE375"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |>
      formatStyle("Establishment and spread, median",
                  background = styleColorBar(cleanfinnprioresults$establishment_and_spread_median, "#6D9F80"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |>
      formatStyle("Invasion, median",
                  background = styleColorBar(cleanfinnprioresults$invasion_median, "#CEB888"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |>
      formatStyle("Impact, median",
                  background = styleColorBar(cleanfinnprioresults$impact_median, "#DE4C9A"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
    
  })
  
  ## Generate a plot in "1. Select pests to plot"-tab:----
  plot_output <- reactive({   
    req(selected_status())
    
    p <- ggplot(selected_status(), #selected_status1(),
                aes_string(x = input$xaxis, 
                           y = input$yaxis, 
                           color = "quarantine_status"
                )) + 
      
      geom_point(size = 3) +  
      
      xlim(min = 0, max = 1) + ylim(min = 0, max = 1)  + 
      labs(
        caption = paste("    The dots indicate the simulated median score, and the whiskers show the 25th and the 75th percentiles of the distribution of the scores.
                         \n    Number of pests: ", nrow(selected_status())), #selected_status1()
        color = "" # "Quarantine status:"
      ) +
      
      gghighlight(use_direct_label = FALSE, 
                  keep_scales = TRUE) +
      
      theme_bw() +
      #### Size of the plot scales:
      theme(axis.text = element_text(size=12), 
            #### Size of the plot labels:
            axis.title = element_text(size=14,face="bold")) +  
      #### Size of the plot title:
      theme(plot.title = element_text(size=16,hjust=0.5, vjust=0.25, face="bold")) +
      #### Size of the caption that shows number of pests:
      theme(plot.caption = element_text(size=10, hjust=0, face="bold"), plot.caption.position = "plot") +                      
      theme(axis.title.x=element_text(vjust=-1))+
      theme(axis.title.y=element_text(vjust=1))+
      guides(colour = guide_legend(nrow = 1)) +
      theme(legend.text = element_text(size=10),
            legend.direction = "horizontal",
            legend.position = "bottom") +
      
      #Add to scale_color_manual 'limits = force', if want to update the legend. Note that the order becomes alphabetical!
      scale_color_manual(aesthetics = "color", values = cols,limits = force) +
      
      # Zoom the plot:
      coord_cartesian(xlim = c(input$xlims[1], input$xlims[2]), ylim = c(input$ylims[1], input$ylims[2]), expand = TRUE)
    
    
    # Plot conditions:---
    # Add label to X:
    if(input$xaxis == "invasion_median") {
      p <- p+labs(x = "Invasion score") 
    } else if(input$xaxis == "entry_median") {
      p <- p+labs(x = "Entry score")
    } else if(input$xaxis == "establishment_and_spread_median") {
      p <- p+labs(x = "Establishment and spread score")
    } else if(input$xaxis == "impact_mediaani") {
      p <- p+labs(x = "Impact score")
    }
    # Add label to Y:
    if(input$yaxis == "invasion_median") {
      p <- p+labs( y = "Invasion score") 
    } else if(input$yaxis == "entry_median") {
      p <- p+labs( y = "Entry score")
    } else if(input$yaxis == "establishment_and_spread_median") {
      p <- p+labs( y = "Establishment and spread score")
    } else if(input$yaxis == "impact_median") {
      p <- p+labs( y = "Impact score")
    }
    
    
    # Switch between single and multiple plots:
    p <- p +
      {switch(input$split_plott,
              "2" = (
                facet_wrap(~ quarantine_status))
      )
      }
    
    
    # Threshold line (Y): 
    p <- p +  
      {if(input$threshold)
        geom_hline(yintercept = input$threshold, 
                   linetype="dotted",
                   colour = "red", 
                   size = 1, 
                   na.rm = FALSE)} 
    
    # Threshold line (X): 
    p <- p +
      {if(input$thresholdX)
        geom_vline(xintercept = input$thresholdX, 
                   linetype="dotted",
                   colour = "red", 
                   size = 1, 
                   na.rm = FALSE)}
    
    
    # Display pests' names when select checkbox: ----
    p <- p +
      {if(input$pest_name)    
        geom_text(aes(label = pest), 
                  size = 4, 
                  vjust = -0.05, 
                  hjust = -0.08)} 
    
    
    # Display error bars from 25th and 75th percentile when select checkbox for X: ----
    p <- p +  
      {if(input$whiskers_for_x && input$xaxis == "invasion_median") 
        geom_errorbar(aes(xmax = invasion_75perc, 
                          xmin = invasion_25perc),  
                      width = 0.01)
        
        else if(input$whiskers_for_x && input$xaxis == "establishment_and_spread_median") 
          geom_errorbar(aes(xmax = establishment_and_spread_75perc, 
                            xmin = establishment_and_spread_25perc),  
                        width = 0.01)
        
        else if(input$whiskers_for_x && input$xaxis == "entry_median") 
          geom_errorbar(aes(xmax = entry_75perc, 
                            xmin = entry_25perc),  
                        width = 0.01)
        
        else if(input$whiskers_for_x && input$xaxis == "impact_median") 
          geom_errorbar(aes(xmax = impact_75perc, 
                            xmin = impact_25perc),  
                        width = 0.01)} 
    
    
    # Display error bars from 25th and 75th percentile when select checkbox for Y: ----
    p <- p + 
      {if(input$whiskers_for_y && input$yaxis == "impact_median") 
        geom_errorbar(aes(ymax = impact_75perc, 
                          ymin = impact_25perc),  
                      width = 0.01)
        
        else if(input$whiskers_for_y && input$yaxis == "invasion_median") 
          geom_errorbar(aes(ymax = invasion_75perc, 
                            ymin = invasion_25perc),  
                        width = 0.01)
        
        else if(input$whiskers_for_y && input$yaxis == "establishment_and_spread_median") 
          geom_errorbar(aes(ymax = establishment_and_spread_75perc, 
                            ymin = establishment_and_spread_25perc),  
                        width = 0.01)
        
        else if(input$whiskers_for_y && input$yaxis == "entry_median") 
          geom_errorbar(aes(ymax = entry_75perc, 
                            ymin = entry_25perc),  
                        width = 0.01)}  
    
    p
    
    
  })
  
  
  output$pest_plot <- renderPlot({
    req(selected_status())
    plot_output()
  })
  
  ## Download the plot:----
  
  output$download <- downloadHandler(
    filename = function() {
      paste("plot", input$extension, sep = ".")
    },
    content = function(file){
      ggsave(file, plot_output(), device = input$extension,  width = 15, height = 10)
    }
  )
  

  ## Generate table with all assessed pests in "2. Show pests in data table"-tab:-----
  output$table_all <- DT::renderDataTable({
    #The format of the following columns is converted from character to factor, so the selectize inputs (list in filter options) to be available:
    cleanfinnprioresults$quarantine_status <- as.factor(cleanfinnprioresults$quarantine_status)
    cleanfinnprioresults$taxonomic_group <- as.factor(cleanfinnprioresults$taxonomic_group)
    cleanfinnprioresults$presence_in_europe <- as.factor(cleanfinnprioresults$presence_in_europe)
    
    #Creating links to EPPO Global Database:
    eppocode = cleanfinnprioresults$eppo_code
    base_link = "https://gd.eppo.int/taxon/"
    eppoGD = paste0('<a href="', base_link, eppocode,'" target="_blank">', eppocode, '</a>')
    #Add new column to the datatable:
    # cleanfinnprioresults[,  ':='(eppo = eppoGD)]
    cleanfinnprioresults$eppo <- eppoGD

    DT::datatable(select(cleanfinnprioresults, 
                         pest, 
                         #eppo_code,
                         eppo,
                         taxonomic_group,
                         quarantine_status,
                         presence_in_europe, 
                         entry_median,
                         establishment_and_spread_median,
                         invasion_median, 
                         impact_median,
                         riska_median,
                         preventability_median,
                         controlability_median,
                         manageability_median,
                         assessment_date
    ),
    
    colnames = c("Sort" = "pest", 
                 "Pest" = "pest", 
                 #"EPPO code" = "eppo_code",
                 "EPPO Code" = "eppo",
                 "Taxonomic group" = "taxonomic_group", 
                 "Quarantine status" = "quarantine_status",
                 "Presence in Europe" = "presence_in_europe", 
                 "Entry" = "entry_median", 
                 "Establishment and spread" = "establishment_and_spread_median",
                 "Invasion" = "invasion_median", 
                 "Impact" = "impact_median",
                 "Risk A" = "riska_median",
                 "Preventability" = "preventability_median",
                 "Controllability" = "controlability_median",
                 "Manageability" = "manageability_median",
                 "Assessed, month/year" = "assessment_date"),
    container = tbl_hdr_all, # -> Transforms the table header into 2 rows. To make changes go to 'tbl_hdr_all' in functions.R
    rownames = TRUE,
    extensions = "Buttons",
    filter = list(position = 'top', clear = FALSE),
    options = list(
      #pageLength = 15,
      #columnDefs = list(),
      #fixedHeader = TRUE,
      
      ## Hide the first column that contains rownames
      columnDefs = list(list(targets = c(0), visible = FALSE)),
      
      paging = FALSE,
      autoWidth = TRUE, 
      
      # Display and customize the buttons:
      dom = "Bfrtip", 
      buttons = list(list(extend = "excel", 
                          text = '<span class="glyphicon glyphicon-th"></span> Excel <sup>1</sup>', title = NULL, 
                          exportOptions = list(columns = ":visible")), 
                     list(extend = "csv", text = '<span class="glyphicon glyphicon-download-alt"></span> CSV <sup>1</sup>', title = NULL, 
                          exportOptions = list(columns = ":visible"))
      )
    ),
    class = "cell-border stripe",
    
    #To use the links in the column 'EPPO Codes', 'escape=FALSE' should be enabled:
    escape = FALSE) |> 
      # escape = c(1,2, 4:13)) %>% 
      
      formatRound(c("Entry","Establishment and spread", "Invasion", 
                    "Impact", "Risk A", "Preventability", "Controllability", "Manageability"), 3) |> 
      formatStyle("Pest",  fontWeight = "bold", fontStyle = "normal") |> 
      
      formatStyle("Entry",
                  background = styleColorBar(cleanfinnprioresults$entry_median, "#DAE375"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |> 
      formatStyle("Establishment and spread",
                  background = styleColorBar(cleanfinnprioresults$establishment_and_spread_median, "#6D9F80"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |> 
      formatStyle("Invasion",
                  background = styleColorBar(cleanfinnprioresults$invasion_median, "#CEB888"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |> 
      formatStyle("Impact",
                  background = styleColorBar(cleanfinnprioresults$impact_median, "#DE4C9A"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |> 
      formatStyle("Risk A",
                  background = styleColorBar(cleanfinnprioresults$riska_median, "#DE4C9A"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |> 
      formatStyle("Manageability",
                  background = styleColorBar(cleanfinnprioresults$manageability_median, "#ADD2EE"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |> 
      formatDate("Assessed, month/year", method =  "toLocaleDateString", 
                 params = list(
                   'en-US', 
                   list(
                     month = 'numeric',
                     year = 'numeric' 
                   )
                 ))
    
  })
  
  ### Table with mean ---
  output$table_all_2 <- DT::renderDataTable({
    #The format of the following columns is converted from character to factor, so the selectize inputs (list in filter options) to be available:
    cleanfinnprioresults$quarantine_status <- as.factor(cleanfinnprioresults$quarantine_status)
    cleanfinnprioresults$taxonomic_group <- as.factor(cleanfinnprioresults$taxonomic_group)
    cleanfinnprioresults$presence_in_europe <- as.factor(cleanfinnprioresults$presence_in_europe)
    
    #Creating links to EPPO Global Database:
    eppocode = cleanfinnprioresults$eppo_code
    base_link = "https://gd.eppo.int/taxon/"
    eppoGD = paste0('<a href="', base_link, eppocode,'" target="_blank">', eppocode, '</a>')
    #Add new column to the datatable:
    # cleanfinnprioresults[,  ':='(eppo = eppoGD)]
    cleanfinnprioresults$eppo <- eppoGD
    
    DT::datatable(select(cleanfinnprioresults, 
                         pest, 
                         #eppo_code,
                         eppo,
                         taxonomic_group,
                         quarantine_status,
                         presence_in_europe, 
                         entry_median,
                         establishment_and_spread_mean,
                         invasion_mean, 
                         impact_mean,
                         riska_mean,
                         preventability_mean,
                         controlability_mean,
                         manageability_mean,
                         assessment_date
    ),
    
    colnames = c("Sort" = "pest", 
                 "Pest" = "pest", 
                 #"EPPO code" = "eppo_code",
                 "EPPO Code" = "eppo",
                 "Taxonomic group" = "taxonomic_group", 
                 "Quarantine status" = "quarantine_status",
                 "Presence in Europe" = "presence_in_europe", 
                 "Entry" = "entry_median", 
                 "Establishment and spread" = "establishment_and_spread_mean",
                 "Invasion" = "invasion_mean", 
                 "Impact" = "impact_mean",
                 "Risk A" = "riska_mean",
                 "Preventability" = "preventability_mean",
                 "Controllability"= "controlability_mean",
                 "Manageability"= "manageability_mean",
                 "Assessed, month/year" = "assessment_date"),
    container = tbl_hdr_all_2, # -> Transforms the table header into 2 rows. To make changes go to 'tbl_hdr_all' in functions.R
    rownames = TRUE,
    extensions = "Buttons",
    filter = list(position = 'top', clear = FALSE),
    options = list(
      #pageLength = 15,
      #columnDefs = list(),
      #fixedHeader = TRUE,
      
      ## Hide the first column that contains rownames
      columnDefs = list(list(targets = c(0), visible = FALSE)),
      
      paging = FALSE,
      autoWidth = TRUE, 
      
      # Display and customize the buttons:
      dom = "Bfrtip", 
      buttons = list(list(extend = "excel", 
                          text = '<span class="glyphicon glyphicon-th"></span> Excel <sup>1</sup>', title = NULL, 
                          exportOptions = list(columns = ":visible")), 
                     list(extend = "csv", text = '<span class="glyphicon glyphicon-download-alt"></span> CSV <sup>1</sup>', title = NULL, 
                          exportOptions = list(columns = ":visible"))
      )
    ),
    class = "cell-border stripe",
    
    #To use the links in the column 'EPPO Codes', 'escape=FALSE' should be enabled:
    escape = FALSE) |> 
      # escape = c(1,2, 4:13)) %>% 
      
      formatRound(c("Entry","Establishment and spread", "Invasion", 
                    "Impact", "Risk A", "Preventability", "Controllability", "Manageability"), 3) |> 
      formatStyle("Pest",  fontWeight = "bold", fontStyle = "normal") |> 
      
      formatStyle("Entry",
                  background = styleColorBar(cleanfinnprioresults$entry_median, "#DAE375"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |> 
      formatStyle("Establishment and spread",
                  background = styleColorBar(cleanfinnprioresults$establishment_and_spread_mean, "#6D9F80"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |> 
      formatStyle("Invasion",
                  background = styleColorBar(cleanfinnprioresults$invasion_mean, "#CEB888"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |> 
      formatStyle("Impact",
                  background = styleColorBar(cleanfinnprioresults$impact_mean, "#DE4C9A"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |> 
      formatStyle("Risk A",
                  background = styleColorBar(cleanfinnprioresults$riska_mean, "#DE4C9A"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |> 
      formatStyle("Manageability",
                  background = styleColorBar(cleanfinnprioresults$manageability_mean, "#ADD2EE"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |> 
      formatDate("Assessed, month/year", method =  "toLocaleDateString", 
                 params = list(
                   'en-US', 
                   list(
                     month = 'numeric',
                     year = 'numeric' 
                   )
                 ))
    
  })
  
  
  # observe({
  #   updateSelectInput(session,"pest1_sel",choices=colnames(pestquestions))
  #   #updateSelectInput(session,"sizes",choices=colnames(df_p_samp()))
  # }) 
  
  
  ## Generate table with FinnPRIO assessments allowing comparison of two pests in "3. Compare pests by questions"-tab:----
  output$pest_1_2 <- DT::renderDataTable({
    
    req(input$Codes)
    # Filter the questions' groups based on the info in one column using dropdown menu -> Entry, Establishment and spread,Ipact, Management:
    pestquestion_codes <- reactive({  
      pestquestions |> 
        filter(Codes %in% input$Codes)
    }) 
    
    
    req(input$pest1_sel,input$pest2_sel) 
    dat <- datatable(
      
      select(pestquestion_codes(),
             "Question",
             "Answer for",
             input$pest1_sel,
             input$pest2_sel),
      #class = 'cell-border stripe',
      extensions = "FixedHeader",
      options = list(initComplete = JS(
        
        ###Adds color to the table header:
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#343841', 'color': '#fff'});",
        "}"),
        
        columnDefs = list(list(className = 'dt-right', targets = 1), list(targets = 
                                                                            "_all", width = "70px")),
        
        ## Remove the option to order the columns -> in our case is useful because the content of the table is text or there are empty rows:
        ordering=FALSE, 
        ## All results appear on same page:
        paging=FALSE, 
        ## Removes the search box above the table & the text with number of rows under the table:
        dom = "t"       
      ),
      rownames = FALSE) |> 
      
      
      ### Formating specific cells/words in the table 
      # (https://rstudio.github.io/DT/), 
      # (https://rstudio.github.io/DT/functions.html), (https://rstudio.github.io/DT/010-style.html):
      formatStyle(columns = 1:2, 
                  
                  fontWeight = styleEqual(c("most likely", ""),
                                          c('bold', '')),
                  
                  fontStyle = styleEqual(c("min", "max"),
                                         c('italic', 'italic'))
                  
      ) |> 
      
      formatStyle(input$Codes == "ENT",
                  "Question",
                  target = 'row',
                  backgroundColor = styleEqual(c("Pathway 1","Pathway 2", "Pathway 3", "Pathway 4", "Pathway 5"), 
                                               c("#CEB888", "#CEB888", "#CEB888", "#CEB888", "#CEB888")),
                  fontWeight = styleEqual(c("Pathway 1", "Pathway 2", "Pathway 3", "Pathway 4", "Pathway 5"),
                                          c('bold', 'bold', 'bold', 'bold', 'bold')),
                  fontSize = styleEqual(c("Pathway 1", "Pathway 2", "Pathway 3", "Pathway 4", "Pathway 5"), c(14, 14, 14, 14, 14)))
    
    return(dat)
  })
  
  
  ## Show explanations for the pathways in Entry questions in "3. Compare pests by questions"-tab:----
  output$helpPW <- renderUI({
    req(input$Codes)
    
    if (input$Codes == "ENT") {
      
      wellPanel(
        tags$h4(strong("Help for ENT questions"), style="color:#7C6A56"),
        
        help_pw(),
        tags$p ("ENT2A & ENT2B"),
        
        help_pw_a_h(),
        tags$p ("Pathways A-H")
        
      )
    }
  })
  
  ## Generate a table with ranks and HV in "4. Ranking pests"-tab:-----
  output$table_hv <- DT::renderDataTable({
    
    #The format of the following columns is converted from character to factor, so the selectize inputs (list of filter options) to be available:
    hv$quarantine_status <- as.factor(hv$quarantine_status)
    
    DT::datatable(hv, colnames = c("Pest" = "pest",
                                   "Quarantine status" = "quarantine_status",
                                   "Entry, rank" = "rank_entry",
                                   "Entry, hypervolume" = "hv_entry",
                                   "Establishment and spread, rank" = "rank_establishment",
                                   "Establishment and spread, hypervolume" = "hv_establishment",
                                   "Invasion, rank" = "rank_invasion",
                                   "Invasion, hypervolume" = "hv_invasion",
                                   "Impact, rank" = "rank_impact",
                                   "Impact, hypervolume" = "hv_impact"
    ),
    container = tbl_hdr_map, # -> Transforms the table header into 2 rows. To make changes go to 'tbl_hdr_map' in functions.R
    rownames = TRUE,
    extensions = "Buttons",
    options = list(#pageLength = 15,
      ## Hide the first column that contains rownames
      columnDefs = list(list(targets = c(0), visible = FALSE), list(targets = c(0),className = 'dt-head-center')),
      
      paging=FALSE,
      autoWidth = FALSE, 
      # Display and customize the buttons:
      dom = "Bfrtip", 
      buttons = list(list(extend = "excel", text = '<span class="glyphicon glyphicon-th"></span> Excel <sup>1</sup>', title = NULL, 
                          exportOptions = list(columns = ":visible")), 
                     list(extend = "csv", text = '<span class="glyphicon glyphicon-download-alt"></span> CSV <sup>1</sup>', title = NULL, 
                          exportOptions = list(columns = ":visible")) #, I('colvis')
      )
    ),
    #class = "cell-border stripe",
    filter = "top") |> 
      formatStyle("Pest",  fontWeight = "bold", fontStyle = "normal") |> 
      formatRound(c("Entry, hypervolume", "Establishment and spread, hypervolume", "Invasion, hypervolume", "Impact, hypervolume"), 2)
    
  })
  
  ## Generate a treemap for ranks and hypervolume scores:----
  output$hv_tree <- renderPlot ({
    
    Sys.sleep(1.5)
    t <- ggplot(hv, aes(area = hv_impact, fill = rank_impact, 
                        subgroup = rank_impact, 
                        label = paste(pest))) + 
      geom_treemap() +
      geom_treemap_subgroup_border(color = "#DB4258")+
      geom_treemap_subgroup_text(place = "centre", grow = TRUE, alpha = 0.5, color = "#CEB888", min.size = 0)+
      
      geom_treemap_text(colour = "white", place = "middle", reflow = TRUE, fontface = "italic") 
    # theme(legend.position = "none")
    
    t <- t +
      
      # Switch between plots:
      
      {switch(input$switch_tree,
              "1" = aes(area = hv_entry, fill = rank_entry, 
                        subgroup = rank_entry, 
                        label = paste(pest)),
              "2" = aes(area = hv_establishment, fill = rank_establishment, 
                        subgroup = rank_establishment, 
                        label = paste(pest)),
              "3" = aes(area = hv_invasion, fill = rank_invasion, 
                        subgroup = rank_invasion, 
                        label = paste(pest))
      )
      }
    
    t
    
  })
}