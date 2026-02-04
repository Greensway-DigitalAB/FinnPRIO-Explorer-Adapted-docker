# Define UI for application that allow to explore FinnPRIO results: ----

ui <- function(request){
  
  navbarPage("FinnPRIO Explorer Adapted",
             theme = shinythemes::shinytheme("sandstone"),
             header = tagList(
               # Initialize shinyjs
               # useShinyjs(),
               tags$head(
                 tags$link(rel = "shortcut icon", href = "./img/bug-slash-solid-full-gray.svg")#,
                 # Include our custom CSS
                 # tags$link(rel = "stylesheet", href = "styles.css")
               )
             ),
             tabPanel("1. Plot pests on a graph",
                      
                      fluidPage(
                        
                        sidebarLayout(
                          
                          # Sidebar with inputs: ----
                          sidebarPanel(
                            
                            h3(strong(style = "font-size:24px;","Selection criteria")),
                            
                            fluidRow(column(6,
                                            tags$h4(strong("FinnPRIO sections to be plotted"), style = "color:#7C6A56"),
                                            
                                            ## Select variable for Y-axis:----
                                            selectizeInput("yaxis", 
                                                           label = "y-axis",
                                                           choices = c("Entry" = "entry_median",
                                                                       "Establishment and spread" = "establishment_and_spread_median",
                                                                       "Invasion" = "invasion_median",
                                                                       "Impact" = "impact_median"),
                                                           selected = "impact_median"
                                            ),
                                            ## Select variable for X-axis:----
                                            selectizeInput("xaxis", 
                                                           label = "x-axis",
                                                           choices = c("Entry" = "entry_median",
                                                                       "Establishment and spread" = "establishment_and_spread_median",
                                                                       "Invasion" = "invasion_median",
                                                                       "Impact" = "impact_median"),
                                                           selected = "invasion_median"
                                            ),
                                            tags$hr(style="border-color: gray;"),
                                            tags$h4(strong("Quarantine status"), style="color:#7C6A56"),
                                            
                                            ## Select pests' status according to the new EU Regulation:----
                                            checkboxGroupInput(inputId = "quarantine_status",
                                                               label = NULL,
                                                               choices = quaran$name,
                                                               selected = quaran$name,
                                                               inline = FALSE
                                            ),
                                            
                                            tags$hr(style="border-color: gray;"),
                                            tags$h4(strong("Taxonomic group"), style="color:#7C6A56"),
                                            
                                            ## Select pest taxonomic group:----
                                            checkboxGroupInput(inputId = "taxonomic_group",
                                                               label = NULL,
                                                               choices = taxa$name,
                                                               selected = taxa$name,
                                                               inline = FALSE
                                            ),
                                            
                            ),
                            
                            column(6, offset = 0,
                                   tags$h4(strong("Presence in Europe"), style="color:#7C6A56"),
                                   
                                   ## Select pest presence in Europe: ----
                                   checkboxGroupInput(inputId = "presence_in_europe",
                                                      label = NULL, 
                                                      choices = c("Present" = TRUE,
                                                                  "Absent" = FALSE),
                                                      selected = c(TRUE, FALSE),
                                                      inline = FALSE
                                   ),
                                   
                                   tags$hr(style="border-color: gray;"),
                                   ## Select threatened sectors: ----
                                   uiOutput("threat_checkboxes"),
                                   # tags$h4(strong("Threatened sector"), style="color:#7C6A56"),
                            
                            )),
                            
                            tags$hr(style="border-color: gray;"),
                            tags$h4(strong("Determine the pests shown on the plot based on"), style="color:#7C6A56"),
                            fluidRow(
                              column(3,
                                     tags$br(),
                                     tags$h5(strong("Entry score")),
                                     tags$br(),
                                     tags$h5(strong("Establishment and spread score")),
                                     tags$br(),
                                     tags$h5(strong("Invasion score")),
                                     tags$br(),
                                     tags$h5(strong("Impact score")),
                                     
                              ),
                              
                              column(9,
                                     ## Scores selections----
                                     sliderInput(inputId = "entry_score", 
                                                 label = NULL,
                                                 min = 0, max = 1, value = c(0,1), step = 0.05, width = "350px"),
                                     
                                     sliderInput(inputId = "establishment_score", 
                                                 label = NULL,
                                                 min = 0, max = 1, value = c(0,1), step = 0.05, width = "350px"),
                                     
                                     sliderInput(inputId = "invasion_score", 
                                                 label = NULL,
                                                 min = 0, max = 1, value = c(0,1), step = 0.05, width = "350px"),
                                     sliderInput(inputId = "impact_score", 
                                                 label = NULL,
                                                 min = 0, max = 1, value = c(0,1), step = 0.05, width = "350px"),
                              ))
                          ),
                          
                          # Main panel with the results after selections: -----
                          mainPanel(
                            fluidRow(
                              column(9,
                                     tags$br(),
                                     ## Display a plot that presents Invasion vs. Impact: --
                                     plotOutput(outputId = "pest_plot",
                                                height = "650px", 
                                                brush = brushOpts(id = "plot_brush", resetOnNew = TRUE)
                                     ), 
                                     tags$br(),
                                     column(12,
                                            helpText("To generate a table that contains some (or all) of the pests that are presented on the plot, select an area using the pointer.",
                                                     "Note that this works only for the 'Single plot' option.")
                                     )
                              ),
                              
                              column(3,
                                     wellPanel( 
                                       tags$h4(strong("Show")),
                                       # Test... input for median or mean
                                       # selectInput(inputId = "center",
                                       #             label = "Center tendency measure",
                                       #             choices = c("median", "mean"),
                                       #             width = "auto"),
                                       # Pests' names on the plot:
                                       checkboxInput(inputId = "pest_name",
                                                     label = "Pest names",
                                                     value = FALSE,
                                                     width = "auto"),
                                       
                                       # Error bars on the plot:
                                       help_add(),
                                       tags$h5(strong("Uncertainty")),
                                       checkboxInput(inputId = "whiskers_for_y",
                                                     label = "Whiskers for y",
                                                     value = TRUE),
                                       checkboxInput(inputId = "whiskers_for_x",
                                                     label = "Whiskers for x",
                                                     value = TRUE),
                                       
                                       help_threshold(), 
                                       tags$h5(strong("Threshold line")),
                                       fluidRow(
                                         column(1,
                                                tags$h5(strong("y")),
                                                tags$br(),
                                                tags$h5(strong("x"))
                                                
                                         ),
                                         column(10,
                                                # A threshold line for the impact on the plot:
                                                numericInput(inputId ="threshold", 
                                                             label=NULL, 
                                                             value = 0,
                                                             min = 0, max = 1, step = 0.05),
                                                numericInput(inputId ="thresholdX", 
                                                             label=NULL, 
                                                             value = 0,
                                                             min = 0, max = 1, step = 0.05),
                                         ))
                                     ),
                                     
                                     wellPanel(
                                       help_plot_grid(),
                                       #Number of plots:
                                       radioButtons(inputId = "split_plott",
                                                    label = "Number of plots",
                                                    choices = c("Single plot" = 1, 
                                                                "Multiple plots" = 2
                                                    ), 
                                                    selected = 1),
                                       tags$h5(strong("Zoom by")),
                                       fluidRow(
                                         column(1,
                                                tags$h5(strong("y")),
                                                tags$br(),tags$br(),
                                                tags$h5(strong("x"))
                                         ),
                                         column(10,
                                                #Zoom the plot by x and y:
                                                sliderInput(inputId="ylims",
                                                            label=NULL, 
                                                            min = 0, max = 1, value = c(0,1), step = 0.05),
                                                sliderInput(inputId="xlims",
                                                            label=NULL, 
                                                            min = 0, max = 1, value = c(0,1), step = 0.05),
                                         ))
                                     ),
                                     wellPanel(
                                       fluidRow(
                                         column(7,
                                                #Download with options:
                                                downloadButton(outputId = "download", 
                                                               label = "Save Plot")
                                         ),
                                         column(5,offset = 0,
                                                radioButtons(inputId = "extension", 
                                                             label = NULL,
                                                             choices = c("png", "pdf"), inline = FALSE)
                                         )
                                       )))
                            ),
                            
                            tags$hr(style="border-color: gray;"),
                            # helpText("Note that currently the downloaded table contains the scores for all sectors, i.e., Entry, Establishments and spread, Invasion and Impact"),
                            fluidRow(
                              # Table brush selection:
                              
                              helpText(tags$sup(1), "Note that the upper row of the table's header is not visible in the downloaded table."),
                              
                              DT::dataTableOutput(outputId = "tableBrush"
                              )
                            ),
                            tags$br()
                            
                          )
                        )
                      )
             ),
             tabPanel("2. Show pests in data table",
                      tabsetPanel(id = "all_assessments",
                                  tabPanel(#id = "all_assessments", 
                                    title = tagList(icon("laptop-file", class = "fas"), "Assessments based on Median"),
                                    value = 1,
                                    helpText(tags$sup(1), "Note that the upper row of the table's header is not visible in the downloaded table."),
                                    #helpText(strong(style="color: #D2132E;", tags$sup(2), "Link to EPPO GD.")),
                                    DT::dataTableOutput(outputId = "table_all")
                                  ),
                                  tabPanel(#id = "all_assessments", 
                                    title = tagList(icon("laptop-file", class = "fas"), "Assessments based on Mean"),
                                    value = 2,
                                    helpText(tags$sup(1), "Note that the upper row of the table's header is not visible in the downloaded table."),
                                    DT::dataTableOutput(outputId = "table_all_2")
                                  )
                                  
                      )
             ),
             tabPanel("3. Risk rank Plot",
                      fluidPage(
                        fluidRow(
                          column(9,
                                 plotOutput(outputId = "riskrank_plot",
                                            height = "1200px")
                                 ),
                          column(3,
                                wellPanel(
                                  fluidRow(
                                    column(7,
                                           #Download with options:
                                           downloadButton(outputId = "download_risk", 
                                                          label = "Save Plot")
                                    ),
                                    column(5,offset = 0,
                                           radioButtons(inputId = "extension_risk", 
                                                        label = NULL,
                                                        choices = c("png", "pdf"), inline = FALSE)
                                           )
                                    )
                                  )
                          )
                        )
                      )
             ),
             tabPanel("4. Compare pests by questions",
                      fluidPage(
                        fluidRow(
                          column(3,
                                 wellPanel(h3(strong(style = "font-size:24px;", "Selection criteria")),
                                           tags$br(),
                                           selectInput(inputId = "Codes",
                                                       label = "Section",
                                                       choices = c("Entry" = "ENT",
                                                                   "Establishment and spread" = "EST",
                                                                   "Impact" = "IMP",
                                                                   "Management" = "MAN"),
                                                       selected = c("Entry" = "ENT")
                                           ),
                                           tags$hr(style="border-color: gray;"),
                                           tags$br(),
                                           selectInput(inputId = "pest1_sel",
                                                       label = "Pest 1",
                                                       #choices=c()
                                                       choices = unique(cleanfinnprioresults$pest)
                                           ),
                                           tags$br(),
                                           selectInput(inputId = "pest2_sel",
                                                       label = "Pest 2",
                                                       choices = unique(cleanfinnprioresults$pest),
                                                       selected = "Aculops fuchsiae"
                                           )
                                           
                                 ) ,
                                 tags$br(),
                                 uiOutput(outputId = "helpPW")
                                 
                          ),
                          column(9,
                                 DT::dataTableOutput(outputId = "pest_1_2"),
                                 tags$br()
                          )
                        )
                      )
                      
             ),
             
             # tabPanel("4. Rank pests",
             #          tags$h4(strong("Ranking of the pests based on stochastic dominance and hypervolume"), style="color:#7C6A56"),
             #          radioButtons(inputId = "switch_tree",
             #                       label = NULL,
             #                       choices = c("Entry" = 1, 
             #                                   "Establishment and spread" = 2,
             #                                   "Invasion" = 3,
             #                                   "Impact" = 4
             #                       ), 
             #                       selected = 4,
             #                       inline = TRUE),
             #          
             #          shinycssloaders::withSpinner(plotOutput("hv_tree", width = "100%", height = "600px"),
             #                                       type = 7, color = "#7C6A56", size = 1),
             #          
             #          
             #          tags$br(),
             #          helpText(tags$sup(1), "Note that the upper row of the table's header is not visible in the downloaded table."),
             #          DT::dataTableOutput(outputId = "table_hv")
             # ),
             
             tabPanel("About the app",
                      fluidRow(column(6,
                                      
                                      wellPanel( 
                                        p(tags$p(
                                          tags$b(style = "font-size:13px;",
                                                 "FinnPRIO Explorer Adapted presents results from FinnPRIO assessments conducted for Sweden made with the FinnPRIO model", 
                                                 tags$a("(Heikkila et al. 2016)",href="https://doi.org/10.1007/s10530-016-1123-4", target="_blank"),
                                                 ". It is a modified version of the original app FinnPRIO-Explorer developed in the Risk Assessment Unit of the Finnish Food Authority.",
                                                 tags$a("(Marinova-Todorova et al. 2022)",href="https://finnprio-explorer.2.rahtiapp.fi/", target="_blank"))),
                                          tags$br(),
                                          tags$b("FinnPRIO model"),
                                          tags$p(),
                                          tags$p(style = "font-size:13px;",
                                                 "FinnPRIO is a model for ranking non-native plant pests based on the risk that they pose to plant health", 
                                                 tags$a("(Heikkila et al. 2016)",href="https://doi.org/10.1007/s10530-016-1123-4", target="_blank"),". 
                                                 It is composed of five sections: likelihood of entry, likelihood of establishment and spread, magnitude of 
                                                 impacts, preventability, and controllability. The score describing the likelihood of invasion is a product 
                                                 of entry and establishment scores. The score describing the manageability of invasion is the minimum of 
                                                 prevantability and controllability scores."),
                                          tags$p(),
                                          tags$p(style = "font-size:13px;",
                                                 "FinnPRIO consists of multiple-choice questions with different answer options yielding a different number 
                                                 of points. For each question, the most likely answer option and the plausible minimum and maximum options 
                                                 are selected based on the available scientific evidence. The selected answer options are used to define 
                                                 a PERT probability distribution and the total section scores are obtained with Monte Carlo simulation. 
                                                 The resulting probability distributions of the section scores describe the uncertainty of the assessment.
                                                 Summary statistics of the score distributions can be explored in the tab 'Plot pests on a graph'"),
                                          
                                          tags$br(),
                                          tags$b("FinnPRIO-Explorer Adapted vs. FinnPRIO-Explorer"),
                                          tags$p(style = "font-size:13px;","FinnPRIO-Explorer Adapted introduces additional functionality: risk scores are 
                                                 shown directly in the interface and a ranking based on risk is included. Users can also select to display all 
                                                 scores not only as median values but also as mean values. The uncertainty is displayed by showing the 5th percentile 
                                                 and the 95th percentile."),
                                          tags$br(),
                                          tags$b("FinnPRIO assessments for Sweden"),
                                          tags$p(),
                                          tags$p(style = "font-size:13px;",
                                                 "The results presented in this app are based on all FinnPRIO assessments done for Sweden and the calculations 
                                                 were done using the FinnPRIO-Assessor app", 
                                                  tags$a("(ZENODO)",href="https://doi.org/10.5281/zenodo.17816319", target="_blank")),
                                          tags$p(),
                                          tags$p(style = "font-size:13px;",
                                                 "The probability distributions of the scores were simulated with 50 000 iterations. The likelihood of entry 
                                                 is assessed taking into account the current management measures.")
                                          # tags$br(),
                                          # tags$b("Ranking FinnPRIO assessments using the hypervolume approach"),
                                          # tags$p(),
                                          # tags$p(style = "font-size:13px;", 
                                          #        "To facilitate comparison of the FinnPRIO score distributions, a hypervolume (HV) approach was used to 
                                          #        aggregate distributions into a simple single-dimensional form that reveals the preference order relationship 
                                          #        of the distributions (for details, see  ", 
                                          #        tags$a("Yemshanov et al. 2012,",href="https://doi.org/10.1111/j.1472-4642.2011.00848.x", target="_blank"),  
                                          #        tags$a("2017;",href="https://doi.org/10.1016/j.jenvman.2017.02.021", target="_blank"), "for practical examples, see",
                                          #        tags$a("Tuomola et al. 2018;",href="https://doi.org/10.3391/mbi.2018.9.2.05", target="_blank"), 
                                          #        tags$a("Marinova-Todorova et al. 2020",href="https://doi.org/10.1111/epp.12667", target="_blank"),")."),
                                          # tags$p(),
                                          # tags$p(style = "font-size:13px;", 
                                          #        "Ranking with the pairwise stochastic dominance rule and the HV indicator calculations were performed using 
                                          #        a stand-alone program written in C++ that applies the hypervolume calculation algorithm from", 
                                          #        tags$a("While et al. (2012)", href="https://ieeexplore.ieee.org/document/5766730", target="_blank"), ". 
                                          #        The program was kindly provided by Denys Yemshanov from Natural Resources Canada. The cumulative distribution 
                                          #        functions were calculated from the score distributions at 70 equal intervals and ordered using the first-order 
                                          #        stochastic dominance rule.")
                                        )
                                      )),
                               
                               
                               column(5,
                                      tabsetPanel(
                                        
                                        tabPanel("References",
                                                 p(tags$p(
                                                   tags$br(),
                                                   
                                                   tags$p(style = "font-size:13px;","Heikkila J, Tuomola J, Pouta E & Hannunen S (2016) FinnPRIO: a model for ranking invasive 
                                                          plant pests based on risk. Biological Invasions 18, 1827- 1842. ",
                                                          tags$a("doi.org/10.1007/s10530-016-1123-4",href="https://doi.org/10.1007/s10530-016-1123-4", target="_blank")),
                                                   tags$br(),
                                                   tags$p(style = "font-size:13px;","Marinova-Todorova M, Bjorklund N, Boberg J, Flo D, Tuomola J, Wendell M & Hannunen S (2020), 
                                                          Screening potential pests of Nordic coniferous forests associated with trade in ornamental plants. 
                                                          EPPO Bulletin 50: 249- 267. ",
                                                          tags$a("doi.org/10.1111/epp.12667",href="https://doi.org/10.1111/epp.12667", target="_blank")),
                                                   tags$br(),
                                                   tags$p(style = "font-size:13px;","Marinova-Todorova M, Tuomola J, Heikkila J & Hannunen S. (2019) A graphical user interface for the FinnPRIO model: 
                                                          A model for ranking plant pests based on risk (Version 1.0). Finnish Food Authority. ",
                                                          tags$a("doi.org/10.5281/zenodo.2784027",href="https://doi.org/10.5281/zenodo.2784027", target="_blank")),
                                                   tags$br(),
                                                   tags$p(style = "font-size:13px;","Marinova-Todorova M, Tuomola J & Hannunen S. (2022) FinnPRIO-Explorer - A tool for examining assessments made with the FinnPRIO model.
                                                          Finnish Food Authority. Available at ", tags$a("https://finnprio-explorer.2.rahtiapp.fi/", target="_blank"),
                                                          tags$a("https://doi.org/10.5281/zenodo.7016771",href="https://doi.org/10.5281/zenodo.7016771", target="_blank")),
                                                   tags$br(),
                                                   tags$p(style = "font-size:13px;","Ruete, A., Björklund, N., & Boberg, J. (2025). FinnPRIO-Explorer Adapted: Explore and visualize FinnPRIO assessment results (Version 1.0) 
                                                          [Web application]. Swedish University of Agricultural Sciences. Available from ",
                                                          tags$a("doi.org/10.5281/zenodo.17813062",href="https://doi.org/10.5281/zenodo.17813062", target="_blank"))
                                                 ))
                                                 
                                        ),
                                        tabPanel("Source code",
                                                 p(tags$p(
                                                   tags$br(),
                                                   tags$p(style = "font-size:13px;",
                                                          "The source code is available at ",
                                                          tags$a("Zenodo",href="https://doi.org/10.5281/zenodo.17813062", target="_blank"),
                                                          " under the ",tags$a("GNU General Public License version 4", href="https://creativecommons.org/licenses/by/4.0/legalcode", target="_blank"), ".")
                                                   
                                                 ))
                                        ),
                                        tabPanel("Cite this app?",
                                                 p(tags$p(
                                                   tags$br(),
                                                   tags$p(style = "font-size:13px;",
                                                          "Ruete, A., Björklund, N., & Boberg, J. (2025). FinnPRIO-Explorer Adapted: Explore and visualize FinnPRIO assessment results (Version 1.0) [Web application]. Swedish University of Agricultural Sciences. Available from ",
                                                          tags$a("https://finnprio-explorer-adapted.serve.scilifelab.se/app/finnprio-explorer-adapted", href="https://finnprio-explorer-adapted.serve.scilifelab.se/app/finnprio-explorer-adapted", target="_blank"),", ",
                                                          tags$a("doi.org/10.5281/zenodo.17813062", href="https://doi.org/10.5281/zenodo.17813062", target="_blank"))
                                                   
                                                 ))
                                        )
                                        
                                      ))
                      )
             ))
  
}
