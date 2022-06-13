library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(shinydisconnect)
library(ECharts2Shiny)
# library(car)
# library(nortest)
# library(tseries)
# library(RcmdrMisc)
# library(lmtest)


options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)
shinyUI(fluidPage(
  disconnectMessage(
  text = "Your session timed out, reload the application!",
  refresh = "Reload now",
  background = "#f89f43",
  colour = "white",
  overlayColour = "grey",
  overlayOpacity = 0.75,
  top = 250,
  refreshColour = "brown"
),
  theme = shinytheme("cosmo"),

    navbarPage("Immune Exhaustion",
               tabPanel(icon=icon("home"),
                        'Home',
                        
                        fluidRow(
                                 column(

                                   br(),
                                   p("Through this application, it is intended to develop a learning environment for anyone who is starting in the study of statistical modeling,
                                          specifically linear regression through the method of ordinary least squares.
                                          In any case, we will focus on the procedure (graphics, interpretations, hypotheses, etc.) and not on the mathematical processes.",
                                     strong("But do not worry!"), "you will find alternatives to learn all these technical aspects independently.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   br(),

                                   p("The data used in this application are publicly available on the page of the", em("Anuario Estadístico de Antioquia"), "by the administrative planning department.
                                          The data extracted from this public entity correspond to a series of social, educational, sports and safety variables in the rural areas of Antioquia in
                                          Colombia for the year 2016.",style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),

                                   width=8,
                                   offset = 3),
                                 # column(
                                 #     br(),
                                 #     tags$img(src="Gobernacion.png",width="200px",height="130px"),
                                 #     br(),
                                 #     br(),
                                 #     p("For more information please check the",em("Anuario Estadístico de Antioquia's"),"page clicking",
                                 #     br(),
                                 #     a(href="http://www.antioquiadatos.gov.co/index.php/anuario-estadistico-de-antioquia-2016", "Here",target="_blank"),style="text-align:center;color:black"),width=2)
                        ),

                        hr(),
                        fluidRow(column(tags$img(src="Figure7.png",width="1000px",height="1000px",style="border: 1px solid black"),width=6,offset = 3)),
                        # tags$style(".fa-database {color:#E87722}"),
                        # h3(p(em("Dataset "),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
                        # fluidRow(column(DT::dataTableOutput("RawData"), width = 12)),
                        # 
                        # hr(),
                        # p(em("Developed by"),br("Daniel Rivera B."),style="text-align:center; font-family: times")
                        ),
               tabPanel("Signature expression",

                        fluidRow(
                       
                          column(
                            h2(p("Cell type distribution of BALF among healthy control ,moderate and severe COVID-19 patients")),
                            br(),
                            hr(),
                            style = "background-color:#F2EFEF;",
                            width=6,
                            #tags$img(src="Dimplot.png",width="900px",height="400px",style="border: 1px solid black")
                            withSpinner(plotOutput("dimplot"), image = "loading_afa04a3.gif"),
                            downloadButton("downloadDimPlot", "Download")
                                ),
                          column(
                            h4(p('Highly expressed genes in different celltypes')),
                            br(),
                            hr(),
                            width= 4,offset = 1,
                            withSpinner(DT::dataTableOutput('celltypegenes'),image = "loading_afa04a3.gif")
                          )
                    
                          
                        ),
                        br(),
                        br(),
                        h2(p("Explore common gene expression between COVID-19 and lung squamous cell carcinoma.")),
                        
                        fluidRow(
                          # column(2,
                                 h4(p("Please enter a gene")),
                                 #width = 2,
                                 
                                 textInput("genename", "Gene symbol:", "CCL2"),
                                 submitButton("Update View"),
                                 hr(),
                        column(3,
                               h3(p("Gene expression among different cell types")),
                               br(),
                               hr(),
                               withSpinner(plotlyOutput("featureplot"),image = "loading_afa04a3.gif"),
                               downloadButton("downloadfeatureplot", "Download")
                        ),
                        column(6,
                               h3(p("Gene expression value in immune exhaustion class vs rest class")),
                               br(),
                               hr(),
                               withSpinner(plotlyOutput("LUSCboxplot"),image = "loading_afa04a3.gif"),
                              h3(p("Gene expression value in immune exhaustion class vs rest class")),
                              br(),
                              hr(),
                              withSpinner(plotlyOutput("hiPSC_CMsboxplot"),image = "loading_afa04a3.gif")
                        )
                        
                      ),
                      br(),
                      br(),
                      h2(p("Explore common gene expression between COVID-19 and lung squamous cell carcinoma.")),
                      fluidRow(
                        column(width=8,
                               h3(p("KEGG pathway")),
                               withSpinner(DT::dataTableOutput('KEGG'),image = "loading_afa04a3.gif")
                          
                        ),
                        column(width=8,
                               h3(p("GO function annotation")),
                               withSpinner(DT::dataTableOutput('GO'),image = "loading_afa04a3.gif")
                               
                        ),
                        column(width=8,
                               h3(p("Drug target")),
                               withSpinner(DT::dataTableOutput('Drug'),image = "loading_afa04a3.gif")
                               
                        )
                      )
                      
                ),
                    
                        
                        
                        
               tabPanel("Immune exhaustion classifier",
                        h3(p("Gene expression value in immune exhaustion class vs rest class"))
                        
                        
               ),
               
               tabPanel("Immune exhaustion score",
                        h3(p("Gene expression value in immune exhaustion class vs rest class"))
                        
               ),
               tabPanel("Pronosis",
                        h3(p("Gene expression value in immune exhaustion class vs rest class")),
                        fluidRow(
                          column(width=3),
                          column(
                          
                          textInput("genenameSur", "Gene symbol:", ""),
                          numericInput("percent","High group cutoff",50),
                          submitButton("Update View"),
                          width = 3
                          ),
                          column(width=3,
                            plotOutput('survival'),
                            downloadButton("downloadsurvival", "Download")
                          ),
                          column(width=3)
                          ),
                          # column(
                          #   width=3,
                          #   plotlyOutput('pieGenderHigh')
                          # )
                        br(),
                        fluidRow(
                          
                          column(width=3),
                          column(h3(p("Age,gender,tumor stage distribution of high and low gene expression groups")),width = 6)
                        ),
                        fluidRow(
                          
                          column(width=3),
                          column(plotlyOutput('Age'),
                                 width = 6)
                        ),
                        fluidRow(
                          column(width=1),
                          column(br(),
                            h3(p("High expression group")),
                                 width=5,
                                 loadEChartsLibrary(),

                                 tags$div(id="Highgender", style="width:50%;height:400px;"),
                                 deliverChart(div_id = "Highgender")
                                 ),
                          column(br(),
                            h3(p("Low expression group")),
                                 width=5,
                                 loadEChartsLibrary(),
                                 
                                 tags$div(id="Lowgender", style="width:50%;height:400px;"),
                                 deliverChart(div_id = "Lowgender")
                          )
                          
                        ),
                        fluidRow(
                          column(width=1),
                          column(br(),
                                 h3(p("High expression group")),
                                 width=5,
                                 loadEChartsLibrary(),
                                 
                                 tags$div(id="Highstage", style="width:50%;height:400px;"),
                                 deliverChart(div_id = "Highstage")
                          ),
                          column(br(),
                                 h3(p("Low expression group")),
                                 width=5,
                                 loadEChartsLibrary(),
                                 
                                 tags$div(id="Lowstage", style="width:50%;height:400px;"),
                                 deliverChart(div_id = "Lowstage")
                          )
                          
                        )
               ),
               
               tabPanel("Correlation",
                        fluidRow(
                          column(width=3,h3(p("Gene expression value in immune exhaustion class vs rest class")))
                        ),
                        fluidRow(
                          column(width=3,
                                 textInput("geneA",label = "GeneA",value = "CCL2"),
                                 textInput("geneB",label = "GeneB",value = "CCR2"),
                                 submitButton("Update View")
                                 ),
                          column(width=4,
                                 withSpinner(plotOutput("correlation"), image = "loading_afa04a3.gif"),
                                 downloadButton("downloadcorrelation", "Download")
                          ),
                          column(width=4,
                                 withSpinner(plotlyOutput("SCcorrelation"), image = "loading_afa04a3.gif")
                          )
                        ),
                        fluidRow(

                        )
                        
                       
               ),
               tabPanel(icon("pencil-alt"),
                        h3(p("Gene expression value in immune exhaustion class vs rest class"))
                        
                        
                        )
               
               )
         )
  )
