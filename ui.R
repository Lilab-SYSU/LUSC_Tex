library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(shinydisconnect)
library(recharts)
# library(maftools)
#library(ECharts2Shiny)
# library(data.table)
library(shinydashboard)
source('script/homePage.R')
source('script/mod_Expression_ui.R')
source('script/mod_Clinic.R')
source('script/mod_classifier.R')
source('script/mod_Snv.R')
source('script/mod_Drug.R')
source('script/mod_methy.R')
source('script/mod_miRNA.R')

# luscexpression <- fread('LUSC_Symbols_latestage.csv')
# genes <- luscexpression$Genes
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

    navbarPage(strong("LUSC immune"),
               id="test",
               tabPanel(
                 icon=icon("home"),
                        'Home',
                        tags$style(
                          HTML(
                            ".navbar-nav > li > a {
                            font-size: 18px;
                            font-weight: bold;
                            }
                            "
                          )
                          ),
                         homePage
						             ),
						    tabPanel(icon=icon("dna"),
						         "Signature expression",
						         mod_expression_ui('se')

						    ),
						    
						    tabPanel(icon=icon("people-arrows"),
						             "Exhausted immune classifier",
						             h2(p("Predict exhausted immune class of patients with LUSC",style="text-align:center")),
						             br(),
						             hr(),
						             br(),
						             mod_classifier_ui('cf'),
						    ),
						    tabPanel(
						      icon=icon("people-arrows"),
						             "Somatic mutation",
						             mod_snv_ui('snv'),
						             br(),
						             br(),
						             br(),
						             br(),
						             br(),
						             br(),
						             br(),
						             br(),
						             br(),
						    ),


						tabPanel(
						  icon=icon("people-arrows"),
						         "MicroRNA",
						         mod_miRNA_ui('microRNA'),
						         #br(),
						         #mod_classifier_ui('cf'),
						),
						
						tabPanel(icon=icon("people-arrows"),
						         "Methylation",
						         mod_methy_ui('methy')
						),
						
						tabPanel(icon=icon("clinic-medical"),
						         "Clinic",
						         mod_clinic_ui('ci')
						),

						tabPanel(icon=icon("capsules"),
						         "Chemotherapy drugs",
						         mod_drug_ui('cd'),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						),
						tabPanel(icon=icon("download"),
						         "Download",
						         #box( width = 12, height = 100,
						         h2(p(strong("The generated data in this study can be downloaded."))),
						         hr(),
						         h3(p("Supplementary materials along with our study.")),
						         tags$a(href="Supplementary_Tables.xlsx", h3("Supplementary tables")),
						         # )
						         br(),
						         hr(),
						         h3(p("Early-stage(stage I to II) LUSC subtype information.")),
						         tags$a(href="Group_Exh_Rest_early_stage.xls",h3("Early-stage Subtype file")),
						         br(),
						         h3(p("Late-stage(stage IIA to IV) LUSC subtype information.")),
						         tags$a(href="Group_Exh_Rest_late_stage.xls",h3("Late-stage Subtype file")),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						         br(),
						),
						box(width = 12,
						    background = "black",
						    p(em("Contact:Minglei Yang"),br("Email:yangmlei3@mail2.sysu.edu.cn"),tags$a(href="http://lilab2.sysu.edu.cn/", h2("Li Lab @Sun Yat-sen University")))
						)
    )
)
)
