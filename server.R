######Release version v2
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(recharts)
#library(Seurat)
library(ggsci)
library(plotly)
library(ggsignif)
library(data.table)
library(survival)
library(survminer)
library(pRRophetic)
library(maftools)
library(data.table)
#library(ECharts2Shiny)
options(shiny.maxRequestSize=30*1024^2)
library("drugTargetInteractions")

# luscexpression <- fread('LUSC_Symbols_latestage.csv')
# genes <- luscexpression$Genes

#luscexpression <- fread('LUSC_Symbols_latestage.csv')
# lmaf <- read.maf(maf="data/TCGA.LUSC.mutect.95258183-63ea-4c97-ae29-1bae9ed06334.DR-10.0.somatic.maf.gz")
shinyServer(function(input, output,session) {

   callModule(mod_expression_server, "se")
# 
# #######clinical data
   callModule(mod_clinic_server, "ci")
# 
# #######classifier
   callModule(mod_classifier_server, "cf")
#   
# ######SNV
   callModule(mod_snv_server, "snv")
#   
# ######microRNA
   callModule(mod_miRNA_server,"microRNA")
#   
######methylation
   callModule(mod_methy_server,"methy")
  # callModule(mod_methy_server,"methy1")
   
######Drug
  callModule(mod_drug_server,"cd")
  
  observeEvent(input$disconnect, {
    session$close()
  })

}
)
