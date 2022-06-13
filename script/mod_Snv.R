mutationgenesdf <- read.csv('LUSC_mutation_summary.csv',header=T)
mutationgenes <- mutationgenesdf$Hugo_Symbol
# lmaf <- read.maf(maf="data/TCGA.LUSC.mutect.95258183-63ea-4c97-ae29-1bae9ed06334.DR-10.0.somatic.maf.gz")

mod_snv_ui <- function(id){
  ns <-NS(id)
  tagList(
    column(width = 10,
           offset = 1,
           box(width = 12,background = "maroon",
               h2(strong('Explore mutation related to potential resistance by correlation with Exhausted Immune class',style="text-align:center"))
               )
           ),

             # column(width = 10,
                    # offset = 1,
                    box(width = 12,
                    navlistPanel(widths=c(2,10),
                    tabPanel(h4(strong("Landscape of mutations")),
                             
                    #column(width = 2,     
                        box(width=3,height = 6,
                            selectInput(ns("tumorstage"), h2(strong("Tumor Stage:")),
                                        multiple = TRUE,
                                        selected = c("Stage I","Stage IA"),
                                        c("Stage I" = "Stage I",
                                          "Stage IA" = "Stage IA",
                                          "Stage IB" = "Stage IB",
                                          "Stage II" = "Stage II",
                                          "Stage IIA" = "Stage IIA",
                                          "Stage IIB" = "Stage IIB",
                                          "Stage III" = "Stage III",
                                          "Stage IIIA" = "Stage IIIA",
                                          "Stage IIIB" = "Stage IIIB",
                                          "Stage IV" = "Stage IV")),
                            selectInput(ns("class"),h2(strong("Class")),
                                        selected = NULL,
                                        multiple = TRUE,
                                        c("e.g. Exhausted Immune class" = "",
                                          "Exhausted Immune class" = "Exhausted",
                                          "Rest class" = "Rest")),
                            selectInput(ns("mutationgenes"), h2("Mutated Genes:"),
                                        multiple = TRUE,
                                        selected = c('TP53','TTN','CSMD3','MUC16','RYR2','LRP1B','USH2A','SYNE1','ZFHX4','KMT2D','FAM135B',
                                                       'NAV3','ADAMTS12','COL11A1','PKHD1','SPTA1','XIRP2','CDH10','DNAH8','PCLO','EGFR','PIK3CA','DDR2'),
                                        choices = mutationgenes
                                        ),
                            
                            # submitButton("Update view")
                            actionButton(ns("landmutsubmit"),"Submit")
                        ),
                        #),
                        #column(width = 8,
                          column(width=9,
                            box(width = 11,
                                title = h3(strong("Summary of mutation landscape")),status = "warning",
                                withSpinner(plotOutput(ns('SummaryPlot')),image = "custom.gif"),
                            ),
                            box(width=11,
                                title = h3(strong("Oncoplot for selected mutated genes")),status = "warning",
                                withSpinner(plotOutput(ns('Oncoplot')),image = "custom.gif")
                              ),
                            box(width = 11,
                                title = h3(strong("Somatic mutation interactions")),status = "warning",
                                withSpinner(plotOutput(ns('somaticInter')),image = "custom.gif")
                            )
                              ),
                          column(width = 12,
                                 box(width=3,
                                     title = h3(strong("Gene for lollipop plot")),status = "warning",
                                     selectInput(ns("Geneforlollipop"), h2("Mutated Genes:"),
                                                # multiple = TRUE,
                                                 selected = c('TP53'),
                                                 choices = mutationgenes),
                                     # submitButton("Update view")
                                     ),
                                 box(width = 5,
                                     title = h3(strong("Lollipop plot")),
                                     status = "warning",
                                     withSpinner(plotOutput(ns('Lollipop')),image = "custom.gif")
                                     )
                                 )
                            #)
                    ),
                  tabPanel(h4(strong("Comparative analysis")),
                           box(width = 12,
                               box(width = 3,
                                   h2(strong("Cohort A:")),
                                   selectInput(ns("cohortA"), h3("Class:"),
                                               c("Exhausted Immune class" = "Exhausted",
                                                 "Rest class" = "Rest")
                                               ),
                                   selectInput(ns("tumorstageCohortA"), h3("Tumor Stage:"),
                                               multiple = TRUE,
                                               selected = c("Stage I","Stage IA"),
                                               c("Stage I" = "Stage I",
                                                 "Stage IA" = "Stage IA",
                                                 "Stage IB" = "Stage IB",
                                                 "Stage II" = "Stage II",
                                                 "Stage IIA" = "Stage IIA",
                                                 "Stage IIB" = "Stage IIB",
                                                 "Stage III" = "Stage III",
                                                 "Stage IIIA" = "Stage IIIA",
                                                 "Stage IIIB" = "Stage IIIB",
                                                 "Stage IV" = "Stage IV")),
                                   h2(strong("Cohort B:")),
                                   selectInput(ns("cohortB"), h3("Class:"),
                                               selected = "Rest",
                                               c("Exhausted Immune class" = "Exhausted",
                                                 "Rest class" = "Rest")
                                              ),
                                   selectInput(ns("tumorstageCohortB"), h3("Tumor Stage:"),
                                               multiple = TRUE,
                                               selected = c("Stage I","Stage IA"),
                                               c("Stage I" = "Stage I",
                                                 "Stage IA" = "Stage IA",
                                                 "Stage IB" = "Stage IB",
                                                 "Stage II" = "Stage II",
                                                 "Stage IIA" = "Stage IIA",
                                                 "Stage IIB" = "Stage IIB",
                                                 "Stage III" = "Stage III",
                                                 "Stage IIIA" = "Stage IIIA",
                                                 "Stage IIIB" = "Stage IIIB",
                                                 "Stage IV" = "Stage IV")),
                                   # submitButton("Update view")
                                   actionButton(ns('comparesubmit'),"Submit")
                                   
                                   ),
                               box(width = 9,
                                   box(width=6,
                                       h2(p(strong("Forest Plot"))),
                                   withSpinner(plotOutput(ns('forestPlot')),image = "custom.gif")
                                   ),
                                   box(width=12,
                                       h2(p(strong("Differentially mutated genes between two cohorts"))),
                                   withSpinner(DT::dataTableOutput(ns('compareDT')),image = "custom.gif")
                                   )
                                   )
                               )
                           ),
                  tabPanel(h4(strong("Survival analysis")),
                           box(width = 3,
                               selectInput(ns("cohortSur"), h3("Class:"),
                                           multiple = TRUE,
                                           selected = c("Exhausted","Rest"),
                                           c("Exhausted Immune class" = "Exhausted",
                                             "Rest class" = "Rest")
                               ),
                               selectInput(ns("tumorstageCohortSur"), h3("Tumor Stage:"),
                                           multiple = TRUE,
                                           selected = c("Stage I","Stage IA"),
                                           c("Stage I" = "Stage I",
                                             "Stage IA" = "Stage IA",
                                             "Stage IB" = "Stage IB",
                                             "Stage II" = "Stage II",
                                             "Stage IIA" = "Stage IIA",
                                             "Stage IIB" = "Stage IIB",
                                             "Stage III" = "Stage III",
                                             "Stage IIIA" = "Stage IIIA",
                                             "Stage IIIB" = "Stage IIIB",
                                             "Stage IV" = "Stage IV")),
                               # selectInput(ns("SurvivalGene"), h2("Mutated Genes:"),
                               #             # multiple = TRUE,
                               #             selected = c('TP53'),
                               #             choices = mutationgenes),
                               selectizeInput(ns("SurvivalGene"),"Gene symbol",choices =NULL),
                               # submitButton("Update view")
                               actionButton(ns("sursubmit"),"Submit")
                               ),
                           box(width = 5,
                               withSpinner(plotOutput(ns('SurPlot')),image = "custom.gif")
                               ),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                    )
                  
             )
      )
   
    # )
  )
}


mod_snv_server <- function(input, output, session) {
  ns <- session$ns
  #library(maftools)
  mutationgenesdf <- read.csv('LUSC_mutation_summary.csv',header=T)
  mutationgenes <- mutationgenesdf$Hugo_Symbol
  updateSelectizeInput(session, 'SurvivalGene', selected=c("TP53"),choices = mutationgenes, server = TRUE)
  
  lmaf <- read.maf(maf="data/TCGA.LUSC.mutect.95258183-63ea-4c97-ae29-1bae9ed06334.DR-10.0.somatic.maf.gz")
  ClinicDat <- fread('data/NMF_group_matched_clinical_from_TCGA_UCSC.xls')
  
  values <- eventReactive(input$landmutsubmit,{
    tumorstage = input$tumorstage
    class = input$class
    genes = input$mutationgenes
    return(list(tumorstage=tumorstage,class=class,genes=genes))
    })
  values1 <- reactive({
    if(input$landmutsubmit == 0){
      tumorstage = c("Stage I","Stage IA")
      class = NULL
      genes = c('TP53','TTN','CSMD3','MUC16','RYR2','LRP1B','USH2A','SYNE1','ZFHX4','KMT2D','FAM135B',
                'NAV3','ADAMTS12','COL11A1','PKHD1','SPTA1','XIRP2','CDH10','DNAH8','PCLO','EGFR','PIK3CA','DDR2')
    }else{
      tumorstage = values()$tumorstage
      class = values()$class
      genes = values()$genes
    }
    return(list(tumorstage,class,genes))
  })
  
  SelectClinic <- reactive({
    loc <- match(ClinicDat$pathologic_stage,values1()[[1]])
    print(values1()[[1]])
    print(loc)
    ClinicDatStage = ClinicDat[!is.na(loc),]
    # print(ClinicDatStage)
    print(values1()[[2]])
    if(is.null(values1()[[2]])){
      ClinicDatStageClass = ClinicDatStage
      print(ClinicDatStageClass)
    }else{
      # print(values1()[[2]])
      print(values1()[[2]])
      loc1 <- match(ClinicDatStage$group,values1()[[2]])
      ClinicDatStageClass = ClinicDatStage[!is.na(loc1),]
      
    }
    print(ClinicDatStageClass$Tumor_Sample_Barcode)
    SelectedMAF=subsetMaf(maf = lmaf, tsb =ClinicDatStageClass$Tumor_Sample_Barcode)
    list(maf=SelectedMAF,clinic=ClinicDatStageClass)
  })
  
  
  # genes = c('TP53','TTN','CSMD3','MUC16','RYR2','LRP1B','USH2A','SYNE1','ZFHX4','KMT2D','FAM135B',
  #           'NAV3','ADAMTS12','COL11A1','PKHD1','SPTA1','XIRP2','CDH10','DNAH8','PCLO','EGFR','PIK3CA','DDR2')
  MutatedGene <- reactive({values1()[[3]]})
  
  vc_cols1 = RColorBrewer::brewer.pal(n = 8, name = 'Set1')
  names(vc_cols1) = c(
    'Frame_Shift_Del',
    'Missense_Mutation',
    'Nonsense_Mutation',
    'Multi_Hit',
    'Frame_Shift_Ins',
    'In_Frame_Ins',
    'Splice_Site',
    'In_Frame_Del'
  )
  
  output$SummaryPlot <- renderPlot({
    SelectedMAF <- SelectClinic()$maf
    #SelectedMAF=subsetMaf(maf = lmaf, tsb =SeleteClinicDat$Tumor_Sample_Barcode)
    plotmafSummary(maf = SelectedMAF, rmOutlier = TRUE, addStat = 'median', dashboard = TRUE, titvRaw = FALSE)
  })
  
  output$Oncoplot <- renderPlot({
    SelectedMAF <- SelectClinic()$maf
    SeleteClinicDat <- SelectClinic()$clinic
    genes <- MutatedGene()
    #SelectedMAF=subsetMaf(maf = lmaf, tsb =SeleteClinicDat$Tumor_Sample_Barcode)
    oncoplot(SelectedMAF,colors = vc_cols1,genes =genes ,annotationDat = SeleteClinicDat,
           clinicalFeatures = c("group",'gender','race','pathologic_stage','Smoke','vital_status'),
           sortByAnnotation=T,anno_height = 5)
  })
  
  output$somaticInter <- renderPlot({
    SelectedMAF <- SelectClinic()$maf
    somaticInteractions(maf = SelectedMAF, top = 25, pvalue = c(0.05, 0.1))
    })
  
  output$Lollipop <- renderPlot({
    SelectedMAF <- SelectClinic()$maf
    #SelectedMAF=subsetMaf(maf = lmaf, tsb =SeleteClinicDat$Tumor_Sample_Barcode)
    gene <- input$Geneforlollipop
    p=lollipopPlot(
      maf = SelectedMAF,
      gene = gene,
      AACol = 'HGVSp_Short',
      showMutationRate = TRUE
      #labelPos = 882
    )
    print(p)
  })
  
  ####compare panel
  
  
  comparevalues <- eventReactive(input$comparesubmit,{
    cohortA = input$cohortA
    tumorstageCohortA = input$tumorstageCohortA
    cohortB = input$cohortB
    tumorstageCohortB = input$tumorstageCohortB
    return(list(cohortA,tumorstageCohortA,cohortB,tumorstageCohortB))
  })
  comparevalues1 <- reactive({
    if(input$comparesubmit==0){
      cohortA ="Exhausted"
      tumorstageCohortA = c("Stage I","Stage IA")
      cohortB = "Rest"
      tumorstageCohortB = c("Stage I","Stage IA")
    }else{
      cohortA = comparevalues()[[1]]
      tumorstageCohortA = comparevalues()[[2]]
      cohortB = comparevalues()[[3]]
      tumorstageCohortB = comparevalues()[[4]]
    }
    return(list(cohortA=cohortA,tumorstageCohortA=tumorstageCohortA,cohortB=cohortB,tumorstageCohortB=tumorstageCohortB))
  })
  
  cohortAmaf <- reactive({
    cohortAlocbool = rep(TRUE,length(ClinicDat$Tumor_Sample_Barcode))
    if(!is.null(comparevalues1()$cohortA)){
      classAbool=c(ClinicDat$group == comparevalues1()$cohortA)
      cohortAlocbool = cohortAlocbool & classAbool
    }
    if(!is.null(comparevalues1()$tumorstageCohortA)){
      stageAbool = c(!is.na(match(ClinicDat$pathologic_stage,comparevalues1()$tumorstageCohortA)))
      cohortAlocbool = cohortAlocbool & stageAbool
    }
    cohortAloc = which(cohortAlocbool)
    
    ClinicDatCohortA = ClinicDat[cohortAloc,]
    
    SelectedMAF=subsetMaf(maf = lmaf, tsb =ClinicDatCohortA$Tumor_Sample_Barcode)
    list(maf=SelectedMAF,clinic=ClinicDatCohortA)
  })
  
  cohortBmaf <- reactive({
    cohortBlocbool = rep(TRUE,length(ClinicDat$Tumor_Sample_Barcode))
    if(!is.null(comparevalues1()$cohortB)){
      classBbool=c(ClinicDat$group == comparevalues1()$cohortB)
      cohortBlocbool = cohortBlocbool & classBbool
    }
    if(!is.null(comparevalues1()$tumorstageCohortB)){
      stageBbool = c(!is.na(match(ClinicDat$pathologic_stage,comparevalues1()$tumorstageCohortB)))
      cohortBlocbool = cohortBlocbool & stageBbool
    }
    cohortBloc = which(cohortBlocbool)
    
    ClinicDatCohortB = ClinicDat[cohortBloc,]
    
    SelectedMAF=subsetMaf(maf = lmaf, tsb =ClinicDatCohortB$Tumor_Sample_Barcode)
    list(maf=SelectedMAF,clinic=ClinicDatCohortB)
  })
  
  # print(cohortAmaf())
  # print(cohortBmaf())
  compareRes <- reactive({
                pt.vs.rt <- mafCompare(m1 = cohortAmaf()$maf, m2 = cohortBmaf()$maf, m1Name = 'Cohort A', m2Name = 'Cohort B', minMut = 5)
                        })
  
  output$forestPlot <- renderPlot({
        forestPlot(mafCompareRes = compareRes(), pVal = 0.05)
  })
  
  output$compareDT <- DT::renderDataTable({
    as.data.frame(compareRes()$results)
  },extensions = c('Buttons'),options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  
  ####Survival tabpanel
  survalues <- eventReactive(input$sursubmit,{
    cohortSur <- input$cohortSur
    tumorstageCohortSur <- input$tumorstageCohortSur
    gene <- input$SurvivalGene
    return(list(cohortSur=cohortSur,tumorstageCohortSur=tumorstageCohortSur,gene=gene))
  })
  survalues1 <- reactive({
    if(input$sursubmit == 0){
      cohortSur <- c("Exhausted","Rest")
      tumorstageCohortSur <- c("Stage I","Stage IA")
      gene <- "TP53"
    }else{
      cohortSur <- survalues()$cohortSur
      tumorstageCohortSur <- survalues()$tumorstageCohortSur
      gene <- survalues()$gene
    }
    return(list(cohortSur=cohortSur,tumorstageCohortSur=tumorstageCohortSur,gene=gene))
  })
  
  cohortSurDat <- reactive({
    cohortSurlocbool = rep(TRUE,length(ClinicDat$Tumor_Sample_Barcode))
    if(!is.null(survalues1()$cohortSur)){
      classSurbool=c(!is.na(match(ClinicDat$group,survalues1()$cohortSur)))
      cohortSurlocbool = cohortSurlocbool & classSurbool
    }
    if(!is.null(survalues1()$tumorstageCohortSur)){
      stageSurbool = c(!is.na(match(ClinicDat$pathologic_stage,survalues1()$tumorstageCohortSur)))
      cohortSurlocbool = cohortSurlocbool & stageSurbool
    }
    cohortSurloc = which(cohortSurlocbool)
    
    ClinicDatCohortSur = ClinicDat[cohortSurloc,]
    
    SelectedMAF=subsetMaf(maf = lmaf, tsb =ClinicDatCohortSur$Tumor_Sample_Barcode)
    list(maf=SelectedMAF,clinic=ClinicDatCohortSur)
  })
  
  output$SurPlot <- renderPlot({
    # if(input$SurvivalGene ==""){
    #   gene="TP53"
    # }else{
    #   gene <- input$SurvivalGene
    #   }
    gene = survalues1()$gene
    
    clinidat = cohortSurDat()$clinic
    clinidat$vital_status = c(clinidat$vital_status=="Dead")
    print("Gene symbol:")
    print(gene)
    mafSurvival(maf = cohortSurDat()$maf, clinicalData=clinidat,genes = gene, time = 'survi_all_time', Status = 'vital_status')
  })
  
    
  
}
