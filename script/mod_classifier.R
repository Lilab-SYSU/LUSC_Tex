mod_classifier_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width=2,
             textAreaInput(ns("caption"), "167 genes-based exhausted immune classifier", "DEFA5\nTAC1\nIRX1\nIL2RA\nITK\nCCL13\nTNFSF8\nPIK3CG\nMFAP4\nWDFY4\nVSIG1\nDOK5\nKRT7\nDMBT1\nCCL18\nFGF7\nWNT6\nITIH5\nCYBB\nTLR10\nTHBS1\nFBN1\nPEBP4\nPLN\nGZMK\nZNF385B\nCCR4\nCD163\nTMEM119\nTIMP3\nVGLL3\nCCR2\nMT1A\nITGBL1\nAOC3\nHMCN1\nLRRC15\nHLA-DOA\nFOLR1\nSFTPD\nCLIC5\nDDIT4L\nGPR39\nWBSCR17\nRETN\nCTSE\nSSC5D\nCXCR3\nRET\nSLC22A31\nSUSD2\nPAEP\nPRRX1\nRND1\nMRC1\nNFATC2\nAQP4\nF5\nMS4A15\nCOL8A1\nREM1\nTRPA1\nABI3BP\nLSAMP\nSPON1\nCCL7\nPLA2G1B\nC7\nATP10A\nIGLL1\nGREM1\nHLA-DQA2\nCADM3\nMYO1G\nPARM1\nPTGER2\nSLC16A4\nC16orf89\nSERPINA1\nCOMP\nTNFRSF9\nSFTA2\nABCA3\nIL21R\nPADI2\nMOXD1\nGFY\nCIITA\nCRTAC1\nCHRDL2\nPNMA2\nPCP4\nMAMDC2\nUBD\nC4BPA\nMSLN\nTGM2\nSFTPC\nCOL4A4\nARL14\nL1CAM\nLTB\nADRA2A\nCCBE1\nNAPSA\nADAM8\nBIRC3\nICAM1\nKCNA3\nCLDN18\nSHISA3\nHGF\nNR4A3\nNPTX1\nSELE\nFOXI1\nCYP1B1\nFN1\nDPP4\nKCNN4\nLRRN4\nKANK4\nLMO3\nCCL19\nSYNPO2\nMARCO\nSLCO2A1\nOLR1\nCSF2RB\nTFPI2\nADORA1\nNKX2-1\nCLEC1A\nSLC34A2\nSLC14A1\nTMEM52B\nSPOCK2\nNDNF\nMYBPC2\nANKRD1\nBMPER\nSFTA3\nVCAM1\nCILP\nHABP2\nAGTR2\nFGG\nKREMEN2\nADAMTS16\nCD70\nVSTM2L\nEXOC3L4\nANKRD33B\nMIA\nFABP7\nCAPN6\nKEL\nSSTR2\nSOX14\nLRRC55\nPAGE2\nFGB\nODAM\nMYCN\nSPIB\nCLDN6\nIL17REL
", width = "300px",height="500px")),
      column(width=2,
             fileInput(ns('file'),"Upload your expression matrix"),
             helpText("Note: columns correspond to samples, rows to genes."),
             # submitButton("Submit")
             actionButton(ns('submit'),strong("Submit"))
      ),
      column(width=8,
             h3(p("This is a example,you can use other samples for prediction",style="text-align:center")),
             hr(),
             downloadButton(ns("downloadHeatmap"), "Download"),
             withSpinner(plotOutput(ns("heatmapClass")), image = "custom.gif"),
             withSpinner(DT::dataTableOutput(ns('PredictGroup')),image = "custom.gif")
      )
      
    )
  )
}


mod_classifier_server <- function(input, output, session) {
  ns <- session$ns
  filedata <- reactive({
    library(data.table)
    # print(input$file)
    uploadf <- eventReactive(input$submit,{input$file})
    
    if(input$submit ==0){
      dat <- fread('sqcc_expr.csv')
      
    }else{
      print(uploadf())
      print(class(uploadf()))
      dat <- fread(uploadf()$datapath)
    }
    dat <- as.data.frame(dat)
    geneidfactor<-factor(dat[,1])
    gene_exp_matrix<-apply(dat[,-1],2,function(x) tapply(x,geneidfactor,mean))
    gene_exp_matrix
    
  })
  
  es <- reactive({
    gene_exp_matrix <- filedata()
    library(GSVA)
    gmt <- fread('Exhaustion_TEX_immune_cells_cytokine.xls',header=T)
    if(length(input$caption) > 0){
      IECgenes=strsplit(input$caption,"\n")[[1]]
      gmt$IEC = c(IECgenes,rep("",274-length(IECgenes)))
      print(strsplit(input$caption,"\n")[[1]])
    }
    
    gmt.list=as.list(gmt)
    for(i in 1:length(gmt.list)){
      gmt.list[[i]]=gmt.list[[i]][!gmt.list[[i]]==""]
    }
    es.dif <- gsva(gene_exp_matrix, gmt.list, mx.diff=TRUE, verbose=FALSE, parallel.sz=1,method="ssgsea")
    sigmoid = function(x,a=1){1/(1+exp(-a*x))}
    es.dif=t(scale(t(es.dif)))
    es.dif=sigmoid(es.dif)
    es.dif=as.data.frame(es.dif)
    es.dif1 = es.dif[,order(es.dif['IEC',])]
    es.dif1
    
  })
  
  output$heatmapClass <- renderPlot(
    {
      
      es.dif1 <- es()
      exhaustion = colnames(es.dif1)[es.dif1['IEC',] >0.654]
      rest = colnames(es.dif1)[es.dif1['IEC',] <=0.654]
      exhdf = data.frame(exhaustion,"Exhausted immune class")
      restdf = data.frame(rest,"Rest")
      colnames(exhdf) = c('samples','group')
      colnames(restdf) = c('samples','group')
      annodf = rbind(exhdf,restdf)
      library(ComplexHeatmap)
      library(circlize)
      f1 = colorRamp2(seq(min(es.dif1), max(es.dif1), length = 3), c("#dce6f1", "white","#ff0000"))
      order <- match(annodf$samples,colnames(es.dif1))
      es.diforder <- es.dif1[,order]
      ha=HeatmapAnnotation(NMF=as.character(annodf$group),annotation_name_side = "left")
      Heatmap(es.diforder,border=T,col =f1,height = unit(4*nrow(es.diforder), "mm"),show_column_names=T,top_annotation =ha,cluster_rows = FALSE,cluster_columns=FALSE,row_names_side = "left",name="immue")
    }
  )
  
  output$downloadHeatmap <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      es.dif1 <- es()
      exhaustion = colnames(es.dif1)[es.dif1['IEC',] >0.654]
      rest = colnames(es.dif1)[es.dif1['IEC',] <=0.654]
      exhdf = data.frame(exhaustion,"Exhausted immune class")
      restdf = data.frame(rest,"Rest")
      colnames(exhdf) = c('samples','group')
      colnames(restdf) = c('samples','group')
      annodf = rbind(exhdf,restdf)
      library(ComplexHeatmap)
      library(circlize)
      f1 = colorRamp2(seq(min(es.dif1), max(es.dif1), length = 3), c("#dce6f1", "white","#ff0000"))
      order <- match(annodf$samples,colnames(es.dif1))
      es.diforder <- es.dif1[,order]
      
      ha=HeatmapAnnotation(NMF=as.character(annodf$group),annotation_name_side = "left")
      p=Heatmap(es.diforder,border=T,col =f1,height = unit(4*nrow(es.diforder), "mm"),show_column_names=T,top_annotation =ha,cluster_rows = FALSE,cluster_columns=FALSE,row_names_side = "left",name="immue")
      pdf(file,width = 0.22*ncol(es.diforder))
      # Heatmap(es.diforder,border=T,col =f1,height = unit(4*nrow(es.diforder), "mm"),show_column_names=T,top_annotation =ha,cluster_rows = FALSE,cluster_columns=FALSE,row_names_side = "left",name="immue")
      draw(p)
      dev.off()
    }
  )
  output$PredictGroup <- DT::renderDataTable({
    es.dif1 <- es()
    exhaustion = colnames(es.dif1)[es.dif1['IEC',] >0.654]
    rest = colnames(es.dif1)[es.dif1['IEC',] <=0.654]
    exhdf = data.frame(exhaustion,"Exhausted immune class")
    restdf = data.frame(rest,"Rest")
    colnames(exhdf) = c('samples','group')
    colnames(restdf) = c('samples','group')
    annodf = rbind(exhdf,restdf)
    annodf
  },extensions = c('Buttons', 'Scroller'),
  options = list(
    dom = 'Bfrtip',
    deferRender = TRUE,
    scrollY = 400,
    scroller = TRUE,
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  )
  )
  
  
}
