mod_expression_ui <- function(id) {
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(disable = T),
      dashboardSidebar(disable = T),
      skin = "black",
      dashboardBody(
    fluidRow(
      
      column(width=10,
             offset = 1,
             box(
               width = 12, background = "maroon",
               span(strong("Explore the expression alterations of the given gene between EIC and Rest class in lung squamous cell carcinoma"),
                    style = "font-size:24px;text-align:center"
                   ),
                ),
               box(
                 width = 12,
                 box(
                   h4(p(strong("Please enter a gene"))),
                   width = 2,
                   
                   #textInput(ns("genename"), "Gene symbol:", "PDCD1"),
                   selectizeInput(ns("genename"),"Gene symbol",choices =NULL),
                   #selectInput(ns("genename"),"Gene symbol",selected = "PDCD1",choices =c("e.g. PDCD1"="",c("PDCD1",genes))),
                   #uiOutput(ns("inputgene")),
                   actionButton(ns("exprsubmit"),"Submit")
                   # submitButton("Update View")
                 ),
                 column(width = 1),
                 box(
                   title ="Late stages(stage IIA to IV)", status = "warning",solidHeader = TRUE,
                   withSpinner(plotlyOutput(ns("LUSCboxplot")),image = "custom.gif"),
                   #style="border:1px solid black",
                   width = 4
                 ),
                 column(width = 1),
                 box(
                   title = "Early stages(stage I to II)", status = "primary",solidHeader = TRUE,
                   withSpinner(plotlyOutput(ns("LUSCboxplotEarly")),image = "custom.gif"),
                   #style="border:1px solid black",
                   width = 4
                 )
             #h2(p("Explore the expression alterations of the given gene between EIC and Rest class in lung squamous cell carcinoma.", style="background-color:	#B0C4DE; color:#000000; text-align:center; padding:10px;font-size:15px")
                ),
             box(width = 12,
               h2(p("KEGG pathway, GO function and target drug of the given gene.",
                    style="text-align:center"))
             ),
             box(width = 12,
               column(width = 1),
               column(width=10,
                      h3(p("KEGG pathway")),
                      hr(),
                      withSpinner(DT::dataTableOutput(ns('KEGG')),image = "custom.gif")
                      
               )
             ),
             box(width = 12,
               column(width = 1),
               column(width=10,
                      h3(p("GO function annotation")),
                      hr(),
                      withSpinner(DT::dataTableOutput(ns('GO')),image = "custom.gif")
                      
               )
             ),
             box(width = 12,
               #column(width = 1),
               #column(width=10,
                      h3(p("Drug target")),
                      withSpinner(DT::dataTableOutput(ns('Drug')),image = "custom.gif")
               #)
             )
           )
        ),

    br(),
    hr(),
    
    fluidRow(
      column(width = 10,
             offset=1,
             box(
               width = 12, background = "maroon",
               span(strong("Co-expression analysis between two interested genes."),
                    style = "font-size:24px"
               ),
             ),
             box(width = 12,
             box(width=2,
                    textInput(ns("geneA"),label = "GeneA",value = "CCL2"),
                    textInput(ns("geneB"),label = "GeneB",value = "CCR2"),
                    # submitButton("Update View"),
                    actionButton(ns("corsubmit"),"Submit")
                    #style="border:1px solid black",
             ),
             column(width = 1),
             box(width=4,
                 title = "Late stages(stage IIA to IV)",
                    withSpinner(plotOutput(ns("correlation")), image = "custom.gif"),
                    downloadButton(ns("downloadcorrelation"), "Download"),
                    #style="border:1px solid black"
             ),
             column(width = 1),
             box(width=4,
                 title = "Early stages(stage I to II)",
                    withSpinner(plotOutput(ns("correlationEarly")), image = "custom.gif"),
                    downloadButton(ns("downloadcorrelationEarly"), "Download"),
                    #style="border:1px solid black"
             ),
        
      )
      ),

    ),
    br(),
    br(),
  )
 )
)
}

##########server
#luscexpression <- fread('LUSC_Symbols_latestage.csv')
mod_expression_server <- function(input, output, session) {
  ns <- session$ns
  luscexpression <- fread('LUSC_Symbols_latestage.csv')
  genes <- luscexpression$Genes
  updateSelectizeInput(session, 'genename', selected=c("PDCD1"),choices = genes, server = TRUE)
  #luscexpression <- fread('LUSC_Symbols_latestage.csv')
  # output$inputgene<-renderUI({
  #   #textInput(ns("genename"), "Gene symbol:", "PDCD1")
  #   #luscexpression <- fread('LUSC_Symbols_latestage.csv')
  #   selectInput(ns("genename"),"Select only character variable",choices =c("PDCD1",luscexpression$Genes[1:5000]))
  #   
  # })
  # 
  # gene = reactive({input$genename})
  gene= eventReactive(input$exprsubmit, {
    input$genename
  })
  
  corgenesevent = eventReactive(input$corsubmit, {
    genea = input$geneA
    geneb = input$geneB
    return(list(genea,geneb))
  })
  
  corgenes <- reactive({
    if(input$corsubmit==0){
    GeneA = input$geneA
    GeneB = input$geneB
  }else{
    GeneA = corgenesevent()[[1]]
    GeneB = corgenesevent()[[2]]
  }
    return(list(GeneA,GeneB))
    })
  # 
  ######for late stages
  output$LUSCboxplot <- renderPlotly(
    {
      #luscexpression <- fread('LUSC_Symbols_latestage.csv')
      if(input$exprsubmit==0){
        gene <- "PDCD1"}else{
          gene <- gene()
        }
      loc <- match(gene,luscexpression$Genes)
      df <- as.data.frame(t(as.data.frame(luscexpression[loc,-1])))
      group <- fread('Group_Exh_Rest_late_stage.xls')
      
      df$V1 = log2(df$V1+1)
      colnames(df) = "FPKM"
      df <- data.frame(df,group)
      #print(df)
      my_comparisons <- list( c("Exhausted", "Rest"))
      a=wilcox.test(FPKM~class,data = df)
      p=ggplot(df, aes(x=class, y=FPKM,fill=class)) +labs(x="Class",y=paste0(gene," expression log2(FPKM+1)"))+ggtitle(paste0("Pvalue = ",a$p.value))+
        geom_boxplot() +scale_fill_d3()+geom_point(position=position_jitter(0.2))+theme(panel.grid.major = element_blank(),panel.background = element_blank(),panel.border = element_rect(colour="black", fill=NA))+
        geom_signif(comparisons = my_comparisons)
      
      ggplotly(p)
    }
  )
  
  output$correlation <- renderPlot({
    GeneA = corgenes()[[1]]
    GeneB = corgenes()[[2]]
    sampleNames <- colnames(luscexpression)[-1] 
    group <- fread('Group_Exh_Rest_late_stage.xls')
    
    loc <- match(sampleNames,group$samples)
    
    GeneAexpr <- as.numeric(luscexpression[luscexpression$Genes == GeneA,-1])
    GeneBexpr <- as.numeric(luscexpression[luscexpression$Genes == GeneB,-1])
    plotdf = data.frame(GeneA=GeneAexpr,GeneB=GeneBexpr,class=group$class[loc])
    
    p <- ggplot(data = plotdf, aes(x = log2(GeneA+1), y =log2(GeneB+1) )) + 
      geom_point(aes(color=plotdf$class,size=0.1)) + geom_smooth(method = 'lm')+
      scale_discrete_manual(values=c('red', 'gray'), aesthetics = 'colour')+
      stat_cor(method = 'pearson')+
      theme(panel.background = element_blank(),
            axis.text = element_text(size = 13),  
            panel.border = element_rect(colour="black", fill=NA, size = 1.3))+
      labs(x = paste0(GeneA," log2(FPKM+1)"),y= paste0(GeneB," log2(FPKM+1)"),color="group")
    p
  }
  )
  
  output$downloadcorrelation <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      GeneA = corgenes()[[1]]
      GeneB = corgenes()[[2]]
      
      sampleNames <- colnames(luscexpression)[-1] 
      group <- fread('Group_Exh_Rest_late_stage.xls')
      
      loc <- match(sampleNames,group$samples)
      
      GeneAexpr <- as.numeric(luscexpression[luscexpression$Genes == GeneA,-1])
      GeneBexpr <- as.numeric(luscexpression[luscexpression$Genes == GeneB,-1])
      plotdf = data.frame(GeneA=GeneAexpr,GeneB=GeneBexpr,class=group$class[loc])
      
      p <- ggplot(data = plotdf, aes(x = log2(GeneA+1), y =log2(GeneB+1) )) + 
        geom_point(aes(color=plotdf$class,size=0.1)) + geom_smooth(method = 'lm')+
        scale_discrete_manual(values=c('red', 'gray'), aesthetics = 'colour')+
        stat_cor(method = 'pearson')+
        theme(panel.background = element_blank(),
              axis.text = element_text(size = 13),  
              panel.border = element_rect(colour="black", fill=NA, size = 1.3))+
        labs(x = paste0(GeneA," log2(FPKM+1)"),y= paste0(GeneB," log2(FPKM+1)"))
      ggsave(p,filename = file)
    }
  )
  
  ######for early stage
  luscexpressionEarly <- fread('data/LUSC_Symbols_earlystage.csv')
  output$LUSCboxplotEarly <- renderPlotly(
    {
      #luscexpression <- fread('LUSC_Symbols_latestage.csv')
      if(input$exprsubmit==0){
        gene <- "PDCD1"}else{
          gene <- gene()
        }
      #luscexpressionEarly <- fread('data/LUSC_Symbols_earlystage.csv')
      #gene = "CCL2"
      loc <- match(gene,luscexpressionEarly$Genes)
      df <- as.data.frame(t(as.data.frame(luscexpressionEarly[loc,-1])))
      group <- fread('data/Group_Exh_Rest_early_stage.xls')
      
      df$V1 = log2(df$V1+1)
      colnames(df) = "FPKM"
      loc = match(rownames(df),group$samples)
      df <- data.frame(df,group[loc])
      #print(df)
      my_comparisons <- list( c("Exhausted", "Rest"))
      a=wilcox.test(FPKM~class,data = df)
      p=ggplot(df, aes(x=class, y=FPKM,fill=class)) +labs(x="Class",y=paste0(gene," expression log2(FPKM+1)"))+ggtitle(paste0("Pvalue = ",a$p.value))+
        geom_boxplot() +scale_fill_d3()+geom_point(position=position_jitter(0.2))+theme(panel.grid.major = element_blank(),panel.background = element_blank(),panel.border = element_rect(colour="black", fill=NA))+
        geom_signif(comparisons = my_comparisons)
      
      ggplotly(p)
    }
  )
  
  output$correlationEarly <- renderPlot({
    GeneA = corgenes()[[1]]
    GeneB = corgenes()[[2]]
    sampleNames <- colnames(luscexpressionEarly)[-1] 
    group <- fread('data/Group_Exh_Rest_early_stage.xls')
    
    loc <- match(sampleNames,group$samples)
    
    GeneAexpr <- as.numeric(luscexpressionEarly[luscexpressionEarly$Genes == GeneA,-1])
    GeneBexpr <- as.numeric(luscexpressionEarly[luscexpressionEarly$Genes == GeneB,-1])
    plotdf = data.frame(GeneA=GeneAexpr,GeneB=GeneBexpr,class=group$class[loc])
    
    p <- ggplot(data = plotdf, aes(x = log2(GeneA+1), y =log2(GeneB+1) )) + 
      geom_point(aes(color=plotdf$class,size=0.1)) + geom_smooth(method = 'lm')+
      scale_discrete_manual(values=c('red', 'gray'), aesthetics = 'colour')+
      stat_cor(method = 'pearson')+
      theme(panel.background = element_blank(),
            axis.text = element_text(size = 13),  
            panel.border = element_rect(colour="black", fill=NA, size = 1.3))+
      labs(x = paste0(GeneA," log2(FPKM+1)"),y= paste0(GeneB," log2(FPKM+1)"),color="group")
    p
  }
  )
  
  output$downloadcorrelationEarly <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      GeneA = corgenes()[[1]]
      GeneB = corgenes()[[2]]
      sampleNames <- colnames(luscexpressionEarly)[-1] 
      group <- fread('data/Group_Exh_Rest_early_stage.xls')
      
      loc <- match(sampleNames,group$samples)
      
      GeneAexpr <- as.numeric(luscexpressionEarly[luscexpressionEarly$Genes == GeneA,-1])
      GeneBexpr <- as.numeric(luscexpressionEarly[luscexpressionEarly$Genes == GeneB,-1])
      plotdf = data.frame(GeneA=GeneAexpr,GeneB=GeneBexpr,class=group$class[loc])
      
      p <- ggplot(data = plotdf, aes(x = log2(GeneA+1), y =log2(GeneB+1) )) + 
        geom_point(aes(color=plotdf$class,size=0.1)) + geom_smooth(method = 'lm')+
        scale_discrete_manual(values=c('red', 'gray'), aesthetics = 'colour')+
        stat_cor(method = 'pearson')+
        theme(panel.background = element_blank(),
              axis.text = element_text(size = 13),  
              panel.border = element_rect(colour="black", fill=NA, size = 1.3))+
        labs(x = paste0(GeneA," log2(FPKM+1)"),y= paste0(GeneB," log2(FPKM+1)"))
      ggsave(p,filename = file)
    }
  )
  
  ######KEGG GO DRUG target
  output$KEGG <- DT::renderDataTable(
    {
      if(gene()==""){
        gene <- "PDCD1"}else{
          gene <- gene()
        }
      KEGG <- fread('./data/KEGG2GeneSymbol2Pathway.xls')
      #loc <- match(gene,KEGG$GeneSymbol)
      KEGG[KEGG$GeneSymbol==gene,]
    }
  )
  
  output$GO <- DT::renderDataTable(
    {
      if(input$genename==""){
        gene <- "PDCD1"}else{
          gene <- input$genename
        }
      GO <- fread('./data/Human_GO1.csv')
      #loc <- match(gene,GO$GeneSymbol)
      GO[GO$GeneSymbol==gene,]
    }
  )
  
  output$Drug <- DT::renderDataTable(
    {
      if(input$genename==""){
        gene <- "PDCD1"}else{
          gene <- input$genename
          if(gene == "PDCD1"){
            gene <- c('PDCD1','PDCD1') 
          }
        }
      
      chembldb <- system.file("extdata", "chembl_sample.db", package="drugTargetInteractions")
      resultsPath <- system.file("extdata", "results", package="drugTargetInteractions")
      config <- genConfig(chemblDbPath=chembldb, resultsPath=resultsPath)
      uniprot <- fread('Uniprotid_to_genename.csv')
      loc = match(gene,uniprot$V3)
      uniprotID = uniprot$V1[loc]
      queryBy <- list(molType="protein", idType="UniProt_ID", ids=uniprotID)
      qresult2 <- drugTargetAnnot(queryBy, config=config)
      qresult2[,1:10]
    }
  )
  }
