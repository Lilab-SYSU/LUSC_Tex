mod_methy_ui <- function(id) {
  ns <- NS(id)
  # tabPanel(icon=icon("people-arrows"),
  #          "Methylation",
  tagList(
    box(width = 12,
        h2(strong("All differentially methylated CpG sites between exhausted immune class and rest class")),
        withSpinner(DT::dataTableOutput(ns('diffmethy')),image = "custom.gif"),
        # submitButton("Update correlation")
         # actionButton(ns("submit"), "Get row")
        
        ),
    column(width = 4),
    box(width = 4,
        #verbatimTextOutput(ns('devel')))
        title = "Correlation between selected CpG and correponding gene",
        withSpinner(plotOutput(ns('corPlot')),image = "custom.gif")
       )
    )
  # )
}

mod_methy_server <- function(input, output, session) {
  ns <- session$ns
  dat <- fread('data/All_DMP_ExhVsRest_V1.csv')
  
  output$diffmethy <- DT::renderDataTable({
    
    dat
  },selection=list(mode='single',selected=c(1)),extensions = c('Buttons'),options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
  )
  
  # rowid1 <- eventReactive(input$submit,{
  #   print(input$diffmethy_rows_selected)
  #   input$diffmethy_rows_selected})
  # 
  output$corPlot <- renderPlot({
    #req(length(input$fileList_rows_selected) > 0)
    #print(input$diffmethy_rows_selected)
    CpGvalue <- fread('data/methylation/DMP_corresponding_beta_match_value.csv')
    luscexpression <- fread('LUSC_Symbols_latestage.csv')
    group <- fread('data/methylation/late_group_Exh_Rest_file_matched.csv')
    locexpr <- match(group$samples,colnames(luscexpression))
    luscexpressionMethy = luscexpression[,..locexpr]
    # dim(luscexpressionMethy)
    #luscexpression[1:5,1:5]
    
    rowid=input$diffmethy_rows_selected
    # rowid = rowid1()
    # print(rowid)
    if(is.null(rowid)){
      print(rowid)
      rowid=1
    }
    
    # dat[rowid,]
    CG = dat$CG[rowid]
    CGloc = which(CpGvalue$CG==CG)
    CGvaldf = as.data.frame(t(CpGvalue[CGloc,-1]))
    colnames(CGvaldf) = 'CG'
      
    gene = dat$gene[rowid]
    geneloc = which(luscexpression$Genes==gene)
    exprdf = as.data.frame(t(luscexpressionMethy[geneloc,]))
    colnames(exprdf) = 'gene'
    plotdf = cbind(CGvaldf,exprdf,group=group$class)
    
    p <- ggplot(data = plotdf, aes(x = CG, y =log2(gene+1) )) + 
      geom_point(aes(color=plotdf$group,size=0.1)) + geom_smooth(method = 'lm')+
      scale_discrete_manual(values=c('red', 'gray'), aesthetics = 'colour')+
      stat_cor(method = 'pearson')+
      theme(panel.background = element_blank(),
            axis.text = element_text(size = 13),  
            panel.border = element_rect(colour="black", fill=NA, size = 1.3))+
      labs(x = paste0(CG," beta value"),y= paste0(gene," log2(FPKM+1)"),color="group")
    p
    
  })
}