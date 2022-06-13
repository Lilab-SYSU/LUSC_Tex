mod_miRNA_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(width = 12,
        h2(p(strong("Explore microRNA related to immunotherapy resistance by correlating Exhausted Immune class(EIC)"),style="text-align:center")),
        br(),
        hr(),
        box(width=4,
            h2(p(strong("Volcano plot"),style="text-align:center")),
            withSpinner(plotlyOutput(ns('volcano')),image = "custom.gif")
        ),
        column(width = 3),
        box(width = 6,
            h2(p(strong('Differentially expressed microRNA between EIC and rest class'),style="text-align:center")),
            withSpinner(DT::dataTableOutput(ns('diffmiRNA')),image = "custom.gif"),
            #submitButton("Update tageted genes")
        )
      ),
    box(width = 12,
        h2(p(strong("The experimentally validated miRNA-target genes from miRTarBase"))),
        withSpinner(DT::dataTableOutput(ns('target')),image = "custom.gif")
        ),
    box(width = 12,
        h2(p(strong("Functional enrichment analysis"))),
    column(width = 3,
        selectInput(ns("genesets"),"Gene sets",selected = c('GOBP'),choices = c("Kyoto Encyclopedia of Genes and Genomes"="KEGG","Gene Ontology:Biology Process"="GOBP","Gene ontology:Cellular Component"="GOCC","Gene Ontology:Molecular Function"="GOMF")),
        ),
    column(width = 2,
           # submitButton("Update enrichment")
           )
    ),
    box(width = 12,
        withSpinner(DT::dataTableOutput(ns('enrichDf')),image = "custom.gif")
        )
  )
}

mod_miRNA_server <- function(input, output, session) {
  ns <- session$ns
  dat <- fread('data/miRNA/LUSC_miRNA_diffgenes.xls')
  output$diffmiRNA <- DT::renderDataTable({
    dat
  },selection=list(mode='single',selected=c(1)),extensions = c('Buttons'),options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
  )
  
  output$volcano <- renderPlotly({
    p=ggplot(dat,aes(x=logFC,y=-log(PValue,10)))+geom_point(aes(color=singnificant))+labs(x='log2(fold change)',y='-log10(Pvalue)',color='Significant')+theme(plot.title=element_text(hjust=.5))+geom_vline(xintercept=0,linetype="dashed")+geom_hline(yintercept = 1.3, linetype = 4) +
      geom_vline(xintercept = c(-1, 1), linetype = 4) + 
      # ylim(0, 15) +
      xlim(-5,9)+
      scale_color_manual(values = c("green","black","red"))+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))
    ggplotly(p)
  })
  
  ####microRNA tageted genes
  microRNAtaget <- fread('data/miRNA/hsa_MTI.txt')
  #print(head(microRNAtaget))
  miRNAtaget <- reactive({
    if(is.null(input$diffmiRNA_rows_selected)){
      rowid=1
    }else{rowid=input$diffmiRNA_rows_selected}
    miRNA=dat$miRNA[rowid]
    loc = which(microRNAtaget$miRNA==miRNA)
    microRNAtaget[loc,]
  })
  
  
  output$target <- DT::renderDataTable({
   miRNAtaget()
  },extensions = c('Buttons'),options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
  )
  
  output$enrichDf <- DT::renderDataTable({
    library(clusterProfiler)
    tagetgenes = unique(miRNAtaget()$`Target Gene`)
    TG = bitr(tagetgenes, fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Hs.eg.db")
    if(input$genesets == "KEGG"){
      ego <- enrichKEGG(gene         = TG$ENTREZID ,organism     = 'hsa',pvalueCutoff = 0.1)
    }else if(input$genesets == "GOBP"){
      ego <- enrichGO(gene=TG$ENTREZID,OrgDb = 'org.Hs.eg.db',ont= "BP",pAdjustMethod = "BH",pvalueCutoff  = 0.1,qvalueCutoff  = 1,readable=TRUE)
    }else if(input$genesets == "GOCC"){
      ego <- enrichGO(gene=TG$ENTREZID,OrgDb = 'org.Hs.eg.db',ont= "CC",pAdjustMethod = "BH",pvalueCutoff  = 0.1,qvalueCutoff  = 1,readable=TRUE)
    }else if(input$genesets =="GOMF"){
      ego <- enrichGO(gene=TG$ENTREZID,OrgDb = 'org.Hs.eg.db',ont= "MF",pAdjustMethod = "BH",pvalueCutoff  = 0.1,qvalueCutoff  = 1,readable=TRUE)
    }
    as.data.frame(ego)
    })

}