mod_clinic_ui <- function(id){
  ns <- NS(id)
  tagList(
    # dashboardPage(
      # dashboardHeader(disable = T),
      # dashboardSidebar(disable = T),
      # #skin = "black",
      # dashboardBody(
      # 
    column( width=10,
              offset=1,
      box( width = 12,
        h1(p(strong("The correlation between gene expression and clinical information "), style="text-align:center"))
      )
    ),
    br(),
    hr(),
    column(width = 10,
           offset = 1,
        box(width=12,
               offset=1,
               box(width = 2,
                   radioButtons(ns("radio"), h3("Survival analysis by group:"),
                                c("Exhausted Immune class" = 1, 
                                  "Gene expression" = 2
                                )),
                   br(),
                   "Group by expression of input gene:",
                   #uiOutput(ns('geneInput')),
                   #uiOutput(ns("dim")),
                   textInput(ns("genenameSur"), "Gene symbol:", "CCL2"),
                   numericInput(ns("percent"),"High group cutoff top(%)",30),
                   # submitButton("Update View"),
                   actionButton(ns("clisubmit"),"Submit")
                   
               ),
               
               
               box(width=4,
                   plotOutput(ns('survival')),
                   downloadButton(ns("downloadsurvival"), "Download")
               ),
               column(width = 1),
               box(width = 5,
                   h2(strong('Age'),style="text-align:center"),
                   plotlyOutput(ns('Age')),
               ),
        )
    ),
    hr(),
    column(width = 10,
           offset = 1,
           box(width = 12,
             h1(p(strong("Gender,tumor stage distribution of high and low gene expression groups", style="text-align:center")))),

    ),
    column(width = 10,
             offset=1,
             box(width = 12,
                 column(width = 12,h2(p(strong("Gender"), style="text-align:center"))),
                 #h2(p("Gender", style="text-align:center")),
                 box(width = 5,
                     h2(p(textOutput(ns('titlehigh')), style="text-align:center")),
                     eChartOutput(ns('highgender')),
                     ),
                 column(width = 2),
                 box(width = 5,
                     h2(p(textOutput(ns('titlelow')), style="text-align:center")),
                     eChartOutput(ns('lowgender')),
                 )
             )
      ),
     column(width = 10,
            offset = 1,
            box(width = 12,
                column(width = 12,h2(p(strong("Tumor stages"), style="text-align:center"))),
                #h2(p("Clinical stages", style="text-align:center")),
                box(
                  h2(p(textOutput(ns('titlehigh1')), style="text-align:center")),
                  width=5,
                  eChartOutput(ns('highstage')),
                  #style="border:1px solid black"
                ),
                column(width = 2),
                box(
                  h2(p(textOutput(ns('titlelow1')), style="text-align:center")),
                  width=5,
                  eChartOutput(ns('lowstage')),
                  #style="border:1px solid black"
                  )
                )
            )
  # )
 #)
)
}


######server
mod_clinic_server <- function(input, output, session) {
  ns <- session$ns
  #clindat <- fread('./data/Clindat.xls')
  # observeEvent(input$obsEventButton, {
  #   print('test')
  #   'ABC'
  #   })
  # radion <- reactive({
  #   radio = input$radio
  #   radio
  # })
    # output$geneInput = output$dim<-renderUI({
    #   if(input$radio==2){
    #     textInput(ns("genenameSur"), "Gene symbol:", "CCL2")
    #   }
    # })
    # 
    # output$dim<-renderUI({
    #   if(input$radio==2){
    #   #textInput(ns("genenameSur"), "Gene symbol:", "CCL2")
    #   numericInput(ns("percent"),"High group cutoff top(%)",30)
    #   }
    # })
  values <- eventReactive(input$clisubmit,{
    gene= input$genenameSur
    print(gene)
    percent = input$percent
    print(percent)
    return(list(gene,percent))
  })
  
  values1 <- reactive({
    if(input$clisubmit == 0){
      gene = "CCL2"
      percent = 30
    }else{
      gene = values()[[1]]
      percent = values()[[2]]
    }
    return(list(gene,percent))
  })
  
   clin <- reactive({
     if(input$radio == 2){
       # if(input$clisubmit == 0){
       # percent = values1[[2]]/100
       # # print(percent)
       # gene = 
       # # if(input$genenameSur == ""){
       # #   gene = "CCL2"
       # # }else{
       # #   gene=input$genenameSur
       # # }
       # }else{ 
       #   gene = values()[[1]]
       #   percent = values()[[2]]
       #   }
       gene= values1()[[1]]
       percent = values1()[[2]]/100
       
       #gene= "CCL2"
       luscexpression <- fread('LUSC_Symbols_latestage.csv')
       loc <- match(gene,luscexpression$Genes)
       print(luscexpression)
       #order(as.numeric(luscexpression[loc,-1]))
       samples=colnames(luscexpression[loc,-1])[order(as.numeric(luscexpression[loc,-1]),decreasing = T)]
       totalnum =length(samples)
       print(percent)
       highnum = floor(percent*length(samples))
       print(totalnum)
       print(highnum)
       class = rep(c('High','Low'),c(highnum,totalnum-highnum))
       #class = paste0(gene,class)
       group = data.frame(samples,class)
       
       clindat <- fread('./data/Clindat.xls')
       loc1 = match(clindat$samples,group$samples)
       clindat$class = group$class[loc1]
       clindat}else{
      clindat <- fread('./data/Clindat.xls')
    }
  })
  
  
  output$survival <- renderPlot(
    { 
      clindat <- clin()
      gene=values1()[[1]]
      if(input$radio == 1){
        my.surv <- Surv(as.numeric(clindat$survi_all_time)/30,clindat$vital_status=='Dead')
        
        fit  <- do.call( survfit , list ( my.surv  ~  class , data  =  clindat ))
        p = ggsurvplot(fit,
                       pval=TRUE,
                       data = clindat,
                       risk.table=TRUE,
                       #legend.labs=c(paste0(gene," high"),paste0(gene," low")),
                       palette=c("dodgerblue2", "orchid2"),
                       xlab="Follow-up(months)",
                       ylab="Overall survival")
                       
      }else{
      my.surv <- Surv(as.numeric(clindat$survi_all_time)/30,clindat$vital_status=='Dead')

      fit  <- do.call( survfit , list ( my.surv  ~  class , data  =  clindat ))
      p = ggsurvplot(fit,
                     pval=TRUE,
                     data = clindat,
                     risk.table=TRUE,
                     legend.labs=c(paste0(gene," high"),paste0(gene," low")),
                     palette=c("dodgerblue2", "orchid2"),
                     xlab="Follow-up(months)",
                     ylab="Overall survival"
                     
      )
      }
      print(p)
      
    }
    
  )
  
  output$downloadsurvival <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      clindat <- clin()
      gene=values1[[1]]
      #print(head(clindat))
      my.surv <- Surv(as.numeric(clindat$survi_all_time)/30,clindat$vital_status=='Dead')
      #kmfit3 <- survfit(Surv(as.numeric(clindat$survi_all_time)/30,clindat$vital_status=='Dead')~clindat$class,data=clindat)
      # print(kmfit3)
      #plot(kmfit3)
      fit  <- do.call( survfit , list ( my.surv  ~  class , data  =  clindat ))
      print(fit)
      psur = ggsurvplot(fit,
                        pval=TRUE,
                        data = clindat,
                        risk.table=TRUE,
                        legend.labs=c(paste0(gene," high"),paste0(gene," low")),
                        palette=c("dodgerblue2", "orchid2"),
                        xlab="Follow-up(months)",
                        ylab="Overall survival"
                        
      )
      pdf(file)
      print(psur)
      dev.off()
    }
  )
  

  output$Age <- renderPlotly({
    clindat <- clin()
    plotdf = data.frame(class=clindat$class,Age=clindat$age_at_index)
    plotdf1 = plotdf[plotdf$Age != "'--",]
    plotdf1$Age = as.numeric(plotdf1$Age)
    
    p=ggplot(plotdf1,aes(x = class, y = Age,fill=class))+geom_boxplot()+scale_fill_d3()+geom_point(position=position_jitter(0.2))+theme(panel.grid.major = element_blank(),panel.background = element_blank(),panel.border = element_rect(colour="black", fill=NA))
    ggplotly(p)
  }
  )
  
  output$titlehigh = renderText({
    if(input$radio == 1){
      "Exhausted Immune class"
    }else{paste("High",values1()[[1]],"expression group",sep=" ")}
  })
  output$titlelow = renderText({
    if(input$radio == 1){
      "Rest class"
    }else{paste("Low",values1()[[1]],"expression group",sep=" ")}
  })
  
  output$titlehigh1 = renderText({
    if(input$radio == 1){
      "Exhausted Immune class"
    }else{paste("High",values1()[[1]],"expression group",sep=" ")}
  })
  output$titlelow1 = renderText({
    if(input$radio == 1){
      "Rest class"
    }else{paste("Low",values1()[[1]],"expression group",sep=" ")}
  })
  
  output$highgender = renderEChart({
    if(input$radio == 1){
       data = as.data.frame(table(clin()$gender[clin()$class=="Exhausted"]))
    }else{
      data = as.data.frame(table(clin()$gender[clin()$class=="High"]))
    }
    #datadf = 
    chart = echartr(data, Var1, Freq,type="pie")
    chart})
  output$lowgender = renderEChart({
    if(input$radio == 1){
      data = as.data.frame(table(clin()$gender[clin()$class=="Rest"]))
    }else{
      data = as.data.frame(table(clin()$gender[clin()$class=="Low"]))
    }
    #datadf = 
    chart = echartr(data, Var1, Freq,type="pie",)
    chart})
  
  output$highstage = renderEChart({
    if(input$radio == 1){
      data = as.data.frame(table(clin()$ajcc_pathologic_stage[clin()$class=="Exhausted"]))
    }else{
      data = as.data.frame(table(clin()$ajcc_pathologic_stage[clin()$class=="High"]))
    }#datadf = 
    chart = echartr(data, Var1, Freq,type="pie")
    chart})
  
  output$lowstage = renderEChart({
    if(input$radio == 1){
      data = as.data.frame(table(clin()$ajcc_pathologic_stage[clin()$class=="Rest"]))
    }else{
      data = as.data.frame(table(clin()$ajcc_pathologic_stage[clin()$class=="Low"]))
    }
    #datadf = 
    chart = echartr(data, Var1, Freq,type="pie")
    chart})
  
}