mod_drug_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 12,
        h2(p(strong("Prediction of chemocherapy sensitive using 'pRRophetic' R package"),style="text-align:center;color:black")),
        box(width = 3,
        fileInput(ns('drugfile'),"Upload your expression matrix"),
        helpText("Note: columns correspond to samples, rows to genes."),
        selectInput(ns("Drugtype"),"Drug name",selected = c('Docetaxel'),choices = c('A.443654', 'A.770041', 'ABT.263', 'ABT.888','AG.014699', 'AICAR', 'AKT.inhibitor.VIII', 'AMG.706', 'AP.24534','AS601245', 'ATRA', 'AUY922', 'Axitinib', 'AZ628', 'AZD.0530', 'AZD.2281','AZD6244', 'AZD6482', 'AZD7762', 'AZD8055', 'BAY.61.3606', 'Bexarotene','BI.2536', 'BIBW2992', 'Bicalutamide', 'BI.D1870', 'BIRB.0796','Bleomycin', 'BMS.509744', 'BMS.536924', 'BMS.708163', 'BMS.754807','Bortezomib', 'Bosutinib', 'Bryostatin.1', 'BX.795', 'Camptothecin','CCT007093', 'CCT018159', 'CEP.701', 'CGP.082996', 'CGP.60474','CHIR.99021', 'CI.1040', 'Cisplatin', 'CMK', 'Cyclopamine', 'Cytarabine','Dasatinib', 'DMOG', 'Docetaxel', 'Doxorubicin', 'EHT.1864','Elesclomol', 'Embelin', 'Epothilone.B', 'Erlotinib', 'Etoposide','FH535', 'FTI.277', 'GDC.0449', 'GDC0941', 'Gefitinib', 'Gemcitabine','GNF.2', 'GSK269962A', 'GSK.650394', 'GW.441756', 'GW843682X','Imatinib', 'IPA.3', 'JNJ.26854165', 'JNK.9L', 'JNK.Inhibitor.VIII','JW.7.52.1', 'KIN001.135', 'KU.55933', 'Lapatinib', 'Lenalidomide','LFM.A13', 'Metformin', 'Methotrexate', 'MG.132', 'Midostaurin','Mitomycin.C', 'MK.2206', 'MS.275', 'Nilotinib', 'NSC.87877', 'NU.7441','Nutlin.3a', 'NVP.BEZ235', 'NVP.TAE684', 'Obatoclax.Mesylate','OSI.906', 'PAC.1', 'Paclitaxel', 'Parthenolide', 'Pazopanib','PD.0325901', 'PD.0332991', 'PD.173074', 'PF.02341066', 'PF.4708671','PF.562271', 'PHA.665752', 'PLX4720', 'Pyrimethamine', 'QS11','Rapamycin', 'RDEA119', 'RO.3306', 'Roscovitine', 'Salubrinal')
                    ),
        
        selectInput(ns("tissueType"),"Tissue type",selected = c('lung'),choices = c("blood", "breast", "CNS", "GItract" ,"lung", "skin", "upper aerodigestive")),
        # submitButton("Update View")
        actionButton(ns('drugsubmit'),strong("Submit"))
        ),
        box(width = "5",
            h2(p("This is an example: 61 LUSC patients from GSE30219")),
            withSpinner(DT::dataTableOutput(ns('drugPredict')),image = "custom.gif"),
            helpText("Note: The lower the IC50, the more sensitive they are to the drug."),
          
        ),
        
      ),
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
  )
}

#### server drug prediction
mod_drug_server <- function(input, output, session) {
  ns <- session$ns
  
  submitfile <- eventReactive(input$drugsubmit,{
    if(is.null(input$drugfile)){
      return('sqcc_expr.csv')
    }else{
      return(input$drugfile$datapath)
      }
  })
  submitVal <- eventReactive(input$drugsubmit,{
          # file= input$drugfile
          drug <- input$Drugtype
          tissue <- input$tissueType
          return(list(drug,tissue))
    })
  
  filedata <- reactive({
    if(input$drugsubmit ==0){
      dat <- fread('sqcc_expr.csv')
      print('First initiall')
    }else{
    dat <- fread(submitfile())
    print('upload file!!!!!!!!!!')
    }
    
    # if(input$drugsubmit ==0){
    #   dat <- fread('sqcc_expr.csv')
    # }else{
    #   dat <- fread(submitVal()[[1]]$datapath)
    # }
    dat <- as.data.frame(dat)
    geneidfactor<-factor(dat[,1])
    gene_exp_matrix<-apply(dat[,-1],2,function(x) tapply(x,geneidfactor,mean))
    gene_exp_matrix
  })
  
  output$drugPredict <- DT::renderDataTable({
    if(input$drugsubmit ==0){
      drug <- input$Drugtype
      tissue <- input$tissueType
    }else{
      drug <- submitVal()[[1]]
      tissue <- submitVal()[[2]]
    }
    dat <- filedata()
    datMat <- as.matrix(dat)
    datMatlog <- log2(datMat+1)

    predictRes =  pRRopheticPredict(datMatlog,drug=drug,tissueType=tissue,selection = 1)
    predictDf = as.data.frame(predictRes)
    predictDf = cbind(Samples=rownames(predictDf),predictDf)
    colnames(predictDf) = c('Samples','IC50')
    
    predictDf[order(predictDf$IC50),]
    
  })
  
}
  
  