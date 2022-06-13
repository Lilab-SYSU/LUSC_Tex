# homePage <- dashboardPage(
  # dashboardHeader(disable = T),
  # dashboardSidebar(disable = T),
  #skin = "black",
  # dashboardBody(
homePage<-    box(width = 12,
    fluidRow(column(width=1),
             column(
               #box(width=12,
                   h1(p(strong("Explore potential mechanism of immunotherapy resistance in LUSC "),style="text-align:center;color:#1A98BC")),
                  # ),
              h3(p(strong("Althrough immunotherapy has revolutionized the treatment of lung squamous carcinoma, a significant proportion of patients which had high PDL1 expression showed resistance to immnotherapy. 
                           Based on gene expression profiles, we used virtual microdissection method to deconvolute the expression patterns and identify an immunosuppressive subtype which showed potential resistance to immune checkpoint blockade therapy. Then we defined this subtype as an Exhausted Immune class and developed an Exhausted Immune classifier to predict patients belonging to this class")),
                   strong("Then we also constructed this database web app for clinical researchers to explore the mechanism of potential immunotherapy resistance at the multiomics level."),
                   style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px"),
               width=10)
               ),
    br(),
    fluidRow(column(width=1),
             column(
               h3(p(" This application consists of seven functional modules including signature expression, exhausted immune classifier, somatic mutation, microRNA, methylation, clinic and chemotherapy drugs. The dedailed usage of each module was described as below.", 
                    style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px")),
               width=10
             )
    ),
    hr(),
    fluidRow(align="center",tags$img(src="introduction1.png",width="1200px",height="900px")),
    hr(),
    h2(p(strong('Signature expression'),style="text-align:center; padding:10px;font-size:30px")),
    
    h2(p('In this module,user can perfomed the co-expression analysis of given gene between exhausted immune class and rest class of LUSC cohort. The KEGG pathways, GO function and target drug of this given gene was listed in table. And user can investage expression correlation of two interested genes',style="text-align:center; padding:10px;font-size:30px")),
    br(),
    h2(p(strong('Exhausted immune classifier'),style="text-align:center; padding:10px;font-size:30px")),
    
    h2(p('User can upload a lung squamous carcinoma expression matrix to predict exhausted immumne class with potential resistance to immunotherapy.',style="text-align:center; padding:10px;font-size:30px")),
    br(),
    
    h2(p(strong('Somatic mutation'),style="text-align:center; padding:10px;font-size:30px")),
    
    h2(p('In this module,we developed 3 functional panels.Landscape of mutations panel enables user to explore overall mutation landscape of LUSC cohorts selected by tumor stages or exhausted immune class and to check  amino acid changes information of individual gene among the cohort.Comparative analysis panel can make user to compare two cohorts to detect differentially mutated genes. Survival analysis panel enables user to check whether the mutation of given gene is associated with prognosis.',style="text-align:center; padding:10px;font-size:30px")),
    br(),
    h2(p(strong('MicroRNA'),style="text-align:center; padding:10px;font-size:30px")),
    
    h2(p('User can browse differentially expressed microRNA between exhausted immune class and rest class, and the targeted genes of microRNA. And user can also perform correlaiton analysis of microRNA and target gene and functional enrichment analysis of the targeted genes.',style="text-align:center; padding:10px;font-size:30px")),
    br(),
    
    h2(p(strong('Methylation'),style="text-align:center; padding:10px;font-size:30px")),
    
    h2(p('All differentially methylated CpGs between exhausted immune class and rest class can be queried by user. And the correlation between CpG methylation value and expression of corresponding gene can be performed.',style="text-align:center; padding:10px;font-size:30px")),
    br(),
    
    h2(p(strong('Clinic'),style="text-align:center; padding:10px;font-size:30px")),
    
    h2(p('User can check association of exhausted immune class with the prognosis, gender, age, and clinical stage of LUSC patients, and can also explore whether the expression of given gene is associated with these clinical variables.',style="text-align:center; padding:10px;font-size:30px")),
    br(),
    h2(p(strong('Chemotherapy drugs'),style="text-align:center; padding:10px;font-size:30px")),
    
    h2(p('User can predict chemotherapy sensitivity of patients based on expression profiles.',style="text-align:center; padding:10px;font-size:30px")),
    br(),
    
    
    # box(width = 12,background = "black",
    # p(em("Contact:Minglei Yang"),br("Email:yangmlei3@mail2.sysu.edu.cn"),br(tags$a(href="http://lilab2.sysu.edu.cn/", h2("Li Lab @Sun Yat-sen University"))))
    # )
  )
 # )
# )
