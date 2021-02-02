library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(wordcloud2)
library(viridis)
library(stringr)
Sys.setenv(LANG="en_US.UTF-8")
load("data/fulldata.RData")


ui <- dashboardPage(
  dashboardHeader(title = "FindICD",
                  disable = FALSE
  ),

  dashboardSidebar(
    width = "200pt",
    disable = TRUE
    
  ),
  
  dashboardBody(
    conditionalPanel(condition = "input.table_rows_selected.length>=1",
                      fluidRow(column(12,align="center",
                                      textOutput("wordtitle",container = h2)))),

    fluidRow(
      column(2),
      column(3,     
             dropdown(
               awesomeRadio(
                 inputId = "method",
                 label = "Mapping method", 
                 choices = c("UMLS", "UMLS + NER"),
                 selected = "UMLS",
                 inline = TRUE, 
                 status = "success"),
               
               hr(),
               h4("Possible strings"),
               br(),
               DT::dataTableOutput("table"),
               style = "unite", icon = icon("table"),
               status = "primary", width = "350px",right = TRUE,
               tooltip = "Try to click one row \n as your cui string input!",
               
               animate = animateOptions(
                 enter = animations$fading_entrances$fadeInDown,
                 exit = animations$fading_exits$fadeOutUp,
                 duration = 0
               )
             )),
      column(4),
      column(3,dropdown(
        
        h4("Do you know?"),
        br(),
        htmlOutput("doyouknow"),
        hr(),
        DT::dataTableOutput("tabledoyouknow"),
        style = "unite", icon = icon("exclamation-circle"),
        status = "primary", width = "350px",right=TRUE,
        
        animate = animateOptions(
          enter = animations$fading_entrances$fadeInDown,
          exit = animations$fading_exits$fadeOutUp,
          duration = 0
        )
      ))),
      fluidRow(
        column(width = 6,
               conditionalPanel(condition =  "input.table_rows_selected.length>=1",
                                box(width = NULL,
                                    title = "Wordcloud",
                                    wordcloud2Output("wordcloud_1"),

                                    hr(),
                                    h4("Download the word embeddings of the most n related ICD(s):"),
                                    fluidRow( 
                                      column(6,selectInput("related",label="",
                                                           choices = paste0("n=",c(1:20)),
                                                           width = "50%")),
                                      column(6, h3(""),downloadButton("downloadData", "Download as .csv"))),
                                    
                                    
                                    
                                
                                )
               )
       
    
      ),
      column(width=6,
             conditionalPanel(condition = "input.table_rows_selected.length>=1",
                              box(width = NULL,
                                  title = "Matched info",
                                  DT::dataTableOutput("table3"),
                                  downloadButton("downloadData_2", "Download as .csv")
                                 
                              ))

             
      )
      )
   )
  )












server <- function(input, output, session) {
  output$doyouknow <- renderUI({
    s = input$table_rows_selected
    if(length(s)>0){
      if(input$method == "UMLS"){
        otherstr_cui = cui_dict_1[[cui_str_s_1$loc.dict[s]]]
        if(length(otherstr_cui)>1){
            HTML(paste0("The following ",length(otherstr_cui)-1, " strings share the same CUI (",cui_str_s_1$cui[s],
                 ") as <b>", cui_str_s_1$str[s],"</b>: <br/>"))
        }else{
          HTML(paste0("No string shares the same CUI (",cui_str_s_1$cui[s],
                 ") as the one you selected - \n ", cui_str_s_1$str[s])) 
        }
      }else{
        otherstr_cui = cui_dict_2[[cui_str_s_2$loc.dict[s]]]
        if(length(otherstr_cui)>1){
          otherstr_cui = setdiff(otherstr_cui,cui_str_s_2$str[s])
          otherstr_cui_str = paste(otherstr_cui, collapse = "<br/>")
          HTML(paste0("The following ",length(otherstr_cui)-1, " strings share the same CUI (",cui_str_s_2$cui[s],
                      ") as <b>", cui_str_s_2$str[s],"</b>: <br/>"))
        }else{
          HTML(paste0("No string shares the same CUI (",cui_str_s_2$cui[s],
                      ") as the one you selected - \n ", cui_str_s_2$str[s])) 
        }
      }

    }else{
     
      
    }
   
    
  })
  
  
  
  output$wordtitle <- renderText({
    s = input$table_rows_selected
    if(length(s)>0){
      if(input$method == "UMLS"){
        cui_str_s_1$str[s]
      }else{
        cui_str_s_2$str[s]
      }
    }else{
      paste0("Try to click a row in the table button.") 
    }
  })
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    if(input$method == "UMLS"){
      data.frame("Possible str"=cui_str_s_1$str)
    }else{
      data.frame("Possible str"=cui_str_s_2$str)
    }
   }, rownames = FALSE,options = list(
    pageLength = 7
      ),selection = 'single'),server = TRUE)
  
  output$tabledoyouknow <- DT::renderDataTable(DT::datatable({
    s = input$table_rows_selected
    if(length(s)>0){
      if(input$method == "UMLS"){
        otherstr_cui = cui_dict_1[[cui_str_s_1$loc.dict[s]]]
        if(length(otherstr_cui)>1){
          otherstr_cui = setdiff(otherstr_cui,cui_str_s_1$str[s])
        }
      }else{
        otherstr_cui = cui_dict_2[[cui_str_s_2$loc.dict[s]]]
        if(length(otherstr_cui)>1){
          otherstr_cui = setdiff(otherstr_cui,cui_str_s_2$str[s])
        }
      }
      data.frame("string"=otherstr_cui, "cui"=cui_str_s_1$cui[s])
    }else{
      
      
    }
    
  }, rownames = FALSE,options = list(
    pageLength = 7
  ),selection = 'single'),server = TRUE)
  
  
  
  output$table3 <- DT::renderDataTable(DT::datatable({
    s = input$table_rows_selected
    if(length(s)>0){
      if(input$method == "UMLS"){
        target_row_id = cui_str_s_1$loc.map[input$table_rows_selected]
        target_row_id = as.numeric(unlist(str_split(target_row_id,",")))
      }else{
        target_row_id = cui_str_s_2$loc.map[input$table_rows_selected]
        target_row_id = as.numeric(unlist(str_split(target_row_id,",")))
      }
      selected_s = mapping_s[target_row_id,]
      data.frame("ICD code" = selected_s$ICD_CODE,
                 "ICD string" = selected_s$icd_string,
                 "Phe code" = selected_s$Phecode,
                 "Phe string" = selected_s$Phecode_String)
    }
   
  }, rownames = FALSE,options = list(
    pageLength = 7
  ),selection = 'single'))
  


  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("word_embedding.csv")
    },
    content = function(path) {
      s = input$table_rows_selected
      if(length(s)>0){
        if(input$method=="UMLS"){
          target_emb_row_id = cui_str_s_1$loc.emb[s]
          target_emb_row_id = as.numeric(target_emb_row_id)
          cui_str_s = cui_str_s_1
        }else{
          target_emb_row_id = cui_str_s_2$loc.emb[s]
          target_emb_row_id = as.numeric(target_emb_row_id)
          cui_str_s = cui_str_s_2
        }
        if(is.na(target_emb_row_id)==FALSE){
          selected_cosine = cosine_matrix_top100[target_emb_row_id,]
          
          n = as.numeric(str_replace(input$related,"n=","")) + 1
          selected_str = selected_cosine[101+c(1:n)]
          
          selected_str_id = sapply(selected_str, function(x){
            if(sum(emb$terms == x)==0){
              return(NA)
            }else{
              return(which(emb$terms == x))
            }
          })
          selected_emb = emb[selected_str_id,]
          selected_emb = cbind(selected_cosine[1+c(1:n)],selected_emb)
          colnames(selected_emb)[1] = paste0("cosine to '",cui_str_s$str[s],"'") 
          write.csv(selected_emb,path,row.names = FALSE)
        }else{
          write.csv(data.frame("info"="No related ICD is found!"),path,row.names = FALSE)
        }

      }else{
        write.csv(data.frame("info"="You haven't selected an ICD string!"),path,row.names = FALSE)
      }
      
    }
  )
  
  
  output$downloadData_2 <- downloadHandler(
    filename = function() {
      paste("mapping_info.csv")
    },
    content = function(path) {
      s = input$table_rows_selected
      if(length(s)>0){
        if(input$method == "UMLS"){
          target_row_id = cui_str_s_1$loc.map[s]
        }else{
          target_row_id = cui_str_s_2$loc.map[s]
        }
        if(target_row_id!=""){
          target_row_id = as.numeric(unlist(str_split(target_row_id,",")))
          selected_s = mapping_s[target_row_id,]
          if(input$method == "UMLS"){
            file = data.frame("ICD code" = selected_s$ICD_CODE,
                              "ICD string" = selected_s$icd_string,
                              "UMLS ICD CUI" = selected_s$UMLS_ICDSTR_CUI,
                              "Phe code" = selected_s$Phecode,
                              "Phe string" = selected_s$Phecode_String,
                              "UMLS Phe CUI" = selected_s$UMLS_PHESTR_CUI)
          }else{
            file = data.frame("ICD code" = selected_s$ICD_CODE,
                              "ICD string" = selected_s$icd_string,
                              "UMLS ICD CUI" = selected_s$UMLS_ICDSTR_CUI,
                              "NER ICD CUI" = selected_s$ICD_CUI,
                              "Phe code" = selected_s$Phecode,
                              "Phe string" = selected_s$Phecode_String,
                              "UMLS Phe CUI" = selected_s$UMLS_PHESTR_CUI,
                              "NER Phe CUI" = selected_s$PHECODE_CUI)
          }
        
          write.csv(file,path,row.names = FALSE) 
        }else{
          write.csv(data.frame("info"="No mapping ICD code/Phe code is found."),path,row.names = FALSE)
        }
        
      }else{
        write.csv(data.frame("info"="You haven't selected an ICD string!"),path,row.names = FALSE)
        
      }
    }
  )
    
  
  output$wordcloud_1 <- renderWordcloud2({
    s = input$table_rows_selected
    if(length(s)>0){
      if(input$method=="UMLS"){
        target_emb_row_id = cui_str_s_1$loc.emb[s]
        target_emb_row_id = as.numeric(target_emb_row_id)
      }else{
        target_emb_row_id = cui_str_s_2$loc.emb[s]
        target_emb_row_id = as.numeric(target_emb_row_id)
      }
      selected_cosine = cosine_matrix_top100[target_emb_row_id,]
      top.n = 1:50
      if(is.na(target_emb_row_id)==FALSE){
        wordcloud2(data.frame(word = selected_cosine[2+top.n+100],freq = round(abs(as.numeric(selected_cosine[2+top.n])),3)),
                   color  = viridis(length(top.n)),shuffle = FALSE,
                   size = .2, minRotation = -pi/4, maxRotation = pi/4,)
      }else{
        wordcloud2(data.frame(word = c("no related ICD is found","Based on the embedding file,"),freq = c(1,1)),
                   color  = c('black','black'),shuffle = FALSE,size=.2,
                   rotateRatio = 0)
      }
      
    }else{

    }
  }
  )
  
}



shinyApp(ui = ui, server = server)
