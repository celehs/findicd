library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(wordcloud2)
library(viridis)
library(stringr)
Sys.setenv(LANG="en_US.UTF-8")
load("cutdata2.RData")

jscode <- '
$(function() {
  var $els = $("[data-proxy-click]");
  $.each(
    $els,
    function(idx, el) {
      var $el = $(el);
      var $proxy = $("#" + $el.data("proxyClick"));
      $el.keydown(function (e) {
        if (e.keyCode == 13) {
          $proxy.click();
        }
      });
    }
  );
});
'

ui <- dashboardPage(
  dashboardHeader(title = "FindICD",
                  disable = FALSE
  ),
  
  dashboardSidebar(
    width = "200pt",
    disable = TRUE
    
  ),
  
  dashboardBody(
    fluidRow(column(4),
             column(8,align="center",
                    textOutput("wordtitle",container = h2))),
    
    fluidRow(
      column(4,
             fluidRow(column(8,
                             tagAppendAttributes(
                               textInput("str", label="Specify the disease condition:", value = "cold"),
                               `data-proxy-click` = "goButton")),
                      column(4,
                             br(),
                             tags$head(tags$script(HTML(jscode))),
                             actionButton("goButton", "Search", 
                                          icon = tags$i(class = "fas fa-search",
                                                        style="font-size: 10px"), 
                                          class = "btn-primary"))),
             fluidRow(column(8,
                             checkboxInput("excludeind", "Exclude strings?", FALSE),
                             conditionalPanel(condition =  "input.excludeind==1",
                                              textInput("exclude", label="String that should be filtered out: (split by ;)", value = "")
                             ),
             )),
             
             fluidRow(column(12,DT::dataTableOutput("table"))),
      ),
      
      # column(width = 6,
      #        conditionalPanel(condition =  "input.table_rows_selected.length>=1",
      #                         box(width = NULL,
      #                             title = "Wordcloud",
      #                             wordcloud2Output("wordcloud_1"),
      #                             
      #                             hr(),
      #                             h4("Download the word embeddings of the most n related ICD(s):"),
      #                             fluidRow( 
      #                               column(6,selectInput("related",label="",
      #                                                    choices = paste0("n=",c(1:20)),
      #                                                    width = "50%")),
      #                               column(6, h3(""),downloadButton("downloadData", "Download as .csv"))),
      #                             
      #                             
      #                             
      #                             
      #                         )
      #        )
      #        
      #        
      # ),
      column(width=8,
             box(width = NULL,
                 title = "Matched info",
                 DT::dataTableOutput("table3"),
                 downloadButton("downloadData_2", "Download as .csv")
                 
             )
             
             
      )
    )
  )
)












server <- function(input, output, session) {
  output$table <- DT::renderDataTable(DT::datatable({
    input$goButton
    isolate({
      string = input$str
      if(string!="" & (!string %in% mapping_s$Phecode_String) &
         (!string %in% mapping_s$icd_string) &
         (!string %in% cui_str_s_2$str)){
        b = cui_str_s_2$str[str_detect(cui_str_s_2$str,string)]
        b.part = b[str_detect(b,paste0("^",string))]
        b = c(b.part, setdiff(b,b.part))
        if(length(b)!=0){
          data.frame("possible string"= b)
        }
      }
    })
  }, rownames = FALSE,caption="No exact match. Do you mean:",
  selection = 'single',
  extensions = c('Buttons', 'Scroller'),
  options = list(scrollY = 300,
                 scrollX = 250,
                 deferRender = TRUE,
                 scroller = TRUE,
                 dom = c("t")
                 # paging = TRUE,
                 # pageLength = 25,
  )),server = TRUE)
  
  output$wordtitle <- renderText({
    input$goButton
    string = isolate(input$str)
    if(string!=""){
      if(string!="" & (!string %in% mapping_s$Phecode_String) &
         (!string %in% mapping_s$icd_string) &
         (!string %in% cui_str_s_2$str)){
        
      }else{
        string
      }
    }else{
      paste0("Try to specify the disease condition.") 
    }
    
  })
  
  observe({
    string = input$str
    s = input$table_rows_selected

      if(string!="" & (!string %in% mapping_s$Phecode_String) &
         (!string %in% mapping_s$icd_string) &
         (!string %in% cui_str_s_2$str)){
        b = cui_str_s_2$str[str_detect(cui_str_s_2$str,string)]
        b.part = b[str_detect(b,paste0("^",string))]
        b = c(b.part, setdiff(b,b.part))
        bb = rank(b)
        b =  cui_str_s_2[str_detect(cui_str_s_2$str,string),]
        b = b[bb,]
        if(nrow(b)!=0){
          s = input$table_rows_selected
          if(length(s)>0){
            updateTextInput(session, "str", value = b$str[s])
          }
        }
      }

  })
  

  output$table3 <- DT::renderDataTable(DT::datatable({
    input$goButton
    string = isolate(input$str)
    ex = isolate(input$exclude)
    if(string!=""){
      if((!string %in% mapping_s$Phecode_String) &
         (!string %in% mapping_s$icd_string) &
         (!string %in% cui_str_s_2$str)){
        
      }else{
        if((string %in% mapping_s$Phecode_String) |
           (string %in% mapping_s$icd_string)){
          target_row_id = unique(c(which(mapping_s$Phecode_String==string),
                                   which(mapping_s$icd_string==string)))
          selected_s = mapping_s[target_row_id,]
        }else{
          s = which(cui_str_s_2$str == string)
          target_row_id = cui_str_s_2$loc.map[s]
          target_row_id = as.numeric(unlist(str_split(target_row_id,",")))
          selected_s = mapping_s[target_row_id,]
        }
        if(ex!=''&input$excludeind==TRUE){
          ex.vec = paste0("(",str_replace(ex,";",")|("),")")
          selected_s = selected_s[!str_detect(selected_s$icd_string,ex)&
                                    !str_detect(selected_s$Phecode_String,ex.vec),]
        }
        if(nrow(selected_s)>0){
          data.frame("ICD code" = selected_s$ICD_CODE,
                     "ICD string" = selected_s$icd_string,
                     "Phe code" = selected_s$Phecode,
                     "Phe string" = selected_s$Phecode_String)
        }else{
          data.frame("info" = paste0("After filtering out strings ",ex," no
                                       matched ICD is found."))
        }
      }
    }
  }, rownames = FALSE,options = list(
    pageLength = 7
  ),selection = 'single'))
  
  
  
  
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste("word_embedding.csv")
  #   },
  #   content = function(path) {
  #     input$goButton
  #     string = isolate(input$str)
  #     if(string!=""){
  #       b = cui_str_s_2$str[str_detect(cui_str_s_2$str,string)]
  #       b.part = b[str_detect(b,paste0("^",string))]
  #       b = c(b.part, setdiff(b,b.part))
  #       bb = rank(b)
  #       b =  cui_str_s_2[str_detect(cui_str_s_2$str,string),]
  #       b = b[bb,]
  #       s = input$table_rows_selected
  #       if(length(s)>0){
  #         target_emb_row_id = b$loc.emb[s]
  #         if(target_emb_row_id!="NA"){
  #           target_emb_row_id = as.numeric(target_emb_row_id)
  #           cui_str_s = b
  #           selected_cosine = cosine_matrix_top20[target_emb_row_id,]
  #           
  #           n = as.numeric(str_replace(input$related,"n=","")) + 1
  #           selected_str = selected_cosine[22+c(1:n)]
  #           
  #           selected_str_id = sapply(selected_str, function(x){
  #             if(sum(emb$terms == x)==0){
  #               return(NA)
  #             }else{
  #               return(which(emb$terms == x))
  #             }
  #           })
  #           selected_emb = emb[selected_str_id,]
  #           selected_emb = cbind(selected_cosine[1+c(1:n)],selected_emb)
  #           colnames(selected_emb)[1] = paste0("cosine to '",cui_str_s$str[s],"'") 
  #           write.csv(selected_emb,path,row.names = FALSE)
  #         }else{
  #           write.csv(data.frame("info"="No related ICD is found!"),path,row.names = FALSE)
  #         }
  #         
  #       }else{
  #         write.csv(data.frame("info"="You haven't selected an ICD string!"),path,row.names = FALSE)
  #       }
  #     }
  #   }
  # )
  # 
  
  output$downloadData_2 <- downloadHandler(
    filename = function() {
      paste("mapping_info.csv")
    },
    content = function(path) {
      input$goButton
      string = isolate(input$str)
      ex = isolate(input$exclude)
      if(string!=""){
        if((!string %in% mapping_s$Phecode_String) &
           (!string %in% mapping_s$icd_string) &
           (!string %in% cui_str_s_2$str)){
            write.csv(data.frame("info"="No mapping ICD code/Phe code is found."),path,row.names = FALSE)
        }else{
          if((string %in% mapping_s$Phecode_String) |
             (string %in% mapping_s$icd_string)){
            target_row_id = unique(c(which(mapping_s$Phecode_String==string),
                                     which(mapping_s$icd_string==string)))
          }else{
            s = which(cui_str_s_2$str == string)
            target_row_id = cui_str_s_2$loc.map[s]
            target_row_id = as.numeric(unlist(str_split(target_row_id,",")))
          }
          selected_s = mapping_s[target_row_id,]
          if(ex!=''&input$excludeind==TRUE){
            ex.vec = paste0("(",str_replace(ex,";",")|("),")")
            selected_s = selected_s[!str_detect(selected_s$icd_string,ex)&
                                      !str_detect(selected_s$Phecode_String,ex.vec),]
          }
          file = data.frame("ICD code" = selected_s$ICD_CODE,
                            "ICD string" = selected_s$icd_string,
                            "UMLS ICD CUI" = selected_s$UMLS_ICDSTR_CUI,
                            "NER ICD CUI" = selected_s$ICD_CUI,
                            "Phe code" = selected_s$Phecode,
                            "Phe string" = selected_s$Phecode_String,
                            "UMLS Phe CUI" = selected_s$UMLS_PHESTR_CUI,
                            "NER Phe CUI" = selected_s$PHECODE_CUI)
          write.csv(file,path,row.names = FALSE) 
        }
      }else{
        write.csv(data.frame("info"="You haven't input a string!"),path,row.names = FALSE)
      }
    }
  )
  
  # 
  # output$wordcloud_1 <- renderWordcloud2({
  #   input$goButton
  #   string = isolate(input$str)
  #   if(string!=""){
  #     b = cui_str_s_2$str[str_detect(cui_str_s_2$str,string)]
  #     b.part = b[str_detect(b,paste0("^",string))]
  #     b = c(b.part, setdiff(b,b.part))
  #     bb = rank(b)
  #     b =  cui_str_s_2[str_detect(cui_str_s_2$str,string),]
  #     b = b[bb,]
  #     s = input$table_rows_selected
  #     if(length(s)>0){
  #       target_emb_row_id = b$loc.emb[s]
  #       if(target_emb_row_id!="NA"){
  #         target_emb_row_id = as.numeric(target_emb_row_id)
  #         selected_cosine = cosine_matrix_top20[target_emb_row_id,]
  #         top.n = 1:20
  #         wordcloud2(data.frame(word = selected_cosine[2+top.n+20],freq = round(abs(as.numeric(selected_cosine[1+top.n])),3)),
  #                    color  = viridis(length(top.n)),shuffle = FALSE,
  #                    size = .2, minRotation = -pi/4, maxRotation = pi/4,)
  #       }else{
  #         wordcloud2(data.frame(word = c("no related ICD is found","Based on the embedding file,"),freq = c(1,1)),
  #                    color  = c('black','black'),shuffle = FALSE,size=.2,
  #                    rotateRatio = 0)
  #       }
  #     }
  #   }
  # }
  # )
  
}



shinyApp(ui = ui, server = server)