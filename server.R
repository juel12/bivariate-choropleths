library(readxl) #data import
library(xlsx) #export data; Java muss installiert sein
#Sys.setenv(JAVA_HOME="C:\Program Files (x86)\Java\jre-1.8")
library(eurostat) #eurostat data
library(rvest) #scrape web pages

#library(RColorBrewer) #color
library(Hmisc)
library(stringr)
library(dplyr) #filter, mutate, reshape
library(reshape2) #melt
'%!in%' <- function(x,y)!('%in%'(x,y))

#library(sf)
library(sp) #shapefile
library(ggplot2) #visualization
#System ->Advanced --> change Path for Rtools, e.g. C:\Rtools40\usr\bin depending on version
library(leaflet) #visualization/ map
library(leafem)
library(mapview)  #save plot
#install.packages('terra', repos='https://rspatial.r-universe.dev')
library(terra)
library(webshot)
#webshot::install_phantomjs()


library(data.table)#auswahl für visualisierung eurostat data
library(DT) #auswahl für visualisierung eurostat data

library(shiny)
library(shinyjs) #enable & disable/ show & hide
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

library(shinydashboard)
library(dashboardthemes)
#install_github("nik01010/dashboardthemes")
#library(shinyWidgets)
options(shiny.fullstacktrace = TRUE) #, shiny.error = browser

#https://cran.r-project.org/web/packages/pals/vignettes/bivariate_choropleths.html
################################################

shinyServer(function(input,output,session){
  
###################  
#Select input type/ Upload data
output$data_input2 <- renderUI({
  if((is.null(input$data_input1))|| (input$data_input1=="Choose")){NULL}
  else{
      if(input$data_input1 == "Upload own dataset")
        {fileInput("file1", "Please select an excel dataset (.xlsx or .xls)",
                    accept = c(
                   #"text/csv",
                   #"text/comma-separated-values,text/plain",
                   #".csv",
                   ".xlsx",
                   ".xls"))
          }else{
          textInput("file2", "Please enter a code corresponding to a Eurostat data", 
                    value = "") #hlth_silc_18
          }
      }
})


observe({if(dataset_names$df_data==1 || length(dataset())>0){
  hide("data_input1")
}
})

observe({if(dataset_names$df_data==1){
  hide("file2")
}
})
###############################
#map
selected_map<- reactive({
  if(is.null(input$data_input1)||input$data_input1=="Choose") 
  {NULL}
  else{
    if(input$data_input1=="Eurostat data"){
      selected_map<- geojsonio::geojson_read("EU.json", what = "sp")
      selected_map
    }
    else{
      if(input$data_input1=="Upload own dataset" & 
         input$maptype=="Germany and federal states"){
        selected_map<-readRDS("r_sp1.rds") 
        names(selected_map)[names(selected_map)=="NAME_1"] <- "iso_a2" 
        selected_map
      }else{
        selected_map<- geojsonio::geojson_read("EU.json", what = "sp")
        selected_map
      }
    }
  }
})
#####################
#search for eurostat data
output$text<-renderUI({  if((is.null(input$data_input1))|| 
                            (input$data_input1=="Choose") || 
                            (input$data_input1=="Upload own dataset"))
{NULL}
 else{
textInput("textsearch", "Search for eurostat data", value = "")
 } 
})

observe({if(searchgo$afa==1 & dataset_names$df_data==1){
      hide("textsearch")
      }
})

observe({
  if((is.null(input$textsearch) || input$textsearch == "") & searchgo$afa==0){
    disable("search2")}
  else{
    if(searchgo$afa==1 & dataset_names$df_data==1){
     hide("search2")
    }else{
      enable("search2")
      }
    }
})


output$searchbutton2<-renderUI({
  if((is.null(input$data_input1))|| (input$data_input1=="Choose") || 
     (input$data_input1=="Upload own dataset"))
  {NULL}
  else{actionButton(inputId = "search2", label = "Search", icon = icon("search")) 
  }
})

searchgo2<-reactiveValues(afa=0) 
observeEvent(input$search2,{searchgo2$afa<-1})

datatable0<-reactive({if (searchgo2$afa==0)
{NULL}
  else{
    d<-input$textsearch
    search_eurostat(d)
  }
}) 

#display datasets that match the search term
output$eurostatdata <- renderDT(
  datatable0(), # reactive data
  class = "display nowrap compact", # style
  options = list(
    scrollX = TRUE, #scroll wide tables horizontally
    search = list(search = "")
  ))

################################################################################
#spezielles Dataset suchen
observe({
  if(is.null(input$file2) || input$file2 == ""){
    disable("search1")
  }
  else{if(dataset_names$df_data==1)
  {hide("search1")}
    else{
    enable("search1")
    }
  }
})
observe({if(dataset_names$df_data==1){
  hide("file2")
}
})

output$searchbutton<-renderUI({
  if((is.null(input$data_input1))|| (input$data_input1=="Choose") || 
     (input$data_input1=="Upload own dataset"))
    {NULL}
  else{actionButton(inputId = "search1", label = "Enter code", icon = icon("search")) #
    }
})

searchgo<-reactiveValues(afa=0) 
observeEvent(input$search1,{searchgo$afa<-1})

observe({
  if(searchgo$afa==1)
  {shinyjs::hide("eurostatdata")}
  else{shinyjs::show("eurostatdata")}
})

#https://stackoverflow.com/questions/46891095/disable-action-button-when-textinput-is-empty-in-shiny-app-r
################################################################################
#Check if eurostat id/ dataset exists
alldatasetnames<-as.data.frame(search_eurostat(pattern=""))

ID<-reactive({input$search1
  isolate({
    if(is.null(input$file2) || input$file2 == ""){NULL}
    else{
      ID<-as.character(input$file2)
      ID
    }
  })
})

dataset_names <- reactiveValues(df_data = 0)
observeEvent(input$search1, {
  if(is.null(ID())||((ID() %in% alldatasetnames$code)== FALSE) ||
     (length(datatable1())>0 | length(datatable2())>0))
  {dataset_names$df_data<-0}
  #if((ID() %in% alldatasetnames$code)== TRUE)
  else{dataset_names$df_data<-1}
})


#load eurostat dataset 
dataset0 <- reactiveValues(df_data = NULL)
observeEvent(input$search1,{
  if(dataset_names$df_data==0){NULL}
  else{
  ID <- ID()    #as.character(input$file2)
  dataset0$df_data <- get_eurostat(ID, time_format = "num") 
  }
  })


description<-reactive({if (is.null(dataset0$df_data)||
                           (input$data_input1=="Upload own dataset")||
                           (input$data_input1=="Choose"))
{NULL}
  else{
    ID <- ID()
    toc <- get_eurostat_toc()
    allmatches<-toc[which(toc$code==ID),]
    allmatches[1,]
  }
}) 

#display dataset description
output$description1 <- renderTable(
  description(), # reactive data
  bordered =T,
  spacing = "s"
  )

################################################################################
#eurostat & own dataset
dataset <- reactive({
  if(is.null(input$data_input1) || input$data_input1=="Choose"){NULL}
    else{
    if((input$data_input1 == "Eurostat data") & (is.null(dataset0$df_data)))
      {NULL}
    else{
      if(input$data_input1 == "Eurostat data"){
        dataset<-dataset0$df_data
        dataset$time<-as.factor(dataset$time) # level in DT
        dataset
        }
      else{
        if((input$data_input1 == "Upload own dataset") & is.null(input$file1))
        {NULL}
         else{dataset <- as.data.frame(read_excel(input$file1$datapath)) 
              dataset
           }
        }
      }
    }
})

################################################################################
#render User interface (own dataset)

output$maptype1 <- renderUI({
  if (is.null(dataset())||(input$data_input1=="Eurostat data") ) 
    return(NULL)
  else{
  selectInput("maptype", "Please choose a map", 
              choices = c("Choose", "Germany and federal states", 
                          "European countries"))
      }
})
 
output$Vgl1.1 <- renderUI({
  if (is.null(dataset()) || (input$data_input1=="Eurostat data") || 
      (input$data_input1=="Choose"))
    return(NULL)
  else
   selectInput("Vgl1.2", "Please choose the 1st variable for the plot", 
       choices = c("Choose Variable 1", 
       names(dataset())))
})
 
output$Vgl2.1 <- renderUI({
   if (is.null(dataset()) || (input$data_input1=="Eurostat data") || 
       (input$data_input1=="Choose"))
     return(NULL)
   else
     selectInput("Vgl2.2", "Please choose the 2nd variable for the plot", 
                 choices = c("Choose Variable 2", 
       names(dataset())))
            # [, -which(names(dataset()) == input$Vgl1.2)])))
})

output$Jahr1 <- renderUI({
  if (is.null(dataset())||(input$data_input1=="Eurostat data") )
    return(NULL)
  else 
      selectInput("Jahr2", "Please choose a time variable (e.g. year, date) 
                  or leave the default 'None'", choices = c("None", 
                  names(dataset())))
                #[, -which(names(dataset()) 
                # %in% c(input$Vgl1.2, input$Vgl2.2))])))
  })
observe({
    if(input$Jahr4 == "Choose"|| is.null(input$Jahr4)){
    enable("Jahr2")}
    else{disable("Jahr2")}
})


output$Region1 <- renderUI({
  if (is.null(dataset())||(input$data_input1=="Eurostat data") || 
      (input$data_input1=="Choose"))
    return(NULL)
  else 
    selectInput("Region2", "Please choose a variable containing geographic areas 
              (e.g. countries, states)", 
                choices = c("Choose", names(dataset())))
               #[, -which(names(dataset()) 
               # %in% c(input$Jahr2, input$Vgl1.2, input$Vgl2.2))])))
})

hideregion<-reactiveValues(afa=0)
observeEvent(input$go1,{
  if(input$Region2=="Choose"){hideregion$afa<-0}
  else{hideregion$afa<-1}
  })

observe({
  if(hideregion$afa==1)
  {shinyjs::disable("Region2")}
  else{shinyjs::enable("Region2")}
})

output$normalize <- renderUI({
  if ((input$data_input1=="Eurostat data") || (input$data_input1=="Choose") 
      || is.null(dataset()))
    return(NULL)
  else
    selectInput("population", (HTML(paste("Please choose a variable containing population 
                sizes/ densities for each area. <br/> Why?", a(href="https://xkcd.com/1138/", "Click here"), sep=" "))), 
                choices = c("Choose", 
                            names(dataset())))
                #[, -which(names(dataset())
            # %in% c(input$Vgl1.2, input$Vgl2.2, input$Jahr2, input$Region2))])))
})

################################################################################
#UI wenn eurostat

output$Vgl3.1 <- renderUI({
  if (is.null(dataset()) || 
      is.null(dataset0$df_data) || 
      (input$data_input1 == "Upload own dataset"))
    return(NULL)
  else
    selectInput("Vgl3.2", "Please choose the 1st variable for the plot", 
                choices = c("Choose Variable 1", 
                            names(dataset()[, -which(names(dataset()) 
                                 %in% c("geo", "time", "values"))])))
})

output$Vgl4.1 <- renderUI({
  if (is.null(dataset()) || 
      is.null(dataset0$df_data) || 
      (input$data_input1 == "Upload own dataset"))
    return(NULL)
  else
    selectInput("Vgl4.2", "Please choose the 2nd variable for the plot", 
                choices = c("Choose Variable 2", 
                            names(dataset()[, -which(names(dataset()) 
                                 %in% c("geo", "time", "values"))])))
                            #names(dataset())))
})
observe({
  if(input$data_input1=="Eurostat data" & 
     (is.null(datatable2()) & 
      is.null(datatable1())) )
  {shinyjs::show("testtab2")}
  else{shinyjs::hide("testtab2")}
})

#Table mit selected dataset
output$testtab2 <- renderDT(
  dataset(), # reactive data
  server = TRUE, #server-side processing
  class = "display nowrap compact", # style
  #filter = "top", # location of column filters
  #editable = TRUE,
  options = list(
    scrollX = TRUE, #allow user to scroll wide tables horizontally
    pageLength = 5,
    lengthMenu = c(5, 10, 15, 20)
  ))
################################################################################
#sort columns -> inactive filters in dt
datasetb<- reactive({
  if (is.null(input$data_input1) || 
      is.null(dataset0$df_data) || 
      (is.null(dataset()))||
      (is.null(input$Vgl3.2)) ||
      (is.null(input$Vgl4.2))||
      input$Vgl3.2 == "Choose Variable 1"||
      input$Vgl4.2 == "Choose Variable 2" ||
      input$data_input1=="Choose"||
      input$data_input1 == "Upload own dataset"){NULL}
  else{
    if(input$Vgl3.2==input$Vgl4.2)
    {dataset<-dataset()
    X1<-input$Vgl3.2
    dataset<- dataset%>% #reorder position ->in render DT no search 1,2
      select("geo", "values", X1, everything()) 
    dataset  
    }
    else{
    dataset<-dataset()
    X1<-input$Vgl3.2
    X2<-input$Vgl4.2
    dataset<- dataset%>% #reorder position ->in render DT no search 1,2
      select("geo", "values", X1, X2, everything()) 
    dataset
    }
  }
})
################################################################################
#eurostat: Datatable & Filter

#wenn Vgl1.2 != Vgl2.2
datatable1<-reactive({if (is.null(dataset())|| 
                          is.null(dataset0$df_data) || 
                          (is.null(datasetb())) ||
                          (is.null(input$Vgl3.2)) || 
                          (is.null(input$Vgl4.2)) || 
                          (input$Vgl3.2 ==input$Vgl4.2) ||
                          (input$Vgl3.2 == "Choose Variable 1") ||
                          (input$Vgl4.2 == "Choose Variable 2")||
                          (input$data_input1=="Upload own dataset"))
{NULL}
  else{
    datasetb()
  } 
})

output$tbl2 <- renderDT(
  datatable1(), # reactive data
  selection="none", #no row filters
  #selection=list(mode="single", target="cell"),
  server = TRUE, #server-side processing
  class = "display nowrap compact", # style
  filter = "top", # location of column filters
  options = list(#stateSave = TRUE,
    autoWidth = F,#TRUE,
    #width = '500px', 
    columnDefs = list(
      list(targets=c(1, 2, 3, 4), 
           searchable = FALSE)
    ),
    scrollX = TRUE#,  allow user to scroll wide tables horizontally
    #search = list(search = "", regex = FALSE, caseInsensitive = TRUE)
  ))

#wenn Vgl1.2 == Vgl2.2
datatable2<-reactive({if (is.null(dataset())|| 
                          is.null(dataset0$df_data) || 
                          (is.null(datasetb())) ||
                          (is.null(input$Vgl3.2)) || 
                          (is.null(input$Vgl4.2)) || 
                          (input$Vgl3.2 == "Choose Variable 1") ||
                          (input$Vgl4.2 == "Choose Variable 2")||
                          (input$Vgl3.2!=input$Vgl4.2) ||
                          (input$data_input1=="Upload own dataset"))
{NULL}
  else{
    datasetb()
  } 
})

output$tbl <- renderDT(
  datatable2(), # reactive data
  selection="none", #no row filters
  #selection=list(mode="single", target="cell"),
  server = TRUE, #server-side processing
  class = "display nowrap compact", # style
  filter = "top", # location of column filters
  options = list(#stateSave = TRUE,
    autoWidth = F,#TRUE,
    #width = '500px', 
    columnDefs = list(
      list(targets=c(1, 2, 3), 
           searchable = FALSE)
    ),
    scrollX = TRUE#,  allow user to scroll wide tables horizontally
    #search = list(search = "", regex = FALSE, caseInsensitive = TRUE)
  ))

output$instructions<-renderUI({
  if ((is.null(datatable2()) & is.null(datatable1())) ||
      (applyfilter$afa==3))
    return(NULL)
  else{HTML(paste("Please filter all possible columns by selecting ONE term per 
                  column and click apply filters.", "For time, any chosen year 
                  has to be between 1990 and 2016.",sep = "<br/>"))}
})

output$filter1 <- renderUI({
  if ((is.null(datatable2()) & is.null(datatable1())) ||
      applyfilter$afa==3)
    return(NULL)
  else{actionButton("filter2", "Apply filters")}
}) 

observe({
  if(input$data_input1=="Eurostat data" & 
     (is.null(datatable2()) & is.null(datatable1())
     ))
  {shinyjs::disable("filter2")}
  else{shinyjs::enable("filter2")}
})
 
#github.com/rstudio/DT/issues/76  --> input$tbl_state
#https://rstudio.github.io/DT/shiny.html

rowfilter<-reactive({if(is.null(datatable1()) & is.null(datatable2()))
{NULL}
  else{
    if(is.null(datatable1())){
    rowfilter<-input$tbl_rows_all
    rowfilter}
    else{
      rowfilter<-input$tbl2_rows_all
      rowfilter
    }
  }
    
})
################################################################################
#own dataset & eurostat
applyfilter<-reactiveValues(afa=0) 
observeEvent(input$filter2,{
  if(applyfilter$afa==1 || applyfilter$afa==2 ||applyfilter$afa==3){NULL}
  else{
    if(applyfilter$afa==0 & length(rowfilter())>0)
    {applyfilter$afa<-1}
    else{applyfilter$afa<-0}
  }
}) 

dataset1<-reactive({
  if(is.null(dataset())|| is.null(input$data_input1)
     ||(input$data_input1=="Choose")){NULL}
  else{
    if(input$data_input1=="Upload own dataset")
    {dataset1<-as.data.frame(dataset())
    dataset1}
    else{
      if(input$data_input1=="Eurostat data" & 
         (is.null(rowfilter())|| length(rowfilter()) ==0 || 
          is.null(dataset0$df_data))
          )
         {NULL}
         else{
      if((applyfilter$afa==0) 
         || (length(rowfilter()) < 1)
         || (length(rowfilter()) >= nrow(dataset())))
      {NULL}
      else{
        if((length(rowfilter()) > 0)
           && (length(rowfilter() < nrow(dataset())))){
          row_filter<-rowfilter()
          dataset<-as.data.frame(dataset())
          dataset1<-dataset[row_filter, ] 
          colnames(dataset1)[which(names(dataset1) == "geo")] <- "iso_a2"
          dataset1
        }else{
          if((length(rowfilter())==0)){
            dataset1<-as.data.frame(dataset())
            colnames(dataset1)[which(names(dataset1) == "geo")] <- "iso_a2"
            dataset1
          }else{message("Fehler6")}
        }
      }
    }
    }
  }
})


#check if time is valid --> population density available for 1990:2016
time<-reactive({
  if(is.null(dataset1())){NULL}
  else{
    time<-dataset1()$time
    time<-time[1]
    time
      }
})

observeEvent(input$filter2,{
  if(is.null(time())|| is.null(dataset1())|| 
     applyfilter$afa ==0 || applyfilter$afa ==2 || applyfilter$afa ==3)
    {NULL}
  else{
    if(applyfilter$afa ==1 & time() %in% c(1990:2016))
    {applyfilter$afa<-2}
    else{applyfilter$afa<-0}
  }
})

#check if multiple levels were selected
correctfilter<-reactive({
  if (is.null(dataset1())) {NULL}
      else{
        if(applyfilter$afa ==2){
                  dataset<-dataset1()[,which(colnames(dataset1()) %!in% c("iso_a2", "values", input$Vgl3.2, input$Vgl4.2))]
                  correctfilter<-dataset[1, sapply(dataset, function(col) length(unique(col))) > 1]
                  correctfilter<-dim(correctfilter)[2]
                  correctfilter}
          else{NULL}
        }
})
            
observeEvent(input$filter2,{
  if (is.null(dataset1()) || is.null(correctfilter())){NULL}
   else{
     if(applyfilter$afa ==2 & correctfilter()>0) {applyfilter$afa <-0}
    else{
      if(applyfilter$afa ==2 & correctfilter()==0){applyfilter$afa <-3}
      else{NULL}
        }
      }
})

#observe({message(head(dataset1()))})
#observe({message(applyfilter$afa)})
################################################################################
observe({
  if(((applyfilter$afa==3)&&(length(rowfilter())>0)))
  {shinyjs::hide("tbl")}
  else{shinyjs::show("tbl")}
})

observe({
  if(((applyfilter$afa==3)&&(length(rowfilter())>0)))
  {shinyjs::hide("tbl2")}
  else{shinyjs::show("tbl2")}
})

observe({
  if(input$data_input1=="Eurostat data" & applyfilter$afa!=3)
  {shinyjs::hide("testtab")}
  else{shinyjs::show("testtab")}
})

#Table mit gefiltertem dataset
output$testtab <- renderDT(
  dataset1(), # reactive data
  server = TRUE, #server-side processing
  class = "display nowrap compact", # style
  #filter = "top", # location of column filters
  #editable = TRUE,
  options = list(
    scrollX = TRUE, #allow user to scroll wide tables horizontally
    pageLength = 5,
    lengthMenu = c(5, 10, 15, 20)
  ))
################################################################################
#select column with year -->choices Jahr4
subyear<-reactive({
  if (is.null(input$Jahr2) || input$Jahr2 == "None" ||
      (input$data_input1=="Eurostat data") ) 
  {NULL}
  else{
    data<- as.data.frame(dataset())
    year <- input$Jahr2
    subyear <- as.factor(data[,which(colnames(data)==year)])
    subyear
  }
})

output$Jahr3 <- renderUI({
   if (is.null(input$Jahr2) || (input$Jahr2 == "None")|| 
       (input$data_input1 == "Eurostat data") || (is.null(subyear()))) 
     return(NULL)
   else
   selectInput("Jahr4", "Please choose any year/ date", 
               choices = c("Choose", levels(subyear())))
 })

################################################################################
#eudata long -> wide etc
eudata1<-reactive({
  if(applyfilter$afa<3 ||
     is.null(input$Vgl3.2) || 
     is.null(input$Vgl4.2) ||
     input$Vgl3.2 == "Choose Variable 1" ||
     input$Vgl4.2 == "Choose Variable 2" || 
     is.null(dataset1()) ||
     is.null(dataset0$df_data) || 
     input$data_input1 == "Upload own dataset"
     )
  {NULL}
else{
  if(input$Vgl3.2==input$Vgl4.2){
    cleandata<-as.data.frame(dataset1())
    nodubs<-cleandata[, sapply(cleandata, function(col) length(unique(col))) > 1]
    xvar<-as.character(input$Vgl3.2)
    names(nodubs)[names(nodubs) == xvar] <-"var" #input$Vgl1.2
    eudata1 <- reshape(nodubs, idvar = "iso_a2", timevar = "var", sep = "", 
                       direction = "wide")
    colnames(eudata1)<-gsub("values", "", colnames(eudata1))
    eudata1
  }else{
    cleandata<-as.data.frame(dataset1())
    nodubs<-cleandata[, sapply(cleandata, function(col) length(unique(col))) > 1]
    xvar1<-as.character(input$Vgl3.2)
    xvar2<-as.character(input$Vgl4.2)
    names(nodubs)[names(nodubs) == xvar1] <-"var1" #input$Vgl1.2
    names(nodubs)[names(nodubs) == xvar2] <-"var2" #input$Vgl2.2
    
    a<-nodubs[, which(names(nodubs) !="var2")]
    eudata1 <- reshape(a, idvar = "iso_a2", timevar = "var1", sep = "", 
                       direction = "wide")
    colnames(eudata1)<-gsub("values", "", colnames(eudata1))
    eudata1
    }
  }
}) 

eudata2<-reactive({
  if(applyfilter$afa<3 ||
     is.null(input$Vgl3.2) || 
     is.null(input$Vgl4.2) ||
     input$Vgl3.2 == "Choose Variable 1"||
     input$Vgl4.2 == "Choose Variable 2"|| 
     is.null(dataset1())||
     is.null(dataset0$df_data) || 
     input$data_input1=="Upload own dataset"
     )
  {NULL}
else{
  if(input$Vgl3.2==input$Vgl4.2){
    eudata2<-eudata1()
    eudata2
  }else{
    cleandata<-as.data.frame(dataset1())
    nodubs<-cleandata[, sapply(cleandata, function(col) length(unique(col))) > 1]
    xvar1<-as.character(input$Vgl3.2)
    xvar2<-as.character(input$Vgl4.2)
    names(nodubs)[names(nodubs) == xvar1] <-"var1" #input$Vgl1.2
    names(nodubs)[names(nodubs) == xvar2] <-"var2" #input$Vgl2.2
    b<-nodubs[, which(names(nodubs) !="var1")]
    eudata2 <- reshape(b, idvar = "iso_a2", timevar = "var2", sep = "", 
                       direction = "wide")
    colnames(eudata2)<-gsub("values", "", colnames(eudata2))
    eudata2
    }
  }
}) 

################################################################################
#eurostat level
output$level1 <- renderUI({
  if (is.null(eudata1()) || is.null(eudata2()) || 
      (input$data_input1 == "Upload own dataset"))
    return(NULL)
  else 
    selectInput("level1.2", "Choose any level of the 1st variable", 
                choices = c("Choose", 
                names(eudata1()[which(names(eudata1())!="iso_a2")])))
})

output$level2 <- renderUI({
  if (is.null(eudata1()) || is.null(eudata2()) || 
      (input$data_input1 == "Upload own dataset"))
    return(NULL)
  else 
    selectInput("level2.2", "Choose any level of the 2nd variable", 
                choices = c("Choose", 
                names(eudata2()[which(names(eudata2())!="iso_a2")])))
})

#hide Vgl1.2 & Vgl2.2 when level1.2 & level 2.2 have been rendered
observeEvent(input$level1.2,{
  disable("Vgl3.2")
})
observeEvent(input$level2.2,{
  disable("Vgl4.2")
})
################################################################################
#wenn own dataset
output$goa <- renderUI({
  if(is.null(dataset1())||(input$data_input1=="Eurostat data") ) 
    return(NULL)
  else{actionButton("go1", "Create Plot")} 
})

#wenn eurostat dataset
output$gob <- renderUI({
  if(is.null(dataset0$df_data) || 
     applyfilter$afa <3 ||
     is.null(dataset1())||
     (input$data_input1=="Upload own dataset") ) 
    return(NULL)
  else{actionButton("go2", "Create Plot")} 
})

####################
#go wenn own dataset
observe({
  if(is.null(dataset()) ||
     is.null(dataset1())||
     is.null(input$maptype)||
     input$maptype=="Choose"||
     is.null(input$Jahr2) ||
     #is.null(input$Jahr4) |
     is.null(input$Region2) ||
     input$Region2=="Choose"||
     is.null(input$population)|| 
     is.null(input$Vgl1.2) ||
     input$Vgl1.2=="Choose Variable 1"||  
     is.null(input$Vgl2.2)||
     input$Vgl2.2=="Choose Variable 2") 
  {disable("go1")}
  else{
    enable("go1")
  }
})

#go wenn eurostat
observe({
  if(is.null(dataset()) ||
     is.null(dataset1())||
     is.null(eudata1())||
     is.null(eudata2())||
     is.null(input$Vgl3.2) ||
     input$Vgl3.2 == "Choose Variable 1"||  
     is.null(input$Vgl4.2)||
     input$Vgl4.2 == "Choose Variable 2"|| 
     input$level1.2 == "Choose"||
     input$level2.2 == "Choose"||
     is.null(input$level1.2) ||
     is.null(input$level2.2) )
  {disable("go2")}
  else{
    enable("go2")
  }
})


####################
#go wenn own dataset
dataset1a<-reactive({input$go1
  isolate({
  if(is.null(dataset())||
     is.null(dataset1())||
     is.null(input$maptype)||
     input$maptype=="Choose"||
     is.null(input$Jahr2) ||
     #(input$Jahr2!="None" & is.null(input$Jahr4)) ||
     is.null(input$Region2) ||
     input$Region2=="Choose"||
     is.null(input$population)|| 
     is.null(input$Vgl1.2) ||
     input$Vgl1.2=="Choose Variable 1"||  
     is.null(input$Vgl2.2)||
     input$Vgl2.2=="Choose Variable 2") 
    {NULL}
  else{
    if(input$data_input1=="Upload own dataset" & input$maptype=="European countries")
      {
      dataset<-as.data.frame(dataset1())
      dataset1a<-filter(dataset, iso_a2 != "EA" & iso_a2 != "EA18" & 
                         iso_a2 != "EA19" & iso_a2 != "EU" & iso_a2 != "EU27" & 
                         iso_a2 != "EU28")
      #dataset2<-filter(dataset, iso_a2 == "BE" | iso_a2 == "BG" | iso_a2 == "CZ" 
      #| iso_a2 == "DK" | iso_a2 == "DE" | iso_a2 == "EE" | iso_a2 == "IE" | 
      #iso_a2 == "EL" | iso_a2 == "ES" | iso_a2 == "FR" | iso_a2 == "HR" | 
      #iso_a2 == "IT" | iso_a2 == "CY" | iso_a2 == "LV" | iso_a2 == "LT" | 
      #iso_a2 == "LU" | iso_a2 == "HU" | iso_a2 == "MT" | iso_a2 == "NL" | 
      #iso_a2 == "AT" | iso_a2 == "PL" | iso_a2 == "PT" |iso_a2 == "RO" | 
      #iso_a2 == "SI" | iso_a2 == "SK" | iso_a2 == "FI" | iso_a2 == "SE" | 
      #iso_a2 == "UK")
      dataset1a
    }else{
      dataset1a<-as.data.frame(dataset1())
      colnames(dataset1a)[which(names(dataset1a) == input$Region2)] <- "iso_a2"
      dataset1a
        }
    }
  })
})

################################################################################
#eurostat-> population density 
dataset1b<-reactive({input$go2 
  isolate({
  if(applyfilter$afa<3 ||
     is.null(dataset0$df_data)||
     is.null(eudata1())||
     is.null(eudata2())||
     is.null(input$Vgl3.2) || 
     is.null(input$Vgl4.2) ||
     input$Vgl3.2 == "Choose Variable 1"||
     input$Vgl4.2 == "Choose Variable 2"|| 
     input$level1.2 == "Choose"||
     input$level2.2 == "Choose"||
     is.null(input$level1.2) ||
     is.null(input$level2.2) ||
     is.null(dataset1())||
     input$data_input1=="Upload own dataset")
  {NULL}
  else{
    if(input$Vgl3.2==input$Vgl4.2){
      
      eudatab<-eudata1()  
      dens<-get_eurostat("demo_r_d3dens")
      colnames(dens)[colnames(dens)=="geo"] <- "iso_a2"
      colnames(dens)[colnames(dens)=="values"] <- "density"
      dens2<-filter(dens, iso_a2 == "AT"| iso_a2 =="BE"| iso_a2 =="BG"| 
                      iso_a2 =="CY"| iso_a2 =="CZ"| iso_a2 =="DE"| iso_a2 =="DK"| 
                      iso_a2 =="EE"| iso_a2 =="EL"| iso_a2 =="ES"| iso_a2 =="FI"| 
                      iso_a2 =="FR"| iso_a2 =="HR"| iso_a2 =="HU"| iso_a2 =="IS"| 
                      iso_a2 =="IT"| iso_a2 =="LT"| iso_a2 =="LU"| iso_a2 =="LV"| 
                      iso_a2 =="MT"| iso_a2 =="NL"| iso_a2 =="NO"| iso_a2 =="PL"| 
                      iso_a2 =="PT"| iso_a2 =="RO"| iso_a2 =="RS"| iso_a2 =="SE"| 
                      iso_a2 =="SI"| iso_a2 =="SK"| iso_a2 =="UK")
      dens2$time<-gsub("-01-01", "", dens2$time)
      eutime<-dataset1()[1,which(colnames(dataset1())=="time")]
      dens2<-dens2[which(dens2$time==eutime),] #year!!!!
      
      dataset1b<-merge(eudatab, dens2, by="iso_a2")
      dataset1b
    }
    else{
      eudata1<-eudata1()
      eudata2<-eudata2()
      eudatab<-merge(eudata1, eudata2, by="iso_a2")
      #eudata<-as.data.frame(eudata)
      
      #Bevölkerungsdichte
      dens<-get_eurostat("demo_r_d3dens")
      colnames(dens)[colnames(dens)=="geo"] <- "iso_a2"
      colnames(dens)[colnames(dens)=="values"] <- "density"
      dens2<-filter(dens, iso_a2 == "AT"| iso_a2 =="BE"| iso_a2 =="BG"| iso_a2 =="CY"| 
                      iso_a2 =="CZ"| iso_a2 =="DE"| iso_a2 =="DK"| iso_a2 =="EE"| 
                      iso_a2 =="EL"| iso_a2 =="ES"| iso_a2 =="FI"| iso_a2 =="FR"| 
                      iso_a2 =="HR"| iso_a2 =="HU"| iso_a2 =="IS"| iso_a2 =="IT"| 
                      iso_a2 =="LT"| iso_a2 =="LU"| iso_a2 =="LV"| iso_a2 =="MT"| 
                      iso_a2 =="NL"| iso_a2 =="NO"| iso_a2 =="PL"| iso_a2 =="PT"| 
                      iso_a2 =="RO"| iso_a2 =="RS"| iso_a2 =="SE"| iso_a2 =="SI"| 
                      iso_a2 =="SK"| iso_a2 =="UK")
      dens2$time<-gsub("-01-01", "", dens2$time)
      eutime<-dataset1()[1,which(colnames(dataset1())=="time")]
      dens2<-dens2[which(dens2$time==eutime),] #year!!!!
      
      dataset1b<-merge(eudatab, dens2, by="iso_a2")
      dataset1b
    }
  }
}) 
})

#######################################
#Region --> wenn eurostat, dann geo; wenn own dataset, dann input$Region2 
#umbenennen in iso_a2 (& filtern wenn eurostat)

dataset2<-reactive({
  if(is.null(input$data_input1)){NULL}
  else{
    if(input$data_input1=="Eurostat data" & is.null(dataset1b())) {NULL}
    else{
      if(input$data_input1=="Eurostat data"){
        dataset1b()
      }
    else{
      if(input$data_input1=="Upload own dataset" & (is.null(dataset1a())))
      {NULL}
      else{
        dataset1a()
      }
    }
    }
  }
})
####################################
#subset year (optional) -->wenn own dataset none vs spezifisches Jahr
#wenn eurostat, dann time in year umbenennen, nach time wurde schon gefiltert

dataset3<-reactive({
  if (is.null(dataset2()))
{NULL}
  else{
    if ((input$data_input1 == "Upload own dataset")&&
         ((input$Jahr2=="None")||(is.null(input$Jahr4))))
          {
          dataset3<-dataset2()
          dataset3
    }else{
          if(input$data_input1 == "Upload own dataset"){
          dataset2<-dataset2()
          year <- as.character(unlist(input$Jahr2))
          year_selected <- as.numeric(unlist(input$Jahr4))
          dataset3 <- as.data.frame(dataset2[which(dataset2[,which(colnames(dataset2) == year)] == year_selected),]) #col_year
          dataset3
        }else{   
          #if(input$data_input1=="Eurostat data"){
          dataset3<-dataset2()
          dataset3 #nach time wurde schon gefiltert
         # }else{NULL}
        }
      }
    }
  })

################################
#https://dev.to/awwsmm/reactive-datatables-in-r-with-persistent-filters-l26

#normalize variables
dataset4 <- reactive({
  if(is.null(input$data_input1) || is.null(dataset3()))
  {NULL}
  else{
      if(input$data_input1=="Upload own dataset") 
        {
      dataset4 <- dataset3()
      #Vergleichsvariablen
      X1<-input$Vgl1.2
      X2<-input$Vgl2.2
      var1 <- as.numeric(dataset4[,which(colnames(dataset4) == X1)])
      var2 <- as.numeric(dataset4[,which(colnames(dataset4) == X2)])
      
      #relativieren
      population <- input$population
      dataset4$popvar <- as.numeric(dataset4[,which(colnames(dataset4) == population)])
      dataset4$varX1 <- var1/dataset4$popvar
      dataset4$varX2 <- var2/dataset4$popvar
      dataset4
      
    }else{
     dataset4 <- dataset3()
     
     #Vergleichsvar
     X1<-as.character(input$level1.2)
     X2<-as.character(input$level2.2)
     #names(dataset4)[names(dataset4) == X1] <-"varX1" #input$Vgl1.2
     #names(dataset4)[names(dataset4) == X2] <-"varX2" #input$Vgl2.2
     var1<-as.numeric(dataset4[,which(colnames(dataset4)==X1)])
     var2<-as.numeric(dataset4[,which(colnames(dataset4)==X2)])
     
     #relativieren
     dataset4$varX1 <- var1/dataset4$density #as.numeric(dataset4$varX1)
     dataset4$varX2 <- var2/dataset4$density #as.numeric(dataset4$varX2)
     dataset4
    }
  }
 })

################################################################################
#download the final data (wenn eurostat) --> funktioniert nur in browser

output$downloadData1 <- renderUI({
  if(is.null(dataset4()) || input$data_input1=="Upload own dataset" ||
     is.null(dataset0$df_data))
    return(NULL)
  else{
   downloadButton("downloadData", "Download final dataset")
  }
})

output$downloadData <- downloadHandler(
  filename =function() {
      paste("finaldata-", Sys.Date(), ".xlsx", sep="")
        },
  content = function(file) {
      write.xlsx(dataset3(), file, row.names = F)
        }
)

################################################################################
#for legend
X1<-reactive({
  if((is.null(input$Vgl1.2) & is.null(input$level1.2))){NULL}
  else{
    if(input$data_input1=="Upload own dataset"){
      X1<-input$Vgl1.2
      X1}
    else{
      X1<-input$level1.2
      X1}
  }
})

X2<-reactive({
  if((is.null(input$Vgl1.2) & is.null(input$level2.2))){NULL}
  else{
    if(input$data_input1=="Upload own dataset"){
      X2<-input$Vgl2.2
      X2}
    else{
      X2<-input$level2.2
      X2}
  }
})
################################################################################
#color
bvColors=c("#be64ac","#8c62aa","#3b4994","#dfb0d6","#a5add3","#5698b9","#e8e8e8",
           "#ace4e4","#5ac8c8") # #e8e8e8 ersetzen durch #cccccc?

#create plot
m2 <- reactive({
  if(is.null(dataset4())||
     is.null(selected_map())||
     (input$data_input1=="Eurostat data" & 
      (is.null(input$file2) || input$file2 == "")))
  {NULL}
  else{
      req(dataset4())
      Sys.sleep(1)
      
      selected_map<-selected_map()
      finaldata <- dataset4()#as.data.frame(read_excel(input$file1$datapath))

      # categorize rates into 3 bins
      bins<-3
      finaldata<-mutate(finaldata,X1Bin=cut2(finaldata$varX1,g=bins,levels.mean = TRUE))
      finaldata<-mutate(finaldata,X2Bin=cut2(finaldata$varX2,g=bins,levels.mean = TRUE))
      levels(finaldata$X1Bin)<-bins:1
      levels(finaldata$X2Bin)<-bins:1
      finaldata<-mutate(finaldata,region=iso_a2,value=paste(X1Bin,"-",X2Bin,sep=""))

      #merge with map
      df4<-sp::merge(selected_map, finaldata, by = "iso_a2", duplicateGeoms = TRUE)
      plot1<- tm_shape(df4) +
        tm_fill(col = "value", palette = bvColors, legend.show = F) +
        tm_borders()
      m2<-tmap_leaflet(plot1)
      m2
   }
    })

zoom<- reactive({if(input$maptype=="European countries" || 
                    input$data_input1 == "Eurostat data"){
    zoom <- 1.5
    zoom}
  else{
    zoom <- 5
    zoom}
  })

output$bivariat <- renderLeaflet({
  if (is.null(m2())) {NULL}
  else{
    m2()%>% 
    #setView(15.255119, 50.605961, zoom()) %>% 
    setView(10.252743, 51.157299, zoom()) %>%
    addTiles(options = providerTileOptions(noWrap = TRUE)) 
    }
})


################################################################################
#legend

legend <- reactive({
  if(is.null(dataset4())||
     is.null(X1())||is.null(X2())||
     (input$data_input1=="Eurostat data" & 
      (is.null(input$file2) || input$file2 == ""))) 
  {NULL}
    else{
    req(X1())
    req(X2())
    Sys.sleep(1)
    
    #Achsennamen
    Xa<-X1()
    Xb<-X2()
    X1<-paste(Xa,sep=" ", "-->")
    X2<-paste(Xb,sep=" ", "-->")

    #Legende
    legendGoal=melt(matrix(1:9,nrow=3)) #3*3 matrix
    lg <- ggplot(legendGoal, aes(Var2,Var1,fill = as.factor(value)))+ 
                  geom_tile()+
                  scale_fill_manual(name="",values=bvColors)+
                  theme(legend.position="none", 
                        axis.title.x=element_text(size=rel(1),
                        color=bvColors[3]),
                        axis.title.y=element_text(size=rel(1),
                        color=bvColors[3]), 
                        axis.text=element_blank(),
                       line=element_blank())+
                  xlab(X1)+
                  ylab(X2)
    lg
    }
 })

output$legend <- renderPlot({
  if (is.null(legend())) {NULL}
      else{legend()}
})
################################################################################
#save plot

output$downloadPlot1 <- renderUI({
  if(is.null(m2()))
    return(NULL)
  else{
    downloadButton("downloadPlot", "Download plot")
  }
})

output$downloadPlot <- downloadHandler(
  filename = function() { 
  paste("bivariate-choreopleth-", X1(),"-", X2(),"-", Sys.Date(), ".png", sep="")
  },
  content = function(file) {
    mapshot(x = m2()
             , file = file
             , cliprect = "viewport" # the clipping rectangle matches the height & width from the viewing port
             , selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
    )}
)
##########################
#at the end --> reset
#https://stackoverflow.com/questions/42889993/restart-shiny-app-from-within-app-reloading-data
output$reset1 <- renderUI({
  if ((input$data_input1=="Choose") || 
      (input$data_input1=="Eurostat data" & is.null(dataset0$df_data))||
      (input$data_input1=="Upload own dataset" & is.null(dataset()))
  )
    return(NULL)
  else{
    actionButton("reset", "Reset app", icon = icon("refresh"))
  }
})

observeEvent(input$reset,{
  {js$reset()
    }
})

#############################################
#eurostat -> clean eurostat cache
output$cleancache1 <- renderUI({
  if (is.null(m2())|| 
      (input$data_input1=="Upload own dataset") ||
      (input$data_input1=="Choose") ||
      is.null(dataset0$df_data) ||
      (dataset0$df_data == 0)
      )
    return(NULL)
  else{
    actionButton("cleancache", "Clean Eurostat Cache")
  }
})

observeEvent(input$cleancache,{
  {clean_eurostat_cache()
  }
})
##########################
}) #Klammer vom Beginn, Finger weg!!!
