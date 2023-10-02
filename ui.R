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
library(tmap) #tm_shape
#needs Rtools to be installed
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

#fix tmap invalid polygons
tmap_options(check.and.fix = TRUE)
sf_use_s2(FALSE) #https://github.com/afrimapr/afrimapr-book/issues/30

#https://cran.r-project.org/web/packages/pals/vignettes/bivariate_choropleths.html
################################################
## ui.R ##

dashboardPage(
  dashboardHeader(title = "Bivariate Choreopleths"),
  dashboardSidebar(theme_onenote, tags$style(".skin-blue .sidebar a { color: #444; }"),#design
    shinyjs::useShinyjs(),
    #extendShinyjs(text = jsResetCode),
    fluidRow(
      column(12, align = "center", 
             selectInput("data_input1", "Choose data Input",
             choices =list("Choose", "Eurostat data", "Upload own dataset"))
      )),
    fluidRow(
      column(12, align = "center", uiOutput("reset1")
      )),
    fluidRow(
      column(12, align = "center", uiOutput("cleancache1")
      )),
    fluidRow(
      column(12, align = "center", uiOutput("text")
      )),
    fluidRow(
      column(12, align = "center", uiOutput("searchbutton2")
      )), 
    fluidRow(
      column(12, align = "center", uiOutput("data_input2")
      )),
    fluidRow(
      column(12, align = "center", uiOutput("searchbutton")
      )),
    fluidRow(
      column(12, align = "center", uiOutput("maptype1")
      )),
    fluidRow(
      column(12, align = "center", uiOutput("Vgl1.1")
      )),
    fluidRow(
      column(12, align = "center", uiOutput("Vgl2.1")
      )),
    fluidRow(
      column(12, align = "center", uiOutput("Jahr1")
      )),
    fluidRow(
      column(12, align = "center", uiOutput("Jahr3")
      )),
    fluidRow(
      column(12, align = "center", uiOutput("Region1")
      )),
    fluidRow(
      column(12, align = "center", htmlOutput("normalize")
      )),
    #eurostat
    fluidRow(
      column(12, align = "center", uiOutput("Vgl3.1")
      )),    
    fluidRow(
      column(12, align = "center", uiOutput("level1")
      )),
    fluidRow(
      column(12, align = "center", uiOutput("Vgl4.1")
      )),
    fluidRow(
      column(12, align = "center", uiOutput("level2")
      )),    
    #go
    fluidRow(
      column(12, align = "center", uiOutput("goa")
      )),
    fluidRow(
      column(12, align = "center", uiOutput("gob")
      )),
    fluidRow(
     column(12, div(style = "height:200px")
      ))
), #von Beginn sidebar
  dashboardBody(
    fluidPage(
      # tags$head(tags$style(
      #   HTML("input[type='search']:disabled {visibility:hidden}")
      # )),
      shinyjs::useShinyjs(),
      fluidRow(
        column(12, DTOutput("eurostatdata")
        )),
      fluidRow(
        column(12, tableOutput("description1")
        )),
      fluidRow(
        column(12, DTOutput("testtab")
        )),
      fluidRow(
        column(12, DTOutput("testtab2")
        )),
      fluidRow(
        column(12, align = "center", tags$b(htmlOutput("instructions"))
        )),
      fluidRow( 
        column(12, align = "center", uiOutput("filter1")
        )),
      fluidRow(
        column(12, DTOutput("tbl")
        )),
      fluidRow(
        column(12, DTOutput("tbl2")
        )),
      fluidRow( style="padding:20px;",
        column(9, leafletOutput("bivariat")
        ), 
        column(3, plotOutput("legend")
        )),
      fluidRow(
        column(4, align = "center", uiOutput("downloadPlot1")),
        column(4, align = "center", uiOutput("downloadData1"),
        column(4)
        )),
      fluidRow(
        column(12, div(style = "height:50px")
        )),
      #fluidRow(column(12,
      #tags$h5("This app creates bivariate choreopleths mostly following the instructions by", a(href="https://rpubs.com/apsteinmetz/prek", "Art Steinmetz") #,
      #tags$h5("Why are population sizes necessary to normalize the variables displayed in the plot?", a(href="https://xkcd.com/1138/", "Click here to find out")))        
        #))),
      fluidRow(column(12, div(style = "height:100px")
        )) 
)
)
)