library(shiny)
library(leaflet)
library(splancs)
library(sp)
library(ggplot2)
library(shiny)
library(shinydashboard)

#setwd("C:/Users/anderstn/Desktop/RStudio/MangoApp/Paddocks")
setwd("C:/Users/Zhenglin Wang/Documents/testApp/data")

filenames<-list.files(pattern="*.csv")
paddocknames <- substr(filenames,4,5)
filenumbers<-seq(filenames)
b <- 0
for(filenumber in filenumbers){ b <- b+1

tin <- ifelse(nchar(filenames[filenumber])<10, substr(filenames[filenumber], 1, nchar(filenames[filenumber])-4),  substr(filenames[filenumber], 6, nchar(filenames[filenumber])-4))
if (b==1) {name<-tin}else{name <- c(name,tin)}

}
dashboardPage(skin="red",
              dashboardHeader(dropdownMenuOutput("notificationsMenu"), title="Mango Farm Information", titleWidth = 300),                
              dashboardSidebar(
                width=300,
                fluidRow(column(12,div(style = "height:15px"))),
                hr(),
                dateRangeInput('dateRange',
                               label = h4('Select dates to display data:'),
                               start = Sys.Date() - 60, end = Sys.Date(), min = "2015-05-12",
                               format = "dd/mm/yy"),
                fluidRow(column(12,div(style = "height:10px"))),
                fluidRow(column(10, offset = 1, textOutput("DateRange"), tags$head(tags$style("#DateRange{color: white;
                                                                                              font-size: 12px;
                                                                                              text-align: center;
                                                                                              font-weight: bold;
                                                                                              }"
                   )
                ))),
                hr(),
                numericInput(
                  'dmStdrd', 'Select DM standard', 15, min = 10, max = 22, step = 1
                ),
                sliderInput(
                  'increment', 'Select Daily DM% increase', min = 0.01, max = 0.2, 0.1, post = '%'
                ),
                sliderInput(
                  'CI', 'Select confidence interval %', min = 80, max = 100, 90, post = '%'
                ),
                hr(),
                selectInput(
                  'e0', 'Select Block to Display Detail', choices = name,
                  selectize = FALSE
                ),
                radioButtons("DMchoice", label = h3("Dry Matter View"),
                             choices = list("Dry Matter" = 1, "No Dry Matter" = 2),selected = 1),
                radioButtons("treechoice", label = h3("View"),
                             choices = list("Trees" = 1, "Paddock" = 2),selected = 1)
                ),
              
              dashboardBody(fluidRow(tabBox(title="Mango Harvest Status", width = 12 , 
                                            tabPanel(title = "Map of Blocks with Dry Matters",
                                                     (leafletOutput("MangoMap",height=500))),
                                            tabPanel(title = "Test Map of Blocks with Dry Matters",
                                                     (leafletOutput("TestMangoMap",height=500))),
                                            tabPanel(height = 500, title= "Indivdual Block Dry Matters", 
                                                     fluidRow(box(solidHeader=TRUE, status="danger",width=12, title=textOutput("Head"),  plotOutput("paddrymatter", height=300))),
                                                     fluidRow(box(solidHeader=TRUE, status= "danger", collapsible = TRUE, width=12, title = textOutput("Name"),
                                                                  valueBoxOutput("treenum",width=4),
                                                                  valueBoxOutput("dayBox",width=4),
                                                                  valueBoxOutput("meanBox",width=4)))
                                                              
                                            ),
                                            fluidRow(box(width = 12, "This App is developed by the CQUni Data Muster team, delivering data access 
                                                         solutions that enable quick understanding of complex problems. If you want to know more about
                                                         Data Muster contact Dave Swain at D.Swain@cqu.edu.au"))
                                            )
                                            )
                            
              )
              
              )



