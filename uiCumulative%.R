tabPanel(height = 500, title = "Cumulative %",
         fluidRow(box(solidHeader=TRUE, status="danger",width=12, title=textOutput("Head3"),  plotOutput("cumlG", height=300))),
         fluidRow(box(solidHeader=TRUE, status= "danger", collapsible = TRUE, width=12, title = textOutput("Name3"))),
         valueBoxOutput("cumlGSamp",width=6),
         valueBoxOutput("cumlGAvg",width=6))