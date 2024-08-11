### Before first usage, all steps in sites.R have to be performed once. 
### Then, the app can be started via "Run App" button in RStudio


library(leaflet); library(shiny); library(shinythemes); library(shinycssloaders)


maxyear=as.double(substr(Sys.Date(),1,4))

fluidPage(
  theme = shinytheme("cyborg"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition="input.tabselected == 1",
                   wellPanel(h6("Einfärbung"),
                             radioButtons(inputId = "einfaerbung",
                                label = NULL,
                                choices = c("Type", "Visitor", "DateInscribed"),
                                selected = "Type"
                                )
                             ),
                      conditionalPanel("input.einfaerbung == 'Type'",
         radioButtons(inputId = "wer1",
                      label = "Ansicht",
                      choices = c("Alle", "user2", "user1"),
                      selected = "Alle"),
         hr(),
         checkboxGroupInput("checkbox_tab1",
                            label = "Type der Stätten",
                            choices = c("Cultural","Mixed","Natural"),
                            selected = c("Cultural","Mixed","Natural")),
         checkboxInput(inputId = "dangerList",
                       label = "Danger List in rot anzeigen"),
         hr(),
               conditionalPanel("input.wer1 == 'Alle'",
                  radioButtons(inputId = "vonbis1",
                               label = "Datum filtern",
                               choices = c("cumulated","single year"),
                               selected = "cumulated"),  
                  sliderInput(inputId = "jahr1",
                              label = NULL,
                              min = 1978 , max = maxyear,
                              value = maxyear,
                              sep=""),
                  radioButtons(inputId = "datumwahl1",
                               label = NULL,
                               choices = c("DateInscribed"),
                               selected = "DateInscribed")
               ),
               conditionalPanel("input.wer1 != 'Alle'",
                      radioButtons(inputId = "vonbis2",
                                   label = "Datum filtern",
                                   choices = c("cumulated","single year"),
                                   selected = "cumulated"),  
                      sliderInput(inputId = "jahr2",
                                  label = NULL,
                                  min = 2002 , max = maxyear,
                                  value = maxyear,
                                  sep=""),
                      radioButtons(inputId = "datumwahl2",
                                   label = NULL,
                                   choices = c("DateVisited","DateInscribed"),
                                   selected = "DateVisited")
          )
                      ),
         conditionalPanel("input.einfaerbung == 'Visitor'",
                          checkboxGroupInput("checkbox_Visitor",
                                             label = "Ansicht",
                                             choices = c("user2", "user1"),
                                             selected = c("user2", "user1"))
                          ),
      
         conditionalPanel("input.einfaerbung == 'DateInscribed'",
                          radioButtons(inputId = "wer1.E",
                                       label = "Ansicht",
                                       choices = c("Alle", "user2", "user1"),
                                       selected = "Alle"),
         )
         
         
      ),
    conditionalPanel(condition="input.tabselected == 2",
                       sliderInput(inputId = "jahrT",
                                   label = NULL,
                                   min = 2002 , max = maxyear,
                                   value = maxyear,
                                   sep=""),
                       radioButtons(inputId = "aggregation",
                                    label = "Aggregiert nach:",
                                    choices = c("Region", "Country"),
                                    selected = "Region")
      ),
      
      width = 3
    ),
    mainPanel(
      tabsetPanel(#Typee="tabs",
        tabPanel("Karte", value=1,
                 leafletOutput("MapPlot1", height=600)),
        tabPanel("Tabelle", value=2,
                 htmlOutput(outputId = "text"), br(),
                 tableOutput("tabelle1"), br()
                 ,dataTableOutput("tabelle3"), br()
                 ,dataTableOutput("tabelle2")
                ),
        tabPanel("Plots", value=3,
                 withSpinner(plotOutput("plot1")), br(),
                 withSpinner(plotOutput("plot2")), br(),
                 withSpinner(plotOutput("plot3")), br(),
                 withSpinner(plotOutput("plot4"))),
        id = "tabselected"
      ),
      width = 9
    )
  )
)

