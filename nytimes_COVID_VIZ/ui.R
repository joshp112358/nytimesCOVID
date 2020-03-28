
library(shiny)

l <- c(
  "Alabama"             , "Alaska"            ,   "Arizona"          ,    "Arkansas"   ,         
  "California"          , "Colorado"          ,   "Connecticut"      ,     "Delaware"   ,         
  "District of Columbia", "Florida"           ,   "Georgia"          ,     "Guam"       ,         
  "Hawaii"              , "Idaho"             ,   "Illinois"         ,    "Indiana"     ,        
  "Iowa"                , "Kansas"            ,   "Kentucky"        ,     "Louisiana"   ,        
  "Maine"               , "Maryland"          ,   "Massachusetts"   ,     "Michigan"    ,        
  "Minnesota"           , "Mississippi"       ,   "Missouri"        ,     "Montana"     ,        
  "Nebraska"            , "Nevada"            ,   "New Hampshire"   ,     "New Jersey"  ,        
  "New Mexico"          , "New York"          ,   "North Carolina"  ,     "North Dakota" ,       
  "Ohio"                , "Oklahoma"          ,   "Oregon"          ,     "Pennsylvania",        
  "Puerto Rico"         , "Rhode Island"      ,   "South Carolina"  ,     "South Dakota",        
  "Tennessee"           , "Texas"             ,  "Utah"             ,    "Vermont"       ,      
  "Virgin Islands"      , "Virginia"          ,   "Washington"      ,     "West Virginia" ,      
  "Wisconsin"           , "Wyoming"   
)

shinyUI(fluidPage(
  
  titlePanel("New York Times Covid Data"),
  p("by: Joshua Paik"),
  p("email: joshdpaik@gmail.com"),
  p("Data comes from the New York Times (https://github.com/nytimes/covid-19-data)."),

  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("States", 
                  "States",
                   l,
                   "New York"),
      uiOutput("ui2"),
      checkboxInput("Log", "Log"),
      checkboxInput("First_Derivative",
                    "First Derivative")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Confirmed and Dead",
                 plotOutput("HistCD"),
                 textOutput("ConfirmedDead"),
                 br(),br(),br(),br(),br(),br(),
                 br(),br(),br(),br(),br(),br(),
                 br(),br(),br(),br(),br(),br()
                 ),
        tabPanel("Dead",
                 plotOutput("HistDead"),
                 textOutput("DeathToll"),
                 br(),br(),br(),br(),br(),br(),
                 br(),br(),br(),br(),br(),br(),
                 br(),br(),br(),br(),br(),br()
                 ),
        tabPanel("Case Fatality Ratio",
                 plotOutput("NaiveMortality"),
                 textOutput("Mortality"),
                 p("Computing the case fatality ratio as the number dead/the number of comfirmed cases
                   is biased and potentially underestimates the mortality rate. This is because a certain number of those 
                   with the coronavirus will die, and the numerator is hence lower than is real."),
                 br(),br(),br(),br(),br(),br(),
                 br(),br(),br(),br(),br(),br(),
                 br(),br(),br(),br(),br(),br()
                 )
        #,
        # tabPanel("Overlay")
      )
    )
  )
)
)
