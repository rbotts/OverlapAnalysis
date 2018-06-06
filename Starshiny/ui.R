#R Version 3.4.4
#Starshiny UI script

require("shiny")

fluidPage(
  
  #Making the active tab a nice shade of orange...
  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: white;  color:cornflowerblue}
    .tabbable > .nav > li[class=active]    > a {background-color: #EB6536; color:white}
  ")),
  
  titlePanel(""), #For spacing
  
  #Main content
  tabsetPanel(type = "pills",
    tabPanel(title = "Upload Data", #Upload tab ----
      titlePanel(title = "Starshiny: Overlap Analysis with R Shiny", windowTitle = "Starshiny"),
      fluidRow(
        column(8,
          HTML(
            "Instructions about how to use the tool."
          )
        ),
        column(4,
          wellPanel(
            HTML("Upload data with a button in this grey box.")
          )
        )
      )
    ),
    tabPanel(title = "Data Analysis", #Data Analysis tab ----
      fluidRow(column(12,
                      titlePanel(""), #For spacing
                      HTML("Buttons, text boxes, and graphs on this page.")))
    )
  )
)