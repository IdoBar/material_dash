data_tab <- tabItem(
  tabName = "materials",
  
  br(),
  br(),

  column(
    width = 12,
    align = "left",
    h1("Material Information")
  ),

  br(),
  
  fluidRow(
    # element
      box(
        width = 5,
        height=400, 
        title = HTML("List of available materials"), 
        status = def_box_col, 
        solidHeader = FALSE, 
        collapsible = TRUE,
        DTOutput("materialDT")
          # sliderInput(
          #   "slider_boxsidebar", 
          #   "Number of observations:",
          #   min = 0, 
          #   max = 1000, 
          #   value = 500
          # )
        ),
      box(
        width = 3,
        height=400, 
        title = HTML("Selected material prices"), 
        status = def_box_col, 
        solidHeader = FALSE, 
        collapsible = TRUE,
        plotOutput("price_plot"),
        verbatimTextOutput("inputs")
      )
        
      #)
    )
)
