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
        ),
      box(
        width = 3,
      #  height=300, 
        title = HTML("Selected material prices"), 
        status = def_box_col, 
        solidHeader = FALSE, 
        collapsible = TRUE,
        plotOutput("price_plot")
      )),
  fluidRow(
      box(
        width = 5,
        #height=400, 
        title = HTML("Inputs"), 
        status = def_box_col, 
        solidHeader = FALSE, 
        collapsible = TRUE,
       # plotOutput("price_plot"),
        verbatimTextOutput("inputs")
      )
        
      #)
    )
)
