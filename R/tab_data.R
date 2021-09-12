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
        width = 8,
        height=400, 
        title = HTML("List of available materials"), 
        status = def_box_col, 
        solidHeader = FALSE, 
        collapsible = TRUE,
        DTOutput("materialDT"),
        sidebar = boxSidebar(
          id = "matTableSidebar",
          width = 30,
          startOpen = FALSE,
          pickerInput(
            inputId = "filtGroups",
            label = "Filter groups", 
            choices = unique(readxl::read_excel("data/Dataset.xlsx")$CC_Group_Name),
            options = list(
              `actions-box` = TRUE,
              `selected-text-format` = "count > 2"), 
            multiple = TRUE
          )
        )
        ),
      box(
        width = 4,
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
