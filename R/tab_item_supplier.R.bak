item_supplier_tab <- tabItem(
  tabName = "Suppliers",
  
  br(),
  br(),

  column(
    width = 12,
    align = "left",
    h1("Selected Material Information")
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
        DTOutput("item_supplierDT")
        ),
      box(
        width = 4,
      #  height=300, 
        title = HTML("Suppliers Map"), 
        status = def_box_col, 
        solidHeader = FALSE, 
        collapsible = TRUE,
       leafletOutput("SupplierMap", height = 465)
      ))
        
      #)
)
