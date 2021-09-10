# TODO
# Map of suppliers
# Prices trend plot, better handling of multiple selections

function(input, output){
 #  # print list of input events (very useful for debugging the app, remove before deployment)
  output$inputs <-
    renderPrint({reactiveValuesToList(input)})
  # materials table ####
  PoC_data <- readxl::read_excel("data/PoC_Data.xlsx") 
  
  # geodetails <- geocode(PoC_data$Supplier_Location , output = "latlon", messaging=TRUE) %>% 
  #   mutate(Supplier_Location = PoC_data$Supplier_Location) %>% 
  #   write_xlsx(., "data/PoC_Data.xlsx", sheet = "supp_coordinates")
  geodetails <- readxl::read_excel("data/PoC_Data.xlsx", sheet = "supp_coordinates")
  
  material_table <- PoC_data %>% group_by(Item_ID, Item_Description) %>% 
    summarise(Number_of_Suppliers = n(), Mean_Price_AUD = mean(Item_Price_AUD, na.rm=TRUE)) %>% 
    ungroup()
  
  
  output$materialDT <- renderDT(server = FALSE,{
    DT::datatable(material_table ,
                  colnames = gsub("_", " ", names(material_table)), rownames = FALSE,
                  style = 'bootstrap', class = 'table-bordered table-condensed',
                  extensions = c('Buttons','Scroller'),
                  #caption = htmltools::HTML(tab_caption),#
                  options = list(dom = 'ftSiB',
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                 deferRender = TRUE, scrollY = 500,  scroller = TRUE))
  })

  # material prices plot ####
  material_plot_data <- reactive({
    # selected_row <- ifelse(is.null(input$materialDT_rows_selected), 1, 
    #                        as.vector(input$materialDT_rows_selected))
    selected_row <- input$materialDT_rows_selected
    selected_material <- material_table$Item_Description[selected_row] # 
    PoC_data %>% filter(Item_Description %in% selected_material)
  })
  material_price_dist <- function(){
    if (nrow(material_plot_data())==0) return(NULL)
    ggplot(material_plot_data(), aes(x = Item_Price_AUD)) + # , fill = Item_Description
      geom_histogram(fill = "lightskyblue") + facet_wrap(~Item_Description, ncol = 1) +
      labs(y="Count", x="Item Price (AUD)") +
      scale_y_continuous(breaks = int_breaks, limits = int_limits) + 
      # scale_fill_paletteer_d("awtools::a_palette") + 
      guides(fill="none") + 
      clean_theme$theme
      #      coord_flip() + clean_theme$theme
  }
  
   output$price_plot <- renderPlot(height = 300,{
     material_price_dist() +
       theme(plot.margin=margin(0,0,0,0, "mm")) # aspect.ratio = 1/2, 
     # ggsave(filedate("A_rabiei_pathogen_groups_2013-2020_horiz", ext = ".pdf",
     #                 outdir = "../output/", dateformat = FALSE),
     #        width =10, height = 8)
   })
  
}