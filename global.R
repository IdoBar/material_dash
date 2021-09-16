# load custom functions from github
devtools::source_gist("7f63547158ecdbacf31b54a58af0d1cc", filename = "util.R")

# load/install packages
#install.packages("htmltools") #requires version 0.5.1 
# devtools::install_github("RinteRface/shinydashboardPlus")
pacman::p_load(tidyverse, scales, janitor, paletteer, tidyquant, #showtext,
               ggrepel, here,DT, leaflet, ggmap, glue, reactable, ggtext,
               shiny, shinydashboard, shinyAce, ggdist, 
               shinyWidgets, shinyEffects, shinyjqui, shinyjs)
               # kableExtra, formattable)

#c('htmltools', 'scales', 'janitor', 'paletteer', 'ggrepel', 'here','DT', 'shiny', 'shinydashboard', 'shinyAce', 'shinyWidgets', 'shinyEffects', 'shinyjqui', 'kableExtra', 'ISOweek', 'rgdal','sf', 'leaflet', 'leafem','raster', 'readxl')

pacman::p_load_gh("Mikata-Project/ggthemr")
pacman::p_load_gh("hrbrmstr/hrbrthemes")
pacman::p_load_gh("RinteRface/shinydashboardPlus")
# define theme for plots
clean_theme <- ggthemr(text_size = 18, palette = 'fresh', layout = "clean", 
                       set_theme = FALSE,
                       spacing = 0.5)
def_box_col <- "orange"
# add custom font
# font_add_google(name = "Titillium Web",   # Name of the font on the Google Fonts site
#                 family = "Titillium Web")
# # Load the fonts for all graphic devices
# showtext_auto()

