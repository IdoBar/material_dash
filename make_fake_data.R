# load custom functions from github
devtools::source_gist("7f63547158ecdbacf31b54a58af0d1cc", filename = "util.R")

# load/install packages
#install.packages("htmltools") #requires version 0.5.1 
# devtools::install_github("RinteRface/shinydashboardPlus")
pacman::p_load(tidyverse, scales, janitor)

PoC_Supplier <- readxl::read_excel("data/Dataset.xlsx","Supplier") %>% 
  clean_names()

#  Supplier_Location <- geocode(glue("{PoC_Supplier$preferred_name}, Australia") , 
#                       output = "latlon", messaging=TRUE) %>% 
#   mutate(preferred_name = PoC_Supplier$preferred_name) #%>% 
# write_csv(Supplier_Location, "data/supplier_coordinates.csv")

#get it from local
Supplier_Location <- read_csv("data/supplier_coordinates2.csv")

Aus_suppliers <- Supplier_Location %>% filter(lon>0, lat<0) %>% inner_join(PoC_Supplier)

# materials table ####
PoC_data <- readxl::read_excel("data/Dataset.xlsx", "Dataset") 

multi_mat_groups <- PoC_data %>% count(CC_Group_Name) %>% arrange(desc(n)) %>% filter(n>20)

mat_table <- PoC_data %>% filter(CC_Group_Name %in% multi_mat_groups$CC_Group_Name) %>% 
  group_by(Item_Description) %>% slice(1) %>% select(-(Supplier_ID:Cost_Centre))

price_range_perc <- 0.45

make_random_material <- function(item, base_rate){
  mat_tab <- tibble(Supplier_ID=sample(Aus_suppliers$supplier_id, nrow(Aus_suppliers)),
                    Item_Description=item,
                    Rate = round(base_rate + base_rate*sample(seq(-price_range_perc, price_range_perc, 
                                                                  length.out = nrow(Aus_suppliers)), 
                                                              nrow(Aus_suppliers)), digits = 1),
                    Date = sample(seq(as.Date('2021/01/01'), as.Date('2021/09/01'), 
                                      by="day"), nrow(Aus_suppliers))) 
}

mat_table %>% select(-Rate, -Total, -Date) %>% ungroup() %>% 
  left_join(map2_dfr(mat_table$Item_Description, mat_table$Rate, .f = make_random_material)) %>% 
  write_xlsx("data/Dataset_fake.xlsx", "Dataset_fake")

  


