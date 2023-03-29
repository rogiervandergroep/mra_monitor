
### in dit script 

library(tidyverse)
library(openxlsx)


load("output/mra_data_kerncijfers.rds")

load("output/mra_data_gemcijfers.RData")

### Row bind data
meta_totaal <- bind_rows(files_meta)

###koppelen thema en onderwerp aan data
meta_thema<- meta_totaal%>%
  select(thema, variabele, onderwerp)

function_thema <- function(x) {
  
  x<- x %>%
    left_join(meta_thema, by= "variabele") %>%
    
    relocate("thema", "onderwerp", "gebied_niveau", 
             "gebied_code", "gebied_naam",  "peiljaar_gebiedsindeling",  
             "variabele", "jaar", "waarde")
  
}
files_data_def<- map(files_data, function_thema)





geb_niv_levels = c("Nederland","MRA", "deelregio", "gemeenten", "wijk")


filter_functie <- function(x, jaartal) {
  x%>%
    filter(jaar          == jaartal,
           gebied_niveau %in% geb_niv_levels)
  
  
}


files_data_22 <- map(files_data_def, ~ filter_functie(., "2022"))  
files_data_21 <- map(files_data_def, ~ filter_functie(., "2021"))  
files_data_20 <- map(files_data_def, ~ filter_functie(., "2020"))  
files_data_19 <- map(files_data_def, ~ filter_functie(., "2019"))  

df_data22<- bind_rows(files_data_22)
df_data21<- bind_rows(files_data_21)
df_data20<- bind_rows(files_data_20)
df_data19<- bind_rows(files_data_19)

df_data22$gebied_niveau<-factor(df_data22$gebied_niveau, levels= geb_niv_levels)
df_data21$gebied_niveau<-factor(df_data21$gebied_niveau, levels= geb_niv_levels)
df_data20$gebied_niveau<-factor(df_data20$gebied_niveau, levels= geb_niv_levels)
df_data19$gebied_niveau<-factor(df_data19$gebied_niveau, levels= geb_niv_levels)

write.xlsx(df_data21, "output/kerncijfersMRA2021.xlsx", withFilter=T, overwrite=T)
write.xlsx(df_data20, "output/kerncijfersMRA2020.xlsx", withFilter=T, overwrite=T)
write.xlsx(df_data19, "output/kerncijfersMRA2019.xlsx", withFilter=T, overwrite=T)


function_wide_var <- function(x) {
  
  x%>%
  arrange(jaar)%>%
  select(-c(peiljaar_gebiedsindeling, thema))%>%
  pivot_wider(names_from = variabele, values_from = waarde)
  
  
}
  
list_df <- list("kerncijfers 2019" = df_data19,
                "kerncijfers 2020" = df_data20,
                "kerncijfers 2021" = df_data21)

list_df_wide_var  <-map(list_df,function_wide_var )
write.xlsx(list_df_wide_var, "output/kerncijfers21_19_wide_var.xlsx", overwrite=T)

