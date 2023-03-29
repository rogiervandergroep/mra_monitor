
library(tidyverse)
library(openxlsx)

load("output/mra_data_gemcijfers.RData")

### Row bind data
meta_totaal <- bind_rows(files_meta)

###koppelen thema en onderwerp aan data
meta_thema<- meta_totaal%>%
  select(thema, onderwerp, variabele, label, rekeneenheid_tekst)

function_thema <- function(x) {
  
   x<- x %>%
    left_join(meta_thema, by= "variabele")
  
}
files_data_def<- map(files_data_def, function_thema)

### netjes ordenen kolommen
functie_kol_netjes <- function(x) {
  
  x<- x %>%
    relocate("thema", "onderwerp",
             "gebied_niveau", "gebied_code", "gebied_naam", 
             "peiljaar_gebiedsindeling",  
             "variabele", "label","jaar", "waarde", "rekeneenheid_tekst")		
  
  }

files_data_def<- map(files_data_def, functie_kol_netjes)

data_totaal <- bind_rows(files_data_def)

saveRDS(data_totaal, file = "output/mra_data_gemcijfers_def.rds")

# pivot wider data

data_wide_jaar<-data_totaal%>%
  arrange(jaar)%>%
  filter(jaar>2010)%>%
  select(-peiljaar_gebiedsindeling)%>%
  pivot_wider(names_from = jaar, values_from = waarde)

data_wide_var<-data_totaal%>%
  arrange(jaar)%>%
  filter(jaar>2010)%>%
  select(-c(thema, onderwerp, label))%>%
  pivot_wider(names_from = variabele, values_from = waarde)

write.xlsx(data_wide_var,  "output/dataset_mra_wide_var.xlsx", overwrite = T)
write.xlsx(data_wide_jaar, "output/dataset_mra_wide_jaar.xlsx", overwrite = T)
write.xlsx(data_totaal,    "output/dataset_mra_long.xlsx", overwrite = T)
write.csv(data_totaal,     "output/dataset_mra_long.csv") 
write.xlsx(meta_totaal,    "output/metadata_mra_totaal.xlsx", overwrite = T)

# voor het dashbard van bas
write.xlsx(files_data_def, "output/Dashboard Bas/data_mra_persheet.xlsx", overwrite = T)
write.xlsx(files_meta,     "output/Dashboard Bas/meta_mra_persheet.xlsx", overwrite = T)


# aanvulling Bas Arbeidsmarkt -

data_arbeidsmarkt <- data_totaal %>%
  filter(thema=='Arbeidsmarkt') %>%
  
  mutate(variabele=
           case_when(
             variabele=='arb_werkzaam_aant' ~ 'arb_netpart_aant',
             TRUE ~ variabele),
         label=    
           case_when(
             label == 'Netto arbeidsparticipatie (%)' ~ 'Werkzame beroepsbevolking',
             TRUE~ label))

# arb_werkzaam (aantallen) en arb_netpart (percentages) 


data_arbeidsmarkt$variabele<- str_remove_all(data_arbeidsmarkt$variabele, "_aant")
data_arbeidsmarkt$variabele<- str_remove_all(data_arbeidsmarkt$variabele, "_perc")




data_arbeidsmarkt<- data_arbeidsmarkt %>%
  pivot_wider(names_from = rekeneenheid_tekst, values_from = waarde)%>%
  mutate(absoluut=absoluut*1000,
         percentage= percentage/100)

write.xlsx(data_arbeidsmarkt, "output/Dashboard Bas/data_arbeidsmarkt.xlsx", overwrite = T)







